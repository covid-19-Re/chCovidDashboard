library(tidyverse)
library(lubridate)
library(DT)
library(slider)

# calculate doubling time from Re using a Gamma distributed generation time 
getDoublingTimeRe <- function(re, mu = 4.8, sigma = 2.3) {
  variance <- sigma * 2
  rate <- mu / variance
  shape <- mu^2 / variance

  beta <- (re^(1 / shape) - 1) * rate

  # Epidemic doubling times
  d <- log(2) / beta
  d <- ifelse(d < 0, -d, d)
  return(d)
}

tablesServer <- function(id, trendsData) {
  moduleServer(
    id,
    function (input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      rEstimatesPath <- reactive({
        rEstimatesPaths <- "data/Re/CHE-Estimates.rds"
        # load newer file of public and regular app if in test app
        if (str_detect(getwd(), "testapp")) {
          if (file.exists("../app/data/Re/CHE-Estimates.rds")) {
            rEstimatesPaths <- c("data/Re/CHE-Estimates.rds",
              "../app/data/Re/CHE-Estimates.rds")
          }
        }
        rEstimatesPathmTime <- file.mtime(rEstimatesPaths)
        rEstimatesPath <- list(
          path = rEstimatesPaths[which.max(rEstimatesPathmTime)],
          mtime = rEstimatesPathmTime[which.max(rEstimatesPathmTime)])
        return(rEstimatesPath)
      })

      rEstimates <- reactive({
        rEstimatesPath <- rEstimatesPath()
        rEstimates <- readRDS(rEstimatesPath$path) %>%
          filter(
            !(str_detect(region, "grR")),
            data_type != "Confirmed cases / tests",
            estimate_type == "Cori_slidingWindow") %>%
          select(region, data_type, date:median_R_lowHPD) %>%
          group_by(region, data_type) %>%
          top_n(1, date) %>%
          ungroup() %>%
          transmute(
            region = recode(region, "CHE" = "CH"),
            age_class = "all",
            event = recode(data_type,
              "Confirmed cases" = "cases",
              "Hospitalized patients" = "hospitalizations",
              "Deaths" = "deaths"),
            Re_estimate = median_R_mean,
            Re_lower = median_R_lowHPD,
            Re_upper = median_R_highHPD
          ) %>%
          mutate(across(.cols = tidyselect::starts_with("Re_"), getDoublingTimeRe, .names = "{.col}_dt"))
      })

      incidenceData <- reactive({
        rEstimatesPath <- rEstimatesPath()
        incidencePath <- str_replace(rEstimatesPath$path, "estimates", "Data")

        incidenceData <- readRDS(incidencePath) %>%
          filter(
            !(data_type %in% c("Stringency Index", "Confirmed cases / tests")),
            date_type == "report_plotting", is.na(local_infection)) %>%
          select(-local_infection, -date_type, -(positiveTests:country)) %>%
          mutate(
            value = replace_na(value, 0),
            valueNorm = replace_na(value / populationSize * 100000, 0)) %>%
          group_by(region, data_type) %>%
          mutate(
            value7day = slide_index_dbl(value, date, mean, .before = days(7)),
            valueNorm7day = slide_index_dbl(valueNorm, date, mean, .before = days(7)),
            value14day = slide_index_dbl(value, date, mean, .before = days(14)),
            valueNorm14day = slide_index_dbl(valueNorm, date, mean, .before = days(14))
          ) %>%
          top_n(1, date) %>%
          ungroup() %>%
          transmute(
            region = recode(region, "CHE" = "CH"),
            age_class = "all",
            event = recode(data_type,
              "Confirmed cases" = "cases",
              "Hospitalized patients" = "hospitalizations",
              "Deaths" = "deaths"),
            value7day = value7day,
            valueNorm7day = valueNorm7day,
            value14day = value14day,
            valueNorm14day = valueNorm14day)
      })

      comparisonData <- reactive({
        rEstimates <- rEstimates()
        incidenceData <- incidenceData()
        allData <- incidenceData %>%
          full_join(trendsData(), by = c("region", "age_class", "event")) %>%
          full_join(rEstimates, by = c("region", "age_class", "event"))
      })

      output$comparisonDataTable <- renderDataTable({
        print(rEstimatesPath())
        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(rowspan = 2, "Region"),
              th(rowspan = 2, "Age class"),
              th(rowspan = 2, "Event"),
              th(colspan = 2, "7 day mean incidence"),
              th(colspan = 2, "14 day mean incidence"),
              th(colspan = 3, "Doubling time (d)"),
              th(colspan = 3, "Weekly change (%)"),
              th(colspan = 3, HTML("R<sub>e</sub><sup>1</sup>")),
              th(colspan = 3, HTML("<span style='color:red;'>Doubling time</span> / <span style='color:green;'>Half-life</span> (d) from R<sub>e</sub><sup>2</sup>"))
            ),
            tr(
              lapply(rep(c("raw", "/100'000"), 2), th),
              lapply(rep(c("estimate", "lower 95% CI", "upper 95% CI"), 4), th),
            )
          ),
          tfoot(
            tr(
              td(colspan = 15,
                HTML(
                  str_c("<sup>1</sup>most recent R<sub>e</sub> estimate (",
                    as.character(rEstimatesPath()$mtime), ") from"),
                    "<a href='https://ibz-shiny.ethz.ch/covid-19-re/' target='blank'>",
                    "https://ibz-shiny.ethz.ch/covid-19-re/</a><br>",
                  "<sup>2</sup>Doubling time / half-life calculated from R<sub>e</sub> assuming",
                    "a gamma distributed generation time with &mu; = 4.8 days and &sigma; = 2.3 days"
                )
              )
            )
          )
        ))

        tableData <- comparisonData() %>%
          mutate(
            region = factor(region, levels = c("CH", "AG", "AI", "AR", "BE", "BL", "BS", "FR",
              "GE", "GL", "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
              "UR", "VD", "VS", "ZG", "ZH", "FL")),
            age_class = factor(age_class),
            event = factor(event)
            ) %>%
          arrange(event, region, age_class)

        table <- datatable(
          tableData,
          rownames = FALSE,
          container = sketch,
          filter = "top",
          options = list(
            dom = "t",
            pageLength = 125,
            lengthMenu = c(25, 50, 100, 125))) %>%
          formatRound(
            columns = c(
              "value7day", "valueNorm7day",
              "value14day", "valueNorm14day"
            ),
            digits = 2) %>%
          formatSignif(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper",
              "wc_estimate", "wc_lower", "wc_upper",
              "Re_estimate", "Re_lower", "Re_upper",
              "Re_estimate_dt", "Re_lower_dt", "Re_upper_dt"
            ),
            digits = 4) %>%
          formatStyle(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper",
              "Re_estimate", "Re_lower", "Re_upper"),
            backgroundColor = c("#f0f8ffab")) %>%
          formatStyle(
            columns = c("Re_estimate_dt", "Re_lower_dt", "Re_upper_dt"),
            valueColumns = c("Re_estimate", "Re_lower", "Re_upper"),
            target = "cell",
            color = styleInterval(1, c("green", "red"))
          ) %>%
          formatStyle(
            columns = c("dt_estimate", "dt_lower", "dt_upper"),
            target = "cell",
            color = styleInterval(0, c("green", "red"))
          )
        return(table)
      })

      output$tableCaption <- renderUI({
        HTML("<p>Doubling time and weekly change in doubling time from a negative binomial generalized linear model. ",
            "We also report here ", str_c("most recent R<sub>e</sub> values (", as.character(rEstimatesPath()$mtime), ") from "),
            "<a href='https://ibz-shiny.ethz.ch/covid-19-re/' target='blank'>https://ibz-shiny.ethz.ch/covid-19-re/</a>",
            "and doubling times calculated from R<sub>e</sub> assuming a gamma distributed generation time with &mu; = 4.8 days ",
            "and &sigma; = 2.3 days.</p>",
            "<p>Use filter fields to filter columns. Click on column names to sort. Shift-Click to sort by multiple columns.</p>")
      })

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("trendsComparison.csv", sep = "")
        },
        content = function(file) {
          write.csv(comparisonData(), file, row.names = FALSE)
        }
      )
    }
  )
}

library(tidyverse)
library(lubridate)
library(DT)
library(slider)

# calculate doubling time from Re using a Gamma distributed generation time 
getDoublingTimeRe <- function(re, mu = 4.8, sigma = 2.3) {
  variance <- sigma^2
  rate <- mu / variance
  shape <- mu^2 / variance

  beta <- (re^(1 / shape) - 1) * rate

  # Epidemic doubling times
  d <- log(2) / beta
  d <- ifelse(d < 0, -d, d)
  return(d)
}

tablesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
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
        rEstimatesPathLIE <- str_replace(rEstimatesPath$path, "CHE", "LIE")
        rEstimates <- readRDS(rEstimatesPath$path) %>%
          bind_rows(readRDS(rEstimatesPathLIE)) %>%
          filter(
            !(str_detect(region, "grR")),
            data_type != "Confirmed cases / tests",
            estimate_type == "Cori_slidingWindow") %>%
          select(region, data_type, date:median_R_lowHPD) %>%
          group_by(region, data_type) %>%
          top_n(1, date) %>%
          ungroup() %>%
          transmute(
            region = recode(region, "CHE" = "CH", "LIE" = "FL"),
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

        return(rEstimates)
      })

      incidenceDataPath <- reactive({
        incidenceDataPaths <- "data/trends-incidenceTable.qs"
        # load newer file of public and regular app if in test app
        if (str_detect(getwd(), "testapp")) {
          if (file.exists("../app/data/trends-incidenceTable.qs")) {
            incidenceDataPaths <- c("data/trends-incidenceTable.qs",
              "../app/data/trends-incidenceDataTable.qs")
          }
        }
        incidenceDataPathmTime <- file.mtime(incidenceDataPaths)
        incidenceDataPath <- list(
          path = incidenceDataPaths[which.max(incidenceDataPathmTime)],
          mtime = incidenceDataPathmTime[which.max(incidenceDataPathmTime)])
        return(incidenceDataPath)
      })

      incidenceData <- reactive({
        incidenceDataPath <- incidenceDataPath()
        eventCounts <- qs::qread(incidenceDataPath$path)
      })

      trendsData <- reactive({
        incidenceDataPath <- incidenceDataPath()
        trendsData <- qs::qread(str_replace(incidenceDataPath$path, "incidenceTable", "predictionsTable"))
      })

      comparisonData <- reactive({
        rEstimates <- rEstimates()
        incidenceData <- incidenceData() %>%
          select(-value7daySum, -valueNorm7daySum, -value14daySum, -valueNorm14daySum)
        allData <- incidenceData %>%
          full_join(trendsData(), by = c("region", "age_class", "event")) %>%
          full_join(rEstimates, by = c("region", "age_class", "event"))
      })

      output$comparisonDataTable <- renderDataTable({

        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(colspan = 3, "", style = "border-bottom:0px;"),
              th(colspan = 2, "7 day mean incidence"),
              th(colspan = 2, "14 day mean incidence"),
              th(colspan = 3, "Doubling time (d)"),
              th(colspan = 3, "Weekly change (%)"),
              th(colspan = 3, HTML("R<sub>e</sub><sup>1</sup>")),
              th(colspan = 3, HTML("<span style='color:red;'>Doubling time</span> / <span style='color:green;'>Half-life</span> (d) from R<sub>e</sub><sup>2</sup>"))
            ),
            tr(
              th("Region"),
              th("Age class"),
              th("Event"),
              lapply(rep(c("raw", "/100'000"), 2), th),
              lapply(rep(c("estimate", "lower 95% CI", "upper 95% CI"), 4), th),
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
              lengthMenu = c(25, 50, 100, 125),
              scrollX = TRUE,
              fixedColumns = list(leftColumns = 3))
          ) %>%
          formatRound(
            columns = c(
              "value7dayAve", "valueNorm7dayAve",
              "value14dayAve", "valueNorm14dayAve"
            ),
            digits = 2) %>%
          formatRound(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper",
              "wc_estimate", "wc_lower", "wc_upper",
              "Re_estimate", "Re_lower", "Re_upper",
              "Re_estimate_dt", "Re_lower_dt", "Re_upper_dt"
            ),
            digits = 2) %>%
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
            "We also report here ", str_c("most recent R<sub>e</sub> values (calculated on ", as.character(rEstimatesPath()$mtime), ") from "),
            "<a href='https://ibz-shiny.ethz.ch/covid-19-re-international/' target='blank'>https://ibz-shiny.ethz.ch/covid-19-re-international/</a>",
            "and doubling times calculated from R<sub>e</sub> assuming a gamma distributed generation time with &mu; = 4.8 days ",
            "and &sigma; = 2.3 days.</p>",
            "<p>Use filter fields to filter columns. Click on column names to sort. Shift-Click to sort by multiple columns.</p>")
      })

      output$tableFooter <- renderUI({
        HTML("<span class='help-block'>",
          str_c("<sup>1</sup>most recent R<sub>e</sub> estimate (calculated on ",
            as.character(rEstimatesPath()$mtime), ") from"),
            "<a href='https://ibz-shiny.ethz.ch/covid-19-re-international/' target='blank'>",
            "https://ibz-shiny.ethz.ch/covid-19-re-international/</a><br>",
          "<sup>2</sup>Doubling time / half-life calculated from R<sub>e</sub> assuming",
            "a gamma distributed generation time with &mu; = 4.8 days and &sigma; = 2.3 days",
          "</span>"
        )
      })

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("trendsComparison.csv", sep = "")
        },
        content = function(file) {
          write.csv(comparisonData(), file, row.names = FALSE)
        }
      )

      output$dataDownloads <- renderUI({
        if (str_detect(getwd(), "testapp")) {
          ui <- tabsetPanel(
            type = "pills", id = "summaryTabs",
            tabPanel(p(class = "tab-title", "Data download"), value = "downloads",
              div(class = "panel panel-primary panel-tab",
                div(class = "panel-body", style = "background:white;",
                  HTML(
                    "<a id='tables-downloadEventCOunt' class='btn btn-default' href='eventCounts.csv' target='_blank' download=''>
                        <i class='fa fa-download'></i>
                        Download event counts .csv
                    </a>"
                  )
                )
              )
            )
          )
        } else {
          ui <- ""
        }
        return(ui)
      })
    }
  )
}

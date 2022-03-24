library(tidyverse)
library(lubridate)
library(cowplot)
library(DT)

source("R/trendsModule-Files/trendsModule-global.R")

trendsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      cols <- RColorBrewer::brewer.pal(4, "Set1")
      t.cols <- cols
      for (i in seq_along(cols)) {
        x <- col2rgb(cols[i])
        t.cols[i] <- rgb(x[1, ], x[2, ], x[3, ], alpha = 125, maxColorValue = 255)
      }

      reactPars <- reactive({
        reactPars <- pars
        reactPars$time_window <- input$time_window

        reactPars$lastday <- input$lastday
        reactPars$begin <- reactPars$lastday - reactPars$delete + 1 - reactPars$time_window
        reactPars$end <- reactPars$lastday - reactPars$delete

        return(reactPars)
      })

      output$lastDataUpdate <- renderUI({
        HTML(glue::glue("<b>Last Data Update:</b> {max(modelOutput()$predictions$date)}"))
      })

      eventCounts <- reactive({
        reactPars <- reactPars()

        eventCountsAll <- getEventCountsPublic()

        eventCounts <- lapply(eventCountsAll, function(df) {
          df2wk <- df %>%
            filter(date >= reactPars$begin[df$event[1]] & date <= reactPars$end[df$event[1]])
          return(df2wk)
        })

        return(eventCounts)
      })

      modelOutput <- reactive({
        # only calculate if inputs are not default
        if (file.exists("data/trends-predictions.qs") & input$lastday == today() & input$time_window == 14) {
          modelOutput <- list(
            predictions = qs::qread("data/trends-predictions.qs"),
            doublingTimes = qs::qread("data/trends-doublingTimes.qs"),
            ranking = qs::qread("data/trends-ranking.qs")
          )
        } else {
          models <- calcTrendsModel(eventCounts())
          modelOutput <- list(
            predictions = calcPredictions(models),
            doublingTimes = calcDoublingTimes(models)
          )
          modelOutput$ranking <- calcRanking(modelOutput$doublingTimes)
        }
        return(modelOutput)
      })

      output$countryPlots <- renderPlot({

        countryPlotsList <- list(
          plotPredictions(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
            regionSelect = "CH", eventSelect = "cases",
            fillColor = t.cols[4],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH")),
          plotPredictions(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
            regionSelect = "CH", eventSelect = "hospitalizations",
            fillColor = t.cols[3],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH")),
          plotPredictions(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
            regionSelect = "CH", eventSelect = "icu",
            fillColor = t.cols[2],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: BAG")),
          plotPredictions(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
            regionSelect = "CH", eventSelect = "deaths",
            fillColor = t.cols[1],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH"))
        )
        plot <- plot_grid(plotlist = countryPlotsList, ncol = 2)
        # ggsave("www/pdf/trends_CHE.pdf", plot, width = 16, height = 9)
        return(plot)
      })

      output$cantonPlotsCases <- renderPlot({

        plot <- plotCantons(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
          eventSelect = "cases", color = t.cols[4],
          sorting = input$cantonSortCases, lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_regional_cases.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$cantonPlotsHospitalizations <- renderPlot({

        plot <- plotCantons(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
          eventSelect = "hospitalizations", color = t.cols[3],
          sorting = input$cantonSortHospitalizations, lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_regional_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$ageClassPlotsCases <- renderPlot({
        plot <- plotAgeClass(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
          eventSelect = "cases", color = t.cols[4],
          lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_age_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$ageClassPlotsHospitalizations <- renderPlot({
        plot <- plotAgeClass(modelOutput()$predictions, modelOutput()$doublingTimes, modelOutput()$ranking,
          eventSelect = "hospitalizations", color = t.cols[3],
          lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_age_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$rankingPlotCases <- renderPlot({

        rankingPlot <- modelOutput()$ranking %>% filter(age_class == "all", event == "cases")
        rankingPlot$region <- fct_reorder(rankingPlot$region, rankingPlot$estimate, min)

        plot <- plotRanking(rankingPlot, cols, lang = input$plot_language)
        return(plot)
      })

      output$rankingPlotHospitalizations <- renderPlot({

        rankingPlot <- modelOutput()$ranking %>% filter(age_class == "all", event == "hospitalizations")
        rankingPlot$region <- fct_reorder(rankingPlot$region, rankingPlot$estimate, min)

        plot <- plotRanking(rankingPlot, cols, lang = input$plot_language)
        return(plot)
      })

      comparisonData <- reactive({

        doublingTimesRaw <- modelOutput()$doublingTimes
        rankingRaw <- modelOutput()$ranking

        doublingTimesTable <- doublingTimesRaw %>%
          transmute(
            region, age_class_type, age_class, event,
            dt_estimate = estimate,
            dt_lower = lower,
            dt_upper = upper)

        rankingTable <- rankingRaw %>%
          transmute(
            region, age_class_type, age_class, event,
            wc_estimate = estimate * 100,
            wc_lower = lower * 100,
            wc_upper = upper * 100)

        allData <- doublingTimesTable %>%
          full_join(rankingTable, by = c("region", "age_class_type", "age_class", "event"))

        return(allData)
      })

      output$comparisonDataTable <- renderDataTable({
        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(rowspan = 2, "Region"),
              th(rowspan = 2, "Age class type"),
              th(rowspan = 2, "Age class"),
              th(rowspan = 2, "Event"),
              th(colspan = 3, "Doubling time (d)"),
              th(colspan = 3, "Weekly change (%)")),
            tr(
              lapply(rep(c("estimate", "lower 95% CI", "upper 95% CI"), 2), th),
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
          formatSignif(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper",
              "wc_estimate", "wc_lower", "wc_upper"
            ),
            digits = 4) %>%
          formatStyle(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper"),
            backgroundColor = c("#f0f8ffab")) %>%
          formatStyle(
            columns = c("dt_estimate", "dt_lower", "dt_upper"),
            target = "cell",
            color = styleInterval(0, c("green", "red"))
          )
        return(table)
      })

      output$tableCaption <- renderUI({
        HTML("<p>Doubling time and weekly change in doubling time from a negative binomial generalized linear model. ",
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

library(rlang)


tsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      # TODO Why is it not required to put the id of the sub module into a ns() function?
      basicFilterServers <- basicFilters %>% map2(names(.), function (f, n) { f$server(n) })

      ### Basic UI Logic: Enabling/disabling/setting filters etc. ###

      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      observe({
        shinyjs::toggleState(id = "event", condition = !input$normalization)
        shinyjs::toggleState(id = "display_prob", condition = !input$normalization)
        shinyjs::toggleState(id = "given", condition = input$display_prob && !input$normalization)

        for (fs in basicFilterServers) {
          # TODO Dangerous stuff since it reaches into a sub module. Can this be improved?
          shinyjs::toggleState(selector = paste0("#", fs()$session$ns("compare")), condition = !input$normalization)
          if (input$normalization) {
            updateCheckboxInput(fs()$session, "compare", value = FALSE)
          }
        }
      })

      # If one compare checkbox is checked, the others should be unchecked.
      lapply(  # TODO Why does a for-loop not work? What's the difference to lapply()?
        X = basicFilterServers,
        FUN = function(fs){
          observeEvent(fs()$compare, {
            if (fs()$compare) {
              for (fs2 in basicFilterServers) {
                if (fs2()$attributeName != fs()$attributeName) {
                  # TODO Dangerous stuff since it reaches into a sub module. Can this be improved?
                  updateCheckboxInput(fs2()$session, "compare", value = FALSE)
                }
              }
            }
          })
        }
      )

      # Date slider for the map
      observe({
        if (input$display_prob || input$granularity == "Days") {
          updateSliderInput(
            session, "map_selected_day",
            min = as.Date("2020-03-01"), max = today(),
            timeFormat = "%F"
          )
        } else if (input$granularity == "Weeks") {
          updateSliderInput(
            session, "map_selected_day",
            min = as.Date("2020-03-01"), max = today(),
            timeFormat = "%F (week %W)"
          )
        } else if (input$granularity == "Months") {
          updateSliderInput(
            session, "map_selected_day",
            min = as.Date("2020-03-01"), max = today(),
            timeFormat = "%b %Y"
          )
        }
      })

      # Normalization

      observeEvent(input$normalization, {
        disabledJs <- if (input$normalization) "false" else "true"
        shinyjs::runjs(
          paste0("jQuery(\"[data-id='", ns("normalization_timerange"), "'\").prop(\"disabled\", ", disabledJs, ")"))
        if (input$normalization) {
          updateRadioButtons(session, "event", selected = "Positive test")
          updateCheckboxInput(session, "display_prob", value = FALSE)
        }
      })


      ### Loading, preparing and filtering Data ###

      data <- reactive({
        d <- load_and_process_data()

        # Introduces an ID to make the rows unique and easier identifiable.
        d <- cbind(id = as.integer(rownames(d)), d)

        # Creates the "date" column
        if (input$display_prob) {
          d <- d %>% mutate(date = fall_dt)
        } else {
          d <- d %>% mutate(date = !!as.symbol(tsConstants$eventDateCols[[input$event]]))
        }

        return(d)
      })

      compare <- reactive({
        for (fs in basicFilterServers) {
          if (fs()$compare) {
            return (fs()$attributeName)
          }
        }
        return(NA)
      })

      compare_proportions <- reactive({
        for (fs in basicFilterServers) {
          if (fs()$compareProportions) {
            return (TRUE)
          }
        }
        return (FALSE)
      })

      availablePlotTypes <- reactive({
        # Plot types: histogram, line chart, area chart, map
        available <- NULL
        if (!input$display_prob && !compare_proportions()) {
          available <- append(available, c("histogram", "line", "area"))
        } else {
          available <- append(available, "line")
        }
        if (compare_proportions()) {
          available <- append(available, "area")
        }
        if (replace_na(compare() == "canton" || compare() == "expCountryCode", FALSE)
          && !compare_proportions()) {
          available <- append(available, "map")
        }
        return (available)
      })

      plotType <- reactiveValues(value = "histogram")
      observeEvent(input$plotTypeHistogram, { plotType$value <- "histogram" })
      observeEvent(input$plotTypeLine, { plotType$value <- "line" })
      observeEvent(input$plotTypeArea, { plotType$value <- "area" })
      observeEvent(input$plotTypeMap, { plotType$value <- "map" })

      currentPlotType <- reactive({
        available <- availablePlotTypes()
        if (plotType$value %in% available) {
          return (plotType$value)
        } else {
          plotType$value <- available[1]
          return (available[1])
        }
      })

      plot_type <- reactive({
        if (currentPlotType() == "histogram" || currentPlotType() == "map") {
          return ("discrete")
        } else {
          return ("continuous")
        }
      })

      observe({
        available <- availablePlotTypes()
        shinyjs::toggleState("plotTypeHistogram", condition = "histogram" %in% available)
        shinyjs::toggleState("plotTypeLine", condition = "line" %in% available)
        shinyjs::toggleState("plotTypeArea", condition = "area" %in% available)
        shinyjs::toggleState("plotTypeMap", condition = "map" %in% available)

        current <- currentPlotType()
        shinyjs::toggleCssClass("plotTypeHistogram", "ts-active-btn", condition = "histogram" == current)
        shinyjs::toggleCssClass("plotTypeLine", "ts-active-btn", condition = "line" == current)
        shinyjs::toggleCssClass("plotTypeArea", "ts-active-btn", condition = "area" == current)
        shinyjs::toggleCssClass("plotTypeMap", "ts-active-btn", condition = "map" == current)

        shinyjs::toggleCssClass("map_slider", "ts-hidden", condition = "map" != current)
      })

      # Returns a list of normalization constants (e.g., hospitalisation rate). Each age group has
      # a corresponding value. The values are not affected by the filters.
      normalizationConstants <- reactive({
        if (!input$normalization) {
          return (NULL)
        }

        validate(
          need(
            !is.null(input$normalization_timerange),
            paste0("Must specify at least one month for normalization.")
          )
        )

        startDates <- ymd(input$normalization_timerange)
        selectedTimeIntervals <- as.list(interval(startDates, startDates %m+% months(1) %m-% days(1)))

        results <- list()

        dataProc <- data() %>%
          filter(date %within% selectedTimeIntervals)
        for (category in unique(dataProc$ageGroup)) {
          if (is.na(category) || category == "Unknown") {
            next
          }

          # All positive tests
          dDenominator <- dataProc %>%
            filter(ageGroup == category) %>%
            filter(positiveTest)
          dNumerator <- dDenominator %>% filter(hospitalisation == 1)
          rate <- sum(dNumerator$mult) / sum(dDenominator$mult)
          results[[category]] <- rate
        }

        resultsTibble <- tibble(names(results), unlist(results))
        names(resultsTibble) <- c("ageGroup", "normalizationConstant")
        # TODO Handling zeros. It should not happen so often currently since the smallest granularity is month
        #     ... and unfortunately, we have enough cases.
        return (resultsTibble)
      })

      # Validators

      notFalselyUsingNegativeTestDataValidator <- reactive({
        if ((input$event == "Test (any result)") || (input$display_prob && input$given == "Test (any result)")) {
          validate(
            need(
              !basicFilterServers[['travelClass']]()$isFiltering && !basicFilterServers[['travelClass']]()$compare,
              "Currently lacking information about travel status for negative test data."
            )
          )
        }
      })

      validators <- reactive({
        notFalselyUsingNegativeTestDataValidator()
      })

      # Filters

      clinicalEventFiltered <- reactive({
        return (
          switch(input$event,
                 "Test (any result)" = data(),  # Nothing to do
                 "Positive test" = data() %>% filter(positiveTest),
                 "Hospitalisation" = data() %>% filter(hospitalisation == 1),
                 "Death" = data() %>% filter(pttod == 1),
                 "ICU admission" = data() %>% filter(icu_aufenthalt == 1)
          )
        )
      })

      givenClinicalEventFiltered <- reactive({
        if (input$display_prob) {
          return (
            switch(input$given,
                   "Test (any result)" = data(),  # Nothing to do
                   "Positive test" = data() %>% filter(positiveTest),
                   "Hospitalisation" = data() %>% filter(hospitalisation == 1),
                   "Death" = data() %>% filter(pttod == 1),
                   "ICU admission" = data() %>% filter(icu_aufenthalt == 1)
            )
          )
        } else {
          return (data())
        }
      })

      ### Processors ###
      # Processors are functions that manipulate data. They are defined as reactive expressions since they have direct
      # access to the user input. However, they do not access the data but are defined as function factories: they
      # return a function which takes the data and gives a modified version of the data back.

      # Rounds the date depending on the granularity
      dateRoundingProcessor <- reactive({
        function(data) {
          return (
            switch(input$granularity,
                   Days = data,
                   Weeks = data %>% mutate(date = floor_date(date, unit = "week",
                                                             week_start = getOption("lubridate.week.start", 1))),
                   Months = data %>% mutate(date = floor_date(date, unit = "month"))
            )
          )
        }
      })


      ### Control the display options ###
      observe({
        shinyjs::toggleState(id = "stack_histograms", condition = plot_type() == "discrete" && !input$log_scale
          && !is.na(compare()))
        shinyjs::toggleState(id = "granularity", condition = plot_type() == "discrete")
        shinyjs::toggleState(id = "smoothing_window", condition = plot_type() == "continuous")

        if (input$log_scale && input$stack_histograms) {
          updateCheckboxInput(session, "stack_histograms", value = FALSE)
        }
      })


      ### Putting everything together ###
      output$mainPlot <- renderPlotly({
        validators()

        # Apply basic filters
        dataProc <- data()
        filterWithActiveComparison <- NULL
        for (fs in basicFilterServers) {
          dataProc <- fs()$filter(dataProc)
          if (fs()$compare) {
            filterWithActiveComparison <- fs()
          }
        }

        # Exclude all data before start of stratified negative test records
        # TODO Describe the background of this step.
        if ((input$event == "Test (any result)") || (input$display_prob && input$given == "Test (any result)")) {
          doFilter <- FALSE
          if (!is.na(compare())) {
            doFilter <- TRUE
          }
          for (fs in basicFilterServers) {
            if (fs()$isFiltering) {
              doFilter <- TRUE
            }
          }
          if (doFilter) {
            stratifiedTestingStart <- min((dataProc %>% filter(positiveTest == FALSE, !is.na(canton)))$fall_dt)
            dataProc <- dataProc %>% filter(fall_dt >= stratifiedTestingStart)
          }
        }

        # General transformations and calculations for later use
        if (input$display_prob) {
          xlabel <- "Date of Test"
        } else {
          xlabel <- paste("Date of", input$event)
        }
        smoothing_interval <- tsConstants$slidingWindowChoicesToIntervals[[input$smoothing_window]]
        minDate <- min((dataProc %>% drop_na(date))$date)
        maxDate <- max((dataProc %>% drop_na(date))$date)

        # Case 1a: showing the total frequencies (no normalization)
        if (!input$normalization && !input$display_prob && is.na(compare())) {
          plotData <- dataProc %>%
            dplyr::intersect(clinicalEventFiltered())
          if (plot_type() == "discrete") {
              plotData <- dateRoundingProcessor()(plotData)
            }
          plotData <- plotData %>%
            group_by(date) %>%
            summarize(count = sum(mult), .groups = "drop")
          if (plot_type() == "continuous") {
            plotData$count <- slide_index_dbl(plotData$count, plotData$date, sum, .before = smoothing_interval)
          }

          plotDef <- list(
            plotData, "date", "count",
            ylab = "Total count"
          )
        }

        # Case 1b: with normalization
        if (input$normalization && !input$display_prob && is.na(compare())) {
          plotData <- dataProc %>%
            filter(hospitalisation == 1) %>%
            group_by(ageGroup, date) %>%
            summarize(count = sum(mult), .groups = "drop") %>%
            inner_join(normalizationConstants(), by = "ageGroup") %>%
            mutate(normalized = count / normalizationConstant)
          if (plot_type() == "discrete") {
            plotData <- dateRoundingProcessor()(plotData)
          }
          plotData <- plotData %>%
            group_by(date) %>%
            summarize(count = sum(normalized), .groups = "drop")
          if (plot_type() == "continuous") {
            plotData$count <- slide_index_dbl(plotData$count, plotData$date, sum, .before = smoothing_interval)
          }
          plotDef <- list(
            plotData, "date", "count",
            ylab = "Total count"
          )
        }

        # Case 2: comparing the frequencies
        if (!input$display_prob && !is.na(compare())) {
          dataProc <- dataProc %>%
            dplyr::intersect(clinicalEventFiltered())
          if (plot_type() == "discrete") {
            dataProc <- dataProc %>% dateRoundingProcessor()()
          }
          plotData <- NULL
          for (compare_val in filterWithActiveComparison$getComparisonGroups(dataProc)) {
            d <- dataProc %>%
              filterWithActiveComparison$getEntriesOfGroup(compare_val) %>%
              group_by(date) %>%
              summarize(count = sum(mult), .groups = "drop")
            if (compare_proportions()) {
              d <- d %>%
                complete(date = seq.Date(minDate, maxDate, by = "day")) %>%
                mutate(count = replace_na(count, 0)) %>%
                drop_na(date)
              d$smoothedCount <- slide_index_dbl(d$count, d$date, sum, .before = smoothing_interval)
            } else if (plot_type() == "continuous") {
              d$count <- slide_index_dbl(d$count, d$date, sum, .before = smoothing_interval)
            }
            d[, compare()] <- compare_val
            plotData <- bind_rows(plotData, d)
          }
          if (!compare_proportions()) {
            if (currentPlotType() == "map") {
              if (!is.null(plotData)) {
                selectedDate <- switch(
                  input$granularity,
                  "Days" = input$map_selected_day,
                  "Weeks" = floor_date(input$map_selected_day, unit = "week",
                                       week_start = getOption("lubridate.week.start", 1)),
                  "Months" = floor_date(input$map_selected_day, unit = "month")
                )
                plotData <- plotData %>%
                  filter(date == selectedDate) %>%
                  group_by(!!as.symbol(compare())) %>%
                  summarize(
                    count = sum(count)
                  )
                plotDef <- list(
                  plotData,
                  region = if (compare() == "canton") "switzerland" else "world"
                )
              }
            } else {
              plotDef <- list(
                plotData, "date", "count",
                groupingAttributeName = compare(),
                ylab = "Total count"
              )
              if (plot_type() == "discrete") {
                plotDef$stacked <- input$stack_histograms
              }
            }
          } else {
            plotData <- plotData %>%
              group_by(date) %>%
              mutate(proportion = smoothedCount / sum(smoothedCount))

            plotDef <- list(
              plotData, "date", "proportion",
              groupingAttributeName = compare(),
              ylab = paste0("Proportion of ", input$event, "s")
            )
          }
        }

        # Case 3: looking at the probabilities when a type of event is given
        if (input$display_prob && is.na(compare())) {
          denominatorData <- dplyr::intersect(dataProc, givenClinicalEventFiltered())
          numeratorData <- dplyr::intersect(denominatorData, clinicalEventFiltered())

          processDataInternal <- function(d) {
            d <- d %>%
              group_by(date) %>%
              summarize(
                count = sum(mult),
                .groups = "drop"
              ) %>%
              complete(date = seq.Date(minDate, maxDate, by = "day")) %>%
              mutate(count = replace_na(count, 0))
            return (d)
          }
          denominatorData <- processDataInternal(denominatorData)
          numeratorData <-  processDataInternal(numeratorData)

          num <- slide_index_dbl(numeratorData$count, numeratorData$date, sum, .before = smoothing_interval)
          denom <- slide_index_dbl(denominatorData$count, denominatorData$date, sum, .before = smoothing_interval)

          plotData <- tibble(date = denominatorData$date, prob = num / denom)
          plotDef <- list(
            plotData, "date", "prob",
            ylab = paste0("Fraction of ", input$given, "s involving ", input$event)
          )
        }

        # Case 4: comparing the probabilities
        if (input$display_prob && !is.na(compare())) {
          denominatorData <- dplyr::intersect(dataProc, givenClinicalEventFiltered())
          numeratorData <- dplyr::intersect(denominatorData, clinicalEventFiltered())

          processDataInternal <- function(d) {
            d <- d %>%
              group_by(date) %>%
              summarize(
                count = sum(mult),
                .groups = "drop"
              ) %>%
              complete(date = seq.Date(minDate, maxDate, by = "day")) %>%
              mutate(count = replace_na(count, 0))
            return (d)
          }

          plotData <- NULL
          for (compare_val in filterWithActiveComparison$getComparisonGroups(dataProc)) {
            d <- dataProc %>%
              filterWithActiveComparison$getEntriesOfGroup(compare_val)
            dDenom <- dplyr::intersect(d, denominatorData)
            dNum <- dplyr::intersect(d, numeratorData)

            dDenom <- processDataInternal(dDenom)
            dNum <- processDataInternal(dNum)
            denom <- slide_index_dbl(dDenom$count, dDenom$date, sum, .before = smoothing_interval)
            num <- slide_index_dbl(dNum$count, dNum$date, sum, .before = smoothing_interval)

            d <- tibble(date = dDenom$date, prob = num / denom)
            d[, compare()] <- compare_val
            plotData <- bind_rows(plotData, d)
          }

          if (!compare_proportions() && currentPlotType() == "map") {
            if (!is.null(plotData)) {
              plotData <- plotData %>%
                filter(date == input$map_selected_day) %>%
                mutate(count = prob)
              plotDef <- list(
                plotData,
                region = if (compare() == "canton") "switzerland" else "world"
              )
            }
          } else {
            if (compare_proportions()) {
              plotData <- plotData %>%
                mutate(prob = replace_na(prob, 0)) %>%
                group_by(date) %>%
                mutate(prob = prob / sum(prob))
            }

            plotDef <- list(
              plotData, "date", "prob",
              groupingAttributeName = compare(),
              ylab = paste0("Fraction of ", input$given, "s involving ", input$event)
            )
          }
        }

        validate(need(
          nrow(plotData) > 0,
          "No data matches the requested combination of filters."
        ))

        p <- do.call(tsPlots[[currentPlotType()]], plotDef)
        if (currentPlotType() != "map") {
          # Finalize ggplot and transform to plotly
          p <- p +
            xlab(xlabel) +
            scale_x_date(date_breaks = "months", labels = date_format("%m-%Y")) +
            theme_light()
          if (input$log_scale) {
            p <- p + scale_y_log10()
          }
          p <- ggplotly(p)
        }

        # Draw the plot
        plotlyPlot <- p %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("zoom2d", "toImage", "resetScale2d", "pan2d")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )

        # Give the data from the recent 30 days a gray background to mark them as uncertain.
        # Annotating in ggplot2 did not work as it was not transferred. Calling the layout() of plotly also failed
        # (see https://stackoverflow.com/a/50361382). Therefore, this solution:
        if (currentPlotType() != "map") {
          todayDaysSince1970 <- as.integer(as.POSIXct(Sys.Date())) / 60 / 60 / 24
          plotlyPlot[['x']][['layout']][['shapes']] <- list(
            list(type = "rect",
                 fillcolor = "grey", line = list(color = "gray"), opacity = 0.2,
                 # Inf and -Inf don't work here.
                 x0 = todayDaysSince1970 - 30, x1 = todayDaysSince1970 + 100, xref = "x",
                 y0 = -99999999, y1 = 99999999, yref = "y")
          )
        }

        plotlyPlot
      })
    }
  )
}

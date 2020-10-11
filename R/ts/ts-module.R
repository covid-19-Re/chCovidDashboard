library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)

source("R/utilities.R")
source("R/ts/ts-constants.R")
source("R/ts/ts-utils.R")
source("R/ts/ts-load_and_process_data.R")

# Submodules
source("R/ts/ts-basic-filter-module.R")

basicFilters <- list(
  "ageGroup" = createBasicFilter("Age group", tsConstants$ageGroups, "ageGroup"),
  "canton" = createBasicFilter("Canton", tsConstants$cantons, "canton"),
  "travelClass" = createBasicFilter("Travel related status", tsConstants$travelRelatedStatus, "travelClass"),
  "expContactPath" = createBasicFilter("Possible Exposure Source", sort(tsConstants$expContactPaths), "expContactPath"),
  "quarantBeforePositiveTest" = createBasicFilter("Was patient in quarantine before positive result?",
                                                  tsConstants$quarantBeforePositiveTest,"quarantBeforePositiveTest"),
  "labReason" = createBasicFilter("Reason for the test", tsConstants$labReason, "labReason")
)


tsUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    fluidPage(
      fluidRow(
        column(3,
          bootstrapPanel(
            heading = "Parameter", class = "panel-primary",
            radioButtons(
              inputId = ns("event"),
              label = "Clinical event",
              choices = tsConstants$events,
              selected = tsConstants$events[1], inline = TRUE
            ),
            checkboxInput(
              inputId = ns("display_prob"),
              label = tags$b("Clinical event probability given:")
            ),
            disabled(radioButtons(
              inputId = ns("given"),
              label = NULL,
              choices = tsConstants$events,
              selected = tsConstants$events[1], inline = TRUE
            ))
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("filter"), "Filter/stratify time series"),
            basicFilters %>% map2(names(.), function (f, n) { (f$ui(ns(n))) }),
          )
        ),
        column(9,
          bootstrapPanel(
            class = "panel-info", heading = "Basic time series",
            plotlyOutput(ns("mainPlot"), height = "600px"),
            tags$div(
              HTML("<span style='width: 50px; height: 12px; display: inline-block; background-color: #e7e7e7;'></span>
                  <small>The data from the recent 30 days might be incomplete and are subject to change.</small>")
            ),
            helpText("Data Source: Swiss Federal Office of Public Health", style = "text-align: right;"),
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("cog"), "Display options"),
            fluidRow(
              column(4,
                checkboxInput(
                  inputId = ns("log_scale"),
                  label = "Use log scale"
                ),
                checkboxInput(
                  inputId = ns("stack_histograms"),
                  label = "Stack histograms", value = TRUE
                ),
                checkboxInput(
                  inputId = ns("area_instead_of_line_plot"),
                  label = "Use an area chart instead of a line chart", value = TRUE
                )
              ),
              column(4,
                radioButtons(
                  inputId = ns("granularity"),
                  label = "Time granularity (histogram only)",
                  choices = tsConstants$granularityChoices,
                  selected = tsConstants$granularityChoices[1],
                  inline = TRUE
                )
              ),
              column(4,
                radioButtons(
                  inputId = ns("smoothing_window"),
                  label = "Sliding window average (probabilities only)",
                  choices = tsConstants$slidingWindowChoices,
                  selected = tsConstants$slidingWindowChoices[1],
                  inline = TRUE
                )
              )
            )
          )
        )
      )
    )
  )
}

tsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # TODO Why is it not required to put the id of the sub module into a ns() function?
      basicFilterServers <- basicFilters %>% map2(names(.), function (f, n) { f$server(n) })

      ### Basic UI Logic: Enabling/disabling/setting filters etc. ###

      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      observeEvent(input$display_prob, {
        shinyjs::toggleState(id = "given", condition = input$display_prob)
      })

      # If one compare checkbox is checked, the others should be unchecked.
      lapply(  # TODO Why does a for-loop not work? What's the difference to lapply()?
        X = basicFilterServers,
        FUN = function(fs){
          observeEvent(fs()$compare, {
            if (fs()$compare) {
              for (fs2 in basicFilterServers) {
                if (fs2()$attributeName != fs()$attributeName) {
                  updateCheckboxInput(fs2()$session, "compare", value = FALSE)
                }
              }
            }
          })
        }
      )


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

      plot_type <- reactive({
        if (!input$display_prob && !compare_proportions()) {
          return ("discrete")
        } else {
          return ("continuous")
        }
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

      # Exclude all data before start of stratified negative test records
      # TODO Describe the background of this step and maybe remove code reduandancy within the function.
      stratifiedTestRecordFiltered <- reactive({
        if ((input$event == "Test (any result)") || (input$display_prob && input$given == "Test (any result)")) {
          if (!is.na(compare())) {
            stratifiedTestingStart <- min((data() %>% filter(positiveTest == FALSE, !is.na(canton)))$fall_dt)
            return (data() %>% filter(fall_dt >= stratifiedTestingStart))
          }
          for (fs in basicFilterServers) {
            if (fs()$isFiltering) {
              stratifiedTestingStart <- min((data() %>% filter(positiveTest == FALSE, !is.na(canton)))$fall_dt)
              return (data() %>% filter(fall_dt >= stratifiedTestingStart))
            }
          }
        }
        return (data())
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
        shinyjs::toggleState(id = "area_instead_of_line_plot", condition = compare_proportions())

        if (input$log_scale && input$stack_histograms) {
          updateCheckboxInput(session, "stack_histograms", value = FALSE)
        }
      })


      ### Putting everything together ###
      output$mainPlot <- renderPlotly({
        validators()

        # Apply basic filters
        dataProc <- multiIntersect(
          data(),
          stratifiedTestRecordFiltered()
        )
        for (fs in basicFilterServers) {
          dataProc <- fs()$filter(dataProc)
        }

        validate(need(
          nrow(dataProc) > 0,
          "No data matches the requested combination of filters."
        ))

        # General transformations and calculations for later use
        if (input$display_prob) {
          xlabel <- "Date of Test"
        } else {
          xlabel <- paste("Date of", input$event)
        }
        smoothing_interval <- tsConstants$slidingWindowChoicesToIntervals[[input$smoothing_window]]
        minDate <- min((dataProc %>% drop_na(date))$date)
        maxDate <- max((dataProc %>% drop_na(date))$date)

        # Case 1: showing the total frequencies
        if (!input$display_prob && is.na(compare())) {
          dataProc <- dataProc %>%
            intersect(clinicalEventFiltered()) %>%
            dateRoundingProcessor()() %>%
            group_by(date) %>%
            summarize(count = sum(mult), .groups = "drop")

          # Define the ggplot
          p <- ggplot(dataProc) +
            geom_histogram(aes(x = date, y = count), stat = "identity") +
            ylab("Total count")
        }

        # Case 2: comparing the frequencies
        if (!input$display_prob && !is.na(compare())) {
          dataProc <- dataProc %>%
            intersect(clinicalEventFiltered())
          if (!compare_proportions()) {
            dataProc <- dataProc %>% dateRoundingProcessor()()
          }
          plot_data <- NULL
          for (compare_val in unique(dataProc[[compare()]])) {
            d <- dataProc %>%
              filter(!!as.symbol(compare()) == compare_val) %>%
              group_by(date) %>%
              summarize(count = sum(mult), .groups = "drop")
            if (compare_proportions()) {
              d <- d %>%
                complete(date = seq.Date(minDate, maxDate, by = "day")) %>%
                mutate(count = replace_na(count, 0)) %>%
                drop_na(date)
              d$smoothedCount <- slide_index_dbl(d$count, d$date, sum, .before = smoothing_interval)
            }
            d[, compare()] <- compare_val
            plot_data <- bind_rows(plot_data, d)
          }
          if (!compare_proportions()) {
            p <- ggplot(plot_data, aes(x = date, y = count, fill = !!as.symbol(compare()))) +
              geom_histogram(stat = "identity", position = (if (input$stack_histograms) "stack" else "dodge")) +
              ylab("Total count")

          } else {
            plot_data <- plot_data %>%
              group_by(date) %>%
              mutate(proportion = smoothedCount / sum(smoothedCount))
            p <- ggplot(plot_data, aes(x = date, y = proportion, col = !!as.symbol(compare()),
                                       fill = !!as.symbol(compare()))) +
              (if (!input$area_instead_of_line_plot) geom_line() else geom_area()) +
              scale_x_date(date_breaks = "months") +
              xlab(paste("Date of", input$event)) +
              ylab(paste0("Proportion of ", input$event, "s"))
          }
        }

        # Case 3: looking at the probabilities when a type of event is given
        if (input$display_prob && is.na(compare())) {
          denominatorData <- intersect(dataProc, givenClinicalEventFiltered())
          numeratorData <- intersect(denominatorData, clinicalEventFiltered())

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
          p <- ggplot(plotData, aes(x = date, y = prob)) +
            geom_line() +
            ylab(paste0("Fraction of ", input$given, "s involving ", input$event))
        }

        # Case 4: comparing the probabilities
        if (input$display_prob && !is.na(compare())) {
          denominatorData <- intersect(dataProc, givenClinicalEventFiltered())
          numeratorData <- intersect(denominatorData, clinicalEventFiltered())

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

          plot_data <- NULL
          for (compare_val in unique(dataProc[[compare()]])) {
            d <- dataProc %>% filter(!!as.symbol(compare()) == compare_val)
            dDenom <- intersect(d, denominatorData)
            dNum <- intersect(d, numeratorData)

            dDenom <- processDataInternal(dDenom)
            dNum <- processDataInternal(dNum)
            denom <- slide_index_dbl(dDenom$count, dDenom$date, sum, .before = smoothing_interval)
            num <- slide_index_dbl(dNum$count, dNum$date, sum, .before = smoothing_interval)

            d <- tibble(date = dDenom$date, prob = num / denom)
            d[, compare()] <- compare_val
            plot_data <- bind_rows(plot_data, d)
          }

          if (!compare_proportions()) {
            p <- ggplot(plot_data, aes(x = date, y = prob, col = !!as.symbol(compare()))) +
              geom_line() +
              ylab(paste0("Fraction of ", input$given, "s involving ", input$event))
          } else {
            plot_data <- plot_data %>%
              mutate(prob = replace_na(prob, 0)) %>%
              group_by(date) %>%
              mutate(proportions = prob / sum(prob))

            p <- ggplot(plot_data, aes(x = date, y = proportions, col = !!as.symbol(compare()),
                                       fill = !!as.symbol(compare()))) +
              (if (!input$area_instead_of_line_plot) geom_line() else geom_area()) +
              ylab(paste0("Fraction of ", input$given, "s involving ", input$event))
          }
        }

        # Final plot configurations
        p <- p +
          xlab(xlabel) +
          scale_x_date(date_breaks = "months", labels = date_format("%m-%Y")) +
          theme_light()
        if (input$log_scale) {
          p <- p + scale_y_log10()
        }

        # Draw the plot
        plotlyPlot <- ggplotly(p) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("toImage", "resetScale2d")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )

        # Give the data from the recent 30 days a gray background to mark them as uncertain.
        # Annotating in ggplot2 did not work as it was not transferred. Calling the layout() of plotly also failed
        # (see https://stackoverflow.com/a/50361382). Therefore, this solution:
        todayDaysSince1970 <- as.integer(as.POSIXct(Sys.Date())) / 60 / 60 / 24
        plotlyPlot[['x']][['layout']][['shapes']] <- list(
          list(type = "rect",
               fillcolor = "grey", line = list(color = "gray"), opacity = 0.2,
               # Inf and -Inf don't work here.
               x0 = todayDaysSince1970 - 30, x1 = todayDaysSince1970 + 100, xref = "x",
               y0 = -99999999, y1 = 99999999, yref = "y")
        )

        plotlyPlot
      })
    }
  )
}

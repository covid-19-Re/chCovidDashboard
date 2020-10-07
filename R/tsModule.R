library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)


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
              choices = events,
              selected = events[1], inline = TRUE
            ),
            checkboxInput(
              inputId = ns("display_prob"),
              label = tags$b("Clinical event probability given:")
            ),
            disabled(radioButtons(
              inputId = ns("given"),
              label = NULL,
              choices = events,
              selected = events[1], inline = TRUE
            ))
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("filter"), "Filter/stratify time series"),
            pickerInput(
              inputId = ns("age_groups"),
              label = "Age groups",
              choices = ageGroups,
              selected = ageGroups,
              multiple = TRUE,
              options = list(size = 8)
            ),
            actionGroupButtons(
              inputIds = c(ns("all_ages"), ns("clear_ages")),
              labels = c("Select all", "Clear"),
              size = "xs"
            ),
            checkboxInput(inputId = ns("compare_ages"), label = "Compare ages"),
            checkboxInput(inputId = ns("compare_ages_proportions"), label = "Show proportions"),
            pickerInput(
              inputId = ns("cantons"),
              label = "Cantons",
              choices = cantons,
              selected = cantons,
              multiple = TRUE,
              options = list(size = 8)
            ),
            actionGroupButtons(
              inputIds = c(ns("all_cantons"), ns("clear_cantons")),
              labels = c("Select all", "Clear"),
              size = "xs"
            ),
            checkboxInput(
              inputId = ns("compare_cantons"),
              label = "Compare cantons"
            ),
            checkboxInput(
              inputId = ns("compare_cantons_proportions"),
              label = "Show proportions"
            ),
            radioButtons(
              inputId = ns("travel"),
              label = "Import status",
              choices = travelChoices,
              selected = travelChoices[1],
              inline = TRUE
            ),
            checkboxInput(
              inputId = ns("compare_travel"),
              label = "Compare import statuses"
            ),
            checkboxInput(
              inputId = ns("compare_travel_proportions"),
              label = "Show proportions"
            ),
          )
        ),
        column(9,
          bootstrapPanel(
            class = "panel-info", heading = "Basic time series",
            plotlyOutput(ns("mainPlot"), height = "600px"),
            helpText("Data Source: Swiss Federal Office of Public Health", style = "text-align: right;")
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
                )
              ),
              column(4,
                radioButtons(
                  inputId = ns("granularity"),
                  label = "Histogram time granularity",
                  choices = granularityChoices,
                  selected = granularityChoices[1],
                  inline = TRUE
                )
              ),
              column(4,
                radioButtons(
                  inputId = ns("smoothing_window"),
                  label = "Sliding window average (probabilities only)",
                  choices = slidingWindowChoices,
                  selected = slidingWindowChoices[1],
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
      
      ### Basic UI Logic: Enabling/disabling/setting filters etc. ###
      
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      observeEvent(input$all_ages, {
        updatePickerInput(session,
          inputId = "age_groups",
          selected = ageGroups
        )
      })

      observeEvent(input$clear_ages, {
        updatePickerInput(session,
          inputId = "age_groups",
          selected = character(0)
        )
      })

      observeEvent(input$all_cantons, {
        updatePickerInput(session,
          inputId = "cantons",
          selected = cantons
        )
      })

      observeEvent(input$clear_cantons, {
        updatePickerInput(session,
          inputId = "cantons",
          selected = character(0)
        )
      })

      observeEvent(input$display_prob, {
        shinyjs::toggleState(id = "given", condition = input$display_prob)
      })

      observeEvent(input$compare_ages, {
        if (input$compare_ages) {
          updateCheckboxInput(session, "compare_cantons", value = FALSE)
          updateCheckboxInput(session, "compare_travel", value = FALSE)
        }
        shinyjs::toggleState(id = "compare_ages_proportions", condition = input$compare_ages)
      })

      observeEvent(input$compare_cantons, {
        if (input$compare_cantons) {
          updateCheckboxInput(session, "compare_ages", value = FALSE)
          updateCheckboxInput(session, "compare_travel", value = FALSE)
        }
        shinyjs::toggleState(id = "compare_cantons_proportions", condition = input$compare_cantons)
      })

      observeEvent(input$compare_travel, {
        if (input$compare_travel) {
          updateCheckboxInput(session, "compare_ages", value = FALSE)
          updateCheckboxInput(session, "compare_cantons", value = FALSE)
        }
        shinyjs::toggleState(id = "compare_travel_proportions", condition = input$compare_travel)
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
          d <- d %>% mutate(date = !!as.symbol(eventDateCols[[input$event]]))
        }

        return(d)
      })

      compare <- reactive({
        if (input$compare_ages == TRUE) {
          return("Age group")
        } else if (input$compare_cantons == TRUE) {
          return("Canton")
        } else if (input$compare_travel == TRUE) {
          return("Import status")
        }
        return(NA)
      })
      
      compare_proportions <- reactive({
        return (
          (input$compare_ages && input$compare_ages_proportions) ||
          (input$compare_cantons && input$compare_cantons_proportions) ||
          (input$compare_travel && input$compare_travel_proportions)
        )
      })
      
      plot_type <- reactive({
        if (!input$display_prob && !compare_proportions()) {
          return ("discrete")
        } else {
          return ("continuous")
        }
      })
      
      # Validators
      
      nonEmptyFiltersValidator <- reactive({
        validate(
          need(
            !is.null(input$age_groups),
            "Must specify at least one age group."
          ),
          need(
            !is.null(input$cantons),
            "Must specify at least one canton."
          )
        )
      })
      
      notFalselyUsingNegativeTestDataValidator <- reactive({
        if ((input$event == "Test (any result)") || (input$display_prob && input$given == "Test (any result)")) {
          validate(
            need(
              input$travel == "All cases" && (is.na(compare()) || compare() != "Import status"),
              "Currently lacking information about travel status for negative test data."
            )
          )
        }
      })
      
      validators <- reactive({
        notFalselyUsingNegativeTestDataValidator()
        nonEmptyFiltersValidator()
      })
      
      # Filters
      
      ageGroupFiltered <- reactive({
        if (length(input$age_groups) < length(ageGroups)) {
          return (data() %>% filter(ageGroup %in% input$age_groups))
        }
        return (data())
      })
      
      cantonFiltered <- reactive({
        if (length(input$cantons) < length(cantons)) {
          return (data() %>% filter(canton %in% input$cantons))
        }
        return (data())
      })
      
      travelClassFiltered <- reactive({
        if (input$travel != "All cases") {
          return (data() %>% filter(travelClass == input$travel))
        } else {
          return (data())
        }
      })
      
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
      stratifiedTestRecordFiltered <- reactive({
        if ((input$event == "Test (any result)") || (input$display_prob && input$given == "Test (any result)")) {
          if (!is.na(compare()) || length(input$age_groups) != length(ageGroups)
              || length(input$cantons) != length(cantons)) {
            stratifiedTestingStart <- min((data() %>% filter(positiveTest == FALSE, !is.na(canton)))$fall_dt)
            return (data() %>% filter(fall_dt >= stratifiedTestingStart))
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
              Weeks = data %>% mutate(date = round_date(date, unit = "week")),
              Months = data %>% mutate(date = round_date(date, unit = "month"))
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
        dataProc <- multiIntersect(
          data(),
          ageGroupFiltered(),
          cantonFiltered(),
          travelClassFiltered(),
          stratifiedTestRecordFiltered()
        )
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
        smoothing_interval <- switch(input$smoothing_window,
          None = days(0),
          "7 days" = days(7),
          "14 days" = days(14),
          "28 days" = days(28)
        )
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
            geom_histogram(aes(x = date, y = count), stat = "identity")
          p <- p + ylab("Total count")
        }
        
        # Case 2: comparing the frequencies
        if (!input$display_prob && !is.na(compare())) {
          dataProc <- dataProc %>%
            intersect(clinicalEventFiltered()) %>%
            dateRoundingProcessor()()
          
          plot_data <- NULL
          for (compare_val in unique(dataProc[[categoryCols[[compare()]]]])) {
            d <- dataProc %>%
              filter(!!as.symbol(categoryCols[[compare()]]) == compare_val) %>%
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
            p <- ggplot(plot_data, aes(x = date, y = count, fill = !!as.symbol(compare())))
            if (input$stack_histograms) {
              p <- p + geom_histogram(stat = "identity")
            } else {
              p <- p + geom_histogram(stat = "identity", position = "dodge")
            }
            p <- p + ylab("Total count")

          } else {
            plot_data <- plot_data %>%
              group_by(date) %>%
              mutate(proportion = smoothedCount / sum(smoothedCount))
            p <- ggplot(plot_data, aes(x = date, y = proportion, col = !!as.symbol(compare()))) +
              geom_line() +
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
          for (compare_val in unique(dataProc[[categoryCols[[compare()]]]])) {
            d <- dataProc %>% filter(!!as.symbol(categoryCols[[compare()]]) == compare_val)
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
            
            p <- ggplot(plot_data, aes(x = date, y = proportions, col = !!as.symbol(compare()))) +
              geom_line() +
              ylab(paste0("Fraction of ", input$given, "s involving ", input$event))
          }
        }
        
        # Final plot configurations
        p <- p + xlab(xlabel) + scale_x_date(date_breaks = "months")
        if (input$log_scale) {
          p <- p + scale_y_log10()
        }
        
        # Draw the plot
        ggplotly(
          p
        ) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("toImage")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )
      })
    }
  )
}

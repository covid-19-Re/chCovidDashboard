library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)

tsCasesUI <- function(id) {
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
            )
          )
        ),
        column(9,
          bootstrapPanel(
            class = "panel-info", heading = "Basic time series",
            plotlyOutput(ns("mainPlot"), height = "600px")
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

tsCasesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
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
        if (input$display_prob) {
          shinyjs::enable("given")
        } else {
          shinyjs::disable("given")
        }
      })

      observeEvent(input$compare_ages, {
        if (input$compare_ages) {
          updateCheckboxInput(session, "compare_cantons", value = FALSE)
          updateCheckboxInput(session, "compare_travel", value = FALSE)
        }
      })

      observeEvent(input$compare_cantons, {
        if (input$compare_cantons) {
          updateCheckboxInput(session, "compare_ages", value = FALSE)
          updateCheckboxInput(session, "compare_travel", value = FALSE)
        }
      })

      observeEvent(input$compare_travel, {
        if (input$compare_travel) {
          updateCheckboxInput(session, "compare_ages", value = FALSE)
          updateCheckboxInput(session, "compare_cantons", value = FALSE)
        }
      })

      observeEvent(input$log_scale, {
        if (input$log_scale) {
          shinyjs::disable("stack_histograms")

          if (input$stack_histograms) {
            updateCheckboxInput(session, "stack_histograms", value = FALSE)
          }
        } else {
          shinyjs::enable("stack_histograms")
        }
      })

      data <- reactive({
        return(load_and_process_data())
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

      output$mainPlot <- renderPlotly({
        ggplotly(
          generate_timeseries_plot(
            data(),
            input$event,
            input$given,
            input$age_groups,
            input$cantons,
            input$travel,
            input$granularity,
            input$smoothing_window,
            compare = compare(),
            displayProb = input$display_prob,
            logScale = input$log_scale,
            stackHistograms = input$stack_histograms
          )
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

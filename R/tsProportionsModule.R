library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)

tsProportionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidPage(
      fluidRow(
        column(3,
          bootstrapPanel(heading = "Parameter", class = "panel-primary",
            radioButtons(inputId = ns("proportion_event"),
              label = "Clinical event",
              choices = events, inline = TRUE),
            radioButtons(inputId = ns("proportion_category"),
              label = "Relative frequency per:",
              choices = categoryNames)
          )
        ),
        column(9,
          bootstrapPanel(class = "panel-info", heading = "Basic time series",
            plotlyOutput(ns("mainPlot"), height = "600px")
          ),
          bootstrapPanel(class = "panel-primary",
            heading = tagList(icon("cog"), "Display options"),
            fluidRow(
              column(4,
                checkboxInput(inputId = ns("log_scale"),
                  label = "Use log scale"),
                checkboxInput(inputId = ns("stack_histograms"),
                  label = "Stack histograms", value = TRUE)
              ),
              column(4,
                radioButtons(inputId = ns("granularity"),
                  label = "Histogram time granularity",
                  choices = granularityChoices,
                  selected = granularityChoices[1],
                  inline = TRUE)
              ),
              column(4,
                radioButtons(inputId = ns("smoothing_window"),
                  label = "Sliding window average (probabilities only)",
                  choices = slidingWindowChoices,
                  selected = slidingWindowChoices[1],
                  inline = TRUE)
              )
            )
          )
        )
      )
    )
  )
}

tsProportionsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
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

      output$mainPlot <- renderPlotly({
        ggplotly(
          generate_proportion_plot(
              data(),
              input$proportion_event,
              input$proportion_category,
              input$smoothing_window,
              logScale = input$log_scale)) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("toImage")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )
      })
    }
  )
}

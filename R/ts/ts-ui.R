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

                 # Plot types
                  actionButton(ns("plotTypeHistogram"), "Histogram", icon = icon("chart-bar")),
                 actionButton(ns("plotTypeLine"), "Line Chart", icon = icon("chart-line")),
                 actionButton(ns("plotTypeArea"), "Area Chart", icon = icon("chart-area")),
                 actionButton(ns("plotTypeMap"), "Map", icon = icon("map")),

                 tags$div(
                   id = ns("map_slider"),
                   class = "ts-hidden",
                   style = "padding-left: 25px; padding-right: 25px;",
                   sliderInput(inputId = ns("map_selected_day"), "Date:", min = as.Date("2020-03-01"), max = today(),
                               value = today() %m-% days(7), width = "100%")
                 ),

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
               ),
               bootstrapPanel(
                 heading = "Normalization", class = "panel-primary",
                 checkboxInput(
                   inputId = ns("normalization"),
                   label = "Activate normalization"
                 ),
                 tags$div(
                   HTML("The normalization calculates the positive case numbers if the hospitalisation rate is
                   assumed to be constant. It aims to improve the comparability of the numbers between different
                   months. <b>If this field is activated, the shown plot does not present the actual numbers.</b>")
                 ),
                 HTML("<div style='font-weight: bolder; font-size: 1.2em; color: #008cba; margin-right: 10px;
                 margin-top: 20px; margin-bottom: 20px;'>Assumption:</div>"),
                 tags$div(
                   style = "line-height: 3;",
                   "The hospitalization rate per age group is constant and equals the value in",
                   tags$div(
                     style = "display: inline-block; vertical-align:top; width: 250px;",
                     pickerInput(
                       inputId = ns("normalization_timerange"),
                       choices = tsConstants$normalizationTimerangeOptions,
                       selected = c(ymd('2020-03-01'), ymd('2020-04-01'), ymd('2020-05-01')),
                       multiple = TRUE
                     )
                   ),
                   "."
                 )
               )
        )
      )
    )
  )
}
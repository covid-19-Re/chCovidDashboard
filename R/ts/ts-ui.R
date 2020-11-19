tsUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    fluidPage(
      fluidRow(
        column(
          3,
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
              label = tagList(
                tags$b("Clinical event probability given:"),
                tooltip("If selected, the plot shows the estimated probability of the above selected event
                occuring given the below selected event. This can, for example, be used to calculate the
                hospitalization rate (Hospitalization given Positive test).")
              )
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
            basicFilters %>% map2(names(.), function(f, n) { (f$ui(ns(n))) }),
          )
        ),
        column(
          9,
          bootstrapPanel(
            class = "panel-info", heading = "Time series",

            # Plot types
            actionButton(ns("plotTypeHistogram"), "Histogram", icon = icon("chart-bar")),
            actionButton(ns("plotTypeLine"), "Line Chart", icon = icon("chart-line")),
            actionButton(ns("plotTypeArea"), "Area Chart", icon = icon("chart-area")),
            actionButton(ns("plotTypeMap"), "Map", icon = icon("map"), NULL,
                         tooltip('Select the "compare" option for canton or exposure country to use the map.')),

            tags$div(
              id = ns("map_slider"),
              class = "ts-hidden",
              style = "padding-left: 25px; padding-right: 25px;",
              uiOutput(ns("map_selected_day_output"))
            ),

            plotlyOutput(ns("mainPlot"), height = "600px"),
            tags$div(
              HTML("<small>The number of positive tests from the recent 2 days and the number of hospitalization,
                   ICU admissions and deaths from the recent 5 days might be incomplete due to reporting delays.</small>")
            ),
            helpText(textOutput(ns("dataLastUpdatedAt")), style = "text-align: right;"),
            helpText("Data Source: Swiss Federal Office of Public Health", style = "text-align: right;"),
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("cog"), "Display options"),
            fluidRow(
              column(
                4,
                checkboxInput(
                  inputId = ns("log_scale"),
                  label = "Use log scale"
                ),
                checkboxInput(
                  inputId = ns("stack_histograms"),
                  label = "Stack histograms", value = TRUE
                ),
                checkboxInput(
                  inputId = ns("show_confidence_interval"),
                  label = tagList(
                    "Show confidence interval",
                    tooltip('This option is available for probabilities (i.e., "Clinical event probability given"
                    is selected in the top-left panel). It shows the 95% confidence interval')
                  ),
                  value = TRUE
                )
              ),
              column(
                4,
                radioButtons(
                  inputId = ns("granularity"),
                  label = tagList(
                    "Time granularity",
                    tooltip("This option is available for histograms and maps.")
                  ),
                  choices = tsConstants$granularityChoices,
                  selected = tsConstants$granularityChoices[1],
                  inline = TRUE
                )
              ),
              column(
                4,
                radioButtons(
                  inputId = ns("smoothing_window"),
                  label = tagList(
                    "Sliding window average",
                    tooltip("This option is available for line and area plots. In case of absolute numbers, it
                    calculates the average over the selected window of time. In case of probabilities, it shows the
                    estimated probability for within the selected time window. The sliding window takes the data of
                    the days before the selected date (i.e., a 7-days sliding window for today would show the data
                    from the past 6 days and today).")
                  ),
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
                  selected = ymd('2020-07-01'),
                  multiple = TRUE
                )
              ),
              "."
            ),
            HTML("<div style='font-weight: bolder; font-size: 1.2em; color: #008cba; margin-right: 10px;
                 margin-top: 20px; margin-bottom: 20px;'>Method:</div>"),
            tags$p(
              "Due to changing testing capacities and regimes, it is difficult to compare the numbers of different
              points in time. The normalization mode assumes that the true hospitalization rate per age group did not
              change and calculates the normalized number of positive cases using the following formula:"
            ),
            tags$p(withMathJax(
              "$$\\#PositiveCases = \\sum_{i\\in \\{AgeGroups\\}}\\#Hospitalizations_i \\times
              \\frac{\\#HospitalizaionsInSelectedMonths_i}{\\#PositiveTestsInSelectedMonths_i} $$
            "))
          )
        )
      )
    )
  )
}

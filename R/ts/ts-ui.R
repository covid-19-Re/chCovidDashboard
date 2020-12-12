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
              choices = ts_constants$events,
              selected = ts_constants$events[1], inline = TRUE
            ),
            checkboxInput(
              inputId = ns("display_prob"),
              label = tagList(
                tags$b("Clinical event probability given:"),
                tooltip("If selected, the plot shows the estimated probability of the above selected event
                occuring given the below selected event. This can, for example, be used to calculate the
                hospitalisation rate (Hospitalisation given Positive test).")
              )
            ),
            disabled(radioButtons(
              inputId = ns("given"),
              label = NULL,
              choices = ts_constants$events,
              selected = ts_constants$events[1], inline = TRUE
            ))
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("filter"), "Filter"),
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

            actionButton(ns("resetAll"), "Reset all filters",
                         style = "background: none; border: none; outline: none; text-decoration: underline;"),

            tags$div(
              id = ns("map_slider"),
              class = "ts-hidden",
              style = "padding-left: 25px; padding-right: 25px;",
              uiOutput(ns("map_selected_day_output"))
            ),

            plotlyOutput(ns("mainPlot"), height = "600px"),
            tags$div(
              HTML("<small>
              <span style='width: 10px;background: lightgray;height: 10px;display: inline-block;
              margin-right: 5px;'></span>
              The number of positive tests from the recent 2 days and the number of hospitalisation and deaths from the
              recent 5 days might be incomplete due to reporting delays.
              </small>")
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
                  choices = ts_constants$granularityChoices,
                  selected = ts_constants$granularityChoices[1],
                  inline = TRUE
                )
              ),
              column(
                4,
                disabled(radioButtons(
                  inputId = ns("smoothing_window"),
                  label = tagList(
                    "Sliding window average",
                    tooltip("This option is available for line and area plots. In case of absolute numbers, it
                    calculates the average over the selected window of time. In case of probabilities, it shows the
                    estimated probability for within the selected time window. The sliding window takes the data of
                    the days before the selected date (i.e., a 7-days sliding window for today would show the data
                    from the past 6 days and today).")
                  ),
                  choices = ts_constants$slidingWindowChoices,
                  selected = ts_constants$slidingWindowChoices[1],
                  inline = TRUE
                ))
              )
            )
          ),
          bootstrapPanel(
            heading = "Normalisation", class = "panel-primary",
            checkboxInput(
              inputId = ns("normalization"),
              label = "Activate normalisation"
            ),
            tags$div(
              HTML("The normalisation calculates the positive case numbers if the hospitalisation rate per age group is
                   assumed to be constant. It aims to improve the comparability of the numbers between different
                   months. <b>If this field is activated, the shown plot does not present the actual numbers.</b>")
            ),
            HTML("<div style='font-weight: bolder; font-size: 1.2em; color: #008cba; margin-right: 10px;
                 margin-top: 20px; margin-bottom: 20px;'>Assumption:</div>"),
            tags$div(
              style = "line-height: 3;",
              "The hospitalisation rate per age group is constant and equals the value in",
              tags$div(
                style = "display: inline-block; vertical-align:top; width: 250px;",
                pickerInput(
                  inputId = ns("normalization_timerange"),
                  choices = ts_constants$normalizationTimerangeOptions,
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
              points in time. The normalisation mode assumes that the true hospitalisation rate per age group did not
              change and calculates the normalized number of positive cases using the following formula:"
            ),
            tags$p(withMathJax(
              "$$\\#PositiveCases = \\sum_{i\\in \\{AgeGroups\\}}\\#Hospitalisations_i \\times
              \\frac{\\#PositiveTestsInSelectedMonths_i}{\\#HospitalisaionsInSelectedMonths_i} $$
            "))
          ),
          bootstrapPanel(
            heading = "Frequently Asked Questions (FAQ)", class = "panel-primary",
            tags$h4('What does the dashboard show?'),
            tags$p(
              "The dashboard presents three types of information:"
            ),
            tags$ul(
              tags$li(
                'In the default case - when neither "Clinical event probability given" nor the normalisation is
                selected - ', tags$b('absolute numbers'), ' are displayed. Depending on the selected clinical event that
                is selected in the top-left panel of the website, the plot shows the number of positive tests taken on
                a particular day, the number of patients hospatalised or died on a day, or the total number of tested
                persons on a day.'
              ),
              tags$li(
                'When "Clinical event probability given" is checked, ', tags$b('estimated probabilities or relative
                numbers'), ' are shown. For example, if "Hospitalisation given Positive test" is chosen, the dashboard
                plots the hospitalisation rate of patients who were positively tested on a particular day. In other
                words, the number of hospitalised persons who had taken their test on a day is divided by the number
                of all positive tests on that day.'
              ),
              tags$li(
                'When "Activate normalisation" is selected, the plot does not show the true number of cases but the ',
                tags$b('theoretical number of cases'), ' under the (strong) assumption that the hospitalisation rate per
                age group did not change throughout the pandemic. More information can be found in the above box.'
              )
            ),
            tags$br(),
            tags$h4('Are the hospitalisations and deaths all because of COVID-19?'),
            tags$p('No, that is not the case. The plots show the number of patients who were tested positive with
             SARS-CoV-2 and were hospitalised or died. This does not mean that the hospitalisations or deaths are
             actually caused by COVID-19. Some patients were admitted to the hospital long before they were
             tested positive.'),
            tags$br(),
            tags$h4('How are the confidence intervals calculated?'),
            tags$p('Let\'s take the hospitalisation rate ("Hospitalisation given Positive test") as an example.'),
            tags$p('If the "Clinical event probability given" option is selected, the shown numbers can be interpreted
            as probabilities. This means that we get the estimated probability of a person being hospitalised when the
            person was tested positively on a specific day. The certainly of the estimation depends on the sample size:
            when many people had a positive test on a day, the certainty is higher.'),
            tags$p(
              'The ', tags$a('binomial distribution', target = '_blank',
                             href = 'https://en.wikipedia.org/wiki/Binomial_distribution#Confidence_intervals'),
              ' is used as the underlying model. We set the "number of trials" ', tags$i('n'), ' = number of positive
              cases, "the number of successes" ', tags$i('k'), ' = number of hospitalisations, and the probability ',
              tags$i('p = k/n'), '. The confidence interval is then calculated with the ',
              tags$a('Clopper-Pearson method', target = '_blank',
                     href = 'https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Clopper%E2%80%93Pearson_interval'),
              '.'
            ),
            tags$p('When a sliding window is selected, all hospitalisations and positive tests in the chosen time window
             are taken into account.')
          )
        )
      )
    )
  )
}

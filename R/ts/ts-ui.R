tsUI <- function(id) {
  return ("This dashboard is currently disabled.")

  ns <- NS(id)

  tagList(
    useShinyjs(),
    usei18n(ts_i18n),
    fluidPage(
      fluidRow(
        column(
          3,
          bootstrapPanel(
            heading = ts_i18n$t("ts.general.header"), class = "panel-primary",
            radioButtons(
              inputId = ns("event"),
              label = ts_i18n$t("ts.general.clinical_event"),
              choices = ts_constants$events,
              selected = ts_constants$events[1], inline = TRUE
            ),
            checkboxInput(
              inputId = ns("display_prob"),
              label = tagList(
                tags$b(ts_i18n$t("ts.general.given")), tags$b(":"),
                tooltip(ts_i18n$t("ts.general.given.tooltip"))
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
            heading = tagList(icon("filter"), ts_i18n$t("ts.filter.header")),
            basicFilters %>% map2(names(.), function(f, n) { (f$ui(ns(n))) }),
          )
        ),
        column(
          9,
          bootstrapPanel(
            class = "panel-info", heading = ts_i18n$t("ts.plot.header"),

            # Plot types
            actionButton(ns("plotTypeHistogram"), ts_i18n$t("ts.plot.histogram"), icon = icon("chart-bar")),
            actionButton(ns("plotTypeLine"), ts_i18n$t("ts.plot.line"), icon = icon("chart-line")),
            actionButton(ns("plotTypeArea"), ts_i18n$t("ts.plot.area"), icon = icon("chart-area")),
            actionButton(ns("plotTypeMap"), ts_i18n$t("ts.plot.map"), icon = icon("map"), NULL,
                         tooltip(ts_i18n$t("ts.plot.map.tooltip"))),

            actionButton(ns("resetAll"), ts_i18n$t("ts.plot.reset_all"),
                         style = "background: none; border: none; outline: none; text-decoration: underline;"),

            tags$div(
              id = ns("map_slider"),
              class = "ts-hidden",
              style = "padding-left: 25px; padding-right: 25px;",
              uiOutput(ns("map_selected_day_output"))
            ),

            plotlyOutput(ns("mainPlot"), height = "600px"),
            tags$div(
              HTML(paste0("<small>
              <span style='width: 10px;background: lightgray;height: 10px;display: inline-block;
              margin-right: 5px;'></span>", ts_i18n$t("ts.plot.uncertain_recent_data_node"), "</small>"))
            ),
            helpText(
              style = "text-align: right;",
              ts_i18n$t("ts.plot.data_last_update_note"),
              textOutput(ns("dataLastUpdatedAt"), inline = TRUE)
            ),
            helpText(ts_i18n$t("ts.plot.data_source"), style = "text-align: right;"),
          ),
          bootstrapPanel(
            class = "panel-primary",
            heading = tagList(icon("cog"), ts_i18n$t("ts.display.header")),
            fluidRow(
              column(
                4,
                checkboxInput(
                  inputId = ns("log_scale"),
                  label = ts_i18n$t("ts.display.log_scale")
                ),
                checkboxInput(
                  inputId = ns("stack_histograms"),
                  label = ts_i18n$t("ts.display.stack_histograms"), value = TRUE
                ),
                checkboxInput(
                  inputId = ns("show_confidence_interval"),
                  label = tagList(ts_i18n$t("ts.display.confidence_interval"),
                                  tooltip(ts_i18n$t("ts.display.confidence_interval.tooltip"))),
                  value = TRUE
                )
              ),
              column(
                4,
                radioButtons(
                  inputId = ns("granularity"),
                  label = tagList(ts_i18n$t("ts.display.granularity"),
                                  tooltip(ts_i18n$t("ts.display.granularity.tooltip"))),
                  choices = ts_constants$granularityChoices,
                  selected = ts_constants$granularityChoices[1],
                  inline = TRUE
                )
              ),
              column(
                4,
                disabled(radioButtons(
                  inputId = ns("smoothing_window"),
                  label = tagList(ts_i18n$t("ts.display.smoothing_window"),
                                  tooltip(ts_i18n$t("ts.display.smoothing_window.tooltip"))),
                  choices = ts_constants$slidingWindowChoices,
                  selected = ts_constants$slidingWindowChoices[1],
                  inline = TRUE
                ))
              )
            )
          ),
          bootstrapPanel(
            heading = ts_i18n$t("ts.normalization.header"), class = "panel-primary",
            checkboxInput(
              inputId = ns("normalization"),
              label = ts_i18n$t("ts.normalization.activate")
            ),
            tags$div(
              ts_i18n$t("ts.normalization.general_description1"),
              tags$b(ts_i18n$t("ts.normalization.general_description2"))
            ),
            tags$div(
              style = "font-weight: bolder; font-size: 1.2em; color: #008cba; margin-right: 10px; margin-top: 20px;
                margin-bottom: 20px;",
              ts_i18n$t("ts.normalization.assumption")
            ),
            tags$div(
              style = "line-height: 3;",
              ts_i18n$t("ts.normalization.assumption_text_part"),
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
            tags$div(
              style = "font-weight: bolder; font-size: 1.2em; color: #008cba; margin-right: 10px; margin-top: 20px;
                margin-bottom: 20px;",
              ts_i18n$t("ts.normalization.method")
            ),
            tags$p(ts_i18n$t("ts.normalization.method_text")),
            tags$p(withMathJax(
              "$$\\#PositiveCases = \\sum_{i\\in \\{AgeGroups\\}}\\#Hospitalisations_i \\times
              \\frac{\\#PositiveTestsInSelectedMonths_i}{\\#HospitalisaionsInSelectedMonths_i} $$
            "))
          ),
          bootstrapPanel(
            # TODO Don't fix the language, allow i18n.
            heading = ts_i18n$t("ts.faq.header"), class = "panel-primary ts-faq",
            HTML(markdown::markdownToHTML("data/ts-translations/faq.en.md", fragment.only = TRUE))
          )
        )
      )
    )
  )
}

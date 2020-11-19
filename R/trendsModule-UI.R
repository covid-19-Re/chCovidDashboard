library(shinycssloaders)

trendsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(10,
          bootstrapPanel(
            heading = HTML(
              "<h1>Analyzing epidemic trends of SARS-CoV-2 in Switzerland</h1><br>",
              "<i>Nanina Anderegg, Julien Riou, Christian L. Althaus (Institute of Social and Preventive Medicine, ",
              "Universität Bern)</i>"
            ),
            class = "panel-primary",
            p("This tool estimates national trends in daily confirmed cases, hospitalizations, ICU occupancy and ",
              "deaths using a negative binomial generalized linear model. The model uses reported numbers as the ",
              "response and date and weekend (0: work day, 1: weekend) as predictors. Confirmed cases and ",
              "hospitalizations are further stratified by canton and age groups. Due to reporting delays, ",
              "the last 3 and 5 days of confirmed cases and hospitalizations/deaths are removed, respectively. ",
              "Lines and ribbons show the maximum likelihood estimate of the exponential increase/decrease and ",
              "the 95% prediction intervals of the model fit, respectively."),
            uiOutput(ns("lastDataUpdate"))
          )
        ),
        column(2,
          bootstrapPanel(
            heading = "Parameter",
            class = "panel-primary",
            fluidRow(
              column(12,
                dateInput(ns("lastday"), "last day of data to include in analysis", value = today())
              ),
              column(12,
                numericInput(ns("time_window"), "size (days) of time window", value = 14, step = 1)
              ),
              column(12,
                selectizeInput(ns("plot_language"), "language for plots",
                  choices = c("en", "de"), selected = "de")
              )
            )
          )
        )
      ),
      tabsetPanel(
        type = "pills", id = "countryTabs",
        tabPanel(p(class = "tab-title", "Country-wide trends"), value = "country",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("countryPlots"), height = "800px") %>% withSpinner()
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "ageTabs",
        tabPanel(p(class = "tab-title", "Age Groups: Confirmed cases"), value = "cases",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("ageClassPlotsCases"), height = "800px") %>% withSpinner()
          ))
        ),
        tabPanel(p(class = "tab-title", "Age Groups: Hospitalisations"), value = "hospitalizations",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("ageClassPlotsHospitalizations"), height = "800px") %>% withSpinner()
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "cantonTabs",
        tabPanel(p(class = "tab-title", "Cantons: Confirmed cases"), value = "cases",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body",
            column(6,
              bootstrapPanel(
                heading = "Cantonal trends",
                class = "panel-info",
                plotOutput(ns("cantonPlotsCases"), height = "1200px") %>% withSpinner(),
                selectInput(ns("cantonSortCases"),
                  label = "sort order",
                  choices = c(
                    "alphabetical" = "alpha",
                    "weekly growth (ascending)" = "growthAsc", "weekly growth (descending)" = "growthDesc"
                  ),
                  selected = "growthDesc"
                )
              )
            ),
            column(6,
              bootstrapPanel(
                heading = "Ranking",
                class = "panel-info",
                plotOutput(ns("rankingPlotCases"), height = "800px") %>% withSpinner()
              )
            )
          ))
        ),
        tabPanel(p(class = "tab-title", "Cantons: Hospitalizations"), value = "hospitalizations",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body",
            column(6,
              bootstrapPanel(
                heading = "Cantonal trends",
                class = "panel-info",
                plotOutput(ns("cantonPlotsHospitalizations"), height = "1200px") %>% withSpinner(),
                selectInput(ns("cantonSortHospitalizations"),
                  label = "sort order",
                  choices = c(
                    "alphabetical" = "alpha",
                    "weekly growth (ascending)" = "growthAsc", "weekly growth (descending)" = "growthDesc"
                  ),
                  selected = "growthDesc"
                )
              )
            ),
            column(6,
              bootstrapPanel(
                heading = "Ranking",
                class = "panel-info",
                plotOutput(ns("rankingPlotHospitalizations"), height = "800px") %>% withSpinner()
              )
            )
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "summaryTabs",
        tabPanel(p(class = "tab-title", "Summary Table"), value = "summaryTable",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            HTML("<p>Doubling time, weekly change in doubling time from a negative binomial generalized linear model ",
            "and most recent R<sub>e</sub> values from ",
            "<a href='https://ibz-shiny.ethz.ch/covid-19-re/' target='blank'>https://ibz-shiny.ethz.ch/covid-19-re/</a>",
            "and corresponding doubling times assuming a gamma distributed generation time with &mu; = 4.8 days ",
            "and &sigma; = 2.3 days.</p>",
            "<p>Use filter fields to filter columns. Click on column names to sort. Shift-Click to sort by multiple columns.</p>"),
            DT::dataTableOutput(ns("comparisonDataTable")) %>% withSpinner(),
            downloadButton(ns("downloadData"), "Download .csv")
          ))
        )
      )
    )
  )
}

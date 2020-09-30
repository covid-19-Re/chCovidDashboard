library(shinycssloaders)

trendsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(8,
          bootstrapPanel(
            heading = HTML(
              "<h1>Analyzing epidemic trends of SARS-CoV-2 in Switzerland</h1><br>",
              "<i>Nanina Anderegg, Julien Riou, Christian L. Althaus (ISPM, Universität Bern)</i>"
            ),
            class = "panel-primary",
            "some text, maybe?",
            uiOutput(ns("lastDataUpdate"))
          )
        ),
        column(4,
          bootstrapPanel(
            heading = "Parameter",
            class = "panel-primary",
            fluidRow(
              column(7,
                dateInput(ns("lastday"), "last day of data to include in analysis", value = today())
              ),
              column(5,
                numericInput(ns("truncation"), "truncation (days)", value = 4)
              )
            ),
            fluidRow(
              column(7,
                numericInput(ns("time_window"), "size (days) of time window", value = 28, step = 1)
              )
            ),
          )
        )
      ),
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = "Country-wide trends",
            class = "panel-info",
            fluidRow(
              column(4, plotOutput(ns("chPlotCases")) %>% withSpinner()),
              column(4, plotOutput(ns("chPlotHospitalizations")) %>% withSpinner()),
              column(4, plotOutput(ns("chPlotDeaths")) %>% withSpinner())
            )
          )
        )
      ),
      fluidRow(
        column(6,
          bootstrapPanel(
            heading = "Cantonal trends",
            class = "panel-info",
            plotOutput(ns("cantonPlots"), height = "1200px") %>% withSpinner(),
            selectInput(ns("cantonSort"), label = "sort order",
              choices = c("alphabetical" = "alpha",
                "weekly growth (ascending)" = "growthAsc", "weekly growth (descending)" = "growthDesc"),
            selected = "growthDesc")
          )
        ),
        column(6,
          bootstrapPanel(
            heading = "Ranking",
            class = "panel-info",
            plotOutput(ns("rankingPlot"), height = "800px") %>% withSpinner()
          )
        )
      )
    )
  )
}

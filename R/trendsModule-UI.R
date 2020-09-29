library(shinycssloaders)

trendsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Analyzing epidemic trends of SARS-CoV-2 in Switzerland</h1><br>",
              "<i>Nanina Anderegg, Julien Riou, Christian L. Althaus (ISPM, Universität Bern)</i>"
            ),
            class = "panel-primary",
            "some text, maybe?",
            uiOutput(ns("lastDataUpdate"))
          )
        )
      ),
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = "Country-wide trends",
            class = "panel-primary",
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
            class = "panel-primary",
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
            class = "panel-primary",
            plotOutput(ns("rankingPlot"), height = "800px") %>% withSpinner()
          )
        )
      )
    )
  )
}

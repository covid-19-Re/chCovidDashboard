library(shinycssloaders)
library(plotly)

vaccUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Vaccinations in Switzerland</h1>"
            ),
            class = "panel-primary",
            includeMarkdown("R/vaccinationModule-md.md"),
            hr(),
            fluidRow(
              column(2,
                selectInput(
                  ns("dataType"),
                  label = "Select data type to show",
                  choices = c(
                    "fully vaccinated" = "nFullyVaccTotal",
                    "partially vaccinated" = "nPartiallyVaccTotal",
                    "partially or fully vaccinated" = "nPartiallyOrFullyVacTotal",
                    "administred doses" = "nDosesAdminTotal"
                  ),
                  selected = "nFullyVaccTotal"
                )
              ),
              column(10,
                checkboxInput(ns("normalisation"), "Show value / 100 people", value = TRUE)
              )
            )
          )
        )
      ),
      tabsetPanel(
        type = "pills", id = "vaccTabs",
        tabPanel(p(class = "tab-title", "Timeseries"), value = "timeseries",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotlyOutput(ns("timelinePlot"), height = "800px") %>% withSpinner()
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "vaccTabs",
        tabPanel(p(class = "tab-title", "By canton & age group"), value = "ageGroup",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotlyOutput(ns("plotByAge"), height = "1200px") %>% withSpinner()
          ))
        )
      )
    )
  )
}

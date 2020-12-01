library(shinycssloaders)
library(highcharter)

tablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Data Tables</h1>",
            ),
            class = "panel-primary",
            p("A collection of indicator values for Switzerland")
          )
        )
      ),
      tabsetPanel(
        type = "pills", id = "summaryTabs",
        tabPanel(p(class = "tab-title", "Summary Table"), value = "summaryTable",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            uiOutput(ns("tableCaption")),
            DT::dataTableOutput(ns("comparisonDataTable")),
            uiOutput(ns("tableFooter")),
            downloadButton(ns("downloadData"), "Download .csv")
          ))
        )
      ),
      highchartOutput("", height = 0), # Only used to load the Highcharts library
      HTML("<span class='help-block'>Method to integrate highcharts.js sparklines into datatables from https://github.com/nuno-agostinho/Highcharter-in-DataTables</span>")
    )
  )
}

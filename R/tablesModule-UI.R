library(shinycssloaders)

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
            DT::dataTableOutput(ns("comparisonDataTable")) %>% withSpinner(),
            uiOutput(ns("tableFooter")),
            downloadButton(ns("downloadData"), "Download .csv")
          ))
        )
      ),
      uiOutput(ns("dataDownloads"))
    )
  )
}

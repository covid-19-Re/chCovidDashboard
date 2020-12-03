lagebeurteilungUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        uiOutput(ns("lagebericht"))
      )
    )
  )
}

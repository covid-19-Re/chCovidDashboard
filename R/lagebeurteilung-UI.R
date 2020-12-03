lagebeurteilungUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
        bootstrapPanel(
          column(12,
            includeHTML("www/lagebeurteilung/Lagebeurteilung.html")
          )
        )
      )
  )
}

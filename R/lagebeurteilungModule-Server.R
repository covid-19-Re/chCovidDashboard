lagebeurteilungServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lagebericht <- renderUI({
        bootstrapPanel(
          heading = tagList(
            h1("Epidemiologische Lagebeurteilung"),
            HTML("Swiss National COVID-19 Science Task Force<br>"),
            fluidRow(
              column(6,
                HTML(
                  "<i class='small'>Daten aktualisiert (täglich): ",
                    format(file.mtime("www/lagebeurteilung/lagebeurteilung-shiny.html"), "%d.%m.%Y %H:%M"),
                  "</i>&nbsp;|&nbsp;",
                  "<i class='small'>Text aktualisiert (finale Version jeweils am Montag Abend): ",
                    format(file.mtime("R/trendsModule-Files/Lagebeurteilung.Rmd"), "%a, %d.%m.%Y %H:%M"),
                  "</i>"
                )
              ),
              column(6,
                HTML(
                  "<div style='float: right'>",
                    "Downloads:&nbsp; ",
                    "<a href='https://ibz-shiny.ethz.ch/covidDashboardTest/lagebeurteilung/Lagebeurteilung.pdf' style='color:white;'><i class= 'fa fa-file-pdf'></i>&nbsp;pdf</a>&nbsp;",
                    "<a href='https://ibz-shiny.ethz.ch/covidDashboardTest/lagebeurteilung/Lagebeurteilung.docx' style='color:white;'><i class= 'fa fa-file-word'></i>&nbsp;docx</a>",
                  "</div>")
              )
            )
          ),
          class = "panel-primary",
          includeHTML("www/lagebeurteilung/lagebeurteilung-shiny.html")
        )
      })
    }
  )
}

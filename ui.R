library("shinyjs")

tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  navbarPage("CH Covid-19 Dashboard", id = "main", theme = "theme.min.css",
    tabPanel("Time series - cases",
      tsCasesUI("tsCases")
    ),
    tabPanel("Time series - proportions",
      tsProportionsUI("tsProportions")
    ),
    tabPanel("Contact tracing",
      ctUI("contactTracing")
    ),
    tabPanel("Quarantine duration",
      quarantineDurationUI("quarantineDuration")
    ),
    navbarMenu("About",
      tabPanel("About", icon = icon("question-circle"))
    )
  ),
  tags$script(src = "navbarRight.js")
)

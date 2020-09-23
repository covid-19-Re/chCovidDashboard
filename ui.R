fluidPage(width = "100%", class = "main-container", theme = "theme.min.css",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  navbarPage("CH Covid-19 Dashboard", id = ".main",
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
    )
  )
)

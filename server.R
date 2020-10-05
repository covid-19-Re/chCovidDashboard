server <- function(input, output, session) {
  # global
  observe({
    # Trigger this observer every time the tab changes
    input$tab
    session$doBookmark()
  })

  observe_helpers(help_dir = "R/forecastModule-Files/helpfiles")

  observeEvent(input$selectQuarantine, {
    updateTabsetPanel(session, "tab",
      selected = "quarantineDuration"
    )
  })
  observeEvent(input$selectContactTracing, {
    updateTabsetPanel(session, "tab",
      selected = "contactTracing"
    )
  })
  observeEvent(input$selectTs, {
    updateTabsetPanel(session, "tab",
      selected = "ts"
    )
  })
  observeEvent(input$selectTrends, {
    updateTabsetPanel(session, "tab",
      selected = "trends"
    )
  })
  observeEvent(input$selectForecast, {
    updateTabsetPanel(session, "tab",
      selected = "forecast"
    )
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  observe({
    toExclude <- setdiff(names(input), "tab")
    setBookmarkExclude(toExclude)
  })

  # contact tracing
  ctServer("contactTracing")

  # quarantine duration
  quarantineDurationServer("quarantineDuration")

  # Time series
  tsServer("ts")

  # trends
  trendsServer("trends")

  # forecast
  forecastServer("forecast")
}

server <- function(input, output, session) {

  observe_helpers(help_dir = "R/forecastModule-Files/helpfiles")

  observeEvent(input$selectQuarantine, {
    updateTabsetPanel(session, "tab",
      selected = "quarantineDuration"
    )
  })
  observeEvent(input$selectTTIQ, {
    updateTabsetPanel(session, "tab",
      selected = "ttiq"
    )
  })
   observeEvent(input$selectTs, {
     updateTabsetPanel(session, "tab",
       selected = "ts"
     )
   })
   observeEvent(input$selectTsDataQuality, {
     updateTabsetPanel(session, "tab",
       selected = "tsDataQuality"
     )
   })
  observeEvent(input$selectTrends, {
    updateTabsetPanel(session, "tab",
      selected = "trends"
    )
  })
  observeEvent(input$selectTables, {
    updateTabsetPanel(session, "tab",
      selected = "tables"
    )
  })
  observeEvent(input$selectForecast, {
    updateTabsetPanel(session, "tab",
      selected = "forecast"
    )
  })

  ttiqServer("ttiq")

  quarantineDurationServer("quarantineDuration")

   tsServer("ts")

   tsDataQualityServer("tsDataQuality")

  trendsServer("trends")

  tablesServer("tables")

  forecastServer("forecast")

}

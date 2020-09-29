server <- function(input, output, session) {
# global
  observe({
    # Trigger this observer every time the tab changes
    input$tab
    session$doBookmark()
  })
  
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
  observeEvent(input$selectTsProportions, {
    updateTabsetPanel(session, "tab",
      selected = "tsProportions"
    )
  })
  observeEvent(input$selectTsCases, {
    updateTabsetPanel(session, "tab",
      selected = "tsCases"
    )
  })
  observeEvent(input$selectTrends, {
    updateTabsetPanel(session, "tab",
      selected = "trends"
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

# Cases time series
  tsCasesServer("tsCases")

# trends
  trendsServer("trends")

# Proportions time series
  tsProportionsServer("tsProportions")


}

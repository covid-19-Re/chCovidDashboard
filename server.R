server <- function(input, output, session) {
# global
  observe({
    # Trigger this observer every time an input changes
    input$tab
    session$doBookmark()
  })
  observeEvent(input$selectQuarantine, {
    print(input$tab)
    updateTabsetPanel(session, "tab",
      selected = "quarantineDuration"
    )
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), "tab")
    setBookmarkExclude(toExclude)
  })

# contact tracing
  #ctServer("contactTracing")

# quarantine duration
  quarantineDurationServer("quarantineDuration")

# Cases time series
  #tsCasesServer("tsCases")

# Proportions time series
  #tsProportionsServer("tsProportions")

}

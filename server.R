server <- function(input, output, session) {
# global

# contact tracing
  ctServer("contactTracing")

# quarantine duration
  quarantineDurationServer("quarantineDuration")

# Cases time series
  tsCasesServer("tsCases")

# Proportions time series
  tsProportionsServer("tsProportions")

}

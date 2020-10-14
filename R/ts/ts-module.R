library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)
library(sf)

source("R/utilities.R")
source("R/ts/ts-constants.R")
source("R/ts/ts-utils.R")
source("R/ts/ts-load_and_process_data.R")
source("R/ts/ts-plots.R")

# Submodules
source("R/ts/ts-basic-filter-module.R")

basicFilters <- list(
  "ageGroup" = createBasicFilter("Age group", tsConstants$ageGroups, "ageGroup"),
  "canton" = createBasicFilter("Canton", tsConstants$cantons, "canton"),
  "travelClass" = createBasicFilter("Travel related status", tsConstants$travelRelatedStatus, "travelClass"),
  "expCountryCode" = createBasicFilter("Exposure country", tsConstants$expCountryCode, "expCountryCode"),
  "expContactPath" = createBasicFilter("Possible Exposure Source", sort(tsConstants$expContactPaths), "expContactPath"),
  "quarantBeforePositiveTest" = createBasicFilter("Was patient in quarantine before positive result?",
                                                  tsConstants$quarantBeforePositiveTest,"quarantBeforePositiveTest"),
  "labReason" = createBasicFilter("Reason for the test", tsConstants$labReason, "labReason")
)


source("R/ts/ts-ui.R")
source("R/ts/ts-server.R")

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
  "sex" = createBasicFilter("Sex", tsConstants$sex, "sex"),
  "canton" = createBasicFilter("Canton", tsConstants$cantons, "canton"),
  "travelClass" = createBasicFilter("Travel related status", tsConstants$travelRelatedStatus, "travelClass"),
  "expCountryCode" = createBasicFilter("Exposure country", tsConstants$expCountryCode, "expCountryCode"),
  "expContactPath" = createBasicFilter("Exposure contact path", sort(tsConstants$expContactPaths), "expContactPath"),
  "quarantBeforePositiveTest" = createBasicFilter("Was patient in quarantine before positive result?",
                                                  tsConstants$quarantBeforePositiveTest,"quarantBeforePositiveTest"),
  "labReason" = createBasicFilter("Reason for the test", tsConstants$labReason, "labReason"),

  "underlyingDisease" = createBasicFilter(
    "Underlying disease", tsConstants$underlyingDisease, "underlyingDisease",
    "Because a person can have multiple underlying diseases, comparing them will show a plot with a
                      higher total number of cases then the actual number.",
    function (input, output, session) {
      return (
        function (data) {
          p <- input$picker
          c <- tsConstants$underlyingDisease
          if (length(p) < length(c)) {
            return (data %>% filter(
              replace_na((grunderkr_diabetes == 1 & c[1] %in% p), FALSE) |
                replace_na((grunderkr_cardio == 1 & c[2] %in% p), FALSE) |
                replace_na((grunderkr_hypertonie == 1 & c[3] %in% p), FALSE) |
                replace_na((grunderkr_resp_chron == 1 & c[4] %in% p), FALSE) |
                replace_na((grunderkr_krebs == 1 & c[5] %in% p), FALSE) |
                replace_na((grunderkr_immunsup == 1 & c[6] %in% p), FALSE) |
                replace_na((grunderkr_andere == 1 & c[7] %in% p), FALSE) |
                replace_na((grunderkr_keine == 1 & c[8] %in% p), FALSE) |
                (
                  are_na(grunderkr_diabetes) & are_na(grunderkr_cardio) & are_na(grunderkr_hypertonie) &
                    are_na(grunderkr_resp_chron) & are_na(grunderkr_krebs) & are_na(grunderkr_immunsup) &
                    are_na(grunderkr_andere) & are_na(grunderkr_keine) & c[9] %in% p
                )
            ))
          }
          return (data)
        }
      )
    },
    function (input, output, session) {
      return (
        function (data) {
          return (c(
            "grunderkr_diabetes", "grunderkr_cardio", "grunderkr_hypertonie", "grunderkr_resp_chron", "grunderkr_krebs",
            "grunderkr_immunsup", "grunderkr_andere", "grunderkr_keine", NA
          ))
        }
      )
    },
    function (input, output, session) {
      return (
        function (data, group) {
          if (!is.na(group)) {
            return (filter(data, !!as.symbol(group) == 1))
          } else (
            return (
              filter(data, are_na(grunderkr_diabetes) & are_na(grunderkr_cardio) & are_na(grunderkr_hypertonie) &
                are_na(grunderkr_resp_chron) & are_na(grunderkr_krebs) & are_na(grunderkr_immunsup) &
                are_na(grunderkr_andere) & are_na(grunderkr_keine))
            )
          )
        }
      )
    }
  )
)


source("R/ts/ts-ui.R")
source("R/ts/ts-server.R")

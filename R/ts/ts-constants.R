library(lubridate)
library(rnaturalearth)
library(rgeos)


tsConstants <- list()

tsConstants$ageGroups <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
  "70-79", "80+", "Unknown"
)

tsConstants$cantons <- c(
  "AG", "AI", "AR", "BE", "BL", "BS", "FL", "FR", "GE", "GL", "GR",
  "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
  "UR", "VD", "VS", "ZG", "ZH"
)

tsConstants$travelRelatedStatus <- c("Travel-related", "Non-travel-related")

tsConstants$expCountryCode <- c(
  "CHE",
  sort(unlist((ne_countries(returnclass = "sf") %>% filter(iso_a3 != "CHE"))$iso_a3)),
  "Unknown"
)

tsConstants$expContactPaths <- c(
  "Family member",
  "as medical staff",
  "other contacts",
  " Unknown",
  "School/child care etc",
  "Work",
  "private party",
  "Disco/Club",
  "Bar/Restaurant",
  "Demonstration/Event",
  "spontaneous crowd of people",
  " not filled"
)

tsConstants$expContactPathsFromCode <- function(code) {
  if (is.na(code)) {
    return (" not filled")
  } else {
    return (tsConstants$expContactPaths[code]) # This works only because the code is a numerical value starting from 1
  }
}

tsConstants$quarantBeforePositiveTest <- c(
  "  Yes",
  " No",
  "Unknown",
  "Not filled"
)

tsConstants$quarantBeforePositiveTestFromCode <- function(code) {
  if (is.na(code)) {
    return ("Not filled")
  } else {
    return (tsConstants$quarantBeforePositiveTest[code])
  }
}

tsConstants$labReason <- c(
  " Symptoms compatible with COVID-19",
  " Outbreak investigation",
  "Other",
  " SwissCovid App",
  "Not filled"
)

tsConstants$labReasonFromCode <- function(code) {
  if (is.na(code)) {
    return ("Not filled")
  } else {
    res <- tsConstants$labReason[code]
    # There seems to be one case where the value is wrong.
    if(is.na(res)) {
      return ("Not filled")
    }
    return (res)
  }
}

tsConstants$eventDateCols <- list(
  "Positive test" = "fall_dt",
  "Hospitalisation" = "hospdatin",
  "ICU admission" = "em_hospit_icu_in_dt",
  "Death" = "pttoddat",
  "Test (any result)" = "fall_dt"
)

tsConstants$events <- names(tsConstants$eventDateCols)

tsConstants$granularityChoices <- c("Days", "Weeks", "Months")

tsConstants$slidingWindowChoices <- c("None", "7 days", "14 days", "28 days")

tsConstants$slidingWindowChoicesToIntervals <- list(
  "None" = lubridate::days(0),
  "7 days" = lubridate::days(7),
  "14 days" = lubridate::days(14),
  "28 days" = lubridate::days(28)
)

tsConstants$normalizationTimerangeOptions <- seq(ymd('2020-03-01'), today() %m-% months(2), by = '1 month')
names(tsConstants$normalizationTimerangeOptions) <- as.character(tsConstants$normalizationTimerangeOptions,
                                                                 format = "%b %Y")
tsConstants$normalizationTimerangeOptions <- as.list(tsConstants$normalizationTimerangeOptions)

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

tsConstants$expContactPaths <- c(
  "Family member",
  "as medical staff",
  "other contacts",
  "Unknown",
  "School/child care etc",
  "Work",
  "private party",
  "Disco/Club",
  "Bar/Restaurant",
  "Demonstration/Event",
  "spontaneous crowd of people",
  "not filled"
)

tsConstants$expContactPathsFromCode <- function(code) {
  if (is.na(code)) {
    return ("not filled")
  } else {
    return (tsConstants$expContactPaths[code]) # This works only because the code is a numerical value starting from 1
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

tsConstants$travelChoices <- c("All cases", "Travel-related", "Non-travel-related")

tsConstants$granularityChoices <- c("Days", "Weeks", "Months")

tsConstants$slidingWindowChoices <- c("None", "7 days", "14 days", "28 days")

tsConstants$slidingWindowChoicesToIntervals <- list(
  "None" = days(0),
  "7 days" = days(7),
  "14 days" = days(14),
  "28 days" = days(28)
)

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

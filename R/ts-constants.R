ageGroups <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
  "70-79", "80+", "Unknown"
)

cantons <- c(
  "AG", "AI", "AR", "BE", "BL", "BS", "FL", "FR", "GE", "GL", "GR",
  "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
  "UR", "VD", "VS", "ZG", "ZH"
)

eventDateCols <- list(
  "Positive test" = "fall_dt",
  "Hospitalisation" = "hospdatin",
  "ICU admission" = "em_hospit_icu_in_dt",
  "Death" = "pttoddat",
  "Test (any result)" = "fall_dt"
)

events <- names(eventDateCols)

travelChoices <- c("All cases", "Travel-related", "Non-travel-related")

granularityChoices <- c("Days", "Weeks", "Months")

slidingWindowChoices <- c("None", "7 days", "14 days", "28 days")


categoryCols <- list(
  "Canton" = "canton",
  "Age group" = "ageGroup",
  "Import status" = "travelClass"
)

categoryNames <- c("Canton", "Age group", "Import status")

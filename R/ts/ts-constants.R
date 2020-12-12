library(lubridate)
library(rnaturalearth)
library(rgeos)


ts_constants <- list()

ts_constants$age_group <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
  "70-79", "80+", "Unknown"
)

ts_constants$sex <- c("Male", "Female", "Unknown")

ts_constants$grossregion <- c(
  "Lake Geneva region",
  "Espace Mittelland",
  "Grossregion Nordwestschweiz",
  "Grossregion Zurich",
  "Ostschweiz",
  "Central Switzerland",
  "Grossregion Tessin",
  "Fürstentum Liechtenstein"
)

ts_constants$cantons <- c(
  "AG", "AI", "AR", "BE", "BL", "BS", "FL", "FR", "GE", "GL", "GR",
  "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
  "UR", "VD", "VS", "ZG", "ZH"
)

ts_constants$travel_class <- c("Travel-related", "Non-travel-related", "Unknown")

ts_constants$exp_land_code <- c(
  "CHE",
  sort(unlist((ne_countries(returnclass = "sf") %>% filter(iso_a3 != "CHE"))$iso_a3)),
  "Unknown"
)

ts_constants$exp_kontakt_art <- c(
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
  "spontaneous crowd of people"
)

ts_constants$quarant_vor_pos <- c(
  "Yes",
  "No",
  "Unknown"
)

ts_constants$lab_grund <- c(
  "Symptoms compatible with COVID-19",
  "Outbreak investigation",
  "Other",
  "SwissCovid App",
  "Unknown"
)

ts_constants$underlyingDisease <- c(
  "Diabetes",
  "Cardio",
  "Hypertension",
  "Chronic respiratory disease",
  "Cancer",
  "Immunosuppression",
  "Other",
  "None",
  "Unknown"
)

ts_constants$eventDateCols <- list(
  "Positive test" = "fall_dt",
  "Hospitalisation" = "hospdatin",
  # "ICU admission (unreliable)" = "em_hospit_icu_in_dt",
  "Death" = "pttoddat",
  "Test (any result)" = "fall_dt"
)

ts_constants$events <- names(ts_constants$eventDateCols)

ts_constants$granularityChoices <- c("Days", "Weeks", "Months")

ts_constants$slidingWindowChoices <- c("None", "7 days", "14 days", "28 days")

ts_constants$slidingWindowChoicesToIntervals <- list(
  "None" = list(before = lubridate::days(0), after = lubridate::days(0)),
  "7 days" = list(before = lubridate::days(6), after = lubridate::days(0)),
  "14 days" = list(before = lubridate::days(13), after = lubridate::days(0)),
  "28 days" = list(before = lubridate::days(27), after = lubridate::days(0))
)

ts_constants$normalizationTimerangeOptions <- seq(ymd('2020-03-01'), today() %m-% months(2), by = '1 month')
names(ts_constants$normalizationTimerangeOptions) <- as.character(ts_constants$normalizationTimerangeOptions,
                                                                 format = "%b %Y")
ts_constants$normalizationTimerangeOptions <- as.list(ts_constants$normalizationTimerangeOptions)

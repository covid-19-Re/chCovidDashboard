library(tidyverse)
library(scales)
library(lubridate)

##  Definitions

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


## Utility functions

round_dates <- function(.data, granularity) {
  switch(granularity,
    Days = {
      .data
    },
    Weeks = {
      .data %>% mutate(date = round_date(date, unit = "week"))
    },
    Months = {
      .data %>% mutate(date = round_date(date, unit = "month"))
    }
  )
}


## Plotting functions
generate_proportion_plot <- function(data, event, propCategoryName,
                                     smoothing, logScale = FALSE) {
  propCategoryCol <- categoryCols[[propCategoryName]]

  validate(need(
    propCategoryCol != "travelClass" || event != "Test (any result)",
    "Currently lacking information about travel status for negative test data."
  ))

  data_proc <- data

  if (event == "Test (any result)") {
    ## Exclude all data before start of stratified negative test records
    stratifiedTestingStart <- min((data_proc %>%
      filter(positiveTest == FALSE, !is.na(canton)))$fall_dt)

    data_proc <- data_proc %>% filter(fall_dt >= stratifiedTestingStart)
  }


  data_proc <- data_proc %>%
    mutate(date = !!as.symbol(eventDateCols[[event]])) %>%
    filter(!is.na(date))

  if (event == "Positive test") {
    data_proc <- data_proc %>% filter(positiveTest)
  }

  smoothing_interval <- switch(smoothing,
    None = days(0),
    "7 days" = days(7),
    "14 days" = days(14),
    "28 days" = days(28)
  )

  # Set the range of the histogram
  minDate <- min(data_proc$date)
  maxDate <- max(data_proc$date)

  plot_data <- NULL
  for (val in unique(data_proc[[propCategoryCol]])) {

    ## Select entries matching compare_val.
    d <- data_proc %>% filter(!!as.symbol(propCategoryCol) == val)

    d <- d %>%
      group_by(date) %>%
      summarize(count = sum(mult, na.rm = TRUE), .groups = "drop") %>%
      complete(date = seq.Date(minDate, maxDate, by = "day")) %>%
      mutate(count = replace_na(count, 0))

    d[, propCategoryName] <- val

    d$smoothedCount <- slide_index_dbl(d$count, d$date, sum, .before = smoothing_interval)

    plot_data <- bind_rows(plot_data, d)
  }

  plot_data <- plot_data %>%
    group_by(date) %>%
    mutate(proportion = smoothedCount / sum(smoothedCount))

  p <- ggplot(plot_data, aes(x = date, y = proportion, col = !!as.symbol(propCategoryName))) +
    geom_line() + # geom_point() +
    scale_x_date(date_breaks = "months") +
    xlab(paste("Date of", event)) +
    ylab(paste0("Proportion of ", event, "s"))

  if (logScale) {
    p <- p + scale_y_log10()
  }

  p
}

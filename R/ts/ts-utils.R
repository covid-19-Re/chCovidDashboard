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

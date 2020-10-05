#' Find the intersection between multiple sets/vectors. This function can deal with every collection that intersect()
#' supports.
#'
#' @examples
#' multiIntersect(
#'   (1:20),
#'   (10:30),
#'   (5:15)
#' )
multiIntersect <- function(...) {
  sets <- list(...)
  intersection = NULL
  for (s in sets) {
    if (is.null(intersection)) {
      intersection = s
    } else {
      intersection <- intersect(intersection, s)
    }
  }
  return (intersection)
}


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

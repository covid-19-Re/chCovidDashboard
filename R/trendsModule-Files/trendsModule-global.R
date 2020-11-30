source(here::here("R/trendsModule-Files/trendsModule-fns.R"))

newestBAGfile <- getNewestBagFile(path = here::here("data/BAG"))
bagData <- readRDS(newestBAGfile) %>% as_tibble()
updateDate <- strptime(
  stringr::str_match(newestBAGfile, ".*\\/(\\d*-\\d*-\\d*)_\\d*-\\d*-\\d*")[, 2],
  format = "%Y-%m-%d"
)

newestICUfile <- getNewestICUfile(path = here::here("data/ICU"))
icuDataRaw <- read_csv(newestICUfile, col_types = cols(
  .default = col_double(),
  Date = col_date(format = "")
))
icuDataRaw$CH <- rowSums(icuDataRaw[, 2:27], na.rm = TRUE)
icuDataRaw <- icuDataRaw %>%
  pivot_longer(cols = -Date, names_to = "region", values_to = "count") %>%
  transmute(
    region = region,
    age_class = "all",
    date = Date,
    event = "icu",
    count = count,
    weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0)
  ) %>%
  filter(!is.na(count))

pars <- list(
  time_window = 14,
  delete = c(
      cases = 3,
      hospitalizations = 5,
      icu = 0,
      deaths = 5
    )
)
pars$lastday <- today()
pars$begin <- pars$lastday - pars$delete + 1 - pars$time_window
pars$end <- pars$lastday - pars$delete

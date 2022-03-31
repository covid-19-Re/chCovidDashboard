source(here::here("R/trendsModule-Files/trendsModule-fns.R"))

pars <- list(
  time_window = 14,
  delete = c(
      cases = 3,
      hospitalizations = 5,
      icu = 0,
      deaths = 5
    ),
  lastday = c(
    cases = today(),
    hospitalizations = today(),
    icu = today(),
    deaths = today()
    )
)

pars$begin <- pars$lastday - pars$delete + 1 - pars$time_window
pars$end <- pars$lastday - pars$delete

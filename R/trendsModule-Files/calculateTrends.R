library(tidyverse)
library(lubridate)
library(slider)
library(here)

source(here("R/trendsModule-Files/trendsModule-global.R"))

eventCounts <- list()
eventCounts$cases <- bagData %>% getEventCounts(fall_dt, "cases")
eventCounts$hospitalizations <- bagData %>% getEventCounts(hospdatin, "hospitalizations")
eventCounts$deaths <- bagData %>% getEventCounts(pttoddat, "deaths")
eventCounts$icu <- icuDataRaw %>% filter(region == "CH")

qs::qsave(eventCounts, here("data/trends-eventCounts.qs"))

# save data if on testServer
if (str_detect(getwd(), "testapp")) {
  write_csv(eventCounts %>% bind_rows(), file = here("www/eventCounts.csv"))
}

eventCounts2wk <- lapply(eventCounts, function(df) {
  df2wk <- df %>%
    filter(date >= pars$begin[df$event[1]] & date <= pars$end[df$event[1]])
  return(df2wk)
})

models <- calcTrendsModel(eventCounts2wk)
predictions <- calcPredictions(models)
qs::qsave(predictions, here("data/trends-predictions.qs"))
doublingTimes <- calcDoublingTimes(models)
qs::qsave(doublingTimes, here("data/trends-doublingTimes.qs"))
ranking <- calcRanking(doublingTimes)
qs::qsave(ranking, here("data/trends-ranking.qs"))

doublingTimesTable <- doublingTimes %>%
  transmute(
    region, age_class, event,
    dt_estimate = estimate,
    dt_lower = lower,
    dt_upper = upper)

rankingTable <- ranking %>%
  transmute(
    region, age_class, event,
    wc_estimate = estimate * 100,
    wc_lower = lower * 100,
    wc_upper = upper * 100)

trendsTable <- doublingTimesTable %>%
  full_join(rankingTable, by = c("region", "age_class", "event"))

qs::qsave(trendsTable, here("data/trends-predictionsTable.qs"))

popSizes <- read_csv(here("data/popSizeAgeCHELIE.csv"),
  col_types = cols(
    region = col_character(),
    age_class = col_character(),
    populationSize = col_double()
  )
)

incidenceData <- eventCounts2wk %>%
  bind_rows() %>%
  left_join(popSizes, by = c("region", "age_class")) %>%
  mutate(
    value = replace_na(count, 0),
    valueNorm = replace_na(value / populationSize * 100000, 0)) %>%
  arrange(region, age_class, event, date) %>%
  group_by(region, age_class, event) %>%
  mutate(
    value7daySum = slide_index_dbl(value, date, sum, .before = days(7 - 1)),
    value7dayAve = slide_index_dbl(value, date, mean, .before = days(7 - 1)),
    valueNorm7daySum = slide_index_dbl(valueNorm, date, sum, .before = days(7 - 1)),
    valueNorm7dayAve = slide_index_dbl(valueNorm, date, mean, .before = days(7 - 1)),
    value14daySum = slide_index_dbl(value, date, sum, .before = days(14 - 1)),
    value14dayAve = slide_index_dbl(value, date, mean, .before = days(14 - 1)),
    valueNorm14daySum = slide_index_dbl(valueNorm, date, sum, .before = days(14 - 1)),
    valueNorm14dayAve = slide_index_dbl(valueNorm, date, mean, .before = days(14 - 1))
  ) %>%
  top_n(1, date) %>%
  ungroup() %>%
  select(
    region, age_class, event,
    value7daySum:valueNorm14dayAve)

qs::qsave(incidenceData, here("data/trends-incidenceTable.qs"))

rmarkdown::render(
  here("R/trendsModule-Files/Lagebeurteilung.Rmd"),
  output_format = "all",
  output_dir = here("www/lagebeurteilung"),
  encoding = "UTF-8",
  quiet = TRUE)

rmarkdown::render(
  here("R/trendsModule-Files/Lagebeurteilung.Rmd"),
  output_file = "lagebeurteilung-shiny.html",
  output_format = "html_fragment",
  output_dir = here("www/lagebeurteilung"),
  encoding = "UTF-8",
  quiet = TRUE)


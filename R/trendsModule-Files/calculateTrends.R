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
    value7day = slide_index_dbl(value, date, mean, .before = days(7)),
    valueNorm7day = slide_index_dbl(valueNorm, date, mean, .before = days(7)),
    value14day = slide_index_dbl(value, date, mean, .before = days(14)),
    valueNorm14day = slide_index_dbl(valueNorm, date, mean, .before = days(14))
  ) %>%
  top_n(1, date) %>%
  ungroup() %>%
  select(
    region, age_class, event,
    value7day = value7day,
    valueNorm7day = valueNorm7day,
    value14day = value14day,
    valueNorm14day = valueNorm14day)

qs::qsave(incidenceData, here("data/trends-incidenceTable.qs"))

rmarkdown::render(
  here("R/trendsModule-Files/Lagebeurteilung.Rmd"),
  output_format = "all",
  output_dir = here("www/lagebeurteilung"),
  encoding = "UTF-8",
  quiet = TRUE)


library(tidyverse)
library(lubridate)

getNewestBagFile <- function(path = "data/BAG") {
  bagFiles <- tibble(
    path = list.files(path,
      pattern = "*FOPH_COVID19_data_extract.rds",
      full.names = TRUE,
      recursive = TRUE)
    ) %>%
  mutate(
    date = strptime(
      stringr::str_match(path, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
      format = "%Y-%m-%d_%H-%M-%S"),
    weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0)
  )

  if (!str_detect(getwd(), "testapp")) {
    # don't use new weekend data if not in testapp
    bagFiles <- bagFiles %>%
      filter(weekend != 1)
  }

  newestBAGfile <- bagFiles$path[which(bagFiles$date == max(bagFiles$date))[1]]
  return(newestBAGfile)
}

getNewestICUfile <- function(path = "data/ICU") {
  icuFiles <- tibble(
    path = list.files(path,
      pattern = "icus_canton_.*.csv",
      full.names = TRUE,
      recursive = TRUE)
    ) %>%
  mutate(
    date = strptime(
      stringr::str_match(path, "icus_canton_(.*).csv")[, 2],
      format = "%d%m%Y")
  )

  newestICUfile <- icuFiles$path[which(icuFiles$date == max(icuFiles$date))[1]]
  return(newestICUfile)
}

getEventCounts <- function(df, event_dt, event_name) {
  # CH
  countsCH <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    group_by({{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # cantons
  countsRegions <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    group_by(ktn, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = ktn, age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # age groups
  countsAgeClass <- df %>%
    mutate(age_class = cut(altersjahr, breaks = c(seq(0, 80, by = 10), 200), right = FALSE)) %>%
    dplyr::select({{ event_dt }}, age_class) %>%
    group_by(age_class, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = age_class, date = {{ event_dt }}, event = event_name, count = count) %>%
    complete(region, age_class, date, event, fill = list(count = 0))

  levels(countsAgeClass$age_class)[levels(countsAgeClass$age_class) == "[80,200)"] <- "[80,∞)"

  # all
  counts <- bind_rows(countsCH, countsRegions) %>%
    complete(region, age_class, date, event, fill = list(count = 0)) %>%
    bind_rows(countsAgeClass) %>%
    filter(!is.na(date)) %>%
    mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0))

  return(counts)
}

getEventCounts2 <- function(df, event_dt, event_name) {
  # CH
  countsCH <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    group_by({{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # cantons
  countsRegions <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    group_by(ktn, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = ktn, age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # age groups
  countsAgeClass <- df %>%
    mutate(age_class = cut(altersjahr, breaks = c(0, 7, 16, 25, 35, 45, 55, 65, 75, 200), right = FALSE)) %>%
    dplyr::select({{ event_dt }}, age_class) %>%
    group_by(age_class, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = age_class, date = {{ event_dt }}, event = event_name, count = count) %>%
    complete(region, age_class, date, event, fill = list(count = 0))

  levels(countsAgeClass$age_class)[levels(countsAgeClass$age_class) == "[75,200)"] <- "[75,∞)"

  # all
  counts <- bind_rows(countsCH, countsRegions) %>%
    complete(region, age_class, date, event, fill = list(count = 0)) %>%
    bind_rows(countsAgeClass) %>%
    filter(!is.na(date)) %>%
    mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0))

  return(counts)
}


trendsModelFunction <- function(df) {
    try(MASS::glm.nb(count ~ date + weekend, data = df))
}

calcTrendsModel <- function(eventCounts) {
  eventCounts$deaths <- eventCounts$deaths %>%
    filter(region == "CH", age_class == "all")
  
  eventCountsDf <- eventCounts %>%
    bind_rows()

  models <- eventCountsDf %>%
    group_by(region, age_class, event) %>%
    nest() %>%
    mutate(model = map(data, trendsModelFunction))

  return(models)
}

predictFun <- function(model) {
  prediction <- try(exp(predict(model)))
  if (class(prediction) == "try-error") {
    prediction <- NA
  }
  return(prediction)
}

predictCIfun <- function(model, q) {
  predictCI <- try(qnbinom(q, mu = exp(predict(model)), size = model$theta))
  if (class(predictCI) == "try-error") {
    predictCI <- NA
  }
  return(predictCI)
}


calcPredictions <- function(models) {
  predictions <- models %>%
    mutate(
      prediction = map(model, predictFun),
      lower = map(model, predictCIfun, q = 0.025),
      upper = map(model, predictCIfun, q = 0.975)
    ) %>%
    select(-model) %>% 
    unnest(cols = c(data, prediction, upper, lower))
  return(predictions)
}

calcDoublingTimes <- function(models) {
  doublingTimes <- models %>%
    mutate(
      fit.ci = map(model, ~ tryCatch(confint(profile(.x)), error = function(e) {
        matrix(NA, 2, 2)
      })),
      estimate = map_dbl(model, ~ tryCatch(log(2) / coef(.x)[2], error = function(e) {NA})),
      lower = map_dbl(fit.ci, ~ tryCatch(log(2) / .x[2, 2], error = function(e) {NA})),
      upper = map_dbl(fit.ci, ~ tryCatch(log(2) / .x[2, 1], error = function(e) {NA}))
    ) %>%
    select(-data, -model, -fit.ci)

  return(doublingTimes)
}

calcRanking <- function(doublingTimes) {
  ranking <- doublingTimes %>%
    mutate(across(estimate:upper, ~ exp(log(2) / .x * 7) - 1))

  return(ranking)
}

# Plot Functions
source(here::here("R/plotTheme.R"))

plotPredictions <- function(predictions, doublingTimes, ranking, regionSelect, eventSelect,
  ageSelect = "all", fillColor, lang = "de", longLabel = TRUE) {
  plotData <- filter(predictions, region == regionSelect, event == eventSelect, age_class == ageSelect)
  doublingTimesData <- filter(doublingTimes, region == regionSelect, event == eventSelect, age_class == ageSelect)
  ranking <- filter(ranking, region == regionSelect, event == eventSelect, age_class == ageSelect)

  if (lang == "de") {
    yLabel <- case_when(
      eventSelect == "cases" ~ "Laborbestätigte Fälle",
      eventSelect == "icu" ~ "Patienten auf Intensivstation",
      eventSelect == "hospitalizations" ~ "Hospitalisationen",
      eventSelect == "deaths" ~ "Todesfälle"
    )
    if (regionSelect != "CH") {
      title <- regionSelect
    } else if (regionSelect == "CH" & ageSelect != "all") {
      title <- ageSelect
    } else {
      title <- yLabel
    }
    subLabel <- if_else(longLabel, "Veränderung (7d): ", "")
    to <- "bis"
  } else {
    yLabel <- case_when(
      eventSelect == "cases" ~ "Confirmed cases",
      eventSelect == "icu" ~ "Patients in ICU",
      eventSelect == "hospitalizations" ~ "Hospitalizations",
      eventSelect == "deaths" ~ "Deaths"
    )
    if (regionSelect != "CH") {
      title <- regionSelect
    } else if (regionSelect == "CH" & ageSelect != "all") {
      title <- ageSelect
    } else {
      title <- yLabel
    }
    subLabel <- if_else(longLabel, "Change (7d): ", "")
    to <- "to"
  }

  subtitle <- glue::glue_data(ranking,
    "{subLabel}{round(estimate * 100, 1)}% ({round(lower * 100, 1)}% {to} {round(upper * 100, 1)}%)")

  subtitle[which(is.na(ranking$estimate) | is.na(ranking$lower) | is.na(ranking$upper))] <- ""

  plot <- ggplot(
    data = plotData) +
    geom_ribbon(mapping = aes(date, prediction, ymin = lower, ymax = upper), fill = fillColor) +
    geom_point(mapping = aes(date, count), shape = 21) +
    geom_line(mapping = aes(date, prediction)) +
    scale_y_continuous(name = yLabel) +
    coord_cartesian(ylim = c(0, 1.5 * max(plotData$count))) +
    scale_x_date(name = "", date_breaks = "3 days", date_minor_breaks = "1 day", date_labels = "%b %d") +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    plotTheme

  return(plot)
}

plotRanking <- function(ranking, cols, lang = "de") {
  colors <- rep("black", length(ranking$region))
  names(colors) <- ranking$region
  colors["CH"] <- cols[1]

  changeRange <- c(
    min(-0.5, floor(min(c(ranking$lower, ranking$upper), na.rm = TRUE) * 10) / 10),
    max(min(4, ceiling(max(c(ranking$lower, ranking$upper), na.rm = TRUE) * 10) / 10), 1)
  )

  plot <- ggplot(
    data = ranking,
    mapping = aes(x = estimate, y = region, xmin = lower, xmax = upper, color = region)) +
    geom_point(size = 2) +
    geom_errorbar(width = 0.4, size = 0.75) +
    scale_x_continuous(
      name = if_else(lang == "de", "Wöchentliches Wachstum", "Weekly change"), labels = scales::percent) +
    coord_cartesian(xlim = changeRange) +
    scale_y_discrete(name = "") +
    scale_color_manual(values = colors, guide = "none") +
    plotTheme +
    labs(caption = if_else(lang == "de", "Daten: BAG", "Data: FOPH"))

  return(plot)
}

plotCantons <- function(predictions, doublingTimes, ranking, eventSelect, color, sorting = "growthAsc", lang = "de") {
  if (sorting == "growthAsc") {
    cantons <- levels(fct_reorder(ranking$region, ranking$estimate))
  } else if (sorting == "growthDesc") {
    cantons <- levels(fct_reorder(ranking$region, ranking$estimate, .desc = TRUE))
  } else {
    cantons <- ranking$region
  }

  cantonPlotsList <- list()
  for (i in cantons) {
    cantonPlotsList[[i]] <- plotPredictions(predictions, doublingTimes, ranking,
      regionSelect = i, eventSelect,
      fillColor = color,
      longLabel = FALSE,
      lang = lang
    ) +
      theme(text = element_text(size = 12)) +
      labs(title = i)
  }
  plot <- plot_grid(plotlist = cantonPlotsList, ncol = 4)
  return(plot)
}

plotAgeClass <- function(predictions, doublingTimes, ranking, eventSelect, color, lang = "de") {

  ageClasses <- unique(predictions$age_class)

  ageClassPlotList <- list()
  for (i in ageClasses) {
    ageClassPlotList[[i]] <- plotPredictions(predictions, doublingTimes, ranking,
      regionSelect = "CH", eventSelect = eventSelect, ageSelect = i,
      fillColor = color,
      longLabel = FALSE,
      lang = lang
    ) +
      theme(text = element_text(size = 12)) +
      labs(title = i)
  }
  plot <- plot_grid(plotlist = ageClassPlotList, ncol = 5)
  return(plot)
}

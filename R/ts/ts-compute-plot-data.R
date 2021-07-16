compute_plot_data <- function (model, language, data_store) {
  i18n <- Translator$new(translation_json_path = "data/ts-translations/ts-translations.json")
  i18n$set_translation_language(language)

  query <- data_store$load_main_data()

  # Creates the "date" column
  if (model$general$display_prob) {
    query <- query %>% mutate(date = fall_dt)
  } else {
    query <- query %>% mutate(date = !!as.symbol(ts_constants$eventDateCols[[model$general$event]]))
  }

  # Removes data that are outside of the investigated time range
  query <- query %>% filter(date >= as.Date("2020-02-19"))

  comparison_info <- get_comparison_info(model)

  active_filter_names <- NULL
  for (filter_name in names(model$filter)) {
    filter_model <- model$filter[[filter_name]]
    this_filter <- basicFilters[[filter_name]]

    # At least one value must always be selected
    if (length(filter_model$selected) == 0) {
      return(list(error = list(
        message = paste0("Must specify at least one value for '", this_filter$label, "'.")
      )))
    }

    # Collect the active filters
    if (this_filter$is_filtering(model)) {
      active_filter_names <- c(active_filter_names, filter_name)
    }

    # Filter data
    query <- this_filter$filter(query, filter_model)
  }

  query_involves_negative_tests <- model$general$event == "Test (any result)" ||
    (model$general$display_prob && model$general$given == "Test (any result)")

  # We only have the canton, grossregion and age group information for negative tests since 2020-05-23 and do not have any other
  # information. This means:
  #   (1) no filters/comparisons -> whole time range possible
  #   (2) age group/canton/grossregion filter/comparison -> only show data after 2020-05-23
  #   (3) other filters/comparisons -> not possible
  if (query_involves_negative_tests) {
    if (is.null(active_filter_names) && !comparison_info$is_comparing) {
      # (1)
    } else if (all(active_filter_names %in% c("age_group", "canton", "grossregion")) &&
      (!comparison_info$is_comparing || comparison_info$compare_attribute %in% c("age_group", "canton", "grossregion"))) {
      # (2)
      query <- query %>% filter(date >= as.Date("2020-05-23")) # TODO read the date from the dataset
    } else {
      return(list(error = list(
        message = "This operation is not possible because negative test data can only be stratified by age group, canton, and grossregion."
      )))
    }
  }

  # Population data can only be stratified by age group, sex, grossregion and canton. Since they are needed to compute
  # the numbers per 100,000 people, other filters may not be set.
  if (comparison_info$is_comparing && comparison_info$is_comparing_per_100k_people) {
    if (!all(active_filter_names %in% c("age_group", "sex", "grossregion", "canton"))) {
      return(list(error = list(
        message = "When comparing the number of events per 100,000 people, only the filters for age group, sex, grossregion and canton are allowed to be set."
      )))
    }
  }

  # Apply time granularity
  if (model$plot_type == "histogram" || model$plot_type == "map") {
    query <- switch(
      model$display$granularity,
      Days = query,
      Weeks = query %>% mutate(date = date(date_trunc('week', date))),
      Months = query %>% mutate(date = date(date_trunc('month', date)))
    )
  }

  min_date <- ymd(20200219)
  dashboard_state <- data_store$load_dashboard_state() %>% collect()
  max_date <- dashboard_state$last_data_update
  smoothing_interval <- ts_constants$slidingWindowChoicesToIntervals[[model$display$smoothing_window]]

  granularity <- "day"
  if (model$plot_type == "histogram" || model$plot_type == "map") {
    granularity <- switch(
      model$display$granularity,
      Days = "day",
      Weeks = "week",
      Months = "month"
    )
  }
  complete_date_sequence <- seq.Date(floor_date(min_date, unit = granularity, week_start = 1),
                                     floor_date(max_date, unit = granularity, week_start = 1),
                                     by = granularity)

  # Case 1a: showing the total frequencies (no normalization)
  if (!model$normalization$selected && !model$general$display_prob && !comparison_info$is_comparing) {
    query <- query %>%
      .filter_event(model) %>%
      group_by(date) %>%
      summarize(count = as.integer(sum(mult))) %>%
      ungroup()
    plotData <- query %>%
      collect() %>%
      complete(date = complete_date_sequence) %>%
      mutate(
        count = replace_na(count, 0)
      ) %>%
      drop_na(date)
    if (model$plot_type == "line" || model$plot_type == 'area') {
      plotData$count <- slide_index_dbl(plotData$count, plotData$date, mean,
                                        .before = smoothing_interval$before, .after = smoothing_interval$after)
    }
    plotData <- plotData %>%
      mutate(
        tooltipText = paste0("Date: ", as.character(date), "\nCount: ", round(count))
      )
    plotDef <- list(
      plotData, "date", "count",
      ylab = i18n$t("ts.plot.total_count")
    )
  }


  # Case 1b: with normalization
  if (model$normalization$selected) {
    if (is.null(model$normalization$timerange)) {
      return(list(error = list(message = "Must specify at least one month for normalization.")))
    }

    # Compute normalization constants for each age group
    start_dates <- ymd(model$normalization$timerange)
    end_dates <- start_dates %m+% months(1) %m-% days(1)

    date_filter_condition <- expr(FALSE)
    for (i in seq_along(start_dates)) {
      start_date <- start_dates[i]
      end_date <- end_dates[i]
      date_filter_condition <- expr(!!date_filter_condition | between(date, !!start_date, !!end_date))
    }
    normconst_base_query <- query %>%
      filter(!!date_filter_condition) %>%
      filter(positive_test)
    positive_tests_per_age_group_query <- normconst_base_query %>%
      group_by(age_group) %>%
      summarize(count_pos = sum(mult)) %>%
      ungroup()
    hospitalizations_per_age_group_query <- normconst_base_query %>%
      filter(hospitalisation == 1L) %>%
      group_by(age_group) %>%
      summarize(count_hosp = sum(mult)) %>%
      ungroup()
    normconst_final_query <- positive_tests_per_age_group_query %>%
      left_join(hospitalizations_per_age_group_query, by = "age_group") %>%
      mutate(normalization_constant = as.double(count_hosp) / count_pos) %>%
      select(age_group, normalization_constant)

    # TODO Handling zeros? It should not happen so often currently since the smallest granularity is month
    #     ... and unfortunately, we have enough cases.

    query <- query %>%
      filter(hospitalisation == 1L) %>%
      group_by(age_group, date) %>%
      summarize(count = sum(mult)) %>%
      ungroup() %>%
      inner_join(normconst_final_query, by = "age_group") %>%
      mutate(normalized = count / normalization_constant) %>%
      group_by(date) %>%
      summarize(count = sum(normalized)) %>%
      ungroup()
    plotData <- query %>% collect()

    if (model$plot_type == "line" || model$plot_type == "area") {
      plotData$count <- slide_index_dbl(plotData$count, plotData$date, mean,
                                        .before = smoothing_interval$before, .after = smoothing_interval$after)
    }
    plotData <- plotData %>%
      mutate(tooltipText = paste0("Date: ", as.character(plotData$date), "\nCount: ", round(plotData$count)))
    plotDef <- list(
      plotData, "date", "count",
      ylab = i18n$t("ts.plot.total_count")
    )
  }


  # Case 2: comparing the frequencies
  if (!model$general$display_prob && comparison_info$is_comparing) {
    query <- query %>%
      .filter_event(model) %>%
      group_by(!!as.symbol(comparison_info$compare_attribute), date) %>%
      summarize(count = as.integer(sum(mult))) %>%
      ungroup()  # I don't know why an ungroup() is required.

    if (comparison_info$is_comparing_per_100k_people) {
      populationData <- data_store$load_population_data()
      for (filter_name in names(model$filter)) {
        populationData <- basicFilters[[filter_name]]$filter(populationData, model$filter[[filter_name]])
      }

      populationData <- populationData %>%
        group_by(!!as.symbol(comparison_info$compare_attribute)) %>%
        summarize(population = sum(count))

      query <- query %>%
        left_join(populationData, by = comparison_info$compare_attribute) %>%
        mutate(count = count * 100000 / population)
    }
    dataProc <- query %>%
      collect()

    plotData <- NULL
    filterWithActiveComparison <- basicFilters[[comparison_info$compare_attribute]]
    for (compare_val in  filterWithActiveComparison$get_comparison_groups(dataProc)) {
      d <- dataProc %>%
        filterWithActiveComparison$get_entries_of_group(compare_val) %>%
        complete(date = complete_date_sequence) %>%
        mutate(count = replace_na(count, 0)) %>%
        drop_na(date)
      if (model$plot_type == "line" || model$plot_type == 'area') {
        d$count <- slide_index_dbl(d$count, d$date, mean,
                                   .before = smoothing_interval$before, .after = smoothing_interval$after)
      }
      d[, comparison_info$compare_attribute] <- compare_val
      plotData <- bind_rows(plotData, d)
    }
    roundingDigits <- 0
    if (comparison_info$is_comparing_per_100k_people) {
      roundingDigits <- 2
    }
    if (!comparison_info$is_comparing_proportions) {
      if (model$plot_type == "map") {
        if (!is.null(plotData)) {
          selectedDate <- floor_date(model$map_selected_day, unit = granularity, week_start = 1)
          plotData <- plotData %>%
            filter(date == selectedDate) %>%
            group_by(!!as.symbol(comparison_info$compare_attribute)) %>%
            summarize(
              count = sum(count)
            )
          if (comparison_info$compare_attribute == "canton") {
            plotData <- plotData %>%
              mutate(tooltipText = paste0(i18n$t(paste0("ts.constant.canton.", canton)),
                                          "\nCount: ", round(count, digits = roundingDigits)))
          } else {
            plotData <- plotData %>%
              mutate(tooltipText = paste0(i18n$t(paste0("ts.constant.exp_land_code.", exp_land_code)),
                                          "\nCount: ", round(count, digits = roundingDigits)))
          }
          plotDef <- list(
            plotData,
            region = if (comparison_info$compare_attribute == "canton") "switzerland" else "world"
          )
        }
      } else {
        plotData <- plotData %>%
          mutate(tooltipText = paste0(
            i18n$t(paste0("ts.constant.", comparison_info$compare_attribute, ".", !!as.symbol(comparison_info$compare_attribute))),
            "\nDate: ", as.character(plotData$date), "\nCount: ", round(plotData$count, digits = roundingDigits)
          ))
        plotDef <- list(
          plotData, "date", "count",
          groupingAttributeName = comparison_info$compare_attribute,
          ylab = i18n$t("ts.plot.total_count"),
          stacked = model$display$stack_histograms
        )
      }
    } else {
      if (!is.null(plotData)) {
        plotData <- plotData %>%
          group_by(date) %>%
          mutate(proportion = count / sum(count)) %>%
          mutate(tooltipText = paste0(
            i18n$t(paste0("ts.constant.", comparison_info$compare_attribute, ".", !!as.symbol(comparison_info$compare_attribute))),
            "\nDate: ", as.character(date), "\nProportion: ", round(proportion * 100, digits = 2), "%"))
      }

      plotDef <- list(
        plotData, "date", "proportion",
        groupingAttributeName = comparison_info$compare_attribute,
        ylab = paste0("Proportion of ", i18n$t(paste0("ts.constant.event.", model$general$event)) , "s")
      )
    }
  }


  # Case 3: looking at the probabilities when a type of event is given
  if (model$general$display_prob && !comparison_info$is_comparing) {
    denominatorQuery <- query %>%
      .filter_given(model)
    numeratorQuery <- denominatorQuery %>%
      .filter_event(model)

    processDataInternal <- function(q) {
      q <- q %>%
        group_by(date) %>%
        summarize(
          count = sum(mult)
        )
      d <- q %>%
        collect() %>%
        complete(date = seq.Date(min_date, max_date, by = "day")) %>%
        mutate(count = replace_na(count, 0))
      d$count <- slide_index_dbl(d$count, d$date, sum,
                                 .before = smoothing_interval$before, .after = smoothing_interval$after)
      return (d)
    }
    denominatorData <- processDataInternal(denominatorQuery)
    numeratorData <-  processDataInternal(numeratorQuery)
    prob <- numeratorData$count / denominatorData$count

    ciYMin <- NULL
    ciYMax <- NULL
    for (i in seq_along(numeratorData$count)) {
      if (is.nan(prob[i])) {
        ci <- c(NaN, NaN)
      } else {
        # binom.test uses the Clopper–Pearson method to calculate the confidence interval
        # See: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/binom.test.html
        ci <- binom.test(numeratorData$count[i], denominatorData$count[i], p = prob[i])$conf.int
      }
      ciYMin <- c(ciYMin, ci[1])
      ciYMax <- c(ciYMax, ci[2])
    }

    plotData <- tibble(date = denominatorData$date, prob = prob, ymin = ciYMin, ymax = ciYMax) %>%
      mutate(tooltipText = paste0("Date: ", as.character(date), "\nProbability: ",
                                  round(prob * 100, digits = 2), "%"))
    plotDef <- list(
      plotData, "date", "prob",
      ylab = paste0("Fraction of ", i18n$t(paste0("ts.constant.event.", model$general$given)),
                    "s involving ", i18n$t(paste0("ts.constant.event.", model$general$event))),
      addConfidenceInterval = model$display$show_confidence_interval
    )
  }


  # Case 4: comparing the probabilities
  if (model$general$display_prob && comparison_info$is_comparing) {
    denominatorQuery <- query %>%
      .filter_given(model)
    numeratorQuery <- denominatorQuery %>%
      .filter_event(model)
    denominatorQuery <- denominatorQuery %>%
      group_by(date, !!as.symbol(comparison_info$compare_attribute)) %>%
      summarize(count = as.integer(sum(mult))) %>%
      ungroup()
    numeratorQuery <- numeratorQuery %>%
      group_by(date, !!as.symbol(comparison_info$compare_attribute)) %>%
      summarize(count = as.integer(sum(mult))) %>%
      ungroup()
    denominatorData <- denominatorQuery %>% collect()
    numeratorData <- numeratorQuery %>% collect()

    processDataInternal <- function(d) {
      d <- d %>%
        complete(date = seq.Date(min_date, max_date, by = "day")) %>%
        mutate(count = replace_na(count, 0)) %>%
        arrange(date)
      d$count <- slide_index_dbl(d$count, d$date, sum,
                                 .before = smoothing_interval$before, .after = smoothing_interval$after)
      return (d)
    }

    plotData <- NULL
    filterWithActiveComparison <- basicFilters[[comparison_info$compare_attribute]]
    for (compare_val in filterWithActiveComparison$get_comparison_groups(denominatorData)) {
      dDenom <- denominatorData %>%
        filterWithActiveComparison$get_entries_of_group(compare_val)
      dNum <- numeratorData %>%
        filterWithActiveComparison$get_entries_of_group(compare_val)

      dDenom <- processDataInternal(dDenom)
      dNum <- processDataInternal(dNum)

      prob <- dNum$count / dDenom$count

      ciYMin <- NULL
      ciYMax <- NULL
      for (i in seq_along(dNum$count)) {
        if (is.nan(prob[i])) {
          ci <- c(NaN, NaN)
        } else {
          ci <- binom.test(dNum$count[i], dDenom$count[i], p = prob[i])$conf.int
        }
        ciYMin <- c(ciYMin, ci[1])
        ciYMax <- c(ciYMax, ci[2])
      }

      d <- tibble(date = dDenom$date, prob = prob, ymin = ciYMin, ymax = ciYMax)
      d[, comparison_info$compare_attribute] <- compare_val
      plotData <- bind_rows(plotData, d)
    }

    if (!comparison_info$is_comparing_proportions && model$plot_type == "map") {
      if (!is.null(plotData)) {
        plotData <- plotData %>%
          filter(date == model$map_selected_day) %>%
          mutate(count = prob)
        if (comparison_info$compare_attribute == "canton") {
          plotData <- plotData %>%
            mutate(tooltipText = paste0(canton, "\nProb.: ", round(count * 100, digits = 2), "%"))
        } else {
          plotData <- plotData %>%
            mutate(tooltipText = paste0(exp_land_code, "\nProb.: ", round(count * 100, digits = 2), "%"))
        }
        plotDef <- list(
          plotData,
          region = if (comparison_info$compare_attribute == "canton") "switzerland" else "world"
        )
      }
    } else {
      if (comparison_info$is_comparing_proportions) {
        plotData <- plotData %>%
          mutate(prob = replace_na(prob, 0)) %>%
          group_by(date) %>%
          mutate(prob = prob / sum(prob))
      }
      plotData <- plotData %>%
        mutate(tooltipText = paste0(!!as.symbol(comparison_info$compare_attribute), "\nDate: ",
                                    as.character(date), "\nProbability: ",
                                    round(prob * 100, digits = 2), "%"))
      plotDef <- list(
        plotData, "date", "prob",
        groupingAttributeName = comparison_info$compare_attribute,
        ylab = paste0("Fraction of ", i18n$t(paste0("ts.constant.event.", model$general$given)),
                      "s involving ", i18n$t(paste0("ts.constant.event.", model$general$event))),
        addConfidenceInterval = model$display$show_confidence_interval &&
          !comparison_info$is_comparing_proportions
      )
    }
  }

  # TODO Show an error message if no data matches the requested combination of filters.

  plotDef$max_date <- max_date
  return(plotDef)
}


### Provide functions only used by compute_plot_data

.filter_event <- function (data, model) {
  return (
    switch(
      model$general$event,
      "Test (any result)" = data,
      "Positive test" = data %>% filter(positive_test),
      "Hospitalisation" = data %>% filter(hospitalisation == 1),
      "Death" = data %>% filter(pttod),
      "ICU admission (unreliable)" = data %>% filter(icu_aufenthalt == 1)
    )
  )
}

.filter_given <- function (data, model) {
  return (
    switch(
      model$general$given,
      "Test (any result)" = data,
      "Positive test" = data %>% filter(positive_test),
      "Hospitalisation" = data %>% filter(hospitalisation == 1),
      "Death" = data %>% filter(pttod),
      "ICU admission (unreliable)" = data %>% filter(icu_aufenthalt == 1)
    )
  )
}


### Utility functions to work with the model ###
get_comparison_info <- function (model) {
  result <- list(
    is_comparing = FALSE,
    compare_attribute = NA,
    is_comparing_per_100k_people = FALSE,
    is_comparing_proportions = FALSE
  )
  for (filter_name in names(model$filter)) {
    filter_value <- model$filter[[filter_name]]
    if (filter_value$compare) {
      result <- list(
        is_comparing = TRUE,
        compare_attribute = filter_name,
        is_comparing_per_100k_people = filter_value$compare_per_100k_people,
        is_comparing_proportions = filter_value$compare_proportions
      )
      break
    }
  }
  return(result)
}


get_available_plot_types <- function (model) {
  # Plot types: histogram, line chart, area chart, map
  available <- NULL
  comp <- get_comparison_info(model)
  if (!model$general$display_prob && !comp$is_comparing_proportions) {
    available <- append(available, c("histogram", "line", "area"))
  } else {
    available <- append(available, "line")
  }
  if (comp$is_comparing_proportions) {
    available <- append(available, "area")
  }
  if (replace_na(comp$compare_attribute == "canton" || comp$compare_attribute == "exp_land_code", FALSE)
    && !comp$is_comparing_proportions) {
    available <- append(available, "map")
  }
  return (available)
}

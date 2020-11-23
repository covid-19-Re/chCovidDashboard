library(tidyverse)
library(lubridate)
library(cowplot)
library(DT)

getEventCounts <- function(df, event_dt, event_name, pars) {
  # CH
  countsCH <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    filter({{ event_dt }} >= pars$begin[event_name] & {{ event_dt }} <= pars$end[event_name]) %>%
    group_by({{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # cantons
  countsRegions <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    filter({{ event_dt }} >= pars$begin[event_name] & {{ event_dt }} <= pars$end[event_name]) %>%
    group_by(ktn, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = ktn, age_class = "all", date = {{ event_dt }}, event = event_name, count = count)

  # age groups
  countsAgeClass <- df %>%
    mutate(age_class = cut(altersjahr, breaks = c(seq(0, 80, by = 10), 121), right = FALSE)) %>%
    dplyr::select({{ event_dt }}, age_class) %>%
    filter({{ event_dt }} >= pars$begin[event_name] & {{ event_dt }} <= pars$end[event_name]) %>%
    group_by(age_class, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", age_class = age_class, date = {{ event_dt }}, event = event_name, count = count) %>%
    complete(region, age_class, date, event, fill = list(count = 0))

  # all
  counts <- bind_rows(countsCH, countsRegions) %>%
    complete(region, age_class, date, event, fill = list(count = 0)) %>%
    bind_rows(countsAgeClass) %>%
    mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0))

  return(counts)
}

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
    doublingLabel <- if_else(longLabel, "Verdoppelung: ", "")
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
    doublingLabel <- if_else(longLabel, "Doubling time: ", "")
  }

  subtitle <- glue::glue_data(doublingTimesData,
    "{doublingLabel}{round(estimate, 1)} d (95% CI: {round(lower,1)} to {round(upper,1)}d)")
  # subtitle <- glue::glue_data(
  #   ranking,
  #   "Weekly change:\n{round(estimate*100, 1)}% (95% CI: {round(lower*100,1)}% to {round(upper*100,1)}%)"
  # )

  # subtitle <- str_c(subtitle1, "\n", subtitle2)

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
    min(-0.5, floor(min(c(ranking$lower, ranking$upper)) * 10) / 10),
    max(min(4, ceiling(max(c(ranking$lower, ranking$upper)) * 10) / 10), 1)
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

trendsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      cols <- RColorBrewer::brewer.pal(4, "Set1")
      t.cols <- cols
      for (i in seq_along(cols)) {
        x <- col2rgb(cols[i])
        t.cols[i] <- rgb(x[1, ], x[2, ], x[3, ], alpha = 125, maxColorValue = 255)
      }

      pars <- reactive({
        time_window <- input$time_window
        delete <- c(
          cases = 3,
          hospitalizations = 5,
          icu = 0,
          deaths = 5
        )
        lastday <- input$lastday
        begin <- lastday - delete + 1 - time_window
        end <- lastday - delete

        pars <- list(
          time_window = time_window,
          delete = delete,
          lastday = lastday,
          begin = begin,
          end = end
        )

        return(pars)
      })

      newestBAGfile <- reactive({
        bagFiles <- tibble(
          path = list.files("data/BAG",
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
      })

      newestICUfile <- reactive({
        icuFiles <- tibble(
          path = list.files("data/ICU",
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
      })

      output$lastDataUpdate <- renderUI({
        updateDate <- strptime(
          stringr::str_match(newestBAGfile(), ".*\\/(\\d*-\\d*-\\d*)_\\d*-\\d*-\\d*")[, 2],
          format = "%Y-%m-%d"
        )
        HTML(glue::glue("<b>Last Data Update:</b> {updateDate}"))
      })

      bagData <- reactive({
        newestBAGfile <- newestBAGfile()
        bagData <- readRDS(newestBAGfile) %>% as_tibble()
        return(bagData)
      })

      icuData <- reactive({
        newestICUfile <- newestICUfile()
        pars <- pars()

        icuDataRaw <- read_csv(newestICUfile, col_types = cols(
          .default = col_double(),
          Date = col_date(format = "")
        ))
        icuDataRaw$CH <- rowSums(icuDataRaw[, 2:27], na.rm = TRUE)

        icuData <- icuDataRaw %>%
          pivot_longer(cols = -Date, names_to = "region", values_to = "count") %>%
          transmute(
            region = region,
            age_class = "all",
            date = Date,
            event = "icu",
            count = count,
            weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0)
          ) %>%
          filter(!is.na(count)) %>%
          filter(date >= pars$begin["icu"] & date <= pars$end["icu"])

        return(icuData)
      })

      eventCounts <- reactive({
        bagData <- bagData()
        icuData <- icuData()
        pars <- pars()

        eventCountsList <- list()
        eventCountsList$cases <- bagData %>% getEventCounts(fall_dt, "cases", pars)
        eventCountsList$hospitalizations <- bagData %>%
          getEventCounts(hospdatin, "hospitalizations", pars)
        eventCountsList$deaths <- bagData %>%
          getEventCounts(pttoddat, "deaths", pars) %>%
          filter(region == "CH", age_class == "all")
        eventCountsList$icu <- icuData %>%
          filter(region == "CH")

        eventCounts <- bind_rows(eventCountsList)

        return(eventCounts)
      })

      modelFunction <- function(df) {
        MASS::glm.nb(count ~ date + weekend, data = df)
      }

      models <- reactive({
        eventCounts <- eventCounts()

        models <- eventCounts %>%
          group_by(region, age_class, event) %>%
          nest() %>%
          mutate(model = map(data, modelFunction))

        return(models)
      })

      predictions <- reactive({
        models <- models()

        predictions <- models %>%
          mutate(
            prediction = map(model, ~ exp(predict(.x))),
            lower = map(model, ~ qnbinom(0.025, mu = exp(predict(.x)), size = .x$theta)),
            upper = map(model, ~ qnbinom(0.975, mu = exp(predict(.x)), size = .x$theta))
          ) %>%
          select(-model) %>%
          unnest(cols = c(data, prediction, upper, lower))
      })

      doublingTimes <- reactive({
        models <- models()

        doublingTimes <- models %>%
          mutate(
            fit.ci = map(model, ~ tryCatch(confint(profile(.x)), error = function(e) {
              matrix(NA, 2, 2)
            })),
            estimate = map_dbl(model, ~ log(2) / coef(.x)[2]),
            lower = map_dbl(fit.ci, ~ log(2) / .x[2, 2]),
            upper = map_dbl(fit.ci, ~ log(2) / .x[2, 1])
          ) %>%
          select(-data, -model, -fit.ci)

        return(doublingTimes)
      })

      ranking <- reactive({
        doublingTimes <- doublingTimes()

        ranking <- doublingTimes %>%
          mutate(across(estimate:upper, ~ exp(log(2) / .x * 7) - 1))

        return(ranking)
      })

      output$countryPlots <- renderPlot({
        countryPlotsList <- list(
          plotPredictions(predictions(), doublingTimes(), ranking(),
            regionSelect = "CH", eventSelect = "cases",
            fillColor = t.cols[4],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH")),
          plotPredictions(predictions(), doublingTimes(), ranking(),
            regionSelect = "CH", eventSelect = "hospitalizations",
            fillColor = t.cols[3],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH")),
          plotPredictions(predictions(), doublingTimes(), ranking(),
            regionSelect = "CH", eventSelect = "icu",
            fillColor = t.cols[2],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: KSD", "Data: KSD")),
          plotPredictions(predictions(), doublingTimes(), ranking(),
            regionSelect = "CH", eventSelect = "deaths",
            fillColor = t.cols[1],
            lang = input$plot_language
          ) + labs(caption = if_else(input$plot_language == "de", "Daten: BAG", "Data: FOPH"))
        )
        plot <- plot_grid(plotlist = countryPlotsList, ncol = 2)
        # ggsave("www/pdf/trends_CHE.pdf", plot, width = 16, height = 9)
        return(plot)
      })

      output$cantonPlotsCases <- renderPlot({
        predictions <- predictions()
        doublingTimes <- doublingTimes()
        ranking <- ranking()

        plot <- plotCantons(predictions, doublingTimes, ranking, eventSelect = "cases", color = t.cols[4],
          sorting = input$cantonSortCases, lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_regional_cases.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$cantonPlotsHospitalizations <- renderPlot({
        predictions <- predictions()
        doublingTimes <- doublingTimes()
        ranking <- ranking()

        plot <- plotCantons(predictions, doublingTimes, ranking, eventSelect = "hospitalizations",color = t.cols[3],
          sorting = input$cantonSortHospitalizations, lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_regional_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$ageClassPlotsCases <- renderPlot({
        predictions <- predictions()
        doublingTimes <- doublingTimes()
        ranking <- ranking()

        plot <- plotAgeClass(predictions, doublingTimes, ranking, eventSelect = "cases", color = t.cols[4],
          lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_age_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$ageClassPlotsHospitalizations <- renderPlot({
        predictions <- predictions()
        doublingTimes <- doublingTimes()
        ranking <- ranking()

        plot <- plotAgeClass(predictions, doublingTimes, ranking, eventSelect = "hospitalizations", color = t.cols[3],
          lang = input$plot_language)

        # ggsave("www/pdf/trends_CHE_age_hospitalizations.pdf", plot, width = 16, height = 25)
        return(plot)
      })

      output$rankingPlotCases <- renderPlot({
        ranking <- ranking() %>% filter(age_class == "all", event == "cases")
        ranking$region <- fct_reorder(ranking$region, ranking$estimate, min)

        plot <- plotRanking(ranking, cols, lang = input$plot_language)
        return(plot)
      })

      output$rankingPlotHospitalizations <- renderPlot({
        ranking <- ranking() %>% filter(age_class == "all", event == "hospitalizations")
        ranking$region <- fct_reorder(ranking$region, ranking$estimate, min)

        plot <- plotRanking(ranking, cols, lang = input$plot_language)
        return(plot)
      })

      comparisonData <- reactive({
        doublingTimesRaw <- doublingTimes()
        rankingRaw <- ranking()
        time_window <- input$time_window

        doublingTimes <- doublingTimesRaw %>%
          transmute(
            region, age_class, event,
            dt_estimate = estimate,
            dt_lower = lower,
            dt_upper = upper)

        ranking <- rankingRaw %>%
          transmute(
            region, age_class, event,
            wc_estimate = estimate * 100,
            wc_lower = lower * 100,
            wc_upper = upper * 100)

        allData <- doublingTimes %>%
          full_join(ranking, by = c("region", "age_class", "event"))
      })

      output$comparisonDataTable <- renderDataTable({
        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(rowspan = 2, "Region"),
              th(rowspan = 2, "Age class"),
              th(rowspan = 2, "Event"),
              th(colspan = 3, "Doubling time (d)"),
              th(colspan = 3, "Weekly change (%)")),
            tr(
              lapply(rep(c("estimate", "lower 95% CI", "upper 95% CI"), 2), th),
            )
          )
        ))

        tableData <- comparisonData() %>%
          mutate(
            region = factor(region, levels = c("CH", "AG", "AI", "AR", "BE", "BL", "BS", "FR",
              "GE", "GL", "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
              "UR", "VD", "VS", "ZG", "ZH", "FL")),
            age_class = factor(age_class),
            event = factor(event)
            ) %>%
          arrange(event, region, age_class)

        table <- datatable(
          tableData,
          rownames = FALSE,
          container = sketch,
          filter = "top",
          options = list(
            dom = "t",
            pageLength = 125,
            lengthMenu = c(25, 50, 100, 125))) %>%
          formatSignif(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper",
              "wc_estimate", "wc_lower", "wc_upper"
            ),
            digits = 4) %>%
          formatStyle(
            columns = c(
              "dt_estimate", "dt_lower", "dt_upper"),
            backgroundColor = c("#f0f8ffab")) %>%
          formatStyle(
            columns = c("dt_estimate", "dt_lower", "dt_upper"),
            target = "cell",
            color = styleInterval(0, c("green", "red"))
          )
        return(table)
      })

      output$tableCaption <- renderUI({
        HTML("<p>Doubling time and weekly change in doubling time from a negative binomial generalized linear model. ",
            "<p>Use filter fields to filter columns. Click on column names to sort. Shift-Click to sort by multiple columns.</p>")
      })

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("trendsComparison.csv", sep = "")
        },
        content = function(file) {
          write.csv(comparisonData(), file, row.names = FALSE)
        }
      )

      returnData <- reactive({
        returnData <- list(
          counts = eventCounts(),
          estimates = comparisonData()
        )
      })

      return(returnData)
    }
  )
}

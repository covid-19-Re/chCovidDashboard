library(MASS)
library(tidyverse)
library(lubridate)
library(cowplot)


getEventCounts <- function(df, event_dt, event_name, pars) {
  # CH
  countsCH <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    filter({{ event_dt }} >= pars$begin & {{ event_dt }} <= pars$end) %>%
    group_by({{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = "CH", date = {{ event_dt }}, event = event_name, count = count)

  # cantons
  countsRegions <- df %>%
    dplyr::select({{ event_dt }}, ktn) %>%
    filter({{ event_dt }} >= pars$begin & {{ event_dt }} <= pars$end) %>%
    group_by(ktn, {{ event_dt }}) %>%
    summarize(
      count = n(),
      .groups = "drop"
    ) %>%
    transmute(region = ktn, date = {{ event_dt }}, event = event_name, count = count)

  # all
  counts <- bind_rows(countsCH, countsRegions) %>%
    complete(region, date, event, fill = list(count = 0)) %>%
    mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7, 1, 0))

  return(counts)
}

plotPredictions <- function(predictions, doublingTimes, ranking, regionSelect, eventSelect, fillColor) {
  plotData <- filter(predictions, region == regionSelect, event == eventSelect)
  doublingTimesData <- filter(doublingTimes, region == regionSelect, event == eventSelect)
  ranking <- filter(ranking, region == regionSelect, event == eventSelect)

  title <- case_when(
    regionSelect != "CH" ~ regionSelect,
    eventSelect == "cases" ~ "Confirmed cases",
    eventSelect == "hospitalizations" ~ "Hospitalizations",
    eventSelect == "deaths" ~ "Deaths",
  )


  # subtitle <- glue::glue_data(doublingTimesData,
  #   "{round(estimate, 1)} d (95% CI: {round(lower,1)} to {round(upper,1)}d)")
  subtitle <- glue::glue_data(
    ranking,
    "Weekly change:\n{round(estimate*100, 1)}% (95% CI: {round(lower*100,1)}% to {round(upper*100,1)}%)"
  )

  # subtitle <- str_c(subtitle1, "\n", subtitle2)

  plot <- ggplot(
    data = plotData
  ) +
    geom_ribbon(mapping = aes(date, prediction, ymin = lower, ymax = upper), fill = fillColor) +
    geom_point(mapping = aes(date, count), shape = 21) +
    geom_line(mapping = aes(date, prediction)) +
    scale_y_continuous(name = title) +
    coord_cartesian(ylim = c(0, 1.5 * max(plotData$count))) +
    scale_x_date(name = "", date_breaks = "1 week", date_labels = "%b-%d") +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    plotTheme

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

      cols <- RColorBrewer::brewer.pal(3, "Set1")
      t.cols <- cols
      for (i in seq_along(cols)) {
        x <- col2rgb(cols[i])
        t.cols[i] <- rgb(x[1, ], x[2, ], x[3, ], alpha = 125, maxColorValue = 255)
      }

      newestBAGFile <- reactive({
        bagFiles <- list.files("data/BAG",
          pattern = "*FOPH_COVID19_data_extract.rds",
          full.names = TRUE,
          recursive = TRUE
        )
        bagFileDates <- strptime(
          stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
          format = "%Y-%m-%d_%H-%M-%S"
        )
        newestBAGFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]
        return(newestBAGFile)
      })

      output$lastDataUpdate <- renderUI({
        updateDate <- strptime(
          stringr::str_match(newestBAGFile(), ".*\\/(\\d*-\\d*-\\d*)_\\d*-\\d*-\\d*")[, 2],
          format = "%Y-%m-%d"
        )
        HTML(glue::glue("<b>Last Data Update:</b> {updateDate}"))
      })

      bagData <- reactive({
        newestBAGFile <- newestBAGFile()
        bagData <- readRDS(newestBAGFile)
        return(bagData)
      })

      pars <- reactive({
        time_window <- input$time_window
        delete <- input$truncation
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

      eventCounts <- reactive({
        bagData <- bagData()
        pars <- pars()

        eventCountsList <- list()
        eventCountsList$cases <- bagData %>% getEventCounts(fall_dt, "cases", pars)
        eventCountsList$hospitalizations <- bagData %>%
          getEventCounts(hospdatin, "hospitalizations", pars) %>%
          filter(region == "CH")
        eventCountsList$deaths <- bagData %>%
          getEventCounts(pttoddat, "deaths", pars) %>%
          filter(region == "CH")

        eventCounts <- bind_rows(eventCountsList)

        return(eventCounts)
      })

      modelFunction <- function(df) {
        glm.nb(count ~ date + weekend, data = df)
      }

      models <- reactive({
        eventCounts <- eventCounts()

        models <- eventCounts %>%
          group_by(region, event) %>%
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
            lower = map_dbl(fit.ci, ~ log(2) / .x[2, 1]),
            upper = map_dbl(fit.ci, ~ log(2) / .x[2, 2])
          ) %>%
          select(-data, -model, -fit.ci)
      })

      ranking <- reactive({
        doublingTimes <- doublingTimes()

        ranking <- doublingTimes %>%
          mutate(across(estimate:upper, ~ exp(log(2) / .x * 7) - 1))

        return(ranking)
      })

      output$chPlotCases <- renderPlot({
        plotPredictions(predictions(), doublingTimes(), ranking(),
          regionSelect = "CH", eventSelect = "cases",
          fillColor = t.cols[2]
        )
      })
      output$chPlotHospitalizations <- renderPlot({
        plotPredictions(predictions(), doublingTimes(), ranking(),
          regionSelect = "CH", eventSelect = "hospitalizations",
          fillColor = t.cols[3]
        )
      })
      output$chPlotDeaths <- renderPlot({
        plotPredictions(predictions(), doublingTimes(), ranking(),
          regionSelect = "CH", eventSelect = "deaths",
          fillColor = t.cols[1]
        )
      })

      output$cantonPlots <- renderPlot({
        predictions <- predictions()
        doublingTimes <- doublingTimes()
        ranking <- ranking()


        if (input$cantonSort == "growthAsc") {
          cantons <- levels(fct_reorder(ranking$region, ranking$estimate))
        } else if (input$cantonSort == "growthDesc") {
          cantons <- levels(fct_reorder(ranking$region, ranking$estimate, .desc = TRUE))
        } else {
          cantons <- ranking$region
        }

        cantonPlotsList <- list()
        for (i in cantons) {
          cantonPlotsList[[i]] <- plotPredictions(predictions, doublingTimes, ranking(),
            regionSelect = i, eventSelect = "cases",
            fillColor = t.cols[2]
          ) +
            theme(text = element_text(size = 12)) +
            labs(title = i)
        }
        return(plot_grid(plotlist = cantonPlotsList, ncol = 4))
      })

      output$rankingPlot <- renderPlot({
        ranking <- ranking() %>% filter(event == "cases")
        ranking$region <- fct_reorder(ranking$region, ranking$estimate, min)

        colors <- rep("black", length(ranking$region))
        names(colors) <- ranking$region
        colors["CH"] <- cols[1]

        plot <- ggplot(
          data = ranking,
          mapping = aes(x = estimate, y = region, xmin = lower, xmax = upper, color = region)
        ) +
          geom_point(size = 2) +
          geom_errorbar(width = 0.4, size = 0.75) +
          scale_x_continuous(name = "Weekly change", labels = scales::percent) +
          coord_cartesian(xlim = c(-0.5, 1)) +
          scale_y_discrete(name = "") +
          scale_color_manual(values = colors, guide = "none") +
          plotTheme

        return(plot)
      })
    }
  )
}

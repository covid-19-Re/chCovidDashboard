library(lubridate)

tsPlotsAgeGroupColors <- tibble(
  ageGroup = c(
    "0-6", "07-12", "13-17", "18-24", "25-34", "35-44", "45-54",
    "55-64", "65-74", "75+", "Unknown"
  ),
  color = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c",
    "#fdbf6f",
    "#ff7f00",
    "#cab2d6",
    "#6a3d9a",
    "#ffff99"
  )
)

tsPlots <- list()


tsPlots$histogram <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  stacked = FALSE,
  i18n = NULL,
  ...  # To allow unused arguments
) {
  if (is.null(groupingAttributeName)) {
    plot <- plot_ly(x = data[[xAttributeName]], y = data[[yAttributeName]], type = "bar",
                    text = data$tooltipText, hoverinfo = 'text')
  } else {
    plot <- plot_ly(type = "bar")
    data %>%
      group_by(!!as.symbol(groupingAttributeName)) %>%
      group_walk(function (x, y) {
        if (groupingAttributeName == "age_group") {
          # For age groups, we define the colors ourselves to make sure that they are consistent
          plot <<- plot %>%
            add_trace(x = x[[xAttributeName]], y = x[[yAttributeName]],
                      name = i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                      text = x$tooltipText, hoverinfo = 'text',
                      marker = list(color = tsPlotsAgeGroupColors$color[match(y[[groupingAttributeName]], tsPlotsAgeGroupColors$ageGroup)]))
        } else {
          plot <<- plot %>%
            add_trace(x = x[[xAttributeName]], y = x[[yAttributeName]],
                      name = i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                      text = x$tooltipText, hoverinfo = 'text')
        }
      })
  }

  plot <- plot %>%
    layout(
      barmode = ifelse(stacked, "stack", "group"),
      bargap = 0.3
    )

  return (plot)
}


tsPlots$line <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  addConfidenceInterval = FALSE,
  ...  # To allow unused arguments
) {
  plot <- plot_ly()

  if (addConfidenceInterval) {
    data <- data %>%
      mutate(
        ymin = replace_na(ymin, 0),
        ymax = replace_na(ymax, 0)
      )
    if (is.null(groupingAttributeName)) {
      plot <- plot %>%
        add_trace(
          x = data[[xAttributeName]], y = data$ymax, hoverinfo = "none",
          type = "scatter", mode = "lines", showlegend = FALSE,
          line = list(color = 'transparent')) %>%
        add_trace(
          x = data[[xAttributeName]], y = data$ymin, hoverinfo = "none",
          type = "scatter", mode = "lines", showlegend = FALSE,
          line = list(color = 'transparent'), fillcolor = "#e1e1e1", fill = "tonexty")
    } else {
      data %>%
        group_by(!!as.symbol(groupingAttributeName)) %>%
        group_walk(function (x, y) {
          plot <<- plot %>%
            add_trace(
              x = x[[xAttributeName]], y = x$ymax, hoverinfo = "none",
              type = "scatter", mode = "lines", showlegend = FALSE,
              line = list(color = 'transparent')) %>%
            add_trace(
              x = x[[xAttributeName]], y = x$ymin, hoverinfo = "none",
              type = "scatter", mode = "lines", showlegend = FALSE,
              line = list(color = 'transparent'), fillcolor = "#e1e1e1", fill = "tonexty")
        })
    }
  }

  if (is.null(groupingAttributeName)) {
    plot <- add_trace(plot, x = data[[xAttributeName]], y = data[[yAttributeName]], type = "scatter", mode = "lines",
                      text = data$tooltipText, hoverinfo = 'text')
  } else {
    data %>%
      group_by(!!as.symbol(groupingAttributeName)) %>%
      group_walk(function (x, y) {
        if (groupingAttributeName == "age_group") {
          # For age groups, we define the colors ourselves to make sure that they are consistent
          plot <<- add_trace(plot, x = x[[xAttributeName]], y = x[[yAttributeName]],
                             name = ts_i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                             text = x$tooltipText, hoverinfo = 'text',
                             type = "scatter", mode = "lines",
                             line = list(color = tsPlotsAgeGroupColors$color[match(y[[groupingAttributeName]], tsPlotsAgeGroupColors$ageGroup)])
          )
        } else {
          plot <<- add_trace(plot, x = x[[xAttributeName]], y = x[[yAttributeName]],
                             name = ts_i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                             text = x$tooltipText, hoverinfo = 'text',
                             type = "scatter", mode = "lines"
          )
        }
      })
  }

  return (plot)
}


tsPlots$area <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  ...  # To allow unused arguments
) {
  plot <- plot_ly(type = 'scatter', mode = 'none', stackgroup = 'one')
  if (is.null(groupingAttributeName)) {
    plot <- add_trace(plot, x = data[[xAttributeName]], y = data[[yAttributeName]],
                      text = data$tooltipText, hoverinfo = 'text')
  } else {
    data %>%
      group_by(!!as.symbol(groupingAttributeName)) %>%
      group_walk(function (x, y) {
        if (groupingAttributeName == "age_group") {
          # For age groups, we define the colors ourselves to make sure that they are consistent
          plot <<- add_trace(plot, x = x[[xAttributeName]], y = x[[yAttributeName]],
                             name = ts_i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                             text = x$tooltipText, hoverinfo = 'text',
                             fillcolor  = tsPlotsAgeGroupColors$color[match(y[[groupingAttributeName]], tsPlotsAgeGroupColors$ageGroup)]
          )
        } else {
          plot <<- add_trace(plot, x = x[[xAttributeName]], y = x[[yAttributeName]],
                             name = ts_i18n$t(paste0("ts.constant.", groupingAttributeName, ".", y[[groupingAttributeName]])),
                             text = x$tooltipText, hoverinfo = 'text')
        }
      })
  }
  return (plot)
}


# CH Map
# The file was downloaded from https://gist.github.com/mbostock/4207744.
# Supposedly, it was created with interactivethings/swiss-maps (https://github.com/interactivethings/swiss-maps)
# and uses data of the Federal Office of topography swisstopo as source.
fileName <- "./data/switzerland-geo.json"
geojsonData <- readChar(fileName, file.info(fileName)$size)
chMapData <- sf::st_read(geojsonData, quiet = TRUE) %>%
  mutate(canton = id)


# World Map
worldMapData <- rnaturalearth::ne_countries(returnclass = "sf")


#' Creates a map plot of Switzerland's cantons
#'
#' @param plotData (tibble)
#' @param region (character, length=1) - the possible values are: "switzerland" and "world"
#' @return (A plotly object)
tsPlots$map <- function (
  data, region,
  ...  # To allow unused arguments
) {
  if (region == "switzerland") {
    mapData <- chMapData %>%
      inner_join(data, by = "canton", na_matches = "never")
    return (
      plot_ly(mapData, split = ~tooltipText, color = ~count, showlegend = FALSE,
              # It should not matter too much which number is chosen here.
              colors = rev(colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(10)),
              alpha = 1, span = I(1), stroke = I("black")
      )
    )
  }

  if (region == "world") {
    mapData <- worldMapData %>%
      left_join(data, by = c("iso_a3" = "exp_land_code"), na_matches = "never") %>%
      mutate(tooltipText = coalesce(tooltipText, iso_a3))
    return (
      plot_ly(mapData, split = ~tooltipText, color = ~count, showlegend = FALSE,
              # It should not matter too much which number is chosen here.
              colors = rev(colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(10)),
              alpha = 1, span = I(1), stroke = I("black")
      )
    )
  }
}


finalize_plot <- function (plot_def, model, language) {
  i18n <- Translator$new(translation_json_path = "data/ts-translations/ts-translations.json")
  i18n$set_translation_language(language)
  plot_def$i18n <- i18n

  comparison_info <- get_comparison_info(model)

  plotlyPlot <- do.call(tsPlots[[model$plot_type]], plot_def)
  if (model$plot_type != "map") {
    plotlyPlot <- plotlyPlot %>%
      layout(
        xaxis = list(title = i18n$t("ts.plot.date")),
        yaxis = list(title = plot_def$ylab)
      )
    if (model$display$log_scale) {
      plotlyPlot <- plotlyPlot %>%
        layout(
          yaxis = list(type = "log")
        )
    }
    if (comparison_info$is_comparing) {
      plotlyPlot <- plotlyPlot %>%
        layout(legend = list(title = list(
          text = paste0("<b>", i18n$t(paste0("ts.constant.", comparison_info$compare_attribute, ".label")), "</b>"))))
    } else {
      plotlyPlot <- plotlyPlot %>%
        layout(showlegend = FALSE)
    }
  }

  plotlyPlot <- plotlyPlot %>%
    config(
      displaylogo = FALSE,
      modeBarButtons = list(list("zoom2d", "toImage", "resetScale2d", "pan2d")),
      toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
    )

  if (model$plot_type != "map") {
    # The data from the most recent few days are subject to change due to reporting delays.
    numberUncertainDays <- 2
    if (model$general$event == "Hospitalisation"
      || model$general$event == "ICU admission (unreliable)"
      || model$general$event == "Death" ||
      (model$general$display_prob && (
        model$general$given == "Hospitalisation"
          || model$general$given == "ICU admission (unreliable)"
          || model$general$given == "Death"))
    ) {
      numberUncertainDays <- 5
    }

    # Find out the max. height
    plot_data <- plot_def[[1]]
    x_attribute <- plot_def[[2]]
    y_attribute <- plot_def[[3]]
    if (
      comparison_info$is_comparing &&
        (model$plot_type == "area" || (model$plot_type == "histogram" && model$display$stack_histograms))
    ) {
      tmp <- plot_data %>%
        group_by(!!as.symbol(x_attribute)) %>%
        summarize(height_sum = sum(!!as.symbol(y_attribute)), .groups = "drop")
      max_y_value <- max(tmp$height_sum, na.rm = TRUE)
    } else {
      max_y_value <- max(plot_data[[y_attribute]], na.rm = TRUE)
    }

    plotlyPlot <- layout(
      plotlyPlot,
      shapes = list(
        list(type = "rect",
             fillcolor = "gray", line = list(color = "gray"), opacity = 0.3,
             x0 = plot_def$max_date - numberUncertainDays, x1 = plot_def$max_date + 5, xref = "x",
             y0 = 0, y1 = max_y_value, yref = "y")))
  }

  return(plotlyPlot)
}


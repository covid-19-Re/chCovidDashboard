library(RColorBrewer)
library(lubridate)
library(rnaturalearth)
library(rgeos)


tsPlots <- list()


tsPlots$histogram <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  stacked = FALSE,
  ylab = NULL,
  ...  # To allow unused arguments
) {
  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText,
                                  fill = !!as.symbol(groupingAttributeName)))
  }

  plot <- plot +
    geom_histogram(stat = "identity", position = (if (stacked) "stack" else "dodge")) +
    ylab(ylab)

  return (plot)
}


tsPlots$line <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  ylab = NULL,
  addConfidenceInterval = FALSE,
  ...  # To allow unused arguments
) {
  if (addConfidenceInterval) {
    data <- data %>%
      mutate(
        ymin = replace_na(ymin, 0),
        ymax = replace_na(ymax, 0)
      )
  }

  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText,
                             group = 1))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText,
                                            group = 1, color = !!as.symbol(groupingAttributeName)))
  }

  plot <- plot +
    geom_line() +
    ylab(ylab)
  if (addConfidenceInterval) {
    plot <- plot + geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15)
  }

  return (plot)
}


tsPlots$area <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  ylab = NULL,
  ...  # To allow unused arguments
) {
  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText, group = 1))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName), text = tooltipText, group = 1,
                             fill = !!as.symbol(groupingAttributeName)))
  }

  plot <- plot +
    geom_area() +
    ylab(ylab)

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
worldMapData <- ne_countries(returnclass = "sf")


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
              colors = rev(colorRampPalette(brewer.pal(10,"RdYlGn"))(10)),
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
              colors = rev(colorRampPalette(brewer.pal(10,"RdYlGn"))(10)),
              alpha = 1, span = I(1), stroke = I("black")
      )
    )
  }
}




finalize_plot <- function (plot_def, model) {
  if (model$general$display_prob) {
    xlabel <- "Date of Test"
  } else {
    xlabel <- paste("Date of", model$general$event)
  }
  comparison_info <- get_comparison_info(model)

  p <- do.call(tsPlots[[model$plot_type]], plot_def)
  if (model$plot_type != "map") {
    # Finalize ggplot and transform to plotly
    p <- p +
      xlab(xlabel) +
      scale_x_date(date_breaks = "months", labels = date_format("%m-%Y")) +
      theme_light()
    if (model$display$log_scale) {
      p <- p + scale_y_log10()
    }
    if (comparison_info$is_comparing) {
      p <- p +
        guides(fill = guide_legend(title = comparison_info$compare_attribute),
               color = guide_legend(title = comparison_info$compare_attribute))
    }
    p <- ggplotly(p, tooltip = "text")
  }

  # Draw the plot
  plotlyPlot <- p %>%
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

    # Annotating in ggplot2 did not work as it was not transferred. Calling the layout() of plotly also failed
    # (see https://stackoverflow.com/a/50361382). Therefore, this solution:
    todayDaysSince1970 <- as.integer(plot_def$max_date)
    plotlyPlot[['x']][['layout']][['shapes']] <- list(
      list(type = "rect",
           fillcolor = "grey", line = list(color = "gray"), opacity = 0.2,
           # Inf and -Inf don't work here.
           x0 = todayDaysSince1970 - (numberUncertainDays + 0.5), x1 = todayDaysSince1970 + 100, xref = "x",
           y0 = -99999999, y1 = 99999999, yref = "y")
    )
  }

  return(plotlyPlot)
}


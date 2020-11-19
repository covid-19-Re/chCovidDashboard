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
chMapData <- sf::st_read(geojsonData) %>%
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
      left_join(data, by = c("iso_a3" = "expCountryCode"), na_matches = "never") %>%
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

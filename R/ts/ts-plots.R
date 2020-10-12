library(RColorBrewer)
library(lubridate)


tsPlots <- list()


tsPlots$histogram <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  stacked = FALSE,
  ylab = NULL
) {
  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName)))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName),
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
  ylab = NULL
) {
  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName)))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName),
                             color = !!as.symbol(groupingAttributeName)))
  }

  plot <- plot +
    geom_line() +
    ylab(ylab)

  return (plot)
}


tsPlots$area <- function (
  data, xAttributeName, yAttributeName,
  groupingAttributeName = NULL,
  ylab = NULL
) {
  if (is.null(groupingAttributeName)) {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName)))
  } else {
    plot <- ggplot(data, aes(x = !!as.symbol(xAttributeName), y = !!as.symbol(yAttributeName),
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
fileName <- "./switzerland-geo.json"
geojsonData <- readChar(fileName, file.info(fileName)$size)
chMapData <- sf::st_read(geojsonData) %>%
  mutate(canton = id)

#' Creates a map plot of Switzerland's cantons
#'
#' @param plotData (tibble) - plotData must contains these two columns: canton and count
#' @return (A plotly object)
tsPlots$map <- function (
  data
) {

  mapData <- chMapData %>%
    inner_join(data, by = "canton")

  return (
    plot_ly(mapData, split = ~canton, color = ~count, showlegend = FALSE,
            # It should not matter too much which number is chosen here.
            colors = rev(colorRampPalette(brewer.pal(10,"RdYlGn"))(10)),
            alpha = 1, span = I(1), stroke = I("black")
    )
  )
}

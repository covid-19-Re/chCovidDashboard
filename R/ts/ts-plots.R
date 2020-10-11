library(RColorBrewer)


tsPlots <- list()

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
tsPlots$switzerlandMap <- function (plotData) {

  mapData <- chMapData %>%
    inner_join(plotData, by = "canton")

  return (
    plot_ly(mapData, split = ~canton, color = ~count, showlegend = FALSE,
            # It should matter too much which number is chosen here.
            colors = rev(colorRampPalette(brewer.pal(10,"RdYlGn"))(10)),
            alpha = 1, span = I(1), stroke = I("black")
    )  %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage", "resetScale2d")),
        toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
      )
  )
}

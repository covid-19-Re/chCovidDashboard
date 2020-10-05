plotTheme <- theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 16)
  )

plotlyTheme <- theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 12)
  )

makePlotly <- function(p, height = 450, show_legend = FALSE, legend_title = NULL) {
  ggplotly(p + plotlyTheme, height = height, tooltip = c("text")) %>%
    layout(
      showlegend = show_legend,
      legend = list(
        title = list(text = legend_title),
        x = 0.5, y = -0.3,
        xanchor = "center", yanchor = "middle",
        orientation = "h"),
      font = list(family = "sans-serif"),
      yaxis = list(fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    ) %>%
    config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
}

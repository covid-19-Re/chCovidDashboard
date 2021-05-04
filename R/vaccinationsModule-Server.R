library(tidyverse)
library(plotly)

vaccServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      vaccinationData <- reactive({
        vaccinationData <- qs::qread("data/vaccinationData.qs") %>%
          mutate(
            geoRegion = fct_relevel(geoRegion, c("CHFL", "CH", "FL"))
          )
      })

      vaccinationDataByAge <- reactive({
        vaccinationDataByAge <- qs::qread("data/vaccinationDataByAge.qs") %>%
          mutate(
            geoRegion = fct_relevel(geoRegion, c("CHFL", "CH", "FL"))
          )
      })
      
      vaccinationAxisTitle <- reactive({
        vaccinationAxisTitle <- c(
          "nFullyVaccTotal" = "# fully vaccinated people",
          "nPartiallyVaccTotal" = "# of partially vaccinated people",
          "nPartiallyOrFullyVacTotal" = "# of partially or fully vaccinated people",
          "nDosesAdminTotal" = "# of administred doses people"
        )[input$dataType]

        if (input$normalisation) {
          vaccinationAxisTitle <- str_c(vaccinationAxisTitle, " / 100 people")
        }
        return(vaccinationAxisTitle)
      })

      output$timelinePlot <- renderPlotly({
        vaccinationData <- vaccinationData()
        vaccinationAxisTitle <- vaccinationAxisTitle()

        plotData <- vaccinationData %>%
          filter(
            name == input$dataType,
            geoRegion != "CH")

        if (input$normalisation) {
          yDataFormula <- ~valueP100
        } else {
          yDataFormula <- ~value
        }

        timelinePlot <- plot_ly() %>%
          add_trace(
            data = filter(plotData, geoRegion != "CHFL"),
            x = ~date, y = yDataFormula, color = ~geoRegion,
            type = "scatter", mode = "lines",
            legendgroup = ~geoRegion, showlegend = TRUE,
            text = ~str_c("<i>", format(date, "%Y-%m-%d"),
              "</i> ", geoRegion, " <br>",
              round(valueP100, 2), " / 100<br>",
              "Total: ", round(value, 2),
             "<extra></extra>"),
            hovertemplate = "%{text}") %>%
          add_trace(
            data = filter(plotData, geoRegion == "CHFL"),
            x = ~date, y = yDataFormula, color = ~geoRegion, line = list(width = 4,  color = "black"),
            type = "scatter", mode = "lines",
            legendgroup = ~geoRegion, showlegend = TRUE,
            text = ~str_c("<i>", format(date, "%Y-%m-%d"),
              "</i> ", geoRegion, " <br>",
              round(valueP100, 2), " / 100<br>",
              "Total: ", round(value, 2),
             "<extra></extra>"),
            hovertemplate = "%{text}") %>%
          layout(
            yaxis = list(title = vaccinationAxisTitle)
          ) %>%
          config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1,
              filename = "vaccinations"),
            scrollZoom = FALSE, responsive = TRUE)
      })

      vaccBarColors <- c(
        "pop" = "grey",
        "value" = "darkgreen",
        "valueP100" = "darkgreen"
      )

      output$plotByAge <- renderPlotly({
        vaccinationDataByAge <- vaccinationDataByAge()
        vaccinationAxisTitle <- vaccinationAxisTitle()

        plotData <- vaccinationDataByAge %>%
          filter(!(geoRegion %in% c("CH", "CHFL"))) %>%
          group_by(geoRegion, ageClass, name) %>%
          filter(date == max(date)) %>%
          ungroup() %>%
          filter(name == input$dataType) %>%
          mutate(
            tooltip = str_c(
              ageClass, "<br>",
              round(valueP100, 2), " / 100 <br>",
              "(", value, " / ", pop, ")")
          ) %>%
          pivot_longer(cols = c(pop, value, valueP100), names_to = "type")

        if (input$normalisation) {
          plotData <- filter(plotData, type == "valueP100")
          scalesType <- "fixed"
        } else {
          plotData <- filter(plotData, type != "valueP100")
          scalesType <- "free_x"
        }

        plot <- ggplot(
          data = plotData,
          mapping = aes(y = ageClass, x = value, fill = type, text = tooltip)
        ) +
        facet_wrap(vars(geoRegion), scales = scalesType) +
        geom_col(position = position_identity()) +
        scale_fill_manual(
          guide = "none",
          name = NULL,
          values = vaccBarColors) +
        labs(x = vaccinationAxisTitle, y = NULL)

        if (input$normalisation & input$dataType != "nDosesAdminTotal") {
          plot <- plot +
            coord_cartesian(xlim = c(0, 100))
        }

        ggplotly(plot, tooltip = "text") %>%
          layout(showlegend = FALSE, margin = list(b = 75)) %>%
          config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
              toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1,
                filename = "vaccinations"),
              scrollZoom = FALSE, responsive = TRUE)

      })
    }
  )
}

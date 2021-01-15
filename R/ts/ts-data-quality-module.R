source("R/utilities.R")
source("R/ts/ts-constants.R")
source("R/ts/ts-view_model.R")
source("R/ts/ts-data_store.R")
source("R/ts/ts-plots.R")


tsDataQualityAnalyzedAttributes <- c(
  "sex", "expContactPath", "quarantBeforePositiveTest", "labReason", "expCountryName"
)


tsDataQualityUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    fluidPage(
      bootstrapPanel(
        heading = "Attributes in Comparison", class = "panel-primary",
        plotlyOutput(ns("attrPlot"), height = "600px")
      ),
      bootstrapPanel(
        heading = "Cantons in Comparison", class = "panel-primary",
        fluidRow(
          column(3,
                 radioButtons(
                   inputId = ns("at"),
                   label = "Select an attribute",
                   choices = names(basicFilters),
                   selected = names(basicFilters)[1]
                 )
          ),
          column(
                 9,
                 "Proportion of missing data",
                 plotlyOutput(ns("cantonPlot"), height = "600px")
          )
        )
      )
    )
  )
}


tsDataQualityServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      data_store <- ts_data_store  # Provided in ts-server.R

      output$attrPlot <- renderPlotly({
        base_query <- data_store$load_main_data() %>%
          mutate(date = fall_dt) %>%
          filter(positive_test)
        min_date <- ymd(20200219)
        dashboard_state <- data_store$load_dashboard_state() %>% collect()
        max_date <- dashboard_state$last_data_update

        plotData <- NULL
        for (at in names(basicFilters)) {
          d <- base_query %>%
            mutate(missing = as.integer(
              is.na(!!as.symbol(at)) | !!as.symbol(at) == 'Unknown'
            )) %>%
            dplyr::group_by(date) %>%
            dplyr::summarize(count = as.double(sum(missing)) / n()) %>%
            collect() %>%
            complete(date = seq.Date(min_date, max_date, by = "day"), fill = list(count = 0))


          d$count <- slide_index_dbl(d$count, d$date, mean, .before = lubridate::days(14))
          d <- d %>%
            mutate(
              proportion = count,
              tooltipText = paste0(at, "\nProportion: ", proportion)
            )
          d$attribute <- at
          plotData <- bind_rows(plotData, d)
        }

        p <- tsPlots$line(plotData, "date", "proportion", groupingAttributeName = "attribute")
        plotlyPlot <- p %>%
          layout(
            yaxis = list(title = "Proportion of missing data")
          ) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("zoom2d", "toImage", "resetScale2d", "pan2d")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )

        return (plotlyPlot)
      })

      output$cantonPlot <- renderPlotly({
        data_store <- ts_data_store  # Provided in ts-server.R

        query <- data_store$load_main_data() %>%
          mutate(date = fall_dt) %>%
          filter(positive_test)

        at <- input$at

        plotData <- query %>%
          mutate(missing = as.integer(
            is.na(!!as.symbol(at)) | !!as.symbol(at) == 'Unknown'
          )) %>%
          dplyr::group_by(canton) %>%
          dplyr::summarize(count = as.double(sum(missing)) / n()) %>%
          dplyr::mutate(tooltipText = paste0(canton, "\nCount: ", round(count * 100, digits = 4), "%")) %>%
          collect()

        p <- tsPlots$map(plotData, "switzerland")
        plotlyPlot <- p %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("zoom2d", "toImage", "resetScale2d", "pan2d")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )

        return (plotlyPlot)
      })

    }
  )
}

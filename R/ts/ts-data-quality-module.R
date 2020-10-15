source("R/utilities.R")
source("R/ts/ts-constants.R")
source("R/ts/ts-utils.R")
source("R/ts/ts-load_and_process_data.R")
source("R/ts/ts-plots.R")


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
                   choices = c(
                     "sex", "expContactPath", "quarantBeforePositiveTest", "labReason", "expCountryName"
                   ),
                   selected = "sex"
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

      data <- reactive({
        d <- load_and_process_data()

        # Introduces an ID to make the rows unique and easier identifiable.
        d <- cbind(id = as.integer(rownames(d)), d)

        d <- d %>%
          mutate(date = fall_dt) %>%
          filter(positiveTest)

        return(d)
      })

      attrNames <- c(
        "sex", "expContactPath", "quarantBeforePositiveTest", "labReason", "expCountryName"
      )

      output$attrPlot <- renderPlotly({
        dataProc <- data()
        minDate <- min((dataProc %>% drop_na(date))$date)
        maxDate <- max((dataProc %>% drop_na(date))$date)

        plotData <- NULL
        for (at in attrNames) {
          d <- dataProc %>%
            dplyr::mutate(missing = as.integer(
              are_na(!!as.symbol(at)) | replace_na(!!as.symbol(at) %in% c("Unknown", " not filled", "Not filled"), FALSE)
            )) %>%
            dplyr::group_by(date) %>%
            dplyr::summarize(count = sum(missing) / n(), .groups = "drop") %>%
            tidyr::complete(date = seq.Date(minDate, maxDate, by = "day"), fill = list(count = 0))


          d$count <- slide_index_dbl(d$count, d$date, mean, .before = lubridate::days(14))
          d <- d %>% mutate(proportion = count)
          d$attribute <- at
          plotData <- bind_rows(plotData, d)
        }

        p <- tsPlots$line(plotData, "date", "proportion", groupingAttributeName = "attribute",
                          ylab = "Proportion of missing data")
        p <- p + theme_light()
        plotlyPlot <- ggplotly(p) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("zoom2d", "toImage", "resetScale2d", "pan2d")),
            toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1)
          )

        return (plotlyPlot)
      })

      output$cantonPlot <- renderPlotly({
        dataProc <- data()

        at <- input$at

        plotData <- dataProc %>%
          dplyr::mutate(missing = as.integer(
            are_na(!!as.symbol(at)) | replace_na(!!as.symbol(at) %in% c("Unknown", " not filled", "Not filled"), FALSE)
          )) %>%
          dplyr::group_by(canton) %>%
          dplyr::summarize(count = sum(missing) / n(), .groups = "drop")

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

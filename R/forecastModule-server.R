library(shiny)
library(zoo)
library(plotly)
library(shinyjs)
library(plyr)

source("R/forecastModule-Files/scripts/plotting_scripts.R")
source("R/forecastModule-Files/scripts/define_constants.R")
source("R/forecastModule-Files/scripts/ts_scripts.R")

# Define server logic
forecastServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            # suppress warnings
            storeWarn <- getOption("warn")
            options(warn = -1)

            output$FOPH_text <- renderText({
                "Swiss Federal Office of Public Health (FOPH)"})
            output$CORONA_text <- renderText({
                "corona-data.ch"})

            pulled_data <- reactive({
                # Import the data scraped from BAG and corona-data.ch
                pulled_data <- read.table(file = "data/forecastModule-Data.txt", sep = "\t",
                                header = TRUE, stringsAsFactors = FALSE)
                pulled_data$day_reported <- ymd(pulled_data$day_reported)

                return(pulled_data)
            })

            m_date <- reactive({
                m_date <- as.Date(file.info("data/forecastModule-Data.txt")$mtime) - 4
            })

            arima_res <- reactive({
                run_arima_all_date(pulled_data(), as.Date(input$date_model_train),
                    7, TRUE, TRUE)
            })
            
            # Plot the monitoring and forecasting plots
            output$ARIMA_analysis <- renderPlotly({
                # Render plots for the selected time series and data sources
                if(!is.null(input$check_source) & !is.null(input$radio_ts)) {
                    figure <- render_plots_ts(arima_res(), input$check_source,
                                            input$radio_ts, input$radio_disp,
                                            input$check_model, input$check_conf,
                                            m_date())
                } else {
                    figure <- NULL
                }
                # figure
                p3 <- ggplotly(figure)
                p4 <- p3 %>% style(p3, showlegend = FALSE)
                p4
            })

            # Deactivate model uncertainty display option if no model is shown
            observeEvent(input$radio_model, {
                if (input$radio_model == "1") {
                    updateCheckboxInput(session, "check_conf", value = FALSE)
                    disable("check_conf")
                } else {
                    enable("check_conf")
                }
            })

            observeEvent(input$link_to_methods, {
                updateTabsetPanel(session, "main_menu", "Methods")
            })

            observeEvent(input$link_to_about, {
                updateTabsetPanel(session, "main_menu", "About")
            })

            observe_helpers(session = shiny::getDefaultReactiveDomain())
                            # help_dir = "helpfiles", withMathJax = FALSE)

            # Download data tables
            output$save_ts <- downloadHandler(
                filename = function() {paste("COVID-19_Dashboard_Data.csv")},
                content = function(file) {
                    write.csv(arima_res(), file, row.names = FALSE)
                }
            )
        }
    )
}

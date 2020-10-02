library(shiny)
library(plotly)
library(shinyhelper)



forecastUI <- function(id) {
  ns <- NS(id)
	# Retrieve the date of the last file change (date of the last data point)
	current_date <- file.info("data/forecastModule-Data.txt")$mtime
	# Remove the 4 days where the data is incomplete
	max_date <- as.Date(current_date) - 4
  tagList(
    tags$head(HTML("<title>COVID-19 Forecast</title>")),
    shinyjs::useShinyjs(),
    navbarPage(
      id = ns("main_menu"), title = div(
        img(
          src = "forecastModule-coronaLogo.png",
          height = 29, width = 34
        ),
        "COVID-19 Forecast"
      ), inverse = TRUE,
      tabPanel("Home",
        # style = "width: 1200px;",
        fluidRow(
          align = "right",
          tags$img(src = "forecastModule-logo_TPH.png", width = 97.8, height = 28.114),
          tags$img(
            src = "forecastModule-logo_UniBas.png", width = 93.8, height = 30.8,
            style = "margin-left: 25px;"
          )
        ),
        titlePanel(h3("Live time-series analysis to monitor and forecast the COVID-19 outbreak in Switzerland")),
        p(
          "This resource provides real-time analysis of time series data describing the COVID-19 outbreak in Switzerland 
           using statistical models to interpret trends and forecast short-term development (up to 7 days forecast from last training date).
           The left-hand panel allows the user to change visualisation settings, including last date for training the forecast (allowing validation) and data (cases, hospitalizations or fatalities).
           To find out more about the functionalities of the dashboard, check the help icons associated to each panel. 
           You can read more about the methods", actionLink(ns("link_to_methods"), "here"),
          "and about the resource developers", actionLink(ns("link_to_about"), "here"), "."
        ),
        tags$br(),
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          # Sidebar panel for inputs ----
          sidebarPanel(
            width = 2, height = 10,
            helper(tags$br(),
              icon = "question-circle",
              colour = "black",
              type = "markdown",
              content = "ActionTabHelp"
            ),
            h5("Visualization settings:"),
            hr(style = "background-color: black; height: 1px; border: 0;"),
            # wellPanel()
            checkboxGroupInput(ns("check_source"),
              label = "Data sources",
              choices = list(
                "FOPH" = 1,
                "corona-data.ch" = 2
              ),
              selected = c(1, 2)
            ),
            radioButtons(ns("radio_ts"),
              label = "Displayed data outputs",
              choices = list(
                "New cases" = 1,
                "New hospitalizations" = 2,
                "Current hospitalizations" = 3,
                "New fatalities" = 4
              ),
              selected = 1
            ),
            radioButtons(ns("radio_disp"),
              label = "Data smoothing",
              choices = list(
                "Raw data" = 1,
                "Smoothed" = 2
              ),
              selected = 1
            ),
            h5(strong("Time series model:")),
            checkboxInput(ns("check_model"), label = "Show model", value = TRUE),
            checkboxInput(ns("check_conf"), label = "Show model uncertainty", value = TRUE),
            dateInput(ns("date_model_train"),
              label = h5("Training until date:"),
              value = max_date,
              max = max_date
            ),
            hr(),
            downloadButton(ns("save_ts"), label = "Download data")
          ),

          # Main panel for displaying outputs ----
          mainPanel(
            width = 10,
            tabsetPanel(
              id = "analysis_tabs",
              tabPanel(
                "Monitoring and forecasting",
                helper(tags$br(),
                  icon = "question-circle",
                  colour = "black",
                  type = "markdown",
                  content = "PlotTabHelp_arima"
                ),
                p(
                  "The figure below displays the results of time series analyses
                                                      performed on data provided by the ",
                  span(textOutput(ns("FOPH_text"), inline = TRUE),
                    style = "color:#fb6a4a"
                  ),
                  "and data individually released by the Swiss cantons 
                                                            processed by",
                  span(textOutput(ns("CORONA_text"), inline = TRUE),
                    style = "color:#42B3D5"
                  ), ".", paste("Last data update:", current_date)
                ),
                p("The figure is interactive and allows seeing the data and model-fitted points.
                    Furthermore, the user can select a rectangular area for obtaining a detailed visualization. 
                    After zooming in, double-click allows to return to the initial figure. 
                    The data beyond the dotted line is incomplete and not used in the analysis."),
                plotlyOutput(outputId = ns("ARIMA_analysis"))
              )
            )
          )
        )
      ),
      tabPanel("Methods",
        # style = "width: 1300px;",
        fluidRow(
          align = "right",
          tags$img(src = "forecastModule-logo_TPH.png", width = 97.8, height = 28.114),
          tags$img(
            src = "forecastModule-logo_UniBas.png", width = 93.8, height = 30.8,
            style = "margin-left: 25px;"
          )
        ),
        titlePanel(h3("COVID-19 Dashboard: Live time-series analysis 
                                          to monitor and forecast the COVID-19 outbreak in Switzerland")),
        p("Description of data sources, 
                              statistical methods and displayed results", style = "color:grey"),
        hr(),
        withMathJax(includeMarkdown("R/forecastModule-Files/Methods.Rmd"))
      ),
      tabPanel("About",
        style = "width: 1300px;",
        fluidRow(
          align = "right",
          tags$img(src = "forecastModule-logo_TPH.png", width = 97.8, height = 28.114),
          tags$img(
            src = "forecastModule-logo_UniBas.png", width = 93.8, height = 30.8,
            style = "margin-left: 25px;"
          )
        ),
        titlePanel(h3("COVID-19 Dashboard: Live time-series analysis 
                                          to monitor and forecast the COVID-19 outbreak in Switzerland")),
        p("Authors", style = "color:grey"),
        hr(),
        includeMarkdown("R/forecastModule-Files/About.Rmd")
      )
    )
  )
}

library(shinycssloaders)

trendsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(10,
          bootstrapPanel(
            heading = HTML(
              "<h1>Analyzing epidemic trends of SARS-CoV-2 in Switzerland</h1><br>",
              "<i>Nanina Anderegg, Julien Riou, Christian L. Althaus (ISPM, Universität Bern)</i>"
            ),
            class = "panel-primary",
            p("National and cantonal trends in incidence data. Lines and ribbons show the fit of a negative
              binomial model and the 95% confidence interval. The model uses case numbers as the response and date
               and weekend (0: work day, 1: weekend) as predictors"),
            uiOutput(ns("lastDataUpdate"))
          )
        ),
        column(2,
          bootstrapPanel(
            heading = "Parameter",
            class = "panel-primary",
            fluidRow(
              column(12,
                dateInput(ns("lastday"), "last day of data to include in analysis", value = today())
              ),
              column(12,
                numericInput(ns("time_window"), "size (days) of time window", value = 14, step = 1)
              ),
              column(12,
                selectizeInput(ns("plot_language"), "language for plots",
                  choices = c("en", "de"), selected = "de")
              )
            )
          )
        )
      ),
      tabsetPanel(
        type = "pills", id = "countryTabs",
        tabPanel(p(class = "tab-title", "Country-wide trends"), value = "country",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("countryPlots"), height = "800px") %>% withSpinner()
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "ageTabs",
        tabPanel(p(class = "tab-title", "Age Groups: Confirmed cases"), value = "cases",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("ageClassPlotsCases"), height = "800px") %>% withSpinner()
          ))
        ),
        tabPanel(p(class = "tab-title", "Age Groups: Hospitalisations"), value = "hospitalizations",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            plotOutput(ns("ageClassPlotsHospitalizations"), height = "800px") %>% withSpinner()
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "cantonTabs",
        tabPanel(p(class = "tab-title", "Cantons: Confirmed cases"), value = "cases",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body",
            column(6,
              bootstrapPanel(
                heading = "Cantonal trends",
                class = "panel-info",
                plotOutput(ns("cantonPlotsCases"), height = "1200px") %>% withSpinner(),
                selectInput(ns("cantonSortCases"),
                  label = "sort order",
                  choices = c(
                    "alphabetical" = "alpha",
                    "weekly growth (ascending)" = "growthAsc", "weekly growth (descending)" = "growthDesc"
                  ),
                  selected = "growthDesc"
                )
              )
            ),
            column(6,
              bootstrapPanel(
                heading = "Ranking",
                class = "panel-info",
                plotOutput(ns("rankingPlotCases"), height = "800px") %>% withSpinner()
              )
            )
          ))
        ),
        tabPanel(p(class = "tab-title", "Cantons: Hospitalizations"), value = "hospitalizations",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body",
            column(6,
              bootstrapPanel(
                heading = "Cantonal trends",
                class = "panel-info",
                plotOutput(ns("cantonPlotsHospitalizations"), height = "1200px") %>% withSpinner(),
                selectInput(ns("cantonSortHospitalizations"),
                  label = "sort order",
                  choices = c(
                    "alphabetical" = "alpha",
                    "weekly growth (ascending)" = "growthAsc", "weekly growth (descending)" = "growthDesc"
                  ),
                  selected = "growthDesc"
                )
              )
            ),
            column(6,
              bootstrapPanel(
                heading = "Ranking",
                class = "panel-info",
                plotOutput(ns("rankingPlotHospitalizations"), height = "800px") %>% withSpinner()
              )
            )
          ))
        )
      ),
      tabsetPanel(
        type = "pills", id = "summaryTabs",
        tabPanel(p(class = "tab-title", "Summary Table"), value = "summaryTable",
          div(class = "panel panel-primary panel-tab", div(class = "panel-body", style = "background:white;",
            fluidRow(
              column(4,
                selectizeInput(ns("filterAgeClass"), label = "show age classes",
                  choices = c("all", "[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)",
                    "[50,60)", "[60,70)", "[70,80)", "[80,121)"),
                  selected = c("all", "[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)",
                    "[50,60)", "[60,70)", "[70,80)", "[80,121)"),
                  multiple = TRUE, options = list(plugins = list("remove_button"),
                    closeAfterSelect = TRUE, hideSelected = TRUE), width = "100%"
                ),
              ),
              column(4,
                selectizeInput(ns("filterEvent"), label = "show event",
                  choices = c("cases", "deaths", "hospitalizations", "icu"),
                  selected = c("cases", "deaths", "hospitalizations", "icu"),
                  multiple = TRUE, options = list(plugins = list("remove_button"),
                    closeAfterSelect = TRUE, hideSelected = TRUE), width = "100%"
                )
              ),
              column(4,
                selectizeInput(ns("filterRegion"), label = "show region",
                  choices = c("AG", "AI", "AR", "BE", "BL", "BS", "CH", "FR", "GE", "GL", "GR", "JU", "LU", "NE", "NW",
                    "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH", "FL"),
                  selected = c("AG", "AI", "AR", "BE", "BL", "BS", "CH", "FR", "GE", "GL", "GR", "JU", "LU", "NE", "NW",
                     "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH", "FL"),
                  multiple = TRUE, options = list(plugins = list("remove_button"),
                    closeAfterSelect = TRUE, hideSelected = TRUE), width = "100%"
                )
              )
            ),
            HTML("<p>Data Sources: Doubling Times (dt) & Weekly changes (wc): see above; ",
              "Re: most recent estimate from",
              "<a href='https://ibz-shiny.ethz.ch/covid-19-re/' target='blank'>",
              "https://ibz-shiny.ethz.ch/covid-19-re/</a></p>"),
            DT::dataTableOutput(ns("comparisonDataTable")),
            downloadButton(ns("downloadData"), "Download .csv")
          ))
        )
      )
    )
  )
}

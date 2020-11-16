library(shiny)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(glue)

ttiqUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Quantifying the impact of test-trace-isolate-quarantine (TTIQ) strategies on COVID-19 transmission</h1><br>",
              "Peter Ashcroft<sup>1</sup>, Sonja Lehtinen<sup>1</sup> and Sebastian Bonhoeffer<sup>1</sup><br>",
              "<i class='small'><sup>1</sup>Institute of Integrative Biology, ETH Zurich, Switzerland</i>"
            ),
            class = "panel-primary",
            includeMarkdown("R/ttiqModuleFiles/abstract.md")
          )
        )
      ),

      # EMPIRICAL DISTRIBUTIONS ----
      fluidRow(
        column(
          width = 12,
          bootstrapPanel(
            heading = "Empirical distributions",
            class = "panel-info",
            id = "parsDist",

            # Generation time distribution
            fluidRow(
              column(
                width = 4,
                plotOutput(ns("genDistPlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(ns("genShape"), "shape", step = 0.001, value = 3.2862)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("genScale"), "scale", step = 0.001, value = 6.1244)
                  )
                ),
                helpText(
                  style = "font-size:15px",
                  HTML("Weibull distribution<br>"),
                  sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                             doi = "10.1101/2020.09.04.20188516")
                )
              ),

              # Infectivity profile
              column(
                width = 4,
                plotOutput(ns("infProfPlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(ns("infShift"), "shift", step = 0.001, value = -0.0747)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("infScale"), "scale", step = 0.001, value = 1.8567)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("infDf"), "df", step = 0.001, value = 3.3454)
                  )
                ),
                helpText(
                  style = "font-size:15px",
                  HTML("Shifted Student's <i>t</i> distribution<br>"),
                  sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                             doi = "10.1101/2020.09.04.20188516")
                )
              ),

              # Incubation period
              column(
                width = 4,
                plotOutput(ns("incDistPlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(ns("incMeanLog"), "log(mean)", step = 0.001, value = 1.42)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("sdLog"), "log(sd)", step = 0.001, value = 0.661)
                  )
                ),
                helpText(
                  style = "font-size:15px",
                  HTML("Lognormal distribution<br>"),
                  sourceLink("Li et al., NEJM 2020 382:1199-1207 ",
                             doi = "10.1056/NEJMoa2001316")
                )
              )
            )
          )
        )
      ),

      tabsetPanel(
        type = "pills", id = "casesTabs",

        # TERTIARY CASES UNDER TTIQ ----
        tabPanel(
          title = p(class = "tab-title", "Tertiary cases under TTIQ"),
          value = "terTTIQ",
          div(
            class = "panel panel-primary panel-tab",
            div(
              class = "panel-body",
              fluidRow(
                column(
                  width = 4,
                  bootstrapPanel(
                    heading = "TTIQ parameters",
                    class = "panel-primary",
                    id = "parsTerTTIQ",

                    img(src = "ttiqModule/tracingAndQuarantine.png", width = "100%"),

                    sliderInput(
                      ns("Re_terTTIQ"),
                      extLabel("R<sub>e</sub>",
                               paste("effective reproductive number without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 2.5, value = 1.2,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("f_terTTIQ"),
                      extLabel("f",
                               "fraction of index cases detected"),
                      min = 0, max = 1, value = 0.5,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("Delta1_terTTIQ"),
                      extLabel("&Delta;<sub>1</sub>",
                               "delay from symptom onset to isolation of index case [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("tau_terTTIQ"),
                      extLabel("&tau;",
                               "duration of lookback [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("g_terTTIQ"),
                      extLabel("g",
                               "fraction of contacts detected and isolated"),
                      min = 0, max = 1, value = 0.5,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("Delta2_terTTIQ"),
                      extLabel("&Delta;<sub>2</sub>",
                               "delay from isolating index case to quarantine of contacts [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1,
                      width = "100%"
                    )
                  )
                ),
                column(
                  width = 8,
                  bootstrapPanel(
                    heading = "Tertiary cases under TTIQ",
                    class = "panel-info",
                    id = "plotsTerTTIQ",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput(ns("prob_terTTIQ"), height = "450px") %>% withSpinner()
                      ),
                      column(
                        width = 6,
                        plotlyOutput(ns("time_terTTIQ"), height = "450px") %>% withSpinner()
                      )
                    ),
                    uiOutput(ns("caption_terTTIQ"))
                  )
                )
              )
            )
          )
        ),

        # TERTIARY CASES UNDER TESTING AND ISOLATION ----
        tabPanel(
          title = p(class = "tab-title", "Tertiary cases under TI"),
          value = "terTI",
          div(
            class = "panel panel-primary panel-tab",
            div(
              class = "panel-body",
              fluidRow(
                column(
                  width = 4,
                  bootstrapPanel(
                    heading = "Testing and isolation parameters",
                    class = "panel-primary",
                    id = "parsTerTI",
                    img(src = "ttiqModule/testAndIsolate.png", width = "100%"),

                    sliderInput(
                      ns("Re_terTI"),
                      extLabel("R<sub>e</sub>",
                               paste("effective reproductive number without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 2.5, value = 1.2,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    helpText(
                      style = "font-size:15px",
                      HTML("We set <i>g</i>=0 to eliminate contact tracing."),
                    )
                    # sliderInput(
                    #   ns("f_terTI"),
                    #   extLabel("f",
                    #            "fraction of index cases detected"),
                    #   min = 0, max = 1, value = c(0, 1),
                    #   step = 0.1, round = -2,
                    #   width = "100%"
                    # ),
                    # sliderInput(
                    #   ns("Delta1_terTI"),
                    #   extLabel("&Delta;<sub>1</sub>",
                    #            "delay from symptom onset to isolation of index case [days]"),
                    #   min = 0, max = 6, value = c(0, 4),
                    #   step = 1,
                    #   width = "100%"
                    # )
                  )
                ),
                column(
                  width = 8,
                  bootstrapPanel(
                    heading = "Tertiary cases under testing and isolation",
                    class = "panel-info",
                    id = "plotsTerTI",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput(ns("cases_terTI"), height = "450px") %>% withSpinner()
                      ),
                      column(
                        width = 6,
                        plotlyOutput(ns("region_terTI"), height = "450px") %>% withSpinner()
                      )
                    ),
                    #plotOutput(ns("casesSEC_legend"), height = "75px"),
                    uiOutput(ns("caption_terTI"))
                  )
                )
              )
            )
          )
        ),

        # SECONDARY CASES UNDER TESTING AND ISOLATION ----
        tabPanel(
          title = p(class = "tab-title", "Secondary cases under TI"),
          value = "secTI",
          div(
            class = "panel panel-primary panel-tab",
            div(
              class = "panel-body",
              fluidRow(
                column(
                  width = 4,
                  bootstrapPanel(
                    heading = "Testing and isolation parameters",
                    class = "panel-primary",
                    id = "parsSecTI",
                    img(src = "ttiqModule/testAndIsolate.png", width = "100%"),

                    sliderInput(
                      ns("Re_secTI"),
                      extLabel("R<sub>e</sub>",
                               paste("effective reproductive number without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 2.5, value = 1.2,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    helpText(
                      style = "font-size:15px",
                      HTML("We set <i>g</i>=0 to eliminate contact tracing."),
                    )
                    # sliderInput(
                    #   ns("f_secTI"),
                    #   extLabel("f",
                    #            "fraction of index cases detected"),
                    #   min = 0, max = 1, value = c(0, 1),
                    #   step = 0.1, round = -2,
                    #   width = "100%"
                    # ),
                    # sliderInput(
                    #   ns("Delta1_secTI"),
                    #   extLabel("&Delta;<sub>1</sub>",
                    #            "delay from symptom onset to isolation of index case [days]"),
                    #   min = 0, max = 6, value = c(0, 4),
                    #   step = 1,
                    #   width = "100%"
                    # )
                  )
                ),
                column(
                  width = 8,
                  bootstrapPanel(
                    heading = "Secondary cases under testing and isolation",
                    class = "panel-info",
                    id = "plotsSecTI",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput(ns("cases_secTI"), height = "450px") %>% withSpinner()
                      ),
                      column(
                        width = 6,
                        plotlyOutput(ns("region_secTI"), height = "450px") %>% withSpinner()
                      )
                    ),
                    uiOutput(ns("caption_secTI"))
                  )
                )
              )
            )
          )
        )
      )
    ),
    tags$script(src = "jquery.connections.js"),
    tags$script(src = "ttiqModule/ttiqModule.js")
  )
}

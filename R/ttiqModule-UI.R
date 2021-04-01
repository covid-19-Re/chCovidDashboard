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
                HTML("<br><span class='help-block' style='font-size:15px;'>",
                     "Weibull distribution.",
                     "</span>"),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(ns("genShape"), "shape", step = 0.001, value = 3.277)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("genScale"), "scale", step = 0.001, value = 6.127)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                           doi = "10.1101/2020.09.04.20188516")
              ),

              # Infectivity profile
              column(
                width = 4,
                plotOutput(ns("infProfPlot"), height = "300px") %>% withSpinner(),
                HTML("<br><span class='help-block' style='font-size:15px;'>",
                     "Shifted Student's t-distribution.",
                     "</span>"),
                fluidRow(
                  column(
                    width = 4,
                    numericInput(ns("infShift"), "shift", step = 0.001, value = -0.0776)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("infScale"), "scale", step = 0.001, value = 1.857)
                  ),
                  column(
                    width = 4,
                    numericInput(ns("infDf"), "df", step = 0.001, value = 3.345)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                           doi = "10.1101/2020.09.04.20188516")
              ),

              # Incubation period
              column(
                width = 4,
                plotOutput(ns("incDistPlot"), height = "300px") %>% withSpinner(),
                HTML("<br><span class='help-block' style='font-size:15px;'>",
                     "The distribution of incubation times follows a meta-distribution constructed from the average ",
                     "of seven reported log-normal distributions.",
                     "</span>"),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                           doi = "10.1101/2020.09.04.20188516")
              )
            )
          )
        )
      ),

      tabsetPanel(
        type = "pills", id = "casesTabs",

        # REPRODUCTIVE NUMBER UNDER TTIQ ----
        tabPanel(
          title = p(class = "tab-title", "TTIQ"),
          value = "RTTIQ",
          div(class = "panel panel-primary panel-tab",
            div(class = "panel-body",
              fluidRow(
                column(
                  width = 4,
                  bootstrapPanel(
                    heading = "TTIQ parameters",
                    class = "panel-primary",
                    id = "parsTTIQ",

                    img(src = "ttiqModule/tracingAndQuarantine.png", width = "100%"),

                    h4("Epidemic parameters"),
                    sliderInput(
                      ns("R_TTIQ"),
                      extLabel("R",
                               paste("effective reproductive number without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 2.5, value = 1.1,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("alpha_TTIQ"),
                      extLabel("&alpha;",
                               paste("fraction of transmission from asymptomatics without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 1, value = 0.2,
                      step = 0.05, round = -2,
                      width = "100%"
                    ),
                    h4("Focal TTIQ parameter set"),
                    sliderInput(
                      ns("f_TTIQ"),
                      extLabel("f",
                               "fraction of index cases detected"),
                      min = 0, max = 1, value = 0.5,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("g_TTIQ"),
                      extLabel("g",
                               "fraction of contacts detected and isolated"),
                      min = 0, max = 1, value = 0.5,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("Delta1_TTIQ"),
                      extLabel("&Delta;<sub>1</sub>",
                               "delay from symptom onset to isolation of index case [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("Delta2_TTIQ"),
                      extLabel("&Delta;<sub>2</sub>",
                               "delay from isolating index case to quarantine of contacts [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("tau_TTIQ"),
                      extLabel("&tau;",
                               "duration of lookback [days]"),
                      min = 0, max = 6, value = 2,
                      step = 1, round = -2,
                      width = "100%"
                    )
                  )
                ),
                column(
                  width = 8,
                  bootstrapPanel(
                    heading = "Reproductive number under TTIQ",
                    class = "panel-info",
                    id = "plotsTTIQ",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput(ns("prob_TTIQ"), height = "450px") %>% withSpinner()
                      ),
                      column(
                        width = 6,
                        plotlyOutput(ns("time_TTIQ"), height = "450px") %>% withSpinner()
                      )
                    ),
                    uiOutput(ns("caption_TTIQ"))
                  )
                )
              )
            )
          )
        ),

        # REPRODUCTIVE NUMBER UNDER TESTING AND ISOLATION ----
        tabPanel(
          title = p(class = "tab-title", "Testing & isolation only"),
          value = "RTI",
          div(
            class = "panel panel-primary panel-tab",
            div(
              class = "panel-body",
              fluidRow(
                column(
                  width = 4,
                  bootstrapPanel(
                    heading = "Testing & isolation parameters",
                    class = "panel-primary",
                    id = "parsTI",
                    img(src = "ttiqModule/testAndIsolate.png", width = "100%"),

                    sliderInput(
                      ns("R_TI"),
                      extLabel("R",
                               paste("effective reproductive number without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 2.5, value = 1.1,
                      step = 0.1, round = -2,
                      width = "100%"
                    ),
                    sliderInput(
                      ns("alpha_TI"),
                      extLabel("&alpha;",
                               paste("fraction of transmission from asymptomatics without TTIQ",
                                     "(i.e. <i>f</i>=<i>g</i>=0)")),
                      min = 0, max = 1, value = 0.2,
                      step = 0.05, round = -2,
                      width = "100%"
                    ),
                    helpText(
                      style = "font-size:15px",
                      HTML("We set <i>g</i>=0 to eliminate contact tracing."),
                    )
                  )
                ),
                column(
                  width = 8,
                  bootstrapPanel(
                    heading = "Reproductive number under testing & isolation only",
                    class = "panel-info",
                    id = "plotsTI",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput(ns("lines_TI"), height = "450px") %>% withSpinner()
                      ),
                      column(
                        width = 6,
                        plotlyOutput(ns("density_TI"), height = "450px") %>% withSpinner()
                      )
                    ),
                    uiOutput(ns("caption_TI"))
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

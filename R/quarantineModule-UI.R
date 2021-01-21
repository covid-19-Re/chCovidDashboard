library(shiny)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(glue)
library(plotly)

quarantineDurationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Quantifying the impact of quarantine duration on COVID-19 transmission</h1><br>",
              "Peter Ashcroft<sup>1</sup>, Sonja Lehtinen<sup>1</sup>, Daniel Angst<sup>1</sup>, Nicola Low<sup>2</sup> and Sebastian Bonhoeffer<sup>1</sup><br>",
              "<i class='small'><sup>1</sup>Institute of Integrative Biology, ETH Zurich, Switzerland, <sup>2</sup>Institute of Social and Preventive Medicine, University of Bern, Bern, Switzerland</i>"
            ),
            class = "panel-primary",
            # Include abstract
            includeMarkdown("R/quarantineModuleFiles/abstract.md")
          )
        )
      ),

      # EMPIRICAL DISTRIBUTIONS ----
      fluidRow(
        column(
          12,
          bootstrapPanel(
            heading = "Empirical distributions", class = "panel-info", id = "parsDistr",
            fluidRow(
              column(
                4,
                plotOutput(ns("genTimePlot"), height = "300px") %>% withSpinner(),
                HTML("<br><span class='help-block' style='font-size:15px;'>",
                     "Weibull distribution.",
                     "</span>"),
                fluidRow(
                  column(4,
                         numericInput(ns("genShape"), "shape", step = 0.001, value = 3.277)
                  ),
                  column(4,
                         numericInput(ns("genScale"), "scale", step = 0.001, value = 6.127)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                           doi = "10.1101/2020.09.04.20188516")
              ),
              column(
                4,
                plotOutput(ns("infProfPlot"), height = "300px") %>% withSpinner(),
                HTML("<br><span class='help-block' style='font-size:15px;'>",
                     "Shifted Student's t-distribution.",
                     "</span>"),
                fluidRow(
                  column(4,
                         numericInput(ns("infShift"), "shift", step = 0.001, value = -0.0776)
                  ),
                  column(4,
                         numericInput(ns("infScale"), "scale", step = 0.001, value = 1.857)
                  ),
                  column(4,
                         numericInput(ns("infDf"), "df", step = 0.001, value = 3.345)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516",
                           doi = "10.1101/2020.09.04.20188516")
              ),
              column(
                4,
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

      # SC1: TRACED CONTACTS ----
      tabsetPanel(
        type = "pills", id = "scenarioTabs",
        tabPanel(
          p(class = "tab-title", "Scenario 1: Quarantining traced contacts"),
          value = "sc1",
          div(class = "panel panel-primary panel-tab", div(
            class = "panel-body",

            # SC1: TEST-AND-RELEASE
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Test-and-release parameters", class = "panel-primary", id = "parsSC1-2",
                  img(src = "quarantineModule/timelineTraced.svg", width = "100%"),
                  fluidRow(
                    column(
                      12,
                      helpText(
                        style = "font-size:15px",
                        HTML("<strong>t<sub>E</sub>=0</strong> <i>time of exposure is fixed to zero</i>")
                      ),
                      helpText(
                        style = "font-size:15px",
                        HTML("<sup>*</sup>tests are subject to time-dependent false-negative results:"),
                        sourceLink("Kucirka et al., Ann. Intern. Med. 2020 173:262-267 ",
                                   doi = "10.7326/M20-1495")
                      )
                    )
                  ),

                  sliderInput(
                    ns("sc1_test_DeltaQ"),
                    extLabel("t<sub>Q</sub>&minus;t<sub>E</sub>", "start of quarantine after exposure"),
                    min = 0, max = 10, value = 3, step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc1_test_DeltaT"),
                    extLabel("t<sub>R</sub>&minus;t<sub>T</sub>", "delay between test and release"),
                    min = 0, max = 5, value = c(0, 3), step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc1_test_sPercent"),
                    extLabel("s", "fraction infected",
                             tooltip = "fraction of individuals in quarantine that are infected"
                    ),
                    min = 0, max = 100, value = 10, step = 5, post = "%",
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc1_test_tRCompare"),
                    extLabel("t<sub>R<sup>*</sup></sub>","baseline quarantine duration"),
                    min = 5, max = 15, value = 10, step = 1,
                    width = "100%"
                  ),
                  hr(),
                  fluidRow(
                    column(
                      12,
                      strong("Additional Hygiene & Social Distancing Measures"),
                      checkboxInput(
                        ns("sc1_test_addMeasures"),
                        "Show effect of additional Hygiene & Social Distancing Measures (dashed lines)"),
                      sliderInput(
                        ns("sc1_test_rPercent"),
                        extLabel("r", "transmission reduction",
                                 tooltip = str_c(
                                   "reduced transmission due to extra hygiene and",
                                   "social distancing measures imposed by reduced quarantine",
                                   "after release",
                                   sep = " "
                                 )
                        ),
                        min = 0, max = 100, value = 50, step = 5, post = "%",
                        width = "100%"
                      )
                    )
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Quantifying the impact of quarantine for traced contacts",
                  id = "plotsSC1-2",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc1_test_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc1_test_utility_plot"), height = "450px") %>% withSpinner())
                  ),
                  plotOutput(ns("sc1_test_legend"), height = "75px"),
                  uiOutput(ns("sc1_test_caption"))
                )
              )
            ),

            # SC1: QUARANTINE DURATION ----
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Quarantine duration parameters", class = "panel-primary", id = "parsSC1-1",
                  sliderInput(
                    ns("sc1_noTest_tQ"),
                    extLabel("t<sub>Q</sub>&minus;t<sub>E</sub>", "delay between exposure and the start of quarantine"),
                    min = 0, max = 10, value = c(0, 4), step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc1_noTest_tR.vals"),
                    extLabel("t<sub>R</sub>&minus;t<sub>E</sub>", "days until release after exposure"),
                    min = 0, max = 15, value = c(0, 15), step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc1_noTest_tR.compare"),
                    extLabel("t<sub>R<sup>*</sup></sub>", "baseline quarantine duration"),
                    min = 0, max = 15, value = 10, step = 1,
                    width = "100%"
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Quantifying the effect of duration and delay for the standard quarantine protocol (no test) for traced contacts.",
                  id = "plotsSC1-1",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc1_noTest_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc1_noTest_utility_plot"), height = "450px") %>% withSpinner())
                  ),
                  plotOutput(ns("sc1_noTest_legend"), height = "75px"),
                  uiOutput(ns("sc1_noTest_caption"))
                )
              )
            ),

            # SC1: FURTHER CONSIDERATIONS ----
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Further considerations", class = "panel-primary", id = "parsSC1-3",
                  helpText(
                    style = "font-size:15px",
                    HTML(glue(
                      "<strong>Adherence:</strong>",
                      "Changing the duration of quarantine could affect how many people adhere to the guidelines.",
                      "Here we show the fold-change in adherence required to offset the change in quarantine efficacy if the duration is changed.",
                      "<br>",
                      "<strong>Symptoms:</strong>",
                      "Individuals who develop symptoms should isolate independent of quarantine.",
                      "Here we deduct these cases once they develop symptoms from the transmission prevented by quarantine.",
                      .sep = " "
                    ))
                  ),
                  sliderInput(
                    ns("sc1_isolation_Delta"),
                    extLabel("&Delta;", "delay between symptom onset and isolation"),
                    min = 0, max = 5, value = 0, step = 1,
                    width = "100%"
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Adherence and symptoms", id = "plotsSC1-3",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc1_adherence_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc1_asymptomatic_plot"), height = "450px") %>% withSpinner())
                  ),
                  uiOutput(ns("sc1_further_caption"))
                )
              )
            )
          ))
        ),

        # SC2: RETURNING TRAVELLERS
        tabPanel(
          p(class = "tab-title", "Scenario 2: Quarantining returning travellers"),
          value = "sc2",
          div(class = "panel panel-primary panel-tab", div(
            class = "panel-body",

            # SC2: TEST-AND-RELEASE ----
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Test-and-release parameters", class = "panel-primary", id = "parsSC2-5",
                  img(src = "quarantineModule/timelineTravellers.svg", width = "100%"),
                  fluidRow(
                    column(
                      12,
                      helpText(
                        style = "font-size:15px",
                        HTML("<strong>t<sub>Q</sub>=0</strong> <i>travellers enter quarantine immediately upon arrival</i>")
                      ),
                      helpText(
                        style = "font-size:15px",
                        HTML("<sup>*</sup>tests are subject to time-dependent false-negative results:"),
                        sourceLink("Kucirka et al., Ann. Intern. Med. 2020 173:262-267 ",
                                   doi = "10.7326/M20-1495")
                      )
                    )
                  ),
                  sliderInput(
                    ns("sc2_test_y"),
                    extLabel("y", "travel duration (days)"),
                    min = 0, max = 15, value = 7, step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc2_test_DeltaT"),
                    extLabel("t<sub>R</sub>&minus;t<sub>T</sub>", "delay between test and release"),
                    min = 0, max = 5, value = c(0, 3), step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc2_test_sPercent"),
                    extLabel("s", "fraction infected",
                             tooltip = "fraction of individuals in quarantine that are infected"
                    ),
                    min = 0, max = 100, value = 10, step = 5, post = "%",
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc2_test_tRCompare"),
                    extLabel("t<sub>R<sup>*</sup></sub>","baseline quarantine duration"),
                    min = 5, max = 15, value = 10, step = 1,
                    width = "100%"
                  ),
                  extLabel("Normalisation", "normalise to local transmission or total transmission"),
                  switchInput(
                    inputId = ns("sc2_test_normalisation"),
                    onLabel = "local",
                    offLabel = "total",
                    value = TRUE
                  ),
                  hr(),
                  fluidRow(
                    column(
                      12,
                      strong("Additional Hygiene & Social Distancing Measures"),
                      checkboxInput(
                        ns("sc2_test_addMeasures"),
                        "Show effect of additional Hygiene & Social Distancing Measures (dashed lines)"),
                      sliderInput(
                        ns("sc2_test_rPercent"),
                        extLabel("r", "transmission reduction",
                                 tooltip = str_c(
                                   "reduced transmission due to extra hygiene and ",
                                   "social distancing measures imposed by reduced quarantine ",
                                   "after release"
                                 )
                        ),
                        min = 0, max = 100, value = 50, step = 5, post = "%",
                        width = "100%"
                      )
                    )
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Quantifying the impact of quarantine for returning travellers",
                  id = "plotsSC2-5",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc2_test_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc2_test_utility_plot"), height = "450px") %>% withSpinner()),
                  ),
                  plotOutput(ns("sc2_test_legend"), height = "75px"),
                  uiOutput(ns("sc2_test_caption"))
                )
              )
            ),

            # SC2: QUARANTINE DURATION ----
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Quarantine duration parameters", class = "panel-primary", id = "parsSC2-4",
                  selectizeInput(
                    ns("sc2_noTest_y.vals"),
                    extLabel("y", "duration of travel"),
                    choices = as.character(1:21),
                    selected = as.character(c(1, 2, 3, 5, 7, 10, 14)),
                    multiple = TRUE,
                    options = list(delimiter = ", ", create = TRUE)
                  ),
                  sliderInput(
                    ns("sc2_noTest_tR.vals"),
                    extLabel("t<sub>R</sub>&minus;t<sub>Q</sub>", "day of release after arrival"),
                    min = 0, max = 15, value = c(0, 15), step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("sc2_noTest_tR.compare"),
                    extLabel("t<sub>R<sup>*</sup></sub>", "baseline quarantine duration"),
                    min = 0, max = 15, value = 10, step = 1,
                    width = "100%"
                  ),
                  extLabel("Normalisation", "normalise to local transmission or total transmission"),
                  switchInput(
                    inputId = ns("sc2_noTest_normalisation"),
                    onLabel = "local",
                    offLabel = "total",
                    value = TRUE
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Quantifying the effect of travel duration and quarantine duration for the standard quarantine protocol (no test) for returning travellers",
                  id = "plotsSC2-4",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc2_noTest_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc2_noTest_utility_plot"), height = "450px") %>% withSpinner()),
                  ),
                  plotOutput(ns("sc2_noTest_legend"), height = "75px"),
                  uiOutput(ns("sc2_noTest_caption"))
                )
              )
            ),

            # SC2: FURTHER CONSIDERATIONS ----
            fluidRow(
              column(
                4,
                bootstrapPanel(
                  heading = "Further considerations", class = "panel-primary", id = "parsSC2-6",
                  helpText(
                    style = "font-size:15px",
                    HTML(glue(
                      "<strong>Adherence:</strong>",
                      "Changing the duration of quarantine could affect how many people adhere to the guidelines.",
                      "Here we show the fold-change in adherence required to offset the change in quarantine efficacy if the duration is changed.",
                      "<br>",
                      "<strong>Symptoms:</strong>",
                      "Individuals who develop symptoms should isolate independent of quarantine.",
                      "Here we deduct these cases once they develop symptoms from the transmission prevented by quarantine.",
                      .sep = " "
                    ))
                  ),
                  sliderInput(
                    ns("sc2_isolation_Delta"),
                    extLabel("&Delta;", "delay between symptom onset and isolation"),
                    min = 0, max = 5, value = 0, step = 1,
                    width = "100%"
                  )
                )
              ),
              column(
                8,
                bootstrapPanel(
                  heading = "Adherence and symptoms", id = "plotsSC2-6",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotlyOutput(ns("sc2_adherence_plot"), height = "450px") %>% withSpinner()),
                    column(6, plotlyOutput(ns("sc2_asymptomatic_plot"), height = "450px") %>% withSpinner()),
                  ),
                  uiOutput(ns("sc2_further_caption"))
                )
              )
            )

          ))
        )
      )
    ),
    tags$script(src = "jquery.connections.js"),
    tags$script(src = "quarantineModule/quarantineModule.js")
  )
}

library(shiny)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(glue)

quarantineDurationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = HTML(
              "<h1>Quantifying the impact of quarantine duration on COVID-19 transmission</h1><br>",
              "<i>Peter Ashcroft, Sonja Lehtinen, Daniel Angst and Sebastian Bonhoeffer (D-USYS, ETH Zürich)</i>"
            ),
            class = "panel-primary",
            includeMarkdown("R/quarantineModuleFiles/abstract.md")
          )
        )
      ),
      # empirical distributions
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = "Empirical distributions", class = "panel-info", id = "parsDistr",
            fluidRow(
              column(4,
                plotOutput(ns("genTimePlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(4,
                    numericInput(ns("genShape"), "shape", step = 0.001, value = 3.2862)
                  ),
                  column(4,
                    numericInput(ns("genScale"), "scale", step = 0.001, value = 6.1244)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516", doi = "10.1101/2020.09.04.20188516")
              ),
              column(4,
                plotOutput(ns("infProfPlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(4,
                    numericInput(ns("infShift"), "shift", step = 0.001, value = -0.0747)
                  ),
                  column(4,
                    numericInput(ns("infScale"), "scale", step = 0.001, value = 1.8567)
                  ),
                  column(4,
                    numericInput(ns("infDf"), "df", step = 0.001, value = 3.3454)
                  )
                ),
                sourceLink("Ferretti et al., medRxiv 2020.09.04.20188516", doi = "10.1101/2020.09.04.20188516")
              ),
              column(4,
                plotOutput(ns("incDistPlot"), height = "300px") %>% withSpinner(),
                fluidRow(
                  column(4,
                    numericInput(ns("incMeanLog"), "log(mean)", step = 0.001, value = 1.42)
                  ),
                  column(4,
                    numericInput(ns("sdLog"), "log(sd)", step = 0.001, value = 0.661)
                  )
                ),
                sourceLink("Li et al., NEJM 2020 382:1199-1207 ", doi = "10.1056/NEJMoa2001316")
              )
            )
          )
        )
      ),
      tabsetPanel(
        type = "pills", id = "scenarioTabs",
        tabPanel(p(class = "tab-title", "Scenario 1: Quarantining traced contacts"),
          value = "sc1",
          div(class = "panel panel-primary panel-tab", div(
            class = "panel-body",
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Quarantine duration parameters", class = "panel-primary", id = "parsSC1-1",
                  img(src = "quarantineModule/timeline.png", width = "100%"),
                  sliderInput(
                    ns("quarantineDelay"),
                    extLabel("&Delta;<sub>Q</sub> (= t<sub>Q</sub> - t<sub>E</sub>)", "delay to quarantine"),
                    min = 0, max = 10, value = c(0, 4),
                    step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("quarantineDuration"),
                    extLabel("n (= t<sub>R</sub> - t<sub>E</sub>)", "duration of quarantine from last exposure"),
                    min = 0, max = 30, value = c(0, 15),
                    step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("nCompare"),
                    extLabel("n<sub>compare</sub>", "compare to quarantine duration"),
                    min = 0, max = 30, value = 10,
                    step = 1,
                    width = "100%"
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Standard n-day quarantine", id = "plotsSC1-1",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc1"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc1_utility"), height = "450px") %>% withSpinner())
                  ),
                  plotOutput(ns("sc1_legend"), height = "75px"),
                  uiOutput(ns("sc1_caption"))
                )
              )
            ),
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Test-and-release parameters", class = "panel-primary", id = "parsSC1-2",
                  selectizeInput(
                    ns("deltaQfocal"),
                    extLabel("&Delta;<sub>Q</sub>", "focal delay to quarantine"),
                    choices = as.character(0:4),
                    selected = as.character(3),
                    multiple = FALSE,
                    options = list(delimiter = ", ")
                  ),
                  sliderInput(
                    ns("testResultDelay"),
                    extLabel("&Delta;<sub>T</sub>", "test result delay"),
                    min = 0, max = 5, value = c(0, 3),
                    width = "100%"
                  ),
                  fluidRow(
                    column(6,
                      numericInput(
                        ns("testSpecificity"),
                        extLabel("s", "fraction infected",
                          tooltip = "fraction of individuals in quarantine that are infected"
                        ),
                        value = 0.1,
                        step = 0.05
                      )
                    ),
                    column(6,
                      numericInput(
                        ns("transmissionReduction"),
                        extLabel("r", "transmission reduction",
                          tooltip = str_c(
                            "reduced transmission due to extra hygiene and ",
                            "social distancing measures imposed by reduced quarantine ",
                            "after release"
                          )
                        ),
                        value = 0.5,
                        step = 0.05
                      )
                    )
                  ),
                  fluidRow(
                    column(12,
                      helpText(
                        style = "font-size:15px",
                        HTML("<sup>*</sup>tests are subject to time-dependent false-negative results:"),
                        sourceLink("Kucirka et al., Ann. Intern. Med. 2020 173:262-267 ", doi = "10.7326/M20-1495")
                      )
                    )
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Test-and-release", id = "plotsSC1-2",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc1_test"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc1_test_utility"), height = "450px") %>% withSpinner())
                  ),
                  plotOutput(ns("sc1_test_legend"), height = "75px"),
                  uiOutput(ns("sc1_test_caption"))
                )
              )
            ),
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Further considerations", class = "panel-primary", id = "parsSC1-3",
                  helpText(
                    style = "font-size:15px",
                    HTML(glue(
                      "<strong>Adherence:</strong> Changing the duration of quarantine could affect how many people",
                      "adhere to the guidelines. Here we show the change in adherence required to offset the change in",
                      "quarantine efficacy if the duration is changed.<br>",
                      "<strong>Symptoms:</strong> Individuals who develop symptoms should isolate independent of",
                      "quarantine. Here we deduct these cases once they develop symptoms from the transmission",
                      "prevented by quarantine."
                    ))
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Adherence and symptoms", id = "plotsSC1-3",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc1_adherence"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc1_asymptomatic"), height = "450px") %>% withSpinner())
                  ),
                  uiOutput(ns("sc1_adherence_caption"))
                )
              )
            )
          ))
        ),
        tabPanel(p(class = "tab-title", "Scenario 2: Quarantining returning travellers"),
          value = "sc2",
          div(class = "panel panel-primary panel-tab", div(
            class = "panel-body",
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Quarantine duration parameters", class = "panel-primary", id = "parsSC2-4",
                  img(src = "quarantineModule/timeline.png", width = "100%"),
                  selectizeInput(
                    ns("travelDuration"),
                    extLabel("y", "duration of travel"),
                    choices = as.character(1:21),
                    selected = as.character(c(1, 2, 3, 5, 7, 10, 14)),
                    multiple = TRUE,
                    options = list(delimiter = ", ", create = TRUE)
                  ),
                  sliderInput(
                    ns("quarantineDurationSC2"),
                    extLabel("n (= t<sub>R</sub> - t<sub>E</sub>)", "duration of quarantine from last exposure"),
                    min = 0, max = 30, value = c(0, 15),
                    step = 1,
                    width = "100%"
                  ),
                  sliderInput(
                    ns("nCompareSC2"),
                    extLabel("n<sub>compare</sub>", "compare to quarantine duration"),
                    min = 0, max = 30, value = 10,
                    step = 1,
                    width = "100%"
                  ),
                  extLabel("Normalisation", "Normalise to local transmission or total transmission"),
                  switchInput(
                    inputId = ns("normalisation"),
                    onLabel = "local",
                    offLabel = "total",
                    value = TRUE
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Standard n-day quarantine", id = "plotsSC2-4",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc2"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc2_utility"), height = "450px") %>% withSpinner()),
                  ),
                  plotOutput(ns("sc2_legend"), height = "75px"),
                  uiOutput(ns("sc2_caption"))
                )
              )
            ),
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Test-and-release parameters", class = "panel-primary", id = "parsSC2-5",
                  selectizeInput(
                    ns("yFocus"),
                    extLabel("y", "focal travel duration"),
                    choices = as.character(1:21),
                    selected = as.character(7),
                    multiple = FALSE,
                    options = list(delimiter = ", ")
                  ),
                  sliderInput(
                    ns("testResultDelaySC2"),
                    extLabel("&Delta;<sub>T</sub>", "test result delay"),
                    min = 0, max = 5, value = c(0, 3),
                    width = "100%"
                  ),
                  fluidRow(
                    column(6,
                      numericInput(
                        ns("testSpecificitySC2"),
                        extLabel("s", "fraction infected",
                          tooltip = "fraction of individuals in quarantine that are infected"
                        ),
                        value = 0.1,
                        step = 0.05
                      )
                    ),
                    column(6,
                      numericInput(
                        ns("transmissionReductionSC2"),
                        extLabel("r", "transmission reduction",
                          tooltip = str_c(
                            "reduced transmission due to extra hygiene and ",
                            "social distancing measures imposed by reduced quarantine ",
                            "after release"
                          )
                        ),
                        value = 0.5,
                        step = 0.05
                      )
                    )
                  ),
                  fluidRow(
                    column(12,
                      helpText(
                        style = "font-size:15px",
                        HTML("<sup>*</sup>tests are subject to time-dependent false-negative results:"),
                        sourceLink("Kucirka et al., Ann. Intern. Med. 2020 173:262-267 ", doi = "10.7326/M20-1495")
                      )
                    )
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Test-and-release", id = "plotsSC2-5",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc2_test"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc2_test_utility"), height = "450px") %>% withSpinner()),
                  ),
                  plotOutput(ns("sc2_test_legend"), height = "75px"),
                  uiOutput(ns("sc2_test_caption"))
                )
              )
            ),
            fluidRow(
              column(4,
                bootstrapPanel(
                  heading = "Further considerations", class = "panel-primary", id = "parsSC2-6",
                  helpText(
                    style = "font-size:15px",
                    HTML(glue(
                      "<strong>Adherence:</strong> Changing the duration of quarantine could affect how many people",
                      "adhere to the guidelines. Here we show the change in adherence required to offset the change in",
                      "quarantine efficacy if the duration is changed.<br>",
                      "<strong>Symptoms:</strong> Individuals who develop symptoms should isolate independent of",
                      "quarantine. Here we deduct these cases once they develop symptoms from the transmission",
                      "prevented by quarantine."
                    ))
                  )
                )
              ),
              column(8,
                bootstrapPanel(
                  heading = "Adherence and symptoms", id = "plotsSC2-6",
                  class = "panel-info",
                  fluidRow(
                    column(6, plotOutput(ns("sc2_adherence"), height = "450px") %>% withSpinner()),
                    column(6, plotOutput(ns("sc2_asymptomatic"), height = "450px") %>% withSpinner()),
                  ),
                  uiOutput(ns("sc2_adherence_caption"))
                )
              )
            )
          ))
        )
      )
    ),
    tags$script(src = "quarantineModule/jquery.connections.js"),
    tags$script(src = "quarantineModule/quarantineModule.js")
  )
}

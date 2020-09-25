library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)

quarantineDurationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          bootstrapPanel(
            heading = "Quantifying the impact of quarantine duration on COVID-19 transmission",
            class = "panel-primary",
            includeMarkdown("R/quarantineModuleFiles/abstract.md")
          )
        )
      ),
      # empirical distributions
      fluidRow(
        column(8,
          bootstrapPanel(heading = "Empirical distributions", class = "panel-info", id = "parsDistr",
            fluidRow(
              column(4,
                plotOutput(ns("genTimePlot"), height = "200px") %>% withSpinner(),
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
                plotOutput(ns("infProfPlot"), height = "200px") %>% withSpinner(),
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
                plotOutput(ns("incDistPlot"), height = "200px") %>% withSpinner(),
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
      # Scenario 1: Quarantining secondary cases after contact tracing
      h1("Scenario 1: Quarantining secondary cases after contact tracing", style = "background-color:rgb(238, 238, 238);"),
      br(),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 1", class = "panel-primary", id = "pars1",
            img(src = "quarantineModule/timeline.png", width = "100%"),
            sliderInput(
              ns("quarantineDelay"),
              extLabel("&Delta;<sub>Q</sub> (= t<sub>Q</sub> - t<sub>E</sub>)", "delay to quarantine"),
              min = 0, max = 10, value = 3,
              step = 1,
              width = "100%"),
            sliderInput(
              ns("quarantineDuration"),
              extLabel("n (= t<sub>R</sub> - t<sub>E</sub>)", "duration of quarantine from last exposure"),
              min = 0, max = 30, value = c(0, 15),
              step = 1,
              width = "100%"),
            sliderInput(
              ns("nCompare"),
              extLabel("n<sub>compare</sub>", "compare to quarantine duration"),
              min = 0, max = 30, value = 10,
              step = 1,
              width = "100%")
          )
        ),
        column(8,
          bootstrapPanel(heading = "Quarantine utility", id = "plots1",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("fracNoTestPlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("fracNoTestRelUtilityPlot"), height = "450px") %>% withSpinner())
            ),
            uiOutput(ns("noTestCaption"))
          )
        )
      ),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 2", class = "panel-primary", id = "pars2",
            sliderInput(
              ns("testDay"),
              extLabel("t<sub>T</sub>", "day on which test is conducted"),
              min = 0, max = 10, value = c(3, 8),
              step = 1,
              width = "100%"),
            fluidRow(
              column(5,
                numericInput(
                  ns("testDuration"),
                  extLabel("&Delta;<sub>T</sub>", "days until test result"),
                  value = 2,
                  step = 0.5)
              ),
              column(5,
                numericInput(
                  ns("testSpecificity"),
                  extLabel("s", "quarantine specificity", tooltip = "fraction of quarantined persons who are infected"),
                  value = 0.1,
                  step = 0.05)
              )
            ),
            fluidRow(
              column(5,
                numericInput(
                  ns("transmissionReduction"),
                  extLabel("r", "transmission reduction",
                    tooltip = str_c("reduced transmission due to extra hygiene and",
                      "social distancing measures imposed by reduced quarantine")),
                  value = 0.5,
                  step = 0.05)
              )
            )
          )
        ),
        column(8,
          bootstrapPanel(heading = "Testing and Releasing", id = "plots2",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("fracTestPlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("fracTestRelUtilityPlot"), height = "450px") %>% withSpinner())
            ),
            uiOutput(ns("testCaption"))
          )
        )
      ),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 3", class = "panel-primary", id = "pars3",
            fluidRow(
              column(8,
                sliderInput(
                  ns("adherence"),
                  extLabel("a", "adherence to quarantine"),
                  min = 0, max = 1, value = c(0, 1),
                  step = 0.05,
                  width = "100%")
              ),
              column(4,
                numericInput(
                  ns("adherenceBy"),
                  extLabel("by", ""),
                  value = 0.25,
                  step = 0.01)
              )
            ),
            fluidRow(
              column(5,
                disabled(numericInput(
                  ns("tSymptoms"),
                  extLabel("t<sub>S</sub>", "time to symptoms", tooltip = "equal to t<sub>E</sub> + mean of incubation period distribution t<sub>S</sub>"),
                  value = 5,
                  step = 0.5))
              )
            )
          )
        ),
        column(8,
          bootstrapPanel(heading = "Adherence and symptoms", id = "plots3",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("relAdherencePlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("fracAdherencePlot"), height = "450px") %>% withSpinner())
            ),
            uiOutput(ns("adherenceCaption"))
          )
        )
      ),
      h1("Scenario 2: Quarantining returning travellers", style = "background-color:rgb(238, 238, 238);"),
      br(),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 4", class = "panel-primary", id = "pars4",
            selectizeInput(
              ns("travelDuration"),
              extLabel("y", "duration of travel"),
              choices = as.character(1:21),
              selected = as.character(c(1, 2, 3, 5, 7, 10, 14)),
              multiple = TRUE,
              options = list(delimiter = ", ", create = TRUE))
          )
        ),
        column(8,
          bootstrapPanel(heading = "Quarantine utility", id = "plots4",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("travellerFracNoTestPlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("travellerFracNoTestRelUtilityPlot"), height = "450px") %>% withSpinner()),
            ),
            uiOutput(ns("travellerNoTestCaption"))
          )
        )
      ),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 5", class = "panel-primary", id = "pars5",
            selectizeInput(
              ns("yFocus"),
              extLabel("y", "focal travel duration"),
              choices = as.character(1:21),
              selected = as.character(7),
              multiple = FALSE,
              options = list(delimiter = ", ")),
            sliderInput(
              ns("travellerTestDay"),
              extLabel("t<sub>T</sub>", "day after return on which test is conducted"),
              min = 0, max = 10, value = c(0, 5),
              step = 1,
              width = "100%"),
          )
        ),
        column(8,
          bootstrapPanel(heading = "Testing returning travellers", id = "plots5",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("travellerFracTestPlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("travellerFracTestRelUtilityPlot"), height = "450px") %>% withSpinner()),
            ),
            uiOutput(ns("travellerTestCaption"))
          )
        )
      ),
      fluidRow(
        column(4,
          bootstrapPanel(heading = "Parameter 6", class = "panel-default", id = "pars6",
            tags$i("no additional parameter :(")
          )
        ),
        column(8,
          bootstrapPanel(heading = "Adherence and symptoms", id = "plots6",
            class = "panel-info",
            fluidRow(
              column(6, plotOutput(ns("travellerFracAdherencePlot"), height = "450px") %>% withSpinner()),
              column(6, plotOutput(ns("travellerAsymptomaticPlot"), height = "450px") %>% withSpinner()),
            ),
            uiOutput(ns("travellerAdherenceCaption"))
          )
        )
      )

    ),
    tags$script(src = "quarantineModule/jquery.connections.js"),
    tags$script(src = "quarantineModule/quarantineModule.js")
  )
}

library(tidyverse)
library(shinycssloaders)

ctUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # fluidRow(
      #   column(12,
      #     bootstrapPanel(heading = "Contact Tracing & the number of tertiary cases", class = "panel-primary",
      #       "text")
      #   )
      # ),
      fluidRow(
        column(3,
          bootstrapPanel(heading = "Parameter", class = "panel-primary",
            fluidRow(
              column(9,
                sliderInput(
                  ns("f"),
                  extLabel("f", "fraction of index cases detected"),
                  min = 0, max = 1, value = c(0, 1),
                  step = 0.1, round = -2,
                  width = "100%")
              ),
              column(3,
                numericInput(ns("nfSteps"), "steps", min = 1, max = 10, value = 6)
              )
            ),
            sliderInput(
              ns("delta1"),
              extLabel("𝚫<sub>1</sub>", "delay from symptom onset to isolation of index case [days]"),
              min = 0, max = 10, value = c(0, 4),
              step = 1,
              width = "100%"),
            sliderInput(
              ns("tau"),
              extLabel("𝛕", "duration of lookback prior to symptom onset in the index case [days]"),
              min = 0, max = 10, value = 0,
              step = 1,
              width = "100%"),
            sliderInput(
              ns("g"),
              extLabel("g",
                "probability to quarantine a secondary contact that was infected within the contact tracing window"),
              min = 0, max = 1, value = 0,
              step = 0.1, round = -2,
              width = "100%"),
            sliderInput(
              ns("delta2"),
              extLabel("𝚫<sub>2</sub>",
                "delay between isolating the index case and quarantining the secondary contacts [days]"),
              min = 0, max = 10, value = 0,
              step = 1,
              width = "100%"),
            sliderInput(
              ns("re"),
              extLabel("R<sub>e</sub>", "effective reproductive number"),
              min = 0, max = 15, value = 1.2,
              step = 0.1, round = -2,
              width = "100%")
          )
        ),
        column(9,
          bootstrapPanel(
            heading = "Number of tertiary cases per index case as a function of the testing & isolating delay",
            class = "panel-primary",
              plotOutput(ns("plot"), height = "600px") %>% withSpinner()
          )
        )
      )
    )
  )
}

ctServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      params <- reactive({
        params <- list(
          f = seq(from = input$f[1], to = input$f[2], length.out = input$nfSteps),
          Delta1 = seq(from = input$delta1[1], to = input$delta1[2], by = 1), # time
          tau = input$tau, # times
          g = input$g, # prob
          Delta2 = input$delta2, # times
          Re = input$re
        )
        return(params)
      })

      cInf <- reactive({
        cInf <- computeCasesInf(params())
        cInf$Delta1 <- as.numeric(levels(cInf$Delta1))[cInf$Delta1]

        return(cInf)
      })

      output$test <- renderPrint({
        params()
      })

      output$plot <- renderPlot({
        ggplot(cInf(), aes(x = Delta1, y = cases, colour = f, shape = f)) +
          geom_hline(yintercept = 1, colour = "darkgrey", size = 1.2) +
          geom_hline(yintercept = params()$Re^2, colour = "darkgrey", size = 1.2) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          scale_x_continuous(breaks = params()$Delta1) +
          scale_colour_viridis_d(option = "plasma") +
          labs(x = expression(Delta[1] ~ "(days)"), y = expression("tertiary cases" ~ n[3])) +
          plotTheme +
          theme(
            plot.margin = margin(40, 10, 10, 10, unit = "pt")
          )
      })
    }
  )
}

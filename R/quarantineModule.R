library(tidyverse)
library(shinycssloaders)

quarantineDurationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          div(class = "panel panel-primary",
            div(class = "panel-heading", "Quarantine duration"),
            div(class = "panel-body", "text")
          )
        )
      ),
      # empirical distributions
      fluidRow(
        column(9,
          bootstrapPanel(heading = "Empirical distributions", class = "panel-primary",
            fluidRow(
              column(12, "text")
            ),
            hr(),
            fluidRow(
              column(4,
                plotOutput(ns("incDistPlot"), height = "300px"),
                fluidRow(
                  column(4,
                    numericInput(ns("incMeanLog"), "log(mean)", step = 0.001, value = 1.42)
                  ),
                  column(4,
                    numericInput(ns("sdLog"), "log(sd)", step = 0.001, value = 0.661)
                  )
                )
              ),
              column(4,
                plotOutput(ns("infProfPlot"), height = "300px"),
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
                )
              ),
              column(4,
                plotOutput(ns("genTimePlot"), height = "300px"),
                fluidRow(
                  column(4,
                    numericInput(ns("genShape"), "shape", step = 0.001, value = 3.2862)
                  ),
                  column(4,
                    numericInput(ns("genScale"), "scale", step = 0.001, value = 6.1244)
                  )
                )
                #data.frame(shape = 3.2862, scale = 6.1244)
              )
            )
          )
        ),
        column(3,
          bootstrapPanel(heading = "Something or Nothing", class = "panel-primary",
            "Schroedingers Box"
          ),
          bootstrapPanel(heading = "Moar Boxes", class = "panel-info",
            "MOAR Colors!"
          ),
          bootstrapPanel(heading = "Moar Boxes", class = "panel-success",
            "MOAR Colors!"
          ),
          bootstrapPanel(heading = "Moar Boxes", class = "panel panel-danger",
            "MOAR Colors!"
          )
        )
      ),
      # Scenario 1: Quarantining secondary cases after contact tracing
      fluidRow(
        column(3,
          bootstrapPanel(heading = "Parameter", class = "panel-primary",
            sliderInput(
              ns("quarantineDelay"),
              extLabel("𝚫<sub>quarantine</sub>", "delay to quarantine"),
              min = 0, max = 10, value = c(0, 4),
              step = 1,
              width = "100%"),
            sliderInput(
              ns("quarantineDuration"),
              extLabel("n", "duration of quarantine"),
              min = 0, max = 30, value = c(0, 15),
              step = 1,
              width = "100%"),
            sliderInput(
              ns("t2"),
              extLabel("t<sub>2</sub>", "mean infection time of secondary cases"),
              min = 0, max = 10, value = 0,
              step = 1,
              width = "100%"),
            sliderInput(
              ns("tS2"),
              extLabel("t<sub>S<sub>2</sub></sub>", "mean incubation time"),
              min = 0, max = 10, value = 5,
              step = 1,
              width = "100%")
          )
        ),
        column(9,
          bootstrapPanel(heading = "Scenario 1: Quarantining secondary cases after contact tracing",
            class = "panel-primary",
            fluidRow(
              column(6, plotOutput(ns("fracPlot"), height = "400px")),
              column(6, plotOutput(ns("fracRelUtilityPlot"), height = "400px"))
            )
          )
        )
      )
    )
  )
}

quarantineDurationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #' False negative probabilities
      falseNeg <- approxfun(
        x = c(0, 1, 4, 5, 6, 7, 8, 9),
        y = c(1, 1, 0.67, 0.38, 0.25, 0.21, 0.20, 0.21))

      incParams <- reactive({
        incParams <- data.frame(meanlog = input$incMeanLog, sdlog = input$sdLog)
        incParams$mean <- exp(incParams$meanlog + (incParams$sdlog^2) / 2)
        return(incParams)
      })

      incDist <- reactive({
        times <- seq(0, 20, length.out = 101)
        incDist <- data.frame(
          time = times,
          pdf = getIncubationPeriod(times = times, params = incParams()),
          CDF = getIncubationPeriod(times = times, params = incParams(), CDF = TRUE)
        )
        return(incDist)
      })

      genParams <- reactive({
        genParams <- data.frame(shape = input$genShape, scale = input$genScale)
        return(genParams)
      })

      genTime <- reactive({
        times <- seq(0, 15, 0.1)
        genTime <- data.frame(
          t = times,
          pdf = getGenDist(times = times, params = genParams()),
          CDF = getGenDist(times = times, params = genParams(), CDF = TRUE)
        )
        return(genTime)
      })

      infParams <- reactive({
        infParams <- data.frame(shift = input$infShift, scale = input$infScale, df = input$infDf)
        return(infParams)
      })

      infProf <- reactive({
        times <- seq(-10, 15, 0.5)
        infProf <- data.frame(
          t = times,
          pdf = getInfectivityProfile(times = times, params = infParams()),
          CDF = getInfectivityProfile(times = times, params = infParams(), CDF = TRUE)
        )
        return(infProf)
      })

      fracPars <- reactive({
        fracPars <- list(
          quarantineDelay = seq(input$quarantineDelay[1], input$quarantineDelay[2]),
          t2 = input$t2,
          tS2 = input$tS2,
          quarantineDuration = seq(input$quarantineDuration[1], input$quarantineDuration[2]),
          n = 10)
      })

      frac <- reactive({
        quarantineDelay <- fracPars()$quarantineDelay
        t2 <- fracPars()$t2
        tS2 <- fracPars()$tS2
        quarantineDuration <- fracPars()$quarantineDuration

        frac <- lapply(quarantineDelay, function(d) {
          thisDuration <- quarantineDuration[quarantineDuration > d]
          frac <- getIntegral(upper = t2 + thisDuration - tS2, lower = d - tS2, params = infParams())
          data.frame(
            quarantineDelay = factor(d, levels = quarantineDelay),
            quarantineDuration = thisDuration,
            fraction = frac
          )
        }) %>% bind_rows()

        return(frac)
      })

      fracRelUtility <- reactive({
        quarantineDelay <- fracPars()$quarantineDelay
        t2 <- fracPars()$t2
        tS2 <- fracPars()$tS2
        quarantineDuration <- fracPars()$quarantineDuration
        n <- fracPars()$n

        fracRelUtility <- lapply(quarantineDelay, function(d) {
          thisDuration <- quarantineDuration[quarantineDuration > d]
          relUtility <- ( getIntegral(upper = t2 + thisDuration - tS2, lower = d - tS2, params = infParams()) /
            (t2 + thisDuration - d) ) /
            ( getIntegral(upper = t2 + n - tS2, lower = d - tS2, params = infParams()) / (t2 + n - d) )
          data.frame(
            quarantineDelay = factor(d, levels = quarantineDelay),
            quarantineDuration = thisDuration,
            relUtility = relUtility
          )
        }) %>% bind_rows()

        return(fracRelUtility)
      })

      # outputs
      output$incDistPlot <- renderPlot({
        ggplot(incDist(), aes(x = time, y = pdf)) +
          geom_line() +
          geom_vline(xintercept = incParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
            label = paste0("mean = ", round(incParams()$mean, 1), " days"),
            x = incParams()$mean, y = 0.15,
            hjust = 0, vjust = 0) +
          coord_cartesian(ylim = c(0, 0.25)) +
          labs(x = "incubation period (days)", y = "probability density") +
          ggtitle("A: incubation period") +
          plotTheme

      })
      output$infProfPlot <- renderPlot({
        ggplot(infProf(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = 0, colour = "darkgrey") +
          geom_line() +
          coord_cartesian(ylim = c(0, 0.25)) +
          labs(x = "days after onset of symptoms", y = "probability density") +
          ggtitle("B: infectivity profile") +
          plotTheme
      })
      output$genTimePlot <- renderPlot({
        ggplot(genTime(), aes(x = t, y = pdf)) +
          #geom_vline(xintercept = 0, colour = "darkgrey") +
          geom_line() +
          coord_cartesian(ylim = c(0,0.25)) +
          labs(x = "generation time", y = "probability density") +
          ggtitle("C: generation time dist.") +
          plotTheme
      })

      output$fracPlot <- renderPlot({
        ggplot(frac(), aes(x = quarantineDuration, y = fraction, colour = quarantineDelay)) +
          geom_line() +
          geom_point() +
          scale_x_continuous(limits = input$quarantineDuration) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9, aesthetics = c("colour","fill"), name = "delay to\nquarantine") +
          labs(x = "quarantine duration (days)", y = "fraction of tertiary cases\nprevented by quarantine") +
          plotTheme
      })

      output$fracRelUtilityPlot <- renderPlot({
        ggplot(fracRelUtility(),
            aes(x = quarantineDuration, y = relUtility, colour = quarantineDelay)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = fracPars()$n, color = "darkgrey", size = 1.2) +
          geom_line() +
          geom_point() +
          scale_x_continuous(limits = input$quarantineDuration) +
          #scale_y_continuous(limits = c(0,1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9, aesthetics = c("colour","fill"),
            name = "delay to\nquarantine") +
          labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
          plotTheme
      })

    }
  )
}

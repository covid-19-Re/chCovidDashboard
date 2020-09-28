library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)

getIntegral <- function(upper, lower, tE, params) {
  getGenDist(times = upper - tE, params = params, CDF = T) -
    getGenDist(times = lower - tE, params = params, CDF = T)
}

quarantineDurationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # False negative probabilities
      falseNeg <- approxfun(
        x = c(0, 1, 4, 5, 6, 7, 8, 9, 21),
        y = c(1, 1, 0.67, 0.38, 0.25, 0.21, 0.20, 0.21, 0.66))

      # DISTRIBUTIONS
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
          genParams$mean <- genParams$scale * gamma(1 + 1 / genParams$shape)
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
          infParams$mean <- infParams$shift / infParams$scale
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

        yLim <- c(0, 0.22)
        labY <- 0.21
        output$incDistPlot <- renderPlot({
          ggplot(incDist(), aes(x = time, y = pdf)) +
            geom_vline(xintercept = incParams()$mean, linetype = "dashed", alpha = 0.5) +
            annotate("text",
              label = paste0(" mean = ", round(incParams()$mean, 1), " days"),
              x = incParams()$mean, y = labY,
              hjust = 0, vjust = 0) +
            geom_line() +
            coord_cartesian(ylim = yLim) +
            labs(x = "incubation period (days)", y = "probability density") +
            ggtitle("C: incubation period") +
            plotTheme + theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
        })

        output$infProfPlot <- renderPlot({
          ggplot(infProf(), aes(x = t, y = pdf)) +
            #geom_vline(xintercept = 0, colour = "darkgrey") +
            geom_vline(xintercept = infParams()$mean, linetype = "dashed", alpha = 0.5) +
            annotate("text",
              label = paste0(" mean = ", round(infParams()$mean, 1), " days"),
              x = infParams()$mean, y = labY,
              hjust = 0, vjust = 0) +
            geom_line() +
            coord_cartesian(ylim = yLim) +
            labs(x = "days after onset of symptoms", y = "probability density") +
            ggtitle("B: infectivity profile") +
            plotTheme + theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
        })

        output$genTimePlot <- renderPlot({
          ggplot(genTime(), aes(x = t, y = pdf)) +
            geom_vline(xintercept = genParams()$mean, linetype = "dashed", alpha = 0.5) +
            annotate("text",
              label = paste0(" mean = ", round(genParams()$mean, 1), " days"),
              x = genParams()$mean, y = labY,
              hjust = 0, vjust = 0) +
            geom_line() +
            coord_cartesian(ylim = yLim) +
            labs(x = "generation time", y = "probability density") +
            ggtitle("A: generation time dist.") +
            plotTheme + theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
        })

      # SC1: QUARTANTINE UTILITY, NO TESTING
        fracPars <- reactive({
          fracPars <- list(
            tE = 0,
            nCompare = input$nCompare,
            DeltaQ = input$quarantineDelay,
            DeltaQ.vals = seq(max(0, input$quarantineDelay - 2), input$quarantineDelay + 2),
            n.vals = seq(input$quarantineDuration[1], input$quarantineDuration[2]))
        })

        fracNoTest <- reactive({
          tE <- fracPars()$tE
          nCompare <- fracPars()$nCompare
          DeltaQ.vals <- fracPars()$DeltaQ.vals
          n.vals <- fracPars()$n.vals

          fracNoTest <- lapply(DeltaQ.vals, function(DeltaQ) {
            n <- n.vals[n.vals > DeltaQ]
            frac <- getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams())
            data.frame(
              DeltaQ = factor(DeltaQ, levels = DeltaQ),
              n = n,
              fraction = frac
            )
          }) %>% bind_rows()

          fracRelUtility <- lapply(DeltaQ.vals, function(DeltaQ) {
            nPrime <- n.vals[n.vals > DeltaQ]
            relUtility <-
              (getIntegral(upper = tE + nPrime, lower = DeltaQ, tE = tE, params = genParams()) /
              (tE + nPrime - DeltaQ)) /
              (getIntegral(upper = tE + nCompare, lower = DeltaQ, tE = tE, params = genParams()) /
              (tE + nCompare - DeltaQ))
            data.frame(
              DeltaQ = factor(DeltaQ, levels = DeltaQ),
              n = nPrime,
              relUtility = relUtility
            )
          }) %>% bind_rows()

          return(list(frac = fracNoTest, utility = fracRelUtility))
        })

        output$fracNoTestPlot <- renderPlot({
          labs <- paste(levels(fracNoTest()$frac$DeltaQ), ifelse(levels(fracNoTest()$frac$DeltaQ) == 1, "day", "days"))
          names(labs) <- levels(fracNoTest()$frac$DeltaQ)

          ggplot(fracNoTest()$frac, aes(x = n, y = fraction, colour = DeltaQ)) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour", "fill"), name = "delay to\nquarantine", labels = labs) +
            labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
            plotTheme + theme(legend.position = "right", legend.background = element_blank())
        })

        output$fracNoTestRelUtilityPlot <- renderPlot({
          ggplot(fracNoTest()$utility, aes(x = n, y = relUtility, colour = DeltaQ)) +
            geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
            geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2]), ylim = c(0,4)) +
            #scale_y_continuous(limits = c(0,1), labels = scales::percent) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour", "fill"), name = "delay to\nquarantine") +
            labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
            plotTheme + theme(legend.position = "None")
        })

        output$noTestCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          tE <- fracPars()$tE

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> Fraction of total onward transmission per quarantined infected contact ",
            "that is prevented by quarantine. ",
            "<i>(right)</i> Relative utility of different quarantine strategies compared to ",
            "<strong>n = {nCompare}</strong> days. ",
            "Colours represent the delay to starting quarantine, &Delta;<sub>Q</sub>. ",
            "We use t<sub>E</sub> = 0, which from the infectivity profile is the mean infection time of contacts ",
            "if the index case develops symptoms at t = 0",
            "</span>"))
        })

      # SC1: TESTING & RELEASING

        fracTestPars <- reactive({
          fracTestPars <- list(
            DeltaT = input$testDuration,
            s = input$testSpecificity,
            r = input$transmissionReduction,
            x.vals = seq(input$testDay[2], input$testDay[1]))
          return(fracTestPars)
        })

        # earliest test date is first day of quarantine
        observe({
          sliderValue <- input$testDay
          updateSliderInput(session, "testDay", min = 0, max = 10,
            value = c(max(sliderValue[1], input$quarantineDelay), sliderValue[2]))
        })

        fracTest <- reactive({
          DeltaQ <- fracPars()$DeltaQ
          DeltaT <- fracTestPars()$DeltaT
          tE <- fracPars()$tE
          s <- fracTestPars()$s
          r <- fracTestPars()$r
          nCompare <- fracPars()$nCompare
          n.vals <- fracPars()$n.vals
          x.vals <- fracTestPars()$x.vals


          fracTest <- lapply(x.vals, function(x) {
            nPrime <- n.vals[n.vals >= x + DeltaT]
            frac <- getIntegral(upper = tE + x + DeltaT, lower = DeltaQ, tE = tE, params = genParams()) +
              (1 - falseNeg(x - tE)) *
              getIntegral(upper = tE + nPrime, lower = tE + x + DeltaT, tE = tE, params = genParams())
            data.frame(
              x = factor(x, levels = x.vals),
              n = nPrime,
              fraction = frac
            )
          }) %>% bind_rows()

          fracTestReduced <- lapply(x.vals, function(x) {
            nPrime <- n.vals[n.vals >= x + DeltaT]
            frac <- getIntegral(upper = tE + x + DeltaT, lower = DeltaQ, tE = tE, params = genParams()) +
              (1 - falseNeg(x - tE) + r * falseNeg(x - tE)) *
              getIntegral(upper = tE + nPrime, lower = tE + x + DeltaT, tE = tE, params = genParams())
            data.frame(
              x = factor(x, levels = x.vals),
              n = nPrime,
              fraction = frac
            )
          }) %>% bind_rows()

          fracTestRelUtility <- lapply(x.vals, function(x) {
            nPrime <- n.vals[n.vals >= x + DeltaT]
            relUtility <- (
                (getIntegral(upper = tE + x + DeltaT, lower = DeltaQ, tE = tE, params = genParams()) +
                (1 - falseNeg(x - tE)) *
                getIntegral(upper = tE + nPrime, lower = tE + x + DeltaT, tE = tE, params = genParams())) /
                (tE + x + DeltaT - DeltaQ + s * (1 - falseNeg(x - tE)) * (nPrime - x - DeltaT))
              ) / (
                getIntegral(upper = tE + nCompare, lower = DeltaQ, tE = tE, params = genParams()) /
                (tE + nCompare - DeltaQ)
              )
            data.frame(
              x = factor(x, levels = x.vals),
              n = nPrime,
              relUtility = relUtility
            )
          }) %>% bind_rows()

          relUtilityReduced <- lapply(x.vals, function(x) {
            nPrime <- n.vals[n.vals >= x + 2]
            relUtility <- (
                (getIntegral(upper = tE + x + DeltaT, lower = DeltaQ, tE = tE, params = genParams()) +
                (1 - falseNeg(x - tE) + r * falseNeg(x - tE)) *
                getIntegral(upper = tE + nPrime, lower = tE + x + DeltaT, tE = tE, params = genParams())) /
                (tE + x + DeltaT - DeltaQ + s * (1 - falseNeg(x - tE)) * (nPrime - x - DeltaT))
              ) / (
                getIntegral(upper = tE + nCompare, lower = DeltaQ, tE = tE, params = genParams()) /
                (tE + nCompare - DeltaQ)
              )
            data.frame(
              x = factor(x, levels = x.vals),
              n = nPrime,
              relUtility = relUtility
            )
          }) %>% bind_rows()

          return(list(
            frac = fracTest,
            fracReduced = fracTestReduced,
            utility = fracTestRelUtility,
            utilityReduced = relUtilityReduced))
        })

        output$fracTestPlot <- renderPlot({
          ggplot(fracTest()$frac, aes(x = n, y = fraction, colour = x)) +
            geom_line(data = filter(fracNoTest()$frac, DeltaQ == input$quarantineDelay),
              colour = "black", linetype = "dashed", size = 1.2) +
            geom_line(data = fracTest()$reduced, linetype = "dotted", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            scale_colour_viridis_d(option = "viridis", end = 0.9,
              aesthetics = c("colour","fill"), name = "day of test") +
            labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
            plotTheme + theme(legend.position = "right")
        })

        output$fracTestRelUtilityPlot <- renderPlot({
          ggplot(fracTest()$utility, aes(x = n, y = relUtility, colour = x)) +
            geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
            geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
            geom_line(data = fracTest()$utilityReduced, linetype = "dotted", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2]), ylim = c(0,4)) +
            #scale_y_continuous(limits = c(0,1), labels = scales::percent) +
            scale_colour_viridis_d(option = "viridis", end = 0.9,
              aesthetics = c("colour", "fill"), name = "day of test") +
            labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
            plotTheme + theme(legend.position = "none")
        })

        output$testCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          tE <- fracPars()$tE
          DeltaQ <- fracPars()$DeltaQ
          DeltaT <- fracTestPars()$DeltaT
          s <- fracTestPars()$s
          r <- fracTestPars()$r

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> The impact of the test-and-release quarantine strategy, in terms of what fraction of total ",
            "onward transmission per infected traced contact is prevented by quarantine. ",
            "The dashed line shows the standard quarantine without testing. ",
            "<i>(right)</i> Relative utility of different test-and-release quarantine strategies compared to standard ",
            "quarantine with <strong>n = {nCompare}</strong> days. ",
            "We use t<sub>E</sub> = 0, which from the infectivity profile is the mean infection time of contacts ",
            "if the index case develops symptoms at t = 0, and <strong>&Delta;<sub>Q</sub> = {DeltaQ}</strong> ",
            "as the delay until quarantine begins. ",
            "Individuals are tested on day x after exposure (colour) and released on day x+&Delta;<sub>T</sub> if negative ",
            "(we assume it takes <strong>&Delta;<sub>T</sub> = {DeltaT}</strong> days to receive a test result). ",
            "We assume a specificity of <strong>s = {s}</strong> and that there are no false-positive tests. ",
            "Dotted lines assume the released individuals have a reduced transmission ",
            "(<strong>r = {r}</strong>) due to extra hygiene and social distancing measures imposed ",
            "by reduced quarantine",
            "</span>"))
        })

      # SC1: ADHERENCE AND SYMPTOMS

        fracAdherencePars <- reactive({
          fracAdherencePars <- list(
            tS = input$tSymptoms,
            a.vals = seq(input$adherence[1], input$adherence[2], input$adherenceBy))
          return(fracAdherencePars)
        })

        observe({
          updateNumericInput(session, "tSymptoms", value = fracPars()$tE + incParams()$mean)
        })

        fracAdherence <- reactive({
          DeltaQ <- fracPars()$DeltaQ
          DeltaQ.vals <- fracPars()$DeltaQ.vals
          tE <- fracPars()$tE
          tS <- fracAdherencePars()$tS
          n.vals <- fracPars()$n.vals
          nCompare <- fracPars()$nCompare
          a.vals <- fracAdherencePars()$a.vals


          fracAdherence <- lapply(a.vals, function(a) {
            n <- n.vals[n.vals >= DeltaQ]
            frac <- a * (getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams())) +
              (1 - a) * (getIntegral(upper = pmin(tE + n, tS), lower = DeltaQ, tE = tE, params = genParams()))
            data.frame(
              a = factor(a, levels = a.vals),
              n = n,
              fraction = frac
            )
          }) %>% bind_rows()

          relAdherence <- lapply(DeltaQ.vals, function(DeltaQ) {
            nPrime <- n.vals[n.vals > DeltaQ]
            relAdherence <- getIntegral(upper = tE + nCompare, lower = DeltaQ, tE = tE, params = genParams()) /
              getIntegral(upper = tE + nPrime, lower = DeltaQ, tE = tE, params = genParams())
            data.frame(
              DeltaQ = factor(DeltaQ, levels = DeltaQ.vals),
              n = nPrime,
              relAdherence = relAdherence
            )
          }) %>% bind_rows()

          return(list(
            frac = fracAdherence,
            relAdherence = relAdherence))
        })

        output$fracAdherencePlot <- renderPlot({
          ggplot(fracAdherence()$frac, aes(x = n, y = fraction, colour = a)) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour","fill"), name = "fraction\nasymptomatic") +
            labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
            plotTheme + theme(legend.position = "right", legend.background = element_blank())
        })

        output$relAdherencePlot <- renderPlot({
          ggplot(fracAdherence()$relAdherence, aes(x = n, y = relAdherence, colour = DeltaQ)) +
            geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
            geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            coord_cartesian(ylim = c(0, 4)) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour", "fill"), name = "delay to\nquarantine") +
            labs(x = "quarantine duration (days)",
              y = "relative adherence required to\nmaintain quarantine efficacy") +
            plotTheme + theme(legend.position = "right")
        })

        output$adherenceCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          tE <- fracPars()$tE
          DeltaQ <- fracPars()$DeltaQ
          tS <- fracAdherencePars()$tS

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> The change in adherence needed to maintain quarantine efficacy of the ",
            "<strong>n = {nCompare}</strong> day strategy if we change the quarantine duration to n' days (x-axis). ",
            "<i>(right)</i> The impact of symptomatic cases on the fraction of total onward transmission per infected ",
            "traced contact that is prevented by quarantine. We fix <strong>&Delta;<sub>Q</sub> = {DeltaQ}</strong> ",
            "as the delay until quarantine, and <strong>t<sub>S</sub> = {round(tS,0)}</strong>, ",
            "which is the mean incubation time. ",
            "In both panels we use t<sub>E</sub> = {tE}, which is the mean infection time of secondary cases based ",
            "on the infectivity profile.",
            "</span>"))
        })

      # SC2: NO TESTING
        travellerFracPars <- reactive({
          travellerFracPars <- list(
            y.vals = na.omit(as.integer(input$travelDuration))
            )
        })

        observe({
          y.valsSelected <- input$travelDuration
          updateSelectizeInput(session, "travelDuration", selected = sort(as.integer(y.valsSelected)))
        })

        travellerFracNoTest <- reactive({
          nCompare <- fracPars()$nCompare
          y.vals <- travellerFracPars()$y.vals
          n.vals <- fracPars()$n.vals

          travellerFracNoTest <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            frac <- lapply(n.vals, function(n) {
              frac <- mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()))
              data.frame(
                y = factor(y, levels = y.vals),
                n = n,
                fraction = frac
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          travellerFracRelUtility <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            relUtility <- lapply(n.vals[n.vals > 0], function(n) {
              relUtility <- ( mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()))/n )/
                (mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams()))/nCompare )
              data.frame(
                y = factor(y, levels = y.vals),
                n = n,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          return(list(frac = travellerFracNoTest, utility = travellerFracRelUtility))
        })

        output$travellerFracNoTestPlot <- renderPlot({
          ggplot(travellerFracNoTest()$frac, aes(x = n, y = fraction, colour = y)) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour","fill"), name = "duration\nof travel") +
            labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
            plotTheme +
            theme(legend.position = "right", legend.background = element_blank())
        })

        output$travellerFracNoTestRelUtilityPlot <- renderPlot({
          ggplot(travellerFracNoTest()$utility, aes(x = n, y = relUtility, colour = y)) +
            geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
            geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2]), ylim = c(0,4)) +
            #scale_y_continuous(limits = c(0,1), labels = scales::percent) +
            scale_colour_viridis_d(option = "inferno", end = 0.9,
              aesthetics = c("colour","fill"), name = "duration\nof travel") +
            labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
            plotTheme + theme(legend.position = "None")
        })

        output$travellerNoTestCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          tE <- fracPars()$tE
          DeltaQ <- fracPars()$DeltaQ
          tS <- fracAdherencePars()$tS

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> The fraction of total onward transmission per quarantined ",
            "traveller that is prevented by quarantine. ",
            "<i>(right)</i> The relative utility of different quarantine strategies (x-axis) ",
            "compared to <strong>n = {nCompare}</strong> days. ",
            "Colours represent the duration of travel y and we assume infection can occur on any day ",
            "-y &le; t<sub>E</sub> &le; 0 with uniform probability.",
            "</span>"))
        })

      # SC2: TESTING & RRELEASING
        travellerTestPars <- reactive({
          travellerTestPars <- list(
            y = as.integer(input$yFocus),
            tE.vals = seq(-as.integer(input$yFocus), 0),
            x.vals = seq(input$travellerTestDay[1], input$travellerTestDay[2])
          )
        })

        observe({
          isolate(ySelected <- input$yFocus)
          updateSelectizeInput(session, "yFocus",
            choices = sort(as.integer(input$travelDuration)), selected = ySelected)
        })

        travellerFracTest <- reactive({
          y <- travellerTestPars()$y
          tE.vals <- travellerTestPars()$tE.vals
          s <- fracTestPars()$s
          r <- fracTestPars()$r
          nCompare <- fracPars()$nCompare
          n.vals <- fracPars()$n.vals
          x.vals <- travellerTestPars()$x.vals
          DeltaT <- fracTestPars()$DeltaT

          fracTest <- lapply(x.vals, function(x) {
            frac <- lapply(n.vals[n.vals >= x + DeltaT], function(n) {
              frac <- mean(getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - falseNeg(x - tE.vals)) *
                getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams()))
              data.frame(
                x = factor(x, levels = x.vals),
                n = n,
                fraction = frac
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          #' Fraction prevented by test and release strategy with reduced post-quarantine transmission
          fracTestReduced <- lapply(x.vals, function(x) {
            fracReduced <- lapply(n.vals[n.vals >= x + DeltaT], function(n) {
              frac <- mean(getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
                getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams()))
              data.frame(
                x = factor(x, levels = x.vals),
                n = n,
                fraction = frac
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          relUtility <- lapply(x.vals, function(x) {
            relUtility <- lapply(n.vals[n.vals >= x + DeltaT], function(n) {
              relUtility <- (
                  mean(getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                  (1 - falseNeg(x - tE.vals)) *
                  getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) /
                  mean(x + DeltaT + s * (1 - falseNeg(x - tE.vals)) * (n - x - DeltaT))
                ) / (
                  mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams())) / nCompare
                )
              data.frame(
                x = factor(x, levels = x.vals),
                n = n,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          #' Relative utility with reduced post-quarantine transmission
          relUtilityReduced <- lapply(x.vals, function(x) {
            relUtility <- lapply(n.vals[n.vals >= x + DeltaT], function(n) {
              relUtility <- (
                  mean(getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                  (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
                  getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) /
                  mean(x + DeltaT + s * (1 - falseNeg(x - tE.vals)) * (n - x - DeltaT))
                ) / (
                  mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams())) / nCompare
                )
              data.frame(
                x = factor(x, levels = x.vals),
                n = n,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()

          return(list(
            frac = fracTest,
            fracReduced = fracTestReduced,
            utility = relUtility,
            utilityReduced = relUtilityReduced))
        })

        output$travellerFracTestPlot <- renderPlot({
          ggplot(travellerFracTest()$frac, aes(x = n, y = fraction, colour = x)) +
            geom_line(data = filter(travellerFracNoTest()$frac, y == input$yFocus),
              colour = "black", linetype = "dashed", size = 1.2) +
            geom_line(data = travellerFracTest()$fracReduced, linetype = "dotted", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
            scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
              aesthetics = c("colour", "fill"), name = "day of test") +
            labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
            plotTheme + theme(legend.position = "right")
        })

        output$travellerFracTestRelUtilityPlot <- renderPlot({
          ggplot(travellerFracTest()$utility, aes(x = n, y = relUtility, colour = x)) +
            geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
            geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
            geom_line(data = travellerFracTest()$utilityReduced, linetype = "dotted", size = 1.2) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2]), ylim = c(0,4)) +
            scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
              aesthetics = c("colour", "fill"), name = "day of test") +
            labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
            plotTheme + theme(legend.position = "none")
        })

        output$travellerTestCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          DeltaT <- fracTestPars()$DeltaT
          s <- fracTestPars()$s
          r <- fracTestPars()$r
          y <- travellerTestPars()$y

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> The impact of test-and-release for quarantined travellers, in terms of what fraction of ",
            "total onward transmission per quarantined infected traveller is prevented by quarantine. ",
            "The dashed line shows the result of standard quarantine without testing. ",
            "<i>(right)</i> The relative utility of different test-and-release quarantine durations compared to ",
            "standard quarantine with duration <strong>n = {nCompare}</strong> days. ",
            "We consider a travel duration of <strong>y = {y}</strong> days and we assume infection can occur on any ",
            "day -y &le; t<sub>E</sub> &le; 0 with uniform probability. Individuals are tested on day x (colour) ",
            "after returning on day 0 and released on day x + &Delta; if negative ",
            "(we assume it takes <strong>&Delta;<sub>T</sub> = {DeltaT}</strong> days to receive a test result). ",
            "We assume a specificity of <strong>s = {s}</strong> and that there are no false-positive test results. ",
            "Dotted lines in both panels assume the released travellers have ",
            "a reduced transmission (<strong>r = {r}</strong>) due to extra hygiene and social distancing ",
            "measures imposed by reduced quarantine.",
            "</span>"))
        })

      # SC2: adherence and symptoms
      travellerFracAdherence <- reactive({
        y.vals <- travellerFracPars()$y.vals
        nCompare <- fracPars()$nCompare
        n.vals <- fracPars()$n.vals

        relAdherence <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y,0)
          relAdherence <- sapply(n.vals, function(n) {
            mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams())) /
              mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()))
          })
          data.frame(
            y = factor(y, levels = y.vals),
            n = n.vals,
            relAdherence = relAdherence
          )
        }) %>% bind_rows()

        tE.vals <- travellerTestPars()$tE.vals
        tS.vals <- tE.vals + incParams()$mean
        a.vals <- fracAdherencePars()$a.vals

        fracAdherence <- lapply(a.vals, function(a) {
          frac <- sapply(n.vals, function(n) {
            mean(a * getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) +
            (1 - a) * getIntegral(upper = pmin(n, tS.vals), lower = 0, tE = tE.vals, params = genParams()))
          })
          data.frame(
            a = factor(a, levels = a.vals),
            n = n.vals,
            fraction = frac
          )
        }) %>% bind_rows()

        return(list(
            frac = fracAdherence,
            relAdherence = relAdherence))
      })

      output$travellerFracAdherencePlot <- renderPlot({
        ggplot(travellerFracAdherence()$relAdherence, aes(x = n, y = relAdherence, colour = y)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          #scale_y_continuous(limits = c(0,1), labels = scales::percent) +
          coord_cartesian(ylim = c(0, 4.5)) +
          scale_colour_viridis_d(option = "inferno", end = 0.9,
            aesthetics = c("colour", "fill"), name = "duration\nof travel") +
          labs(x = "quarantine duration (days)", y = "relative adherence required\nto maintain quarantine efficacy") +
          plotTheme + theme(legend.position = "right", legend.background = element_blank())
      })

      output$travellerAsymptomaticPlot <- renderPlot({
        labs <- scales::percent(fracAdherencePars()$a.vals)
        names(labs) <- fracAdherencePars()$a.vals

         ggplot(travellerFracAdherence()$frac, aes(x = n, y = fraction, colour = a)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", direction = -1, end = 0.9,
            aesthetics = c("colour", "fill"), name = "fraction\nasymptomatic", labels = labs) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme + theme(legend.position = "right", legend.background = element_blank())
      })

      output$travellerAdherenceCaption <- renderUI({
          nCompare <- fracPars()$nCompare
          y <- travellerTestPars()$y
          meanInc <- round(incParams()$mean, 0)

          HTML(glue(
            "<span class='help-block' style='font-size:15px;'>",
            "<i>(left)</i> The change in adherence needed to maintain quarantine efficacy of the ",
            "<strong>n = {nCompare}</strong> day strategy if we change the quarantine duration to n' days (x-axis). ",
            "<i>(right)</i> The impact of symptomatic cases on the fraction of total onward transmission per quarantined ",
            "traveller that is prevented by quarantine. We fix the travel duration to <strong>y = {y}</strong> days ",
            "and assume t<sub>E</sub> is uniformly distributed between -y and 0. We use the mean incubation time of ",
            "<strong>{meanInc}</strong> days, such that <strong>t<sub>S</sub> = t<sub>E</sub> + {meanInc}</strong>.",
            "</span>"))
        })

    }
  )
}

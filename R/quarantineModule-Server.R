library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)
library(cowplot)

getIntegral <- function(upper, lower, tE, params) {
  getGenDist(times = upper - tE, params = params, CDF = T) -
    getGenDist(times = lower - tE, params = params, CDF = T)
}

quarantineDurationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })
      # False negative probabilities ----
      falseNeg <- approxfun(
        x = c(0, 1, 4, 5, 6, 7, 8, 9, 21),
        y = c(1, 1, 0.67, 0.38, 0.25, 0.21, 0.20, 0.21, 0.66)
      )

      # DISTRIBUTIONS ----
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

      # DISTRIBUTIONS PLOTS ----
      yLim <- c(0, 0.22)
      labY <- 0.21
      output$incDistPlot <- renderPlot({
        ggplot(incDist(), aes(x = time, y = pdf)) +
          geom_vline(xintercept = incParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
            label = paste0(" mean = ", round(incParams()$mean, 1), " days"),
            x = incParams()$mean, y = labY,
            hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(ylim = yLim) +
          labs(x = "incubation period (days)", y = "probability density") +
          ggtitle("C: incubation period") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      output$infProfPlot <- renderPlot({
        ggplot(infProf(), aes(x = t, y = pdf)) +
          # geom_vline(xintercept = 0, colour = "darkgrey") +
          geom_vline(xintercept = infParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
            label = paste0(" mean = ", round(infParams()$mean, 1), " days"),
            x = infParams()$mean, y = labY,
            hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(ylim = yLim) +
          labs(x = "days after onset of symptoms", y = "probability density") +
          ggtitle("B: infectivity profile") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      output$genTimePlot <- renderPlot({
        ggplot(genTime(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = genParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
            label = paste0(" mean = ", round(genParams()$mean, 1), " days"),
            x = genParams()$mean, y = labY,
            hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(ylim = yLim) +
          labs(x = "generation time", y = "probability density") +
          ggtitle("A: generation time distribution") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      # SC1: QUARTANTINE UTILITY, NO TESTING ----
      fracPars <- reactive({
        fracPars <- list(
          tE = 0,
          nCompare = input$nCompare,
          DeltaQ.vals = seq(input$quarantineDelay[1], input$quarantineDelay[2]),
          n.vals = seq(input$quarantineDuration[1], input$quarantineDuration[2])
        )
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

      # PLOTS ----
      output$fracNoTestPlot <- renderPlot({
        labs <- paste(levels(fracNoTest()$frac$DeltaQ), ifelse(levels(fracNoTest()$frac$DeltaQ) == 1, "day", "days"))
        names(labs) <- levels(fracNoTest()$frac$DeltaQ)

        ggplot(fracNoTest()$frac, aes(x = n, y = fraction, colour = DeltaQ)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$fracNoTestRelUtilityPlot <- renderPlot({
        ggplot(fracNoTest()$utility, aes(x = n, y = relUtility, colour = DeltaQ)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          coord_cartesian(xlim = c(input$quarantineDuration[1], input$quarantineDuration[2]), ylim = c(0, 4)) +
          # scale_y_continuous(limits = c(0,1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$fracNoTestLegend <- renderPlot({
        labs <- paste(levels(fracNoTest()$frac$DeltaQ), ifelse(levels(fracNoTest()$frac$DeltaQ) == 1, "day", "days"))
        names(labs) <- levels(fracNoTest()$frac$DeltaQ)

        plot <- ggplot(fracNoTest()$frac, aes(x = n, y = fraction, colour = DeltaQ)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "delay to quarantine",
            labels = labs, guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          plotTheme
        grid::grid.draw(get_legend(plot))
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
          "</span>"
        ))
      })

      # SC1: TESTING & RELEASING ----
      fracTestPars <- reactive({
        fracTestPars <- list(
          DeltaQ = as.integer(input$deltaQfocal),
          DeltaT.vals = seq(input$testResultDelay[1], input$testResultDelay[2]),
          s = input$testSpecificity,
          r = input$transmissionReduction,
          x.vals = seq(as.integer(input$deltaQfocal), input$nCompare)
        )
        return(fracTestPars)
      })

      observe({
        isolate(deltaQselected <- input$deltaQfocal)
        updateSelectizeInput(session, "deltaQfocal",
          choices = as.character(fracPars()$DeltaQ.vals), selected = deltaQselected
        )
      })

      # earliest test date is first day of quarantine
      observe({
        sliderValue <- input$testDay
        updateSliderInput(session, "testDay",
          min = 0, max = 10,
          value = c(max(sliderValue[1], as.integer(input$deltaQfocal)), sliderValue[2])
        )
      })

      fracTest <- reactive({
        DeltaQ <- fracTestPars()$DeltaQ
        DeltaT.vals <- fracTestPars()$DeltaT.vals
        tE <- fracPars()$tE
        s <- fracTestPars()$s
        r <- fracTestPars()$r
        n.vals <- fracPars()$n.vals
        x.vals <- fracTestPars()$x.vals
        n <- fracPars()$nCompare

        

        #' Fraction based on test and release
        fracTest <- lapply(x.vals, function(x) {
          fraction <- getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x - tE)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams())
          data.frame(
            x = x,
            DeltaT = factor(DeltaT.vals),
            release = x + DeltaT.vals,
            fraction = fraction
          )
        }) %>% bind_rows()

        fracNoTest <- data.frame(
          release = x.vals,
          fraction = getIntegral(upper = tE + x.vals, lower = DeltaQ, tE = tE, params = genParams())
        )

        #' Maximum preventable fraction of transmission
        maxPreventable <- getIntegral(upper = Inf, lower = DeltaQ, tE = tE, params = genParams())

        #' Fraction with reduced post-quarantine transmission
        fracTestReduced <- lapply(x.vals, function(x) {
          fraction <- getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x - tE) + r * falseNeg(x - tE)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams())
          data.frame(
            x = x,
            DeltaT = factor(DeltaT.vals),
            release = x + DeltaT.vals,
            fraction = fraction
          )
        }) %>% bind_rows()

        utilityNoTest <- getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams()) / (tE + n - DeltaQ)

        relUtilityNoTest <- lapply(x.vals, function(x) {
          utility <- getIntegral(upper = x, lower = DeltaQ, tE = tE, params = genParams()) /
            (tE + x - DeltaQ)
          relUtility <- utility / utilityNoTest
          data.frame(
            release = x,
            relUtility = relUtility
          )
        }) %>% bind_rows()
        
        #' Relative utility of test-and-release strategy compared to standard
        relUtility <- lapply(x.vals, function(x) {
          utility <- (getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x - tE)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams())) /
            (tE + x + DeltaT.vals - DeltaQ + s * (1 - falseNeg(x - tE)) * (n - x - DeltaT.vals))
          relUtility <- utility / utilityNoTest
          data.frame(
            x = x,
            DeltaT = factor(DeltaT.vals),
            release = x + DeltaT.vals,
            relUtility = relUtility
          )
        }) %>% bind_rows()

        #' Relative utility of test and release with reduced post-quarantine transmission
        relUtilityReduced <- lapply(x.vals, function(x) {
          utility <- (getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x - tE) + r * falseNeg(x - tE)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams()) )/
            (tE + x + DeltaT.vals - DeltaQ + s * (1 - falseNeg(x - tE)) * (n - x - DeltaT.vals))
          relUtility <- utility / utilityNoTest
          data.frame(
            x = x,
            DeltaT = factor(DeltaT.vals),
            release = x + DeltaT.vals,
            relUtility = relUtility
          )
        })
        relUtilityReduced <- do.call(rbind, relUtilityReduced)

        return(list(
          frac = fracTest,
          fracNoTest = fracNoTest,
          maxPreventable = maxPreventable,
          fracReduced = fracTestReduced,
          relUtilityNoTest = relUtilityNoTest,
          relUtility = relUtility,
          relUtilityReduced = relUtilityReduced
        ))
      })

      # PLOTS ----
      fracTestPlotColors <- reactive({
        labs <- paste(fracTestPars()$DeltaT.vals,
          ifelse(fracTestPars()$DeltaT.vals == 1, "day", "days"))
        names(labs) <- fracTestPars()$DeltaT.vals

        colours <- scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
          name = expression("test result delay " ~ (Delta[T])),
          labels = labs,
          guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1))
      })

      fracTestPlot <- reactive({

        colours <- fracTestPlotColors()

        ggplot(fracTest()$frac, aes(x = release, y = fraction, colour = DeltaT)) +
          geom_hline(yintercept = fracTest()$maxPreventable, colour = "darkgrey", size = 1.2) +
          geom_line(data = fracTest()$fracNoTest, color = "darkgrey", size = 1.2) +
          geom_line(data = fracTest()$fracReduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, input$nCompare), breaks = seq(0, input$nCompare, 2)) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          colours +
          labs(x = "day of release", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme

      })

      output$fracTestPlotOut <- renderPlot({
        fracTestPlot() + theme(legend.position = "None")
      })

      fracTestRelUtilityPlot <- reactive({
        colours <- fracTestPlotColors()

        ggplot(fracTest()$relUtility, aes(x = release, y = relUtility, colour = DeltaT)) + 
          geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_line(data = fracTest()$relUtilityNoTest, colour = "darkgrey", size = 1.1) +
          geom_line(data = fracTest()$relUtilityReduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, input$nCompare), breaks = seq(0, input$nCompare, 2)) +
          coord_cartesian(ylim = c(0, 4)) +
          colours +
          labs(x = "day of release", y = "\nrelative utility of quarantine") +
          plotTheme + theme(legend.position = "None")
      })

      output$fracTestRelUtilityPlotOut <- renderPlot({
        fracTestRelUtilityPlot()
      })

      output$fracTestLegend <- renderPlot({
        grid::grid.draw(get_legend(fracTestPlot()))
      })

      output$testCaption <- renderUI({
        nCompare <- fracPars()$nCompare
        tE <- fracPars()$tE
        DeltaQ <- fracTestPars()$DeltaQ
        s <- fracTestPars()$s
        r <- fracTestPars()$r

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The impact of the test-and-release quarantine strategy, in terms of what fraction of total ",
          "onward transmission per infected traced contact is prevented by quarantine. The thick grey lines ",
          "represent the upper and lower bounds for the fraction of transmission that can be prevented by quarantine ",
          "without testing. ",
          "<i>(right)</i> The relative utility of different test-and-release quarantine durations compared to ",
          "standard quarantine with duration <strong>n = {nCompare}</strong> days. The thick grey curve is the ",
          "relative utility of standard quarantine without testing. In both panels we use t<sub>E</sub> = {tE}, ",
          "which from the infectivity profile is the mean infection time of contacts if the index case develops ",
          "symptoms at t = 0, and <strong>&Delta;<sub>Q</sub> = {DeltaQ}</strong> as the delay until quarantine ",
          "begins. Individuals are tested on day x after exposure and released on day x + &Delta;<sub>T</sub> (x-axis) ",
          "if negative (colour corresponds to &Delta;<sub>T</sub>). Positive testing individuals are released at ",
          "day <strong>n = {nCompare}</strong>. We assume that the fraction of individuals in quarantine that are ",
          "infected is <strong>s = {s}</strong>, and that there are no false-positive test results. Dashed lines in ",
          "both panels assume the released individuals have a reduced transmission (<strong>r = {r}</strong>) due ",
          "to reinforced hygiene and social distancing measures.",
          "</span>"
        ))
      })

      # SC1: ADHERENCE AND SYMPTOMS ----
      fracAdherencePars <- reactive({
        fracAdherencePars <- list(
          tS = fracPars()$tE + incParams()$mean,
          a.vals = seq(0, 1, 0.25)
        )
        return(fracAdherencePars)
      })

      fracAdherence <- reactive({
        DeltaQ <- fracTestPars()$DeltaQ
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
          relAdherence = relAdherence
        ))
      })

      # PLOTS ----
      output$fracAdherencePlot <- renderPlot({
        labs <- scales::percent(fracAdherencePars()$a.vals)
        names(labs) <- fracAdherencePars()$a.vals

        ggplot(fracAdherence()$frac, aes(x = n, y = fraction, colour = a)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "viridis", direction = -1, end = 0.9, name = "fraction\nasymptomatic",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$relAdherencePlot <- renderPlot({
        labs <- paste(
          levels(fracAdherence()$relAdherence$DeltaQ),
          ifelse(levels(fracAdherence()$relAdherence$DeltaQ) == 1, "day", "days")
        )
        names(labs) <- levels(fracAdherence()$relAdherence$DeltaQ)

        ggplot(fracAdherence()$relAdherence, aes(x = n, y = relAdherence, colour = DeltaQ)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = fracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          coord_cartesian(ylim = c(0, 4)) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "delay to\nquarantine",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(
            x = "quarantine duration (days)",
            y = "relative adherence required to\nmaintain quarantine efficacy"
          ) +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$adherenceCaption <- renderUI({
        nCompare <- fracPars()$nCompare
        tE <- fracPars()$tE
        DeltaQ <- fracTestPars()$DeltaQ
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
          "</span>"
        ))
      })

      # SC2: NO TESTING ----
      travellerFracPars <- reactive({
        travellerFracPars <- list(
          nCompare = input$nCompareSC2,
          y.vals = na.omit(as.integer(input$travelDuration)),
          n.vals = seq(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2]),
          normalisation = input$normalisation # TRUE: local, FALSE: total
        )
      })

      observe({
        y.valsSelected <- input$travelDuration
        updateSelectizeInput(session, "travelDuration", selected = sort(as.integer(y.valsSelected)))
      })

      travellerFracNoTest <- reactive({
        nCompare <- travellerFracPars()$nCompare
        y.vals <- travellerFracPars()$y.vals
        n.vals <- travellerFracPars()$n.vals
        normalisation <- travellerFracPars()$normalisation

        if (normalisation) {
          travellerFracNoTest <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            frac <- lapply(n.vals, function(n) {
              frac <- mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams()))
              data.frame(
                y = factor(y, levels = y.vals),
                n = n,
                fraction = frac
              )
            }) %>% bind_rows()
          }) %>% bind_rows()
        } else {
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
        }

        if (normalisation) {
          travellerFracRelUtility <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            relUtility <- lapply(n.vals[n.vals > 0], function(n) {
              relUtility <- (mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams())) / n) /
                (mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams()) /
                  getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams())) / nCompare)
              data.frame(
                y = factor(y, levels = y.vals),
                n = n,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()
        } else {
          travellerFracRelUtility <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            relUtility <- lapply(n.vals[n.vals > 0], function(n) {
              relUtility <- (mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams())) / n) /
                (mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams())) / nCompare)
              data.frame(
                y = factor(y, levels = y.vals),
                n = n,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()
        }
        return(list(frac = travellerFracNoTest, utility = travellerFracRelUtility))
      })
      # PLOTS ----
      output$travellerFracNoTestPlot <- renderPlot({
        ggplot(travellerFracNoTest()$frac, aes(x = n, y = fraction, colour = y)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$travellerFracNoTestRelUtilityPlot <- renderPlot({
        ggplot(travellerFracNoTest()$utility, aes(x = n, y = relUtility, colour = y)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = travellerFracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          coord_cartesian(xlim = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2]), ylim = c(0, 4)) +
          # scale_y_continuous(limits = c(0,1), labels = scales::percent) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$travellerFracNoTestLegend <- renderPlot({
        labs <- paste(levels(travellerFracNoTest()$frac$y),
          ifelse(levels(travellerFracNoTest()$frac$y) == 1, "day", "days"))
        names(labs) <- levels(travellerFracNoTest()$frac$y)

        plot <- ggplot(travellerFracNoTest()$frac, aes(x = n, y = fraction, colour = y)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "duration of travel",
            labels = labs, guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          plotTheme
        grid::grid.draw(get_legend(plot))
      })

      output$travellerNoTestCaption <- renderUI({
        nCompare <- travellerFracPars()$nCompare
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of <strong>{normalisation}</strong> onward transmission per quarantined ",
          "traveller that is prevented by quarantine. ",
          "<i>(right)</i> The relative utility of different quarantine strategies (x-axis) ",
          "compared to <strong>n = {nCompare}</strong> days. ",
          "Colours represent the duration of travel y and we assume infection can occur on any day ",
          "-y &le; t<sub>E</sub> &le; 0 with uniform probability.",
          "</span>"
        ))
      })

      # SC2: TESTING & RELEASING ----
      travellerTestPars <- reactive({
        travellerTestPars <- list(
          y = as.integer(input$yFocus),
          tE.vals = seq(-as.integer(input$yFocus), 0),
          DeltaT.vals = seq(input$testResultDelaySC2[1], input$testResultDelaySC2[2]),
          s = input$testSpecificitySC2,
          r = input$transmissionReductionSC2,
          x.vals = seq(0, input$nCompareSC2)
        )
      })

      observe({
        isolate(ySelected <- input$yFocus)
        updateSelectizeInput(session, "yFocus",
          choices = sort(as.integer(input$travelDuration)), selected = ySelected
        )
      })

      travellerFracTest <- reactive({
        y <- travellerTestPars()$y
        tE.vals <- travellerTestPars()$tE.vals
        s <- travellerTestPars()$s
        r <- travellerTestPars()$r
        n <- travellerFracPars()$nCompare
        x.vals <- travellerTestPars()$x.vals
        DeltaT.vals <- travellerTestPars()$DeltaT.vals
        normalisation <- travellerFracPars()$normalisation

        #' Maximum preventable fractions
        maxPreventable <- getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams())
        if (normalisation) {
          maxPreventablePlot <- 1
        } else {
          maxPreventablePlot <- mean(maxPreventable)
          maxPreventable <- 1
        }

        #' Fraction of infection prevented using normal strategy
        fracNoTest <- data.frame(
          release = x.vals,
          fraction  = sapply(x.vals,
            function(n) mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
              maxPreventable))
        )

        #' Fraction prevented by test-and-release strategy
        fracTest <- lapply(x.vals, function(x) {
          fracTest <- lapply(DeltaT.vals, function(DeltaT) {
            fraction <- mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
              (1 - falseNeg(x - tE.vals)) *
              getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / maxPreventable)
            data.frame(
              x = x,
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              release = x + DeltaT,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()
        
        #' Fraction prevented by test and release strategy with reduced post-quarantine transmission
        fracReduced <- lapply(x.vals, function(x) {
          fracReduced <- lapply(DeltaT.vals, function(DeltaT) {
            fraction <- mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
            (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
            getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / maxPreventable)
            data.frame(
              x = x,
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              release = x + DeltaT,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        #' Utility without testing

        utilityNoTest <- mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
          maxPreventable) / n

        relUtilityNoTest <- lapply(x.vals, function(x) {
          utility <- mean(getIntegral(upper = x, lower = 0, tE = tE.vals, params = genParams()) / maxPreventable) / x
          relUtility <- utility / utilityNoTest
          data.frame(
            release = x,
            relUtility = relUtility
          )
        }) %>% bind_rows()

          relUtility <- lapply(x.vals, function(x) {
            relUtility <- lapply(DeltaT.vals, function(DeltaT) {
              utility <- mean( (getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - falseNeg(x - tE.vals)) *
                getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / maxPreventable) /
                mean(x + DeltaT + s*(1 - falseNeg(x - tE.vals)) * (n - x - DeltaT) )
              relUtility <- utility / utilityNoTest
              data.frame(
                x = x,
                DeltaT = factor(DeltaT, levels = DeltaT.vals),
                release = x + DeltaT,
                relUtility = relUtility
              )
            }) %>% bind_rows()
          }) %>% bind_rows()


        #' Relative utility with reduced post-quarantine transmission
          relUtilityReduced <- lapply(x.vals, function(x) {
            relUtility <- lapply(DeltaT.vals, function(DeltaT) {
              utility <- mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
                getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / maxPreventable) /
                mean(x + DeltaT + s * (1 - falseNeg(x - tE.vals)) * (n - x - DeltaT))
              relUtility <- utility / utilityNoTest
              data.frame(
                x = x,
                DeltaT = factor(DeltaT, levels = DeltaT.vals),
                release = x + DeltaT,
                relUtility = relUtility
              )
            })
            relUtility <- do.call(rbind, relUtility)
          })
          relUtilityReduced <- do.call(rbind, relUtilityReduced)


        return(list(
          frac = fracTest,
          fracNoTest = fracNoTest,
          fracReduced = fracReduced,
          maxPreventable = mean(maxPreventable),
          maxPreventablePlot = maxPreventablePlot,
          relUtility = relUtility,
          relUtilityNoTest = relUtilityNoTest,
          relUtilityReduced = relUtilityReduced
        ))
      })

      # PLOTS ----
      travellerFracTestPlotColors <- reactive({
        labs <- paste(travellerTestPars()$DeltaT.vals,
          ifelse(travellerTestPars()$DeltaT.vals == 1, "day", "days"))
        names(labs) <- travellerTestPars()$DeltaT.vals

        colours <- scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
          name = expression("test result delay " ~ (Delta[T])),
          labels = labs,
          guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1))
      })

      travellerFracTestPlot <- reactive({

        colours <- travellerFracTestPlotColors()

        fracPlot <- ggplot(travellerFracTest()$frac, aes(x = release, y = fraction, colour = DeltaT)) +
          geom_hline(yintercept = travellerFracTest()$maxPreventablePlot, colour = "darkgrey", size = 1.2) +
          geom_line(data = travellerFracTest()$fracNoTest, colour = "darkgrey", size = 1.2) +
          geom_line(data = travellerFracTest()$fracReduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, input$nCompareSC2), breaks = seq(0, input$nCompareSC2, 2)) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          colours +
          labs(x = "day of release", y = "fraction of local transmission\nprevented by quarantine") +
          plotTheme
      })

      output$travellerFracTestPlotOut <- renderPlot({
        travellerFracTestPlot() +
          theme(legend.position = "none")
      })

      output$travellerFracTestRelUtilityPlot <- renderPlot({

        colours <- travellerFracTestPlotColors()

        ggplot(travellerFracTest()$relUtility, aes(x = release, y = relUtility, colour = DeltaT)) + 
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = input$nCompareSC2, color = "darkgrey", size = 1.2) +
          geom_line(data = travellerFracTest()$relUtilityNoTest, colour = "darkgrey", size = 1.2) +
          geom_line(data = travellerFracTest()$relUtilityReduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, input$nCompareSC2), breaks = seq(0, input$nCompareSC2, 2)) +
          coord_cartesian(ylim = c(0, 4)) +
          colours +
          labs(x = "day of release", y = "\nrelative utility of quarantine") +
          plotTheme + theme(legend.position = "None")

      })

      output$travellerFracTestLegend <- renderPlot({
        grid::grid.draw(get_legend(travellerFracTestPlot()))
      })

      output$travellerTestCaption <- renderUI({
        nCompare <- travellerFracPars()$nCompare
        s <- travellerTestPars()$s
        r <- travellerTestPars()$r
        y <- travellerTestPars()$y
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The impact of test-and-release for quarantined travellers, in terms of what fraction of ",
          "<strong>{normalisation}</strong> onward transmission per quarantined infected traveller is prevented by quarantine. ",
          "The thick grey lines represent the upper and lower bounds for the fraction of transmission that can be ",
          "prevented by quarantine without testing. ",
          "<i>(right)</i> The relative utility of different test-and-release quarantine durations compared to ",
          "standard quarantine with duration <strong>n = {nCompare}</strong> days. The thick grey curve is the ",
          "relative utility of standard quarantine without testing. In both panels we consider a travel duration ",
          "of <strong>y = {y}</strong> days and we assume infection can occur on any day -y &le; t<sub>E</sub> &le; 0 ",
          "with uniform probability. Individuals are tested on day x (colour) after returning on day 0 and released ",
          "on day x + &Delta; (x-axis) if negative (colour corresponds to &Delta;<sub>T</sub>). We assume that the ",
          "fraction of individuals in quarantine that are infected is <strong>s = {s}</strong>, and that there are no ",
          "false-positive test results. Dotted lines in both panels assume the released travellers have a reduced ",
          "transmission (<strong>r = {r}</strong>) due to extra hygiene and social distancing measures imposed by ",
          "reduced quarantine.",
          "</span>"
        ))
      })

      # SC2: adherence and symptoms ----
      travellerFracAdherence <- reactive({
        y.vals <- travellerFracPars()$y.vals
        nCompare <- travellerFracPars()$nCompare
        n.vals <- travellerFracPars()$n.vals
        normalisation <- travellerFracPars()$normalisation

        if (normalisation) {
          relAdherence <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
            relAdherence <- sapply(n.vals, function(n) {
              mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams()) /
                getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams())) /
                mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                  getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams()))
            })
            data.frame(
              y = factor(y, levels = y.vals),
              n = n.vals,
              relAdherence = relAdherence
            )
          }) %>% bind_rows()
        } else {
          relAdherence <- lapply(y.vals, function(y) {
            tE.vals <- seq(-y, 0)
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
        }
        tE.vals <- travellerTestPars()$tE.vals
        tS.vals <- tE.vals + incParams()$mean
        a.vals <- fracAdherencePars()$a.vals

        if (normalisation) {
          fracAdherence <- lapply(a.vals, function(a) {
            frac <- sapply(n.vals, function(n) {
              mean((a * getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - a) * getIntegral(upper = pmax(pmin(n, tS.vals), 0), lower = 0, tE = tE.vals, params = genParams())) /
                getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams()))
            })
            data.frame(
              a = factor(a, levels = a.vals),
              n = n.vals,
              fraction = frac
            )
          }) %>% bind_rows()
        } else {
          fracAdherence <- lapply(a.vals, function(a) {
            frac <- sapply(n.vals, function(n) {
              mean(a * getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) +
                (1 - a) * getIntegral(upper = pmax(pmin(n, tS.vals), 0), lower = 0, tE = tE.vals, params = genParams()))
            })
            data.frame(
              a = factor(a, levels = a.vals),
              n = n.vals,
              fraction = frac
            )
          }) %>% bind_rows()
        }

        return(list(
          frac = fracAdherence,
          relAdherence = relAdherence
        ))
      })
      # PLOTS ----
      output$travellerFracAdherencePlot <- renderPlot({
        labs <- paste(levels(travellerFracAdherence()$relAdherence$y), ifelse(levels(travellerFracAdherence()$relAdherence$y) == 1, "day", "days"))
        names(labs) <- levels(travellerFracAdherence()$relAdherence$y)

        ggplot(travellerFracAdherence()$relAdherence, aes(x = n, y = relAdherence, colour = y)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = travellerFracPars()$nCompare, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2])) +
          # scale_y_continuous(limits = c(0,1), labels = scales::percent) +
          coord_cartesian(ylim = c(0, 4.5)) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "duration\nof travel",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(x = "quarantine duration (days)", y = "relative adherence required\nto maintain quarantine efficacy") +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$travellerAsymptomaticPlot <- renderPlot({
        labs <- scales::percent(fracAdherencePars()$a.vals)
        names(labs) <- fracAdherencePars()$a.vals

        ggplot(travellerFracAdherence()$frac, aes(x = n, y = fraction, colour = a)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "viridis", direction = -1, end = 0.9, name = "fraction\nasymptomatic",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$travellerAdherenceCaption <- renderUI({
        nCompare <- travellerFracPars()$nCompare
        y <- travellerTestPars()$y
        meanInc <- round(incParams()$mean, 0)
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The change in adherence needed to maintain quarantine efficacy of the ",
          "<strong>n = {nCompare}</strong> day strategy if we change the quarantine duration to n' days (x-axis). ",
          "<i>(right)</i> The impact of symptomatic cases on the fraction of <strong>{normalisation}</strong> onward ",
          "transmission per quarantined ",
          "traveller that is prevented by quarantine. We fix the travel duration to <strong>y = {y}</strong> days ",
          "and assume t<sub>E</sub> is uniformly distributed between -y and 0. We use the mean incubation time of ",
          "<strong>{meanInc}</strong> days, such that <strong>t<sub>S</sub> = t<sub>E</sub> + {meanInc}</strong>.",
          "</span>"
        ))
      })
    }
  )
}

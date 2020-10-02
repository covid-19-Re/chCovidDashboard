library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)
library(cowplot)

getIntegral <- function(upper, lower, tE, params) {
  getGenDist(times = upper - tE, params = params, CDF = T) -
    getGenDist(times = lower - tE, params = params, CDF = T)
}

getUtility <- function(s = 1, efficacy, time) {
  s * efficacy / time
}

dayLabels <- function(x) {
  labs <- paste(x, ifelse(x == 1, "day", "days"))
  names(labs) <- x
  return(labs)
}

quarantineDurationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

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
      sc1_pars <- reactive({
        sc1_pars <- list(
          tE = 0,
          n = input$nCompare,
          DeltaQ.vals = seq(input$quarantineDelay[1], input$quarantineDelay[2]),
          n.vals = seq(input$quarantineDuration[1], input$quarantineDuration[2])
        )
      })

      sc1_data <- reactive({
        tE <- sc1_pars()$tE
        nCompare <- sc1_pars()$n
        DeltaQ.vals <- sc1_pars()$DeltaQ.vals
        n.vals <- sc1_pars()$n.vals

        frac <- lapply(DeltaQ.vals, function(DeltaQ) {
          n <- n.vals[n.vals > DeltaQ]
          data.frame(
            DeltaQ = factor(DeltaQ, levels = DeltaQ),
            n = n,
            fraction = getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams())
          )
        }) %>% bind_rows()

        rel_utility <- lapply(DeltaQ.vals, function(DeltaQ) {
          nPrime <- n.vals[n.vals > DeltaQ]
          utility.nPrime <- getUtility(efficacy = getIntegral(upper = tE + nPrime, lower = DeltaQ, tE = tE, params = genParams()),
                                       time = tE + nPrime - DeltaQ)
          utility.nCompare <- getUtility(efficacy = getIntegral(upper = tE + nCompare, lower = DeltaQ, tE = tE, params = genParams()),
                     time = tE + nCompare - DeltaQ)
          data.frame(
            DeltaQ = factor(DeltaQ, levels = DeltaQ),
            n = nPrime,
            relUtility = utility.nPrime / utility.nCompare
          )
        }) %>% bind_rows()

        return(list(frac = frac, rel_utility = rel_utility))
      })

      # PLOTS ----
      sc1 <- reactive({
        ggplot(sc1_data()$frac, aes(x = n, y = fraction, colour = DeltaQ)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = expression("delay to quarantine" ~ (Delta[Q])),
            labels = dayLabels(levels(sc1_data()$frac$DeltaQ)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme
      })

      output$sc1 <- renderPlot({
        sc1() + theme(legend.position = "none")
      })

      output$sc1_utility <- renderPlot({
        ggplot(sc1_data()$rel_utility, aes(x = n, y = relUtility, colour = DeltaQ)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = sc1_pars()$n, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          coord_cartesian(
            xlim = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$sc1_legend <- renderPlot({
        grid::grid.draw(get_legend(sc1()))
      })

      output$sc1_caption <- renderUI({
        n <- sc1_pars()$n
        tE <- sc1_pars()$tE

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> Fraction of total onward transmission per quarantined infected contact ",
          "that is prevented by quarantine. ",
          "<i>(right)</i> Relative utility of different quarantine strategies compared to ",
          "<strong>n = {n}</strong> days. ",
          "Colours represent the delay to starting quarantine, &Delta;<sub>Q</sub>. ",
          "We use t<sub>E</sub> = 0, which from the infectivity profile is the mean infection time of contacts ",
          "if the index case develops symptoms at t = 0",
          "</span>"
        ))
      })

      # SC1: TESTING & RELEASING ----
      sc1_test_pars <- reactive({
        sc1_test_pars <- list(
          DeltaQ = as.integer(input$deltaQfocal),
          DeltaT.vals = seq(input$testResultDelay[1], input$testResultDelay[2]),
          s = input$testSpecificity,
          r = input$transmissionReduction,
          x.vals = seq(as.integer(input$deltaQfocal), input$nCompare)
        )
        return(sc1_test_pars)
      })

      observe({
        isolate(deltaQselected <- input$deltaQfocal)
        updateSelectizeInput(session, "deltaQfocal",
          choices = as.character(sc1_pars()$DeltaQ.vals), selected = deltaQselected
        )
      })

      # earliest test date is first day of quarantine
      observe({
        sliderValue <- input$testDay
        updateSliderInput(session, "testDay",
          min = 0, max = 10,
          value = c(max(sliderValue[1], as.integer(isolate(input$deltaQfocal))), sliderValue[2])
        )
      })

      sc1_test_data <- reactive({
        DeltaQ <- sc1_test_pars()$DeltaQ
        DeltaT.vals <- sc1_test_pars()$DeltaT.vals
        tE <- sc1_pars()$tE
        s <- sc1_test_pars()$s
        r <- sc1_test_pars()$r
        x.vals <- sc1_test_pars()$x.vals
        n <- sc1_pars()$n

        #' Fraction based on test and release
        frac <- lapply(x.vals, function(x) {
          fraction <- getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams())
          data.frame(
            DeltaT = factor(DeltaT.vals),
            x = x,
            release = x + DeltaT.vals,
            fraction = fraction
          )
        }) %>% bind_rows()

        frac_no_test <- data.frame(
          DeltaT = NA,
          x = NA,
          release = x.vals,
          fraction = getIntegral(upper = tE + x.vals, lower = DeltaQ, tE = tE, params = genParams())
        )

        #' Maximum preventable fraction of transmission
        max_preventable <- getIntegral(upper = Inf, lower = DeltaQ, tE = tE, params = genParams())

        #' Fraction with reduced post-quarantine transmission
        frac_reduced <- lapply(x.vals, function(x) {
          fraction <- getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
            (1 - falseNeg(x) + r * falseNeg(x)) *
            getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams())
          data.frame(
            DeltaT = factor(DeltaT.vals),
            x = x,
            release = x + DeltaT.vals,
            fraction = fraction
          )
        }) %>% bind_rows()

        utility_no_test <- getUtility(s = s,
                                      efficacy = getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams()),
                                      time = tE + n - DeltaQ)

        rel_utility_no_test <- lapply(x.vals, function(x) {
          utility <- getUtility(s = s,
                                efficacy = getIntegral(upper = tE + x, lower = DeltaQ, tE = tE, params = genParams()),
                                time = tE + x - DeltaQ)
          data.frame(
            DeltaT = NA,
            x = NA,
            release = x,
            relUtility = utility / utility_no_test
          )
        }) %>% bind_rows()

        #' Relative utility of test-and-release strategy compared to standard
        rel_utility <- lapply(x.vals, function(x) {
          utility <- getUtility(s = s,
                                efficacy = getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
                                  (1 - falseNeg(x)) *
                                  getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams()),
                                time = tE + x + DeltaT.vals - DeltaQ + s * (1 - falseNeg(x)) * (n - x - DeltaT.vals))
          data.frame(
            DeltaT = factor(DeltaT.vals),
            x = x,
            release = x + DeltaT.vals,
            relUtility = utility / utility_no_test
          )
        }) %>% bind_rows()

        #' Relative utility of test and release with reduced post-quarantine transmission
        rel_utility_reduced <- lapply(x.vals, function(x) {
          utility <- getUtility(s = s,
                                efficacy = getIntegral(upper = tE + x + DeltaT.vals, lower = DeltaQ, tE = tE, params = genParams()) +
                                  (1 - falseNeg(x) + r * falseNeg(x)) *
                                  getIntegral(upper = tE + n, lower = tE + x + DeltaT.vals, tE = tE, params = genParams()),
                                time = tE + x + DeltaT.vals - DeltaQ + s * (1 - falseNeg(x)) * (n - x - DeltaT.vals))
          data.frame(
            DeltaT = factor(DeltaT.vals),
            x = x,
            release = x + DeltaT.vals,
            relUtility = utility / utility_no_test
          )
        }) %>% bind_rows()

        return(list(
          frac = frac,
          frac_no_test = frac_no_test,
          max_preventable = max_preventable,
          frac_reduced = frac_reduced,
          rel_utility_no_test = rel_utility_no_test,
          rel_utility = rel_utility,
          rel_utility_reduced = rel_utility_reduced
        ))
      })

      # PLOTS ----
      sc1_test_colours <- reactive({
        scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
                               name = expression("test result delay " ~ (Delta[T])),
                               labels = dayLabels(sc1_test_pars()$DeltaT.vals),
                               guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1))
      })

      sc1_test <- reactive({
        ggplot(sc1_test_data()$frac, aes(x = release, y = fraction, colour = DeltaT)) +
          geom_hline(yintercept = sc1_test_data()$max_preventable, colour = "darkgrey", size = 1.2) +
          geom_line(data = sc1_test_data()$frac_no_test, color = "darkgrey", size = 1.2) +
          geom_line(data = sc1_test_data()$frac_reduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, sc1_pars()$n), breaks = seq(0, sc1_pars()$n, 2)) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          sc1_test_colours() +
          labs(x = "day of release", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme
      })

      output$sc1_test <- renderPlot({
        sc1_test() + theme(legend.position = "None")
      })

      output$sc1_test_legend <- renderPlot({
        grid::grid.draw(get_legend(sc1_test()))
      })

      output$sc1_test_utility <- renderPlot({
        ggplot(sc1_test_data()$rel_utility, aes(x = release, y = relUtility, colour = DeltaT)) +
          geom_vline(xintercept = sc1_pars()$n, color = "darkgrey", size = 1.2) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_line(data = sc1_test_data()$rel_utility_no_test, colour = "darkgrey", size = 1.1) +
          geom_line(data = sc1_test_data()$rel_utility_reduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, sc1_pars()$n), breaks = seq(0, sc1_pars()$n, 2)) +
          coord_cartesian(ylim = c(0, 4)) +
          sc1_test_colours() +
          labs(x = "day of release", y = "\nrelative utility of quarantine") +
          plotTheme + theme(legend.position = "None")
      })

      output$sc1_test_caption <- renderUI({
        n <- sc1_pars()$n
        tE <- sc1_pars()$tE
        DeltaQ <- sc1_test_pars()$DeltaQ
        s <- sc1_test_pars()$s
        r <- sc1_test_pars()$r

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The impact of the test-and-release quarantine strategy, in terms of what fraction of total ",
          "onward transmission per infected traced contact is prevented by quarantine. The thick grey lines ",
          "represent the upper and lower bounds for the fraction of transmission that can be prevented by quarantine ",
          "without testing. ",
          "<i>(right)</i> The relative utility of different test-and-release quarantine durations compared to ",
          "standard quarantine with duration <strong>n = {n}</strong> days. The thick grey curve is the ",
          "relative utility of standard quarantine without testing. In both panels we use t<sub>E</sub> = {tE}, ",
          "which from the infectivity profile is the mean infection time of contacts if the index case develops ",
          "symptoms at t = 0, and <strong>&Delta;<sub>Q</sub> = {DeltaQ}</strong> as the delay until quarantine ",
          "begins. Individuals are tested on day x after exposure and released on day x + &Delta;<sub>T</sub> (x-axis) ",
          "if negative (colour corresponds to &Delta;<sub>T</sub>). Positive testing individuals are released at ",
          "day <strong>n = {n}</strong>. We assume that the fraction of individuals in quarantine that are ",
          "infected is <strong>s = {s}</strong>, and that there are no false-positive test results. Dashed lines in ",
          "both panels assume the released individuals have a reduced transmission (<strong>r = {r}</strong>) due ",
          "to reinforced hygiene and social distancing measures.",
          "</span>"
        ))
      })

      # SC1: ADHERENCE AND SYMPTOMS ----
      sc1_adherence_pars <- reactive({
        sc1_adherence_pars <- list(
          tS = sc1_pars()$tE + incParams()$mean,
          a.vals = seq(0, 1, 0.25)
        )
        return(sc1_adherence_pars)
      })

      sc1_adherence_data <- reactive({
        DeltaQ <- sc1_test_pars()$DeltaQ
        DeltaQ.vals <- sc1_pars()$DeltaQ.vals
        tE <- sc1_pars()$tE
        tS <- sc1_adherence_pars()$tS
        n.vals <- sc1_pars()$n.vals
        n <- sc1_pars()$n
        a.vals <- sc1_adherence_pars()$a.vals


        frac <- lapply(a.vals, function(a) {
          n <- n.vals[n.vals >= DeltaQ]
          frac <- a * (getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams())) +
            (1 - a) * (getIntegral(upper = pmin(tE + n, tS), lower = DeltaQ, tE = tE, params = genParams()))
          data.frame(
            a = factor(a, levels = a.vals),
            n = n,
            fraction = frac
          )
        }) %>% bind_rows()

        rel_adherence <- lapply(DeltaQ.vals, function(DeltaQ) {
          nPrime <- n.vals[n.vals > DeltaQ]
          relAdherence <- getIntegral(upper = tE + n, lower = DeltaQ, tE = tE, params = genParams()) /
            getIntegral(upper = tE + nPrime, lower = DeltaQ, tE = tE, params = genParams())
          data.frame(
            DeltaQ = factor(DeltaQ, levels = DeltaQ.vals),
            n = nPrime,
            relAdherence = relAdherence
          )
        }) %>% bind_rows()

        return(list(
          frac = frac,
          rel_adherence = rel_adherence
        ))
      })

      # PLOTS ----
      output$sc1_adherence <- renderPlot({
        ggplot(sc1_adherence_data()$rel_adherence, aes(x = n, y = relAdherence, colour = DeltaQ)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = sc1_pars()$n, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDuration[1], input$quarantineDuration[2])) +
          coord_cartesian(ylim = c(0, 4)) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "delay to\nquarantine",
            labels = dayLabels(sc1_adherence_data()$rel_adherence$DeltaQ),
            guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(
            x = "quarantine duration (days)",
            y = "relative adherence required to\nmaintain quarantine efficacy"
          ) +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$sc1_asymptomatic <- renderPlot({
        labs <- scales::percent(sc1_adherence_pars()$a.vals)
        names(labs) <- sc1_adherence_pars()$a.vals

        ggplot(sc1_adherence_data()$frac, aes(x = n, y = fraction, colour = a)) +
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

      output$sc1_adherence_caption <- renderUI({
        n <- sc1_pars()$n
        tE <- sc1_pars()$tE
        DeltaQ <- sc1_test_pars()$DeltaQ
        tS <- sc1_adherence_pars()$tS

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The change in adherence needed to maintain quarantine efficacy of the ",
          "<strong>n = {n}</strong> day strategy if we change the quarantine duration to n' days (x-axis). ",
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
      sc2_pars <- reactive({
        sc2_pars <- list(
          n = input$nCompareSC2,
          y.vals = na.omit(as.integer(input$travelDuration)),
          n.vals = seq(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2]),
          normalisation = input$normalisation # TRUE: local, FALSE: total
        )
      })

      # sort selected travel durations
      observe({
        y.valsSelected <- input$travelDuration
        updateSelectizeInput(session, "travelDuration", selected = sort(as.integer(y.valsSelected)))
      })

      sc2_data <- reactive({
        nCompare <- sc2_pars()$n
        y.vals <- sc2_pars()$y.vals
        n.vals <- sc2_pars()$n.vals
        normalisation <- sc2_pars()$normalisation

        norm_fn <- function(tE.vals, normalisation) {
          if (normalisation) {
            return(getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams()))
          } else {
            return(1)
          }
        }

        frac <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y, 0)
          frac <- lapply(n.vals, function(n) {
            frac <- mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                           norm_fn(tE.vals, normalisation))
            data.frame(
              y = factor(y, levels = y.vals),
              n = n,
              fraction = frac
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        rel_utility <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y, 0)
          utility.nCompare <- getUtility(efficacy = mean(getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams()) /
                                                           norm_fn(tE.vals, normalisation)),
                                         time = nCompare)
          relUtility <- lapply(n.vals[n.vals > 0], function(n) {
            utility <- getUtility(efficacy = mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                                                    norm_fn(tE.vals, normalisation)),
                                  time = n)
            data.frame(
              y = factor(y, levels = y.vals),
              n = n,
              relUtility = utility / utility.nCompare
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        return(list(frac = frac, rel_utility = rel_utility))
      })

      # PLOTS ----
      sc2 <- reactive({
        ggplot(sc2_data()$frac, aes(x = n, y = fraction, colour = y)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "duration of travel",
            labels = dayLabels(levels(sc2_data()$frac$y)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(x = "quarantine duration (days)", y = "fraction of transmission\nprevented by quarantine") +
          plotTheme
      })

      output$sc2 <- renderPlot({
        sc2() + theme(legend.position = "none")
      })

      output$sc2_utility <- renderPlot({
        ggplot(sc2_data()$rel_utility, aes(x = n, y = relUtility, colour = y)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = sc2_pars()$n, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          coord_cartesian(
            xlim = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2]),
            ylim = c(0, 2.5)) +
          scale_colour_viridis_d(option = "inferno", end = 0.9) +
          labs(x = "quarantine duration (days)", y = "relative utility of quarantine") +
          plotTheme +
          theme(legend.position = "none")
      })

      output$sc2_legend <- renderPlot({
        grid::grid.draw(get_legend(sc2()))
      })

      output$sc2_caption <- renderUI({
        n <- sc2_pars()$n
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of <strong>{normalisation}</strong> onward transmission per quarantined ",
          "traveller that is prevented by quarantine. ",
          "<i>(right)</i> The relative utility of different quarantine strategies (x-axis) ",
          "compared to <strong>n = {n}</strong> days. ",
          "Colours represent the duration of travel y and we assume infection can occur on any day ",
          "-y &le; t<sub>E</sub> &le; 0 with uniform probability.",
          "</span>"
        ))
      })

      # SC2: TESTING & RELEASING ----
      sc2_test_pars <- reactive({
        sc2_test_pars <- list(
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

      sc2_test_data <- reactive({
        y <- sc2_test_pars()$y
        tE.vals <- sc2_test_pars()$tE.vals
        s <- sc2_test_pars()$s
        r <- sc2_test_pars()$r
        n <- sc2_pars()$n
        x.vals <- sc2_test_pars()$x.vals
        DeltaT.vals <- sc2_test_pars()$DeltaT.vals
        normalisation <- sc2_pars()$normalisation

        #' Maximum preventable fractions
        max_preventable <- getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams())
        if (normalisation) {
          max_preventable_plot <- 1
        } else {
          max_preventable_plot <- mean(max_preventable)
          max_preventable <- 1
        }

        #' Fraction of infection prevented using normal strategy
        frac_no_test <- data.frame(
          DeltaT = NA,
          x = NA,
          release = x.vals,
          fraction  = sapply(x.vals,
            function(n) mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
              max_preventable))
        )

        #' Fraction prevented by test-and-release strategy
        frac <- lapply(x.vals, function(x) {
          frac <- lapply(DeltaT.vals, function(DeltaT) {
            fraction <- mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
              (1 - falseNeg(x - tE.vals)) *
              getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / max_preventable)
            data.frame(
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              x = x,
              release = x + DeltaT,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        #' Fraction prevented by test and release strategy with reduced post-quarantine transmission
        frac_reduced <- lapply(x.vals, function(x) {
          frac_reduced <- lapply(DeltaT.vals, function(DeltaT) {
            fraction <- mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
            (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
            getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) / max_preventable)
            data.frame(
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              x = x,
              release = x + DeltaT,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        #' Utility without testing
        utility_no_test <- getUtility(s = s,
                                      efficacy = mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                                                        max_preventable),
                                      time = n)

        rel_utility_no_test <- lapply(x.vals, function(n) {
          utility <- getUtility(s = s,
                                efficacy = mean(getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) / max_preventable),
                                time = n)
          data.frame(
            DeltaT = NA,
            x = NA,
            release = n,
            relUtility = utility / utility_no_test
          )
        }) %>% bind_rows()

        rel_utility <- lapply(x.vals, function(x) {
          rel_utility <- lapply(DeltaT.vals, function(DeltaT) {
            utility <- getUtility(s = s,
                                  efficacy = mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                                                     (1 - falseNeg(x - tE.vals)) *
                                                     getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) /
                                                    max_preventable),
                                  time = mean(x + DeltaT + s * (1 - falseNeg(x - tE.vals)) * (n - x - DeltaT)))
            data.frame(
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              x = x,
              release = x + DeltaT,
              relUtility = utility / utility_no_test
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        #' Relative utility with reduced post-quarantine transmission
        rel_utility_reduced <- lapply(x.vals, function(x) {
          relUtility <- lapply(DeltaT.vals, function(DeltaT) {
            utility <- getUtility(s = s,
                                  efficacy = mean((getIntegral(upper = x + DeltaT, lower = 0, tE = tE.vals, params = genParams()) +
                                                     (1 - falseNeg(x - tE.vals) + r * falseNeg(x - tE.vals)) *
                                                     getIntegral(upper = n, lower = x + DeltaT, tE = tE.vals, params = genParams())) /
                                                    max_preventable),
                                  time = mean(x + DeltaT + s * (1 - falseNeg(x - tE.vals)) * (n - x - DeltaT)))
            data.frame(
              x = x,
              DeltaT = factor(DeltaT, levels = DeltaT.vals),
              release = x + DeltaT,
              relUtility = utility / utility_no_test
            )
          }) %>% bind_rows()
        }) %>% bind_rows()


        return(list(
          frac = frac,
          frac_no_test = frac_no_test,
          frac_reduced = frac_reduced,
          max_preventable_plot = max_preventable_plot,
          rel_utility = rel_utility,
          rel_utility_no_test = rel_utility_no_test,
          rel_utility_reduced = rel_utility_reduced
        ))
      })

      # PLOTS ----
      sc2_test_colours <- reactive({
        scale_colour_viridis_d(option = "viridis", direction = -1, end = 0.9,
                               name = expression("test result delay " ~ (Delta[T])),
                               labels = dayLabels(sc2_test_pars()$DeltaT.vals),
                               guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1))
      })

      sc2_test <- reactive({
        fracPlot <- ggplot(sc2_test_data()$frac, aes(x = release, y = fraction, colour = DeltaT)) +
          geom_hline(yintercept = sc2_test_data()$max_preventable_plot, colour = "darkgrey", size = 1.2) +
          geom_line(data = sc2_test_data()$frac_no_test, colour = "darkgrey", size = 1.2) +
          geom_line(data = sc2_test_data()$frac_reduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, sc2_pars()$n), breaks = seq(0, sc2_pars()$n, 2)) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          sc2_test_colours() +
          labs(x = "day of release", y = "fraction of local transmission\nprevented by quarantine") +
          plotTheme
      })

      output$sc2_test <- renderPlot({
        sc2_test() +
          theme(legend.position = "none")
      })

      output$sc2_test_utility <- renderPlot({
        ggplot(sc2_test_data()$rel_utility, aes(x = release, y = relUtility, colour = DeltaT)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = sc2_pars()$n, color = "darkgrey", size = 1.2) +
          geom_line(data = sc2_test_data()$rel_utility_no_test, colour = "darkgrey", size = 1.2) +
          geom_line(data = sc2_test_data()$rel_utility_reduced, linetype = "dashed") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(0, sc2_pars()$n), breaks = seq(0, sc2_pars()$n, 2)) +
          coord_cartesian(ylim = c(0, 4)) +
          sc2_test_colours() +
          labs(x = "day of release", y = "\nrelative utility of quarantine") +
          plotTheme + theme(legend.position = "None")
      })

      output$sc2_test_legend <- renderPlot({
        grid::grid.draw(get_legend(sc2_test()))
      })

      output$sc2_test_caption <- renderUI({
        n <- sc2_pars()$n
        s <- sc2_test_pars()$s
        r <- sc2_test_pars()$r
        y <- sc2_test_pars()$y
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The impact of test-and-release for quarantined travellers, in terms of what fraction of ",
          "<strong>{normalisation}</strong> onward transmission per quarantined infected traveller is prevented by ",
          "quarantine. The thick grey lines represent the upper and lower bounds for the fraction of transmission ",
          "that can be prevented by quarantine without testing. ",
          "<i>(right)</i> The relative utility of different test-and-release quarantine durations compared to ",
          "standard quarantine with duration <strong>n = {n}</strong> days. The thick grey curve is the ",
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
      sc2_adherence_data <- reactive({
        y.vals <- sc2_pars()$y.vals
        nCompare <- sc2_pars()$n
        n.vals <- sc2_pars()$n.vals
        tE.vals <- sc2_test_pars()$tE.vals
        tS.vals <- tE.vals + incParams()$mean
        a.vals <- sc1_adherence_pars()$a.vals
        normalisation <- sc2_pars()$normalisation

        norm_fn <- function(tE.vals, normalisation) {
          if (normalisation) {
            return(getIntegral(upper = Inf, lower = 0, tE = tE.vals, params = genParams()))
          } else {
            return(1)
          }
        }

        rel_adherence <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y, 0)
          relAdherence <- sapply(n.vals, function(n) {
            mean(
              getIntegral(upper = nCompare, lower = 0, tE = tE.vals, params = genParams()) /
                norm_fn(tE.vals, normalisation)
            ) /
            mean(
              getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams()) /
                norm_fn(tE.vals, normalisation)
            )
          })
          data.frame(
            y = factor(y, levels = y.vals),
            n = n.vals,
            relAdherence = relAdherence
          )
        }) %>% bind_rows()


        frac <- lapply(a.vals, function(a) {
          frac <- sapply(n.vals, function(n) {
            f_asym_infection <- getIntegral(upper = n, lower = 0, tE = tE.vals, params = genParams())
            f_sym_infections <- getIntegral(upper = pmax(pmin(n, tS.vals), 0),
              lower = 0, tE = tE.vals, params = genParams())

            mean(
              (a * f_asym_infection + (1 - a) * f_sym_infections) /
                norm_fn(tE.vals, normalisation)
            )
          })
          data.frame(
            a = factor(a, levels = a.vals),
            n = n.vals,
            fraction = frac
          )
        }) %>% bind_rows()

        return(list(
          frac = frac,
          rel_adherence = rel_adherence
        ))
      })

      # PLOTS ----
      output$sc2_adherence <- renderPlot({
        ggplot(
          data = filter(sc2_adherence_data()$rel_adherence, n > 0),
          mapping = aes(x = n, y = relAdherence, colour = y)) +
          geom_hline(yintercept = 1, color = "darkgrey", size = 1.2) +
          geom_vline(xintercept = sc2_pars()$n, color = "darkgrey", size = 1.2) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          scale_x_continuous(limits = c(input$quarantineDurationSC2[1], input$quarantineDurationSC2[2])) +
          coord_cartesian(ylim = c(0, 4.4)) +
          scale_colour_viridis_d(
            option = "inferno", end = 0.9, name = "duration\nof travel",
            labels = dayLabels(sc2_adherence_data()$rel_adherence$y),
            guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(x = "quarantine duration (days)", y = "relative adherence required\nto maintain quarantine efficacy") +
          plotTheme +
          theme(legend.position = "bottom")
      })

      output$sc2_asymptomatic <- renderPlot({
        labs <- scales::percent(sc1_adherence_pars()$a.vals)
        names(labs) <- sc1_adherence_pars()$a.vals

        ggplot(sc2_adherence_data()$frac, aes(x = n, y = fraction, colour = a)) +
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

      output$sc2_adherence_caption <- renderUI({
        n <- sc2_pars()$n
        y <- sc2_test_pars()$y
        meanInc <- round(incParams()$mean, 0)
        normalisation <- if_else(input$normalisation, "local", "global")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The change in adherence needed to maintain quarantine efficacy of the ",
          "<strong>n = {n}</strong> day strategy if we change the quarantine duration to n' days (x-axis). ",
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

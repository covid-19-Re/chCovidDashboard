library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)
library(cowplot)
library(RColorBrewer)

incDist <- readRDS("R/quarantineModuleFiles/incDist.rds")

quarantineDurationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      plot_line_size <- 1
      plot_point_size <- 1.5

      # DISTRIBUTIONS ----
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
        times <- seq(-10, 15, 0.1)
        infProf <- data.frame(
          t = times,
          pdf = getInfectivityProfile(times = times, params = infParams()),
          CDF = getInfectivityProfile(times = times, params = infParams(), CDF = TRUE)
        )
        return(infProf)
      })

      incParams <- incParamsDefault

      # DISTRIBUTION PLOTS ----
      yLim <- c(0, 0.22)
      xLim <- c(0, 20)
      labY <- 0.21
      output$incDistPlot <- renderPlot({
        ggplot(incDist, aes(x = t, y = pdf)) +
          geom_vline(xintercept = incParams$mean[1], linetype = "dashed", alpha = 0.5) +
          annotate("text",
                   label = paste0(" mean = ", format(round(incParams$mean[1], 1), nsmall = 1), " days"),
                   x = incParams$mean[1], y = labY,
                   hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(ylim = yLim, xlim = xLim) +
          labs(x = "incubation period (days)", y = "probability density") +
          ggtitle("C: incubation period") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      output$infProfPlot <- renderPlot({
        ggplot(infProf(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = infParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
                   label = paste0(" mean = ", format(round(infParams()$mean, 1), nsmall = 1), " days"),
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
                   label = paste0(" mean = ", format(round(genParams()$mean, 1), nsmall = 1), " days"),
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

      # SC1: TESTING & RELEASING ----
      sc1_test_pars <- reactive({
        sc1_test_pars <- list(
          tQ = input$sc1_test_DeltaQ,
          DeltaT.vals = seq(input$sc1_test_DeltaT[2],input$sc1_test_DeltaT[1]),
          tR.compare = input$sc1_test_tRCompare,
          s = input$sc1_test_sPercent/100,
          r = input$sc1_test_rPercent/100
        )
        return(sc1_test_pars)
      })

      sc1_test_data <- reactive({
        # Define input parameters locally
        tE <- 0
        tQ <- sc1_test_pars()$tQ
        tR.compare <- sc1_test_pars()$tR.compare
        tR.vals <- seq(tQ, max(tR.compare, 10))
        DeltaT.vals <- sc1_test_pars()$DeltaT.vals
        s <- sc1_test_pars()$s
        r <- sc1_test_pars()$r
        tEnd <- 10

        # Fraction prevented without testing
        frac_no_test <- data.frame(
          tE = tE,
          tQ = tQ,
          tT = NA,
          tR = tR.vals,
          DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
          release = tR.vals - tE,
          fraction = ifelse(tR.vals >= tQ,
                            getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams()),
                            NA)
        )

        # Fraction prevented based on test-and-release
        fraction.before <- getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams())
        fraction.after <- getIntegral(upper = tEnd, lower = tR.vals, tE = tE, params = genParams())
        frac_test <- lapply(DeltaT.vals, function(DeltaT) {
          tT.vals <- tR.vals - DeltaT

          fraction <- ifelse(tT.vals >= tQ,
                             fraction.before +
                               (1 - falseNeg(tT.vals - tE)) * fraction.after,
                             NA)

          data.frame(
            tE = tE,
            tQ = tQ,
            tT = tT.vals,
            tR = tR.vals,
            DeltaT = factor(DeltaT, levels = c("noTest", DeltaT.vals)),
            release = tR.vals - tE,
            fraction = fraction
          )
        }) %>% bind_rows()
        # Fraction prevented based on test-and-release and reduced transmission
        frac_test_reduced <- lapply(DeltaT.vals, function(DeltaT) {
          tT.vals <- tR.vals - DeltaT

          fraction <- ifelse(tT.vals >= tQ,
                             fraction.before +
                               (1 - falseNeg(tT.vals - tE) + r * falseNeg(tT.vals - tE)) * fraction.after,
                             NA)

          data.frame(
            tE = tE,
            tQ = tQ,
            tT = tT.vals,
            tR = tR.vals,
            DeltaT = factor(DeltaT, levels = c("noTest", DeltaT.vals)),
            release = tR.vals - tE,
            fraction = fraction
          )
        }) %>% bind_rows()

        # Combine test and no-test results
        frac_combined <- rbind(frac_no_test,frac_test)

        # Maximum preventable fraction of transmission
        max_preventable <- getIntegral(upper = Inf, lower = tQ, tE = tE, params = genParams())


        # Baseline Quarantine utility
        utility_compare <- getUtility(s = s,
                                      efficacy = getIntegral(upper = tR.compare, lower = tQ, tE = tE, params = genParams()),
                                      time = tR.compare - tQ)

        # Quarantine utility without testing
        utility_no_test <- getUtility(
          s = s,
          efficacy = ifelse(tR.vals >= tQ,
                            getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams()),
                            NA),
          time = tR.vals - tQ
        )
        rel_utility_no_test <- data.frame(
          s = s,
          tE = tE,
          tQ = tQ,
          tT = NA,
          tR = tR.vals,
          DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
          release = tR.vals - tE,
          relUtility = utility_no_test / utility_compare
        )
        # Quarantine utility under test-and-release
        frac.before <- getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams())
        frac.after <- getIntegral(upper = tEnd, lower = tR.vals, tE = tE, params = genParams())

        time.before <- tR.vals - tQ
        time.after <- tEnd - tR.vals

        rel_utility_test <- lapply(DeltaT.vals, function(DeltaT) {
          tT.vals <- tR.vals - DeltaT
          fraction <- ifelse(tT.vals >= tQ,
                             frac.before +
                               (1 - falseNeg(tT.vals - tE)) * frac.after,
                             NA)

          utility <- getUtility(s = s, efficacy = fraction,
                                time = time.before + s * (1 - falseNeg(tT.vals - tE)) * time.after)

          data.frame(
            s = s,
            tE = tE,
            tQ = tQ,
            tT = tT.vals,
            tR = tR.vals,
            DeltaT = factor(DeltaT, levels = c("noTest",DeltaT.vals)),
            release = tR.vals - tE,
            relUtility = utility / utility_compare
          )
        }) %>% bind_rows()
        # Combine test and no-test results
        rel_utility_combined <- rbind(rel_utility_no_test,rel_utility_test)

        # Quarantine utility of test-and-release with reduced transmission
        rel_utility_test_reduced <- lapply(DeltaT.vals, function(DeltaT) {
          tT.vals <- tR.vals - DeltaT
          fraction <- ifelse(tT.vals >= tQ,
                             frac.before +
                               (1 - falseNeg(tT.vals - tE) + r * falseNeg(tT.vals - tE)) * frac.after,
                             NA)

          utility <- getUtility(s = s, efficacy = fraction,
                                time = time.before + s * (1 - falseNeg(tT.vals - tE)) * time.after)

          data.frame(
            s = s,
            tE = tE,
            tQ = tQ,
            tT = tT.vals,
            tR = tR.vals,
            DeltaT = factor(DeltaT, levels = c("noTest",DeltaT.vals)),
            release = tR.vals - tE,
            relUtility = utility / utility_compare
          )
        }) %>% bind_rows()

        return(list(
          max_preventable = max_preventable,
          frac_combined = frac_combined,
          frac_test_reduced = frac_test_reduced,
          rel_utility_combined = rel_utility_combined,
          rel_utility_test_reduced = rel_utility_test_reduced
        ))
      })

      # PLOTS ----
      sc1_test_colours <- reactive({
        n <- length(sc1_test_pars()$DeltaT.vals)
        cols <- c("#000000",colorRampPalette(brewer.pal(9, "Oranges")[9:4])(n))
        names(cols) <- c("noTest", sc1_test_pars()$DeltaT.vals)
        scale_colour_manual(
          name = expression("delay between test and release" ~ (t[R]-t[T])),
          values = cols,
          labels = c(noTest = "no test", dayLabels(sc1_test_pars()$DeltaT.vals)),
          guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1)
        )
      })

      sc1_test_plot <- reactive({

        xLim <- c(sc1_test_pars()$tQ, max(sc1_test_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc1_test_data()$frac_combined,
          mapping = aes(
            x = release, y = fraction, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1)
            )
          )) +
          geom_hline(yintercept = sc1_test_data()$max_preventable, colour = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          sc1_test_colours() +
          labs(
            x = "&nbsp;\ndays until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "fraction of transmission\nprevented by quarantine\n&nbsp;") +
          plotTheme

        if (input$sc1_test_addMeasures) {
          plot <- plot +
            geom_line(
              data = sc1_test_data()$frac_test_reduced,
              inherit.aes = FALSE,
              mapping = aes(
                x = release, y = fraction, colour = DeltaT, group = DeltaT,
                text = str_c(
                  "reduced transmission<br>",
                  "t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT), "<br>",
                  "prevented = ", scales::percent(fraction, accuracy = 0.1)
                )
              ),
              linetype = "dashed")
        }
        return(plot)
      })

      output$sc1_test_plot <- renderPlotly(
        makePlotly(sc1_test_plot())
      )

      output$sc1_test_legend <- renderPlot({
        grid::grid.draw(get_legend(sc1_test_plot()))
      })

      output$sc1_test_utility_plot <- renderPlotly({

        xLim <- c(sc1_test_pars()$tQ, max(sc1_test_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc1_test_data()$rel_utility_combined,
          mapping = aes(
            x = release, y = relUtility, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "rel.utility = ", format(round(relUtility, 2), nsmall = 2)
            )
          )) +
          geom_vline(xintercept = sc1_test_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          coord_cartesian(ylim = c(0, 4)) +
          sc1_test_colours() +
          labs(
            x = "&nbsp;\ndays until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "\nrelative utility of quarantine\n&nbsp;") +
          plotTheme

        if (input$sc1_test_addMeasures) {
          plot <- plot +
            geom_line(
              data = sc1_test_data()$rel_utility_test_reduced,
              inherit.aes = FALSE,
              mapping = aes(
                x = release, y = relUtility, colour = DeltaT, group = DeltaT,
                text = str_c(
                  "reduced transmission<br>",
                  "t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT), "<br>",
                  "rel. utility = ", format(round(relUtility, 2), nsmall = 2)
                )
              ),
              linetype = "dashed")
        }

        makePlotly(plot)
      })

      output$sc1_test_caption <- renderUI({
        tR.compare <- sc1_test_pars()$tR.compare
        DeltaQ <- sc1_test_pars()$tQ
        s <- input$sc1_test_sPercent
        r <- input$sc1_test_rPercent
        DeltaT.vals <- input$sc1_test_DeltaT

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of transmission that is prevented by quarantining an infected contact.",
          "Quarantine begins at time <strong>t<sub>Q</sub>={DeltaQ}</strong> after exposure at time t<sub>E</sub>=0,",
          "i.e. there is a <strong>{DeltaQ}</strong> day delay between exposure and the start of quarantine.",
          "Under the standard quarantine protocol (black), individuals are released without being tested.",
          "The test-and-release protocol (colours) requires a negative test result before early release,",
          "otherwise individuals remain isolated until they are no longer infectious (day 10).",
          "Colours represents the delay between test and release",
          "(from <strong>{DeltaT.vals[1]}</strong> to <strong>{DeltaT.vals[2]}</strong> days).",
          "The upper grey line represents the maximum attainable prevention by increasing the time of release,",
          "while keeping <strong>t<sub>Q</sub>={DeltaQ}</strong> fixed.",
          "<br>",
          "<i>(right)</i> The relative utility of the quarantine scenarios in panel A compared to",
          "the standard protocol <strong>{tR.compare}</strong> day quarantine.",
          "The lower grey line represents equal utilities (relative utility of 1).",
          "We assume that the fraction of individuals in quarantine that are infected is <strong>{s}%</strong>,",
          "and that there are no false-positive test results.",
          "<br>",
          "[Manuscript Figure 2 and Figure 2-figure supplement 2]",
          "</span>",
          .sep = " "
        ))
      })


      # SC1: QUARTANTINE DURATION AND DELAY (NO TEST) ----
      sc1_noTest_pars <- reactive({
        sc1_noTest_pars <- list(
          tQ.vals = seq(input$sc1_noTest_tQ[1], input$sc1_noTest_tQ[2]),
          tR.vals = seq(input$sc1_noTest_tR.vals[1], input$sc1_noTest_tR.vals[2]),
          tR.compare = input$sc1_noTest_tR.compare
        )
        return(sc1_noTest_pars)
      })

      sc1_noTest_data <- reactive({
        # Define input parameters locally
        tE <- 0
        tQ.vals <- sc1_noTest_pars()$tQ.vals
        tR.vals <- sc1_noTest_pars()$tR.vals
        tR.compare <- sc1_noTest_pars()$tR.compare

        # Fraction fo transmission prevented
        frac <- lapply(tQ.vals, function(tQ) {
          fraction <- ifelse(tR.vals >= tQ,
                             getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams()),
                             NA)
          data.frame(
            tE = tE,
            tQ = tQ,
            tR = tR.vals,
            delay = factor(tQ - tE, levels = tQ.vals - tE),
            release = tR.vals - tE,
            fraction = fraction
          )
        }) %>% bind_rows()

        rel_utility <- lapply(tQ.vals, function(tQ) {
          fraction.compare <- getIntegral(upper = tR.compare, lower = tQ, tE = tE, params = genParams())
          utility.compare <- getUtility(efficacy = fraction.compare, time = tR.compare - tQ)

          fraction <- getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams())
          utility <- ifelse(tR.vals > tQ,
                            getUtility(efficacy = fraction, time = tR.vals - tQ),
                            NA)
          data.frame(
            tE = tE,
            tQ = tQ,
            tR = tR.vals,
            delay = factor(tQ - tE, levels = tQ.vals - tE),
            release = tR.vals - tE,
            relUtility = utility / utility.compare
          )
        }) %>% bind_rows()

        return(list(frac = frac, rel_utility = rel_utility))
      })

      # PLOTS ----
      sc1_noTest_plot <- reactive({

        xLim <- c(min(sc1_noTest_pars()$tR.vals), max(sc1_noTest_pars()$tR.vals))

        ggplot(
          data = sc1_noTest_data()$frac,
          mapping = aes(
            x = release, y = fraction, colour = delay, group = delay,
            text = str_c(
              "t<sub>Q</sub> - t<sub>E</sub> = ", dayLabels(delay), "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1, trim = F)
            )
          )) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "inferno", begin = 0.1, end = 0.9,
            name = expression("delay between exposure and the start of quarantine" ~ (t[Q] - t[E])),
            labels = dayLabels(levels(sc1_noTest_data()$frac$delay)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(
            x = "&nbsp;\ndays until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "fraction of transmission\nprevented by quarantine\n&nbsp;"
          ) +
          plotTheme
      })

      output$sc1_noTest_plot <- renderPlotly({
        makePlotly(sc1_noTest_plot())
      })

      output$sc1_noTest_utility_plot <- renderPlotly({

        xLim <- c(min(sc1_noTest_pars()$tR.vals), max(sc1_noTest_pars()$tR.vals))

        plot <- ggplot(
          data = sc1_noTest_data()$rel_utility,
          mapping = aes(
            x = release, y = relUtility, colour = delay, group = delay,
            text = str_c(
              "t<sub>Q</sub> - t<sub>E</sub> = ", dayLabels(delay), "<br>",
              "rel. utility = ", format(round(relUtility, 2), nsmall = 2)
            )
          )) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_vline(xintercept = sc1_noTest_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_colour_viridis_d(option = "inferno", begin = 0.1, end = 0.9) +
          labs(
            x = "&nbsp;\ndays until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "relative utility of quarantine\n&nbsp;") +
          plotTheme

        makePlotly(plot)
      })

      output$sc1_noTest_legend <- renderPlot({
        grid::grid.draw(get_legend(sc1_noTest_plot()))
      })

      output$sc1_noTest_caption <- renderUI({
        tR.compare <- input$sc1_noTest_tR.compare
        tQ <- input$sc1_noTest_tQ

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of transmission that is prevented by quarantining an infected contact.",
          "We fix the time of exposure to t<sub>E</sub>=0,",
          "and quarantine begins after a delay of <strong>{tQ[1]}</strong> to <strong>{tQ[2]}</strong> days (colour).",
          "<br>",
          "<i>(right)</i> The relative utility of different quarantine durations compared to release",
          "on day <strong>{tR.compare}</strong>.",
          "<br>",
          "[Manuscript Figure 2-figure supplement 1]",
          "</span>"
          , .sep = " "
        ))
      })


      # SC1: ADHERENCE AND SYMPTOMS ----
      sc1_further_pars <- reactive({
        sc1_further_pars <- list(
          tQ = input$sc1_test_DeltaQ,
          DeltaT.vals = seq(input$sc1_test_DeltaT[2],input$sc1_test_DeltaT[1]),
          tR.compare = input$sc1_test_tRCompare,
          tS = incParams$mean[1],
          a.vals = seq(0, 1, 0.25),
          DeltaI = input$sc1_isolation_Delta
        )
        return(sc1_further_pars)
      })

      sc1_further_data <- reactive({
        # Define input parameters locally
        tE <- 0
        tQ <- sc1_further_pars()$tQ
        tR.compare <- sc1_further_pars()$tR.compare
        tR.vals <- seq(tQ, max(tR.compare, 10))
        DeltaT.vals <- sc1_further_pars()$DeltaT.vals
        tEnd <- 10
        tS <- sc1_further_pars()$tS
        a.vals <- sc1_further_pars()$a.vals
        DeltaI <- sc1_further_pars()$DeltaI

        # Calculate the relative adherence
        # First we need the efficacy of the baseline strategy
        fraction.compare <- getIntegral(upper = tR.compare, lower = tQ, tE = tE, params = genParams())
        # Now compute the relative adherence without testing
        rel_adherence.noTest <- data.frame(
          tE = tE,
          tQ = tQ,
          tT = NA,
          tR = tR.vals,
          tR.compare = tR.compare,
          DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
          release = tR.vals - tE,
          relAdherence = fraction.compare / ifelse(tR.vals >= tQ,
                                                   getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams()),
                                                   NA)
        )

        # Now the relative adherence with test-and-release
        frac.before <- getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams())
        frac.after <- getIntegral(upper = tEnd, lower = tR.vals, tE = tE, params = genParams())

        rel_adherence.test <- lapply(DeltaT.vals, function(DeltaT) {
          tT.vals <- tR.vals - DeltaT
          fraction <- ifelse(tT.vals >= tQ,
                             frac.before +
                               (1 - falseNeg(tT.vals - tE)) * frac.after,
                             NA)
          data.frame(
            tE = tE,
            tQ = tQ,
            tT = tT.vals,
            tR = tR.vals,
            tR.compare = tR.compare,
            DeltaT = factor(DeltaT, levels = c("noTest",DeltaT.vals)),
            release = tR.vals - tE,
            relAdherence = fraction.compare / fraction
          )
        }) %>% bind_rows()


        frac_asymptomatic <- lapply(a.vals, function(a) {
          fraction.asymptomatic <- ifelse(tR.vals >= tQ,
                                          getIntegral(upper = tR.vals, lower = tQ, tE = tE, params = genParams()),
                                          NA)
          fraction.symptomatic <- ifelse(tR.vals >= tQ,
                                         getIntegral(upper = pmax(pmin(tR.vals, tS + DeltaI),0), lower = tQ, tE = tE, params = genParams()),
                                         NA)
          data.frame(
            tE = tE,
            tQ = tQ,
            tS = tS,
            tR = tR.vals,
            delay = tQ - tE,
            release = tR.vals - tE,
            a = factor(a, levels = a.vals),
            fraction = a * fraction.asymptomatic + (1 - a) * fraction.symptomatic
          )
        }) %>% bind_rows()

        return(list(
          rel_adherence = rbind(rel_adherence.noTest, rel_adherence.test),
          frac_asymptomatic = frac_asymptomatic
        ))
      })

      # PLOTS ----
      output$sc1_adherence_plot <- renderPlotly({

        xLim <- c(sc1_further_pars()$tQ, max(sc1_further_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc1_further_data()$rel_adherence,
          mapping = aes(
            x = release, y = relAdherence, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "rel. adherence = ", format(round(relAdherence, 2), nsmall = 2)
            )
          )) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_vline(xintercept = sc1_further_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          coord_cartesian(ylim = c(0, 4)) +
          sc1_test_colours() +
          labs(
            x = "days until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "relative adherence required to\nmaintain quarantine efficacy\n&nbsp;"
          ) +
          plotTheme +
          theme(legend.position = "bottom")

        makePlotly(plot, show_legend = TRUE, legend_title = "delay between test\nand release (t<sub>R</sub> - t<sub>T</sub>)")
      })

      output$sc1_asymptomatic_plot <- renderPlotly({
        labs <- scales::percent(sc1_further_pars()$a.vals)
        names(labs) <- sc1_further_pars()$a.vals

        xLim <- c(sc1_further_pars()$tQ, max(sc1_further_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc1_further_data()$frac_asymptomatic,
          mapping = aes(
            x = release, y = fraction, colour = a, group = a,
            text = str_c(
              "fraction asymptomatic = ", scales::percent(as.numeric(levels(sc1_further_data()$frac_asymptomatic$a))[a]),
              "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1)
            )
          )) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "viridis", direction = -1, end = 0.9, name = "fraction\nasymptomatic",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(
            x = "days until release after exposure (t<sub>R</sub> - t<sub>E</sub>)",
            y = "fraction of transmission\nprevented by quarantine\n&nbsp;") +
          plotTheme +
          theme(legend.position = "bottom")

        makePlotly(plot, show_legend = TRUE, legend_title = "fraction\nasymptomatic (a)")
      })

      output$sc1_further_caption <- renderUI({
        tR.compare <- sc1_further_pars()$tR.compare
        DeltaI <- sc1_further_pars()$DeltaI
        tS <- format(round(sc1_further_pars()$tS, 1), nsmall = 1)
        tQ <- sc1_test_pars()$tQ

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fold-change in adherence to a new quarantine strategy that is required to maintain",
          "efficacy of the baseline <strong>{tR.compare}</strong> day standard strategy.",
          "The horizontal grey line represents equal adherence (relative adherence of 1).",
          "<br>",
          "<i>(right)</i> The impact of symptomatic cases on the fraction of total onward transmission per infected",
          "traced contact that is prevented by standard (no test) quarantine.",
          "We assume that symptomatic individuals will self-isolate <strong>{DeltaI}</strong> days after symptom onset.",
          "We use a symptom onset time of <strong>t<sub>S</sub>={tS}</strong>,",
          "where <strong>{tS}</strong> days is the mean incubation time.",
          "We fix the time of exposure at t<sub>E</sub>=0 and the time of",
          "entering quarantine at <strong>t<sub>Q</sub>={tQ}</strong>.",
          "<br>",
          "[Manuscript Figure 3 and Figure 3-figure supplement 1]",
          "</span>",
          .sep = " "
        ))
      })

      # SC2: TESTING & RELEASING ----
      sc2_test_pars <- reactive({
        sc2_test_pars <- list(
          y = input$sc2_test_y,
          tE.vals = seq(-input$sc2_test_y, 0),
          DeltaT.vals = seq(input$sc2_test_DeltaT[2],input$sc2_test_DeltaT[1]),
          tR.compare = input$sc2_test_tRCompare,
          s = input$sc2_test_sPercent/100,
          r = input$sc2_test_rPercent/100,
          normalisation = input$sc2_test_normalisation
        )
        return(sc2_test_pars)
      })

      sc2_test_data <- reactive({
        # Locally define the parameters
        y <- sc2_test_pars()$y
        tE.vals <- sc2_test_pars()$tE.vals
        tQ <- 0
        tR.compare <- sc2_test_pars()$tR.compare
        tR.vals <- seq(tQ, max(tR.compare, 10))
        DeltaT.vals <- sc2_test_pars()$DeltaT.vals
        s <- sc2_test_pars()$s
        r <- sc2_test_pars()$r
        tEnd <- 10
        normalisation <- sc2_test_pars()$normalisation

        # Maximum preventable fractions
        max_preventable <- getIntegral(upper = Inf, lower = tQ, tE = tE.vals, params = genParams())
        if (normalisation) {
          max_preventable_plot <- 1
        } else {
          max_preventable_plot <- mean(max_preventable)
          max_preventable <- rep.int(1, length(tE.vals))
        }

        # Quarantine efficacy without testing
        frac_no_test <- lapply(tR.vals, function(tR) {
          fraction <- mean(
            ifelse(max_preventable < 1e-10,
                   1,
                   getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())/max_preventable)
          )
          data.frame(
            y = y,
            tQ = tQ,
            tT = NA,
            tR = tR,
            DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
            release = tR - tQ,
            fraction = fraction
          )
        }) %>% bind_rows()

        # Quarantine efficacy with test-and-release
        frac_test <- lapply(DeltaT.vals, function(DeltaT) {
          lapply(tR.vals, function(tR) {
            tT <- tR - DeltaT

            fraction.before <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            fraction.after <- getIntegral(upper = tEnd, lower = tR, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(max_preventable < 1e-10,
                     1,
                     (fraction.before + (1 - falseNeg(tT-tE.vals)) * fraction.after)/max_preventable)
            )

            data.frame(
              y = y,
              tQ = tQ,
              tT = tT,
              tR = tR,
              DeltaT = factor(DeltaT, levels = c("noTest", DeltaT.vals)),
              release = tR - tQ,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        # Combine test and no-test results
        frac_combined <- rbind(frac_no_test,frac_test)

        # Fraction prevented based on test-and-release and reduced transmission
        frac_test_reduced <- lapply(DeltaT.vals, function(DeltaT) {
          lapply(tR.vals, function(tR) {
            tT <- tR - DeltaT

            fraction.before <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            fraction.after <- getIntegral(upper = tEnd, lower = tR, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(max_preventable < 1e-10,
                     1,
                     (fraction.before + (1 - falseNeg(tT-tE.vals) + r * falseNeg(tT-tE.vals)) * fraction.after)/max_preventable)
            )

            data.frame(
              y = y,
              tQ = tQ,
              tT = tT,
              tR = tR,
              DeltaT = factor(DeltaT, levels = c("noTest", DeltaT.vals)),
              release = tR - tQ,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()


        # Baseline Quarantine Utility
        fraction_compare <- mean(
          ifelse(max_preventable < 1e-10,
                 1,
                 getIntegral(upper = tR.compare, lower = tQ, tE = tE.vals, params = genParams())/max_preventable)
        )
        utility_compare <- getUtility(s = s,
                                      efficacy = fraction_compare,
                                      time = tR.compare - tQ)

        # Quarantine Utility without testing
        rel_utility_no_test <- lapply(tR.vals, function(tR) {
          fraction <- mean(
            ifelse(max_preventable < 1e-10,
                   1,
                   getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())/max_preventable)
          )
          utility <- ifelse(tR > tQ,
                            getUtility(s = s, efficacy = fraction, time = tR - tQ),
                            NA)
          data.frame(
            y = y,
            tQ = tQ,
            tT = NA,
            tR = tR,
            DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
            release = tR - tQ,
            relUtility = utility / utility_compare
          )
        }) %>% bind_rows()

        # Quarantine Utility with test-and-release
        rel_utility_test <- lapply(DeltaT.vals, function(DeltaT) {
          lapply(tR.vals, function(tR) {
            tT <- tR - DeltaT

            frac.before <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            frac.after <- getIntegral(upper = Inf, lower = tR, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(max_preventable < 1e-10,
                     1,
                     (frac.before + (1 - falseNeg(tT-tE.vals)) * frac.after)/max_preventable)
            )

            time.before <- tR - tQ
            time.after <- tEnd - tR
            time <- mean(time.before + s * (1 - falseNeg(tT-tE.vals)) * time.after)

            utility <- ifelse(tT >= tQ,
                              getUtility(s = s, efficacy = fraction,
                                         time = time),
                              NA)

            data.frame(
              y = y,
              tQ = tQ,
              tT = tT,
              tR = tR,
              DeltaT = factor(DeltaT, levels = c("noTest",DeltaT.vals)),
              release = tR - tQ,
              relUtility = utility / utility_compare
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        # Combine test and no-test results
        rel_utility_combined <- rbind(rel_utility_no_test,rel_utility_test)

        # Quarantine Utility with test-and-release and reduced post-quarantine transmission
        rel_utility_test_reduced <- lapply(DeltaT.vals, function(DeltaT) {
          lapply(tR.vals, function(tR) {
            tT <- tR - DeltaT

            frac.before <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            frac.after <- getIntegral(upper = Inf, lower = tR, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(max_preventable < 1e-10,
                     1,
                     (frac.before + (1 - falseNeg(tT-tE.vals) + r * falseNeg(tT-tE.vals)) * frac.after)/max_preventable)
            )

            time.before <- tR - tQ
            time.after <- tEnd - tR
            time <- mean(time.before + s * (1 - falseNeg(tT-tE.vals)) * time.after)

            utility <- ifelse(tT >= tQ,
                              getUtility(s = s, efficacy = fraction,
                                         time = time),
                              NA)

            data.frame(
              y = y,
              tQ = tQ,
              tT = tT,
              tR = tR,
              DeltaT = factor(DeltaT, levels = c("noTest",DeltaT.vals)),
              release = tR - tQ,
              relUtility = utility / utility_compare
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        return(list(
          max_preventable_plot = max_preventable_plot,
          frac_combined = frac_combined,
          frac_test_reduced = frac_test_reduced,
          rel_utility_combined = rel_utility_combined,
          rel_utility_test_reduced = rel_utility_test_reduced
        ))
      })

      # PLOTS ----
      sc2_test_colours <- reactive({
        n <- length(sc2_test_pars()$DeltaT.vals)
        cols <- c("#000000",colorRampPalette(brewer.pal(9, "Oranges")[9:4])(n))
        names(cols) <- c("noTest", sc2_test_pars()$DeltaT.vals)
        scale_colour_manual(
          name = expression("delay between test and release" ~ (t[R]-t[T])),
          values = cols,
          labels = c(noTest = "no test", dayLabels(sc2_test_pars()$DeltaT.vals)),
          guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 1)
        )
      })

      sc2_test_plot <- reactive({

        xLim <- c(0, max(sc2_test_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc2_test_data()$frac_combined,
          mapping = aes(
            x = release, y = fraction, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1)
            )
          )) +
          geom_hline(yintercept = sc2_test_data()$max_preventable_plot, colour = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          sc2_test_colours() +
          labs(
            x = "&nbsp;\nday of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = "fraction of local transmission\nprevented by quarantine\n&nbsp;") +
          plotTheme

        if (input$sc2_test_addMeasures) {
          plot <- plot +
            geom_line(
              data = sc2_test_data()$frac_test_reduced,
              inherit.aes = FALSE,
              mapping = aes(
                x = release, y = fraction, colour = DeltaT, group = DeltaT,
                text = str_c(
                  "reduced transmission<br>",
                  "t<sub>R</sub>-t<sub>T</sub> = ", dayLabels(DeltaT), "<br>",
                  "prevented = ", scales::percent(fraction, accuracy = 0.1)
                )
              ),
              linetype = "dashed")
        }

        return(plot)
      })

      output$sc2_test_plot <- renderPlotly({
        makePlotly(sc2_test_plot())
      })

      output$sc2_test_utility_plot <- renderPlotly({

        xLim <- c(0, max(sc2_test_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc2_test_data()$rel_utility_combined,
          mapping = aes(
            x = release, y = relUtility, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "rel.utility = ", format(round(relUtility, 2), nsmall = 2)
            )
          )) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_vline(xintercept = sc2_test_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          coord_cartesian(ylim = c(0, 4)) +
          sc2_test_colours() +
          labs(
            x = "&nbsp;\nday of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = "\nrelative utility of quarantine\n&nbsp;") +
          plotTheme

        if (input$sc2_test_addMeasures) {
          plot <- plot +
            geom_line(
              data = sc2_test_data()$rel_utility_test_reduced,
              inherit.aes = FALSE,
              mapping = aes(
                x = release, y = relUtility, colour = DeltaT, group = DeltaT,
                text = str_c(
                  "reduced transmission<br>",
                  "t<sub>R</sub>-t<sub>T</sub> = ", dayLabels(DeltaT), "<br>",
                  "rel. utility = ", format(round(relUtility, 2), nsmall = 2)
                )
              ),
              linetype = "dashed")
        }

        makePlotly(plot)
      })

      output$sc2_test_legend <- renderPlot({
        grid::grid.draw(get_legend(sc2_test_plot()))
      })

      output$sc2_test_caption <- renderUI({
        tR.compare <- sc2_test_pars()$tR.compare
        s <- input$sc2_test_sPercent
        r <- input$sc2_test_rPercent
        y <- sc2_test_pars()$y
        DeltaT.vals <- input$sc2_test_DeltaT
        normalisation <- ifelse(input$sc2_test_normalisation, "local", "total")

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of <strong>{normalisation}</strong> transmission that is prevented by",
          "quarantining an infected traveller returning from a <strong>y={y}</strong> day trip.",
          "Quarantine begins upon return at time t<sub>Q</sub>=0, and we assume exposure could have occurred",
          "at any time during the trip, i.e. <strong>-{y}&le;t<sub>E</sub>&le;0</strong>.",
          "Under the standard quarantine protocol (black), individuals are released without being tested ",
          "The test-and-release protocol (colours) requires a negative test result before early release,",
          "otherwise individuals remain isolated until they are no longer infectious (day 10).",
          "Colour intensity represents the delay between test and release",
          "(from <strong>{DeltaT.vals[1]}</strong> to <strong>{DeltaT.vals[2]}</strong> days).",
          "<br>",
          "<i>(right)</i> The relative utility of the quarantine scenarios compared to the standard protocol <strong>{tR.compare}</strong> day quarantine.",
          "The horizontal grey line represents equal utilities (relative utility of 1).",
          "We assume that the fraction of individuals in quarantine that are infected is",
          "<strong>s={s}%</strong>, and that there are no false-positive test results.",
          "<br>",
          "[Manuscript Figure 4 and Figure 4-figure supplement 2]",
          "</span>",
          .sep = " "
        ))
      })

      # SC2: QUARTANTINE AND TRAVEL DURATION (NO TEST)  ----
      sc2_noTest_pars <- reactive({
        sc2_noTest_pars <- list(
          y.vals = as.integer(input$sc2_noTest_y.vals),
          tR.vals = seq(input$sc2_noTest_tR.vals[1], input$sc2_noTest_tR.vals[2]),
          tR.compare = input$sc2_noTest_tR.compare,
          normalisation = input$sc2_noTest_normalisation # TRUE: local, FALSE: total
        )
        return(sc2_noTest_pars)
      })

      # Sort selected travel durations
      observe({
        y.valsSelected <- input$sc2_noTest_y.vals
        updateSelectizeInput(session, "sc2_noTest_y.vals", selected = sort(as.integer(y.valsSelected)))
      })

      sc2_noTest_data <- reactive({
        # Define input parameters locally
        y.vals <- sc2_noTest_pars()$y.vals
        tQ <- 0
        tR.vals <- sc2_noTest_pars()$tR.vals
        tR.compare <- sc2_noTest_pars()$tR.compare
        normalisation <- sc2_noTest_pars()$normalisation

        norm_fn <- function(normalisation, maxPreventable) {
          if (normalisation) {
            return(maxPreventable)
          } else {
            return(rep.int(1, length(maxPreventable)))
          }
        }

        frac <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y,0)
          maxPreventable <- getIntegral(upper = Inf, lower = tQ, tE = tE.vals, params = genParams())

          lapply(tR.vals, function(tR) {
            fraction <- mean(
              ifelse(maxPreventable < 1e-10,
                     1,
                     getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams()) /
                       norm_fn(normalisation, maxPreventable))
            )
            data.frame(
              y = factor(y, levels = y.vals),
              tQ = tQ,
              tR = tR,
              release = tR - tQ,
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        rel_utility <- lapply(y.vals, function(y) {
          tE.vals <- seq(-y,0)
          maxPreventable <- getIntegral(upper = Inf, lower = tQ, tE = tE.vals, params = genParams())

          fraction.compare <- mean(
            ifelse(maxPreventable < 1e-10,
                   1,
                   getIntegral(upper = tR.compare, lower = tQ, tE = tE.vals, params = genParams()) /
                     norm_fn(normalisation, maxPreventable))
          )
          utility.compare <- getUtility(efficacy = fraction.compare, time = tR.compare - tQ)

          lapply(tR.vals, function(tR) {
            fraction <- mean(
              ifelse(maxPreventable < 1e-10,
                     1,
                     getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams()) /
                       norm_fn(normalisation, maxPreventable))
            )
            utility <- ifelse(tR > tQ,
                              getUtility(efficacy = fraction, time = tR - tQ),
                              NA)

            data.frame(
              y = factor(y, levels = y.vals),
              tQ = tQ,
              tR = tR,
              release = tR - tQ,
              relUtility = utility / utility.compare
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        return(list(frac = frac, rel_utility = rel_utility))
      })

      # PLOTS ----
      sc2_noTest_plot <- reactive({

        xLim <- c(min(sc2_noTest_pars()$tR.vals), max(sc2_noTest_pars()$tR.vals))

        ggplot(
          data = sc2_noTest_data()$frac,
          mapping = aes(
            x = release, y = fraction, colour = y, group = y,
            text = str_c(
              "travel duration = ", dayLabels(y),
              "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1)
            )
        )) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "inferno", begin = 0.1, end = 0.9, name = "duration of travel",
            labels = dayLabels(levels(sc2_noTest_data()$frac$y)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(
            x = "&nbsp;\nday of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = paste("fraction of", ifelse(sc2_noTest_pars()$normalisation, "local", "total"), "transmission\nprevented by quarantine\n&nbsp;")) +
          plotTheme
      })

      output$sc2_noTest_plot <- renderPlotly({
        makePlotly(sc2_noTest_plot())
      })

      output$sc2_noTest_utility_plot <- renderPlotly({

        xLim <- c(min(sc2_noTest_pars()$tR.vals), max(sc2_noTest_pars()$tR.vals))

        plot <- ggplot(
          data = sc2_noTest_data()$rel_utility,
          mapping = aes(
            x = release, y = relUtility, colour = y, group = y,
            text = str_c(
              "travel duration = ", dayLabels(y),
              "<br>",
              "rel. utility = ", format(round(relUtility, 2), nsmall = 2)
            )
        )) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_vline(xintercept = sc2_noTest_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          coord_cartesian( ylim = c(0, 2.5)) +
          scale_colour_viridis_d(option = "inferno", begin = 0.1, end = 0.9) +
          labs(
            x = "&nbsp;\nday of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = "relative utility of quarantine\n&nbsp;") +
          plotTheme +
          theme(legend.position = "none")

        makePlotly(plot)
      })

      output$sc2_noTest_legend <- renderPlot({
        grid::grid.draw(get_legend(sc2_noTest_plot()))
      })

      output$sc2_noTest_caption <- renderUI({
        tR.compare <- sc2_noTest_pars()$tR.compare
        normalisation <- if_else(input$sc2_noTest_normalisation, "local", "total")
        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fraction of <strong>{normalisation}</strong> transmission that is prevented by",
          "quarantining an infected traveller.",
          "<br>",
          "<i>(right)</i> The relative utility of the different quarantine durations compared to release on day",
          "<strong>{tR.compare}</strong>, based on the total fraction of transmission prevented.",
          "Colours represent the duration of travel y and we assume infection can occur with equal probability",
          "on each day t<sub>E</sub> which satisfies -y&le;t<sub>E</sub>&le;0.",
          "Quarantine begins at time t<sub>Q</sub>=0, which is the time of arrival.",
          "<br>",
          "[Manuscript Figure 4-figure supplement 1]",
          "</span>",
          .sep = " "
        ))
      })


      # SC2: ADHERENCE AND SYMPTOMS ----
      sc2_further_pars <- reactive({
        sc2_further_pars <- list(
          y = input$sc2_test_y,
          tE.vals = seq(-input$sc2_test_y, 0),
          DeltaT.vals = seq(input$sc2_test_DeltaT[2],input$sc2_test_DeltaT[1]),
          tR.compare = input$sc2_test_tRCompare,
          a.vals = seq(0, 1, 0.25),
          DeltaI = input$sc2_isolation_Delta
        )
        return(sc2_further_pars)
      })

      sc2_further_data <- reactive({
        # Define input parameters locally
        y <- sc2_further_pars()$y
        tE.vals <- sc2_further_pars()$tE.vals
        tQ <- 0
        tR.compare <- sc2_further_pars()$tR.compare
        tR.vals <- seq(tQ, max(tR.compare, 10))
        DeltaT.vals <- sc2_further_pars()$DeltaT.vals
        tS.vals <- tE.vals + incParams$mean[1]
        tEnd <- 10
        a.vals <- sc2_further_pars()$a.vals
        DeltaI <- sc2_further_pars()$DeltaI

        # Calculate the relative adherence
        # First we need the maximum attainable efficacy
        maxPreventable <- getIntegral(upper = Inf, lower = tQ, tE = tE.vals, params = genParams())
        # We need the efficacy of the baseline strategy
        fraction.compare <- mean(
          ifelse(maxPreventable < 1e-10,
                 1,
                 getIntegral(upper = tR.compare, lower = tQ, tE = tE.vals, params = genParams()) / maxPreventable
                 )
          )

        # Now compute the relative adherence without testing
        rel_adherence_noTest <- lapply(tR.vals, function(tR) {
          fraction <- mean(
            ifelse(maxPreventable < 1e-10,
                   1,
                   getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())/maxPreventable
            )
          )

          data.frame(
            y = y,
            tQ = tQ,
            tT = NA,
            tR = tR,
            DeltaT = factor("noTest", levels = c("noTest", DeltaT.vals)),
            release = tR - tQ,
            relAdherence = ifelse(tR > tQ,
                                  fraction.compare / fraction,
                                  NA)
          )
        }) %>% bind_rows()


        rel_adherence_test <- lapply(DeltaT.vals, function(DeltaT) {
          lapply(tR.vals, function(tR) {
            tT <- tR - DeltaT

            fraction.before <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            fraction.after <- getIntegral(upper = tEnd, lower = tR, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(maxPreventable < 1e-10,
                     1,
                     (fraction.before + (1 - falseNeg(tT - tE.vals)) * fraction.after) /
                       maxPreventable
              )
            )

            data.frame(
              y = y,
              tQ = tQ,
              tT = tT,
              tR = tR,
              DeltaT = factor(DeltaT, levels = c("noTest", DeltaT.vals)),
              release = tR - tQ,
              relAdherence = ifelse(tR >= tQ,
                                    fraction.compare / fraction,
                                    NA)
            )
          }) %>% bind_rows()
        }) %>% bind_rows()


        # Now calculate the symptomatic and asymptomatic fractions
        frac_asymptomatic <- lapply(a.vals, function(a) {
          lapply(tR.vals, function(tR) {
            fraction.asymptomatic <- getIntegral(upper = tR, lower = tQ, tE = tE.vals, params = genParams())
            fraction.symptomatic <- getIntegral(upper = pmax(pmin(tR, tS.vals+DeltaI),tQ), lower = tQ, tE = tE.vals, params = genParams())
            fraction <- mean(
              ifelse(maxPreventable < 1e-10,
                     1,
                     (a * fraction.asymptomatic + (1 - a) * fraction.symptomatic)/maxPreventable))

            data.frame(
              y = y,
              tQ = tQ,
              tR = tR,
              release = tR - tQ,
              a = factor(a, levels = a.vals),
              fraction = fraction
            )
          }) %>% bind_rows()
        }) %>% bind_rows()

        return(list(
          rel_adherence = rbind(rel_adherence_noTest, rel_adherence_test),
          frac_asymptomatic = frac_asymptomatic
        ))
      })

      # PLOTS ----
      output$sc2_adherence_plot <- renderPlotly({

        xLim <- c(0, max(sc2_further_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc2_further_data()$rel_adherence,
          mapping = aes(
            x = release, y = relAdherence, colour = DeltaT, group = DeltaT,
            text = str_c(
              ifelse(
                DeltaT == "noTest",
                "no test",
                paste0("t<sub>R</sub> - t<sub>T</sub> = ", dayLabels(DeltaT))),
              "<br>",
              "rel. adherence = ", format(round(relAdherence, 2), nsmall = 2)
            )
          )) +
          geom_hline(yintercept = 1, color = "darkgrey", size = plot_line_size) +
          geom_vline(xintercept = sc2_further_pars()$tR.compare, color = "darkgrey", size = plot_line_size) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          coord_cartesian(ylim = c(0, 4.4)) +
          sc2_test_colours() +
          labs(
            x = "day of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = "relative adherence required to\nmaintain local quarantine efficacy\n&nbsp;") +
          plotTheme +
          theme(legend.position = "bottom")

        makePlotly(plot,
                   show_legend = TRUE,
                   legend_title = "delay between test\nand release (t<sub>R</sub> - t<sub>T</sub>)")
      })

      output$sc2_asymptomatic_plot <- renderPlotly({
        labs <- scales::percent(sc1_further_pars()$a.vals)
        names(labs) <- sc1_further_pars()$a.vals

        xLim <- c(0, max(sc2_further_pars()$tR.compare, 10))

        plot <- ggplot(
          data = sc2_further_data()$frac_asymptomatic,
          mapping = aes(
            x = release, y = fraction, colour = a, group = a,
            text = str_c(
              "fraction asymptomatic = ", scales::percent(as.numeric(levels(sc2_further_data()$frac_asymptomatic$a))[a]),
              "<br>",
              "prevented = ", scales::percent(fraction, accuracy = 0.1)
                           )
            )) +
          geom_line(size = plot_line_size) +
          geom_point(size = plot_point_size) +
          scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2])) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          scale_colour_viridis_d(
            option = "viridis", direction = -1, end = 0.9, name = "fraction\nasymptomatic",
            labels = labs, guide = guide_legend(title.position = "left", title.hjust = 0.5, nrow = 2, byrow = T)
          ) +
          labs(
            x = "day of release after arrival (t<sub>R</sub> - t<sub>Q</sub>)",
            y = "local fraction of transmission\nprevented by quarantine\n&nbsp;") +
          plotTheme +
          theme(legend.position = "bottom")

        makePlotly(plot, show_legend = TRUE, legend_title = "fraction\nasymptomatic")
      })

      output$sc2_further_caption <- renderUI({
        tR.compare <- sc2_further_pars()$tR.compare
        y <- sc2_further_pars()$y
        meanInc <- format(round(incParams$mean[1], 1), nsmall = 1)
        DeltaI <- sc2_further_pars()$DeltaI

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The fold-change in adherence to a new quarantine strategy that is required to",
          "maintain efficacy (local fraction of transmission prevented) of the",
          "baseline <strong>{tR.compare}</strong> day standard strategy.",
          "The horizontal grey line represents equal adherence (relative adherence of 1).",
          "<br>",
          "<i>(right)</i> The impact of symptomatic cases on the fraction of local transmission per infected",
          "traveller that is prevented by standard (no test) quarantine.",
          "We assume that symptomatic individuals will self-isolate <strong>{DeltaI}</strong> days after symptom onset.",
          "We use a symptom onset time of t<sub>S</sub>=t<sub>E</sub>+{meanInc},",
          "where {meanInc} days is the mean incubation time.",
          "For both panels we fix the trip duration to <strong>{y}</strong> days and assume exposure can occur at",
          "any time <strong>-{y}&le;t<sub>E</sub>&le;0</strong>.",
          "Quarantine begins at time t<sub>Q</sub>=0.",
          "<br>",
          "[Manuscript Figure 4-figure supplement 3]",
          "</span>",
          .sep = " "
        ))
      })
    }
  )
}

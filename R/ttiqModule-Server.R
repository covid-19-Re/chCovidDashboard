library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)
library(cowplot)

#incDist <- readRDS("R/ttiqModuleFiles/incDist.rds")

ttiqServer <- function(id) {
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
      stepSize <- 1e-2
      times <- seq(-25, 25, stepSize)
      i0 <- which(times == 0)
      iMax <- length(times)

      # Generation time
      genParams <- reactive({
        genParams <- data.frame(shape = input$genShape, scale = input$genScale)
        genParams$mean <- genParams$scale * gamma(1 + 1 / genParams$shape)
        return(genParams)
      })

      genDist <- reactive({
        data.frame(
          t = times,
          pdf = getGenDist(times = times, params = genParams()),
          CDF = getGenDist(times = times, params = genParams(), CDF = TRUE)
        )
      })

      # Infectivity profile
      infParams <- reactive({
        infParams <- data.frame(shift = input$infShift, scale = input$infScale, df = input$infDf)
        infParams$mean <- infParams$shift / infParams$scale
        return(infParams)
      })

      infProf <- reactive({
        data.frame(
          t = times,
          pdf = getInfectivityProfile(times = times, params = infParams()),
          CDF = getInfectivityProfile(times = times, params = infParams(), CDF = TRUE)
        )
      })

      # Incubation period
      # incParams <- reactive({
      #   incParams <- data.frame(meanlog = input$incMeanLog, sdlog = input$sdLog)
      #   incParams$mean <- exp(incParams$meanlog + (incParams$sdlog^2) / 2)
      #   return(incParams)
      # })
      incParams <- incParamsDefault

      incDist <- reactive({
        data.frame(
          t = times,
          pdf = getIncubationPeriod(times = times, params = incParams),
          CDF = getIncubationPeriod(times = times, params = incParams, CDF = TRUE)
        )
      })

      # Integral of g(t')Q(t'+t) from 0 to \infty with 0 \le t < \infty
      integral <- reactive({
        data.frame(
          t = times,
          J = sapply(seq_along(times), function(i) {
            g <- incDist()[seq(i0, iMax), "pdf"]
            Qt <- genDist()[seq(i, iMax - i0 + i), "CDF"]
            Qt[is.na(Qt)] <- 1
            sum(g * Qt) * stepSize
          }, USE.NAMES = F)
        )
      })

      # DISTRIBUTIONS PLOTS ----
      yLim <- c(0, 0.22)
      xLim <- c(0, 20)
      labY <- 0.21
      output$incDistPlot <- renderPlot({
        ggplot(incDist(), aes(x = t, y = pdf)) +
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

      output$genDistPlot <- renderPlot({
        ggplot(genDist(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = genParams()$mean, linetype = "dashed", alpha = 0.5) +
          annotate("text",
                   label = paste0(" mean = ", format(round(genParams()$mean, 1), nsmall = 1), " days"),
                   x = genParams()$mean, y = labY,
                   hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(xlim = xLim, ylim = yLim) +
          labs(x = "generation time", y = "probability density") +
          ggtitle("A: generation time distribution") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      # RTTIQ: REPRODUCTIVE NUMBER UNDER TTIQ ----
      # Parameters
      pars_TTIQ <- reactive({
        list(
          R = input$R_TTIQ,
          alpha = input$alpha_TTIQ,
          f = input$f_TTIQ,
          Delta1 = input$Delta1_TTIQ,
          tau = input$tau_TTIQ,
          g = input$g_TTIQ,
          Delta2 = input$Delta2_TTIQ
        )
      })
      # Reproductive number across parameter values
      data_TTIQ <- reactive({
        focalParams <- pars_TTIQ()
        print(focalParams)
        #' Number of cases under focal parameters
        focal <- getTertiaryCases(paramList = focalParams, infProf = infProf(),
                                  integral = integral(), genDist = genDist(),
                                  stepSize = stepSize)
        print(focal)
        #' Perturb parameters and compute cases
        df <- bind_rows(
          perturbParam(paramList = focalParams, param = "f", values = seq(0,1,0.1),
                       infProf = infProf(), integral = integral(),
                       genDist = genDist(), stepSize = stepSize),
          perturbParam(paramList = focalParams, param = "g", values = seq(0,1,0.1),
                       infProf = infProf(), integral = integral(),
                       genDist = genDist(), stepSize = stepSize),
          perturbParam(paramList = focalParams, param = "Delta1", values = seq(0,6,0.5),
                       infProf = infProf(), integral = integral(),
                       genDist = genDist(), stepSize = stepSize),
          perturbParam(paramList = focalParams, param = "Delta2", values = seq(0,6,0.5),
                       infProf = infProf(), integral = integral(),
                       genDist = genDist(), stepSize = stepSize),
          perturbParam(paramList = focalParams, param = "tau", values = seq(0,6,0.5),
                       infProf = infProf(), integral = integral(),
                       genDist = genDist(), stepSize = stepSize)
        )

        return(list(focal = focal, df = df))
      })

      # PLOTS ----
      yLim2 <- c(0.4,2.5)

      output$prob_TTIQ <- renderPlotly({
        df <- data_TTIQ()$df
        df <- df[df$param %in% c("f","g"),]

        plot <- ggplot(
          data = df,
          mapping = aes(x = value, y = ter.per.sec, colour = param, group = param,
                        text = str_c(
                          param, " = ", scales::percent(value, accuracy = 0.1), "<br>",
                          "R<sub>TTIQ</sub> = ", format(round(ter.per.sec, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = pars_TTIQ()$R, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = data_TTIQ()$focal$ter.per.sec, colour = "black") +
          geom_line(size = plot_line_size) +
          scale_x_continuous(limits = c(0,1), labels = scales::percent) +
          scale_y_continuous(limits = yLim2) +
          scale_colour_manual(values = c(f = palette_OkabeIto[1], g = palette_OkabeIto[4])) +
          labs(
            x = "probability",
            y = "R<sub>TTIQ</sub>\n&nbsp;"
          ) +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "")
      })

      output$time_TTIQ <- renderPlotly({
        df <- data_TTIQ()$df
        df <- df[df$param %in% c("Delta1","Delta2","tau"),]
        df$param <- factor(df$param, levels = c("Delta1","Delta2","tau"))

        plot <- ggplot(
          data = df,
          mapping = aes(x = value, y = ter.per.sec, colour = param, group = param,
                        text = str_c(
                          param, " = ", dayLabels(value), "<br>",
                          "R<sub>TTIQ</sub> = ", format(round(ter.per.sec, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = pars_TTIQ()$R, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = data_TTIQ()$focal$ter.per.sec, colour = "black") +
          geom_line(size = plot_line_size) +
          scale_x_continuous(limits = c(0,6)) +
          scale_y_continuous(limits = yLim2) +
          scale_colour_manual(values = c(Delta1 = palette_OkabeIto[2], Delta2 = palette_OkabeIto[5], tau = palette_OkabeIto[3])) +
          labs(
            x = "time (days)",
            y = "R<sub>TTIQ</sub>\n&nbsp;") +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "")
      })

      output$caption_TTIQ <- renderUI({
        R <- input$R_TTIQ
        alpha <- input$alpha_TTIQ
        f <- input$f_TTIQ
        Delta1 <- input$Delta1_TTIQ
        tau <- input$tau_TTIQ
        g <- input$g_TTIQ
        Delta2 <- input$Delta2_TTIQ

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "The response of the reproductive number R<sub>TTIQ</sub> to single TTIQ parameter pertubations.",
          "We consider perturbations from the focal TTIQ parameter combination with",
          "<strong>f = {f}</strong>,",
          "<strong>g = {g}</strong>,",
          "<strong>&Delta;<sub>1</sub> = {Delta1} days</strong>,",
          "<strong>&Delta;<sub>2</sub> = {Delta2} days</strong>, and",
          "<strong>&tau;<sub>1</sub> = {tau} days</strong>.",
          "We then vary each parameter individually,",
          "keeping the remaining four parameters fixed at the above values.",
          "R<sub>TTIQ</sub> for the focal parameter set is shown as a thin black line.",
          "With f=0 (no TTIQ) we expect R<sub>TTIQ</sub> = R (upper grey line).",
          "The critical threshold for controlling an epidemic is R<sub>TTIQ</sub> = 1 (lower grey line).<br>",
          "<i>(left)</i> The probability parameters f and g.<br>",
          "<i>(right)</i> The parameters which carry units of time (days).<br>",
          "We set the baseline <strong>R = {R}</strong> throughout, which is the intensity of the epidemic in the absence of any TTIQ intervention.",
          "We fix the fraction of transmission that is attributed to asymptomatic infections to",
          "<strong>&alpha; = {alpha}</strong>.",
          "</span>",
          .sep = " "
        ))
      })


      # RTI: REPRODUCTIVE NUMBER UNDER TESTING AND ISOLATING ----
      # Parameters
      pars_TI <- reactive({
        list(
          R = input$R_TI,
          alpha = input$alpha_TI,
          f = seq(0,1,0.05),
          Delta1 = seq(0,4,1)
        )
      })

      # Data
      data_TI <- reactive({
        #' Tertiary cases as a function of f and Delta_1
        tertiaryCases <- getTertiaryCasesNoTracing(paramList = pars_TI(),
                                                    infProf = infProf(),
                                                    integral = integral())
        #' Critical region where epidemic is controlled
        paramList <- pars_TI()
        paramList$f <- seq(min(paramList$f), max(paramList$f), 0.05) #' Finer grid for interpolation
        paramList$Delta1 <- seq(0, max(paramList$Delta1), 0.05) #' Finer grid for interpolation
        c <- getTertiaryCasesNoTracing(paramList = paramList, infProf = infProf(),
                                       integral = integral())
        #' Compute the threshold between controlled and uncontrolled epidemics (R0 <> 1)
        tertiarySummary <- by(c, INDICES = list(c$alpha, c$f, c$R), function(df) {
          df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]
          Tc <- -Inf
          if (all(df$ter.per.sec < 1)) Tc <- Inf
          else if (any(df$ter.per.sec < 1) & any(df$ter.per.sec > 1)) Tc <- approx(x = df$ter.per.sec, y = df$Delta1, xout = 1)$y
          data.frame(
            alpha = factor(unique(df$alpha), levels = paramList$alpha),
            f = factor(unique(df$f), levels = paramList$f),
            R = factor(unique(df$R), levels = paramList$R),
            Delta1 = Tc
          )
        })
        tertiarySummary <- do.call(rbind, tertiarySummary)
        return(list(tertiaryCases = tertiaryCases, tertiaryCases2 = c, tertiarySummary = tertiarySummary))
      })

      # PLOTS ----
      output$lines_TI <- renderPlotly({
        df <- data_TI()$tertiaryCases
        df$f <- as.numeric(levels(df$f))[df$f]


        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = ter.per.sec, colour = Delta1, group = Delta1,
                        text = str_c(
                          "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                          "Δ<sub>1</sub> = ", dayLabels(Delta1), "<br>",
                          "R<sub>TTIQ</sub> = ", format(round(ter.per.sec, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, size = plot_line_size, colour = "darkgrey") +
          geom_line(size = plot_line_size) +
          #geom_point(size = plot_point_size) +
          scale_x_continuous(limits = c(min(pars_TI()$f),max(pars_TI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(1, input$R_TI))) +
          scale_colour_viridis_d(
            option = "plasma", end = 0.9,
            name = "delay &Delta;<sub>1</sub> (days)",
            labels = dayLabels(levels(df$Delta1)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(
            x = "fraction of symptomatic\nindividuals isolated (f)",
            y = "R<sub>TTIQ</sub>\n&nbsp;") +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "delay Δ<sub>1</sub> (days)")
      })

      output$density_TI <- renderPlotly({
        df <- data_TI()$tertiaryCases2
        df$f <- as.numeric(levels(df$f))[df$f]
        df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]

        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = Delta1, group = R)) +
          geom_raster(aes(fill = ter.per.sec, text = str_c(
            "f = ", scales::percent(f, accuracy = 0.1), "<br>",
            "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days", "<br>",
            "R<sub>TTIQ</sub> = ", format(round(ter.per.sec, 2), nsmall = 2)
          )), interpolate = T) +
          #geom_contour(aes(z = tertiaryCases), colour = "black", size = plot_line_size, breaks = 1) +
          scale_fill_gradientn(colours = c("seagreen","white","white", "white","salmon"), name = "R_TTIQ",
                               values = scales::rescale(c(0,1-.Machine$double.eps,1,1+.Machine$double.eps,max(1.1,input$R_TI))),
                               limits = c(0,max(1.1,input$R_TI)),
                               guide = guide_colorbar(title.position = "left", direction = 1)) +
          scale_x_continuous(limits = c(min(pars_TI()$f),max(pars_TI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(pars_TI()$Delta1)), expand = F) +
          labs(
            x = "fraction of symptomatic\nindividuals isolated (f)",
            y = "delay Δ<sub>1</sub> (days)\n&nbsp;") +
          plotTheme

        #' Add RTTIQ == 1 contour
        df2 <- data_TI()$tertiarySummary
        df2 <- df2[df2$Delta1 >= 0,]
        if (nrow(df2) > 0) {
          df2$f <- as.numeric(levels(df2$f))[df2$f]

          plot <- plot + geom_line(data = df2,
                                   mapping = aes(x = f, y = Delta1, text = str_c(
                                     "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                                     "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days"
                                   )), size = plot_line_size, colour = "black")
        }

        makePlotly(plot, show_legend = T, legend_title = "R<sub>TTIQ</sub>")
      })

      output$caption_TI <- renderUI({
        R <- input$R_TI
        alpha <- input$alpha_TI

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The reproductive number R<sub>TI</sub> as a function",
          "of the testing & isolation coverage f (x-axis) and delay &Delta;<sub>1</sub> (colour).",
          "Horizontal grey line is the threshold for epidemic control (R<sub>TI</sub> = 1).<br>",
          "<i>(right)</i> The impact of testing &amp; isolation on R<sub>TI</sub>",
          "as a function of the fraction of symptomatic individuals that are isolated (f; x-axis)",
          "and delay to isolation after symptom onset (&Delta;<sub>1</sub>; y-axis).",
          "The black line represents the criticial reproductive number R<sub>TI</sub> = 1.",
          "Above this line (red zone) we have on average more than one secondary infection per infected and the epidemic is growing.",
          "Below this line (green zone) we have less than one secondary infection per infected and the epidemic is suppressed.<br>",
          "We use <strong>R = {R}</strong> for the average number ",
          "of secondary infections in the absence of testing &amp; isolating",
          "(i.e. <i>f</i>=0).",
          "We fix the fraction of transmission that is attributed to asymptomatic infections to",
          "<strong>&alpha; = {alpha}</strong>.",
          "</span>",
          .sep = " "
        ))
      })
    }
  )
}

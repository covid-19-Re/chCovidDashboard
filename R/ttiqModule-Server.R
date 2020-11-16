library(shiny)
library(tidyverse)
library(shinycssloaders)
library(glue)
library(cowplot)

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
      incParams <- reactive({
        incParams <- data.frame(meanlog = input$incMeanLog, sdlog = input$sdLog)
        incParams$mean <- exp(incParams$meanlog + (incParams$sdlog^2) / 2)
        return(incParams)
      })

      incDist <- reactive({
        data.frame(
          t = times,
          pdf = getIncubationPeriod(times = times, params = incParams()),
          CDF = getIncubationPeriod(times = times, params = incParams(), CDF = TRUE)
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
      yLim <- c(0, 0.23)
      labY <- 0.212

      output$genDistPlot <- renderPlot({
        ggplot(genDist(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = genParams()$mean, linetype = "dashed", colour = "darkgrey") +
          annotate("text",
                   label = paste0(" mean = ", round(genParams()$mean, 1), " days"),
                   x = genParams()$mean, y = labY,
                   hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(xlim = c(0,15), ylim = yLim) +
          labs(x = "generation time", y = "probability density") +
          ggtitle("A: generation time distribution") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      output$infProfPlot <- renderPlot({
        ggplot(infProf(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = infParams()$mean, linetype = "dashed", colour = "darkgrey") +
          annotate("text",
                   label = paste0(" mean = ", round(infParams()$mean, 1), " days"),
                   x = infParams()$mean, y = labY,
                   hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(xlim = c(-10,15), ylim = yLim, expand = F) +
          labs(x = "days after onset of symptoms", y = "probability density") +
          ggtitle("B: infectivity profile") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })

      output$incDistPlot <- renderPlot({
        ggplot(incDist(), aes(x = t, y = pdf)) +
          geom_vline(xintercept = incParams()$mean, linetype = "dashed", colour = "darkgrey") +
          annotate("text",
                   label = paste0(" mean = ", round(incParams()$mean, 1), " days"),
                   x = incParams()$mean, y = labY,
                   hjust = 0, vjust = 0
          ) +
          geom_line() +
          coord_cartesian(xlim = c(0,20), ylim = yLim, expand = F) +
          labs(x = "incubation period (days)", y = "probability density") +
          ggtitle("C: incubation period") +
          plotTheme +
          theme(plot.title = element_text(size = plotTheme$text$size, face = "bold"))
      })


      # terTTIQ: TERTIARY CASES UNDER TTIQ ----
      # Parameters
      pars_terTTIQ <- reactive({
        list(
          Re = input$Re_terTTIQ,
          f = input$f_terTTIQ,
          Delta1 = input$Delta1_terTTIQ,
          tau = input$tau_terTTIQ,
          g = input$g_terTTIQ,
          Delta2 = input$Delta2_terTTIQ
        )
      })
      # Tertiary cases across parameter values
      data_terTTIQ <- reactive({
        focalParams <- pars_terTTIQ()
        #' Number of cases under focal parameters
        focal <- getTertiaryCases(paramList = focalParams, infProf = infProf(),
                                  integral = integral(), genDist = genDist(),
                                  stepSize = stepSize)
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

      output$prob_terTTIQ <- renderPlotly({
        df <- data_terTTIQ()$df
        df <- df[df$param %in% c("f","g"),]

        plot <- ggplot(
          data = df,
          mapping = aes(x = value, y = tertiaryCases, colour = param, group = param,
                        text = str_c(
                          param, " = ", scales::percent(value, accuracy = 0.1), "<br>",
                          "n<sub>3</sub> = ", format(round(tertiaryCases, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = pars_terTTIQ()$Re^2, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = data_terTTIQ()$focal$tertiaryCases, colour = "darkgrey",
                     size = plot_line_size, linetype = "dashed") +
          geom_line(size = plot_line_size) +
          scale_x_continuous(limits = c(0,1), labels = scales::percent) +
          scale_y_continuous(limits = yLim2) +
          scale_colour_manual(values = c(f = palette_OkabeIto[1], g = palette_OkabeIto[4])) +
          labs(
            x = "probability",
            y = "tertiary cases (n<sub>3</sub>)\n&nbsp;"
          ) +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "")
      })

      output$time_terTTIQ <- renderPlotly({
        df <- data_terTTIQ()$df
        df <- df[df$param %in% c("Delta1","Delta2","tau"),]
        df$param <- factor(df$param, levels = c("Delta1","Delta2","tau"))

        plot <- ggplot(
          data = df,
          mapping = aes(x = value, y = tertiaryCases, colour = param, group = param,
                        text = str_c(
                          param, " = ", dayLabels(value), "<br>",
                          "n<sub>3</sub> = ", format(round(tertiaryCases, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = pars_terTTIQ()$Re^2, colour = "darkgrey", size = plot_line_size) +
          geom_hline(yintercept = data_terTTIQ()$focal$tertiaryCases, colour = "darkgrey",
                     size = plot_line_size, linetype = "dashed") +
          geom_line(size = plot_line_size) +
          scale_x_continuous(limits = c(0,6)) +
          scale_y_continuous(limits = yLim2) +
          scale_colour_manual(values = c(Delta1 = palette_OkabeIto[2], Delta2 = palette_OkabeIto[5], tau = palette_OkabeIto[3])) +
          labs(
            x = "time (days)",
            y = "tertiary cases (n<sub>3</sub>)\n&nbsp;") +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "")
      })

      output$caption_terTTIQ <- renderUI({
        Re <- input$Re_terTTIQ
        f <- input$f_terTTIQ
        Delta1 <- input$Delta1_terTTIQ
        tau <- input$tau_terTTIQ
        g <- input$g_terTTIQ
        Delta2 <- input$Delta2_terTTIQ

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "The number of tertiary cases per index case in the presence of contact tracing. ",
          "We consider perturbations from the focal TTIQ parameter combination with ",
          "<strong>f = {f}</strong>, ",
          "<strong>g = {g}</strong>, ",
          "<strong>&Delta;<sub>1</sub> = {Delta1} days</strong>, ",
          "<strong>&Delta;<sub>2</sub> = {Delta2} days</strong>, and ",
          "<strong>&tau;<sub>1</sub> = {tau} days</strong>. ",
          "The number of tertiary cases for the focal parameter set is shown as the dashed grey line. ",
          "We then vary each parameter individually, ",
          "keeping the remaining four parameters fixed at the above values. ",
          "We set <strong>R<sub>e</sub> = {Re}</strong> throughout. ",
          "With f = 0 we expect R<sub>e</sub><sup>2</sup> tertiary cases (upper grey line). ",
          "The critical threshold for controlling an epidemic is one tertiary case per index case (lower grey line). ",
          "<i>(left)</i> The probability parameters f and g. ",
          "<i>(right)</i> The parameters which carry units of time (days).",
          "</span>"
        ))
      })


      # terTI: TERTIARY CASES UNDER TESTING AND ISOLATING ----
      # Parameters
      pars_terTI <- reactive({
        list(
          Re = input$Re_terTI,
          #f = seq(input$f_terTI[1], input$f_terTI[2], 0.1),
          #Delta1 = seq(input$Delta1_terTI[1], input$Delta1_terTI[2], 1)
          f = seq(0,1,0.05),
          Delta1 = seq(0,4,1)
        )
      })

      # Tertiary cases and summary
      data_terTI <- reactive({
        #' Tertiary cases as a function of f and Delta_1
        tertiaryCases <- getTertiaryCasesNoTracing(paramList = pars_terTI(),
                                                    infProf = infProf(),
                                                    integral = integral())
        #' Critical region where epidemic is controlled
        paramList <- pars_terTI()
        paramList$f <- seq(min(paramList$f), max(paramList$f), 0.05) #' Finer grid for interpolation
        paramList$Delta1 <- seq(0, max(paramList$Delta1), 0.05) #' Finer grid for interpolation
        c <- getTertiaryCasesNoTracing(paramList = paramList, infProf = infProf(),
                                       integral = integral())
        #' Compute the threshold between controlled and uncontrolled epidemics (R0 <> 1)
        tertiarySummary <- by(c, INDICES = list(c$f, c$Re), function(df) {
          df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]
          Tc <- -Inf
          if (all(df$tertiaryCases < 1)) Tc <- Inf
          else if (any(df$tertiaryCases < 1) & any(df$tertiaryCases > 1)) Tc <- approx(x = df$tertiaryCases, y = df$Delta1, xout = 1)$y
          data.frame(
            f = factor(unique(df$f), levels = paramList$f),
            Delta1 = Tc,
            Re = factor(unique(df$Re), levels = paramList$Re)
          )
        })
        tertiarySummary <- do.call(rbind, tertiarySummary)
        return(list(tertiaryCases = tertiaryCases, tertiaryCases2 = c, tertiarySummary = tertiarySummary))
      })

      # PLOTS ----
      output$cases_terTI <- renderPlotly({
        df <- data_terTI()$tertiaryCases
        df$f <- as.numeric(levels(df$f))[df$f]

        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = tertiaryCases, colour = Delta1, group = Delta1,
                        text = str_c(
                          "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                          "Δ<sub>1</sub> = ", dayLabels(Delta1), "<br>",
                          "n<sub>3</sub> = ", format(round(tertiaryCases, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, size = plot_line_size, colour = "darkgrey") +
          geom_line(size = plot_line_size) +
          #geom_point(size = plot_point_size) +
          scale_x_continuous(limits = c(min(pars_terTI()$f),max(pars_terTI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(1, input$Re_terTI^2))) +
          scale_colour_viridis_d(
            option = "plasma", end = 0.9,
            name = "delay &Delta;<sub>1</sub> (days)",
            labels = dayLabels(levels(df$Delta1)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(
            x = "fraction of index cases\nfound and isolated (f)",
            y = "tertiary cases (n<sub>3</sub>)\n&nbsp;") +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "delay Δ<sub>1</sub> (days)")
      })

      # output$region_terTI <- renderPlotly({
      #   df <- data_terTI()$tertiarySummary
      #   df$f <- as.numeric(levels(df$f))[df$f]
      #
      #   plot <- ggplot(
      #     data = df[df$Delta1 >= 0,],
      #     mapping = aes(x = f, y = Delta1, fill = Re, group = Re,
      #                   text = str_c(
      #                     "f = ", scales::percent(f, accuracy = 0.1), "<br>",
      #                     "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days", "<br>"
      #                   )
      #     )) +
      #     geom_area(position = position_identity(), colour = "white") +
      #     scale_x_continuous(limits = c(min(pars_terTI()$f),max(pars_terTI()$f)), labels = scales::percent) +
      #     coord_cartesian(ylim = c(0, max(pars_terTI()$Delta1)), expand = F) +
      #     scale_fill_viridis_d() +
      #     labs(
      #       x = "fraction of index cases\nfound and isolated (f)",
      #       y = "delay Δ<sub>1</sub> (days)\n&nbsp;") +
      #     plotTheme +
      #     theme(legend.position = "none")
      #
      #   makePlotly(plot)
      # })

      output$density_terTI <- renderPlotly({
        df <- data_terTI()$tertiaryCases2
        df$f <- as.numeric(levels(df$f))[df$f]
        df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]

        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = Delta1, group = Re)) +
          geom_raster(aes(fill = tertiaryCases, text = str_c(
            "f = ", scales::percent(f, accuracy = 0.1), "<br>",
            "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days", "<br>",
            "n<sub>3</sub> = ", format(round(tertiaryCases, 2), nsmall = 2)
          )), interpolate = T) +
          #geom_contour(aes(z = tertiaryCases), colour = "black", size = plot_line_size, breaks = 1) +
          scale_fill_gradientn(colours = c("seagreen","white","white", "white","salmon"), name = "tertiary\ncases",
                               values = scales::rescale(c(0,1-.Machine$double.eps,1,1+.Machine$double.eps,max(1.1,input$Re_terTI^2))),
                               limits = c(0,max(1.1,input$Re_terTI^2)),
                               guide = guide_colorbar(title.position = "left", direction = 1)) +
          scale_x_continuous(limits = c(min(pars_terTI()$f),max(pars_terTI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(pars_terTI()$Delta1)), expand = F) +
          labs(
            x = "fraction of index cases\nfound and isolated (f)",
            y = "delay Δ<sub>1</sub> (days)\n&nbsp;") +
          plotTheme

        #' Add n3 == 1 contour
        df2 <- data_terTI()$tertiarySummary
        df2 <- df2[df2$Delta1 >= 0,]
        if (nrow(df2) > 0) {
          df2$f <- as.numeric(levels(df2$f))[df2$f]

          plot <- plot + geom_line(data = df2,
                                   mapping = aes(x = f, y = Delta1, text = str_c(
                                     "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                                     "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days"
                                   )), size = plot_line_size, colour = "black")
        }

        makePlotly(plot, show_legend = T, legend_title = "tertiary\ncases")
      })

      output$caption_terTI <- renderUI({
        Re <- input$Re_terTI

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The number of tertiary cases per index case as a function ",
          "of the testing & isolation coverage f (x-axis) and delay &Delta;<sub>1</sub> (colour).<br>",
          #"<i>(right)</i> Regions in which the expected number of tertiary cases ",
          #"is above one (uncoloured) or below one (coloured). ",
          "<i>(right)</i> The number of tertiary cases (colour) as a function ",
          "of the testing & isolation coverage f (x-axis) and delay &Delta;<sub>1</sub> (y axis). ",
          "Green regions have n<sub>3</sub> < 1, while red regions have n<sub>3</sub> > 1. ",
          "The black line separates these regions (n<sub>3</sub> = 1).<br>",
          "We use <strong>R<sub>e</sub> = {Re}</strong> for the average number ",
          "of secondary infections in the absence of testing and isolating ",
          "(i.e. <i>f</i>=0).",
          "</span>"
        ))
      })

      # secTI: SECONDARY CASES UNDER TESTING AND ISOLATING ----
      pars_secTI <- reactive({
        list(
          Re = input$Re_secTI,
          #f = seq(input$f_secTI[1], input$f_secTI[2], 0.1),
          #Delta1 = seq(input$Delta1_secTI[1], input$Delta1_secTI[2], 1)
          f = seq(0,1,0.05),
          Delta1 = seq(0,4,1)
        )
      })

      data_secTI <- reactive({
        #' Secondary cases as a function of f and Delta_1
        secondaryCases <- getSeconaryCases(paramList = pars_secTI(), infProf = infProf())

        #' Critical region where epidemic is controlled
        paramList <- pars_secTI()
        paramList$f <- seq(min(paramList$f), max(paramList$f), 0.05) #' Finer grid for interpolation
        paramList$Delta1 <- seq(0, max(paramList$Delta1), 0.05) #' Finer grid for interpolation
        c <- getSeconaryCases(paramList = paramList, infProf = infProf())
        #' Compute the threshold between controlled and uncontrolled epidemics (R0 <> 1)
        secondarySummary <- by(c, INDICES = list(c$f, c$Re), function(df) {
          df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]
          Tc <- -Inf
          if (all(df$secondaryCases < 1)) Tc <- Inf
          else if (any(df$secondaryCases < 1) & any(df$secondaryCases > 1)) Tc <- approx(x = df$secondaryCases, y = df$Delta1, xout = 1)$y
          data.frame(
            f = factor(unique(df$f), levels = paramList$f),
            Delta1 = Tc,
            Re = factor(unique(df$Re), levels = paramList$Re)
          )
        })
        secondarySummary <- do.call(rbind, secondarySummary)
        return(list(secondaryCases = secondaryCases, secondaryCases2 = c, secondarySummary = secondarySummary))
      })

      # PLOTS ----
      output$cases_secTI <- renderPlotly({
        df <- data_secTI()$secondaryCases
        df$f <- as.numeric(levels(df$f))[df$f]

        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = secondaryCases, colour = Delta1, group = Delta1,
                        text = str_c(
                          "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                          "Δ<sub>1</sub> = ", dayLabels(Delta1), "<br>",
                          "n<sub>2</sub> = ", format(round(secondaryCases, 2), nsmall = 2)
                        )
          )) +
          geom_hline(yintercept = 1, size = plot_line_size, colour = "darkgrey") +
          geom_line(size = plot_line_size) +
          #geom_point(size = plot_point_size) +
          scale_x_continuous(limits = c(min(pars_secTI()$f),max(pars_secTI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(1, input$Re_secTI))) +
          scale_colour_viridis_d(
            option = "plasma", end = 0.9,
            name = "delay &Delta;<sub>1</sub> (days)",
            labels = dayLabels(levels(df$Delta1)),
            guide = guide_legend(title.position = "left", nrow = 1)
          ) +
          labs(
            x = "fraction of index cases\nfound and isolated (f)",
            y = "secondary cases (n<sub>2</sub>)\n&nbsp;") +
          plotTheme

        makePlotly(plot, show_legend = T, legend_title = "delay Δ<sub>1</sub> (days)")
      })

      # output$region_secTI <- renderPlotly({
      #   df <- data_secTI()$secondarySummary
      #   df$f <- as.numeric(levels(df$f))[df$f]
      #
      #   plot <- ggplot(
      #     data = df[df$Delta1 >= 0,],
      #     mapping = aes(x = f, y = Delta1, fill = Re, group = Re,
      #                   text = str_c(
      #                     "f = ", scales::percent(f, accuracy = 0.1), "<br>",
      #                     "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days", "<br>"
      #                   )
      #     )) +
      #     geom_area(position = position_identity(), colour = "white") +
      #     scale_x_continuous(limits = c(min(pars_secTI()$f),max(pars_secTI()$f)), labels = scales::percent) +
      #     coord_cartesian(ylim = c(0, max(pars_secTI()$Delta1)), expand = F) +
      #     scale_fill_viridis_d() +
      #     labs(
      #       x = "fraction of index cases\nfound and isolated (f)",
      #       y = "delay Δ<sub>1</sub> (days)\n&nbsp;") +
      #     plotTheme +
      #     theme(legend.position = "none")
      #
      #   makePlotly(plot)
      # })

      output$density_secTI <- renderPlotly({
        df <- data_secTI()$secondaryCases2
        df$f <- as.numeric(levels(df$f))[df$f]
        df$Delta1 <- as.numeric(levels(df$Delta1))[df$Delta1]

        plot <- ggplot(
          data = df,
          mapping = aes(x = f, y = Delta1, group = Re)) +
          geom_raster(aes(fill = secondaryCases, text = str_c(
            "f = ", scales::percent(f, accuracy = 0.1), "<br>",
            "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days", "<br>",
            "n<sub>2</sub> = ", format(round(secondaryCases, 2), nsmall = 2)
          )), interpolate = T) +
          #geom_contour(aes(z = secondaryCases), colour = "black", size = plot_line_size, breaks = 1) +
          scale_fill_gradientn(colours = c("seagreen","white","white", "white","salmon"), name = "secondary\ncases",
                               values = scales::rescale(c(0,1-.Machine$double.eps,1,1+.Machine$double.eps,max(1.1,input$Re_secTI))),
                               limits = c(0,max(1.1,input$Re_secTI)),
                               guide = guide_colorbar(title.position = "left", direction = 1)) +
          scale_x_continuous(limits = c(min(pars_secTI()$f),max(pars_secTI()$f)), labels = scales::percent) +
          coord_cartesian(ylim = c(0, max(pars_secTI()$Delta1)), expand = F) +
          labs(
            x = "fraction of index cases\nfound and isolated (f)",
            y = "delay Δ<sub>1</sub> (days)\n&nbsp;") +
          plotTheme

        #' Add n2 == 1 contour
        df2 <- data_secTI()$secondarySummary
        df2 <- df2[df2$Delta1 >= 0,]
        if (nrow(df2) > 0) {
          df2$f <- as.numeric(levels(df2$f))[df2$f]

          plot <- plot + geom_line(data = df2,
                                   mapping = aes(x = f, y = Delta1, text = str_c(
                                     "f = ", scales::percent(f, accuracy = 0.1), "<br>",
                                     "Δ<sub>1</sub> = ", format(round(Delta1, 2), nsmall = 2), " days"
                                   )), size = plot_line_size, colour = "black")
        }

        makePlotly(plot, show_legend = T, legend_title = "secondary\ncases")
      })

      output$caption_secTI <- renderUI({
        Re <- input$Re_secTI

        HTML(glue(
          "<span class='help-block' style='font-size:15px;'>",
          "<i>(left)</i> The number of secondary cases per index case as a function ",
          "of the testing & isolation coverage f (x-axis) and delay &Delta;<sub>1</sub> (colour).<br>",
          #"<i>(right)</i> Regions in which the expected number of secondary cases ",
          #"is above one (uncoloured) or below one (coloured). ",
          "<i>(right)</i> The number of secondary cases (colour) as a function ",
          "of the testing & isolation coverage f (x-axis) and delay &Delta;<sub>1</sub> (y axis). ",
          "Green regions have n<sub>2</sub> < 1, while red regions have n<sub>2</sub> > 1. ",
          "The black line separates these regions (n<sub>2</sub> = 1).<br>",
          "We use <strong>R<sub>e</sub> = {Re}</strong> for the average number ",
          "of secondary infections in the absence of testing and isolating ",
          "(i.e. <i>f</i>=0).",
          "</span>"
        ))
      })
    }
  )
}

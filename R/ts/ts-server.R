library(rlang)
library(stats)


db_config <- config::get("database")
ts_data_store <- get_postgresql_datastore(
  host = db_config$host,
  port = db_config$port,
  username = db_config$username,
  password = db_config$password,
  dbname = db_config$dbname
)
tsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      ### ---------- Basic Initializations ----------

      observe({
        toExclude <- setdiff(names(input), "tab")
        setBookmarkExclude(toExclude)
      })

      initial_model <- list(
        general = list(
          event = "Positive test",
          display_prob = FALSE,
          given = "Positive test"
        ),
        filter = list(),
        plot_type = "histogram",
        display = list(
          log_scale = FALSE,
          stack_histograms = TRUE,
          show_confidence_interval = TRUE,
          granularity = "Days",
          smoothing_window = "None"
        ),
        normalization = list(
          selected = FALSE,
          timerange = '2020-07-01'
        )
      )
      for (filter_name in names(basicFilters)) {
        f <- basicFilters[[filter_name]]
        initial_model$filter[[filter_name]] <- list(
          selected = f$choices,
          compare = FALSE,
          compare_per_100k_people = FALSE,
          compare_proportions = FALSE
        )
      }
      model_container <- reactiveValues(model = initial_model)

      # Initialize the filter sub modules
      basicFilterServers <- basicFilters %>% map2(names(.), function (f, n) { f$server(n) })

      # Reset all
      observeEvent(input$resetAll, {
        model_container$model <- initial_model
      })

      observeEvent(model_container$model, {
        current_model <- model_container$model

        # Ignore the map_selected_day setting as it is often invisible and not relevant
        current_model$map_selected_day <- NULL

        if (!identical(initial_model, current_model)) {
          shinyjs::show("resetAll")
        } else {
          shinyjs::hide("resetAll")
        }
      })

      # ---------- General parameter ----------

      get_parameter_value <- function () {
        return(list(
          event = input$event,
          display_prob = input$display_prob,
          given = input$given
        ))
      }

      general_view_model <- NULL

      set_general_view_model <- function (view_model) {
        # The first time this function is called is during the initialization. The default view defined in ui.R is
        # propagated back and there is no need for any updates.
        if (is.null(general_view_model)) {
          general_view_model <<- view_model
          return()
        }

        new <- view_model
        old <- general_view_model

        set_radio_view_model(session, "event", new$event, old$event)
        set_checkbox_view_model(session, "display_prob", new$display_prob, old$display_prob)
        set_radio_view_model(session, "given", new$given, old$given)

        general_view_model <<- new
      }

      observeEvent({
        input$event
        input$display_prob
        input$given
        TRUE
      }, {
        # Update view model
        general_view_model$event$selected <<- input$event
        general_view_model$display_prob$selected <<- input$display_prob
        general_view_model$given$selected <<- input$given

        # Update model
        previous_value <- model_container$model$general
        new_value <- get_parameter_value()
        if (identical(previous_value, new_value)) {
          return()
        }
        new_model <- model_container$model
        new_model$general <- new_value

        # Set an allowed plot type if the current plot type does not fit.
        available_plot_types <- get_available_plot_types(new_model)
        if (!(new_model$plot_type %in% available_plot_types)) {
          new_model$plot_type <- available_plot_types[[1]]
        }

        model_container$model <<- new_model
      })


      # ---------- Filter ----------
      lapply(
        X = basicFilterServers,
        FUN = function(fs){
          observeEvent(fs()$value, {
            fs_result <- fs()
            fs_name <- fs_result$attributeName
            previous_value <- model_container$model$filter[[fs_name]]
            new_value <- fs_result$value
            new_model <- model_container$model
            if (identical(previous_value, new_value)) {
              return()
            }
            new_model$filter[[fs_name]] <- new_value

            # If one compare checkbox is checked, the others should be unchecked.
            if (fs_result$value$compare) {
              for (fs2_name in names(new_model$filter)) {
                if (fs2_name != fs_name) {
                  new_model$filter[[fs2_name]]$compare <- FALSE
                }
              }
            }

            # Set an allowed plot type if the current plot type does not fit.
            available_plot_types <- get_available_plot_types(new_model)
            if (!(new_model$plot_type %in% available_plot_types)) {
              new_model$plot_type <- available_plot_types[[1]]
            }

            model_container$model <<- new_model
          })
        }
      )

      # Date slider for the map
      map_selected_day_model <- reactiveValues(model = list(
        display_prob = FALSE,
        granularity = "Days"
      ))
      observeEvent(model_container$model, {
        map_model <- map_selected_day_model$model
        global_model <- model_container$model

        shinyjs::toggleCssClass("map_slider", "ts-hidden", condition = "map" != global_model$plot_type)
        if (global_model$general$display_prob != map_model$display_prob ||
          global_model$display$granularity != map_model$granularity) {
          map_model <- list(
            display_prob = global_model$general$display_prob,
            granularity = global_model$display$granularity
          )
          map_selected_day_model$model <<- map_model
        }
      })
      output$map_selected_day_output <- renderUI({
        display_prob <- map_selected_day_model$model$display_prob
        granularity <- map_selected_day_model$model$granularity
        if (display_prob || granularity == "Days") {
          timeFormat <- "%F"
          step <- 1
        } else if (granularity == "Weeks") {
          timeFormat <- "%F (week %W)"
          step <- 7
        } else if (granularity == "Months") {
          timeFormat <- "%b %Y"
          step <- 30  # TODO 30 != month
        }
        sliderInput(
          inputId = ns("map_selected_day"), "Date:", min = as.Date("2020-03-01"), max = today(),
          value = today() %m-% days(7), width = "100%",
          timeFormat = timeFormat, step = step,
          animate = animationOptions(
            interval = 3000, loop = TRUE,
            playButton = HTML(
              '<span style="color: black; font-weight: bold;"><i class="glyphicon glyphicon-play"></i> Run animation</span>'
            )
          ),
        )
      })
      outputOptions(output, "map_selected_day_output", suspendWhenHidden = FALSE)

      observeEvent(input$map_selected_day, {
        model_container$model$map_selected_day <<- input$map_selected_day
      })


      # ---------- Plot type ----------
      observeEvent(input$plotTypeHistogram, { model_container$model$plot_type <- "histogram" })
      observeEvent(input$plotTypeLine, { model_container$model$plot_type <- "line" })
      observeEvent(input$plotTypeArea, { model_container$model$plot_type <- "area" })
      observeEvent(input$plotTypeMap, { model_container$model$plot_type <- "map" })

      available_plot_types_view_model <- NULL

      set_available_plot_types_view_model <- function (view_model) {
        new <- view_model
        old <- available_plot_types_view_model

        set_action_btn_view_model(session, "plotTypeHistogram", new$histogram, old$histogram)
        set_action_btn_view_model(session, "plotTypeLine", new$line, old$line)
        set_action_btn_view_model(session, "plotTypeArea", new$area, old$area)
        set_action_btn_view_model(session, "plotTypeMap", new$map, old$map)

        available_plot_types_view_model <<- new
      }


      # ---------- Display options ----------
      get_display_value <- function () {
        return(list(
          log_scale = input$log_scale,
          stack_histograms = input$stack_histograms,
          show_confidence_interval = input$show_confidence_interval,
          granularity = input$granularity,
          smoothing_window = input$smoothing_window
        ))
      }

      display_view_model <- NULL
      set_display_view_model <- function (view_model) {
        # The first time this function is called is during the initialization. The default view defined in ui.R is
        # propagated back and there is no need for any updates.
        if (is.null(display_view_model)) {
          display_view_model <<- view_model
          return()
        }

        new <- view_model
        old <- display_view_model

        set_checkbox_view_model(session, "log_scale", new$log_scale, old$log_scale)
        set_checkbox_view_model(session, "stack_histograms", new$stack_histograms, old$stack_histograms)
        set_checkbox_view_model(session, "show_confidence_interval", new$show_confidence_interval,
                                old$show_confidence_interval)
        set_radio_view_model(session, "granularity", new$granularity, old$granularity)
        set_radio_view_model(session, "smoothing_window", new$smoothing_window, old$smoothing_window)

        display_view_model <<- new
      }

      observeEvent({
        input$log_scale
        input$stack_histograms
        input$show_confidence_interval
        input$granularity
        input$smoothing_window
        TRUE
      }, {
        # Update view model
        display_view_model$log_scale$selected <<- input$log_scale
        display_view_model$stack_histograms$selected <<- input$stack_histograms
        display_view_model$show_confidence_interval$selected <<- input$show_confidence_interval
        display_view_model$granularity$selected <<- input$granularity
        display_view_model$smoothing_window$selected <<- input$smoothing_window

        # Update model
        previous_value <- model_container$model$display
        new_value <- get_display_value()
        new_model <- model_container$model
        if (identical(previous_value, new_value)) {
          return()
        }
        new_model$display <- new_value
        model_container$model <<- new_model
      })

      # ---------- Normalization ----------
      get_normalization_value <- function () {
        return(list(
          selected = input$normalization,
          timerange = input$normalization_timerange
        ))
      }

      normalization_view_model <- NULL
      set_normalzation_view_model <- function (view_model) {
        # The first time this function is called is during the initialization. The default view defined in ui.R is
        # propagated back and there is no need for any updates.
        if (is.null(normalization_view_model)) {
          normalization_view_model <<- view_model
          return()
        }

        new <- view_model
        old <- normalization_view_model

        set_checkbox_view_model(session, "normalization", new$selected, old$selected)
        set_picker_view_model(session, "normalization_timerange", new$timerange, old$timerange)

        normalization_view_model <<- new
      }

      observeEvent({
        input$normalization
        input$normalization_timerange
        TRUE
      }, {
        # Update view model
        normalization_view_model$selected$selected <<- input$normalization
        normalization_view_model$timerange$selected <<- input$normalization_timerange

        # Update model
        previous_value <- model_container$model$normalization
        new_value <- get_normalization_value()
        if (identical(previous_value, new_value)) {
          return()
        }

        new_model <- model_container$model
        new_model$normalization <- new_value

        # If the normalization is activated, nothing can be compared and only positive tests should be shown.
        if (new_value$selected) {
          new_model$general$event <- "Positive test"
          new_model$general$display_prob <- FALSE
          for (filter_name in names(new_model$filter)) {
            new_model$filter[[filter_name]]$compare <- FALSE
          }
        }

        model_container$model <<- new_model
      })


      # ---------- Apply model back to the view model ----------
      set_view_model <- function (view_model) {
        set_general_view_model(view_model$general)
        for (fs in basicFilterServers) {
          fs_result <- fs()
          fs_name <- fs_result$attributeName
          fs_result$set_view_model(view_model$filter[[fs_name]])
        }
        set_available_plot_types_view_model(view_model$available_plot_types)
        set_display_view_model(view_model$display)
        set_normalzation_view_model(view_model$normalization)
      }

      observeEvent(model_container$model, {
        model <- model_container$model
        view_model <- get_view_model_from_model(model_container$model)
        set_view_model(view_model)
      })


      # ---------- Output ----------
      output$mainPlot <- renderPlotly({
        model <- model_container$model
        plot_def <- compute_plot_data(model, ts_data_store)

        validate(
          need(
            is.null(plot_def$error),
            plot_def$error$message
          )
        )

        plotlyPlot <- finalize_plot(plot_def, model)
        return(plotlyPlot)
      })

      output$dataLastUpdatedAt <- renderText({
        dashboard_state <- ts_data_store$load_dashboard_state() %>% collect()
        return (paste("Data last updated on", date_format("%d.%m.%Y")(dashboard_state$last_data_update)))
      })
    }
  )
}

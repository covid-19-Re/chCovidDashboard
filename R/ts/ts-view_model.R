get_view_model_from_model <- function (model) {
  view_model <- list(
    general = list(
      event = list(
        selected = model$general$event,
        choices = ts_constants$events,
        enabled = !model$normalization$selected
      ),
      display_prob = list(
        selected = model$general$display_prob,
        enabled = !model$normalization$selected
      ),
      given = list(
        selected = model$general$given,
        choices = ts_constants$events,
        enabled = !model$normalization$selected && model$general$display_prob
      )
    ),
    filter = list(),
    available_plot_types = NULL,
    display = list(
      log_scale = list(
        selected = model$display$log_scale,
        enabled = TRUE
      ),
      stack_histograms = list(
        selected = model$display$stack_histograms,
        enabled = model$plot_type == "histogram"
      ),
      show_confidence_interval = list(
        selected = model$display$show_confidence_interval,
        enabled = model$plot_type == "line" && model$general$display_prob
      ),
      granularity = list(
        selected = model$display$granularity,
        choices = ts_constants$granularityChoices,
        enabled = model$plot_type == "histogram" || model$plot_type == "map"
      ),
      smoothing_window = list(
        selected = model$display$smoothing_window,
        choices = ts_constants$slidingWindowChoices,
        enabled = model$plot_type == "line" || model$plot_type == "area"
      )
    ),
    normalization = list(
      selected = list(
        selected = model$normalization$selected,
        enabled = TRUE
      ),
      timerange = list(
        selected = model$normalization$timerange,
        choices = ts_constants$normalizationTimerangeOptions,
        enabled = model$normalization$selected
      )
    )
  )
  for (filter_name in names(model$filter)) {
    filter_model <- model$filter[[filter_name]]
    view_model$filter[[filter_name]] <- list(
      picker = list(
        selected = filter_model$selected,
        choices = ts_constants[[filter_name]],
        enabled = TRUE
      ),
      compare = list(
        selected = filter_model$compare,
        enabled = !model$normalization$selected
      ),
      compare_per_100k_people = list(
        selected = filter_model$compare_per_100k_people,
        enabled = filter_model$compare && !filter_model$compare_proportions && !model$general$display_prob
      ),
      compare_proportions = list(
        selected = filter_model$compare_proportions,
        enabled = filter_model$compare && !filter_model$compare_per_100k_people
      )
    )
  }
  view_model$available_plot_types <- list(
    histogram = list(
      enabled = FALSE
    ),
    line = list(
      enabled = FALSE
    ),
    area = list(
      enabled = FALSE
    ),
    map = list(
      enabled = FALSE
    )
  )
  for (p in get_available_plot_types(model)) {
    view_model$available_plot_types[[p]]$enabled <- TRUE
  }
  return(view_model)
}


#' This function sets the view model of a picker input. If the previous view model is provided, changes will be detected
#' to reduce the number of view updates.
#'
#' @examples
#' set_picker_view_model(
#'   session,
#'   "my_picker",
#'   list(
#'     choices = c("Apple", "Orange"),
#'     selected = "Orange",
#'     enabled = TRUE
#'   )
#' )
set_picker_view_model <- function (session, id, view_model, old_view_model = NULL) {
  ns <- session$ns
  new <- view_model
  old <- old_view_model

  if (!identical(new$choices, old$choices) || !identical(new$selected, old$selected)) {
    shinyWidgets::updatePickerInput(session, inputId = id, choices = new$choices, selected = new$selected)
  }
  if (!identical(new$enabled, old$enabled)) {
    disabled_js <- if (new$enabled) "false" else "true"
    shinyjs::runjs(
      paste0("jQuery(\"[data-id='", ns(id), "'\").prop(\"disabled\", ", disabled_js, ")"))
  }
}


#' This function sets the view model of a checkbox input. If the previous view model is provided, changes will be
#' detected to reduce the number of view updates.
#'
#' @examples
#' set_checkbox_view_model(
#'   session,
#'   "my_checkbox",
#'   list(
#'     selected = TRUE,
#'     enabled = TRUE
#'   )
#' )
set_checkbox_view_model <- function (session, id, view_model, old_view_model = NULL) {
  new <- view_model
  old <- old_view_model

  if (!identical(new$selected, old$selected)) {
    shiny::updateCheckboxInput(session, id, value = new$selected)
  }
  if (!identical(new$enabled, old$enabled)) {
    shinyjs::toggleState(id = session$ns(id), condition = new$enabled, asis = TRUE)
  }
}


#' This function sets the view model of a radio button input. If the previous view model is provided, changes will be
#' detected to reduce the number of view updates.
#'
#' @examples
#' set_radio_view_model(
#'   session,
#'   "my_radio_button",
#'   list(
#'     selected = "Orange",
#'     choices = c("Apple", "Orange"),
#'     enabled = TRUE
#'   )
#' )
set_radio_view_model <- function (session, id, view_model, old_view_model = NULL) {
  new <- view_model
  old <- old_view_model

  if (!identical(new$choices, old$choices) || !identical(new$selected, old$selected)) {
    # TODO updateRadioButtons enables the radio buttons.
    shiny::updateRadioButtons(session, inputId = id, choices = new$choices, selected = new$selected, inline = TRUE)
  }
  if (!identical(new$enabled, old$enabled)) {
    # Because updateRadioButtons would enable the radio buttons again, the delay is aimed to achieve that the
    # toggleState command is performed after updateRadioButtons. It seems to work but it is unclear if it will always
    # work.
    shinyjs::delay(100, shinyjs::toggleState(id = session$ns(id), condition = new$enabled, asis = TRUE))
  }
}


#' This function sets the view model of an action button. If the previous view model is provided, changes will be
#' detected to reduce the number of view updates.
#'
#' @examples
#' set_action_btn_view_model(
#'   session,
#'   "my_action_button",
#'   list(
#'     enabled = TRUE
#'   )
#' )
set_action_btn_view_model <- function (session, id, view_model, old_view_model = NULL) {
  new <- view_model
  old <- old_view_model

  if (!identical(new$enabled, old$enabled)) {
    shinyjs::toggleState(id = session$ns(id), condition = new$enabled, asis = TRUE)
  }
}

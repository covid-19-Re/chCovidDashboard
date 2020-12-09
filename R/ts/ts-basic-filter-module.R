#' Creates a shiny module containing a basic filter. This module is not independent: it is not in control of an own
#' model or view model but solely serves as a small helper.
#'
#' @param label (character[length=1])
#' @param choices (character)
#' @param attributeName (character[length=1])
#' @param description (character[length=1])
#' @param customFilterFunctionBuilder
#' @param customIsFilteringFunctionBuilder
#' @param customGetComparisonGroupsFunctionBuilder
#' @param customGetEntriesOfGroupFunctionBuilder
#' @param comparePer100kPeoplePossible
#' @return
create_basic_filter <- function (
  label, choices, attributeName,
  description = "",
  customFilterFunctionBuilder = NULL,
  customIsFilteringFunctionBuilder = NULL,
  customGetComparisonGroupsFunctionBuilder = NULL,
  customGetEntriesOfGroupFunctionBuilder = NULL,
  comparePer100kPeoplePossible = FALSE
) {

  ui <- function (id) {
    ns <- NS(id)

    tagList(
      pickerInput(
        inputId = ns("picker"),
        label = label,
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(size = 8)
      ),
      actionGroupButtons(
        inputIds = c(ns("all"), ns("clear")),
        labels = c("Select all", "Clear"),
        size = "xs"
      ),
      tags$div(
        tags$small(description)
      ),
      checkboxInput(
        inputId = ns("compare"),
        label = tagList(
          "Compare",
          tooltip(glue::glue("If selected, the numbers for each {label} will be plotted."))
        )
      ),
      tags$div(
        checkboxInput(
          inputId = ns("compare_per_100k_people"),
          label = tagList(
            "Compare number events per 100,000 people"
          )
        ),
        style = if (!comparePer100kPeoplePossible) "display: none;" else ""
      ),
      checkboxInput(
        inputId = ns("compare_proportions"),
        label = tagList(
          "Show proportions",
          tooltip(glue::glue("If selected, the plot shows the percentage for each {label}. It is only available if
          Compare is selected."))
        )
      )
    )
  }

  server <- function (id) {
    moduleServer(
      id,
      function (input, output, session) {

        view_model <- NULL

        set_view_model <- function (new_view_model) {
          # The first time this function is called is during the initialization. The default view defined in ui.R is
          # propagated back and there is no need for any updates.
          if (is.null(view_model)) {
            view_model <<- new_view_model
            return()
          }

          new <- new_view_model
          old <- view_model

          set_picker_view_model(session, "picker", new$picker, old$picker)
          set_checkbox_view_model(session, "compare", new$compare, old$compare)
          set_checkbox_view_model(session, "compare_per_100k_people", new$compare_per_100k_people,
                                  old$compare_per_100k_people)
          set_checkbox_view_model(session, "compare_proportions", new$compare_proportions, old$compare_proportions)
          view_model <<- new
        }

        observeEvent(input$all, {
          vm <- view_model
          vm$picker$selected <- choices
          set_view_model(vm)
        })

        observeEvent(input$clear, {
          vm <- view_model
          vm$picker$selected <- character(0)
          set_view_model(vm)
        })

        return (eventReactive({
          input$picker
          input$compare
          input$compare_per_100k_people
          input$compare_proportions
          TRUE
        }, {
          # Update view model
          view_model$picker$selected <<- input$picker
          view_model$compare$selected <<- input$compare
          view_model$compare_per_100k_people$selected <<- input$compare_per_100k_people
          view_model$compare_proportions$selected <<- input$compare_proportions

          # Inform parent module (who holds the model)
          result <- list(
            attributeName = attributeName,
            value = list(
              selected = input$picker,
              compare = input$compare,
              compare_per_100k_people = input$compare_per_100k_people,
              compare_proportions = input$compare_proportions
            ),
            set_view_model = set_view_model
          )
          return(result)
        }))
      }
    )
  }

  if (is.null(customFilterFunctionBuilder)) {
    filter_data <- function (data, model) {
      selected <- model$selected
      if (length(selected) < length(choices)) {
        return (data %>% filter(!!as.symbol(attributeName) %in% selected))
      }
      return(data)
    }
  } else {
    filter_data <- customFilterFunctionBuilder()
  }

  if (is.null(customIsFilteringFunctionBuilder)) {
    is_filtering <- function (model) {
      selected <- model$filter[[attributeName]]$selected
      return(length(selected) < length(choices))
    }
  } else {
    is_filtering <- customIsFilteringFunctionBuilder()
  }

  if (is.null(customGetComparisonGroupsFunctionBuilder)) {
    get_comparison_groups <- function (data) {
      return (unique(data[[attributeName]]))
    }
  } else {
    result$get_comparison_groups <- customGetComparisonGroupsFunctionBuilder()
  }

  if (is.null(customGetEntriesOfGroupFunctionBuilder)) {
    get_entries_of_group <- function(data, group) {
      return (filter(data, !!as.symbol(attributeName) == group))
    }
  } else {
    get_entries_of_group <- customGetEntriesOfGroupFunctionBuilder()
  }

  return (list(
    ui = ui,
    server = server,
    label = label,
    filter = filter_data,
    is_filtering = is_filtering,
    get_comparison_groups = get_comparison_groups,
    get_entries_of_group = get_entries_of_group
  ))
}

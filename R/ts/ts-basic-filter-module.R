#' Creates a shiny module containing a basic filter
#'
#' @param label (character[length=1])
#' @param choices (character)
#' @param attributeName (character[length=1])
#' @return (list: ui, server)
createBasicFilter <- function (
  label, choices, attributeName,
  description = "",
  customFilterFunctionBuilder = NULL,
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

        observeEvent(input$all, {
          updatePickerInput(session,
            inputId = "picker",
            selected = choices
          )
        })

        observeEvent(input$clear, {
          updatePickerInput(session,
            inputId = "picker",
            selected = character(0)
          )
        })

        observe({
          shinyjs::toggleState(id = "compare_proportions", condition = input$compare && !input$compare_per_100k_people)
          shinyjs::toggleState(id = "compare_per_100k_people", condition = input$compare && !input$compare_proportions)
        })

        return (reactive({
          result <- list()

          validate(
            need(
              !is.null(input$picker),
              paste0("Must specify at least one '", label, "'.")
            )
          )

          if (is.null(customFilterFunctionBuilder)) {
            result$filter <- function (data) {
              if (length(input$picker) < length(choices)) {
                return (data %>% filter(!!as.symbol(attributeName) %in% input$picker))
              }
              return (data)
            }
          } else {
            result$filter <- customFilterFunctionBuilder(input, output, session)
          }

          if (is.null(customGetComparisonGroupsFunctionBuilder)) {
            result$getComparisonGroups <- function (data) {
              return (unique(data[[attributeName]]))
            }
          } else {
            result$getComparisonGroups <- customGetComparisonGroupsFunctionBuilder(input, output, session)
          }

          if (is.null(customGetEntriesOfGroupFunctionBuilder)) {
            result$getEntriesOfGroup <- function(data, group) {
              return (filter(data, !!as.symbol(attributeName) == group))
            }
          } else {
            result$getEntriesOfGroup <- customGetEntriesOfGroupFunctionBuilder(input, output, session)
          }

          result$label <- label
          result$session <- session
          result$attributeName <- attributeName

          result$isFiltering <- length(input$picker) < length(choices)
          result$compare <- input$compare
          result$comparePer100kPeople <- input$compare && input$compare_per_100k_people
          result$compareProportions <- input$compare && input$compare_proportions

          return (result)
        }))
      }
    )
  }

  return (list(ui = ui, server = server))
}

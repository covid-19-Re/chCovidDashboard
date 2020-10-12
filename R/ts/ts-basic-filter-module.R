#' Creates a shiny module containing a basic filter
#'
#' @param label (character[length=1])
#' @param choices (character)
#' @param attributeName (character[length=1])
#' @return (list: ui, server)
createBasicFilter <- function (label, choices, attributeName) {

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
      checkboxInput(
        inputId = ns("compare"),
        label = "Compare"
      ),
      checkboxInput(
        inputId = ns("compare_proportions"),
        label = "Show proportions"
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

        observeEvent(input$compare, {
          shinyjs::toggleState(id = "compare_proportions", condition = input$compare)
        })

        return (reactive({
          result <- list()

          validate(
            need(
              !is.null(input$picker),
              paste0("Must specify at least one '", label, "'.")
            )
          )

          result$filter <- function (data) {
            if (length(input$picker) < length(choices)) {
              return (data %>% filter(!!as.symbol(attributeName) %in% input$picker))
            }
            return (data)
          }

          result$session <- session
          result$attributeName <- attributeName

          result$isFiltering <- length(input$picker) < length(choices)
          result$compare <- input$compare
          result$compareProportions <- input$compare && input$compare_proportions

          return (result)
        }))
      }
    )
  }

  return (list(ui = ui, server = server))
}

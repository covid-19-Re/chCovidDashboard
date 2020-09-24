# bootstrap panel for shiny
bootstrapPanel <- function(heading, class = "panel-primary", ...) {
  div(class = str_c("panel", class, sep = " "),
    div(class = "panel-heading", heading),
    div(class = "panel-body",
      ...
    )
  )
}

# shiny input label with helptext
extLabel <- function(name, helptext, tooltip = NULL, tooltipPlacement = "right") {
  extLabel <- glue::glue(
    "<b style='font-family:serif; font-size:15px'>{name}</b><span class='helptext' style='font-size:15px'>{helptext}</span>")

  if (!is.null(tooltip)) {
    tooltip <- glue::glue("<i class='fa fa-question-circle fa-fw ttIndicator'",
      "data-toggle='tooltip' data-html='true' data-placement='{tooltipPlacement}' title='{tooltip}'></i>")
  }

  return(HTML(str_c(extLabel, tooltip)))
}

sourceLink <- function(citation, doi) (
  sourceLink <- HTML(
    glue::glue(
      "<small><a href=' https://doi.org/{doi}' target='_blank'>
        <i class='fa fa-book-open fa-fw'></i>
        {citation}
      </a></small>"
    )
  )
)

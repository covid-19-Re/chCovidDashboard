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
extLabel <- function(name, helptext) {
  extLabel <- HTML(
    glue::glue("<b>{name}</b><span class='helptext'>{helptext}</span>"))

  return(extLabel)
}

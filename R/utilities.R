# bootstrap panel for shiny
bootstrapPanel <- function(heading, class = "panel-primary", ..., id = NULL) {
  div(
    class = str_c("panel", class, sep = " "), id = id,
    div(class = "panel-heading", heading),
    div(
      class = "panel-body",
      ...
    )
  )
}

# bootstrap panel for shiny
thumbnailPanel <- function(title, authors, affiliation, thumbnail, tabId) {
  div(
    class = "col-sm-3", style = "width: 300px; height: 300px;",
    div(
      class = "panel panel-primary",
      div(
        class = "panel-body",
        img(src = thumbnail, width = "100%", class = "thumbImg"),
        div(
          class = "thumbText",
          tags$a(
            id = tabId, href = "#", class = "action-button shiny-bound-input",
            HTML("<p class = 'thumbTitle'>", title, "</p>")
          ),
          HTML("<p style='font-size:15px'>", authors, "</p>"),
          HTML("<p style='font-size:12px'>", affiliation, "</p>")
        )
      )
    )
  )
}

thumbnailPanelExt <- function(title, authors, affiliation, thumbnail, href) {
  div(
    class = "col-sm-3", style = "width: 300px; height: 300px;",
    div(
      class = "panel panel-primary",
      div(
        class = "panel-body",
        img(src = thumbnail, width = "100%", class = "thumbImg"),
        div(
          class = "thumbText",
          tags$a(
            href = href, target = "_blank",
            HTML("<p class = 'thumbTitle'>", title, "</p>")
          ),
          HTML("<p style='font-size:15px'>", authors, "</p>"),
          HTML("<p style='font-size:12px'>", affiliation, "</p>")
        )
      )
    )
  )
}

# shiny input label with helptext
extLabel <- function(name, helptext, tooltip = NULL, tooltipPlacement = "right") {
  extLabel <- glue::glue(
    "<b style='font-family:serif; font-size:15px'>{name}</b>",
    "<span class='helptext' style='font-size:15px'>{helptext}</span>"
  )

  if (!is.null(tooltip)) {
    tooltip <- glue::glue(
      "<i class='fa fa-question-circle fa-fw ttIndicator'",
      "data-toggle='tooltip' data-html='true' data-placement='{tooltipPlacement}' title='{tooltip}'></i>"
    )
  }

  return(HTML(str_c(extLabel, tooltip)))
}

sourceLink <- function(citation, doi) {
  (
    sourceLink <- HTML(
      glue::glue(
        "<small><a href=' https://doi.org/{doi}' target='_blank'>
        <i class='fa fa-book-open fa-fw'></i>
        {citation}
      </a></small>"
      )
    )
  )
}

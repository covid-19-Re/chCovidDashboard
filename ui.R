library("shinyjs")

ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    useShinyjs(),
    navbarPage("CH Covid-19 Dashboard", id = "tab", theme = "theme.min.css", collapsible = TRUE,
      tabPanel("",  value = "home", icon = icon("home"),
        fluidRow(
          thumbnailPanel(
            title = "Time Series of Cases",
            authors = "Timothy Vaughan, Tanja Stadler",
            affiliation = "Computational Evolution Group, D-BSSE, ETH Zurich, Switzerland",
            thumbnail = "tsCases-thumbnail.png",
            tabId = "selectTsCases"
          ),
          thumbnailPanel(
            title = "Relative frequency of clinical events",
            authors = "Timothy Vaughan, Tanja Stadler",
            affiliation = "Computational Evolution Group, D-BSSE, ETH Zurich, Switzerland",
            thumbnail = "tsProportions-thumbnail.png",
            tabId = "selectTsProportions"
          ),
          thumbnailPanel(
            title = "Effectiveness of Contact Tracing",
            authors = "Peter Ashcroft",
            affiliation = "Institute of Integrative Biology, ETH Zurich, Switzerland",
            thumbnail = "ct-thumbnail.png",
            tabId = "selectContactTracing"
          ),
          thumbnailPanel(
            title = "Quantifying the impact of quarantine duration on COVID-19 transmission",
            authors = "Peter Ashcroft, Sonja Lehtinen, and Sebastian Bonhoeffer",
            affiliation = "Institute of Integrative Biology, ETH Zurich, Switzerland",
            thumbnail = "quarantineModule-thumbnail.png",
            tabId = "selectQuarantine"
          )
        ),
        hr(),
        fluidRow(
          thumbnailPanelExt(
            title = "R<sub>e</sub> Estimation",
            authors = "Jérémie Scire, Jana S. Huisman et al.",
            affiliation = "ETH Zürich, D-BSSE & D-USYS <br><br> <i>(opens in a new window)</i>",
            thumbnail = "re-thumbnail.png",
            href = "https://ibz-shiny.ethz.ch/covid-19-re/"
          )
        ),
      ),
      tabPanel("Time series - cases", value = "tsCases",
        tsCasesUI("tsCases")
      ),
      tabPanel("Time series - proportions", value = "tsProportions",
        tsProportionsUI("tsProportions")
      ),
      tabPanel("Contact tracing", value = "contactTracing",
        ctUI("contactTracing")
      ),
      tabPanel("Quarantine duration", value = "quarantineDuration",
        quarantineDurationUI("quarantineDuration")
      ),
      navbarMenu("About",
        tabPanel("About", icon = icon("question-circle"), value = "about",
          includeMarkdown("README.md")
        )
      )
    ),
    tags$script(src = "navbarRight.js")
)}

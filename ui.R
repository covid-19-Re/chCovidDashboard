library("shinyjs")

ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    useShinyjs(),
    navbarPage("CH Covid-19 Dashboard",
      id = "tab", theme = "theme.min.css", collapsible = TRUE,
      tabPanel("",
        value = "home", icon = icon("home"),
        fluidRow(
          thumbnailPanel(
            title = "Quantifying the impact of quarantine duration on COVID-19 transmission",
            authors = "Peter Ashcroft, Sonja Lehtinen, Daniel Angst, Nicola Low and Sebastian Bonhoeffer",
            affiliation = "ETH Zurich & ISPM Universität Bern",
            thumbnail = "quarantineModule-thumbnail.png",
            tabId = "selectQuarantine"
          ),
          thumbnailPanel(
            title = "Time Series",
            authors = "Timothy Vaughan, Tanja Stadler",
            affiliation = "Computational Evolution Group, D-BSSE, ETH Zurich, Switzerland",
            thumbnail = "ts-thumbnail.png",
            tabId = "selectTs"
          ),
          thumbnailPanel(
            title = "Analyzing epidemic trends of SARS-CoV-2 in Switzerland",
            authors = "Nanina Anderegg, Julien Riou, Christian L. Althaus",
            affiliation = "Institute of Social and Preventive Medicine, Universität Bern, Switzerland<br><br><i>(preliminary)</i>",
            thumbnail = "trends-thumbnail.png",
            tabId = "selectTrends"
          ),
          thumbnailPanel(
            title = "Effectiveness of Contact Tracing",
            authors = "Peter Ashcroft, Sonja Lehtinen, and Sebastian Bonhoeffer",
            affiliation = "Institute of Integrative Biology, ETH Zurich, Switzerland<br><br><i>(preliminary)</i>",
            thumbnail = "ct-thumbnail.png",
            tabId = "selectContactTracing"
          ),
          thumbnailPanel(
            title = "Live time-series analysis to monitor and forecast the COVID-19 outbreak in Switzerland",
            authors = "Monica Golumbeanu and Melissa Penny",
            affiliation = "Swiss Tropical and Public Health Institute and University of Basel<br><i>(preliminary)</i>",
            thumbnail = "forecastModule-thumbnail.png",
            tabId = "selectForecast"
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
          ),
          thumbnailPanelExt(
            title = "nextstrain: Phylogenetic analysis of Swiss SARS-CoV-2 genomes in their international context",
            authors = "maintained by Emma Hodcroft, Richard Neher, Sarah Nadeau and Tanja Stadler.",
            affiliation = "<i>(opens in a new window)</i>",
            thumbnail = "nextstrain-thumbnail.png",
            href = "https://nextstrain.org/groups/swiss/ncov/switzerland"
          ),
          thumbnailPanelExt(
            title = "icumonitoring.ch",
            authors = str_c("Cheng Zhao, Nicola Criscuolo, Burcu Tepekule, Monica Golumbeanu, Melissa Penny,
              Peter Ashcroft, Thomas Van Boeckel"),
            affiliation = "ETH Zürich, Swiss TPH, Universitätsspital Zürich <br><br><i>(opens in a new window)</i>",
            thumbnail = "icumonitoring-thumbnail.png",
            href = "https://icumonitoring.ch"
          )
        ),
      ),
      tabPanel("Time Series",
        value = "ts",
        tsUI("ts")
      ),
      tabPanel("Trends",
        value = "trends",
        trendsUI("trends")
      ),
      tabPanel("Contact tracing",
        value = "contactTracing",
        ctUI("contactTracing")
      ),
      tabPanel("Quarantine duration",
        value = "quarantineDuration",
        quarantineDurationUI("quarantineDuration")
      ),
      tabPanel("Forecast",
        value = "forecast",
        forecastUI("forecast")
      ),
      navbarMenu(
        "About",
        menuName = "about",
        tabPanel("About",
          icon = icon("question-circle"), value = "about",
          includeMarkdown("README.md")
        )
      )
    ),
    tags$script(src = "navbarRight.js")
  )
}

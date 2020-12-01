#' Declare previous data points as part of a new sparkline
#' 
#' @note The Highchart object will add the class "sparkline" 
#' 
#' @examples
#' hc <- highchart() %>% 
#'      hc_chart(type="area") %>%
#'      hc_add_series(name="Pears", data=c(43, 62, 15, 73, 17)) %>%
#'      hc_add_series(name="Oranges", data=c(2, 72, 64, 21, 65)) %>%
#'      hc_new_sparkline() %>%
#'      hc_add_series(name="Pears", data=c(21, 67, 21, 43, 92)) %>%
#'      hc_add_series(name="Oranges", data=c(84, 76, 22, 16, 52)) %>%
#'      hc_new_sparkline()
hc_new_sparkline <- function(hc) {
    series <- hc$x$hc_opts$series
    
    # Check which series were not yet stored
    stored <- seq_along(series) %in% unlist(attr(hc, "sparklines"))
    
    # Group all unstored series
    attr(hc, "sparklines") <- c(attr(hc, "sparklines"), list(which(!stored)))
    attr(hc, "class") <- c("sparkline", attr(hc, "class"))
    return(hc)
}

#' Converts the Sparkline object to an HTML object that may be included in a 
#' data frame to be rendered in a Shiny app
#' 
#' @details The JSON from the Sparkline object is saved in the HTML object. The
#' JavaScript function \code{drawSparklines} needs to be called then to parse
#' the JSON code and convert the Sparkline HTML elements to interactive 
#' Highcharts objects
#' 
#' @param hc Sparkline object
#' @param auto If true, automatically overrides some Highcharter options to make
#' the plot look great as a sparkline; otherwise, the Highchart object won't be
#' touched
#' @param width Integer width of the sparkline (default is 120)
#' @param height Integer height of the sparkline (default is 20)
#' 
#' @return Vector of Sparkline HTML elements
hchart.sparkline <- function (hc, auto=TRUE, width=120, height=20) {
    if (auto) {
        # Add automatic options for best sparkline plots in the DataTables
        hc <- hc %>%
            hc_chart(width=width, height=height, backgroundColor="",
                     margin=c(2, 0, 2, 0),
                     style=list(overflow='visible')) %>%
            hc_title(text="") %>%
            hc_credits(enabled=FALSE) %>%
            hc_xAxis(visible=FALSE) %>%
            hc_yAxis(endOnTick=FALSE, startOnTick=FALSE, visible=FALSE) %>%
            hc_exporting(enabled=FALSE) %>%
            hc_legend(enabled=FALSE) %>%
            hc_tooltip(hideDelay=0, shared=TRUE) %>%
            hc_plotOptions(series=list(animation=FALSE, lineWidth=1,
                                       marker=list(radius=1),
                                       fillOpacity=0.25))
    }
    series <- hc$x$hc_opts$series
    
    # Create an HTML object for each sparkline in the Sparkline object
    html <- NULL
    for (sparkline in attr(hc, "sparklines")) {
        params <- hc
        params$x$hc_opts$series <- params$x$hc_opts$series[sparkline]
        params <- toJSON(params$x$hc_opts, auto_unbox = TRUE)
        html <- c(html,
                  sprintf("<sparkline data-sparkline='%s'/>", params))
    }
    return(html)
}

#' Render a data table with Sparkline HTML elements
#' 
#' @details This slighlty modified version of \code{\link{renderDataTable}}
#' calls a JavaScript function to convert the Sparkline HTML elements to
#' interactive Highcharts
#' 
#' @param ... Arguments to pass to \code{\link{renderDataTable}}
#' @param options List of options to pass to \code{\link{renderDataTable}}
renderDataTableSparklines <- function(..., options=NULL) {
    # Escape is set to FALSE to render the Sparkline HTML elements
    renderDataTable(..., escape=FALSE, env=parent.frame(n=1), options=c(
        list(drawCallback=I("drawSparklines")), options))
}
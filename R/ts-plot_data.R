library(tidyverse)
library(scales)
library(lubridate)

##  Definitions

ageGroups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                  "70-79", "80+", "Unknown")

cantons <- c("AG", "AI", "AR", "BE", "BL", "BS", "FL", "FR", "GE", "GL", "GR",
             "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI",
             "UR", "VD", "VS", "ZG", "ZH")

eventDateCols <- list("Positive test"="fall_dt",
                      "Hospitalisation" = "hospdatin",
                      "ICU admission" = "em_hospit_icu_in_dt",
                      "Death" = "pttoddat",
                      "Test (any result)" = "fall_dt")

events <- names(eventDateCols)

travelChoices <- c("All cases", "Travel-related", "Non-travel-related")

granularityChoices <- c("Days", "Weeks", "Months")

slidingWindowChoices <- c("None", "7 days", "14 days", "28 days")


categoryCols <- list("Canton" = "canton",
                     "Age group" = "ageGroup",
                     "Import status" = "travelClass")

categoryNames <- c("Canton", "Age group", "Import status")


## Utility functions

round_dates <- function(.data, granularity) {
    switch(granularity,
           Days = {
               .data
           },
           Weeks = {
               .data %>% mutate(date = round_date(date, unit="week"))
           },
           Months = {
               .data %>% mutate(date = round_date(date, unit="month"))
           })
}


## Plotting functions

generate_timeseries_plot <- function(data, event, given, selectedAgeGroups, selectedCantons,
                          travelChoice, granularity, smoothing, compareCategoryName,
                          displayProb=FALSE, logScale=FALSE, stackHistograms=TRUE) {

    data_proc <- data

    if (is.na(compareCategoryName))
        compare <- NA
    else
        compare <- categoryCols[[compareCategoryName]]

    if ((event == "Test (any result)") || (displayProb && given == "Test (any result)")) {
        validate(
        need(travelChoice == "All cases" && (is.na(compare) || compare != "Import status"),
             "Currently lacking information about travel status for negative test data.")
        )

        if (!is.na(compare) || length(selectedAgeGroups) != length(ageGroups)
            ||  length(selectedCantons) != length(cantons)) {

            ## Exclude all data before start of stratified negative test records
            stratifiedTestingStart <- min((data_proc %>%
                                           filter(positiveTest==FALSE, !is.na(canton)))$fall_dt)

            data_proc <- data_proc %>% filter(fall_dt >= stratifiedTestingStart)
        }
    }

    validate(need(!is.null(selectedAgeGroups),
                  "Must specify at least one age group."),
             need(!is.null(selectedCantons),
                  "Must specify at least one canton."))

    if (displayProb) {
        data_proc <- data_proc %>% mutate(date = fall_dt)
        xlabel <- "Date of Test"

    } else {
        data_proc <- data_proc %>% mutate(date = !!as.symbol(eventDateCols[[event]]))
        xlabel <- paste("Date of", event)
    }

    

    if (displayProb) {

        smoothing_interval <- switch(smoothing,
                                     None = days(0),
                                     "7 days" = days(7),
                                     "14 days" = days(14),
                                     "28 days" = days(28))
        
        switch(given,
               "Test (any result)" = {
                   # Nothing to do
               },
               "Positive test" = {
                   data_proc <- data_proc %>% filter(positiveTest)
               },
               "Hospitalisation" = {
                   data_proc <- data_proc %>%
                       filter(hospitalisation == 1)
               },
               "Death" = {
                   data_proc <- data_proc %>%
                       filter(pttod == 1)
               },
               "ICU admission" = {
                   data_proc <- data_proc %>%
                       filter(icu_aufenthalt == 1)
               })

        switch(event,
               "Test (any result)" = {
                   data_proc <- data_proc %>% mutate(event_occurred = TRUE)
               },
               "Positive test" = {
                   data_proc <- data_proc %>% mutate(event_occurred = positiveTest)
               },
               "Hospitalisation" = {
                   data_proc <- data_proc %>% mutate(event_occurred = (hospitalisation == 1))
               },
               "Death" = {
                   data_proc <- data_proc %>% mutate(event_occurred = (pttod == 1))
               },
               "ICU admission" = {
                   data_proc <- data_proc %>% mutate(event_occurred = (icu_aufenthalt == 1))
               })

    } else {

        
        data_proc <- data_proc %>% round_dates(granularity)


        switch(event,
               "Test (any result)" = {
                   # Nothing to do
               },
               "Positive test" = {
                   data_proc <- data_proc %>% filter(positiveTest)
               },
               "Hospitalisation" = {
                   data_proc <- data_proc %>%
                       filter(hospitalisation == 1)
               },
               "Death" = {
                   data_proc <- data_proc %>%
                       filter(pttod == 1)
               },
               "ICU admission" = {
                   data_proc <- data_proc %>%
                       filter(icu_aufenthalt == 1)
               })
    }

    if (length(selectedAgeGroups)<length(ageGroups))
        data_proc <- data_proc %>% filter(ageGroup %in% selectedAgeGroups)

    if (length(selectedCantons)<length(cantons))
        data_proc <- data_proc %>% filter(canton %in% selectedCantons)

    if (travelChoice != "All cases") 
        data_proc <- data_proc %>% filter(travelClass == travelChoice)

    validate(need(dim(data_proc)[1]>0,
                  "No data matches the requested combination of filters."))
        
    if (displayProb) {

        minDate <- min(data_proc$date)
        maxDate <- max(data_proc$date)
        
        if (is.na(compare)) {

            data_proc <- data_proc %>%
                group_by(date) %>%
                summarize(num = sum(event_occurred*mult, na.rm=TRUE),
                          den = sum(mult), .groups='drop')

            data_proc <- data_proc %>%
                complete(date = seq.Date(minDate, maxDate, by="day")) %>%
                mutate(num = replace_na(num, 0)) %>%
                mutate(den = replace_na(den, 0))
            
            num<-slide_index_dbl(data_proc$num, data_proc$date, sum, .before=smoothing_interval)
            denom<-slide_index_dbl(data_proc$den, data_proc$date, sum, .before=smoothing_interval)

            data_proc <- tibble(date=data_proc$date, prob=num/denom)

            p <- ggplot(data_proc, aes(x=date, y=prob)) +
                geom_line() +
                ## geom_point() +
                ylab(paste0("Fraction of ", given, "s involving ", event))

        } else {

            ## A number of the tests lack age information.  Filter out:
            ## if (compare == "ageGroup")
            ##     data_proc <- data_proc %>% filter(!is.na(ageGroup))

            # Set the range of the histogram

            plot_data <- NULL
            for (compare_val in unique(data_proc[[compare]])) {

                ## Select entries matching compare_val.
                d <- data_proc %>% filter(!!as.symbol(compare) == compare_val)

                d <- d %>%
                    group_by(date) %>%
                    summarize(num = sum(event_occurred*mult, na.rm=TRUE),
                              den = sum(mult),
                              .groups='drop')

                d <- d %>%
                    complete(date = seq.Date(minDate, maxDate, by="day")) %>%
                    mutate(num = replace_na(num, 0)) %>%
                    mutate(den = replace_na(den, 0))
        
                num<-slide_index_dbl(d$num, d$date, sum, .before=smoothing_interval)
                denom<-slide_index_dbl(d$den, d$date, sum, .before=smoothing_interval)

                d <- tibble(date=d$date, prob=num/denom)

                d[,compareCategoryName] <- compare_val

                plot_data <- bind_rows(plot_data, d)
            }

            p <- ggplot(plot_data, aes(x=date, y=prob, col=!!as.symbol(compareCategoryName))) +
                geom_line() +
                ## geom_point() +
                ylab(paste0("Fraction of ", given, "s involving ", event))

        }

    } else {

        if (is.na(compare)) {

            data_proc <- data_proc %>%
                group_by(date) %>%
                summarize(count = sum(mult), .groups='drop')

            p <- ggplot(data_proc) + geom_histogram(aes(x=date, y=count), stat="identity")

        } else {

            plot_data <- NULL
            for (compare_val in unique(data_proc[[compare]])) {

                ## Select entries matching compare_val. 
                d <- data_proc %>% filter(!!as.symbol(compare) == compare_val)

                d <- d %>%
                    group_by(date) %>%
                    summarize(count = sum(mult), .groups='drop')

                d[,compareCategoryName] <- compare_val

                plot_data <- bind_rows(plot_data, d)
            }

            p <- ggplot(plot_data, aes(x=date, y=count, fill=!!as.symbol(compareCategoryName)))

            if (stackHistograms)
                p <- p + geom_histogram(stat="identity")
            else
                p <- p + geom_histogram(stat="identity", position='dodge')
        }

        p <- p + ylab("Total count")
    }

    p <- p + xlab(xlabel) + scale_x_date(date_breaks="months")

    if (logScale)
        p <- p + scale_y_log10()

    p
}

generate_proportion_plot <- function(data, event, propCategoryName,
                                     smoothing, logScale=FALSE) {

    propCategoryCol <- categoryCols[[propCategoryName]]

    validate(need(propCategoryCol != "travelClass" || event != "Test (any result)",
             "Currently lacking information about travel status for negative test data."))

    data_proc <- data
    
    if (event == "Test (any result)") {
        ## Exclude all data before start of stratified negative test records
        stratifiedTestingStart <- min((data_proc %>%
                                       filter(positiveTest==FALSE, !is.na(canton)))$fall_dt)

        data_proc <- data_proc %>% filter(fall_dt >= stratifiedTestingStart)
    }
    

    data_proc <- data_proc %>%
        mutate(date = !!as.symbol(eventDateCols[[event]])) %>%
        filter(!is.na(date))

    if (event == "Positive test")
        data_proc <- data_proc %>% filter(positiveTest)

    smoothing_interval <- switch(smoothing,
                                 None = days(0),
                                 "7 days" = days(7),
                                 "14 days" = days(14),
                                 "28 days" = days(28))

    # Set the range of the histogram
    minDate <- min(data_proc$date)
    maxDate <- max(data_proc$date)

    plot_data <- NULL
    for (val in unique(data_proc[[propCategoryCol]])) {

        ## Select entries matching compare_val.
        d <- data_proc %>% filter(!!as.symbol(propCategoryCol) == val)
        
        d <- d %>%
            group_by(date) %>%
            summarize(count = sum(mult, na.rm=TRUE), .groups='drop') %>%
            complete(date = seq.Date(minDate, maxDate, by="day")) %>%
            mutate(count = replace_na(count, 0))

        d[,propCategoryName] <- val

        d$smoothedCount <- slide_index_dbl(d$count, d$date, sum, .before=smoothing_interval)

        plot_data <- bind_rows(plot_data, d)

    }

    plot_data <- plot_data %>% group_by(date) %>% mutate(proportion = smoothedCount/sum(smoothedCount))

    p <- ggplot(plot_data, aes(x=date, y=proportion, col=!!as.symbol(propCategoryName))) +
        geom_line() + #geom_point() +
        scale_x_date(date_breaks="months") +
        xlab(paste("Date of", event)) +
        ylab(paste0("Proportion of ", event, "s"))

    if (logScale)
        p <- p + scale_y_log10()

    p
}

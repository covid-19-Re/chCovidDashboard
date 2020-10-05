######################
# Extract publicly available COVID-19 data (cases, deaths, hospitalizations) 
# from the BAG and OpenZH/corona-data and construct the data table for the website
# 
# monica.golumbeanu@unibas.ch
######################

library(readxl)
library(lubridate)
library(tidyverse)
library(reshape2)
library(here)

# Global URL variables
BAG_URL = "https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-datengrundlage-lagebericht.xlsx.download.xlsx/200325_Datengrundlage_Grafiken_COVID-19-Bericht.xlsx"
CORONA_URL = "https://github.com/daenuprobst/covid19-cases-switzerland/blob/master/covid_19_data_switzerland.xlsx?raw=true"

# The next functions return a data frame with the following columns (when available):
# day_reported: date
# cases: number of confirmed cases for the given date
# hospitalizations: number of hospitalised cases at the given date
# curr_hosp: number of currently hospitalised cases
# deaths: number of reported deaths at the given date
# data_source: location where the data was taken from

# Extract data from FOPH
get_BAG_online_table = function() {
    # Download the excel file in a temporary folder
    p1f = tempfile(fileext = ".xlsx")
    download.file(BAG_URL, p1f, mode = "wb")
    
    # extract the sheet with cases, hospitalizations, deaths
    p1 = read_excel(path = p1f, sheet = "COVID19 Zahlen", skip = 6)
    
    # Remove the temporary file
    file.remove(p1f)
    
    data_tab = p1[, c("Datum", "Fallzahlen pro Tag", "Hospitalisationen pro Tag", "Todesfälle pro Tag")]
    data_tab = cbind(data_tab, "FOPH online")
    colnames(data_tab) = c("day_reported", "cases", "hospitalizations", 
                           "deaths", "data_source")
    data_tab$day_reported = ymd(data_tab$day_reported)
    return(data_tab)
}

# Extract data from FOPH
get_BAG_online_table_long_format = function() {
    # Download the excel file in a temporary folder
    p1f = tempfile(fileext = ".xlsx")
    download.file(BAG_URL, p1f, mode="wb")
    
    # extract the sheet with cases, hospitalizations, deaths
    p1 = read_excel(path = p1f, sheet = "COVID19 Zahlen", skip = 6)
    
    # Remove the temporary file
    file.remove(p1f)
    
    data_tab = p1[, c("Datum", "Fallzahlen pro Tag", "Hospitalisationen pro Tag", "Todesfälle pro Tag")]
    data_tab = cbind(data_tab, "FOPH online")
    colnames(data_tab) = c("day_reported", "cases", "hospitalizations", 
                           "deaths", "data_source")
    data_tab$day_reported = ymd(data_tab$day_reported)
    final_tab = melt(data_tab, id = c("day_reported", "data_source"))
    return(final_tab)
}

# Extract data from corona-data.ch
get_CORONA_table = function() {
    p1f = tempfile(fileext = ".xlsx")
    download.file(CORONA_URL, p1f, mode="wb")
    # extract cases
    p1 = read_excel(path = p1f, sheet = "Cases")
    cases_tab = cbind.data.frame(ymd(p1$Date), diff(c(0, p1$CH)))
    colnames(cases_tab) = c("day_reported", "n_cases")
    # extract hosp
    p1 = read_excel(path = p1f, sheet = "Hospitalized")
    hosp_tab = cbind.data.frame(ymd(p1$Date), p1$CH)
    colnames(hosp_tab) = c("day_reported", "n_hosp")
    # extract deaths
    p1 = read_excel(path = p1f, sheet = "Fatalities")
    death_tab = cbind.data.frame(ymd(p1$Date), diff(c(0, p1$CH)))
    colnames(death_tab) = c("day_reported", "n_death")
    
    # Remove the temporary file
    file.remove(p1f)
    
    data_tab = Reduce(function(...) merge(..., by = "day_reported", all = TRUE), 
                      list(cases_tab, hosp_tab, death_tab))
    data_tab$data_source = "corona-data.ch"
    colnames(data_tab) = c("day_reported", "cases", "curr_hosp", 
                           "deaths", "data_source")
    return(data_tab)
}

# Extract data from corona-data.ch
get_CORONA_table_long_format = function() {
    p1f = tempfile(fileext = ".xlsx")
    download.file(CORONA_URL, p1f, mode="wb")
    # extract cases
    p1 = read_excel(path = p1f, sheet = "Cases")
    cases_tab = cbind.data.frame(ymd(p1$Date), diff(c(0, p1$CH)))
    colnames(cases_tab) = c("day_reported", "n_cases")
    # extract hosp
    p1 = read_excel(path = p1f, sheet = "Hospitalized")
    hosp_tab = cbind.data.frame(ymd(p1$Date), p1$CH)
    colnames(hosp_tab) = c("day_reported", "n_hosp")
    # extract deaths
    p1 = read_excel(path = p1f, sheet = "Fatalities")
    death_tab = cbind.data.frame(ymd(p1$Date), diff(c(0, p1$CH)))
    colnames(death_tab) = c("day_reported", "n_death")
    
    # Remove the temporary file
    file.remove(p1f)
    
    data_tab = Reduce(function(...) merge(..., by = "day_reported", all = TRUE), 
                      list(cases_tab, hosp_tab, death_tab))
    data_tab$data_source = "corona-data.ch"
    colnames(data_tab) = c("day_reported", "cases", "curr_hosp", 
                           "deaths", "data_source")
    final_tab = melt(data_tab, id = c("day_reported", "data_source"))
    return(final_tab)
}

# Load the data
data_tab_BAG = get_BAG_online_table_long_format()
data_tab_CORONA = get_CORONA_table_long_format()
data_tab = rbind(data_tab_BAG, data_tab_CORONA)

write.table(data_tab, here("data", "forecastModule-Data.txt"), sep = "\t", 
            col.names = TRUE, row.names = FALSE, quote = FALSE)


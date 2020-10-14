library(rnaturalearth)
library(rgeos)


getTravelClass <- function(exp_ort) {
  res <- exp_ort
  res[res == 2 | res == 3] <- "Travel-related"
  res[res != "Travel-related"] <- "Non-travel-related"
  res[is.na(res)] <- "Non-travel-related"

  return(res)
}

mapCountryCode <- function (exp_land) {
  # The country name map is not complete and only contains the countries mentioned in the exp_land field until 14.10.2020.
  # It is difficult to pre-define other countries because it is unclear how the name of a country would be spelled by the
  # authors of the dataset.
  countryNamesGerman <- read_csv2(file = "./data/country-names-german.csv")

  d <- tibble(german_name = exp_land) %>%
    left_join(countryNamesGerman, by = "german_name", na_matches = "never") %>%
    mutate(iso3166_alpha3_code = replace_na(iso3166_alpha3_code, "Unknown"))
  return (unlist(d$iso3166_alpha3_code))
}

mapCountryName <- function (expCountryCode) {
  worldMapData <- ne_countries(returnclass = "sf")
  d <- tibble(expCountryCode = expCountryCode) %>%
    left_join(worldMapData, by = c("expCountryCode" = "iso_a3"), na_matches = "never") %>%
    mutate(admin = replace_na(admin, "Unknown"))
  return (unlist(d$admin))
}

load_and_process_data <- function() {
  BAGdataDir <- "data/BAG"

  bagFiles <- list.files(BAGdataDir,
    pattern = "*FOPH_COVID19_data_extract.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  bagFileDates <- strptime(
    stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
    format = "%Y-%m-%d_%H-%M-%S"
  )

  newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]
  data <- read_csv2(file = newestFile, locale = readr::locale(encoding = "latin1")) %>%
    group_by(altersjahr) %>%
    mutate(ageGroup = tsConstants$ageGroups[min(trunc(altersjahr / 10), 8) + 1]) %>%
    group_by(ageGroup) %>%
    mutate(positiveTest = TRUE, mult = 1) %>%
    mutate(travelClass = getTravelClass(exp_ort)) %>%
    mutate(canton = ktn) %>%
    mutate(expContactPath = unlist(map(exp_kontakt_art, tsConstants$expContactPathsFromCode))) %>%
    mutate(quarantBeforePositiveTest = unlist(map(quarant_vor_pos, tsConstants$quarantBeforePositiveTestFromCode))) %>%
    mutate(labReason = unlist(map(lab_grund, tsConstants$labReasonFromCode))) %>%
    mutate(expCountryCode = mapCountryCode(exp_land)) %>%
    mutate(expCountryName = mapCountryName(expCountryCode)) %>%
    select(
      canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryCode,
      expCountryName
    )

  ## Include positive test results

  bagFiles <- list.files(BAGdataDir,
    pattern = "*Time_series_tests.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  bagFileDates <- strptime(
    stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
    format = "%Y-%m-%d_%H-%M-%S"
  )

  ageGroupConverter <- function(s) {
    str_replace_all(s, " ", "")
  }


  newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]

  dataTS <- read_csv2(file = newestFile) %>%
    mutate(
      fall_dt = Datum,
      hospdatin = NA,
      pttoddat = NA,
      em_hospit_icu_in_dt = NA,
      hospitalisation = NA,
      pttod = NA,
      icu_aufenthalt = NA,
      exp_ort = NA,
      canton = NA,
      ## ageGroup = str_replace_all(Altersklasse, " ",""),
      ageGroup = "Unknown",
      travelClass = NA,
      expContactPath = " not filled",
      quarantBeforePositiveTest = "Not filled",
      labReason = "Not filled",
      positiveTest = FALSE,
      mult = `Negative Tests`,
      expCountryName = "Unknown",
      expCountryCode = "Unknown"
    ) %>%
    select(
      canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryName, expCountryCode
    )


  ## Include space/age-resolved positive test results

  bagFiles <- list.files(BAGdataDir,
    pattern = "*Timeseries_tests_akl.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  bagFileDates <- strptime(
    stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
    format = "%Y-%m-%d_%H-%M-%S"
  )

  ageGroupConverter <- function(s) {
    str_replace_all(s, " ", "")
  }

  newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]

  dataTS_spaceAge <- read_csv2(file = newestFile) %>%
    mutate(
      fall_dt = Datum,
      hospdatin = NA,
      pttoddat = NA,
      em_hospit_icu_in_dt = NA,
      hospitalisation = NA,
      pttod = NA,
      icu_aufenthalt = NA,
      exp_ort = NA,
      canton = ktn,
      ageGroup = str_replace_all(Altersklasse, " ", ""),
      travelClass = NA,
      expContactPath = " not filled",
      quarantBeforePositiveTest = "Not filled",
      labReason = "Not filled",
      positiveTest = FALSE,
      mult = Negative,
      expCountryName = "Unknown",
      expCountryCode = "Unknown"
    ) %>%
    select(
      canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryName,
      expCountryCode
    )

  dataTS_spaceAge <- dataTS_spaceAge %>% mutate(ageGroup = replace_na(ageGroup, "Unknown"))


  ## Filter out entries in dataTS with dates earlier than earliest date
  ## in dataTS_spaceAge

  minSpaceAgeDate <- min(dataTS_spaceAge$fall_dt)

  data <- bind_rows(
    data,
    dataTS_spaceAge,
    dataTS %>% filter(fall_dt < minSpaceAgeDate)
  )

  return(data)
}

library(rnaturalearth)
library(rgeos)


.getTravelClass <- function(exp_ort) {
  res <- exp_ort
  res[res == 2 | res == 3] <- "Travel-related"
  res[res != "Travel-related"] <- "Non-travel-related"
  res[is.na(res)] <- "Non-travel-related"

  return(res)
}

.mapCountryCode <- function (exp_land) {
  # The country name map is not complete and only contains the countries mentioned in the exp_land field until 14.10.2020.
  # It is difficult to pre-define other countries because it is unclear how the name of a country would be spelled by the
  # authors of the dataset.
  countryNamesGerman <- read_csv2(file = "./data/country-names-german.csv")

  d <- tibble(german_name = exp_land) %>%
    left_join(countryNamesGerman, by = "german_name", na_matches = "never") %>%
    mutate(iso3166_alpha3_code = replace_na(iso3166_alpha3_code, "Unknown"))
  return (unlist(d$iso3166_alpha3_code))
}

.mapCountryName <- function (expCountryCode) {
  worldMapData <- ne_countries(returnclass = "sf")
  d <- tibble(expCountryCode = expCountryCode) %>%
    left_join(worldMapData, by = c("expCountryCode" = "iso_a3"), na_matches = "never") %>%
    mutate(admin = replace_na(admin, "Unknown"))
  return (unlist(d$admin))
}

.mapCantonToGrossregion <- function (canton) {
  mapping <- tibble(
    canton = c(
      "GE", "VD", "VS",
      "BE", "SO", "FR", "NE", "JU",
      "BS", "BL", "AG",
      "ZH",
      "SG", "TG", "AI", "AR", "GL", "SH", "GR",
      "UR", "SZ", "OW", "NW", "LU", "ZG",
      "TI",
      "FL"
    ),
    grossregion = c(
      rep("Lake Geneva region", 3),
      rep("Espace Mittelland", 5),
      rep("Grossregion Nordwestschweiz", 3),
      "Grossregion Zurich",
      rep("Ostschweiz", 7),
      rep("Central Switzerland", 6),
      "Grossregion Tessin",
      "Fürstentum Liechtenstein"
    )
  )
  return((tibble(canton = canton) %>% left_join(mapping, by = "canton"))$grossregion)
}

dataCache <- list(
  datasetUpdatedAt = ymd_hm("190001010000"),  # The datetime of the latest dataset from BAG
  lastLoadedAt = ymd_hm("190001010000"),  # The datetime when the dataset was loaded the last time
  data = NULL
)
# The data must be re-loaded after this amount of time. This is just in case that the data in an existing folder
# was changed.
dataCacheMaxAge <- hours(1)

load_and_process_data <- function(BAGdataDir = "data/BAG", useCache = TRUE) {
  bagFiles <- list.files(BAGdataDir,
    pattern = "*FOPH_COVID19_data_extract.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  bagFileDates <- strptime(
    stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
    format = "%Y-%m-%d_%H-%M-%S"
  )

  latestUpdate <- max(bagFileDates)

  if (useCache) {
    if (latestUpdate == dataCache$datasetUpdatedAt && lubridate::now() < (dataCache$lastLoadedAt + dataCacheMaxAge)) {
      return (dataCache$data)
    }
    dataCache$datasetUpdatedAt <<- latestUpdate
    dataCache$lastLoadedAt <<- lubridate::now()
  }

  newestFile <- bagFiles[which(bagFileDates == latestUpdate)[1]]
  data <- read_csv2(file = newestFile, locale = readr::locale(encoding = "latin1")) %>%
    group_by(altersjahr) %>%
    mutate(ageGroup = tsConstants$ageGroups[min(trunc(altersjahr / 10), 8) + 1]) %>%
    group_by(ageGroup) %>%
    mutate(sex = map_chr(sex, tsConstants$sexFromGerman)) %>%
    mutate(positiveTest = TRUE, mult = 1) %>%
    mutate(travelClass = .getTravelClass(exp_ort)) %>%
    mutate(canton = ktn) %>%
    mutate(expContactPath = map_chr(exp_kontakt_art, tsConstants$expContactPathsFromCode)) %>%
    mutate(quarantBeforePositiveTest = map_chr(quarant_vor_pos, tsConstants$quarantBeforePositiveTestFromCode)) %>%
    mutate(labReason = map_chr(lab_grund, tsConstants$labReasonFromCode)) %>%
    mutate(expCountryCode = .mapCountryCode(exp_land)) %>%
    mutate(expCountryName = .mapCountryName(expCountryCode)) %>%
    mutate(grossregion = .mapCantonToGrossregion(canton)) %>%
    select(
      grossregion, canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup, sex,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryCode,
      expCountryName, grunderkr_diabetes, grunderkr_cardio, grunderkr_hypertonie, grunderkr_resp_chron, grunderkr_krebs,
      grunderkr_immunsup, grunderkr_andere, grunderkr_keine
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
      grossregion = NA,
      canton = NA,
      ## ageGroup = str_replace_all(Altersklasse, " ",""),
      ageGroup = "Unknown",
      sex = "Unknown",
      travelClass = NA,
      expContactPath = " not filled",
      quarantBeforePositiveTest = "Not filled",
      labReason = "Not filled",
      positiveTest = FALSE,
      mult = `Negative Tests`,
      expCountryName = "Unknown",
      expCountryCode = "Unknown",
      grunderkr_diabetes = NA,
      grunderkr_cardio = NA,
      grunderkr_hypertonie = NA,
      grunderkr_resp_chron = NA,
      grunderkr_krebs = NA,
      grunderkr_immunsup = NA,
      grunderkr_andere = NA,
      grunderkr_keine = NA
    ) %>%
    select(
      grossregion, canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup, sex,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryName, expCountryCode,
      grunderkr_diabetes, grunderkr_cardio, grunderkr_hypertonie, grunderkr_resp_chron, grunderkr_krebs,
      grunderkr_immunsup, grunderkr_andere, grunderkr_keine
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
      grossregion = .mapCantonToGrossregion(ktn),
      canton = ktn,
      ageGroup = str_replace_all(Altersklasse, " ", ""),
      sex = "Unknown",
      travelClass = NA,
      expContactPath = " not filled",
      quarantBeforePositiveTest = "Not filled",
      labReason = "Not filled",
      positiveTest = FALSE,
      mult = Negative,
      expCountryName = "Unknown",
      expCountryCode = "Unknown",
      grunderkr_diabetes = NA,
      grunderkr_cardio = NA,
      grunderkr_hypertonie = NA,
      grunderkr_resp_chron = NA,
      grunderkr_krebs = NA,
      grunderkr_immunsup = NA,
      grunderkr_andere = NA,
      grunderkr_keine = NA
    ) %>%
    select(
      grossregion, canton, fall_dt, hospdatin, pttoddat, em_hospit_icu_in_dt,
      hospitalisation, pttod, icu_aufenthalt, ageGroup, sex,
      travelClass, expContactPath, quarantBeforePositiveTest, labReason, positiveTest, mult, expCountryName,
      expCountryCode, grunderkr_diabetes, grunderkr_cardio, grunderkr_hypertonie, grunderkr_resp_chron, grunderkr_krebs,
      grunderkr_immunsup, grunderkr_andere, grunderkr_keine
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

  if (useCache) {
    dataCache$data <<- data
  }

  return(data)
}


populationDataCache <- NULL
# Population data downloaded from https://www.bfs.admin.ch/bfs/en/home/statistics/population.assetdetail.14819631.html
# FSO number: px-x-0102030000_101
# Population on 31 December 2019
load_population_data <- function () {
  if (!is.null(populationDataCache)) {
    return(populationDataCache)
  }
  data <- read_tsv("./data/population_canton_age.csv", locale = locale(encoding = "UTF-8"), col_types = cols(
    `Demographic component` = col_character(),
    Canton = col_character(),
    `Citizenship (category)` = col_character(),
    Sex = col_character(),
    Age = col_character(),
    `2019` = col_integer()
  )) %>%
    rename(
      canton = Canton,
      sex = Sex,
      age = Age,
      population = `2019`
    ) %>%
    select(canton, sex, age, population) %>%
    filter(
      canton != "Switzerland" & canton != "Unknown" &
        sex != "Sex - Total" &
        age != "Age - Total" & age != "No indication"
    ) %>%
    mutate(
      ageGroup = tsConstants$ageGroups[
        pmin(trunc(as.integer(str_replace(age, " .*", "")) / 10), 8) + 1],
      sex = unlist(list(Man = "Male", Woman = "Female")[sex], use.names = FALSE),
      canton = unlist(list(
        "Aargau" = "AG",
        "Appenzell Ausserrhoden" = "AR",
        "Appenzell Innerrhoden" = "AI",
        "Basel-Landschaft" = "BL",
        "Basel-Stadt" = "BS",
        "Bern / Berne" = "BE",
        "Fribourg / Freiburg" = "FR",
        "Geneve" = "GE",
        "Glarus" = "GL",
        "Graubunden" = "GR",
        "Jura" = "JU",
        "Luzern" = "LU",
        "Neuchatel" = "NE",
        "Nidwalden" = "NW",
        "Obwalden" = "OW",
        "Schaffhausen" = "SH",
        "Schwyz" = "SZ",
        "Solothurn" = "SO",
        "St. Gallen" = "SG",
        "Thurgau" = "TG",
        "Ticino" = "TI",
        "Uri" = "UR",
        "Valais / Wallis" = "VS",
        "Vaud" = "VD",
        "Zug" = "ZG",
        "Zurich" = "ZH"
      )[canton], use.names = FALSE),
      grossregion = .mapCantonToGrossregion(canton)
    ) %>%
    select(grossregion, canton, sex, ageGroup, population)
  populationDataCache <<- data
  return(data)
}

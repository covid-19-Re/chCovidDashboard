library(shinyWidgets)
library(shinyjs)
library(plotly)
library(slider)
library(sf)

source("R/utilities.R")
source("R/ts/ts-constants.R")
source("R/ts/ts-view_model.R")
source("R/ts/ts-data_store.R")
source("R/ts/ts-compute-plot-data.R")
source("R/ts/ts-plots.R")

# Submodules
source("R/ts/ts-basic-filter-module.R")

basicFilters <- list(
  "age_group" = create_basic_filter(
    "Age group",
    ts_constants$age_group,
    "age_group",
    comparePer100kPeoplePossible = TRUE
  ),
  "sex" = create_basic_filter(
    "Sex",
    ts_constants$sex,
    "sex",
    comparePer100kPeoplePossible = TRUE
  ),
  "grossregion" = create_basic_filter(
    "Grossregion",
    ts_constants$grossregion,
    "grossregion",
    comparePer100kPeoplePossible = TRUE
  ),
  "canton" = create_basic_filter(
    "Canton",
    ts_constants$cantons,
    "canton",
    comparePer100kPeoplePossible = TRUE
  ),
  "travel_class" = create_basic_filter(
    "Travel related status",
    ts_constants$travel_class,
    "travel_class"
  ),
  "exp_land_code" = create_basic_filter(
    "Exposure country",
    ts_constants$exp_land_code,
    "exp_land_code"
  ),
  "exp_kontakt_art" = create_basic_filter(
    "Exposure contact path",
    ts_constants$exp_kontakt_art,
    "exp_kontakt_art"
  ),
  "quarant_vor_pos" = create_basic_filter(
    "Was patient in quarantine before positive result?",
    ts_constants$quarant_vor_pos,
    "quarant_vor_pos"
  ),
  "lab_grund" = create_basic_filter(
    "Reason for the test",
    ts_constants$lab_grund,
    "lab_grund"
  )

  # TODO UnderlyingDisease
  #"underlyingDisease" = create_basic_filter(
  #  "Underlying disease", ts_constants$underlyingDisease, "underlyingDisease",
  #  "Because a person can have multiple underlying diseases, comparing them will show a plot with a
  #                    higher total number of cases then the actual number.",
  #  function () {
  #    return (
  #      function (data, model) {
  #        p <- model$filter[["underlyingDisease"]]$selected
  #        c <- ts_constants$underlyingDisease
  #        if (length(p) < length(c)) {
  #          return (data %>% filter(
  #            replace_na((grunderkr_diabetes == 1 & c[1] %in% p), FALSE) |
  #              replace_na((grunderkr_cardio == 1 & c[2] %in% p), FALSE) |
  #              replace_na((grunderkr_hypertonie == 1 & c[3] %in% p), FALSE) |
  #              replace_na((grunderkr_resp_chron == 1 & c[4] %in% p), FALSE) |
  #              replace_na((grunderkr_krebs == 1 & c[5] %in% p), FALSE) |
  #              replace_na((grunderkr_immunsup == 1 & c[6] %in% p), FALSE) |
  #              replace_na((grunderkr_andere == 1 & c[7] %in% p), FALSE) |
  #              replace_na((grunderkr_keine == 1 & c[8] %in% p), FALSE) |
  #              (
  #                are_na(grunderkr_diabetes) & are_na(grunderkr_cardio) & are_na(grunderkr_hypertonie) &
  #                  are_na(grunderkr_resp_chron) & are_na(grunderkr_krebs) & are_na(grunderkr_immunsup) &
  #                  are_na(grunderkr_andere) & are_na(grunderkr_keine) & c[9] %in% p
  #              )
  #          ))
  #        }
  #        return (data)
  #      }
  #    )
  #  },
  #  function (input, output, session) {
  #    return (
  #      function (data) {
  #        return (c(
  #          "grunderkr_diabetes", "grunderkr_cardio", "grunderkr_hypertonie", "grunderkr_resp_chron", "grunderkr_krebs",
  #          "grunderkr_immunsup", "grunderkr_andere", "grunderkr_keine", NA
  #        ))
  #      }
  #    )
  #  },
  #  function (input, output, session) {
  #    return (
  #      function (data, group) {
  #        if (!is.na(group)) {
  #          return (filter(data, !!as.symbol(group) == 1))
  #        } else (
  #          return (
  #            filter(data, are_na(grunderkr_diabetes) & are_na(grunderkr_cardio) & are_na(grunderkr_hypertonie) &
  #              are_na(grunderkr_resp_chron) & are_na(grunderkr_krebs) & are_na(grunderkr_immunsup) &
  #              are_na(grunderkr_andere) & are_na(grunderkr_keine))
  #          )
  #        )
  #      }
  #    )
  #  }
  #)
)


source("R/ts/ts-ui.R")
source("R/ts/ts-server.R")

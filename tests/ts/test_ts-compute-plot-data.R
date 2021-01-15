setwd("../..")
source("global.R")
library(testthat)


# ---------- Utilities ----------

test_model <- function (model) {
  plot_def <- compute_plot_data(model, "en", ts_data_store)
  expect_null(plot_def$error)
}


# ---------- Resilience Tests for compute_plot_data() ----------

context("Resilience of compute_plot_data()")

test_that("basic event counts does not cause error", {
  initial_model <- ts_create_initial_model()
  clinical_events <- c("Positive test", "Hospitalisation", "Death", "Test (any result)")

  for (ev in clinical_events) {
    model <- initial_model
    model$general$event <- ev
    test_model(model)
  }
})

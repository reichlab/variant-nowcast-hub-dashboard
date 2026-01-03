# Tests for JSON Export Functions
# ================================

library(testthat)
library(tibble)
library(dplyr)
library(jsonlite)

# Source the module being tested
source(here::here("src/dashboard/config.R"))
source(here::here("src/dashboard/export.R"))

# Create a temporary directory for test outputs
test_output_dir <- tempdir()

test_that("export_forecast_json creates valid JSON with correct structure", {
  # Create mock means data
  means <- tibble::tibble(
    model_id = rep(c("model1", "model2"), each = 6),
    target_date = rep(as.Date("2025-01-01") + c(0, 7), 6),
    location = "MA",
    clade = rep(c("24A", "24B", "other"), 4),
    value = runif(12, 0, 1)
  )

  # Create mock quantiles data
  quantiles <- tibble::tibble(
    model_id = rep(c("model1", "model2"), each = 18),
    target_date = rep(rep(as.Date("2025-01-01") + c(0, 7), each = 9), 2),
    location = "MA",
    clade = rep(rep(c("24A", "24B", "other"), each = 3), 4),
    output_type_id = rep(c(0.1, 0.5, 0.9), 12),
    value = runif(36, 0, 1)
  )

  output_dir <- file.path(test_output_dir, "forecasts_test1")

  result <- export_forecast_json(
    means = means,
    quantiles = quantiles,
    multinomial_pi = NULL,
    location = "MA",
    nowcast_date = "2025-01-01",
    output_dir = output_dir
  )

  # Check file was created
  expect_true(file.exists(result))

  # Read and validate JSON
  json <- jsonlite::fromJSON(result)

  expect_equal(json$location, "MA")
  expect_equal(json$nowcast_date, "2025-01-01")
  expect_true("models" %in% names(json))
  expect_true("model1" %in% names(json$models))
  expect_true("model2" %in% names(json$models))

  # Check model structure
  model1 <- json$models$model1
  expect_true("target_date" %in% names(model1))
  expect_true("24A" %in% names(model1))
  expect_true("mean" %in% names(model1$`24A`))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("export_target_json creates valid JSON with correct structure", {
  # Create mock target data
  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01") + c(0, 7), each = 3),
    location = "MA",
    clade = rep(c("24A", "24B", "other"), 2),
    count = c(100, 50, 10, 120, 45, 15),
    proportion = c(0.625, 0.3125, 0.0625, 0.667, 0.25, 0.083),
    total = c(160, 160, 160, 180, 180, 180)
  )

  output_dir <- file.path(test_output_dir, "targets_test1")

  result <- export_target_json(
    target_data = target_data,
    location = "MA",
    nowcast_date = "2025-01-01",
    as_of_date = "2024-12-30",
    version = "latest",
    output_dir = output_dir
  )

  # Check file was created
  expect_true(file.exists(result))

  # Read and validate JSON
  json <- jsonlite::fromJSON(result)

  expect_equal(json$location, "MA")
  expect_equal(json$nowcast_date, "2025-01-01")
  expect_equal(json$as_of, "2024-12-30")
  expect_true("data" %in% names(json))
  expect_true("target_date" %in% names(json$data))
  expect_true("24A" %in% names(json$data))

  # Check clade structure
  clade_24A <- json$data$`24A`
  expect_true("count" %in% names(clade_24A))
  expect_true("proportion" %in% names(clade_24A))
  expect_true("total" %in% names(clade_24A))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("export_options_json creates valid dashboard config", {
  output_dir <- file.path(test_output_dir, "options_test1")

  result <- export_options_json(
    locations = c("MA", "NY", "CA"),
    location_names = c("MA" = "Massachusetts", "NY" = "New York", "CA" = "California"),
    nowcast_dates = c("2024-12-01", "2024-12-08", "2024-12-15"),
    models = c("model1", "model2"),
    initial_selected_models = c("model1"),
    clades_by_date = list(
      "2024-12-01" = c("24A", "24B"),
      "2024-12-08" = c("24A", "24B", "24C"),
      "2024-12-15" = c("24A", "24B", "24C")
    ),
    clade_labels = c("24A" = "24A (JN.1)", "24B" = "24B (JN.1.11)", "24C" = "24C"),
    current_nowcast_date = "2024-12-15",
    output_dir = output_dir
  )

  # Check file was created
  expect_true(file.exists(result))

  # Read and validate JSON
  json <- jsonlite::fromJSON(result)

  expect_equal(json$locations, c("MA", "NY", "CA"))
  expect_equal(json$models, c("model1", "model2"))
  expect_equal(json$initial_selected_models, "model1")
  expect_equal(json$intervals, c(50, 80, 95))
  expect_equal(json$default_interval, 80)
  expect_equal(json$current_nowcast_date, "2024-12-15")
  expect_true("last_updated" %in% names(json))
  expect_true("clades_by_date" %in% names(json))
  expect_true("clade_labels" %in% names(json))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("export functions handle special characters in clades", {
  # Test with clade names that might cause issues
  means <- tibble::tibble(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    clade = c("24A", "other", "recombinant"),
    value = c(0.5, 0.3, 0.2)
  )

  quantiles <- tibble::tibble(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    clade = rep(c("24A", "other", "recombinant"), each = 3),
    output_type_id = rep(c(0.1, 0.5, 0.9), 3),
    value = runif(9, 0, 1)
  )

  output_dir <- file.path(test_output_dir, "forecasts_special")

  result <- export_forecast_json(
    means = means,
    quantiles = quantiles,
    multinomial_pi = NULL,
    location = "MA",
    nowcast_date = "2025-01-01",
    output_dir = output_dir
  )

  # Should not throw error and create valid JSON
  expect_true(file.exists(result))
  json <- jsonlite::fromJSON(result)
  expect_true("other" %in% names(json$models$model1))
  expect_true("recombinant" %in% names(json$models$model1))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("export functions create correct directory structure", {
  target_data <- tibble::tibble(
    target_date = as.Date("2025-01-01"),
    location = "MA",
    clade = c("24A", "24B"),
    count = c(100, 50),
    proportion = c(0.667, 0.333),
    total = c(150, 150)
  )

  output_dir <- file.path(test_output_dir, "dir_structure_test")

  # Export to round-open
  export_target_json(
    target_data = target_data,
    location = "MA",
    nowcast_date = "2025-01-01",
    as_of_date = "2024-12-30",
    version = "round-open",
    output_dir = output_dir
  )

  # Export to latest
  export_target_json(
    target_data = target_data,
    location = "MA",
    nowcast_date = "2025-01-01",
    as_of_date = "2025-01-02",
    version = "latest",
    output_dir = output_dir
  )

  # Check directory structure
  expect_true(dir.exists(file.path(output_dir, "targets", "round-open")))
  expect_true(dir.exists(file.path(output_dir, "targets", "latest")))
  expect_true(file.exists(file.path(output_dir, "targets", "round-open", "MA_2025-01-01.json")))
  expect_true(file.exists(file.path(output_dir, "targets", "latest", "MA_2025-01-01.json")))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

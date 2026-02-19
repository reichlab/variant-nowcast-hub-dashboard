# Integration Tests for Dashboard Pipeline
# =========================================
# These tests verify the pipeline produces complete, valid output.
# They make actual hub connections and are slower than unit tests.
#
# Performance optimization: Tests run with a subset of locations (2 instead of 52)
# and share a single pipeline execution to minimize S3 connections and data processing.

library(testthat)
library(jsonlite)

# Source the pipeline
source(here::here("src/dashboard/pipeline.R"))

# =============================================================================
# Helper Functions
# =============================================================================

skip_if_no_hub_connection <- function() {
  tryCatch({
    # Try to connect to hub
    bucket <- arrow::s3_bucket("covid-variant-nowcast-hub")
    bucket$ls("model-output")
  }, error = function(e) {
    skip("Skipping: cannot connect to hub (offline or network issue)")
  })
}

#' Get a historical date that's known to have data
#' Uses a date from the middle of available dates to avoid issues with
#' the most recent date not having submissions yet
#' @param hub_config Hub configuration
#' @return A historical nowcast date string
get_reliable_test_date <- function(hub_config) {
  all_dates <- get_nowcast_dates(hub_config)
  # Use a date from the middle of available dates
  # This ensures the date has model submissions
  all_dates[ceiling(length(all_dates) / 2)]
}

#' Get a small set of locations for testing
#' Using 2 locations instead of 52 provides ~25x speedup for data processing
#' @return Character vector of test location codes
get_test_locations <- function() {
  c("MA", "CA")
}

# =============================================================================
# Consolidated Integration Test
# =============================================================================
# This test runs the pipeline ONCE with a limited set of locations, then
# validates all assertions from the previous 3 separate tests. This reduces
# integration test time from ~7 minutes to under 30 seconds.

test_that("pipeline produces complete, valid output with correct structure", {
  skip_if_no_hub_connection()

  # Create temp output directory
  test_output_dir <- file.path(tempdir(), paste0("pipeline_test_", Sys.getpid()))
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

  # Get hub config to find a valid date to test with
  hub_config <- fetch_hub_config()
  all_dates <- get_nowcast_dates(hub_config)

  # Use a historical date that's known to have data
  test_date <- get_reliable_test_date(hub_config)
  test_locations <- get_test_locations()

  message("Running pipeline for single date: ", test_date,
          " with ", length(test_locations), " locations: ",
          paste(test_locations, collapse = ", "))

  # Run pipeline once with limited locations for speed
  run_pipeline(
    nowcast_dates = test_date,
    regenerate = FALSE,
    locations = test_locations,
    output_dir = test_output_dir
  )

  # =========================================================================
  # Test Group 1: dashboard-options.json completeness
  # (from "incremental run produces complete dashboard-options.json")
  # =========================================================================

  options_path <- file.path(test_output_dir, "dashboard-options.json")
  expect_true(file.exists(options_path),
              info = "dashboard-options.json should be created")

  options <- jsonlite::fromJSON(options_path)

  # Test: Should have ALL nowcast dates, not just the processed one
  expect_true(
    length(options$nowcast_dates) > 1,
    info = paste("Should have all nowcast dates, got:", length(options$nowcast_dates))
  )
  expect_equal(
    sort(options$nowcast_dates),
    sort(all_dates),
    info = "nowcast_dates should match all dates from hub"
  )

  # Test: Should have ALL models from hub
  all_models <- get_all_model_ids()
  expect_true(
    length(options$models) >= length(all_models) * 0.9,  # Allow some tolerance
    info = paste("Should have most models. Expected:", length(all_models),
                 "Got:", length(options$models))
  )

  # Test: Should have clades for ALL dates, not just processed one
  expect_equal(
    length(options$clades_by_date),
    length(options$nowcast_dates),
    info = "Should have clades_by_date entry for every nowcast date"
  )

  # Test: Should have as_of_dates for ALL dates
  expect_equal(
    length(options$as_of_dates),
    length(options$nowcast_dates),
    info = "Should have as_of_dates entry for every nowcast date"
  )

  # Test: current_nowcast_date should be the most recent
  expect_equal(
    options$current_nowcast_date,
    max(options$nowcast_dates),
    info = "current_nowcast_date should be the most recent date"
  )

  # =========================================================================
  # Test Group 2: dashboard-options.json schema validation
  # (from "dashboard-options.json has all required fields with correct structure")
  # =========================================================================

  # Required top-level fields
  required_fields <- c(
    "locations",
    "location_names",
    "nowcast_dates",
    "models",
    "initial_selected_models",
    "clades_by_date",
    "clade_labels",
    "as_of_dates",
    "intervals",
    "default_interval",
    "current_nowcast_date",
    "last_updated"
  )

  for (field in required_fields) {
    expect_true(
      field %in% names(options),
      info = paste("Missing required field:", field)
    )
  }

  # Validate locations
  expect_true(length(options$locations) >= 50,
              info = "Should have at least 50 US locations")
  expect_true("CA" %in% options$locations,
              info = "Should include expected locations")

  # Validate location_names is a named list matching locations
  expect_true(is.list(options$location_names),
              info = "location_names should be a list")
  expect_true(all(options$locations %in% names(options$location_names)),
              info = "All locations should have names")

  # Validate nowcast_dates are valid date strings
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", options$nowcast_dates)),
              info = "All nowcast_dates should be YYYY-MM-DD format")

  # Validate models is non-empty
  expect_true(length(options$models) > 0,
              info = "Should have at least one model")

  # Validate initial_selected_models is subset of models
  expect_true(all(options$initial_selected_models %in% options$models),
              info = "initial_selected_models should be subset of models")

  # Validate intervals
  expect_true(is.numeric(options$intervals),
              info = "intervals should be numeric")
  expect_true(all(options$intervals > 0 & options$intervals < 100),
              info = "intervals should be percentages between 0-100")
  expect_true(options$default_interval %in% options$intervals,
              info = "default_interval should be one of the intervals")

  # Validate as_of_dates structure for each nowcast date
  for (date in options$nowcast_dates) {
    expect_true(
      date %in% names(options$as_of_dates),
      info = paste("as_of_dates missing entry for:", date)
    )

    as_of <- options$as_of_dates[[date]]
    expect_true("round_open" %in% names(as_of),
                info = paste("as_of_dates[", date, "] missing round_open"))
    expect_true("latest" %in% names(as_of),
                info = paste("as_of_dates[", date, "] missing latest"))
    expect_true("round_closed" %in% names(as_of),
                info = paste("as_of_dates[", date, "] missing round_closed"))

    # Validate date formats
    expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", as_of$round_open),
                info = paste("round_open should be YYYY-MM-DD for:", date))
    expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", as_of$latest),
                info = paste("latest should be YYYY-MM-DD for:", date))
    expect_true(is.logical(as_of$round_closed),
                info = paste("round_closed should be logical for:", date))
  }

  # Validate clades_by_date structure
  for (date in options$nowcast_dates) {
    expect_true(
      date %in% names(options$clades_by_date),
      info = paste("clades_by_date missing entry for:", date)
    )

    clades <- options$clades_by_date[[date]]
    expect_true(length(clades) > 0,
                info = paste("clades_by_date[", date, "] should not be empty"))
    expect_true("other" %in% clades,
                info = paste("clades should include 'other' for:", date))
  }

  # Validate current_nowcast_date
  expect_true(options$current_nowcast_date %in% options$nowcast_dates,
              info = "current_nowcast_date should be in nowcast_dates")

  # Validate last_updated is a timestamp
  expect_true(nchar(options$last_updated) > 10,
              info = "last_updated should be a timestamp string")

  # =========================================================================
  # Test Group 3: Forecast and target file creation
  # (from "pipeline creates forecast and target files for processed date")
  # =========================================================================

  # Check forecast files exist
  forecast_dir <- file.path(test_output_dir, "forecasts")
  expect_true(dir.exists(forecast_dir), info = "forecasts/ directory should exist")

  forecast_files <- list.files(forecast_dir, pattern = "\\.json$")
  expect_true(length(forecast_files) > 0,
              info = "Should have at least one forecast file")

  # Check forecast files were created for test locations
  for (loc in test_locations) {
    expected_file <- paste0(loc, "_", test_date, ".json")
    expect_true(expected_file %in% forecast_files,
                info = paste("Should have forecast file for location:", loc))
  }

  # Check we created files for test locations (not all 52).
  # The refresh loop also regenerates forecasts for recent dates (~13 weeks),
  # so total files = test date + recent dates, all limited to test locations.
  expect_true(length(forecast_files) >= length(test_locations),
              info = paste("Should have at least", length(test_locations),
                           "forecast files (for test locations)"))
  expect_true(all(grepl(paste(test_locations, collapse = "|"), forecast_files)),
              info = "All forecast files should be for test locations only")

  # Check target files exist
  target_round_open_dir <- file.path(test_output_dir, "targets", "round-open")
  target_latest_dir <- file.path(test_output_dir, "targets", "latest")

  expect_true(dir.exists(target_round_open_dir),
              info = "targets/round-open/ directory should exist")
  expect_true(dir.exists(target_latest_dir),
              info = "targets/latest/ directory should exist")

  # Check target files were created for test locations
  round_open_files <- list.files(target_round_open_dir, pattern = "\\.json$")
  latest_files <- list.files(target_latest_dir, pattern = "\\.json$")

  expect_true(length(round_open_files) > 0,
              info = "Should have at least one round-open target file")
  expect_true(length(latest_files) > 0,
              info = "Should have at least one latest target file")

  # Validate a forecast file structure
  sample_forecast_file <- paste0(test_locations[1], "_", test_date, ".json")
  if (sample_forecast_file %in% forecast_files) {
    forecast <- jsonlite::fromJSON(file.path(forecast_dir, sample_forecast_file))

    expect_true("location" %in% names(forecast),
                info = "Forecast should have location field")
    expect_true("nowcast_date" %in% names(forecast),
                info = "Forecast should have nowcast_date field")
    expect_true("models" %in% names(forecast),
                info = "Forecast should have models field")
    expect_true(is.list(forecast$models) && length(forecast$models) > 0,
                info = "Forecast should have at least one model")
  }
})

# Integration Tests for Dashboard Pipeline
# =========================================
# These tests verify the pipeline produces complete, valid output.
# They make actual hub connections and are slower than unit tests.

library(testthat)
library(jsonlite)

# Source the pipeline
source(here::here("src/dashboard/pipeline.R"))

# =============================================================================
# Helper: Skip if no hub connection
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

# =============================================================================
# Test 1: Incremental run produces complete dashboard-options.json
# =============================================================================

test_that("incremental run produces complete dashboard-options.json", {
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

  message("Running incremental pipeline for single date: ", test_date)

  # Run pipeline for just one date (incremental mode)
  run_pipeline(
    nowcast_dates = test_date,
    regenerate = FALSE,
    output_dir = test_output_dir
  )

  # Load the generated options file
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
})

# =============================================================================
# Test 2: Schema validation - dashboard-options.json structure
# =============================================================================

test_that("dashboard-options.json has all required fields with correct structure", {
  skip_if_no_hub_connection()

  # Create temp output directory
  test_output_dir <- file.path(tempdir(), paste0("schema_test_", Sys.getpid()))
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

  # Get hub config and use a reliable historical date
  hub_config <- fetch_hub_config()
  test_date <- get_reliable_test_date(hub_config)

  # Run pipeline
  run_pipeline(
    nowcast_dates = test_date,
    regenerate = FALSE,
    output_dir = test_output_dir
  )

  options <- jsonlite::fromJSON(file.path(test_output_dir, "dashboard-options.json"))

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
  expect_true("US" %in% options$locations || "CA" %in% options$locations,
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
})

# =============================================================================
# Test 3: Forecast and target files are created for processed date
# =============================================================================

test_that("pipeline creates forecast and target files for processed date", {
  skip_if_no_hub_connection()

  # Create temp output directory
  test_output_dir <- file.path(tempdir(), paste0("files_test_", Sys.getpid()))
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

  # Get hub config and use a reliable historical date
  hub_config <- fetch_hub_config()
  test_date <- get_reliable_test_date(hub_config)

  # Run pipeline
  run_pipeline(
    nowcast_dates = test_date,
    regenerate = FALSE,
    output_dir = test_output_dir
  )

  # Check forecast files exist
  forecast_dir <- file.path(test_output_dir, "forecasts")
  expect_true(dir.exists(forecast_dir), info = "forecasts/ directory should exist")

  forecast_files <- list.files(forecast_dir, pattern = "\\.json$")
  expect_true(length(forecast_files) > 0,
              info = "Should have at least one forecast file")

  # Check at least one forecast file matches the test date
  date_pattern <- gsub("-", "-", test_date)  # Escape for regex if needed
  matching_files <- grep(test_date, forecast_files, value = TRUE)
  expect_true(length(matching_files) > 0,
              info = paste("Should have forecast files for date:", test_date))

  # Check target files exist
  target_round_open_dir <- file.path(test_output_dir, "targets", "round-open")
  target_latest_dir <- file.path(test_output_dir, "targets", "latest")

  expect_true(dir.exists(target_round_open_dir),
              info = "targets/round-open/ directory should exist")
  expect_true(dir.exists(target_latest_dir),
              info = "targets/latest/ directory should exist")

  # Validate a forecast file structure
  if (length(matching_files) > 0) {
    forecast <- jsonlite::fromJSON(file.path(forecast_dir, matching_files[1]))

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

# Dashboard Data Pipeline
# =======================
# Main orchestration script for generating pre-computed JSON data files
# for the variant nowcast hub dashboard.
#
# Usage:
#   source("src/dashboard/pipeline.R")
#   run_pipeline()  # Process latest nowcast date
#   run_pipeline(nowcast_dates = "2025-01-01")  # Process specific date
#   run_pipeline(regenerate = TRUE)  # Regenerate all historical data

# Load dependencies
library(hubData)
library(hubUtils)
library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)

# Source local modules
source(here::here("src/dashboard/config.R"))
source(here::here("src/dashboard/multinomial.R"))
source(here::here("src/dashboard/export.R"))

# =============================================================================
# Data Fetching Functions
# =============================================================================

#' Fetch hub configuration (tasks.json)
#' @return List with hub configuration including rounds and model tasks
fetch_hub_config <- function() {
  jsonlite::fromJSON(HUB_CONFIG_URL)
}

#' Get all available nowcast dates from hub config
#' @param hub_config Hub configuration from fetch_hub_config()
#' @return Character vector of nowcast dates
get_nowcast_dates <- function(hub_config) {
  sapply(hub_config$rounds$model_tasks,
         function(x) unlist(x$task_ids$nowcast_date$required))
}

#' Get predicted clades for a specific nowcast date
#' @param hub_config Hub configuration
#' @param nowcast_date Date string (YYYY-MM-DD)
#' @return Character vector of clade names
get_clades_for_date <- function(hub_config, nowcast_date) {
  nowcast_dates <- get_nowcast_dates(hub_config)
  round_idx <- which(nowcast_dates == nowcast_date)

  if (length(round_idx) == 0) {
    stop(paste("Nowcast date not found in hub config:", nowcast_date))
  }

  unlist(hub_config$rounds$model_tasks[[round_idx]]$task_ids$clade$required)
}

#' Get clade labels (identity mapping - clade name is the label)
#' @return Tibble with clade and clade_label columns
fetch_clade_labels <- function() {

  # For now, use identity mapping (clade = label)

  # Could be enhanced to fetch from Nextstrain API if needed

  tibble::tibble(
    clade = character(0),
    clade_label = character(0)
  )
}

#' Fetch target data (observed clade counts) from GitHub hub repo
#' @param as_of_date Date for which to fetch target data snapshot
#' @param nowcast_date The nowcast date to fetch data for
#' @param predicted_clades Vector of clades being predicted (others become "other")
#' @param min_date Minimum date to include
#' @param max_date Maximum date to include
#' @return Tibble with target data
fetch_target_data <- function(as_of_date, nowcast_date, predicted_clades, min_date, max_date) {
  # Target data is stored in GitHub repo with as_of and nowcast_date partitioning
  # Structure: target-data/time-series/as_of={date}/nowcast_date={date}/timeseries.parquet
  as_of_str <- as.character(as.Date(as_of_date))
  nowcast_str <- as.character(as.Date(nowcast_date))
  url <- paste0(TARGET_DATA_HUB_URL, "/as_of=", as_of_str, "/nowcast_date=", nowcast_str, "/timeseries.parquet")

  tryCatch({
    arrow::read_parquet(url) |>
      # Rename columns to match expected format
      dplyr::rename(date = target_date, count = observation) |>
      # Reclassify unpredicted clades to "other"
      dplyr::mutate(clade = ifelse(clade %in% predicted_clades, clade, "other")) |>
      dplyr::filter(
        date >= min_date,
        date <= max_date
      ) |>
      # Aggregate counts for clades that were merged to "other"
      dplyr::group_by(date, location, clade) |>
      dplyr::summarize(count = sum(count), .groups = "drop") |>
      # Ensure all location-clade combinations exist
      dplyr::group_by(date) |>
      tidyr::complete(location, clade, fill = list(count = 0)) |>
      dplyr::ungroup()
  }, error = function(e) {
    message("    Could not fetch target data for as_of=", as_of_str, ", nowcast_date=", nowcast_str)
    NULL
  })
}

#' Find the Tuesday on or before a given date
#' Target data snapshots are taken on Tuesdays
#' @param date A date
#' @return The Tuesday on or before the date
get_tuesday_on_or_before <- function(date) {
  d <- as.Date(date)
  # wday: 1=Sun, 2=Mon, 3=Tue, 4=Wed, 5=Thu, 6=Fri, 7=Sat
  wday <- lubridate::wday(d)
  # Days to subtract to get to Tuesday (wday=3)
  days_back <- (wday - 3) %% 7
  d - days_back
}

#' Find the Monday before a given date (for display as "round open")
#' @param date A date (typically the nowcast date, which is a Wednesday)
#' @return The Monday before the date
get_monday_before <- function(date) {
  d <- as.Date(date)
  # wday: 1=Sun, 2=Mon, 3=Tue, 4=Wed, 5=Thu, 6=Fri, 7=Sat
  wday <- lubridate::wday(d)
  # Days to subtract to get to Monday (wday=2)
  days_back <- (wday - 2) %% 7
  if (days_back == 0) days_back <- 7  # If already Monday, go back a week
  d - days_back
}

#' Check if a round has closed (13 weeks have passed since nowcast date)
#' @param nowcast_date The nowcast date
#' @return TRUE if round has closed
is_round_closed <- function(nowcast_date) {
  as.Date(nowcast_date) + (TARGET_DATA_MAX_WEEKS * 7) <= Sys.Date()
}

#' Calculate the appropriate as_of date for "round-open" target data
#' @param nowcast_date The nowcast/reference date (typically Wednesday)
#' @return The as_of date to use (Tuesday on or before nowcast_date)
get_round_open_as_of_date <- function(nowcast_date) {
  get_tuesday_on_or_before(nowcast_date)
}

#' Fetch available as_of dates from the target data repository
#' @return Character vector of available as_of dates (YYYY-MM-DD format), sorted descending
fetch_available_as_of_dates <- function() {
  tryCatch({
    # Fetch directory listing from GitHub API
    response <- jsonlite::fromJSON(TARGET_DATA_API_URL)

    # Extract as_of dates from directory names (format: "as_of=YYYY-MM-DD")
    dir_names <- response$name
    as_of_dirs <- dir_names[grepl("^as_of=", dir_names)]

    # Parse dates from directory names
    dates <- gsub("^as_of=", "", as_of_dirs)

    # Sort descending (most recent first)
    sort(dates, decreasing = TRUE)
  }, error = function(e) {
    warning("Could not fetch available as_of dates from repository: ", e$message)
    character(0)
  })
}

#' Calculate the appropriate as_of date for "latest" target data
#' Uses the most recent available as_of date from the source repository,
#' constrained by the maximum weeks after nowcast date.
#' @param nowcast_date The nowcast/reference date
#' @param available_as_of_dates Optional vector of available dates (fetched if NULL)
#' @return The as_of date to use for latest data
get_latest_as_of_date <- function(nowcast_date, available_as_of_dates = NULL) {
  nowcast <- as.Date(nowcast_date)
  max_as_of <- nowcast + (TARGET_DATA_MAX_WEEKS * 7)


  # Fetch available dates if not provided
  if (is.null(available_as_of_dates)) {
    available_as_of_dates <- fetch_available_as_of_dates()
  }

  # If we couldn't fetch available dates, fall back to old behavior
  if (length(available_as_of_dates) == 0) {
    warning("Could not determine available as_of dates, using calculated date")
    target_date <- min(max_as_of, Sys.Date())
    return(get_tuesday_on_or_before(target_date))
  }

  # Convert to Date objects for comparison
  available_dates <- as.Date(available_as_of_dates)

  # Filter to dates that are:
  # 1. Not after max_as_of (nowcast + 13 weeks)
  # 2. Not after today
  valid_dates <- available_dates[available_dates <= max_as_of & available_dates <= Sys.Date()]

  if (length(valid_dates) == 0) {
    warning("No valid as_of dates found for nowcast_date ", nowcast_date)
    return(get_tuesday_on_or_before(min(max_as_of, Sys.Date())))
  }

  # Use the most recent valid date
  latest_date <- max(valid_dates)

  # Warn if the latest available date is more than a week old
  days_old <- as.numeric(Sys.Date() - latest_date)
  if (days_old > 7) {
    warning("Latest available target data (as_of=", latest_date,
            ") is ", days_old, " days old. Target data may be stale.")
  }

  as.character(latest_date)
}

#' Get all model IDs from hub by scanning model-output directory
#' @return Character vector of model IDs
get_all_model_ids <- function() {
  bucket <- arrow::s3_bucket("covid-variant-nowcast-hub")
  model_dirs <- bucket$ls("model-output")
  # Extract model ID from path like "model-output/Hub-ensemble"
  gsub("^model-output/", "", model_dirs)
}

#' Fetch model predictions from hub
#' @param nowcast_date The nowcast/reference date
#' @param target_dates Vector of target dates to fetch (typically Saturdays)
#' @return Tibble with model predictions
fetch_hub_predictions <- function(nowcast_date, target_dates) {
  hub_con <- hubData::connect_hub(
    hub_path = arrow::s3_bucket("covid-variant-nowcast-hub")
  )

  hub_con |>
    dplyr::filter(
      nowcast_date == !!nowcast_date,
      target_date %in% target_dates
    ) |>
    hubData::collect_hub()
}

# =============================================================================
# Data Transformation Functions
# =============================================================================

#' Process daily target data to add proportions and totals
#' @param daily_data Daily target data tibble
#' @return Tibble with count, total, and proportion columns
process_daily_target_data <- function(daily_data) {
  daily_data |>
    dplyr::rename(target_date = date) |>
    dplyr::group_by(location, target_date) |>
    dplyr::mutate(total = sum(count)) |>
    dplyr::ungroup() |>
    dplyr::mutate(proportion = ifelse(total == 0, 0, count / total)) |>
    dplyr::arrange(location, target_date, clade)
}

#' Process model predictions to get means and quantiles
#' @param predictions Raw predictions from hub
#' @param quantile_probs Quantile probabilities to compute
#' @return List with means and quantiles tibbles
process_predictions <- function(predictions, quantile_probs = QUANTILE_PROBS) {
  # Separate mean and sample outputs
  mean_dat <- predictions |>
    dplyr::filter(output_type == "mean")

  models_with_means <- unique(mean_dat$model_id)

  sample_dat <- predictions |>
    dplyr::filter(output_type == "sample")

  # Compute means from samples for models that don't provide them
  means_from_samples <- sample_dat |>
    dplyr::filter(!(model_id %in% models_with_means)) |>
    dplyr::group_by(model_id, target_date, location, clade) |>
    dplyr::summarize(value = mean(value), .groups = "drop") |>
    dplyr::mutate(output_type = "mean", output_type_id = "mean")

  # Combine all means
  all_means <- mean_dat |>
    dplyr::select(model_id, target_date, location, clade, value) |>
    dplyr::mutate(output_type = "mean", output_type_id = "mean") |>
    dplyr::bind_rows(means_from_samples) |>
    dplyr::mutate(value = round(value, 4))

  # Compute quantiles from samples using hubUtils
  # Note: hubUtils::convert_output_type() expects specific format
  # For now, compute manually until we confirm hubUtils compatibility
  quantiles <- sample_dat |>
    dplyr::group_by(model_id, target_date, location, clade) |>
    dplyr::reframe(
      output_type_id = quantile_probs,
      value = stats::quantile(value, probs = quantile_probs, names = FALSE)
    ) |>
    dplyr::mutate(
      output_type = "quantile",
      value = round(value, 4)
    )

  list(
    means = all_means,
    quantiles = quantiles,
    samples = sample_dat
  )
}

#' Get target dates (daily) for a nowcast date
#' @param nowcast_date The reference/nowcast date
#' @param days_back Number of days back to include (default 31 = ~4.5 weeks)
#' @param days_forward Number of days forward to include (default 10 = ~1.5 weeks)
#' @return Vector of daily dates
get_target_dates <- function(nowcast_date, days_back = 31, days_forward = 10) {
  ref_date <- as.Date(nowcast_date)
  seq.Date(
    from = ref_date - days_back,
    to = ref_date + days_forward,
    by = "1 day"
  )
}

# =============================================================================
# Main Pipeline Function
# =============================================================================

#' Run the dashboard data pipeline
#' @param nowcast_dates Character vector of dates to process (NULL for latest)
#' @param regenerate If TRUE, regenerate all historical data
#' @param generate_forecasts If TRUE, generate forecast data (default TRUE)
#' @param generate_targets If TRUE, generate target data (default TRUE)
#' @param initial_selected_models Models to show by default in dashboard
#' @param locations Vector of location codes to process (NULL for all US locations)
#' @param output_dir Base output directory
#' @return Invisibly returns list of processed dates
run_pipeline <- function(
    nowcast_dates = NULL,
    regenerate = FALSE,
    generate_forecasts = TRUE,
    generate_targets = TRUE,
    initial_selected_models = DEFAULT_INITIAL_MODELS,
    locations = NULL,
    output_dir = OUTPUT_DIR
) {
  message("Starting dashboard data pipeline...")
  message("  Generate forecasts: ", generate_forecasts)
  message("  Generate targets: ", generate_targets)

  # Set default locations if not specified
  if (is.null(locations)) {
    locations <- US_LOCATIONS
  }
  message("  Processing ", length(locations), " location(s)")

  # Create output directories
  if (generate_forecasts) {
    dir.create(file.path(output_dir, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  }
  if (generate_targets) {
    dir.create(file.path(output_dir, "targets", "round-open"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_dir, "targets", "latest"), recursive = TRUE, showWarnings = FALSE)
  }

  # Fetch hub configuration
  message("Fetching hub configuration...")
  hub_config <- fetch_hub_config()
  all_nowcast_dates <- get_nowcast_dates(hub_config)

  # Determine which dates to process
  if (regenerate) {
    dates_to_process <- all_nowcast_dates
  } else if (is.null(nowcast_dates) || nowcast_dates == "") {
    # Default to most recent date
    dates_to_process <- tail(all_nowcast_dates, 1)
  } else {
    # Parse comma-separated dates if provided
    dates_to_process <- trimws(strsplit(nowcast_dates, ",")[[1]])
  }

  message(paste("Processing", length(dates_to_process), "nowcast date(s)..."))

  # Pre-populate metadata for ALL dates (not just processed ones)
  # This ensures dashboard-options.json contains complete data
  message("Building metadata for all nowcast dates...")

  # Get all models from hub directory structure
  all_models <- get_all_model_ids()
  message("  Found ", length(all_models), " models in hub")

  # Fetch available as_of dates once (for efficiency)
  message("  Fetching available target data dates...")
  available_as_of_dates <- fetch_available_as_of_dates()
  if (length(available_as_of_dates) > 0) {
    message("  Found ", length(available_as_of_dates), " as_of dates (latest: ", available_as_of_dates[1], ")")
  } else {
    warning("  Could not fetch available as_of dates - will use calculated dates")
  }

  # Build clades and as_of dates for all dates
  clades_by_date <- list()
  as_of_dates_by_nowcast <- list()
  current_date <- Sys.Date()

  for (date in all_nowcast_dates) {
    clades_by_date[[date]] <- get_clades_for_date(hub_config, date)
    as_of_dates_by_nowcast[[date]] <- list(
      round_open = as.character(get_monday_before(date)),
      latest = as.character(current_date),  # Will be updated for processed dates
      round_closed = is_round_closed(date)
    )
  }

  # Process each nowcast date
  for (nowcast_date in dates_to_process) {
    message(paste("\n--- Processing nowcast date:", nowcast_date, "---"))

    tryCatch({
      # Get clades for this round
      predicted_clades <- get_clades_for_date(hub_config, nowcast_date)
      clades_by_date[[nowcast_date]] <- predicted_clades

      # Get target dates (daily)
      target_dates <- get_target_dates(nowcast_date)

      # Fetch predictions
      message("  Fetching predictions...")
      predictions <- fetch_hub_predictions(nowcast_date, target_dates)

      if (nrow(predictions) == 0) {
        message("  No predictions found, skipping...")
        next
      }

      # Filter predictions to requested locations
      predictions <- predictions |>
        dplyr::filter(location %in% locations)
      message("  Filtered to ", length(unique(predictions$location)), " location(s)")

      # Process predictions
      message("  Processing predictions...")
      processed <- process_predictions(predictions)

      # Fetch and process target data (round-open version)
      # as_of date for round-open is the Tuesday on or before the nowcast_date
      round_open_as_of <- get_round_open_as_of_date(nowcast_date)
      message("  Fetching target data (round-open, as_of=", round_open_as_of, ")...")
      min_date <- as.Date(nowcast_date) - 96
      max_date <- as.Date(nowcast_date)

      target_data_round_open <- fetch_target_data(
        as_of_date = round_open_as_of,
        nowcast_date = nowcast_date,
        predicted_clades = predicted_clades,
        min_date = min_date,
        max_date = max_date
      )

      if (is.null(target_data_round_open) || nrow(target_data_round_open) == 0) {
        message("    No round-open target data available, skipping...")
        next
      }
      # Filter target data to requested locations
      target_data_round_open <- target_data_round_open |>
        dplyr::filter(location %in% locations)
      target_data_round_open_processed <- process_daily_target_data(target_data_round_open)

      # Fetch latest target data (for comparison)
      # Use the most recent available as_of date from the source repository,
      # constrained by nowcast + 13 weeks
      latest_as_of <- get_latest_as_of_date(nowcast_date, available_as_of_dates)
      latest_max_date <- as.Date(latest_as_of)  # Include all available data up to as_of
      message("  Fetching target data (latest, as_of=", latest_as_of, ")...")

      # Track as_of dates for dashboard-options.json
      as_of_dates_by_nowcast[[nowcast_date]] <- list(
        round_open = as.character(get_monday_before(nowcast_date)),
        latest = as.character(latest_as_of),
        round_closed = is_round_closed(nowcast_date)
      )

      target_data_latest <- fetch_target_data(
        as_of_date = latest_as_of,
        nowcast_date = nowcast_date,
        predicted_clades = predicted_clades,
        min_date = min_date,
        max_date = latest_max_date
      )

      # Fall back to round-open if latest is not available
      if (is.null(target_data_latest) || nrow(target_data_latest) == 0) {
        message("    Could not fetch latest data, using round-open data")
        target_data_latest <- target_data_round_open
      } else {
        # Filter target data to requested locations
        target_data_latest <- target_data_latest |>
          dplyr::filter(location %in% locations)
      }
      target_data_latest_processed <- process_daily_target_data(target_data_latest)

      # Compute multinomial prediction intervals using samples (not means)
      # This properly incorporates both model uncertainty and sampling uncertainty
      message("  Computing multinomial PIs...")
      multinomial_pi <- compute_multinomial_pi(
        sample_predictions = processed$samples,
        target_data = target_data_latest_processed,
        draws_per_sample = 10,  # With ~100 samples, this gives ~1000 total draws
        quantile_probs = QUANTILE_PROBS,
        seed = 42  # For reproducibility
      )
      multinomial_pi_wide <- pivot_multinomial_pi(multinomial_pi)

      # Export JSON files
      message("  Exporting JSON files...")
      export_counts <- export_nowcast_date(
        means = processed$means,
        quantiles = processed$quantiles,
        multinomial_pi = multinomial_pi_wide,
        target_data_round_open = target_data_round_open_processed,
        target_data_latest = target_data_latest_processed,
        nowcast_date = nowcast_date,
        as_of_round_open = round_open_as_of,
        as_of_latest = latest_as_of,
        output_dir = output_dir,
        generate_forecasts = generate_forecasts,
        generate_targets = generate_targets
      )

      message("  Exported: ", export_counts$forecasts, " forecasts, ",
              export_counts$targets_round_open, " round-open targets, ",
              export_counts$targets_latest, " latest targets")

      message("  Done processing ", nowcast_date)

    }, error = function(e) {
      warning(paste("Error processing", nowcast_date, ":", e$message))
    })
  }

  # Generate dashboard-options.json
  message("\nGenerating dashboard-options.json...")

  # Get clade labels for all clades used (identity mapping: clade = label)
  all_clades <- unique(unlist(clades_by_date))
  clade_labels_vec <- stats::setNames(all_clades, all_clades)

  export_options_json(
    locations = US_LOCATIONS,
    location_names = LOCATION_NAMES,
    nowcast_dates = all_nowcast_dates,
    models = all_models,
    initial_selected_models = initial_selected_models,
    clades_by_date = clades_by_date,
    clade_labels = clade_labels_vec,
    as_of_dates_by_nowcast = as_of_dates_by_nowcast,
    current_nowcast_date = tail(all_nowcast_dates, 1),
    output_dir = output_dir
  )

  message("\nPipeline complete!")
  invisible(dates_to_process)
}

# =============================================================================
# CLI Entry Point
# =============================================================================

# If run directly from command line
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  nowcast_dates <- if (length(args) >= 1) args[1] else NULL
  regenerate <- if (length(args) >= 2) as.logical(args[2]) else FALSE

  run_pipeline(nowcast_dates = nowcast_dates, regenerate = regenerate)
}

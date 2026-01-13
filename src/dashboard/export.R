# JSON Export Functions
# =====================
# Functions for exporting dashboard data to JSON format.

library(dplyr)
library(tidyr)
library(jsonlite)

#' Export forecast data to JSON for a specific location and nowcast date
#'
#' @param means Tibble with mean predictions
#' @param quantiles Tibble with quantile predictions
#' @param multinomial_pi Tibble with multinomial PI (wide format) or NULL
#' @param location Location code (e.g., "MA")
#' @param nowcast_date Nowcast/reference date string
#' @param output_dir Base output directory
#' @return Path to written file
export_forecast_json <- function(
    means,
    quantiles,
    multinomial_pi = NULL,
    location,
    nowcast_date,
    output_dir
) {
  # Filter to this location
  means_loc <- means |> dplyr::filter(location == !!location)
  quantiles_loc <- quantiles |> dplyr::filter(location == !!location)

  if (nrow(means_loc) == 0) {
    return(NULL)
  }

  # Build nested structure: models -> clades -> arrays
  models <- unique(means_loc$model_id)

  model_data <- lapply(stats::setNames(models, models), function(model) {
    model_means <- means_loc |>
      dplyr::filter(model_id == !!model) |>
      dplyr::arrange(target_date)

    model_quantiles <- quantiles_loc |>
      dplyr::filter(model_id == !!model) |>
      dplyr::arrange(target_date)

    # Get target dates
    target_dates <- unique(model_means$target_date)

    # Build per-clade data
    clades <- unique(model_means$clade)

    # Use I() to preserve vectors as arrays even with single elements
    clade_data <- lapply(stats::setNames(clades, clades), function(clade) {
      clade_means <- model_means |>
        dplyr::filter(clade == !!clade) |>
        dplyr::arrange(target_date)

      clade_quantiles <- model_quantiles |>
        dplyr::filter(clade == !!clade) |>
        dplyr::arrange(target_date)

      result <- list(mean = I(clade_means$value))

      # Add quantiles
      if (nrow(clade_quantiles) > 0) {
        quantile_probs <- unique(clade_quantiles$output_type_id)
        for (qp in quantile_probs) {
          col_name <- paste0("q", qp)
          result[[col_name]] <- I(clade_quantiles |>
            dplyr::filter(output_type_id == !!qp) |>
            dplyr::arrange(target_date) |>
            dplyr::pull(value))
        }
      }

      # Add multinomial PIs if available
      if (!is.null(multinomial_pi)) {
        model_clade_pi <- multinomial_pi |>
          dplyr::filter(model_id == !!model, clade == !!clade, location == !!location) |>
          dplyr::arrange(target_date)

        if (nrow(model_clade_pi) > 0) {
          pi_cols <- names(model_clade_pi)[grepl("^multinomial_q", names(model_clade_pi))]
          for (col in pi_cols) {
            result[[col]] <- I(model_clade_pi[[col]])
          }
        }
      }

      result
    })

    # Add target_date array to model data
    c(list(target_date = I(as.character(target_dates))), clade_data)
  })

  # Create output structure
  output <- list(
    location = location,
    nowcast_date = nowcast_date,
    models = model_data
  )

  # Write JSON
  dir.create(file.path(output_dir, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, "forecasts", paste0(location, "_", nowcast_date, ".json"))

  jsonlite::write_json(
    output,
    output_path,
    auto_unbox = TRUE,
    digits = 4,
    pretty = FALSE
  )

  output_path
}

#' Export target data to JSON for a specific location and nowcast date
#'
#' @param target_data Tibble with target data (count, proportion, total by clade)
#' @param location Location code
#' @param nowcast_date Nowcast date
#' @param as_of_date Date the target data was fetched
#' @param version Either "round-open" or "latest"
#' @param output_dir Base output directory
#' @return Path to written file
export_target_json <- function(
    target_data,
    location,
    nowcast_date,
    as_of_date,
    version,
    output_dir
) {
  # Filter to this location
  target_loc <- target_data |>
    dplyr::filter(location == !!location) |>
    dplyr::arrange(target_date)

  if (nrow(target_loc) == 0) {
    return(NULL)
  }

  # Get target dates and clades
  target_dates <- unique(target_loc$target_date)
  clades <- unique(target_loc$clade)

  # Build per-clade data
  # Use I() to preserve vectors as arrays even with single elements
  clade_data <- lapply(stats::setNames(clades, clades), function(clade) {
    clade_target <- target_loc |>
      dplyr::filter(clade == !!clade) |>
      dplyr::arrange(target_date)

    list(
      count = I(clade_target$count),
      proportion = I(round(clade_target$proportion, 4)),
      total = I(clade_target$total)
    )
  })

  # Create output structure
  output <- list(
    location = location,
    nowcast_date = nowcast_date,
    as_of = as.character(as_of_date),
    data = c(list(target_date = I(as.character(target_dates))), clade_data)
  )

  # Write JSON
  dir.create(file.path(output_dir, "targets", version), recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, "targets", version, paste0(location, "_", nowcast_date, ".json"))

  jsonlite::write_json(
    output,
    output_path,
    auto_unbox = TRUE,
    digits = 4,
    pretty = FALSE
  )

  output_path
}

#' Export dashboard options configuration
#'
#' @param locations Vector of location codes
#' @param location_names Named vector mapping codes to names
#' @param nowcast_dates Vector of available nowcast dates
#' @param models Vector of model IDs
#' @param initial_selected_models Vector of models to show by default
#' @param clades_by_date Named list mapping dates to clades
#' @param clade_labels Named vector mapping clade codes to labels
#' @param as_of_dates_by_nowcast Named list with round_open and latest dates per nowcast
#' @param current_nowcast_date Most recent nowcast date
#' @param output_dir Output directory
#' @return Path to written file
export_options_json <- function(
    locations,
    location_names,
    nowcast_dates,
    models,
    initial_selected_models,
    clades_by_date,
    clade_labels,
    as_of_dates_by_nowcast,
    current_nowcast_date,
    output_dir
) {
  # Use I() to preserve vectors as arrays even with single elements
  output <- list(
    locations = I(locations),
    location_names = as.list(location_names),
    nowcast_dates = I(nowcast_dates),
    models = I(models),
    initial_selected_models = I(initial_selected_models),
    clades_by_date = clades_by_date,
    clade_labels = as.list(clade_labels),
    as_of_dates = as_of_dates_by_nowcast,
    intervals = c(50, 80, 95),
    default_interval = 80,
    current_nowcast_date = current_nowcast_date,
    last_updated = as.character(Sys.time())
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, "dashboard-options.json")

  jsonlite::write_json(
    output,
    output_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  output_path
}

#' Export all data for a single nowcast date
#'
#' @param means Mean predictions tibble
#' @param quantiles Quantile predictions tibble
#' @param multinomial_pi Multinomial PI tibble (wide format)
#' @param target_data_round_open Target data as of round open
#' @param target_data_latest Target data with latest observations
#' @param nowcast_date Nowcast date
#' @param as_of_round_open Date string for round-open data
#' @param as_of_latest Date string for latest data
#' @param output_dir Output directory
#' @param generate_forecasts If TRUE, export forecast files (default TRUE)
#' @param generate_targets If TRUE, export target files (default TRUE)
#' @return List with counts of exported files
export_nowcast_date <- function(
    means,
    quantiles,
    multinomial_pi,
    target_data_round_open,
    target_data_latest,
    nowcast_date,
    as_of_round_open,
    as_of_latest,
    output_dir,
    generate_forecasts = TRUE,
    generate_targets = TRUE
) {
  locations <- unique(means$location)

  forecast_count <- 0
  target_round_open_count <- 0
  target_latest_count <- 0

  for (loc in locations) {
    # Export forecast
    if (generate_forecasts) {
      result <- export_forecast_json(
        means = means,
        quantiles = quantiles,
        multinomial_pi = multinomial_pi,
        location = loc,
        nowcast_date = nowcast_date,
        output_dir = output_dir
      )
      if (!is.null(result)) forecast_count <- forecast_count + 1
    }

    # Export round-open targets
    if (generate_targets) {
      result <- export_target_json(
        target_data = target_data_round_open,
        location = loc,
        nowcast_date = nowcast_date,
        as_of_date = as_of_round_open,
        version = "round-open",
        output_dir = output_dir
      )
      if (!is.null(result)) target_round_open_count <- target_round_open_count + 1

      # Export latest targets
      result <- export_target_json(
        target_data = target_data_latest,
        location = loc,
        nowcast_date = nowcast_date,
        as_of_date = as_of_latest,
        version = "latest",
        output_dir = output_dir
      )
      if (!is.null(result)) target_latest_count <- target_latest_count + 1
    }
  }

  list(
    forecasts = forecast_count,
    targets_round_open = target_round_open_count,
    targets_latest = target_latest_count
  )
}

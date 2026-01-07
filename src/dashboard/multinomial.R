# Multinomial Prediction Interval Functions
# ==========================================
# Functions for computing prediction intervals based on multinomial sampling.
# These intervals account for BOTH model uncertainty (via samples) AND
# sampling uncertainty (via multinomial draws).

library(dplyr)
library(tidyr)

#' Compute multinomial prediction intervals for full prediction dataset
#'
#' This function properly incorporates both sources of uncertainty:
#' 1. Model uncertainty: by using the sample predictions (not means)
#' 2. Sampling uncertainty: by drawing from multinomial distribution
#'
#' For each model/location/date:
#' - Iterate over each sample prediction (e.g., 100 samples)
#' - For each sample, draw multinomial samples (e.g., 10 draws)
#' - Pool all draws (100 * 10 = 1000) and compute quantiles
#'
#' @param sample_predictions Tibble with columns: model_id, target_date, location,
#'        clade, output_type_id (sample index), value (predicted proportion)
#' @param target_data Tibble with columns: target_date, location, clade, count
#' @param draws_per_sample Number of multinomial draws per model sample (default 10)
#' @param quantile_probs Vector of quantile probabilities to compute
#' @param seed Optional seed for reproducibility
#' @return Tibble with multinomial PI values
compute_multinomial_pi <- function(
    sample_predictions,
    target_data,
    draws_per_sample = 10,
    quantile_probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    seed = NULL
) {
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Get total counts per location-date from target data
  totals <- target_data |>
    dplyr::group_by(target_date, location) |>
    dplyr::summarize(total = sum(count), .groups = "drop")

  # Get unique clades for consistent ordering
  all_clades <- unique(sample_predictions$clade)

  # Process each model-location-date combination
  results <- sample_predictions |>
    dplyr::group_by(model_id, target_date, location) |>
    dplyr::group_split() |>
    lapply(function(group_data) {
      model <- unique(group_data$model_id)
      td <- unique(group_data$target_date)
      loc <- unique(group_data$location)

      # Get observed total for this location-date
      total_row <- totals |>
        dplyr::filter(target_date == td, location == loc)
      observed_total <- if (nrow(total_row) == 0) 0 else total_row$total[1]

      # Handle zero total case - return NAs
      if (is.na(observed_total) || observed_total == 0) {
        return(tidyr::expand_grid(
          model_id = model,
          target_date = td,
          location = loc,
          clade = unique(group_data$clade),
          quantile_prob = quantile_probs
        ) |>
          dplyr::mutate(multinomial_value = NA_real_))
      }

      # Get unique sample IDs
      sample_ids <- unique(group_data$output_type_id)

      # For each sample, draw multinomial samples
      all_proportions <- lapply(sample_ids, function(sample_id) {
        # Get this sample's predicted proportions
        sample_data <- group_data |>
          dplyr::filter(output_type_id == sample_id)

        # Create named vector of proportions
        predicted_props <- stats::setNames(sample_data$value, sample_data$clade)

        # Normalize to ensure sums to 1 (handle floating point issues)
        predicted_props <- predicted_props / sum(predicted_props)

        # Draw from multinomial distribution
        draws <- stats::rmultinom(draws_per_sample, size = observed_total, prob = predicted_props)

        # Convert counts to proportions (matrix: clades x draws)
        draws / observed_total
      })

      # Combine all draws into single matrix (clades x total_draws)
      combined_proportions <- do.call(cbind, all_proportions)

      # Get clade names from the first sample
      clade_names <- names(stats::setNames(
        group_data |> dplyr::filter(output_type_id == sample_ids[1]) |> dplyr::pull(value),
        group_data |> dplyr::filter(output_type_id == sample_ids[1]) |> dplyr::pull(clade)
      ))

      # Compute quantiles for each clade
      clade_results <- lapply(seq_along(clade_names), function(i) {
        clade_proportions <- combined_proportions[i, ]
        quantile_values <- stats::quantile(clade_proportions, probs = quantile_probs, names = FALSE)

        tibble::tibble(
          model_id = model,
          target_date = td,
          location = loc,
          clade = clade_names[i],
          quantile_prob = quantile_probs,
          multinomial_value = round(quantile_values, 4)
        )
      })

      dplyr::bind_rows(clade_results)
    }) |>
    dplyr::bind_rows()

  results
}

#' Pivot multinomial PI from long to wide format
#'
#' @param multinomial_pi Tibble from compute_multinomial_pi
#' @return Tibble with multinomial_q{prob} columns
pivot_multinomial_pi <- function(multinomial_pi) {
  multinomial_pi |>
    dplyr::mutate(quantile_col = paste0("multinomial_q", quantile_prob)) |>
    tidyr::pivot_wider(
      id_cols = c(model_id, target_date, location, clade),
      names_from = quantile_col,
      values_from = multinomial_value
    )
}

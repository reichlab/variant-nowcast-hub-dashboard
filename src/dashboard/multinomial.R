# Multinomial Prediction Interval Functions
# ==========================================
# Functions for computing prediction intervals based on multinomial sampling.
# These intervals account for sampling uncertainty in the target data.

library(dplyr)
library(tidyr)

#' Compute multinomial prediction intervals for a single set of predictions
#'
#' @param predicted_props Named numeric vector of predicted proportions (must sum to 1)
#' @param observed_total Total count of observed sequences (n for multinomial)
#' @param n_draws Number of multinomial draws for simulation (default 1000)
#' @param quantile_probs Vector of quantile probabilities to compute
#' @param seed Optional seed for reproducibility
#' @return Tibble with columns: clade, quantile_prob, multinomial_value
compute_multinomial_pi_single <- function(
    predicted_props,
    observed_total,
    n_draws = 1000,
    quantile_probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    seed = NULL
) {
  # Handle zero total case
  if (is.na(observed_total) || observed_total == 0) {
    return(tidyr::expand_grid(
      clade = names(predicted_props),
      quantile_prob = quantile_probs
    ) |>
      dplyr::mutate(multinomial_value = NA_real_))
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Draw from multinomial distribution
  # rmultinom returns matrix with n_draws columns, nrow = number of categories
  draws <- stats::rmultinom(n_draws, size = observed_total, prob = predicted_props)

  # Convert counts to proportions
  proportions <- draws / observed_total

  # Compute quantiles for each clade
  results <- lapply(seq_along(predicted_props), function(i) {
    clade_name <- names(predicted_props)[i]
    clade_proportions <- proportions[i, ]
    quantile_values <- stats::quantile(clade_proportions, probs = quantile_probs, names = FALSE)

    tibble::tibble(
      clade = clade_name,
      quantile_prob = quantile_probs,
      multinomial_value = round(quantile_values, 4)
    )
  })

  dplyr::bind_rows(results)
}

#' Compute multinomial prediction intervals for full prediction dataset
#'
#' @param predictions Tibble with columns: model_id, target_date, location, clade, value (mean prediction)
#' @param target_data Tibble with columns: target_date, location, clade, count (and optionally total)
#' @param n_draws Number of multinomial draws
#' @param quantile_probs Vector of quantile probabilities
#' @param seed Optional seed for reproducibility
#' @return Tibble with multinomial PI values
compute_multinomial_pi <- function(
    predictions,
    target_data,
    n_draws = 1000,
    quantile_probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    seed = NULL
) {
  # Get total counts per location-date from target data
  totals <- target_data |>
    dplyr::group_by(target_date, location) |>
    dplyr::summarize(total = sum(count), .groups = "drop")

  # Process each model-location-date combination
  results <- predictions |>
    dplyr::group_by(model_id, target_date, location) |>
    dplyr::group_split() |>
    lapply(function(group_data) {
      model <- unique(group_data$model_id)
      target_date <- unique(group_data$target_date)
      location <- unique(group_data$location)

      # Get predicted proportions as named vector
      predicted_props <- stats::setNames(group_data$value, group_data$clade)

      # Normalize to ensure sums to 1 (handle floating point)
      predicted_props <- predicted_props / sum(predicted_props)

      # Get observed total
      total_row <- totals |>
        dplyr::filter(target_date == !!target_date, location == !!location)

      observed_total <- if (nrow(total_row) == 0) 0 else total_row$total[1]

      # Compute multinomial PIs
      pi_result <- compute_multinomial_pi_single(
        predicted_props = predicted_props,
        observed_total = observed_total,
        n_draws = n_draws,
        quantile_probs = quantile_probs,
        seed = seed
      )

      # Add grouping columns
      pi_result |>
        dplyr::mutate(
          model_id = model,
          target_date = target_date,
          location = location
        ) |>
        dplyr::select(model_id, target_date, location, clade, quantile_prob, multinomial_value)
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

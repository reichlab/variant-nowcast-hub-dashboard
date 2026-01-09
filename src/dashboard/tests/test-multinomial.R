# Tests for Multinomial Prediction Interval Functions
# ====================================================

library(testthat)
library(tibble)
library(dplyr)

# Source the module being tested
source(here::here("src/dashboard/config.R"))
source(here::here("src/dashboard/multinomial.R"))

test_that("compute_multinomial_pi returns correct structure", {
  # Create mock sample predictions (simulating 10 samples for simplicity)
  sample_predictions <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:10),
    clade = c("24A", "24B", "other")
  ) |>
    dplyr::mutate(
      # Vary proportions slightly across samples to simulate model uncertainty
      value = dplyr::case_when(
        clade == "24A" ~ 0.5 + rnorm(n(), 0, 0.05),
        clade == "24B" ~ 0.3 + rnorm(n(), 0, 0.03),
        clade == "other" ~ 0.2 + rnorm(n(), 0, 0.02)
      ),
      output_type = "sample"
    )

  # Normalize within each sample
  sample_predictions <- sample_predictions |>
    dplyr::group_by(model_id, target_date, location, output_type_id) |>
    dplyr::mutate(value = value / sum(value)) |>
    dplyr::ungroup()

  # Create mock target data
  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 3),
    location = rep("MA", 3),
    clade = c("24A", "24B", "other"),
    count = c(50, 30, 20)
  )

  result <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data,
    draws_per_sample = 10,
    quantile_probs = c(0.1, 0.5, 0.9),
    seed = 42
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("model_id", "target_date", "location", "clade",
                    "quantile_prob", "multinomial_value") %in% names(result)))

  # Should have results for all clades and quantiles
  expect_equal(nrow(result), 3 * 3)  # 3 clades * 3 quantiles
  expect_setequal(unique(result$clade), c("24A", "24B", "other"))
})

test_that("compute_multinomial_pi is reproducible with seed", {
  # Create mock sample predictions
  set.seed(100)
  sample_predictions <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:10),
    clade = c("24A", "24B")
  ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        clade == "24A" ~ 0.6 + rnorm(n(), 0, 0.05),
        clade == "24B" ~ 0.4 + rnorm(n(), 0, 0.05)
      ),
      output_type = "sample"
    ) |>
    dplyr::group_by(output_type_id) |>
    dplyr::mutate(value = value / sum(value)) |>
    dplyr::ungroup()

  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 2),
    location = rep("MA", 2),
    clade = c("24A", "24B"),
    count = c(60, 40)
  )

  result1 <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data,
    draws_per_sample = 10,
    seed = 123
  )

  result2 <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data,
    draws_per_sample = 10,
    seed = 123
  )

  expect_equal(result1, result2)
})

test_that("compute_multinomial_pi handles zero total", {
  sample_predictions <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:5),
    clade = c("24A", "24B")
  ) |>
    dplyr::mutate(
      value = 0.5,
      output_type = "sample"
    )

  # Zero counts
  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 2),
    location = rep("MA", 2),
    clade = c("24A", "24B"),
    count = c(0, 0)
  )

  result <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data,
    draws_per_sample = 10
  )

  # Should return NA values
  expect_true(all(is.na(result$multinomial_value)))
})

test_that("compute_multinomial_pi has wider intervals for small n", {
  # Create sample predictions with some model uncertainty
  set.seed(42)
  sample_predictions <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:50),
    clade = c("24A", "24B")
  ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        clade == "24A" ~ 0.5 + rnorm(n(), 0, 0.02),
        clade == "24B" ~ 0.5 + rnorm(n(), 0, 0.02)
      ),
      output_type = "sample"
    ) |>
    dplyr::group_by(output_type_id) |>
    dplyr::mutate(value = value / sum(value)) |>
    dplyr::ungroup()

  # Small sample size
  target_data_small <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 2),
    location = rep("MA", 2),
    clade = c("24A", "24B"),
    count = c(5, 5)  # n=10
  )

  # Large sample size
  target_data_large <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 2),
    location = rep("MA", 2),
    clade = c("24A", "24B"),
    count = c(500, 500)  # n=1000
  )

  result_small <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data_small,
    draws_per_sample = 10,
    quantile_probs = c(0.1, 0.9),
    seed = 42
  )

  result_large <- compute_multinomial_pi(
    sample_predictions = sample_predictions,
    target_data = target_data_large,
    draws_per_sample = 10,
    quantile_probs = c(0.1, 0.9),
    seed = 42
  )

  # Calculate interval widths for 24A
  width_small <- result_small$multinomial_value[result_small$clade == "24A" & result_small$quantile_prob == 0.9] -
                 result_small$multinomial_value[result_small$clade == "24A" & result_small$quantile_prob == 0.1]

  width_large <- result_large$multinomial_value[result_large$clade == "24A" & result_large$quantile_prob == 0.9] -
                 result_large$multinomial_value[result_large$clade == "24A" & result_large$quantile_prob == 0.1]

  # Small sample should have wider interval
  expect_gt(width_small, width_large)
})

test_that("compute_multinomial_pi incorporates model uncertainty", {
  # Create sample predictions with HIGH model uncertainty
  set.seed(42)
  sample_predictions_high_var <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:100),
    clade = c("24A", "24B")
  ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        clade == "24A" ~ 0.5 + rnorm(n(), 0, 0.15),  # High variance
        clade == "24B" ~ 0.5 + rnorm(n(), 0, 0.15)
      ),
      output_type = "sample"
    ) |>
    dplyr::group_by(output_type_id) |>
    dplyr::mutate(value = pmax(0.01, value), value = value / sum(value)) |>
    dplyr::ungroup()

  # Create sample predictions with LOW model uncertainty
  sample_predictions_low_var <- tidyr::expand_grid(
    model_id = "model1",
    target_date = as.Date("2025-01-01"),
    location = "MA",
    output_type_id = as.character(1:100),
    clade = c("24A", "24B")
  ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        clade == "24A" ~ 0.5 + rnorm(n(), 0, 0.01),  # Low variance
        clade == "24B" ~ 0.5 + rnorm(n(), 0, 0.01)
      ),
      output_type = "sample"
    ) |>
    dplyr::group_by(output_type_id) |>
    dplyr::mutate(value = pmax(0.01, value), value = value / sum(value)) |>
    dplyr::ungroup()

  # Same target data for both (large n to minimize sampling uncertainty)
  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 2),
    location = rep("MA", 2),
    clade = c("24A", "24B"),
    count = c(5000, 5000)
  )

  result_high_var <- compute_multinomial_pi(
    sample_predictions = sample_predictions_high_var,
    target_data = target_data,
    draws_per_sample = 10,
    quantile_probs = c(0.1, 0.9),
    seed = 42
  )

  result_low_var <- compute_multinomial_pi(
    sample_predictions = sample_predictions_low_var,
    target_data = target_data,
    draws_per_sample = 10,
    quantile_probs = c(0.1, 0.9),
    seed = 42
  )

  # Calculate interval widths
  width_high <- result_high_var$multinomial_value[result_high_var$clade == "24A" & result_high_var$quantile_prob == 0.9] -
                result_high_var$multinomial_value[result_high_var$clade == "24A" & result_high_var$quantile_prob == 0.1]

  width_low <- result_low_var$multinomial_value[result_low_var$clade == "24A" & result_low_var$quantile_prob == 0.9] -
               result_low_var$multinomial_value[result_low_var$clade == "24A" & result_low_var$quantile_prob == 0.1]

  # High model uncertainty should produce wider intervals
  expect_gt(width_high, width_low)
})

test_that("pivot_multinomial_pi creates correct wide format", {
  multinomial_pi <- tibble::tibble(
    model_id = rep("model1", 6),
    target_date = rep(as.Date("2025-01-01"), 6),
    location = rep("MA", 6),
    clade = rep(c("24A", "24B"), each = 3),
    quantile_prob = rep(c(0.025, 0.5, 0.975), 2),
    multinomial_value = c(0.35, 0.40, 0.45, 0.25, 0.30, 0.35)
  )

  result <- pivot_multinomial_pi(multinomial_pi)

  # Check column names
  expect_true("multinomial_q0.025" %in% names(result))
  expect_true("multinomial_q0.5" %in% names(result))
  expect_true("multinomial_q0.975" %in% names(result))

  # Should have 2 rows (one per clade)
  expect_equal(nrow(result), 2)
})

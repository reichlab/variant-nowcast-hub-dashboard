# Tests for Multinomial Prediction Interval Functions
# ====================================================

library(testthat)
library(tibble)
library(dplyr)

# Source the module being tested
source(here::here("src/dashboard/config.R"))
source(here::here("src/dashboard/multinomial.R"))

test_that("compute_multinomial_pi_single returns correct structure", {
  predicted_props <- c("24A" = 0.6, "24B" = 0.3, "other" = 0.1)

  result <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 100,
    n_draws = 100,
    quantile_probs = c(0.1, 0.5, 0.9),
    seed = 42
  )

  # Check structure

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("clade", "quantile_prob", "multinomial_value"))

  # Check we have results for all clades and quantiles
  expect_equal(nrow(result), 3 * 3)  # 3 clades * 3 quantiles
  expect_setequal(unique(result$clade), c("24A", "24B", "other"))
  expect_setequal(unique(result$quantile_prob), c(0.1, 0.5, 0.9))
})

test_that("compute_multinomial_pi_single is reproducible with seed", {
  predicted_props <- c("24A" = 0.6, "24B" = 0.4)

  result1 <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 100,
    n_draws = 100,
    seed = 123
  )

  result2 <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 100,
    n_draws = 100,
    seed = 123
  )

  expect_equal(result1, result2)
})

test_that("compute_multinomial_pi_single handles zero total", {
  predicted_props <- c("24A" = 0.5, "24B" = 0.5)

  result <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 0,
    n_draws = 100
  )

  # Should return NA values
  expect_true(all(is.na(result$multinomial_value)))
})

test_that("compute_multinomial_pi_single median approximates mean for large n", {
  predicted_props <- c("24A" = 0.7, "24B" = 0.3)

  result <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 10000,  # Large sample
    n_draws = 1000,
    quantile_probs = c(0.5),
    seed = 42
  )

  # Median should be close to true proportion for large n
  median_24A <- result$multinomial_value[result$clade == "24A"]
  expect_equal(median_24A, 0.7, tolerance = 0.02)
})

test_that("compute_multinomial_pi_single has wider intervals for small n", {
  predicted_props <- c("24A" = 0.5, "24B" = 0.5)

  result_small <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 10,
    n_draws = 1000,
    quantile_probs = c(0.1, 0.9),
    seed = 42
  )

  result_large <- compute_multinomial_pi_single(
    predicted_props = predicted_props,
    observed_total = 1000,
    n_draws = 1000,
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

test_that("compute_multinomial_pi works with full predictions tibble", {
  # Create mock predictions
  predictions <- tibble::tibble(
    model_id = rep("model1", 4),
    target_date = rep(as.Date("2025-01-01"), 4),
    location = rep("MA", 4),
    clade = c("24A", "24B", "24C", "other"),
    value = c(0.4, 0.3, 0.2, 0.1)
  )

  # Create mock target data
  target_data <- tibble::tibble(
    target_date = rep(as.Date("2025-01-01"), 4),
    location = rep("MA", 4),
    clade = c("24A", "24B", "24C", "other"),
    count = c(40, 30, 20, 10)
  )

  result <- compute_multinomial_pi(
    predictions = predictions,
    target_data = target_data,
    n_draws = 100,
    quantile_probs = c(0.1, 0.5, 0.9),
    seed = 42
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("model_id", "target_date", "location", "clade",
                    "quantile_prob", "multinomial_value") %in% names(result)))

  # Should have results for all clades and quantiles
  expect_equal(nrow(result), 4 * 3)  # 4 clades * 3 quantiles
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

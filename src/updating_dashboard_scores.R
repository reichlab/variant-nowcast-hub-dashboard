library(tidyverse)
library(arrow)
library(tidyr)
# loading the data
scores <- read_tsv("https://covid-variant-nowcast-hub.s3.amazonaws.com/auxiliary-data/scores/scores.tsv")
coverages <- read_parquet("https://covid-variant-nowcast-hub.s3.amazonaws.com/auxiliary-data/scores/coverage.parquet")
coverages <- filter(coverages, scored == T)
scores <- filter(scores, scored == T, status == "success")
#getting the locations and dates to record
locations <- unique(scores$location)
locations <- locations[!is.na(locations)]
coverages$nowcast_date <- as.Date(coverages$nowcast_date)
# computing by nowcast date
coverages <- coverages %>%
  group_by(model_id,nowcast_date,  target_date, location, interval_range) %>%
  summarise(interval_coverage = ifelse(all(interval_coverage == 1), 1, 0))
summaries_nowcast_date <- scores %>%
  group_by(model_id, nowcast_date) %>%
  summarise("Energy Score"      = mean(energy, na.rm = T),
            "Brier Score Point" = mean(brier_point, na.rm = T),
            "Brier Score Dist"  = mean(brier_dist, na.rm = T),
            n = max(sum(!is.na(energy)), sum(!is.na(brier_point))))
quantiles_nowcast_date <- coverages %>%
  group_by(model_id,nowcast_date, interval_range) %>%
  summarise(interval_coverage_mean = mean(interval_coverage, na.rm = T))
quantiles_wide_nowcast_date <- quantiles_nowcast_date %>%
  filter(interval_range %in% c(50, 90)) %>%
  pivot_wider(
    names_from = interval_range,
    values_from = interval_coverage_mean,
    names_prefix = "interval_coverage_"
  ) %>%
  dplyr::rename(
    "Interval Coverage 50" = interval_coverage_50,
    "Interval Coverage 90" = interval_coverage_90
  )
all_data <- left_join(summaries_nowcast_date, quantiles_wide_nowcast_date, by = c("model_id", "nowcast_date"))
all_data <- all_data %>%
  dplyr::relocate(n, .after = last_col())
model_path <- file.path("../scores", "clade_prop","Overall", "nowcast_date")
if(!dir.exists(model_path)){
  dir.create(model_path, recursive = T)
}
write.csv(all_data, file.path(model_path, "scores.csv"), row.names=FALSE)
# scores by location
summaries_location <- scores %>%
  group_by(model_id, location) %>%
  summarise("Energy Score"      = mean(energy, na.rm = T),
            "Brier Score Point" = mean(brier_point, na.rm = T),
            "Brier Score Dist"  = mean(brier_dist, na.rm = T),
            n = max(sum(!is.na(energy)), sum(!is.na(brier_point))))
quantiles_location <- coverages %>%
  group_by(model_id,location, interval_range) %>%
  summarise(interval_coverage_mean = mean(interval_coverage, na.rm = T))
quantiles_wide_location <- quantiles_location %>%
  filter(interval_range %in% c(50, 90)) %>%
  pivot_wider(
    names_from = interval_range,
    values_from = interval_coverage_mean,
    names_prefix = "interval_coverage_"
  ) %>%
  dplyr::rename(
    "Interval Coverage 50" = interval_coverage_50,
    "Interval Coverage 90" = interval_coverage_90
  )
all_data <- left_join(summaries_location, quantiles_wide_location, by = c("model_id", "location"))
all_data <- all_data %>%
  dplyr::relocate(n, .after = last_col())
model_path <- file.path("../scores", "clade_prop","Overall", "location")
if(!dir.exists(model_path)){
  dir.create(model_path, recursive = T)
}
write.csv(all_data, file.path(model_path, "scores.csv"), row.names=FALSE)
# doing the same by horizon
horizons <- seq(-31, 10, by = 1)
nowcast_dates <- unique(scores$nowcast_date)
data_list <- list()
for(h in 1:length(horizons)){

  # Build valid date pairs
  date_pairs <- tibble(
    nowcast_date = as.Date(nowcast_dates),
    target_date  = as.Date(nowcast_dates + horizons[h])
  )
  scores_horizon <- scores %>%
    semi_join(date_pairs, by = c("nowcast_date", "target_date"))

  coverages_horizons <- coverages %>%
    semi_join(date_pairs, by = c("nowcast_date", "target_date"))

  summaries <- scores_horizon %>%
    group_by(model_id) %>%
    summarise(
      "Energy Score"      = mean(energy, na.rm = TRUE),
      "Brier Score Point" = mean(brier_point, na.rm = TRUE),
      "Brier Score Dist"  = mean(brier_dist, na.rm = TRUE),
      n = max(sum(!is.na(energy)), sum(!is.na(brier_point))),
      .groups = "drop"
    )

  quantiles <- coverages_horizons %>%
    group_by(model_id, interval_range) %>%
    summarise(
      interval_coverage_mean = mean(interval_coverage, na.rm = TRUE),
      .groups = "drop"
    )

  quantiles_wide <- quantiles %>%
    filter(interval_range %in% c(50, 90)) %>%
    pivot_wider(
      names_from = interval_range,
      values_from = interval_coverage_mean,
      names_prefix = "interval_coverage_"
    ) %>%
    dplyr::rename(
      "Interval Coverage 50" = interval_coverage_50,
      "Interval Coverage 90" = interval_coverage_90
    )

  all_data <- left_join(summaries, quantiles_wide, by = "model_id")
  all_data$horizon <- rep(horizons[h], times = length(all_data$n) )
  data_list[[h]] <- all_data
}
all_horizons <- do.call(rbind, data_list)
all_horizons <- all_horizons %>%
  dplyr::relocate(n, .after = last_col())
model_path <- file.path("../scores", "clade_prop", "Overall", "horizon")

if(!dir.exists(model_path)){
  dir.create(model_path, recursive = TRUE)
}

write.csv(all_horizons, file.path(model_path, "scores.csv"), row.names=FALSE)
# doing the same for target dates
max_date <- max(scores$target_date)
old_target_date_scores <- read.csv("../scores/clade_prop/Overall/target_date/scores.csv", check.names = FALSE)
#accounting for the overlap in dates
min_date <- as.Date(max(old_target_date_scores$target_date)) -31 + 7
# removing the dates that need to be updated
old_target_date_scores <- filter(old_target_date_scores, target_date < min_date)
target_dates <- min_date:max_date
target_date_list <- list()
for(tar_date in target_dates){
  scores_date <- filter(scores, target_date == as.Date(tar_date))
  coverages_date <- filter(coverages, target_date == as.Date(tar_date))
  summaries <- scores_date %>%
    group_by(model_id) %>%
    summarise("Energy Score"      = mean(energy, na.rm = T),
              "Brier Score Point" = mean(brier_point, na.rm = T),
              "Brier Score Dist"  = mean(brier_dist, na.rm = T),
              n = max(sum(!is.na(energy)), sum(!is.na(brier_point))))
  quantiles <- coverages_date %>%
    group_by(model_id, interval_range) %>%
    summarise(interval_coverage_mean = mean(interval_coverage, na.rm = T))
  quantiles_wide <- quantiles %>%
    filter(interval_range %in% c(50, 90)) %>%
    pivot_wider(
      names_from = interval_range,
      values_from = interval_coverage_mean,
      names_prefix = "interval_coverage_"
    ) %>%
    dplyr::rename(
      "Interval Coverage 50" = interval_coverage_50,
      "Interval Coverage 90" = interval_coverage_90
    )
  all_data <- left_join(summaries, quantiles_wide, by = "model_id")
  all_data$target_date <- rep(as.Date(tar_date), times = length(all_data$n) )
  target_date_list[[tar_date]] <- all_data
}
model_path <- file.path("../scores", "clade_prop","Overall", "target_date")
if(!dir.exists(model_path)){
  dir.create(model_path, recursive = T)
}
all_target_dates <- do.call(rbind, target_date_list)
all_target_dates <- all_target_dates %>%
  dplyr::relocate(n, .after = last_col())
# combining the old and new scores
all_target_dates <- rbind(all_target_dates, old_target_date_scores)
write.csv(all_target_dates, file.path(model_path, "scores.csv"), row.names=FALSE)
# obtaining the Overall scores
summaries <- scores %>%
  group_by(model_id) %>%
  summarise("Energy Score"      = mean(energy, na.rm = T),
            "Brier Score Point" = mean(brier_point, na.rm = T),
            "Brier Score Dist"  = mean(brier_dist, na.rm = T),
            n = max(sum(!is.na(energy)), sum(!is.na(brier_point))))
quantiles <- coverages %>%
  group_by(model_id, interval_range) %>%
  summarise(interval_coverage_mean = mean(interval_coverage, na.rm = T))
quantiles_wide <- quantiles %>%
  filter(interval_range %in% c(50, 90)) %>%
  pivot_wider(
    names_from = interval_range,
    values_from = interval_coverage_mean,
    names_prefix = "interval_coverage_"
  ) %>%
  dplyr::rename(
    "Interval Coverage 50" = interval_coverage_50,
    "Interval Coverage 90" = interval_coverage_90
  )
all_data <- left_join(summaries, quantiles_wide, by = "model_id")
all_data <- all_data %>%
  dplyr::relocate(n, .after = last_col())
model_path <- file.path("../scores", "clade_prop","Overall")
if(!dir.exists(model_path)){
  dir.create(model_path, recursive = T)
}
write.csv(all_data, file.path(model_path, "scores.csv"), row.names=FALSE)

# Dashboard Data Pipeline

This directory contains the R scripts that generate JSON data files for the interactive Explore dashboard.

## Quick Start

```r
# From repo root
source("src/dashboard/pipeline.R")

# Generate data for the latest nowcast date
run_pipeline()

# Regenerate all historical data
run_pipeline(regenerate = TRUE)

# Generate data for specific dates
run_pipeline(nowcast_dates = c("2025-12-24", "2025-12-17"))
```

## Files

| File | Description |
|------|-------------|
| `config.R` | Configuration constants (S3 URLs, quantiles, paths) |
| `pipeline.R` | Main orchestration script |
| `export.R` | JSON export functions |
| `multinomial.R` | Multinomial prediction interval computation |
| `tests/` | Unit tests (run with `testthat::test_dir("tests")`) |

## Dependencies

Install required packages:

```r
install.packages(c("arrow", "dplyr", "tidyr", "jsonlite", "lubridate", "cli"))

# Hubverse packages (from GitHub)
remotes::install_github("hubverse-org/hubData")
remotes::install_github("hubverse-org/hubUtils")
```

## How the Pipeline Works

### 1. Configuration Loading

```r
source("src/dashboard/config.R")
```

Sets up:
- `HUB_BUCKET`: S3 URL for the variant nowcast hub
- `TARGET_BUCKET`: S3 URL for clade count data
- `QUANTILE_PROBS`: Probability levels for quantile computation
- `OUTPUT_DIR`: Where to write JSON files

### 2. Hub Connection

```r
hub_con <- hubData::connect_hub(HUB_BUCKET, output_type_id_datatype = "character")
```

Connects to the hub's model-output data stored in S3 parquet format.

### 3. For Each Nowcast Date

The pipeline processes each nowcast date:

#### a. Fetch Model Predictions

```r
predictions <- hub_con |>
  filter(reference_date == nowcast_date) |>
  collect()
```

Retrieves all model submissions for that round.

#### b. Convert Samples to Quantiles

```r
# Using hubUtils for standard quantile conversion
quantiles <- convert_output_type(predictions, to = "quantile", ...)
```

Transforms sample-based predictions into quantile summaries.

#### c. Compute Multinomial Prediction Intervals

```r
source("src/dashboard/multinomial.R")
multinomial_pis <- compute_multinomial_pi(predictions, target_totals, ...)
```

Generates prediction intervals that account for sampling variability based on actual sequence counts.

#### d. Fetch Target Data

```r
target_data <- fetch_target_data(nowcast_date, as_of = "round_open")
target_data <- fetch_target_data(nowcast_date, as_of = "latest")
```

Retrieves clade count data from the separate target S3 bucket, at two time points:
- **round_open**: Data available when the forecast round opened
- **latest**: Most recent data snapshot

#### e. Export to JSON

```r
source("src/dashboard/export.R")
export_forecast_json(forecast_data, location, nowcast_date)
export_target_json(target_data, version, location, nowcast_date)
```

Writes structured JSON files to `output/forecasts/` and `output/targets/`.

### 4. Dashboard Options Export

After processing all dates:

```r
export_dashboard_options(hub_config, all_nowcast_dates, output_dir)
```

Generates `dashboard-options.json` with global configuration for the frontend.

## Output Structure

```
output/
├── dashboard-options.json
├── forecasts/
│   ├── AL_2024-09-11.json
│   ├── AL_2024-09-18.json
│   └── ...
└── targets/
    ├── round-open/
    │   ├── AL_2024-09-11.json
    │   └── ...
    └── latest/
        ├── AL_2024-09-11.json
        └── ...
```

## Automated Updates (GitHub Actions)

The `.github/workflows/build-data.yaml` workflow runs automatically:

- **Schedule**: Every Thursday at 10:00 UTC (after round closes)
- **Trigger**: Can also be triggered manually via workflow_dispatch

### Manual Trigger Options

From GitHub Actions UI:
- **nowcast_dates**: Comma-separated dates to process (blank = latest)
- **regenerate**: Check to regenerate all historical data
- **publish**: Check to push data to dashboard-data branch

### Workflow Steps

1. Checkout code
2. Setup R environment
3. Install dependencies (hubverse packages, arrow, etc.)
4. Run tests
5. Generate data
6. Checkout `dashboard-data` branch
7. Copy generated files
8. Commit and push

## Customization

### Adding New Quantile Levels

Edit `config.R`:
```r
QUANTILE_PROBS <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
```

Note: Frontend currently supports 50%, 80%, 95% intervals (maps to q0.25/q0.75, q0.1/q0.9, q0.025/q0.975).

### Changing Default Selected Models

Edit `export.R`, function `export_dashboard_options()`:
```r
initial_selected_models <- c("hub-ensemble", "your-model")
```

### Skipping Certain Locations

Edit `pipeline.R`, add to `SKIP_LOCATIONS`:
```r
SKIP_LOCATIONS <- c("US", "PR")  # Add locations to skip
```

## Troubleshooting

### "Error: Unable to connect to hub"

Check that you have network access to S3 and the bucket URL is correct in `config.R`.

### "No predictions found for date X"

The hub may not have data for that round yet. Check the hub's model-output directory.

### "Target data fetch failed"

The clade counts bucket may not have data for that as_of date. Check `TARGET_BUCKET` URL.

### Memory issues with large regeneration

Process dates in batches:
```r
dates <- get_all_nowcast_dates()
for (batch in split(dates, ceiling(seq_along(dates)/10))) {
  run_pipeline(nowcast_dates = batch)
  gc()  # Force garbage collection
}
```

## Testing

```r
testthat::test_dir("src/dashboard/tests")
```

Tests cover:
- Multinomial PI computation
- JSON export structure
- Quantile conversion

---

*Last updated: 2025-01-03*

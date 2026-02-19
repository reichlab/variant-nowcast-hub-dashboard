# Summary: Refresh Latest Targets and Recompute Multinomial PIs for Recent Rounds

## Problem

Two related issues caused stale data on the dashboard:

### 1. Latest target data never updated after initial processing

The weekly scheduled workflow (`build-data.yaml`, Thursdays 14:00 UTC) only processed the single newest nowcast date. Because the run happens 1 day after the Wednesday nowcast date, the "latest" available target data snapshot (Tuesday) was identical to the round-open snapshot. Older nowcast dates were never revisited, so their `targets/latest/` files remained frozen with round-open-era data.

### 2. Multinomial prediction intervals used stale target data totals

The multinomial PIs (`multinomial_q*` columns in forecast JSON files) depend on the total sequence counts from the latest target data — these totals are used as the `size` parameter in `rmultinom()`. With stale target data, the PIs were computed using round-open-era totals (lower counts), producing wider intervals than the actual latest data would warrant.

## Changes

### `src/dashboard/pipeline.R`

Replaced the existing target-only refresh loop (from the previous PR) with a comprehensive refresh that also recomputes multinomial PIs and re-exports forecast files.

**New refresh loop** (after the main processing loop, before `dashboard-options.json` export):

1. Identifies all nowcast dates from the last ~13 weeks whose rounds are still open (`!is_round_closed()`)
2. Excludes dates already fully processed in the main loop
3. For each remaining date:
   - **Fetches latest target data** via `get_latest_as_of_date()` + `fetch_target_data()` using the most recent available `as_of` snapshot
   - **Exports updated `targets/latest/` files** (guarded by `generate_targets` flag)
   - **Re-fetches predictions** from the hub S3 bucket via `fetch_hub_predictions()`
   - **Re-processes predictions** to extract samples, means, and quantiles via `process_predictions()`
   - **Recomputes multinomial PIs** via `compute_multinomial_pi()` using the refreshed target data totals
   - **Re-exports forecast files** with updated PI columns via `export_forecast_json()` (guarded by `generate_forecasts` flag)
   - **Updates `as_of_dates_by_nowcast` metadata** so `dashboard-options.json` reflects the actual `as_of` date
4. Each date is wrapped in `tryCatch` so a failure on one date doesn't block others
5. The outer condition is `generate_targets || generate_forecasts` — the loop runs if either flag is set, with each export type individually guarded

**No new functions introduced** — the implementation reuses existing pipeline functions:
- `is_round_closed()`, `get_clades_for_date()`, `get_latest_as_of_date()`
- `fetch_target_data()`, `fetch_hub_predictions()`
- `process_predictions()`, `process_daily_target_data()`
- `compute_multinomial_pi()`, `pivot_multinomial_pi()`
- `export_target_json()`, `export_forecast_json()`

### `src/dashboard/tests/test-pipeline-integration.R`

Updated the forecast file count assertion. Previously it expected exactly 2 forecast files (1 per test location for the single test date). Now the refresh loop generates additional forecast files for recent dates, so the assertion checks:
- At least 2 forecast files exist (for the test locations)
- All forecast files are for test locations only (not all 52 states)

### No changes to `build-data.yaml`

The workflow's copy steps already handle the additional files:
- `cp -r output/forecasts/*` copies the regenerated forecast files
- `cp -r output/targets/latest/*` copies the refreshed latest target files

## Effect

Each weekly pipeline run now:
- Fully processes the newest nowcast date (as before)
- Refreshes latest target data for ~12 additional recent dates with genuinely newer `as_of` snapshots
- Recomputes multinomial PIs for those dates using updated sequence count totals, producing correctly calibrated prediction intervals
- Re-exports forecast JSON files with the corrected PI columns

# Developer Guide: Interactive Explore Dashboard

This guide provides context for developers working on the interactive "Explore" dashboard feature of the Variant Nowcast Hub Dashboard.

## Overview

The dashboard has two visualization approaches:

1. **Static Weekly Reports** (`pages/*-report.md`) - Pre-rendered R Markdown reports generated weekly, embedded as HTML fragments. These are the original visualization approach.

2. **Interactive Explore Page** (`pages/explore.qmd`) - A new dynamic, client-side dashboard built with Observable Framework (OJS) in Quarto. This is what this guide covers.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Data Pipeline (R)                           │
│  src/dashboard/pipeline.R → Generates JSON files                    │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Data Storage (GitHub)                          │
│  dashboard-data branch: forecasts/, targets/, dashboard-options.json│
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Frontend (Quarto + OJS)                        │
│  pages/explore.qmd → Fetches JSON, renders Plotly charts            │
└─────────────────────────────────────────────────────────────────────┘
```

## Key Files

### R Pipeline (`src/dashboard/`)

| File | Purpose |
|------|---------|
| `config.R` | Constants: S3 bucket URLs, quantile probabilities, file paths |
| `pipeline.R` | Main orchestration: fetches hub data, processes predictions, exports JSON |
| `export.R` | JSON serialization functions for forecasts, targets, and config |
| `multinomial.R` | Computes multinomial prediction intervals from model predictions |
| `tests/` | Unit tests for core functions |

### Frontend (`pages/`)

| File | Purpose |
|------|---------|
| `explore.qmd` | The interactive dashboard page (Quarto + Observable JS) |
| `_quarto.yml` | Site configuration, navigation |
| `.gitignore` | Excludes `_site/output` symlink for local development |

### GitHub Actions (`.github/workflows/`)

| File | Purpose |
|------|---------|
| `build-data.yaml` | Weekly data generation workflow (runs Thursdays after round closes) |

### Generated Data (`output/` locally, `dashboard-data` branch in production)

```
output/
├── dashboard-options.json    # Global config: locations, dates, models, clades
├── forecasts/
│   └── {location}_{date}.json   # Model predictions per location/date
└── targets/
    ├── round-open/
    │   └── {location}_{date}.json   # Target data as of round open
    └── latest/
        └── {location}_{date}.json   # Target data as of latest update
```

## Data Flow

### 1. Data Generation (R Pipeline)

```r
# Run from repo root
source("src/dashboard/pipeline.R")
run_pipeline()  # Processes latest nowcast date
run_pipeline(regenerate = TRUE)  # Regenerates all historical data
```

The pipeline:
1. Connects to the hub S3 bucket via `hubData::connect_hub()`
2. Fetches model predictions (samples) for each nowcast date
3. Converts samples to quantiles using `hubUtils::convert_output_type()`
4. Computes multinomial prediction intervals
5. Fetches target data from the clade counts S3 bucket
6. Exports everything as JSON files

### 2. Frontend Rendering (Observable JS)

The `explore.qmd` page:
1. Fetches `dashboard-options.json` on load
2. Reactively fetches forecast/target JSON based on user selections
3. Builds Plotly traces for each selected model/clade
4. Renders a faceted subplot layout (one subplot per clade)

Key reactive variables:
- `config` - Global configuration
- `forecastData` - Current forecast JSON
- `targetDataByVersion` - Target data for selected versions (round-open/latest)
- `selectedModels`, `selectedClades` - User checkbox selections
- `buildTraces()`, `buildLayout()` - Plotly chart construction

### 3. State Persistence

User selections persist across date changes via `window._dashboardState`:

```javascript
window._dashboardState = {
  modelSelections: [...],      // Selected model names
  cladeSelections: [...],      // Selected clade names
  dataVersionSelections: [...], // "round-open", "latest"
  intervalSettings: {
    showIntervals: true,
    intervalLevel: 80,
    intervalType: "confidence"
  }
}
```

## Local Development

### Prerequisites
- R 4.3+ with packages: hubData, hubUtils, arrow, dplyr, tidyr, jsonlite
- Quarto CLI
- Node.js (for Quarto's OJS runtime)

### Setup

1. **Generate data locally:**
   ```r
   source("src/dashboard/pipeline.R")
   run_pipeline()
   ```
   This creates `output/` directory with JSON files.

2. **Create symlink for local serving:**
   ```bash
   mkdir -p pages/_site
   ln -s ../../output pages/_site/output
   ```

3. **Start Quarto preview:**
   ```bash
   cd pages
   quarto preview --port 4444
   ```

4. **Open browser:** http://localhost:4444/explore.html

The frontend detects `localhost` and fetches from `/output` instead of the GitHub raw URL.

### Testing Changes

- **R pipeline changes:** Re-run `run_pipeline()` and refresh browser
- **Frontend changes:** Quarto auto-reloads on file save
- **Check browser console:** Data fetching logs and errors appear there

## JSON Schemas

### dashboard-options.json

```json
{
  "locations": ["AL", "AK", ...],
  "location_names": {"AL": "Alabama", ...},
  "nowcast_dates": ["2024-09-11", ...],
  "current_nowcast_date": "2025-12-24",
  "models": ["hub-ensemble", ...],
  "initial_selected_models": ["hub-ensemble"],
  "clades_by_date": {
    "2024-09-11": ["24A", "24B", "other"],
    ...
  },
  "clade_labels": {"24A": "24A (JN.1)", ...},
  "as_of_dates": {
    "2024-09-11": {
      "round_open": "2024-09-11",
      "latest": "2025-01-02",
      "round_closed": true
    },
    ...
  }
}
```

### forecasts/{location}_{date}.json

```json
{
  "location": "MA",
  "nowcast_date": "2025-12-24",
  "models": {
    "hub-ensemble": {
      "target_date": ["2025-11-23", "2025-11-30", ...],
      "24A": {
        "mean": [0.45, 0.42, ...],
        "q0.025": [...], "q0.1": [...], "q0.25": [...],
        "q0.5": [...], "q0.75": [...], "q0.9": [...], "q0.975": [...],
        "multinomial_q0.025": [...], "multinomial_q0.1": [...],
        "multinomial_q0.9": [...], "multinomial_q0.975": [...]
      },
      "24B": { ... },
      "other": { ... }
    }
  }
}
```

### targets/{version}/{location}_{date}.json

```json
{
  "location": "MA",
  "nowcast_date": "2025-12-24",
  "as_of": "2025-12-22",
  "data": {
    "target_date": ["2025-11-16", "2025-11-23", ...],
    "24A": {
      "count": [150, 142, ...],
      "proportion": [0.45, 0.42, ...],
      "total": [333, 338, ...]
    }
  }
}
```

## Key Concepts

### Prediction Intervals vs Confidence Intervals

- **Confidence intervals** (`q0.1`, `q0.9`, etc.): Uncertainty in the model's predicted proportion
- **Prediction intervals** (`multinomial_q0.1`, etc.): Uncertainty including sampling variability (what we'd observe if we sequenced N samples)

The frontend allows toggling between these. Prediction intervals require "latest" target data (to know actual sequence counts).

### Target Data Versions

- **round-open**: Data available when the forecast round opened (what modelers saw)
- **latest**: Most recent data update (shows how predictions compare to "truth")

Users can display both simultaneously with different marker styles.

### Model Colors

Colors are assigned consistently based on model order in `config.models`. The same model always gets the same color across dates.

## Common Tasks

### Add a new model to defaults

Edit `src/dashboard/export.R`, function `export_dashboard_options()`:
```r
initial_selected_models <- c("hub-ensemble", "new-model-name")
```

### Change default interval level

Edit `pages/explore.qmd`, the `viewof intervalLevel` section.

### Add a new clade label

The pipeline auto-generates clade labels from hub config. To customize, edit `export_dashboard_options()` in `export.R`.

### Debug data fetching

Open browser DevTools console. The frontend logs:
- "Config loaded: ..." on startup
- "Fetching forecast: ..." on selection change
- "Fetching target: ..." for each target data version

## Known Issues

See `docs/KNOWN_ISSUES.md` for:
- File size optimization options
- Model/clade selection persistence across dates
- URL permanence / deep linking

---

*Last updated: 2025-01-03*

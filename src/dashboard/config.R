# Dashboard Configuration
# =======================
# Constants and configuration for the dashboard data pipeline.

# Hub Configuration
HUB_BUCKET <- "s3://covid-variant-nowcast-hub"
HUB_CONFIG_URL <- "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/main/hub-config/tasks.json"

# Target Data Configuration
TARGET_DATA_BUCKET <- "https://covid-clade-counts.s3.amazonaws.com"
TARGET_DATA_HUB_URL <- "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/main/target-data/time-series"
TARGET_DATA_API_URL <- "https://api.github.com/repos/reichlab/variant-nowcast-hub/contents/target-data/time-series"

# Maximum weeks after nowcast date that target data is available
TARGET_DATA_MAX_WEEKS <- 13

# Clade Labels
CLADE_LABELS_URL <- "https://nextstrain.org/charon/getSourceInfo?prefix=nextstrain.org/ncov/gisaid/global/all-time"

# Quantile Configuration
# For 50%, 80%, 95% prediction intervals
QUANTILE_PROBS <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

# Multinomial PI Configuration
N_MULTINOMIAL_DRAWS <- 1000

# Output Configuration
OUTPUT_DIR <- here::here("output")

# Default models to show in dashboard
DEFAULT_INITIAL_MODELS <- c("Hub-ensemble")

# US Locations (states + DC + PR)
US_LOCATIONS <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
  "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
  "WI", "WY"
)

# Location Names Mapping
LOCATION_NAMES <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
  "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
  "DC" = "District of Columbia", "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii",
  "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
  "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine",
  "MD" = "Maryland", "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota",
  "MS" = "Mississippi", "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska",
  "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey", "NM" = "New Mexico",
  "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio",
  "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "PR" = "Puerto Rico",
  "RI" = "Rhode Island", "SC" = "South Carolina", "SD" = "South Dakota", "TN" = "Tennessee",
  "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont", "VA" = "Virginia",
  "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming"
)

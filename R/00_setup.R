# =============================================================================
# 00_setup.R
# Install required packages and check environment.
# Run this once before executing any other script.
# =============================================================================

required_packages <- c(
  "tidyverse",    # data manipulation + ggplot2
  "depmixS4",     # Hidden Markov Models
  "fixest",       # fast fixed-effects regression
  "patchwork",    # figure composition
  "ggrepel",      # non-overlapping text labels
  "WDI",          # World Bank data API
  "broom"         # tidy model outputs
)

missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

message("All packages available.")

# =============================================================================
# BASE path — edit if cloned to a different location
# =============================================================================

# Option 1: auto-detect from RStudio active file
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  BASE <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Option 2: set manually
  BASE <- getwd()
}

message("BASE: ", BASE)

# =============================================================================
# setup.R
# Package loading, global options, and environment configuration
# =============================================================================

# Package management function
ensure_pkgs <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) {
    install.packages(to_install, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# Load required packages
ensure_pkgs(c(
  # Data manipulation
  "tidyverse", "lubridate", "zoo",
  
  # Data acquisition
  "quantmod", "readxl",
  
  # Statistical analysis
  "broom", "moments", "lmtest", "sandwich", "strucchange", "tseries",
  
  # Time series
  "forecast", "slider",
  
  # Visualization
  "scales", "gridExtra", "grid", "reshape2",
  
  # Tables and reporting
  "knitr", "kableExtra", "modelsummary",
  
  # Utilities
  "glue"
))

# Global options
options(stringsAsFactors = FALSE)
options("getSymbols.warning4.0" = FALSE)
options(scipen = 10)  # Prefer fixed notation over scientific

# ggplot2 theme defaults
theme_set(theme_minimal(base_family = "serif", base_size = 11))
update_geom_defaults("histogram", list(color = "white"))

# knitr defaults for document-wide code chunk options
knitr::opts_chunk$set(
  echo = FALSE,           # Hide code in final output
  warning = FALSE,        # Suppress warnings
  message = FALSE,        # Suppress messages
  fig.align = "center",   # Center figures
  fig.width = 10,         # Default figure width
  fig.height = 6,         # Default figure height
  cache = FALSE           # Disable caching for reproducibility
)

# Color palette for consistent visualizations
colors_custom <- list(
  primary = "#1f77b4",
  secondary = "#ff7f0e",
  negative = "#d62728",
  positive = "#2ca02c",
  neutral = "gray60",
  recession_shade = "gray80",
  expansion = "steelblue",
  recession = "darkorange"
)

# Export to global environment
.GlobalEnv$colors_custom <- colors_custom

cat("✓ Setup complete: Packages loaded, options configured\n")


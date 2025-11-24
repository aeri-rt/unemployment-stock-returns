# =============================================================================
# data_loading.R
# Fetch macroeconomic and financial data from FRED and Yahoo Finance
# =============================================================================

#' Load all required data for analysis
#'
#' Fetches unemployment rate, recession indicator, real PCE, and S&P 500 data
#' from Federal Reserve Economic Data (FRED) and Yahoo Finance.
#'
#' @return Named list containing xts objects for each data series
#' @export
load_raw_data <- function() {
  
  cat("Fetching data from FRED and Yahoo Finance...\n")
  
  # Suppress quantmod messages
  suppressMessages({
    # FRED data
    getSymbols("UNRATE",  src = "FRED")   # US Unemployment Rate
    getSymbols("USREC",   src = "FRED")   # NBER Recession Indicator
    getSymbols("PCECC96", src = "FRED")   # Real Personal Consumption Expenditures
    
    # Yahoo Finance data (must assign to variable explicitly)
    SP500 <- getSymbols("^GSPC", 
                        src = "yahoo", 
                        from = "1950-01-01",
                        auto.assign = FALSE)
  })
  
  # Return as named list
  data_list <- list(
    UNRATE = get("UNRATE"),
    USREC = get("USREC"),
    PCECC96 = get("PCECC96"),
    SP500 = SP500
  )
  
  # Print data ranges for verification
  cat("\n=== Data Coverage ===\n")
  cat(sprintf("UNRATE:  %s to %s\n", start(data_list$UNRATE), end(data_list$UNRATE)))
  cat(sprintf("USREC:   %s to %s\n", start(data_list$USREC), end(data_list$USREC)))
  cat(sprintf("PCECC96: %s to %s\n", start(data_list$PCECC96), end(data_list$PCECC96)))
  cat(sprintf("SP500:   %s to %s\n", start(data_list$SP500), end(data_list$SP500)))
  cat("\n")
  
  return(data_list)
}

cat("✓ Data loading functions defined\n")


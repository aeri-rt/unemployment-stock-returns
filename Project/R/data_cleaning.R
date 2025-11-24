# =============================================================================
# data_cleaning.R
# Data transformations: convert to returns, growth rates, and merge datasets
# =============================================================================

#' Transform raw data and merge into analysis-ready tibble
#'
#' @param raw_data List containing xts objects from load_raw_data()
#' @param include_pce Logical: include PCECC96 (quarterly, reduces to ~301 obs) or not (monthly, ~907 obs)
#' @return Tibble with monthly observations, all variables aligned
#' @export
clean_and_merge_data <- function(raw_data, include_pce = FALSE) {
  
  cat("Transforming and merging data...\n")
  
  # Extract data from list
  SP500_raw <- raw_data$SP500
  UNRATE_raw <- raw_data$UNRATE
  USREC_raw <- raw_data$USREC
  PCECC96_raw <- raw_data$PCECC96
  
  # ===== S&P 500: Daily -> Monthly Log Returns =====
  sp500_monthly <- to.monthly(SP500_raw, indexAt = "lastof", OHLC = TRUE)
  sp500_close <- Cl(sp500_monthly)
  sp500_returns <- diff(log(sp500_close)) * 100  # Log returns in percentage
  
  sp500_tbl <- tibble(
    ym = zoo::as.yearmon(index(sp500_returns)),
    SP500_ret = as.numeric(sp500_returns)
  )
  
  # ===== Unemployment Rate: No transformation needed =====
  unrate_tbl <- tibble(
    ym = zoo::as.yearmon(index(UNRATE_raw)),
    UNRATE = as.numeric(UNRATE_raw)
  )
  
  # ===== Recession Indicator: Convert to integer =====
  usrec_tbl <- tibble(
    ym = zoo::as.yearmon(index(USREC_raw)),
    USREC = as.integer(USREC_raw)
  )
  
  # ===== Merge all series on common monthly dates =====
  # Conditional merge based on include_pce parameter
  if (include_pce) {
    # With PCE (quarterly, ~301 observations)
    cat("Including PCECC96 (quarterly data - reduces sample to ~301 observations)\n")
    
    pcec_tbl <- tibble(
      ym = zoo::as.yearmon(index(PCECC96_raw)),
      PCECC96 = as.numeric(PCECC96_raw)
    ) |>
      arrange(ym) |>
      mutate(PCEC96_growth = (PCECC96 / dplyr::lag(PCECC96) - 1) * 100)
    
    cat(sprintf("Pre-merge sizes: SP500=%d, UNRATE=%d, USREC=%d, PCE=%d\n",
                nrow(sp500_tbl), nrow(unrate_tbl), nrow(usrec_tbl), nrow(pcec_tbl)))
    
    df_merged <- sp500_tbl |>
      inner_join(unrate_tbl, by = "ym") |>
      inner_join(usrec_tbl, by = "ym") |>
      inner_join(pcec_tbl, by = "ym") |>
      drop_na(SP500_ret, UNRATE, USREC, PCEC96_growth) |>
      mutate(
        date = as.Date(zoo::as.yearmon(ym), frac = 1),
        USREC_factor = factor(USREC, levels = c(0, 1), 
                             labels = c("Expansion", "Recession"))
      ) |>
      dplyr::select(date, ym, UNRATE, PCEC96_growth, SP500_ret, USREC, USREC_factor) |>
      arrange(date)
    
  } else {
    # Without PCE (monthly, ~907 observations)
    cat("Excluding PCECC96 - using full monthly sample (~907 observations)\n")
    
    cat(sprintf("Pre-merge sizes: SP500=%d, UNRATE=%d, USREC=%d\n",
                nrow(sp500_tbl), nrow(unrate_tbl), nrow(usrec_tbl)))
    
    df_merged <- sp500_tbl |>
      inner_join(unrate_tbl, by = "ym") |>
      inner_join(usrec_tbl, by = "ym") |>
      drop_na(SP500_ret, UNRATE, USREC) |>
      mutate(
        date = as.Date(zoo::as.yearmon(ym), frac = 1),
        USREC_factor = factor(USREC, levels = c(0, 1), 
                             labels = c("Expansion", "Recession"))
      ) |>
      dplyr::select(date, ym, UNRATE, SP500_ret, USREC, USREC_factor) |>
      arrange(date)
  }
  
  # ===== Summary statistics =====
  cat("\n=== Dataset Summary ===\n")
  cat(sprintf("Time Range:       %s to %s\n", min(df_merged$date), max(df_merged$date)))
  cat(sprintf("Observations:     %d months\n", nrow(df_merged)))
  cat(sprintf("Variables:        %d\n", ncol(df_merged)))
  cat(sprintf("Recession months: %d (%.1f%% of sample)\n", 
              sum(df_merged$USREC), 
              100 * mean(df_merged$USREC)))
  cat("\n")
  
  return(df_merged)
}


#' Add control variables to existing dataset
#'
#' @param df_main Main dataset from clean_and_merge_data()
#' @param control_data List containing control variable xts objects
#' @return Tibble with control variables added
#' @export
add_control_variables <- function(df_main, control_data) {
  
  cat("Adding control variables...\n")
  
  # Federal Funds Rate (already in %)
  dff_tbl <- tibble(
    ym = zoo::as.yearmon(index(control_data$DFF)),
    DFF = as.numeric(control_data$DFF)
  )
  
  # VIX (already in %)
  vix_tbl <- tibble(
    ym = zoo::as.yearmon(index(control_data$VIX)),
    VIX = as.numeric(control_data$VIX)
  )
  
  # Merge with main data
  df_augmented <- df_main |>
    left_join(dff_tbl, by = "ym") |>
    left_join(vix_tbl, by = "ym") |>
    drop_na()  # Remove rows where controls are missing
  
  cat(sprintf("✓ Augmented dataset: %d observations with control variables\n\n", 
              nrow(df_augmented)))
  
  return(df_augmented)
}


#' Create period indicators for structural break analysis
#'
#' @param df Dataset with 'date' column
#' @return Dataset with period indicators added
#' @export
add_period_indicators <- function(df) {
  
  df <- df |>
    mutate(
      # Three-period classification
      period = case_when(
        date < as.Date("2008-01-01") ~ "Pre-2008",
        date >= as.Date("2008-01-01") & date < as.Date("2020-01-01") ~ "Post-2008",
        date >= as.Date("2020-01-01") ~ "Post-2020"
      ),
      period = factor(period, levels = c("Pre-2008", "Post-2008", "Post-2020")),
      
      # Binary pre/post 2008 indicator
      post2008 = as.integer(date >= as.Date("2008-01-01")),
      
      # Binary pre/post 2020 indicator
      post2020 = as.integer(date >= as.Date("2020-01-01"))
    )
  
  return(df)
}

cat("✓ Data cleaning functions defined\n")


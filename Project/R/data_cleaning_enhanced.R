# =============================================================================
# data_cleaning_enhanced.R
# Merge original data with enhanced policy/market variables
# =============================================================================

#' Clean and Merge Enhanced Dataset
#'
#' Takes original data + enhanced variables and creates analysis-ready dataset
#' 
#' @param raw_data List from load_raw_data()
#' @param enhanced_vars List from load_enhanced_variables()
#' @param include_pce Logical, whether to include PCE (reduces sample to quarterly)
#' @return Tibble with all variables merged at monthly frequency
#' @export
clean_and_merge_enhanced <- function(raw_data, enhanced_vars, include_pce = FALSE) {
  
  message("\n=== Cleaning and Merging Enhanced Dataset ===\n")
  
  # Start with original cleaned data (UNRATE, USREC, SP500_ret, optionally PCE)
  df_base <- clean_and_merge_data(raw_data, include_pce = include_pce)
  
  message(sprintf("Base dataset: %d observations from %s to %s",
                  nrow(df_base),
                  format(min(df_base$date), "%Y-%m"),
                  format(max(df_base$date), "%Y-%m")))
  
  # Convert enhanced variables to monthly and merge
  
  # 1. Policy Stance (already monthly from create_policy_stance_composite)
  if (!is.null(enhanced_vars$policy_stance)) {
    policy_df <- data.frame(
      date = as.Date(zoo::index(enhanced_vars$policy_stance), frac = 1),  # End-of-month dates
      PolicyStance = as.numeric(enhanced_vars$policy_stance$PolicyStance),
      PolicyAccommodation = as.numeric(enhanced_vars$policy_stance$PolicyAccommodation)
    )
    df_base <- df_base |>
      left_join(policy_df, by = "date")
    message(sprintf("✓ Policy Stance merged (%d non-missing observations)",
                    sum(!is.na(df_base$PolicyStance))))
  }
  
  # 2. VIX (convert daily to monthly average)
  if (!is.null(enhanced_vars$vix)) {
    vix_monthly <- xts::to.monthly(enhanced_vars$vix, OHLC = FALSE)
    vix_df <- data.frame(
      date = as.Date(zoo::index(vix_monthly), frac = 1),  # End-of-month dates
      VIX = as.numeric(vix_monthly)
    )
    df_base <- df_base |>
      left_join(vix_df, by = "date")
    message(sprintf("✓ VIX merged (%d non-missing observations)",
                    sum(!is.na(df_base$VIX))))
  }
  
  # 3. 10-Year Treasury (convert to monthly average, then first difference)
  if (!is.null(enhanced_vars$treasury_10y)) {
    treas_monthly <- xts::to.monthly(enhanced_vars$treasury_10y, OHLC = FALSE)
    treas_df <- data.frame(
      date = as.Date(zoo::index(treas_monthly), frac = 1),  # End-of-month dates
      Treasury_10Y = as.numeric(treas_monthly)
    )
    df_base <- df_base |>
      left_join(treas_df, by = "date") |>
      mutate(Treasury_10Y_change = Treasury_10Y - lag(Treasury_10Y))
    message(sprintf("✓ 10Y Treasury merged (%d non-missing)",
                    sum(!is.na(df_base$Treasury_10Y))))
  }
  
  # 4. Term Spread (already calculated as 10Y - 2Y)
  if (!is.null(enhanced_vars$term_spread)) {
    spread_monthly <- xts::to.monthly(enhanced_vars$term_spread, OHLC = FALSE)
    spread_df <- data.frame(
      date = as.Date(zoo::index(spread_monthly), frac = 1),  # End-of-month dates
      Term_Spread = as.numeric(spread_monthly)
    )
    df_base <- df_base |>
      left_join(spread_df, by = "date")
    message(sprintf("✓ Term Spread merged (%d non-missing)",
                    sum(!is.na(df_base$Term_Spread))))
  }
  
  # Create interaction terms for convenience
  df_base <- df_base |>
    mutate(
      # Unemployment × Policy Accommodation (main Fed Put test)
      UNRATE_x_Policy = UNRATE * PolicyAccommodation,
      
      # Unemployment × VIX (risk premium channel)
      UNRATE_x_VIX = UNRATE * VIX,
      
      # Create ZLB indicator (Fed Funds at or near zero)
      ZLB_period = if_else(!is.na(PolicyStance) & PolicyStance <= 0.25, 1, 0),
      
      # Create regime indicators
      Pre2008 = if_else(date < as.Date("2008-01-01"), 1, 0),
      Post2008 = if_else(date >= as.Date("2008-01-01"), 1, 0),
      ZLB1 = if_else(date >= as.Date("2008-12-01") & date < as.Date("2016-01-01"), 1, 0),
      ZLB2 = if_else(date >= as.Date("2020-03-01") & date < as.Date("2022-04-01"), 1, 0)
    )
  
  message(sprintf("\n✓ Final dataset: %d observations with %d variables",
                  nrow(df_base), ncol(df_base)))
  
  # Summary of ZLB periods
  zlb_count <- sum(df_base$ZLB_period, na.rm = TRUE)
  message(sprintf("  - ZLB periods: %d months (%.1f%% of sample)",
                  zlb_count, 100 * zlb_count / nrow(df_base)))
  
  return(df_base)
}


#' Quick Summary of Enhanced Dataset
#'
#' @param df Enhanced dataset from clean_and_merge_enhanced()
#' @export
summarize_enhanced_data <- function(df) {
  
  cat("\n=== Enhanced Dataset Summary ===\n\n")
  
  cat("Sample Period:\n")
  cat(sprintf("  From: %s\n", format(min(df$date), "%B %Y")))
  cat(sprintf("  To:   %s\n", format(max(df$date), "%B %Y")))
  cat(sprintf("  N:    %d months\n\n", nrow(df)))
  
  cat("Variable Coverage (non-missing observations):\n")
  coverage <- data.frame(
    Variable = names(df),
    Non_Missing = sapply(df, function(x) sum(!is.na(x))),
    Coverage_Pct = sapply(df, function(x) 100 * sum(!is.na(x)) / length(x))
  ) |>
    filter(!Variable %in% c("date")) |>
    arrange(desc(Non_Missing))
  
  print(coverage, row.names = FALSE)
  
  cat("\n\nPolicy Regime Breakdown:\n")
  cat(sprintf("  Pre-2008:           %d months\n", sum(df$Pre2008)))
  cat(sprintf("  Post-2008:          %d months\n", sum(df$Post2008)))
  cat(sprintf("  ZLB Period 1 (2008-2015): %d months\n", sum(df$ZLB1)))
  cat(sprintf("  ZLB Period 2 (2020-2022): %d months\n", sum(df$ZLB2)))
  
  cat("\n\nPolicy Stance Statistics:\n")
  if ("PolicyStance" %in% names(df)) {
    cat(sprintf("  Mean:   %.2f%%\n", mean(df$PolicyStance, na.rm = TRUE)))
    cat(sprintf("  Median: %.2f%%\n", median(df$PolicyStance, na.rm = TRUE)))
    cat(sprintf("  Min:    %.2f%%\n", min(df$PolicyStance, na.rm = TRUE)))
    cat(sprintf("  Max:    %.2f%%\n", max(df$PolicyStance, na.rm = TRUE)))
  }
  
  cat("\n")
}


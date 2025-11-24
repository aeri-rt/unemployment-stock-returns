# =============================================================================
# descriptive_stats.R
# Calculate summary statistics, correlation matrices, and descriptive tables
# =============================================================================

#' Calculate comprehensive summary statistics for continuous variables
#'
#' @param df Dataset containing variables to summarize
#' @param vars Character vector of variable names (auto-detects which exist)
#' @return Tibble with summary statistics
#' @export
calculate_summary_stats <- function(df, vars = NULL) {
  
  # Auto-detect variables if not specified
  if (is.null(vars)) {
    possible_vars <- c("UNRATE", "PCEC96_growth", "SP500_ret")
    vars <- possible_vars[possible_vars %in% colnames(df)]
  }
  
  summ_stats <- df |>
    summarise(
      across(
        all_of(vars),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          skewness = ~moments::skewness(.x, na.rm = TRUE),
          kurtosis = ~moments::kurtosis(.x, na.rm = TRUE),
          q25 = ~quantile(.x, 0.25, na.rm = TRUE),
          q75 = ~quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = c("Variable", "Statistic"),
      names_pattern = "(.+)_(mean|median|sd|min|max|skewness|kurtosis|q25|q75)$",
      values_to = "Value"
    ) |>
    pivot_wider(
      names_from = Statistic,
      values_from = Value
    ) |>
    dplyr::select(Variable, mean, median, sd, min, q25, q75, max, skewness, kurtosis)
  
  return(summ_stats)
}


#' Calculate period-specific summary statistics
#'
#' @param df Dataset with period indicators
#' @param vars Variables to summarize (auto-detects which exist)
#' @return Tibble with statistics by period
#' @export
calculate_period_stats <- function(df, vars = NULL) {
  
  # Auto-detect variables if not specified
  if (is.null(vars)) {
    possible_vars <- c("UNRATE", "PCEC96_growth", "SP500_ret")
    vars <- possible_vars[possible_vars %in% colnames(df)]
  }
  
  period_stats <- df |>
    group_by(period) |>
    summarise(
      across(
        all_of(vars),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      n = n(),
      .groups = "drop"
    )
  
  return(period_stats)
}


#' Calculate correlation matrix
#'
#' @param df Dataset
#' @param vars Variables to include in correlation matrix (auto-detects which exist)
#' @return Correlation matrix
#' @export
calculate_correlation_matrix <- function(df, vars = NULL) {
  
  # Auto-detect variables if not specified
  if (is.null(vars)) {
    possible_vars <- c("UNRATE", "SP500_ret", "USREC", "PCEC96_growth")
    vars <- possible_vars[possible_vars %in% colnames(df)]
  }
  
  cor_matrix <- df |>
    dplyr::select(all_of(vars)) |>
    cor(use = "complete.obs")
  
  return(cor_matrix)
}


#' Prepare comprehensive correlation data with enhanced variables
#'
#' @param df_base Already-cleaned base dataframe with date, UNRATE, SP500_ret, USREC
#' @return List containing correlation matrix and sample size
#' @export
prepare_enhanced_correlation_data <- function(df_base) {
  
  # Load enhanced variables from scratch (not from df_base)
  # We need to reload and merge properly
  raw_data <- load_raw_data()
  enhanced_vars <- load_enhanced_variables()
  
  # Create policy stance composite BEFORE passing to clean_and_merge_enhanced()
  # clean_and_merge_enhanced() expects enhanced_vars$policy_stance to exist
  policy_stance <- create_policy_stance_composite(
    enhanced_vars$fed_funds,
    enhanced_vars$wuxia
  )
  enhanced_vars$policy_stance <- policy_stance
  
  # clean_and_merge_enhanced() expects raw_data and will call clean_and_merge_data() internally
  # It will also merge policy_stance (with PolicyStance and PolicyAccommodation columns)
  df_enhanced <- clean_and_merge_enhanced(raw_data, enhanced_vars, include_pce = FALSE)
  
  # Add AR(15) decomposition components
  df_enhanced <- decompose_unemployment(df_enhanced, lag_order = 15)
  
  # Remove rows with NA in decomposition components (AR model loses observations)
  df_enhanced <- df_enhanced %>%
    filter(!is.na(UNRATE_anticipated), !is.na(UNRATE_unanticipated))
  
  # Select variables for comprehensive correlation matrix
  # Note: Column names from clean_and_merge_enhanced() are:
  #  - VIX (as-is)
  #  - Treasury_10Y (not DGS10)
  #  - Term_Spread (not TermSpread, has underscore)
  #  - PolicyStance (composite of DFF/WXSR, individual rates not merged)
  cor_vars_comprehensive <- c(
    "UNRATE", "SP500_ret", "USREC",
    "VIX", "Treasury_10Y", "Term_Spread", "PolicyStance",
    "UNRATE_anticipated", "UNRATE_unanticipated"
  )
  
  # Keep only complete cases
  df_cor_complete <- df_enhanced[, cor_vars_comprehensive]
  df_cor_complete <- na.omit(df_cor_complete)
  
  # Calculate correlation matrix
  cor_matrix <- cor(df_cor_complete, use = "complete.obs")
  
  return(list(
    cor_matrix = cor_matrix,
    n_sample = nrow(df_cor_complete),
    df_complete = df_cor_complete
  ))
}


#' Create summary table for enhanced variables
#'
#' @param vix_mean Mean VIX
#' @param vix_sd SD VIX
#' @param vix_min Min VIX
#' @param vix_max Max VIX
#' @param vix_median Median VIX
#' @param fed_funds_mean Mean Fed Funds
#' @param fed_funds_sd SD Fed Funds
#' @param fed_funds_min Min Fed Funds
#' @param fed_funds_max Max Fed Funds
#' @param treasury_mean Mean Treasury
#' @param treasury_sd SD Treasury
#' @param term_spread_mean Mean Term Spread
#' @param term_spread_sd SD Term Spread
#' @param policy_stance_mean Mean Policy Stance
#' @param policy_stance_sd SD Policy Stance
#' @param n_vix_sample Sample size
#' @return Data frame with summary statistics
#' @export
create_enhanced_vars_summary_table <- function(
    vix_mean, vix_sd, vix_min, vix_max, vix_median,
    fed_funds_mean, fed_funds_sd, fed_funds_min, fed_funds_max,
    treasury_mean, treasury_sd,
    term_spread_mean, term_spread_sd,
    policy_stance_mean, policy_stance_sd,
    n_vix_sample
) {
  
  enhanced_summary <- data.frame(
    Variable = c("VIX", "Fed Funds Rate (DFF)", "Wu-Xia Shadow Rate (WXSR)", 
                 "10-Year Treasury (DGS10)", "Term Spread (10Y-2Y)", "Policy Stance Composite"),
    Mean = c(vix_mean, fed_funds_mean, NA, treasury_mean, term_spread_mean, policy_stance_mean),
    SD = c(vix_sd, fed_funds_sd, NA, treasury_sd, term_spread_sd, policy_stance_sd),
    Min = c(vix_min, fed_funds_min, NA, NA, NA, NA),
    Max = c(vix_max, fed_funds_max, NA, NA, NA, NA),
    Median = c(vix_median, NA, NA, NA, NA, NA),
    N = rep(n_vix_sample, 6)
  )
  
  return(enhanced_summary)
}


#' Calculate period-specific correlations
#'
#' @param df Dataset with period indicators
#' @param var1 First variable
#' @param var2 Second variable
#' @return Tibble with correlations by period
#' @export
calculate_period_correlations <- function(df, var1 = "UNRATE", var2 = "SP500_ret") {
  
  period_cors <- df |>
    group_by(period) |>
    summarise(
      correlation = cor(.data[[var1]], .data[[var2]], use = "complete.obs"),
      n = n(),
      .groups = "drop"
    )
  
  return(period_cors)
}


#' Create formatted summary statistics table
#'
#' @param summ_stats Output from calculate_summary_stats()
#' @return kable object for display
#' @export
format_summary_table <- function(summ_stats) {
  
  summ_stats |>
    mutate(across(where(is.numeric), ~round(.x, 3))) |>
    knitr::kable(
      format = "html",
      caption = "Summary Statistics for Key Economic Variables",
      col.names = c("Variable", "Mean", "Median", "Std Dev", "Min", 
                    "Q25", "Q75", "Max", "Skewness", "Kurtosis"),
      align = c("l", rep("r", 9))
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center"
    )
}


#' Create formatted correlation matrix table
#'
#' @param cor_matrix Correlation matrix
#' @return kable object for display
#' @export
format_correlation_table <- function(cor_matrix) {
  
  cor_matrix |>
    round(3) |>
    knitr::kable(
      format = "html",
      caption = "Correlation Matrix: Full Sample (1959-2024)",
      align = "r"
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center"
    )
}


#' Calculate recession frequency table
#'
#' @param df Dataset with USREC variable
#' @return Tibble with recession statistics
#' @export
calculate_recession_stats <- function(df) {
  
  recession_stats <- df |>
    group_by(USREC_factor) |>
    summarise(
      count = n(),
      percentage = 100 * n() / nrow(df),
      .groups = "drop"
    )
  
  return(recession_stats)
}

cat("✓ Descriptive statistics functions defined\n")


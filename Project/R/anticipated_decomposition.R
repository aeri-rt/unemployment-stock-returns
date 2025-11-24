# =============================================================================
# anticipated_decomposition.R
# AR(15) decomposition following Gonzalo & Taamouti (2017) methodology
# =============================================================================

#' Decompose unemployment into anticipated and unanticipated components
#'
#' Following Gonzalo & Taamouti (2017), unemployment is decomposed using
#' an AR(15) model. The anticipated component is the fitted values from
#' the autoregression; the unanticipated component is the residuals.
#'
#' This decomposition tests whether markets react to forecastable changes
#' in unemployment (anticipated) versus unexpected shocks (unanticipated).
#'
#' @param df Data frame containing UNRATE variable
#' @param lag_order Integer, AR lag order (default 15 following G&T)
#' @return Data frame with added columns: UNRATE_anticipated, UNRATE_unanticipated
#' @export
decompose_unemployment <- function(df, lag_order = 15) {
  
  message("Fitting AR(", lag_order, ") model for unemployment decomposition...")
  
  # Fit AR(p) model to unemployment rate
  # Note: arima() with order = c(p, 0, 0) estimates AR(p)
  ar_model <- arima(df$UNRATE, 
                    order = c(lag_order, 0, 0),
                    method = "ML")
  
  # Extract components (convert ts to vector and ensure proper length)
  fitted_values <- as.numeric(fitted(ar_model))
  residual_values <- as.numeric(residuals(ar_model))
  
  # Ensure lengths match the data frame
  n_data <- nrow(df)
  n_fitted <- length(fitted_values)
  n_resid <- length(residual_values)
  
  # Pad or truncate fitted values to match data length
  if (n_fitted < n_data) {
    fitted_values <- c(rep(NA, n_data - n_fitted), fitted_values)
  } else if (n_fitted > n_data) {
    fitted_values <- fitted_values[1:n_data]
  }
  
  # Pad or truncate residuals to match data length
  if (n_resid < n_data) {
    residual_values <- c(rep(NA, n_data - n_resid), residual_values)
  } else if (n_resid > n_data) {
    residual_values <- residual_values[1:n_data]
  }
  
  df$UNRATE_anticipated <- fitted_values
  df$UNRATE_unanticipated <- residual_values
  
  # Calculate summary statistics for verification
  cor_ant_unant <- cor(df$UNRATE_anticipated, df$UNRATE_unanticipated, 
                        use = "complete.obs")
  
  message("\n=== AR(", lag_order, ") Decomposition Results ===")
  message(sprintf("AIC: %.2f", AIC(ar_model)))
  message(sprintf("Anticipated SD: %.3f", sd(df$UNRATE_anticipated, na.rm = TRUE)))
  message(sprintf("Unanticipated SD: %.3f", sd(df$UNRATE_unanticipated, na.rm = TRUE)))
  message(sprintf("Correlation: %.4f (should be ~0)", cor_ant_unant))
  message(sprintf("Observations with components: %d", sum(!is.na(df$UNRATE_anticipated))))
  message("")
  
  # Verify orthogonality
  if (abs(cor_ant_unant) > 0.1) {
    warning("Anticipated and unanticipated components show correlation > 0.1")
  }
  
  return(df)
}


#' Test anticipated vs unanticipated effects on returns
#'
#' Estimate separate regressions for anticipated and unanticipated unemployment
#' to test Gonzalo & Taamouti (2017) hypothesis that only anticipated component
#' affects stock returns.
#'
#' @param df Data frame with SP500_ret, UNRATE_anticipated, UNRATE_unanticipated
#' @param robust_se Logical, use heteroskedasticity-robust standard errors
#' @return List containing model summaries and comparison statistics
#' @export
test_anticipated_vs_unanticipated <- function(df, robust_se = TRUE) {
  
  message("Testing anticipated vs unanticipated components...")
  
  # Remove missing values
  df_complete <- df %>%
    dplyr::select(SP500_ret, UNRATE_anticipated, UNRATE_unanticipated) %>%
    drop_na()
  
  n_obs <- nrow(df_complete)
  message(sprintf("Sample size: N = %d", n_obs))
  
  # Model 1: Anticipated component
  model_anticipated <- lm(SP500_ret ~ UNRATE_anticipated, data = df_complete)
  
  # Model 2: Unanticipated component
  model_unanticipated <- lm(SP500_ret ~ UNRATE_unanticipated, data = df_complete)
  
  # Model 3: Both components (test joint significance)
  model_both <- lm(SP500_ret ~ UNRATE_anticipated + UNRATE_unanticipated, 
                   data = df_complete)
  
  # Apply robust standard errors if requested
  if (robust_se) {
    coef_ant <- coeftest(model_anticipated, vcov = vcovHC(model_anticipated, type = "HC1"))
    coef_unant <- coeftest(model_unanticipated, vcov = vcovHC(model_unanticipated, type = "HC1"))
    coef_both <- coeftest(model_both, vcov = vcovHC(model_both, type = "HC1"))
  } else {
    coef_ant <- coeftest(model_anticipated)
    coef_unant <- coeftest(model_unanticipated)
    coef_both <- coeftest(model_both)
  }
  
  # Extract key statistics
  beta_ant <- coef(model_anticipated)["UNRATE_anticipated"]
  beta_unant <- coef(model_unanticipated)["UNRATE_unanticipated"]
  
  p_ant <- coef_ant["UNRATE_anticipated", "Pr(>|t|)"]
  p_unant <- coef_unant["UNRATE_unanticipated", "Pr(>|t|)"]
  
  r2_ant <- summary(model_anticipated)$r.squared
  r2_unant <- summary(model_unanticipated)$r.squared
  
  # Print results
  message("\n=== Results ===")
  message("\nModel 1: SP500_ret ~ UNRATE_anticipated")
  message(sprintf("  β_anticipated = %.4f (p = %.4f)", beta_ant, p_ant))
  message(sprintf("  R² = %.4f", r2_ant))
  message(sprintf("  %s", ifelse(p_ant < 0.05, "✓ Significant at 5%", "✗ Not significant")))
  
  message("\nModel 2: SP500_ret ~ UNRATE_unanticipated")
  message(sprintf("  β_unanticipated = %.4f (p = %.4f)", beta_unant, p_unant))
  message(sprintf("  R² = %.4f", r2_unant))
  message(sprintf("  %s", ifelse(p_unant < 0.05, "✓ Significant at 5%", "✗ Not significant")))
  
  # Interpretation
  message("\n=== Gonzalo & Taamouti (2017) Replication ===")
  if (p_ant < 0.05 && p_unant >= 0.05) {
    message("✓ Consistent with G&T: Only anticipated component significant")
  } else if (p_ant < 0.05 && p_unant < 0.05) {
    message("⚠ Both components significant (differs from G&T)")
  } else if (p_ant >= 0.05 && p_unant < 0.05) {
    message("✗ Contradicts G&T: Only unanticipated significant")
  } else {
    message("✗ Neither component significant")
  }
  message("")
  
  # Return comprehensive results
  results <- list(
    model_anticipated = model_anticipated,
    model_unanticipated = model_unanticipated,
    model_both = model_both,
    coef_anticipated = coef_ant,
    coef_unanticipated = coef_unant,
    coef_both = coef_both,
    summary_stats = tibble(
      component = c("Anticipated", "Unanticipated"),
      beta = c(beta_ant, beta_unant),
      p_value = c(p_ant, p_unant),
      r_squared = c(r2_ant, r2_unant),
      significant = c(p_ant < 0.05, p_unant < 0.05)
    ),
    n_obs = n_obs
  )
  
  return(results)
}


#' Sensitivity analysis: Test different AR lag orders
#'
#' Gonzalo & Taamouti use AR(15). Test sensitivity to lag order selection
#' to verify results are robust to specification.
#'
#' @param df Data frame containing UNRATE and SP500_ret
#' @param lag_orders Vector of lag orders to test (default: 6, 12, 18, 24)
#' @return Tibble with results for each lag order
#' @export
test_ar_order_sensitivity <- function(df, lag_orders = c(6, 12, 18, 24)) {
  
  message("Testing sensitivity to AR lag order specification...")
  
  results_list <- list()
  
  for (p in lag_orders) {
    # Decompose with lag order p
    df_temp <- decompose_unemployment(df, lag_order = p)
    
    # Test anticipated component
    df_complete <- df_temp %>%
      dplyr::select(SP500_ret, UNRATE_anticipated) %>%
      drop_na()
    
    model <- lm(SP500_ret ~ UNRATE_anticipated, data = df_complete)
    coef_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
    
    # Store results
    results_list[[length(results_list) + 1]] <- tibble(
      lag_order = p,
      beta = coef(model)["UNRATE_anticipated"],
      se = coef_robust["UNRATE_anticipated", "Std. Error"],
      t_stat = coef_robust["UNRATE_anticipated", "t value"],
      p_value = coef_robust["UNRATE_anticipated", "Pr(>|t|)"],
      r_squared = summary(model)$r.squared,
      aic = AIC(model),
      n_obs = nrow(df_complete)
    )
  }
  
  results_df <- bind_rows(results_list)
  
  # Print comparison
  message("\n=== AR Order Sensitivity Results ===")
  print(results_df, n = Inf)
  message("")
  
  # Check stability
  beta_range <- max(results_df$beta) - min(results_df$beta)
  all_significant <- all(results_df$p_value < 0.05)
  
  if (beta_range < 0.05 && all_significant) {
    message("✓ Results stable across lag orders")
  } else if (all_significant) {
    message("⚠ Significance stable but coefficient magnitude varies")
  } else {
    message("✗ Results sensitive to lag order specification")
  }
  
  return(results_df)
}


message("✓ Anticipated/unanticipated decomposition functions loaded")


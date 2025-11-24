# =============================================================================
# diagnostics.R
# Regression diagnostics: heteroskedasticity, autocorrelation, normality tests
# =============================================================================

#' Run comprehensive regression diagnostics
#'
#' @param model lm object
#' @return List containing test results
#' @export
run_diagnostic_tests <- function(model) {

  # Breusch-Pagan test for heteroskedasticity
  bp_test <- lmtest::bptest(model)

  # Durbin-Watson test for autocorrelation
  dw_test <- lmtest::dwtest(model)

  # Shapiro-Wilk test for normality (sample if N > 5000)
  resids <- residuals(model)
  if (length(resids) > 5000) {
    resids_sample <- sample(resids, 5000)
  } else {
    resids_sample <- resids
  }
  sw_test <- shapiro.test(resids_sample)

  # Jarque-Bera test for normality (alternative to Shapiro-Wilk)
  jb_stat <- moments::jarque.test(resids)

  # Collect results
  diagnostics <- list(
    # Heteroskedasticity
    bp_statistic = bp_test$statistic,
    bp_pvalue = bp_test$p.value,
    bp_conclusion = ifelse(
      bp_test$p.value < 0.05,
      "Heteroskedasticity detected (use robust standard errors)",
      "Homoskedasticity assumption reasonable"
    ),

    # Autocorrelation
    dw_statistic = dw_test$statistic,
    dw_pvalue = dw_test$p.value,
    dw_conclusion = ifelse(
      dw_test$p.value < 0.05,
      "Serial correlation detected (consider Newey-West SE or AR terms)",
      "No significant autocorrelation"
    ),

    # Normality
    sw_statistic = sw_test$statistic,
    sw_pvalue = sw_test$p.value,
    sw_conclusion = ifelse(
      sw_test$p.value < 0.05,
      "Non-normal residuals (OLS inference may be unreliable)",
      "Normality assumption reasonable"
    ),

    # Jarque-Bera
    jb_statistic = jb_stat$statistic,
    jb_pvalue = jb_stat$p.value
  )

  return(diagnostics)
}


#' Format diagnostic results as table
#'
#' @param diagnostics Output from run_diagnostic_tests()
#' @return Formatted kable table
#' @export
format_diagnostic_table <- function(diagnostics) {

  diag_df <- data.frame(
    Test = c("Breusch-Pagan (Heteroskedasticity)",
             "Durbin-Watson (Autocorrelation)",
             "Shapiro-Wilk (Normality)"),
    Statistic = c(
      round(diagnostics$bp_statistic, 3),
      round(diagnostics$dw_statistic, 3),
      round(diagnostics$sw_statistic, 4)
    ),
    `P-Value` = c(
      round(diagnostics$bp_pvalue, 4),
      round(diagnostics$dw_pvalue, 4),
      round(diagnostics$sw_pvalue, 4)
    ),
    Conclusion = c(
      diagnostics$bp_conclusion,
      diagnostics$dw_conclusion,
      diagnostics$sw_conclusion
    ),
    check.names = FALSE
  )

  diag_df |>
    knitr::kable(
      format = "html",
      caption = "Regression Diagnostic Tests",
      align = c("l", "r", "r", "l")
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center"
    )
}


#' Calculate variance inflation factors (VIF) for multicollinearity
#'
#' @param model lm object with multiple predictors
#' @return Data frame with VIF values
#' @export
calculate_vif <- function(model) {

  # Extract model matrix (excluding intercept)
  X <- model.matrix(model)[, -1, drop = FALSE]

  # Calculate VIF for each predictor
  vif_values <- sapply(colnames(X), function(var) {
    # Regress each predictor on all others
    formula_vif <- as.formula(paste(var, "~ ."))
    model_vif <- lm(formula_vif, data = as.data.frame(X))
    rsq <- summary(model_vif)$r.squared

    # VIF = 1 / (1 - R²)
    vif <- 1 / (1 - rsq)
    return(vif)
  })

  vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = vif_values,
    row.names = NULL
  ) |>
    mutate(
      Interpretation = case_when(
        VIF < 5 ~ "No multicollinearity concern",
        VIF >= 5 & VIF < 10 ~ "Moderate multicollinearity",
        VIF >= 10 ~ "Severe multicollinearity"
      )
    )

  return(vif_df)
}


#' Test for influential observations (Cook's distance)
#'
#' @param model lm object
#' @param threshold Numeric: Cook's distance threshold (default 4/n)
#' @return Data frame with influential observations
#' @export
identify_influential_obs <- function(model, threshold = NULL) {

  n <- nobs(model)

  if (is.null(threshold)) {
    threshold <- 4 / n
  }

  cooks_d <- cooks.distance(model)

  influential <- data.frame(
    observation = 1:n,
    cooks_distance = cooks_d
  ) |>
    filter(cooks_distance > threshold) |>
    arrange(desc(cooks_distance))

  return(influential)
}


#' Calculate robust standard errors (HC1, heteroskedasticity-consistent)
#'
#' @param model lm object
#' @return Coefficient table with robust SE
#' @export
get_robust_se <- function(model) {

  robust_se <- sandwich::vcovHC(model, type = "HC1")
  robust_coef <- lmtest::coeftest(model, vcov = robust_se)

  return(robust_coef)
}


#' Calculate Newey-West HAC standard errors (for autocorrelation)
#'
#' @param model lm object
#' @param lag Integer: number of lags for HAC correction
#' @return Coefficient table with Newey-West SE
#' @export
get_newey_west_se <- function(model, lag = NULL) {

  if (is.null(lag)) {
    # Rule of thumb: lag = floor(4*(n/100)^(2/9))
    n <- nobs(model)
    lag <- floor(4 * (n / 100)^(2/9))
  }

  nw_vcov <- sandwich::NeweyWest(model, lag = lag)
  nw_coef <- lmtest::coeftest(model, vcov = nw_vcov)

  return(nw_coef)
}


#' Test for ARCH effects (autoregressive conditional heteroskedasticity)
#'
#' @param model lm object
#' @param lags Integer: number of lags to test
#' @return ARCH-LM test result
#' @export
test_arch_effects <- function(model, lags = 12) {

  resids <- residuals(model)
  resids_sq <- resids^2

  # Regress squared residuals on lagged squared residuals
  resids_sq_df <- data.frame(resids_sq = resids_sq)

  for (i in 1:lags) {
    resids_sq_df[[paste0("lag", i)]] <- c(rep(NA, i), resids_sq[1:(length(resids_sq) - i)])
  }

  resids_sq_df <- na.omit(resids_sq_df)

  arch_model <- lm(resids_sq ~ ., data = resids_sq_df)

  # LM test statistic = n * R²
  n <- nobs(arch_model)
  rsq <- summary(arch_model)$r.squared
  lm_stat <- n * rsq

  # Chi-squared test with 'lags' degrees of freedom
  p_value <- 1 - pchisq(lm_stat, df = lags)

  result <- list(
    statistic = lm_stat,
    p_value = p_value,
    conclusion = ifelse(
      p_value < 0.05,
      "ARCH effects detected (consider GARCH modeling)",
      "No significant ARCH effects"
    )
  )

  return(result)
}


#' Test stationarity of time series using ADF test
#'
#' @param df Data frame containing variables to test
#' @return List containing ADF test results
#' @export
test_stationarity <- function(df) {
  
  message("Running Augmented Dickey-Fuller (ADF) tests for stationarity...")
  
  # ADF test for UNRATE
  adf_unrate <- tryCatch({
    tseries::adf.test(df$UNRATE, alternative = "stationary")
  }, error = function(e) {
    warning("ADF test failed for UNRATE: ", e$message)
    return(NULL)
  })
  
  # ADF test for SP500_ret
  adf_returns <- tryCatch({
    tseries::adf.test(df$SP500_ret, alternative = "stationary")
  }, error = function(e) {
    warning("ADF test failed for SP500_ret: ", e$message)
    return(NULL)
  })
  
  # Print results
  cat("\n=== Stationarity Test Results (ADF) ===\n")
  
  if (!is.null(adf_unrate)) {
    cat(sprintf("UNRATE:     Dickey-Fuller = %.4f, p-value = %.4f %s\n", 
                adf_unrate$statistic,
                adf_unrate$p.value,
                ifelse(adf_unrate$p.value < 0.05, "[STATIONARY]", "[UNIT ROOT]")))
  }
  
  if (!is.null(adf_returns)) {
    cat(sprintf("SP500_ret:  Dickey-Fuller = %.4f, p-value = %.4f %s\n", 
                adf_returns$statistic,
                adf_returns$p.value,
                ifelse(adf_returns$p.value < 0.05, "[STATIONARY]", "[UNIT ROOT]")))
  }
  
  cat("\nInterpretation: p < 0.05 rejects unit root (series is stationary)\n\n")
  
  return(list(
    unrate = adf_unrate,
    returns = adf_returns
  ))
}

cat("✓ Diagnostic functions defined\n")

# =============================================================================
# baseline_models.R
# OLS regression models: baseline and augmented specifications
# =============================================================================

#' Estimate baseline OLS model: SP500 ~ UNRATE
#'
#' @param df Dataset
#' @param period Character: "full", "pre2008", "post2008", "post2020"
#' @return List containing model object and summary statistics
#' @export
estimate_baseline_model <- function(df, period = "full") {
  
  # Subset data by period
  if (period == "pre2008") {
    df_subset <- df |> filter(date < as.Date("2008-01-01"))
  } else if (period == "post2008") {
    df_subset <- df |> filter(date >= as.Date("2008-01-01"))
  } else if (period == "post2020") {
    df_subset <- df |> filter(date >= as.Date("2020-01-01"))
  } else {
    df_subset <- df
  }
  
  # Estimate model
  model <- lm(SP500_ret ~ UNRATE, data = df_subset)
  
  # Extract key statistics
  summ <- summary(model)
  
  results <- list(
    model = model,
    period = period,
    n = nobs(model),
    beta_unrate = coef(model)[2],
    se_unrate = summ$coefficients[2, 2],
    t_stat = summ$coefficients[2, 3],
    p_value = summ$coefficients[2, 4],
    rsquared = summ$r.squared,
    adj_rsquared = summ$adj.r.squared,
    rmse = sigma(model)
  )
  
  return(results)
}


#' Estimate augmented model with PCE growth
#'
#' @param df Dataset
#' @param period Character: period identifier
#' @return List containing model object and statistics
#' @export
estimate_augmented_model_pce <- function(df, period = "full") {
  
  # Subset data
  if (period == "pre2008") {
    df_subset <- df |> filter(date < as.Date("2008-01-01"))
  } else if (period == "post2008") {
    df_subset <- df |> filter(date >= as.Date("2008-01-01"))
  } else if (period == "post2020") {
    df_subset <- df |> filter(date >= as.Date("2020-01-01"))
  } else {
    df_subset <- df
  }
  
  # Estimate model
  model <- lm(SP500_ret ~ UNRATE + PCEC96_growth, data = df_subset)
  
  # Extract statistics
  summ <- summary(model)
  
  results <- list(
    model = model,
    period = period,
    n = nobs(model),
    rsquared = summ$r.squared,
    adj_rsquared = summ$adj.r.squared,
    rmse = sigma(model)
  )
  
  return(results)
}


#' Estimate full model with recession interactions
#'
#' @param df Dataset
#' @param period Character: period identifier
#' @return List containing model object and statistics
#' @export
estimate_interaction_model <- function(df, period = "full") {
  
  # Subset data
  if (period == "pre2008") {
    df_subset <- df |> filter(date < as.Date("2008-01-01"))
  } else if (period == "post2008") {
    df_subset <- df |> filter(date >= as.Date("2008-01-01"))
  } else if (period == "post2020") {
    df_subset <- df |> filter(date >= as.Date("2020-01-01"))
  } else {
    df_subset <- df
  }
  
  # Estimate model
  model <- lm(SP500_ret ~ UNRATE * USREC + PCEC96_growth * USREC, 
              data = df_subset)
  
  # Extract statistics
  summ <- summary(model)
  
  results <- list(
    model = model,
    period = period,
    n = nobs(model),
    rsquared = summ$r.squared,
    adj_rsquared = summ$adj.r.squared,
    rmse = sigma(model),
    interaction_coef = coef(model)["UNRATE:USREC"],
    interaction_pval = summ$coefficients["UNRATE:USREC", 4]
  )
  
  return(results)
}


#' Estimate augmented model with control variables (DFF, VIX)
#'
#' @param df Dataset with control variables
#' @param period Character: period identifier
#' @return List containing model object and statistics
#' @export
estimate_model_with_controls <- function(df, period = "full") {
  
  # Subset data
  if (period == "pre2008") {
    df_subset <- df |> filter(date < as.Date("2008-01-01"))
  } else if (period == "post2008") {
    df_subset <- df |> filter(date >= as.Date("2008-01-01"))
  } else if (period == "post2020") {
    df_subset <- df |> filter(date >= as.Date("2020-01-01"))
  } else {
    df_subset <- df
  }
  
  # Estimate model with controls
  model <- lm(SP500_ret ~ UNRATE + PCEC96_growth + DFF + VIX, 
              data = df_subset)
  
  # Extract statistics
  summ <- summary(model)
  
  results <- list(
    model = model,
    period = period,
    n = nobs(model),
    rsquared = summ$r.squared,
    adj_rsquared = summ$adj.r.squared,
    rmse = sigma(model),
    beta_unrate = coef(model)["UNRATE"],
    pval_unrate = summ$coefficients["UNRATE", 4]
  )
  
  return(results)
}


#' Compare models across periods
#'
#' @param df Dataset
#' @param model_fn Model estimation function
#' @return Data frame with period comparison
#' @export
compare_periods <- function(df, model_fn = estimate_baseline_model) {
  
  periods <- c("full", "pre2008", "post2008", "post2020")
  
  results_list <- lapply(periods, function(p) {
    res <- model_fn(df, period = p)
    data.frame(
      period = p,
      n = res$n,
      rsquared = res$rsquared,
      adj_rsquared = res$adj_rsquared,
      rmse = res$rmse
    )
  })
  
  results_df <- bind_rows(results_list)
  
  return(results_df)
}


#' Calculate incremental R-squared
#'
#' @param model_simple Simple model
#' @param model_complex Complex model
#' @return Numeric: incremental R-squared
#' @export
calculate_incremental_rsq <- function(model_simple, model_complex) {
  
  rsq_simple <- summary(model_simple)$r.squared
  rsq_complex <- summary(model_complex)$r.squared
  
  incremental_rsq <- rsq_complex - rsq_simple
  
  return(incremental_rsq)
}


#' Create regression table using modelsummary
#'
#' @param model_list Named list of lm objects
#' @param title Table title
#' @return modelsummary output
#' @export
create_regression_table <- function(model_list, title = "Regression Results") {
  
  modelsummary::modelsummary(
    model_list,
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse"),
    coef_rename = c(
      "UNRATE" = "Unemployment Rate",
      "PCEC96_growth" = "Real PCE Growth",
      "USREC" = "Recession Dummy",
      "DFF" = "Federal Funds Rate",
      "VIX" = "VIX (Volatility Index)",
      "UNRATE:USREC" = "Unemployment × Recession",
      "PCEC96_growth:USREC" = "PCE Growth × Recession"
    ),
    title = title,
    notes = c(
      "Heteroskedasticity-robust standard errors in parentheses.",
      "Statistical significance: * p < 0.10, ** p < 0.05, *** p < 0.01"
    )
  )
}

#' Estimate Policy Interaction Models with Enhanced Data
#'
#' Loads enhanced data (VIX, Fed Funds, Wu-Xia), creates policy stance composite,
#' and estimates 5 models testing Fed Put mechanism.
#'
#' @param raw_data_full Raw data from load_raw_data()
#' @return List containing df_analysis and models_policy
#' @export
estimate_policy_interaction_models <- function(raw_data_full) {
  
  cat("Estimating policy interaction models...\n")
  
  # Load enhanced data with policy measures
  enhanced_vars <- load_enhanced_variables()
  policy_stance <- create_policy_stance_composite(
    enhanced_vars$fed_funds,
    enhanced_vars$wuxia
  )
  enhanced_vars$policy_stance <- policy_stance
  
  # Merge and clean enhanced data
  df_enhanced <- clean_and_merge_enhanced(raw_data_full, enhanced_vars, include_pce = FALSE)
  df_enhanced <- decompose_unemployment(df_enhanced, lag_order = 15)
  
  # Create analysis dataset (VIX-restricted)
  df_analysis <- df_enhanced %>%
    filter(!is.na(SP500_ret), !is.na(UNRATE_anticipated), 
           !is.na(PolicyStance), !is.na(VIX)) %>%
    dplyr::select(date, SP500_ret, UNRATE_anticipated, PolicyStance, VIX)
  
  cat(sprintf("✓ VIX-restricted sample: N = %d observations\n", nrow(df_analysis)))
  
  # Estimate models
  model_baseline_vix <- lm(SP500_ret ~ UNRATE_anticipated, data = df_analysis)
  model_policy <- lm(SP500_ret ~ UNRATE_anticipated + PolicyStance, data = df_analysis)
  model_interaction <- lm(SP500_ret ~ UNRATE_anticipated * PolicyStance, data = df_analysis)
  model_vix <- lm(SP500_ret ~ UNRATE_anticipated + VIX, data = df_analysis)
  model_full <- lm(SP500_ret ~ UNRATE_anticipated * PolicyStance + VIX, data = df_analysis)
  
  # Create model list
  models_policy <- list(
    "Baseline" = model_baseline_vix,
    "Policy" = model_policy,
    "Interaction" = model_interaction,
    "VIX" = model_vix,
    "Full" = model_full
  )
  
  cat("✓ All 5 policy interaction models estimated\n")
  
  return(list(
    df_analysis = df_analysis,
    models = models_policy,
    # Individual models for inline reporting
    model_baseline_vix = model_baseline_vix,
    model_policy = model_policy,
    model_interaction = model_interaction,
    model_vix = model_vix,
    model_full = model_full
  ))
}

cat("Baseline model functions defined\n")


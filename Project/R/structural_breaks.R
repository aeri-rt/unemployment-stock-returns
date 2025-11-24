# =============================================================================
# structural_breaks.R
# Structural break tests: Chow test, rolling regressions, recursive estimates
# =============================================================================

#' Perform Chow test for structural break at specified date
#'
#' @param df Dataset
#' @param formula Model formula
#' @param breakpoint Date: structural break date
#' @return List containing test statistics and results
#' @export
chow_test <- function(df, formula, breakpoint) {
  
  # Split data at breakpoint
  df_pre <- df |> filter(date < breakpoint)
  df_post <- df |> filter(date >= breakpoint)
  
  # Estimate models
  model_pooled <- lm(formula, data = df)
  model_pre <- lm(formula, data = df_pre)
  model_post <- lm(formula, data = df_post)
  
  # Calculate RSS (residual sum of squares)
  rss_pooled <- sum(residuals(model_pooled)^2)
  rss_pre <- sum(residuals(model_pre)^2)
  rss_post <- sum(residuals(model_post)^2)
  rss_unrestricted <- rss_pre + rss_post
  
  # Degrees of freedom
  k <- length(coef(model_pooled))  # number of parameters
  n1 <- nobs(model_pre)
  n2 <- nobs(model_post)
  n <- n1 + n2
  
  # Chow F-statistic
  # F = [(RSS_restricted - RSS_unrestricted) / k] / [RSS_unrestricted / (n - 2k)]
  f_stat <- ((rss_pooled - rss_unrestricted) / k) / 
            (rss_unrestricted / (n - 2 * k))
  
  # P-value
  p_value <- 1 - pf(f_stat, df1 = k, df2 = n - 2 * k)
  
  # Coefficient comparison
  coef_pre <- coef(model_pre)
  coef_post <- coef(model_post)
  coef_change <- coef_post - coef_pre
  
  # R-squared comparison
  rsq_pre <- summary(model_pre)$r.squared
  rsq_post <- summary(model_post)$r.squared
  rsq_change <- rsq_post - rsq_pre
  rsq_pct_change <- 100 * (rsq_post - rsq_pre) / rsq_pre
  
  results <- list(
    f_statistic = f_stat,
    p_value = p_value,
    df1 = k,
    df2 = n - 2 * k,
    n_pre = n1,
    n_post = n2,
    rss_pooled = rss_pooled,
    rss_unrestricted = rss_unrestricted,
    conclusion = ifelse(
      p_value < 0.05,
      "Structural break detected (reject parameter stability)",
      "No significant structural break (cannot reject stability)"
    ),
    model_pre = model_pre,
    model_post = model_post,
    model_pooled = model_pooled,
    coef_pre = coef_pre,
    coef_post = coef_post,
    coef_change = coef_change,
    rsq_pre = rsq_pre,
    rsq_post = rsq_post,
    rsq_change = rsq_change,
    rsq_pct_change = rsq_pct_change,
    breakpoint = breakpoint
  )
  
  return(results)
}


#' Format Chow test results as a table
#'
#' @param chow_results Output from chow_test()
#' @return Formatted kable table
#' @export
format_chow_table <- function(chow_results) {
  
  results_df <- data.frame(
    Statistic = c(
      "F-statistic",
      "Degrees of Freedom (numerator)",
      "Degrees of Freedom (denominator)",
      "P-value",
      "N (Pre-break)",
      "N (Post-break)",
      "R² (Pre-break)",
      "R² (Post-break)",
      "R² Change",
      "R² % Change",
      "Conclusion"
    ),
    Value = c(
      round(chow_results$f_statistic, 3),
      chow_results$df1,
      chow_results$df2,
      format.pval(chow_results$p_value, digits = 4),
      chow_results$n_pre,
      chow_results$n_post,
      round(chow_results$rsq_pre, 4),
      round(chow_results$rsq_post, 4),
      round(chow_results$rsq_change, 4),
      sprintf("%.1f%%", chow_results$rsq_pct_change),
      chow_results$conclusion
    )
  )
  
  results_df |>
    knitr::kable(
      format = "html",
      caption = sprintf("Chow Test for Structural Break at %s", 
                       as.character(chow_results$breakpoint)),
      col.names = c("Test Statistic", "Value"),
      align = c("l", "r")
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center"
    )
}


#' Calculate rolling window regressions
#'
#' @param df Dataset
#' @param window_size Integer: number of months in rolling window
#' @return Data frame with rolling statistics
#' @export
calculate_rolling_regression <- function(df, window_size = 60) {
  
  cat(sprintf("Calculating %d-month rolling regressions...\n", window_size))
  
  # Sort by date
  df <- df |> arrange(date)
  
  # Rolling correlation
  roll_cor <- slider::slide_dbl(
    .x = df,
    .f = ~cor(.x$UNRATE, .x$SP500_ret, use = "complete.obs"),
    .before = window_size - 1,
    .complete = TRUE
  )
  
  # Rolling R-squared (from univariate regression)
  roll_rsq <- slider::slide_dbl(
    .x = df,
    .f = ~{
      if (nrow(.x) >= window_size) {
        model <- lm(SP500_ret ~ UNRATE, data = .x)
        return(summary(model)$r.squared)
      } else {
        return(NA_real_)
      }
    },
    .before = window_size - 1,
    .complete = TRUE
  )
  
  # Rolling beta coefficient
  roll_beta <- slider::slide_dbl(
    .x = df,
    .f = ~{
      if (nrow(.x) >= window_size) {
        model <- lm(SP500_ret ~ UNRATE, data = .x)
        return(coef(model)[2])
      } else {
        return(NA_real_)
      }
    },
    .before = window_size - 1,
    .complete = TRUE
  )
  
  # Add to dataset
  df_rolling <- df |>
    mutate(
      roll_cor_60 = roll_cor,
      roll_rsq_60 = roll_rsq,
      roll_beta_60 = roll_beta
    ) |>
    filter(!is.na(roll_cor_60))  # Remove initial window where stats are NA
  
  cat(sprintf("✓ Rolling statistics calculated for %d windows\n\n", nrow(df_rolling)))
  
  return(df_rolling)
}


#' Calculate rolling regression with confidence intervals
#'
#' @param df Dataset
#' @param window_size Integer: window size
#' @return Data frame with rolling estimates and CI
#' @export
calculate_rolling_regression_ci <- function(df, window_size = 60) {
  
  cat(sprintf("Calculating %d-month rolling regressions with 95%% CI...\n", window_size))
  
  df <- df |> arrange(date)
  
  # Initialize vectors
  n <- nrow(df)
  roll_beta <- rep(NA_real_, n)
  roll_se <- rep(NA_real_, n)
  roll_rsq <- rep(NA_real_, n)
  
  # Loop through windows
  for (i in window_size:n) {
    df_window <- df[(i - window_size + 1):i, ]
    
    model <- lm(SP500_ret ~ UNRATE, data = df_window)
    summ <- summary(model)
    
    roll_beta[i] <- coef(model)[2]
    roll_se[i] <- summ$coefficients[2, 2]
    roll_rsq[i] <- summ$r.squared
  }
  
  # Calculate confidence intervals
  df_rolling <- df |>
    mutate(
      roll_beta = roll_beta,
      roll_se = roll_se,
      roll_rsq = roll_rsq,
      roll_beta_lower = roll_beta - 1.96 * roll_se,
      roll_beta_upper = roll_beta + 1.96 * roll_se
    ) |>
    filter(!is.na(roll_beta))
  
  cat(sprintf("✓ Rolling statistics with CI calculated\n\n"))
  
  return(df_rolling)
}


#' Perform multiple Chow tests at different breakpoints
#'
#' @param df Dataset
#' @param formula Model formula
#' @param breakpoints Vector of dates to test
#' @return Data frame with results for each breakpoint
#' @export
multiple_chow_tests <- function(df, formula, breakpoints) {
  
  results_list <- lapply(breakpoints, function(bp) {
    chow_res <- chow_test(df, formula, bp)
    
    data.frame(
      breakpoint = as.character(bp),
      f_statistic = chow_res$f_statistic,
      p_value = chow_res$p_value,
      rsq_pre = chow_res$rsq_pre,
      rsq_post = chow_res$rsq_post,
      rsq_change = chow_res$rsq_change,
      significant = chow_res$p_value < 0.05
    )
  })

  results_df <- bind_rows(results_list)

  return(results_df)
}


#' Perform recursive Chow test (test stability over time)
#'
#' @param df Dataset
#' @param formula Model formula
#' @param min_obs Integer: minimum observations for initial window
#' @return Data frame with recursive test statistics
#' @export
recursive_chow_test <- function(df, formula, min_obs = 100) {

  df <- df |> arrange(date)
  n <- nrow(df)

  # Test each potential breakpoint from min_obs to n - min_obs
  breakpoint_dates <- df$date[(min_obs + 1):(n - min_obs)]

  results <- multiple_chow_tests(df, formula, breakpoint_dates)

  return(results)
}


#' Plot Chow test statistics over time
#'
#' @param recursive_results Output from recursive_chow_test()
#' @return ggplot object
#' @export
plot_recursive_chow <- function(recursive_results) {

  recursive_results |>
    mutate(breakpoint = as.Date(breakpoint)) |>
    ggplot(aes(x = breakpoint, y = f_statistic)) +
    geom_line(color = colors_custom$primary, size = 1) +
    geom_hline(yintercept = qf(0.95, df1 = 2, df2 = 700),
               linetype = "dashed", color = "darkred") +
    labs(
      title = "Recursive Chow Test: F-statistics Over Time",
      subtitle = "Dashed line indicates 5% critical value",
      x = "Potential Breakpoint Date",
      y = "F-statistic"
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

#' Calculate Rolling Window Coefficients for Anticipated Unemployment
#'
#' Calculates rolling window regression coefficients and p-values for anticipated
#' unemployment predicting stock returns. Returns stability metrics.
#'
#' @param df_full Dataframe with SP500_ret, UNRATE_anticipated, date
#' @param window_size Rolling window size in months (default 120 = 10 years)
#' @return List with rolling_coefs, rolling_pvals, rolling_dates, and stability_metrics
#' @export
calculate_rolling_window_anticipated <- function(df_full, window_size = 120) {
  
  cat(sprintf("Calculating %d-month rolling windows for anticipated unemployment...\n", window_size))
  
  n_obs <- nrow(df_full)
  n_windows <- n_obs - window_size + 1
  
  rolling_coefs <- rep(NA, n_windows)
  rolling_pvals <- rep(NA, n_windows)
  rolling_dates <- df_full$date[1:n_windows]
  
  # Rolling regression loop
  for (i in 1:n_windows) {
    window_data <- df_full[i:(i + window_size - 1), ]
    model_temp <- lm(SP500_ret ~ UNRATE_anticipated, data = window_data)
    rolling_coefs[i] <- coef(model_temp)[2]
    rolling_pvals[i] <- summary(model_temp)$coefficients[2, 4]
  }
  
  # Calculate stability metrics
  coef_mean <- mean(rolling_coefs, na.rm = TRUE)
  coef_sd <- sd(rolling_coefs, na.rm = TRUE)
  coef_cv <- coef_sd / abs(coef_mean)
  coef_range <- range(rolling_coefs, na.rm = TRUE)
  prop_significant <- mean(rolling_pvals < 0.05, na.rm = TRUE)
  
  # Create stability metrics dataframe
  stability_metrics <- data.frame(
    Metric = c("Mean Coefficient", "Standard Deviation", "Coefficient of Variation", 
               "Range (Min)", "Range (Max)", "% Windows Significant (p<0.05)"),
    Value = c(coef_mean, coef_sd, coef_cv, coef_range[1], coef_range[2], prop_significant * 100)
  )
  
  cat(sprintf("✓ Completed %d rolling windows\n", n_windows))
  cat(sprintf("  Mean coefficient: %.4f\n", coef_mean))
  cat(sprintf("  Coefficient of variation: %.1f%%\n", coef_cv * 100))
  cat(sprintf("  %% Significant windows: %.1f%%\n", prop_significant * 100))
  
  return(list(
    rolling_coefs = rolling_coefs,
    rolling_pvals = rolling_pvals,
    rolling_dates = rolling_dates,
    stability_metrics = stability_metrics,
    coef_mean = coef_mean,
    coef_range = coef_range,
    prop_significant = prop_significant
  ))
}

cat("✓ Structural break functions defined\n")

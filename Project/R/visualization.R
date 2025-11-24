# =============================================================================
# visualization.R
# Plotting functions for time series, distributions, and diagnostics
# =============================================================================

#' Create recession shading data for plots
#'
#' @param df Dataset with USREC variable
#' @return Tibble with recession period boundaries
#' @export
create_recession_periods <- function(df) {
  
  recession_periods <- df |>
    mutate(recession_change = USREC - lag(USREC, default = 0)) |>
    mutate(
      recession_start = if_else(recession_change == 1, date, as.Date(NA)),
      recession_end = if_else(recession_change == -1, date, as.Date(NA))
    ) |>
    summarise(
      starts = list(na.omit(recession_start)),
      ends = list(na.omit(recession_end))
    ) |>
    unnest(c(starts, ends)) |>
    mutate(
      xmin = starts,
      xmax = ends
    ) |>
    dplyr::select(xmin, xmax)
  
  # Handle ongoing recession at end of sample
  if (nrow(recession_periods) > 0 && tail(df$USREC, 1) == 1) {
    last_start <- tail(df$date[df$USREC == 1], 1)
    if (!last_start %in% recession_periods$xmin) {
      recession_periods <- recession_periods |>
        add_row(xmin = last_start, xmax = max(df$date))
    }
  }
  
  return(recession_periods)
}


#' Plot time series of all variables
#'
#' @param df Dataset
#' @param recession_periods Recession shading data
#' @return ggplot object
#' @export
plot_time_series <- function(df, recession_periods) {
  
  # Dynamically select variables that exist
  vars_to_plot <- c("UNRATE", "SP500_ret")
  if ("PCEC96_growth" %in% colnames(df)) {
    vars_to_plot <- c("UNRATE", "PCEC96_growth", "SP500_ret")
  }
  
  df_long <- df |>
    dplyr::select(date, all_of(vars_to_plot)) |>
    pivot_longer(-date, names_to = "variable", values_to = "value")
  
  # Dynamic labels
  label_map <- c(
    UNRATE = "Unemployment Rate (%)",
    SP500_ret = "S&P 500 Returns (%)"
  )
  if ("PCEC96_growth" %in% colnames(df)) {
    label_map["PCEC96_growth"] <- "Real PCE Growth (%)"
  }
  
  ggplot(df_long, aes(x = date, y = value)) +
    geom_rect(data = recession_periods,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "gray80", alpha = 0.3) +
    geom_line(color = colors_custom$primary, size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.3) +
    facet_wrap(~variable, scales = "free_y", ncol = 1,
               labeller = as_labeller(label_map)) +
    labs(
      title = "Macroeconomic Time Series: 1950-2025",
      subtitle = "Gray shading indicates NBER-designated recessions",
      x = "Date",
      y = "Value"
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Plot histograms with diagnostics
#'
#' @param df Dataset
#' @param var Variable name to plot
#' @param var_label Label for x-axis
#' @return ggplot object
#' @export
plot_histogram <- function(df, var, var_label) {
  
  var_data <- df[[var]]
  
  # Calculate stats manually to avoid namespace conflicts
  var_mean <- mean(var_data, na.rm = TRUE)
  var_sd <- sd(var_data, na.rm = TRUE)
  var_skew <- moments::skewness(na.omit(var_data))
  
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = ..density..), 
                   bins = 40, 
                   fill = colors_custom$primary, 
                   color = "white", 
                   alpha = 0.7) +
    geom_density(color = colors_custom$negative, size = 1) +
    geom_vline(xintercept = var_mean, 
               linetype = "dashed", 
               color = "darkred", 
               size = 0.8) +
    labs(
      title = paste("Distribution of", var_label),
      subtitle = sprintf("Mean = %.2f, SD = %.2f, Skewness = %.2f",
                        var_mean, var_sd, var_skew),
      x = var_label,
      y = "Density"
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}


#' Plot rolling correlation
#'
#' @param df Dataset with rolling statistics
#' @param recession_periods Recession shading data
#' @return ggplot object
#' @export
plot_rolling_correlation <- function(df_rolling, recession_periods) {
  
  ggplot(df_rolling, aes(x = date, y = roll_cor_60)) +
    geom_rect(data = recession_periods,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "gray80", alpha = 0.3) +
    geom_line(color = colors_custom$primary, size = 1) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = as.Date("2007-12-01"), 
               linetype = "dashed", color = "darkred", size = 0.8) +
    geom_vline(xintercept = as.Date("2020-03-01"), 
               linetype = "dashed", color = "darkred", size = 0.8) +
    labs(
      title = "60-Month Rolling Correlation: UNRATE vs. SP500 Returns",
      subtitle = "Vertical lines mark 2008 financial crisis and 2020 pandemic",
      x = "Date",
      y = "Correlation Coefficient"
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Plot rolling R-squared
#'
#' @param df_rolling Dataset with rolling statistics
#' @param recession_periods Recession shading data
#' @return ggplot object
#' @export
plot_rolling_rsquared <- function(df_rolling, recession_periods) {
  
  ggplot(df_rolling, aes(x = date, y = roll_rsq_60)) +
    geom_rect(data = recession_periods,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "gray80", alpha = 0.3) +
    geom_line(color = colors_custom$positive, size = 1) +
    geom_hline(yintercept = 0.05, 
               linetype = "dotted", color = "darkred", size = 0.8) +
    geom_vline(xintercept = as.Date("2007-12-01"), 
               linetype = "dashed", color = "darkred", size = 0.8) +
    geom_vline(xintercept = as.Date("2020-03-01"), 
               linetype = "dashed", color = "darkred", size = 0.8) +
    annotate("text", x = as.Date("1970-01-01"), y = 0.05,
             label = "R² = 0.05 threshold", vjust = -0.5,
             color = "darkred", family = "serif", size = 3) +
    labs(
      title = "60-Month Rolling R²: UNRATE → SP500 Returns",
      subtitle = "R² < 0.05 indicates < 5% variance explained",
      x = "Date",
      y = "R² (Explanatory Power)"
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.25, 0.05),
      limits = c(0, NA),
      labels = scales::number_format(accuracy = 0.01)
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Plot period-specific scatter plots
#'
#' @param df Dataset with period indicators
#' @return ggplot object
#' @export
plot_period_scatters <- function(df) {
  
  # Calculate period statistics for annotation
  period_stats <- df |>
    group_by(period) |>
    summarise(
      n = n(),
      rsq = summary(lm(SP500_ret ~ UNRATE))$r.squared,
      beta = coef(lm(SP500_ret ~ UNRATE))[2],
      .groups = "drop"
    ) |>
    mutate(label = sprintf("N = %d\nR² = %.3f\nβ = %.2f", n, rsq, beta))
  
  ggplot(df, aes(x = UNRATE, y = SP500_ret)) +
    geom_point(aes(color = USREC_factor), alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = TRUE, 
                color = "darkred", size = 1.2, alpha = 0.2) +
    geom_text(data = period_stats, aes(label = label),
              x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
              family = "serif", size = 3.5, color = "darkred") +
    facet_wrap(~period, ncol = 3) +
    scale_color_manual(
      values = c("Expansion" = colors_custom$expansion,
                "Recession" = colors_custom$recession)
    ) +
    labs(
      title = "Structural Break Analysis: Pre-2008 vs. Post-2008 vs. Post-2020",
      subtitle = "Has the unemployment-returns relationship weakened over time?",
      x = "Unemployment Rate (%)",
      y = "S&P 500 Monthly Return (%)",
      color = "Economic State"
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11)
    )
}


#' Create 4-panel regression diagnostic plots
#'
#' @param model lm object
#' @param title Plot title
#' @return Grid of diagnostic plots
#' @export
plot_regression_diagnostics <- function(model, title = "Regression Diagnostics") {
  
  # Create diagnostic plots
  par(mfrow = c(2, 2), family = "serif")
  plot(model, which = 1:4)
  
  # Add title
  title(main = title, outer = TRUE, line = -1, cex.main = 1.5, font.main = 2)
  
  # Reset par
  par(mfrow = c(1, 1))
}

cat("✓ Visualization functions defined\n")


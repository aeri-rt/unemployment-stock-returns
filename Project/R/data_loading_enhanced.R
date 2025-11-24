# =============================================================================
# data_loading_enhanced.R
# Enhanced data loading with monetary policy variables
# =============================================================================

#' Load Wu-Xia Shadow Federal Funds Rate
#'
#' Downloads Wu-Xia shadow rate from Cynthia Wu's website
#' Note: Shadow rate only exists during ZLB periods (2008-2015, 2020-2022)
#' @return xts object with shadow rate
#' @export
load_wuxia_shadow_rate <- function() {
  
  # Define cache file location
  cache_dir <- file.path("data")
  cache_file <- file.path(cache_dir, "wuxia_shadow_rate_cached.csv")
  
  # Wu-Xia shadow rate hosted by Atlanta Fed / Cynthia Wu
  wuxia_url <- "https://www.atlantafed.org/-/media/documents/cqer/researchcq/shadow_rate/wu_xia_shadow_rate.xlsx"
  
  # Try to download fresh data
  message("Attempting to download Wu-Xia Shadow Federal Funds Rate...")
  
  wuxia_xts <- tryCatch({
    # Download to temp file
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(wuxia_url, temp_file, mode = "wb", quiet = TRUE)
    
    # Read Excel file
    wuxia_data <- readxl::read_excel(temp_file, skip = 0)
    
    # Clean column names
    colnames(wuxia_data) <- tolower(gsub("[^[:alnum:]_]", "_", colnames(wuxia_data)))
    
    # Convert to xts
    wuxia_xts <- xts::xts(
      wuxia_data[[2]],  # Second column is the rate
      order.by = as.Date(wuxia_data[[1]])
    )
    colnames(wuxia_xts) <- "WuXia_Shadow"
    
    # Cache the data for future use
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
      message("Created data/ directory for caching")
    }
    
    # Save to cache
    cache_data <- data.frame(
      date = zoo::index(wuxia_xts),
      WuXia_Shadow = zoo::coredata(wuxia_xts)
    )
    write.csv(cache_data, cache_file, row.names = FALSE)
    message(sprintf("✓ Cached Wu-Xia data to %s", cache_file))
    
    message(sprintf("✓ Wu-Xia Shadow Rate loaded: %d observations from %s to %s",
                    nrow(wuxia_xts),
                    format(zoo::index(wuxia_xts)[1], "%Y-%m"),
                    format(tail(zoo::index(wuxia_xts), 1), "%Y-%m")))
    
    return(wuxia_xts)
    
  }, error = function(e) {
    warning("Wu-Xia shadow rate download failed.")
    warning(paste("Error:", e$message))
    
    # Try to use cached file as fallback
    if (file.exists(cache_file)) {
      message(sprintf("→ Using cached Wu-Xia data from %s", cache_file))
      
      cached_data <- read.csv(cache_file)
      cached_data$date <- as.Date(cached_data$date)
      
      wuxia_xts_cached <- xts::xts(
        cached_data$WuXia_Shadow,
        order.by = cached_data$date
      )
      colnames(wuxia_xts_cached) <- "WuXia_Shadow"
      
      message(sprintf("✓ Loaded cached Wu-Xia data: %d observations from %s to %s",
                      nrow(wuxia_xts_cached),
                      format(zoo::index(wuxia_xts_cached)[1], "%Y-%m"),
                      format(tail(zoo::index(wuxia_xts_cached), 1), "%Y-%m")))
      
      return(wuxia_xts_cached)
    } else {
      warning("No cached Wu-Xia data available. Returning NULL.")
      warning("To manually cache data: download from URL and save as data/wuxia_shadow_rate_cached.csv")
      return(NULL)
    }
  })
  
  return(wuxia_xts)
}


#' Load Enhanced Monetary Policy & Market Variables
#'
#' Loads additional variables for Fed Put mechanism tests:
#' - Effective Federal Funds Rate
#' - VIX Index
#' - 10-Year Treasury Yield
#' - Term Spread (10Y - 2Y)
#' 
#' @return List of xts objects
#' @export
load_enhanced_variables <- function() {
  
  message("\n=== Loading Enhanced Variables ===\n")
  
  # Effective Federal Funds Rate (full sample)
  message("1. Effective Federal Funds Rate (DFF)...")
  fed_funds <- tryCatch({
    quantmod::getSymbols("DFF", src = "FRED", auto.assign = FALSE)
  }, error = function(e) {
    message("✗ Fed Funds download failed, trying alternative...")
    quantmod::getSymbols("FEDFUNDS", src = "FRED", auto.assign = FALSE)
  })
  colnames(fed_funds) <- "Fed_Funds_Rate"
  message(sprintf("✓ Fed Funds loaded: %d observations", nrow(fed_funds)))
  
  # VIX Index (CBOE Volatility Index)
  message("\n2. VIX Index (VIXCLS)...")
  vix <- tryCatch({
    quantmod::getSymbols("VIXCLS", src = "FRED", auto.assign = FALSE)
  }, error = function(e) {
    warning("✗ VIX download failed from FRED, trying Yahoo Finance...")
    quantmod::getSymbols("^VIX", src = "yahoo", auto.assign = FALSE)[, "VIX.Close"]
  })
  colnames(vix) <- "VIX"
  message(sprintf("✓ VIX loaded: %d observations from %s", 
                  nrow(vix), format(zoo::index(vix)[1], "%Y-%m")))
  
  # 10-Year Treasury Constant Maturity Rate
  message("\n3. 10-Year Treasury Yield (DGS10)...")
  treasury_10y <- tryCatch({
    quantmod::getSymbols("DGS10", src = "FRED", auto.assign = FALSE)
  }, error = function(e) {
    warning("✗ 10Y Treasury download failed")
    return(NULL)
  })
  if (!is.null(treasury_10y)) {
    colnames(treasury_10y) <- "Treasury_10Y"
    message(sprintf("✓ 10Y Treasury loaded: %d observations", nrow(treasury_10y)))
  }
  
  # 2-Year Treasury (for term spread calculation)
  message("\n4. 2-Year Treasury Yield (DGS2)...")
  treasury_2y <- tryCatch({
    quantmod::getSymbols("DGS2", src = "FRED", auto.assign = FALSE)
  }, error = function(e) {
    warning("✗ 2Y Treasury download failed")
    return(NULL)
  })
  if (!is.null(treasury_2y)) {
    colnames(treasury_2y) <- "Treasury_2Y"
    message(sprintf("✓ 2Y Treasury loaded: %d observations", nrow(treasury_2y)))
  }
  
  # Calculate Term Spread (if both available)
  term_spread <- NULL
  if (!is.null(treasury_10y) && !is.null(treasury_2y)) {
    term_spread <- treasury_10y - treasury_2y
    colnames(term_spread) <- "Term_Spread"
    message("✓ Term Spread calculated (10Y - 2Y)")
  }
  
  # Wu-Xia Shadow Rate (ZLB periods only)
  message("\n5. Wu-Xia Shadow Rate...")
  wuxia <- load_wuxia_shadow_rate()
  
  # Return list of all variables
  return(list(
    fed_funds = fed_funds,
    vix = vix,
    treasury_10y = treasury_10y,
    treasury_2y = treasury_2y,
    term_spread = term_spread,
    wuxia = wuxia
  ))
}


#' Create Composite Monetary Policy Stance Variable
#'
#' Combines Fed Funds Rate (when > 0.25%) and Wu-Xia Shadow Rate (when <= 0.25%)
#' This creates a continuous policy stance measure across entire sample
#' 
#' @param fed_funds xts with Fed Funds Rate
#' @param wuxia xts with Wu-Xia Shadow Rate (can be NULL)
#' @return xts with composite policy stance
#' @export
create_policy_stance_composite <- function(fed_funds, wuxia = NULL) {
  
  message("\n=== Creating Composite Policy Stance Variable ===\n")
  
  # Convert to monthly if needed
  fed_funds_monthly <- xts::to.monthly(fed_funds, OHLC = FALSE)
  
  # Initialize composite as Fed Funds
  policy_stance <- fed_funds_monthly
  colnames(policy_stance) <- "PolicyStance"
  
  # If Wu-Xia available, substitute during ZLB periods
  if (!is.null(wuxia)) {
    
    # Convert Wu-Xia to monthly
    wuxia_monthly <- xts::to.monthly(wuxia, OHLC = FALSE)
    
    # Merge
    merged <- merge(policy_stance, wuxia_monthly, join = "left")
    
    # Substitute Wu-Xia when Fed Funds <= 0.25%
    # (This captures ZLB periods when shadow rate is more informative)
    zlb_periods <- which(merged$PolicyStance <= 0.25)
    
    if (length(zlb_periods) > 0) {
      message(sprintf("Found %d ZLB months (Fed Funds <= 0.25%%)", length(zlb_periods)))
      
      # Replace with Wu-Xia where available
      wuxia_available <- !is.na(merged$WuXia_Shadow[zlb_periods])
      merged$PolicyStance[zlb_periods][wuxia_available] <- 
        merged$WuXia_Shadow[zlb_periods][wuxia_available]
      
      message(sprintf("Substituted Wu-Xia shadow rate for %d months",
                      sum(wuxia_available)))
    }
    
    policy_stance <- merged$PolicyStance
  }
  
  # Create "Policy Accommodation" variable (negative of policy stance)
  # Higher values = more accommodative (easier to interpret in regressions)
  policy_accommodation <- -1 * policy_stance
  colnames(policy_accommodation) <- "PolicyAccommodation"
  
  # Combine both
  result <- merge(policy_stance, policy_accommodation)
  
  message("\n✓ Composite Policy Stance created")
  message(sprintf("  Range: %.2f%% to %.2f%%",
                  min(result$PolicyStance, na.rm = TRUE),
                  max(result$PolicyStance, na.rm = TRUE)))
  message(sprintf("  Mean: %.2f%%, Median: %.2f%%",
                  mean(result$PolicyStance, na.rm = TRUE),
                  median(result$PolicyStance, na.rm = TRUE)))
  
  return(result)
}


#' Load All Data (Original + Enhanced)
#'
#' Wrapper function that loads both original variables and enhanced policy/market variables
#' @return List with raw_data and enhanced_data
#' @export
load_all_data_enhanced <- function() {
  
  # Load original variables (UNRATE, USREC, PCECC96, SP500)
  message("=== Loading Original Variables ===\n")
  raw_data <- load_raw_data()
  
  # Load enhanced variables
  enhanced_vars <- load_enhanced_variables()
  
  # Create composite policy stance
  policy_stance <- create_policy_stance_composite(
    enhanced_vars$fed_funds,
    enhanced_vars$wuxia
  )
  
  # Add policy stance to enhanced vars
  enhanced_vars$policy_stance <- policy_stance
  
  message("\n=== Data Loading Complete ===\n")
  
  return(list(
    raw_data = raw_data,
    enhanced_vars = enhanced_vars
  ))
}



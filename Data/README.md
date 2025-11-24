# Data Sources and Acquisition

## Overview

This project analyzes the relationship between unemployment and stock returns using monthly U.S. macroeconomic and financial market data spanning **January 1948 – January 2025** (N = 925 months). All data are publicly available and programmatically downloaded via the R scripts included in `../Project/R/`. This README documents the data sources, variable definitions, and acquisition procedures to ensure full reproducibility.

---

## Primary Data Sources

### 1. Federal Reserve Economic Data (FRED)
**Source:** Federal Reserve Bank of St. Louis  
**URL:** https://fred.stlouisfed.org  
**Access Method:** R package `quantmod::getSymbols()` with `src="FRED"`  
**Authentication:** None required (public API)

**Variables Downloaded:**

| FRED Code | Variable Name | Description | Frequency | Start Date |
|-----------|---------------|-------------|-----------|------------|
| `UNRATE` | Unemployment Rate | Civilian unemployment rate (%), seasonally adjusted | Monthly | Jan 1948 |
| `DFF` | Fed Funds Rate | Effective Federal Funds Rate (%) | Daily → Monthly avg | Jul 1954 |
| `FEDFUNDS` | Fed Funds (alt) | Federal Funds Target Rate (backup source) | Monthly | Jul 1954 |
| `DGS10` | 10-Year Treasury | 10-Year Treasury Constant Maturity Rate (%) | Daily → Monthly avg | Apr 1953 |
| `DGS2` | 2-Year Treasury | 2-Year Treasury Constant Maturity Rate (%) | Daily → Monthly avg | Jun 1976 |
| `USREC` | Recession Indicator | NBER-based recession indicator (1 = recession) | Monthly | Jan 1948 |

**Notes:**
- FRED data are automatically updated as new releases become available
- Monthly averages are computed for daily series to match unemployment frequency
- Missing values in early sample periods (e.g., 2Y Treasury pre-1976) are handled via listwise deletion in regression models

---

### 2. Yahoo Finance
**Source:** Yahoo Finance Historical Data  
**URL:** https://finance.yahoo.com  
**Access Method:** R package `quantmod::getSymbols()` with `src="yahoo"`  
**Authentication:** None required (public data)

**Variables Downloaded:**

| Ticker | Variable Name | Description | Frequency | Start Date |
|--------|---------------|-------------|-----------|------------|
| `^GSPC` | S&P 500 Index | S&P 500 price index (adjusted close) | Daily → Monthly | Jan 1950 |
| `^VIX` | VIX Index | CBOE Volatility Index (implied volatility, %) | Daily → Monthly | Jan 1990 |

**Transformations Applied:**
- **S&P 500 Returns:** Log returns computed as `log(P_t / P_{t-1}) × 100` (percentage points)
- **VIX:** Monthly averages of daily values (VIX is reported as an annualized percentage)

**Notes:**
- Yahoo Finance data quality is generally high but may contain rare outliers (verified via visual inspection)
- VIX begins in January 1990; models requiring VIX use this restricted sample
- Adjusted close prices are used to account for dividends and stock splits

---

### 3. Wu-Xia Shadow Federal Funds Rate
**Source:** Federal Reserve Bank of Atlanta / Cynthia Wu and Fan Dora Xia  
**URL:** https://www.atlantafed.org/cqer/research/wu-xia-shadow-federal-funds-rate  
**Access Method:** Direct Excel download via `download.file()` + `readxl::read_excel()`  
**Authentication:** None required (public data)

**Variable Description:**
- **Wu-Xia Shadow Rate:** Estimated "shadow" Fed Funds Rate extending below the zero lower bound (ZLB)
- **Time Period:** January 2008 – December 2022 (zero lower bound episodes)
- **Rationale:** When the Fed Funds Rate hits 0%, the shadow rate continues into negative territory, providing a continuous measure of monetary policy stance during unconventional policy periods (QE, forward guidance)

**Fallback Mechanism:**
- Primary: Download from Atlanta Fed URL
- Fallback: Use cached `wuxia_shadow_rate_cached.csv` (included in this folder) if download fails
- This ensures reproducibility even if the Atlanta Fed URL changes

**Composite Policy Stance Variable:**
```r
PolicyStance = {
  Fed Funds Rate            if date < Jan 2008 or date > Dec 2022
  Wu-Xia Shadow Rate        if Jan 2008 ≤ date ≤ Dec 2022
}
```

---

## Data Acquisition Procedure

### Automated Download (Recommended)

The R Markdown file `Report_Trinh.rmd` sources all data loading functions from `../Project/R/data_loading.R` and `../Project/R/data_loading_enhanced.R`. Running the RMD will automatically:

1. **Load base variables** (UNRATE, S&P 500) via `load_base_data()`
2. **Compute S&P 500 log returns**
3. **Load enhanced variables** (VIX, Fed Funds, Wu-Xia, Treasury yields) via `load_enhanced_variables()`
4. **Merge and align time series** to monthly frequency
5. **Handle missing values** via listwise deletion after all transformations

**Requirements:**
- R packages: `quantmod`, `xts`, `zoo`, `readxl`, `dplyr`, `lubridate`
- Internet connection for FRED API and Yahoo Finance downloads
- No API keys or authentication required

**Execution Time:** ~15-30 seconds (depending on network speed)

### Manual Verification (Optional)

To verify data integrity independently:

```r
# Example: Manually verify UNRATE
library(quantmod)
UNRATE <- getSymbols("UNRATE", src = "FRED", auto.assign = FALSE)
head(UNRATE)
tail(UNRATE)
```

---

## Data Transformations

### 1. AR(15) Unemployment Decomposition
The unemployment rate is decomposed into **anticipated** (forecastable) and **unanticipated** (surprise) components using an AR(15) autoregressive model:

```
UNRATE_t = α + Σ_{i=1}^{15} β_i × UNRATE_{t-i} + ε_t

UNRATE_anticipated_t = fitted values
UNRATE_unanticipated_t = residuals
```

**Implementation:** `../Project/R/anticipated_decomposition.R`  
**Lag Order Justification:** AR(15) selected following Gonzalo & Taamouti (2017) via AIC/BIC minimization  
**Timing Convention:** `UNRATE_anticipated_t` is constructed using information available at time `t-1`, making it a truly predictive variable

**Robustness:** Results are insensitive to lag order (tested AR(6), AR(12), AR(18), AR(24))

### 2. Composite Monetary Policy Stance
To maintain a continuous measure of Fed policy across conventional and unconventional regimes:

```r
PolicyStance = ifelse(date >= "2008-01-01" & date <= "2022-12-31",
                      WuXia_Shadow,
                      Fed_Funds_Rate)
```

### 3. Term Spread
```
Term_Spread = Treasury_10Y - Treasury_2Y
```

A traditional business cycle indicator; negative values (inverted yield curve) often precede recessions.

---

## Data Quality and Limitations

### Stationarity
- **UNRATE:** Stationary via Augmented Dickey-Fuller test (p < 0.01)
- **SP500_ret:** Stationary (log returns eliminate unit root in price levels)

### Missing Data Handling
- **Fed Funds Rate:** Begins July 1954; pre-1954 observations excluded from models requiring this variable
- **VIX:** Begins January 1990; full-sample models exclude VIX to maximize observations
- **Wu-Xia Shadow Rate:** Only available 2008-2022; `PolicyStance` composite uses Fed Funds elsewhere
- **2-Year Treasury:** Begins June 1976; term spread unavailable before this date

### Known Issues
1. **Yahoo Finance Outliers:** Rare anomalous price observations (manually verified and accepted as legitimate market moves)
2. **Wu-Xia URL Fragility:** Atlanta Fed may update file location; cached file provided as fallback
3. **BLS Release Timing:** Unemployment data released ~1 week into month `t+1`; we assume markets observe UNRATE_t at month-end for simplicity (conservative timing assumption)

---

## Sample Periods

| Analysis | Start | End | N | Rationale |
|----------|-------|-----|---|-----------|
| **Full Sample** | Jan 1948 | Jan 2025 | 925 | Maximum historical coverage |
| **Baseline OLS** | Jan 1950 | Jan 2025 | 901 | S&P 500 data begins 1950 |
| **With VIX** | Jan 1990 | Jan 2025 | 421 | VIX begins 1990 |
| **With Policy** | Jul 1954 | Jan 2025 | 847 | Fed Funds begins 1954 |
| **Post-2014** | Jan 2015 | Jan 2025 | 121 | End of Fed taper, structural break candidate |

---

## Reproducibility Statement

All data used in this analysis are **publicly available** and **programmatically downloadable** without authentication. The R scripts in `../Project/R/` provide a fully automated pipeline from raw data to final results. No manual data processing or proprietary sources are required.

**To replicate:**
1. Ensure internet connection for FRED/Yahoo downloads
2. Install required R packages (see `../Project/R/setup.R`)
3. Run `Report_Trinh.rmd` — all data will be downloaded and processed automatically

**Data Vintage Note:** FRED and Yahoo Finance data are subject to revisions. To ensure exact replication of published results, we recommend using the time-stamped version available via the GitHub repository (see main README).

---

## References

**Data Sources:**
- Federal Reserve Bank of St. Louis. (2025). *Federal Reserve Economic Data (FRED)*. https://fred.stlouisfed.org
- Wu, J. C., & Xia, F. D. (2016). Measuring the macroeconomic impact of monetary policy at the zero lower bound. *Journal of Money, Credit and Banking*, 48(2-3), 253-291.
- Yahoo Finance. (2025). *Historical Market Data*. https://finance.yahoo.com

**Methodological References:**
- Gonzalo, J., & Taamouti, A. (2017). The reaction of stock market returns to unemployment. *Nonparametric Econometric Methods*, 38, 1-46. [AR(15) decomposition]

---

## Contact

For questions regarding data sources or acquisition procedures, please refer to the main project README or contact the author via the GitHub repository.

**Last Updated:** January 2025


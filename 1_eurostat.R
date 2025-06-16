# ============================================================================
# Eurostat Climate Indicators Extraction Script
#
# Variable Definitions:
# 1. GHG emissions per capita (sdg_13_10):
#    - freq: Annual frequency code ('A').
#    - src_crf: Source CRF category.
#    - unit: 'T_HAB' = tonnes CO₂-equivalent per inhabitant; 'I90' = index.
#    - geo: Country code.
#    - TIME_PERIOD / time: Reference year, parsed Date.
#    - values: Numeric emissions per capita.
#
# 2. Renewable-energy share (sdg_07_40):
#    - freq: Annual.
#    - nrg_bal: Energy balance item code.
#    - unit: 'PC' = percent share.
#    - geo: Country code.
#    - TIME_PERIOD / time: Year.
#    - values: Numeric share (%).
#
# 3. Green public investment per capita (sdg_13_60):
#    - freq: Annual.
#    - unit: 'PC_POP' = euro per inhabitant; 'MIO_PER' for millions.
#    - geo: Country code.
#    - TIME_PERIOD / time: Year.
#    - values: Numeric per-capita investment (€).
#
# 4. GDP per capita at constant prices (sdg_08_10):
#    - freq: Annual.
#    - unit: 'CLV20_EUR_HAB' = constant 2020 euro per inhabitant; 'CLV_PCH_PRE_HAB' = percent change.
#    - na_item: Level vs change.
#    - geo: Country code.
#    - TIME_PERIOD / time: Year.
#    - values: Numeric GDP per capita.
#
# 5. Public environmental expenditure (gov_10a_exp):
#    - freq: Annual.
#    - unit: 'PC_GDP' = percent of GDP; 'MIO_EUR' for absolute.
#    - sector: 'S13' = general government.
#    - cofog99: '05' or '050' for environmental protection.
#    - geo: Country code.
#    - TIME_PERIOD / time: Year.
#    - values: Numeric expenditure (% of GDP).
#
# 6. Merged Panel:
#    - country: Full country name.
#    - year: Calendar year (numeric).
#    - ghg_tco2_pc: Emissions per capita (tonnes CO₂-eq).
#    - res_share_pc: Renewable share (%).
#    - green_inv_eur_pc: Green investment per capita (€).
#    - gdp_constant20_eur_pc: GDP per capita constant 2020 €.
#    - env_exp_pc_gdp: Environmental expenditure (% of GDP).
# ============================================================================

# Load libraries
library(eurostat)    # For data download and metadata :contentReference[oaicite:35]{index=35}
library(tidyverse)   # For dplyr, purrr, etc. :contentReference[oaicite:36]{index=36}
library(lubridate)   # For date parsing :contentReference[oaicite:37]{index=37}

# Helper function definition (as above)
get_series <- function(code, filters, keep = c("geo", "time", "values")) {
  dat_full <- get_eurostat(code, time_format = "date", cache = TRUE)
  nm <- names(dat_full)
  if ("time" %in% nm) {
    time_col <- "time"
  } else if ("TIME_PERIOD" %in% nm) {
    time_col <- "TIME_PERIOD"
  } else {
    date_cols <- nm[sapply(dat_full, function(x) inherits(x, "Date"))]
    if (length(date_cols) == 1) {
      time_col <- date_cols[[1]]
    } else if (length(date_cols) > 1) {
      stop(sprintf(
        "Multiple Date columns in '%s': %s. Inspect names(dat_full).",
        code, paste(date_cols, collapse = ", ")
      ))
    } else {
      stop(sprintf(
        "No 'time' or 'TIME_PERIOD' column found in '%s'. Columns: %s",
        code, paste(nm, collapse = ", ")
      ))
    }
  }
  if (time_col != "time") {
    dat_full <- dat_full %>% rename(time = all_of(time_col))
    if (!inherits(dat_full$time, "Date")) {
      parsed <- as_date(dat_full$time)
      if (all(!is.na(parsed))) {
        dat_full <- dat_full %>% mutate(time = parsed)
      } else {
        warning(sprintf(
          "Column '%s' in '%s' could not be parsed as Date; kept original",
          time_col, code
        ))
      }
    }
  }
  for (col in names(filters)) {
    if (!col %in% names(dat_full)) {
      stop(sprintf(
        "Filter column '%s' not present in dataset '%s'. Available columns: %s",
        col, code, paste(names(dat_full), collapse = ", ")
      ))
    }
    vals <- filters[[col]]
    actual_vals <- unique(dat_full[[col]])
    missing_vals <- setdiff(vals, actual_vals)
    if (length(missing_vals) > 0) {
      warning(sprintf(
        "In dataset '%s', filter '%s' requested values %s not found. Available sample: %s",
        code, col,
        paste(missing_vals, collapse = ", "),
        paste(head(actual_vals, 10), collapse = ", ")
      ))
    }
  }
  dat_filtered <- dat_full
  for (col in names(filters)) {
    dat_filtered <- dat_filtered %>% filter(.data[[col]] %in% filters[[col]])
  }
  if (nrow(dat_filtered) == 0) {
    warning(sprintf(
      "After filtering, no rows remain for '%s' with filters %s",
      code,
      paste(names(filters), filters, sep = "=", collapse = ", ")
    ))
  }
  missing_cols <- setdiff(keep, names(dat_filtered))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing columns %s after filtering '%s'",
      paste(missing_cols, collapse = ", "), code
    ))
  }
  dat_out <- dat_filtered %>% select(all_of(keep))
  return(dat_out)
}

# ----------------------------------------------------------------------------
# 3. Inspect dimension codes interactively:
# ----------------------------------------------------------------------------

# 3.1 GHG emissions per capita (sdg_13_10)
dat_ghg_raw <- get_eurostat("sdg_13_10", time_format = "raw", cache = TRUE)  # :contentReference[oaicite:38]{index=38}
print(names(dat_ghg_raw))
print(distinct(dat_ghg_raw, unit))
print(distinct(dat_ghg_raw, geo))
print(head(dat_ghg_raw$TIME_PERIOD))

# 3.2 Renewable-energy share (sdg_07_40)
dat_res_raw <- get_eurostat("sdg_07_40", time_format = "raw", cache = TRUE)  # :contentReference[oaicite:39]{index=39}
print(names(dat_res_raw))
print(distinct(dat_res_raw, unit))
print(distinct(dat_res_raw, geo))



# 3.4 GDP per capita at constant prices (sdg_08_10)
dat_gdp_raw <- get_eurostat("sdg_08_10", time_format = "raw", cache = TRUE)  # :contentReference[oaicite:40]{index=40}
print(names(dat_gdp_raw))
print(distinct(dat_gdp_raw, unit))
print(distinct(dat_gdp_raw, na_item))

# 3.5 Public environmental expenditure (gov_10a_exp)
dat_env_raw <- get_eurostat("gov_10a_exp", time_format = "raw", cache = TRUE)  # :contentReference[oaicite:41]{index=41}
print(names(dat_env_raw))
print(distinct(dat_env_raw, unit))
print(distinct(dat_env_raw, sector))
print(distinct(dat_env_raw, cofog99))
print(distinct(dat_env_raw, na_item))

#
# ----------------------------------------------------------------------------
# 4. Merge into country-year panel:
# ----------------------------------------------------------------------------
# Only this package is needed for labeling
library(eurostat)

# Normalize all column names to lowercase
names(dat_ghg_raw) <- tolower(names(dat_ghg_raw))
names(dat_res_raw) <- tolower(names(dat_res_raw))
names(dat_gdp_raw) <- tolower(names(dat_gdp_raw))
names(dat_env_raw) <- tolower(names(dat_env_raw))

# Survey countries to keep
survey_countries <- c("AT", "BE", "CZ", "FI", "FR", "GB", "HU", "IS", "PT", "SI", "PL")

# Prepare each dataset
ghg <- dat_ghg_raw %>%
  subset(unit == "I90" & src_crf == "TOTX4_MEMO", select = c(geo, time_period, values))
names(ghg)[3] <- "ghg_tco2_pc"

res <- dat_res_raw %>%
  subset(unit == "PC" & nrg_bal == "REN", select = c(geo, time_period, values))
names(res)[3] <- "res_share_pc"

gdp <- dat_gdp_raw %>%
  subset(unit == "CLV20_EUR_HAB" & na_item == "B1GQ", select = c(geo, time_period, values))
names(gdp)[3] <- "gdp_const20_eur_pc"

env <- dat_env_raw %>%
  subset(unit == "MIO_EUR" & sector == "S13" & cofog99 == "GF01" & na_item == "DI",
         select = c(geo, time_period, values))
names(env)[3] <- "env_exp_pc_gdp"

# Merge and format panel
panel <- Reduce(function(x, y) merge(x, y, by = c("geo", "time_period"), all = TRUE),
                list(ghg, res, gdp, env))

# Add year and country labels
panel$year    <- as.integer(panel$time_period)
panel$country <- label_eurostat(panel$geo, dic = "geo")

# Keep survey countries only
panel <- subset(panel, geo %in% survey_countries)

# Reorder
panel <- panel[c("country", "year", "ghg_tco2_pc", "res_share_pc",
                 "gdp_const20_eur_pc", "env_exp_pc_gdp")]

# View first few rows
head(panel)

# Save outputs
write_csv(panel, "J:/Mi unidad/Hackathon/cronos1/eurostat_panel.csv")   # :contentReference[oaicite:50]{index=50}
saveRDS(panel, "J:/Mi unidad/Hackathon/cronos1/eurostat_panel.rds")

# Print summary
print(glimpse(panel))
###########
library(tidyverse)
library(zoo)          # rollmeanr()
library(countrycode)  # to match ISO-2 codes if needed

dat_clean <- read_csv("J:/Mi unidad/Hackathon/cronos1/dat_clean.csv") 
panel <- read_csv("J:/Mi unidad/Hackathon/cronos1/eurostat_panel.csv") 
panel <- panel %>%                         # ← contains cumulative cols
  mutate(cntry = countrycode(country,
                             origin      = "country.name",
                             destination = "iso2c"))


###############################################################################
## 1 · add ISO-2 code to panel_cum  ➜  cntry   +  de-duplicate
###############################################################################
library(dplyr)
library(countrycode)
library(zoo)

panel_cum <- panel %>%                         # ← contains cumulative cols
  mutate(cntry = countrycode(country,
                             origin      = "country.name",
                             destination = "iso2c"))

panel_cum_clean <- panel_cum %>%                   # one row / country / year
  group_by(cntry, year) %>%
  summarise(across(where(is.numeric),
                   ~ .x[which.max(!is.na(.x))]),
            .groups = "drop")


###############################################################################
##  1 · 2023 macro snapshot (already cumulative up to 2023)
###############################################################################
macro_2023 <- panel %>%
  dplyr::filter(year == 2023) %>%
  dplyr::select(
    cntry,                 # ISO-2 code
    ghg_tco2_pc,            # cumulative t CO₂-eq / inhab 1990–2021
    res_share_pc,            # trailing 5-yr mean RES share
    gdp_const20_eur_pc  # trailing 5-yr mean env. spending
  )

# inspect
print(macro_2023)

###############################################################################
##  2 · ensure `cluster` exists in dat_clean  (terciles of risk_score)
###############################################################################
if (!"cluster" %in% names(dat_clean)) {
  dat_clean <- dat_clean %>% 
    mutate(
      cluster = cut(
        risk_score,
        quantile(risk_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
        labels = c("Low", "Mid", "High"), include.lowest = TRUE
      )
    )
}

###############################################################################
##  3 · merge context  +  keep rows that have the mandatory macro columns
###############################################################################
dat_full <- dat_clean %>%                       # 9 744 respondents
  left_join(macro_2023, by = "cntry") %>%       # adds six macro columns
  drop_na(w1gq13, ghg_tco2_pc, res_share_pc, cluster) %>%   # ← no gdp_const20_eur_pc
  ## keep gdp_const20_eur_pc even if NA; still include in model
  mutate(
    cluster = droplevels(cluster),
    w1gq13  = factor(w1gq13, ordered = TRUE),
    across(c(ghg_tco2_pc, res_share_pc, gdp_const20_eur_pc), scale)
  )
###############################################################################
## 8 · SURVEY-WEIGHTED ORDINAL LOGIT  (no random effects)
###############################################################################
library(survey)     # svydesign(), svyolr()
library(tibble)     # tibble()

# 1 · build survey design
des_full <- svydesign(
  ids     = ~cntry,                 # simple random sample → one PSU
  weights = ~w1weight,          # CRONOS design weights
  data    = dat_full
)

# 2 · fit survey-weighted cumulative-logit model
model_svy <- svyolr(
  w1gq13 ~ cluster + ghg_tco2_pc + res_share_pc + gdp_const20_eur_pc + gndr + age + eisced + hinctnta + w1xq3 + hincfel,
  design = des_full,
  method = "logistic"           # proportional-odds link
)

# 3 · quick summary
print(summary(model_svy))

# 4 · odds-ratios & 95 % CI
or <- exp(coef(model_svy))
ci <- exp(confint(model_svy))

svy_report <- tibble(
  term    = names(or),
  or      = or,
  or_low  = ci[, 1],
  or_high = ci[, 2]
)

print(svy_report)

# 5 · (optional) write to disk
write_csv(svy_report,
          "J:/Mi unidad/Hackathon/cronos1/table_A1_survey_only_GDP.csv")

cat("✔  Survey-weighted odds-ratios written to",
    "table_A1_survey_only_GDP.csv\n")
###############################################################################
##  Survey-weighted ordinal logit  – by country
###############################################################################
library(survey)   # svydesign(), svyolr()
library(purrr)    # split / map
library(broom)    # tidy()

# 0 · function to fit one model on a survey design ----------------------------
fit_one <- function(df) {
  svyobj <- svydesign(ids = ~1, weights = ~w1weight, data = df)
  svyolr(w1gq13 ~ cluster + ghg_tco2_pc + res_share_pc + gdp_const20_eur_pc + gndr + age + eisced + hinctnta + w1xq3 + hincfel,
         design = svyobj, method = "logistic")
}

# 1 · split data frame by cntry and fit ---------------------------------------
country_models <- dat_full %>%
  split(.$cntry) %>%                 # named list: "AT", "BE", …
  map(fit_one)

# 2 · tidy into OR table ------------------------------------------------------
country_or <- imap_dfr(country_models, \(mod, iso) {
  est  <- tidy(mod, conf.int = TRUE)
  tibble(
    cntry   = iso,
    term    = est$term,
    or      = exp(est$estimate),
    or_low  = exp(est$conf.low),
    or_high = exp(est$conf.high)
  )
})

print(country_or)

# 3 · write to disk -----------------------------------------------------------
write_csv(country_or,
          "J:/Mi unidad/Hackathon/cronos1/table_A1_survey_by_country_GDP.csv")

cat("✔ Survey-weighted country tables → table_A1_survey_by_country_GDP.csv\n")
###############################################################################
##  Export main & per-country odds-ratio tables + model bundle               ##
###############################################################################
library(broom)      # tidy()
library(readr)      # write_csv()

## 0 · define (and, if missing, create) the output folder ---------------------
out_dir <- "J:/Mi unidad/Hackathon/Results/Results"   # final destination
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## 1 · MAIN MODEL  ------------------------------------------------------------
main_or <- tidy(model_svy, conf.int = TRUE) %>% 
  transmute(term,
            or      = exp(estimate),
            or_low  = exp(conf.low),
            or_high = exp(conf.high))

write_csv(main_or, file.path(out_dir, "table_A1_main.csv"))
cat("✔  Saved main OR table  →", file.path(out_dir, "table_A1_main.csv"), "\n")

## 2 · BY-COUNTRY MODELS  -----------------------------------------------------
country_or <- purrr::imap_dfr(country_models, \(mod, iso) {
  tidy(mod, conf.int = TRUE) %>% 
    transmute(cntry   = iso,
              term,
              or      = exp(estimate),
              or_low  = exp(conf.low),
              or_high = exp(conf.high))
})

write_csv(country_or, file.path(out_dir, "table_A1_by_country.csv"))
cat("✔  Saved country OR table →", file.path(out_dir, "table_A1_by_country.csv"), "\n")

## 3 · OPTIONAL: bundle everything as a single .rds  --------------------------
saveRDS(
  list(
    main_model        = model_svy,
    country_models    = country_models,
    main_or_table     = main_or,
    country_or_table  = country_or
  ),
  file = file.path(out_dir, "policy_logit_results.rds")
)
cat("✔  Saved full result bundle →", file.path(out_dir, "policy_logit_results.rds"), "\n")



"w1gq14"

###############################################################################
## 0 · SET-UP -----------------------------------------------------------------
###############################################################################
library(survey)   # svydesign(), svyolr()
library(dplyr)    # mutate(), across(), %>% 
library(purrr)    # map(), imap()
library(broom)    # tidy()
library(readr)    # write_csv()

# ---- editable paths ---------------------------------------------------------
out_dir <- "J:/Mi unidad/Hackathon/Results/Results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- outcome variables ------------------------------------------------------
outcomes <- c("w1gq13","w1gq14", "w1gq15", "w1gq16")   # <- looped responses

###############################################################################
## 1 · RECODE OUTCOMES AS ORDERED FACTORS -------------------------------------
###############################################################################
dat_full <- dat_full %>%
  mutate(across(all_of(outcomes),
                ~ ordered(.x, levels = sort(unique(na.omit(.x))))))

###############################################################################
## 2 · BUILD SURVEY DESIGN (pooled across countries) --------------------------
###############################################################################
des_full <- svydesign(
  ids     = ~cntry,   # one PSU per country
  weights = ~w1weight,
  data    = dat_full
)

###############################################################################
## 3 · MAIN MODELS (pooled) ---------------------------------------------------
###############################################################################
main_models <- set_names(outcomes) |>
  map(\(resp) {
    fml <- reformulate(
      termlabels = c(
        "cluster", "ghg_tco2_pc", "res_share_pc", "gdp_const20_eur_pc",
        "gndr", "age", "eisced", "hinctnta", "w1xq3", "hincfel"
      ),
      response = resp
    )
    svyolr(fml, design = des_full, method = "logistic")
  })

main_or <- imap_dfr(main_models, \(mod, resp) {
  tidy(mod, conf.int = TRUE) %>%
    transmute(
      outcome = resp,
      term,
      or      = exp(estimate),
      or_low  = exp(conf.low),
      or_high = exp(conf.high)
    )
})

write_csv(main_or, file.path(out_dir, "table_A1_main_alloutcomes.csv"))
cat("✔  Pooled OR tables → table_A1_main_alloutcomes.csv\n")

###############################################################################
## 4 · COUNTRY-BY-COUNTRY MODELS ---------------------------------------------
###############################################################################
fit_one <- function(df, outcome) {
  svyobj <- svydesign(ids = ~1, weights = ~w1weight, data = df)
  fml <- reformulate(
    termlabels = c(
      "cluster", "ghg_tco2_pc", "res_share_pc", "gdp_const20_eur_pc",
      "gndr", "age", "eisced", "hinctnta", "w1xq3", "hincfel"
    ),
    response = outcome
  )
  svyolr(fml, design = svyobj, method = "logistic")
}

country_models <- set_names(outcomes) |>
  map(\(resp) {
    # *** NOTE: explicit split() call – no magrittr dot pronoun ***
    split(dat_full, dat_full$cntry) |>
      map(~ fit_one(.x, resp))
  })

country_or <- imap_dfr(country_models, \(mods, resp) {
  imap_dfr(mods, \(mod, iso) {
    tidy(mod, conf.int = TRUE) %>%
      transmute(
        outcome = resp,
        cntry   = iso,
        term,
        or      = exp(estimate),
        or_low  = exp(conf.low),
        or_high = exp(conf.high)
      )
  })
})

write_csv(country_or, file.path(out_dir, "table_A1_by_country_alloutcomes.csv"))
cat("✔  Country OR tables → table_A1_by_country_alloutcomes.csv\n")

###############################################################################
## 5 · SAVE EVERYTHING IN ONE .RDS -------------------------------------------
###############################################################################
saveRDS(
  list(
    main_models       = main_models,
    country_models    = country_models,
    main_or_table     = main_or,
    country_or_table  = country_or
  ),
  file = file.path(out_dir, "policy_logit_results_alloutcomes.rds")
)
cat("✔  Full model bundle → policy_logit_results_alloutcomes.rds\n")


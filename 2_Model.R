# core tools
library(tidyverse)            # pipes, dplyr, purrr, ggplot2
library(forcats)              # factor helpers
library(psych)                # fa() for EFA      :contentReference[oaicite:0]{index=0}
library(nFactors)             # parallel()        :contentReference[oaicite:1]{index=1}
library(FactoClass)           # kmeansW()         :contentReference[oaicite:2]{index=2}
library(cluster)              # silhouette()      :contentReference[oaicite:3]{index=3}
library(survey)               # svydesign()       :contentReference[oaicite:4]{index=4}
library(nnet)                 # multinom()
library(conflicted)           # namespace hygiene
conflict_prefer("select", "dplyr")
raw <- read_csv("J:/Mi unidad/Hackathon/cronos1/CRON3W1e01.1.csv")
View(raw)
risk_vars   <- c("w1gq5","w1gq1", "w1gq10")
policy_vars <- c("w1gq13","w1gq14","w1gq15","w1gq16")
demo_vars   <- c("gndr","age","eisced","hinctnta","w1xq3", "hincfel", "cntry")
weight_var  <- "w1weight"

missing_codes <- list(
  gndr     = 9L,
  age      = 999L,
  eisced   = c(55L,77L, 88L,99L),
  hinctnta = c(77L,88L,99L),
  w1xq3    = 9L, 
  w1gq5    = 9L, 
  w1gq13    = 9L, 
  w1gq14    = 9L, 
  w1gq15    = 9L,
  w1gq16    = 9L,
  w1gq1    = 9L,
  w1gq10    = 99L,
  hincfel    = c(9L, 8L, 7L)
  
)

dat_clean <- raw %>% 
  ## 1. keep only variables of interest
  select(all_of(c(risk_vars, policy_vars, demo_vars, weight_var))) %>% 
  
  ## 2. apply the custom NA codes
  mutate(across(all_of(names(missing_codes)),
                ~ replace(.x, .x %in% missing_codes[[cur_column()]], NA_real_))) %>% 
  
  ## 3. cast the questionnaire items to integer
  mutate(across(all_of(c(risk_vars, policy_vars)), as.integer)) %>% 
  
  ## 4. label gender
  mutate(gndr = factor(gndr,
                       levels = c(1L, 2L),
                       labels = c("Male", "Female"))) %>% 
  
  ## 5. drop respondents with any missing risk-item response
  drop_na(all_of(risk_vars))

library(psych)        # mixed.cor(), fa()
library(polycor)      # alternative: hetcor()

risk_vars <- c("w1gq1", "w1gq5", "w1gq10")           # <- all respondents

# 1. Tell mixed.cor() which variables are ordinal vs. numeric
types <- c("poly", "poly", "pearson")        # order must match risk_vars

mx  <- mixed.cor(dat_clean[risk_vars], c = types)
R   <- mx$rho 

## 1.2  One‑factor EFA on the matrix
efa     <- fa(R, nfactors = 1, fm = "ml", rotate = "none")
scores  <- factor.scores(dat_clean[, risk_vars],
                         efa, method = "tenBerge")$scores[, 1]

## 1.3  Attach score + deterministic terciles
dat_clean <- dat_clean %>%
  mutate(
    risk_score = as.vector(scale(scores)),
    cluster    = cut(risk_score,
                     quantile(risk_score,
                              probs = c(0, 1/3, 2/3, 1),
                              na.rm  = TRUE),
                     labels = c("Low", "Mid", "High"),
                     include.lowest = TRUE)
  )

write_csv(dat_clean, "J:/Mi unidad/Hackathon/cronos1/dat_clean.csv") 
###############################################################################
##  0 · Libraries & helper -----------------------------------------------------
###############################################################################
library(tidyverse)
library(psych)        # fa()
library(FactoClass)   # kmeansW()
library(survey)       # svydesign(), svyolr(), svyby()
library(cluster)      # silhouette()

###############################################################################
##  1 · Analysis core  (UNTOUCHED) --------------------------------------------
###############################################################################
safe_analyse <- safely(function(df, w = "w1weight") {
  # 1 · EFA ----------------------------------------------------------
  efa <- fa(df[risk_vars], nfactors = 1, fm = "ml", rotate = "varimax")
  df$risk_score <- as.vector(efa$scores[, 1])
  
  # 2 · Weighted k-means --------------------------------------------
  z  <- scale(df$risk_score)
  km <- kmeansW(matrix(z, ncol = 1), centers = 3,
                weight = df[[w]], nstart = 25)
  df$cluster <- factor(km$cluster,
                       levels = 1:3,
                       labels = c("Low", "Mid", "High"))
  
  # 3 · Viability gate ----------------------------------------------
  if (n_distinct(na.omit(df$cluster)) < 2 || nrow(df) < 30)
    stop("insufficient data after clustering")
  
  # 4 · Survey design + ordinal logit -------------------------------
  des <- svydesign(ids = ~1, weights = df[[w]], data = df)
  
  list(efa      = efa,
       clusters = table(df$cluster),
       sil      = mean(silhouette(as.integer(df$cluster), dist(z))[, "sil_width"]),
       design   = des)
}, otherwise = NULL, quiet = TRUE)

###############################################################################
##  2 · Reusable runner for **one** policy variable ---------------------------
###############################################################################
run_policy <- function(policy_var,
                       plot_title    = policy_var,
                       subtitle_text = "CRONOS Wave 1 · country-level risk-profile segments") {
  
  message("\n——  Running models for ", policy_var, "  ——")
  
  ## 2·1   fit per country
  country_results <- dat_clean %>%
    group_by(cntry) %>%
    group_split() %>%
    set_names(map_chr(., ~ unique(.x$cntry))) %>%
    purrr::map(safe_analyse)
  
  ## 2·2   successes vs. errors
  success <- purrr::keep(country_results, ~ is.null(.x$error))
  failed  <- purrr::keep(country_results, ~ !is.null(.x$error))
  
  cat("Successfully analysed:", length(success),
      "\nFailed (skipped):    ", length(failed), "\n")
  
  ## 2·3   build plotting data
  plot_df <- imap_dfr(success, function(obj, ctry) {
    des <- obj$result$design
    svyby(reformulate(policy_var), ~ cluster,
          des, svymean, na.rm = TRUE) %>%
      as_tibble() %>%
      mutate(country = ctry) %>%
      select(country, cluster, mean = !!sym(policy_var)) %>%
      drop_na(mean)           # remove empty clusters
  })
  
  ## 2·4   graph
  ggplot(plot_df, aes(cluster, mean, fill = cluster)) +
    geom_col(width = .7, colour = "white") +
    facet_wrap(~ country, ncol = 4) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5,
                       oob = scales::oob_keep) +
    labs(title    = plot_title,
         subtitle = subtitle_text,
         x = NULL,
         y = "Mean support (1 = none … 5 = very high)",
         fill = "Risk profile") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}

###############################################################################
##  3 · Run once per question  -------------------------------------------------
###############################################################################
# Fossil-fuel tax
run_policy("w1gq13",
           plot_title = "Support for fossil-fuel tax  (w1gq13)")

# Subsidies for renewables
run_policy("w1gq14",
           plot_title = "Support for subsidising renewables  (w1gq14)")

# Ban least-efficient appliances
run_policy("w1gq15",
           plot_title = "Support for banning inefficient appliances  (w1gq15)")

# Willingness to pay higher prices
run_policy("w1gq16",
           plot_title = "Favour ban sale of least energy efficient household appliances to reduce climate change  (w1gq16)")


#################################################################################
###############################################################################
##  4 · Compute McDonald's ω (Omega) -----------------------------------------
###############################################################################

# Compute omega reliability on risk variables
omega_result <- omega(R, nfactors = 1)

# Print summary including McDonald's omega total
print(omega_result$omega.tot)


###############################################################################
##  5 · McDonald's ω per country ---------------------------------------------
###############################################################################

# Function to compute omega for a single country
compute_omega_per_country <- function(df, vars = risk_vars, types = c("poly", "poly", "pearson")) {
  # Check if the country has enough non-missing data
  if (nrow(df) < 30) return(NA)
  
  # Compute mixed correlation matrix
  mx <- tryCatch(
    mixed.cor(df[vars], c = types),
    error = function(e) return(NULL)
  )
  if (is.null(mx)) return(NA)
  
  R <- mx$rho
  
  # Compute omega
  omega_result <- tryCatch(
    omega(R, nfactors = 1, plot = FALSE),
    error = function(e) return(NULL)
  )
  
  if (is.null(omega_result)) return(NA)
  
  return(omega_result$omega.tot)
}

# Run for each country
omega_by_country <- dat_clean %>%
  group_by(cntry) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$cntry))) %>%
  map_dbl(~ compute_omega_per_country(.x))

# Print result
print(omega_by_country)

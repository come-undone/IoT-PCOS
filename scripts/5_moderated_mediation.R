# ============================================================================
# PCOS IoT Study - Script 5: Moderated Mediation Analysis
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Test whether age, years since diagnosis, or comorbidity count
#          moderate the IoT -> [mediator] -> PCOSQOL pathway.
# Method:  lavaan::sem(), MLR estimator, FIML for missing data
#          (Hayes, 2022; Field, 2026 Ch.10)
#
# Model structure (moderated mediation at path a):
#   M = a1*X + a2*W + a3*(X*W) + covariates
#   Y = b*M  + c1*X + c2*W + c3*(X*W) + covariates
#   X = iot_c, M = mediator, W = moderator, Y = pcosqol_total
#
# 6 mediators x 3 moderators = 18 primary models
# Sensitivity: 6 mediators x 4 covariate conditions = 24 models
#
# Input:  data-processed/cleaned_data.rds (from Script 1)
# Run order: Script 1 -> 2 -> 3 -> 4 -> THIS SCRIPT
# ============================================================================

library(tidyverse)
library(lavaan)
library(parameters)


# ----------------------------------------------------------------------------
# 1. Load data and create centred variables
# ----------------------------------------------------------------------------
# All variables mean-centred before creating interaction terms to avoid
# collinearity between main effects and interaction terms.

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

df <- df %>%
  mutate(
    MVPA    = MET_moderate + MET_vigorous,
    iot_c   = iot_total         - mean(iot_total,          na.rm = TRUE),
    age_c   = age               - mean(age,                na.rm = TRUE),
    years_c = years_diagnosed   - mean(years_diagnosed,    na.rm = TRUE),
    comor_c = comorbidity_count - mean(comorbidity_count,  na.rm = TRUE),
    iot_x_age   = iot_c * age_c,
    iot_x_years = iot_c * years_c,
    iot_x_comor = iot_c * comor_c
  )

sd_age   <- sd(df$age,               na.rm = TRUE)
sd_years <- sd(df$years_diagnosed,   na.rm = TRUE)
sd_comor <- sd(df$comorbidity_count, na.rm = TRUE)

# Centring check - all should be ~0
cat("\nCentring check:\n")
cat("  iot_c:  ", round(mean(df$iot_c,   na.rm = TRUE), 4), "\n")
cat("  age_c:  ", round(mean(df$age_c,   na.rm = TRUE), 4), "\n")
cat("  years_c:", round(mean(df$years_c, na.rm = TRUE), 4), "\n")
cat("  comor_c:", round(mean(df$comor_c, na.rm = TRUE), 4), "\n")

cat("\nModerator SDs (used for conditional indirect effects at +/- 1 SD):\n")
cat("  SD age:  ", round(sd_age, 2),
    "-> Low ~", round(mean(df$age, na.rm = TRUE) - sd_age, 0),
    "| High ~", round(mean(df$age, na.rm = TRUE) + sd_age, 0), "\n")
cat("  SD years:", round(sd_years, 2), "\n")
cat("  SD comor:", round(sd_comor, 2), "\n\n")


# ----------------------------------------------------------------------------
# 2. Baseline mediation - EDE-Q7, no moderator
# ----------------------------------------------------------------------------
# Re-confirms core EDE-Q7 mediation using iot_c.
# Output should match Script 4 Section 3c exactly.

cat("--- Baseline mediation: EDE-Q7 (no moderator) ---\n")

model_baseline <- '
  ede_q7_global ~ a*iot_c + age + years_diagnosed + comorbidity_count
  pcosqol_total ~ b*ede_q7_global + c*iot_c + age + years_diagnosed + comorbidity_count
  indirect := a * b
  total    := c + (a * b)
'

fit_baseline <- lavaan::sem(model_baseline, data = df,
                            missing = "fiml", estimator = "MLR")
model_parameters(fit_baseline) |> display()


# ----------------------------------------------------------------------------
# 3. Sensitivity analyses - 6 mediators x 4 covariate conditions
# ----------------------------------------------------------------------------
# Tests whether mediation findings hold across different covariate
# specifications. Results reported in supplementary material.

cat("\n--- Sensitivity analyses (6 mediators x 4 covariate conditions) ---\n")
cat("Note: Supplementary material only.\n\n")

run_sensitivity <- function(outcome, mediator, predictor, covariate = NULL,
                            data) {
  cov_str <- if (!is.null(covariate)) paste("+", covariate) else ""
  model <- paste0(
    outcome,  " ~ c*", predictor, " + b*", mediator, cov_str, "\n",
    mediator, " ~ a*", predictor, cov_str, "\n",
    "indirect := a * b\n",
    "total    := c + (a * b)\n"
  )
  lavaan::sem(model, data = data, missing = "fiml", estimator = "MLR")
}

mediators <- list(
  EDE_Q7    = "ede_q7_global",
  SWEMWBS   = "swemwbs_total",
  MET_total = "MET_total_log",
  MVPA      = "MVPA",
  Walk      = "MET_walk",
  Sitting   = "sitting_hours"
)

covariates <- c(None = NA, Age = "age",
                Years = "years_diagnosed", Comorbidities = "comorbidity_count")

for (med_name in names(mediators)) {
  med_var <- mediators[[med_name]]
  cat("\nMEDIATOR:", med_name, "\n")
  for (cov_name in names(covariates)) {
    cov_var <- if (is.na(covariates[cov_name])) NULL else covariates[cov_name]
    cat("  Covariate:", cov_name, "\n")
    fit <- tryCatch(
      run_sensitivity("pcosqol_total", med_var, "iot_total", cov_var, df),
      error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
    )
    if (!is.null(fit)) model_parameters(fit) |> display() |> print()
  }
}


# ----------------------------------------------------------------------------
# 4. Moderated mediation - 18 models (6 mediators x 3 moderators)
# ----------------------------------------------------------------------------
# Key test: a3 - does the moderator change path a (IoT -> mediator)?
# Conditional indirect effects at -1 SD, mean, +1 SD of each moderator.
#
# COLLINEARITY NOTE: When a variable serves as the moderator, it is excluded
# from the covariate list (extra_covs) to avoid perfect collinearity between
# the centred moderator and its raw version. The remaining two demographic
# variables are retained as covariates in all models.

run_modmed <- function(mediator, moderator, interaction, sd_mod, data,
                       extra_covs) {
  model <- paste0('
    ', mediator, ' ~ a1*iot_c + a2*', moderator, ' + a3*', interaction,
    extra_covs, '

    pcosqol_total ~ b*', mediator, ' + c1*iot_c + c2*', moderator,
    ' + c3*', interaction, extra_covs, '

    indirect_mean  := a1 * b
    indirect_low   := (a1 + a3*(-', sd_mod, ')) * b
    indirect_high  := (a1 + a3*(+', sd_mod, ')) * b
    total          := c1 + (a1 * b)
  ')
  lavaan::sem(model, data = data, missing = "fiml", estimator = "MLR")
}

combinations <- list(
  # EDE-Q7
  list(med="ede_q7_global", mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="EDE-Q7 x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="ede_q7_global", mod="years_c", int="iot_x_years", sd=sd_years,
       label="EDE-Q7 x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="ede_q7_global", mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="EDE-Q7 x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # ISI
  list(med="isi_total",     mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="ISI x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="isi_total",     mod="years_c", int="iot_x_years", sd=sd_years,
       label="ISI x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="isi_total",     mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="ISI x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # SWEMWBS
  list(med="swemwbs_total", mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="SWEMWBS x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="swemwbs_total", mod="years_c", int="iot_x_years", sd=sd_years,
       label="SWEMWBS x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="swemwbs_total", mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="SWEMWBS x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # MET total (log)
  list(med="MET_total_log", mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="MET total x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="MET_total_log", mod="years_c", int="iot_x_years", sd=sd_years,
       label="MET total x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="MET_total_log", mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="MET total x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # MVPA
  list(med="MVPA",          mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="MVPA x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="MVPA",          mod="years_c", int="iot_x_years", sd=sd_years,
       label="MVPA x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="MVPA",          mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="MVPA x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # Walking
  list(med="MET_walk",      mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="Walking x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="MET_walk",      mod="years_c", int="iot_x_years", sd=sd_years,
       label="Walking x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="MET_walk",      mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="Walking x Comorbidities",
       extra_covs="+ age + years_diagnosed")
)

cat("\n--- Moderated mediation (18 models) ---\n")

for (combo in combinations) {
  cat("\nMODEL:", combo$label, "\n")
  fit <- tryCatch(
    run_modmed(combo$med, combo$mod, combo$int, combo$sd, df,
               extra_covs = combo$extra_covs),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit)) model_parameters(fit) |> display() |> print()
}

cat("\nScript 5 complete.\n")

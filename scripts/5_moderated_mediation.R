# ============================================================================
# PCOS IoT Study - Script 5: Moderated Mediation Analysis
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Test whether age, years since diagnosis, or comorbidity count
#          moderate the IoT -> [mediator] -> PCOSQOL pathway.
# Method:  lavaan::sem(), MLR estimator, FIML for missing data
#          (Hayes, 2018; Field, 2026 Ch.10)
#
# Model structure (Hayes Model 8: moderation of path a and the direct path):
#   M = a1*X + a2*W + a3*(X*W) + covariates
#   Y = b*M  + c1*X + c2*W + c3*(X*W) + covariates
#   X = iot_c, M = mediator, W = moderator, Y = pcosqol_total
#
# 7 mediators x 3 moderators = 21 primary models
# Sensitivity: 7 mediators x 4 covariate conditions = 28 models
#
# Input:  data-processed/cleaned_data.rds (from Script 1)
# Output: results/Script5_conditional_indirect_age.csv  (Figure 5)
#         results/Script5_moderation_a3.csv             (Figure 5 legend)
#
# Run order: Script 1 -> 2 -> 3 -> 4 -> THIS SCRIPT
# ============================================================================

library(tidyverse)
library(lavaan)
library(parameters)
library(semTools)


# ----------------------------------------------------------------------------
# Monte Carlo CIs for indirect effects
# ----------------------------------------------------------------------------
# Same helper as Script 4, repeated here so this script runs standalone.
# lavaan's default CI for a := parameter is the delta method, which assumes
# a*b is normally distributed. The product of two normals is skewed, so that
# assumption does not hold. Monte Carlo CIs draw from the estimated parameter
# distribution and take the empirical percentiles instead (Preacher & Selig,
# 2012). 100,000 draws; seeded so the CIs are reproducible.

MC_REPS <- 100000
MC_SEED <- 20240607

mc_ci <- function(fit, reps = MC_REPS, seed = MC_SEED) {
  if (is.null(fit)) return(invisible(NULL))
  set.seed(seed)
  out <- tryCatch(
    semTools::monteCarloCI(fit, nRep = reps),
    error = function(e) {
      cat("  [Monte Carlo CI not computed:", conditionMessage(e), "]\n")
      NULL
    }
  )
  if (is.null(out)) return(invisible(NULL))
  cat("\nMonte Carlo 95% CI (", format(reps, big.mark = ",", scientific = FALSE), " draws):\n", sep = "")
  out_print <- round(out, 4)
  class(out_print) <- "data.frame"
  print(out_print)
  invisible(out)
}

# ----------------------------------------------------------------------------
# 1. Load data and create centred variables
# ----------------------------------------------------------------------------
# All variables mean-centred before creating interaction terms to avoid
# collinearity between main effects and interaction terms.

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

df <- df %>%
  mutate(
    # PA sub-components in hundreds of MET-min/week, as in Script 4
    MVPA_h  = (MET_moderate + MET_vigorous) / 100,
    walk_h  = MET_walk / 100,
    iot_c   = iot_total         - mean(iot_total,          na.rm = TRUE),
    age_c   = age               - mean(age,                na.rm = TRUE),
    years_c = years_diagnosed   - mean(years_diagnosed,    na.rm = TRUE),
    comor_c = comorbidity_count - mean(comorbidity_count,  na.rm = TRUE),
    iot_x_age   = iot_c * age_c,
    iot_x_years = iot_c * years_c,
    iot_x_comor = iot_c * comor_c
  )

mean_age <- mean(df$age,               na.rm = TRUE)
sd_age   <- sd(df$age,                 na.rm = TRUE)
sd_years <- sd(df$years_diagnosed,     na.rm = TRUE)
sd_comor <- sd(df$comorbidity_count,   na.rm = TRUE)

# Centring check - all should be ~0
cat("\nCentring check:\n")
cat("  iot_c:  ", round(mean(df$iot_c,   na.rm = TRUE), 4), "\n")
cat("  age_c:  ", round(mean(df$age_c,   na.rm = TRUE), 4), "\n")
cat("  years_c:", round(mean(df$years_c, na.rm = TRUE), 4), "\n")
cat("  comor_c:", round(mean(df$comor_c, na.rm = TRUE), 4), "\n")

cat("\nModerator SDs (used for conditional indirect effects at +/- 1 SD):\n")
cat("  SD age:  ", round(sd_age, 2),
    "-> Low ~", round(mean_age - sd_age, 0),
    "| High ~", round(mean_age + sd_age, 0), "\n")
cat("  SD years:", round(sd_years, 2), "\n")
cat("  SD comor:", round(sd_comor, 2), "\n\n")


# ----------------------------------------------------------------------------
# 2. Baseline mediation - EDE-Q7, no moderator
# ----------------------------------------------------------------------------
# Re-confirms core EDE-Q7 mediation using iot_c.
# Estimates match Script 4 Section 3c; 
# Monte Carlo limits may differ in the third decimal 
# (parameters are listed in a different order).

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
mc_ci(fit_baseline)


# ----------------------------------------------------------------------------
# 3. Sensitivity analyses - 7 mediators x 4 covariate conditions
# ----------------------------------------------------------------------------
# Tests whether mediation findings hold across different covariate
# specifications. Results reported in supplementary material.

cat("\n--- Sensitivity analyses (7 mediators x 4 covariate conditions) ---\n")
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
  ISI       = "isi_total",
  SWEMWBS   = "swemwbs_total",
  MET_total = "MET_total_log",
  MVPA      = "MVPA_h",
  Walk      = "walk_h",
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
    if (!is.null(fit)) {
      model_parameters(fit) |> display() |> print()
      mc_ci(fit)
    }
  }
}


# ----------------------------------------------------------------------------
# 4. Moderated mediation - 21 models (7 mediators x 3 moderators)
# ----------------------------------------------------------------------------
# Key test: a3 - does the moderator change path a (IoT -> mediator)?
# Conditional indirect effects at -1 SD, mean, +1 SD of each moderator.
#
# COLLINEARITY NOTE: When a variable serves as the moderator, it is excluded
# from the covariate list (extra_covs) to avoid perfect collinearity between
# the centred moderator and its raw version. The remaining two covariates
# are retained in all models.

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
    index_modmed   := a3 * b
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
  list(med="MVPA_h",          mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="MVPA x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="MVPA_h",          mod="years_c", int="iot_x_years", sd=sd_years,
       label="MVPA x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="MVPA_h",          mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="MVPA x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # Walking
  list(med="walk_h",      mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="Walking x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="walk_h",      mod="years_c", int="iot_x_years", sd=sd_years,
       label="Walking x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="walk_h",      mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="Walking x Comorbidities",
       extra_covs="+ age + years_diagnosed"),
  # Sitting time (hours/weekday)
  list(med="sitting_hours", mod="age_c",   int="iot_x_age",   sd=sd_age,
       label="Sitting x Age",
       extra_covs="+ years_diagnosed + comorbidity_count"),
  list(med="sitting_hours", mod="years_c", int="iot_x_years", sd=sd_years,
       label="Sitting x Years diagnosed",
       extra_covs="+ age + comorbidity_count"),
  list(med="sitting_hours", mod="comor_c", int="iot_x_comor", sd=sd_comor,
       label="Sitting x Comorbidities",
       extra_covs="+ age + years_diagnosed")
)

cat("\n--- Moderated mediation (21 models) ---\n")

fit_ede_age <- NULL   # kept for the stability check and export below

for (combo in combinations) {
  cat("\nMODEL:", combo$label, "\n")
  fit <- tryCatch(
    run_modmed(combo$med, combo$mod, combo$int, combo$sd, df,
               extra_covs = combo$extra_covs),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit)) {
    model_parameters(fit) |> display() |> print()
    mc_ci(fit)
  }
  if (combo$label == "EDE-Q7 x Age") fit_ede_age <- fit
}

# ----------------------------------------------------------------------------
# 5. Monte Carlo stability check - primary moderated mediation model
# ----------------------------------------------------------------------------
# The three conditional indirect effects reported in the manuscript come from
# the EDE-Q7 x Age model. Repeating the draw under ten seeds confirms that the
# interval bounds are a property of the data and not of the simulation.

cat("\n\n--- Monte Carlo stability check: EDE-Q7 x Age (10 seeds) ---\n")

for (s in 1:10) {
  set.seed(s)
  o <- semTools::monteCarloCI(fit_ede_age, nRep = MC_REPS)
  cat(sprintf("  seed %2d |  low %7.4f [%7.4f, %7.4f]  mean %7.4f [%7.4f, %7.4f]  high %7.4f [%7.4f, %7.4f]\n",
              s,
              o["indirect_low",  "est"], o["indirect_low",  "ci.lower"], o["indirect_low",  "ci.upper"],
              o["indirect_mean", "est"], o["indirect_mean", "ci.lower"], o["indirect_mean", "ci.upper"],
              o["indirect_high", "est"], o["indirect_high", "ci.lower"], o["indirect_high", "ci.upper"]))
}


# ----------------------------------------------------------------------------
# 6. Export for Script 7
# ----------------------------------------------------------------------------
# Figure 5 previously took its values from the console output above. Writing
# them here means the figure is rebuilt from the fitted model rather than from
# transcription. monteCarloCI is re-run under the same seed as Section 4, so
# the values are identical to those already printed.

dir.create("results", showWarnings = FALSE)

cat("\n\n--- Export ---\n")

stopifnot(!is.null(fit_ede_age))

set.seed(MC_SEED)
mc_age <- semTools::monteCarloCI(fit_ede_age, nRep = MC_REPS)

conditional_age <- data.frame(
  age_level = c("low", "mean", "high"),
  age_value = round(c(mean_age - sd_age, mean_age, mean_age + sd_age), 0),
  B         = round(mc_age[c("indirect_low", "indirect_mean", "indirect_high"), "est"],      4),
  ci_lo     = round(mc_age[c("indirect_low", "indirect_mean", "indirect_high"), "ci.lower"], 4),
  ci_hi     = round(mc_age[c("indirect_low", "indirect_mean", "indirect_high"), "ci.upper"], 4),
  row.names = NULL
)

# An interval is treated as excluding zero when both bounds share a sign.
conditional_age$sig <- with(conditional_age, ci_hi < 0 | ci_lo > 0)

print(conditional_age)
write_csv(conditional_age, "results/Script5_conditional_indirect_age.csv")
cat("Saved: results/Script5_conditional_indirect_age.csv\n")

# a3 (age x use on path a), c3 (age x use on the direct path) and the index
# of moderated mediation, quoted in the Results text and the Figure 5 legend
pe_age <- lavaan::parameterEstimates(fit_ede_age)
a3_row <- pe_age[pe_age$label == "a3", ][1, ]
c3_row <- pe_age[pe_age$label == "c3", ][1, ]

moderation_a3 <- data.frame(
  parameter = "a3: IoT x age -> EDE-Q7",
  B         = round(a3_row$est,    6),
  SE        = round(a3_row$se,     6),
  z         = round(a3_row$z,      4),
  p         = round(a3_row$pvalue, 4),
  c3_B      = round(c3_row$est,    4),
  c3_SE     = round(c3_row$se,     4),
  c3_p      = round(c3_row$pvalue, 4),
  index_B   = round(mc_age["index_modmed", "est"],      4),
  index_lo  = round(mc_age["index_modmed", "ci.lower"], 4),
  index_hi  = round(mc_age["index_modmed", "ci.upper"], 4),
  sd_age    = round(sd_age,        4),
  mean_age  = round(mean_age,      4)
)

print(moderation_a3)
write_csv(moderation_a3, "results/Script5_moderation_a3.csv")
cat("Saved: results/Script5_moderation_a3.csv\n")


cat("\nScript 5 complete.\n")


fit_tmp <- run_modmed("swemwbs_total", "comor_c", "iot_x_comor", sd_comor, df,
                      extra_covs = "+ age + years_diagnosed")
set.seed(20240607)
x <- semTools::monteCarloCI(fit_tmp, nRep = 100000)
class(x) <- "data.frame"
print(x["indirect_high", ], digits = 8)
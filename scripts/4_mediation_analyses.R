# ============================================================================
# PCOS IoT Study - Script 4: Mediation Analysis
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Test whether health behaviours mediate the IoT -> PCOSQOL pathway.
# Method:  lavaan::sem(), MLR estimator, FIML for missing data
#          (Field, 2026 Ch.10; Hayes, 2022)
#
# Input:   data-processed/cleaned_data.rds (from Script 1)
# Output:  results/mediation_results.csv
#
# Run order: Script 1 -> 2 -> 3 -> THIS SCRIPT -> Script 5
# ============================================================================

library(tidyverse)
library(lavaan)
library(parameters)


# ----------------------------------------------------------------------------
# 1. Load data and centre predictors
# ----------------------------------------------------------------------------
# Mean-centring throughout for consistency with Script 5.
# B coefficients are per 1 IoT point, not per 1 SD.

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

df <- df %>%
  mutate(
    MVPA          = MET_moderate + MET_vigorous,
    iot_c         = iot_total      - mean(iot_total,      na.rm = TRUE),
    iot_engage_c  = iot_engagement - mean(iot_engagement, na.rm = TRUE),
    iot_selfmon_c = iot_selfmon    - mean(iot_selfmon,    na.rm = TRUE)
  )

# Centring check - all should be ~0
cat("\nCentring check:\n")
cat("  iot_c:        ", round(mean(df$iot_c,         na.rm = TRUE), 4), "\n")
cat("  iot_engage_c: ", round(mean(df$iot_engage_c,  na.rm = TRUE), 4), "\n")
cat("  iot_selfmon_c:", round(mean(df$iot_selfmon_c, na.rm = TRUE), 4), "\n\n")


# ----------------------------------------------------------------------------
# 2. Total effect - IoT -> PCOSQOL (no mediator)
# ----------------------------------------------------------------------------
# c_total must be significant before introducing any mediator.

cat("--- Total effect: IoT -> PCOSQOL ---\n")

model_total <- '
  pcosqol_total ~ c_total*iot_c + age + years_diagnosed + comorbidity_count
'

fit_total <- lavaan::sem(model_total, data = df, missing = "fiml",
                         estimator = "MLR")
model_parameters(fit_total) |> display()


# ----------------------------------------------------------------------------
# 3. Single mediator models
# ----------------------------------------------------------------------------
# Path a:   IoT -> Mediator
# Path b:   Mediator -> PCOSQOL (controlling for IoT)
# c:        Direct effect of IoT -> PCOSQOL after mediator
# indirect: a x b (the mediated pathway - key test)
# Covariates included in both equations.

run_mediation <- function(predictor, mediator, outcome, data, label) {
  cat("\n---", label, "---\n")
  model <- paste0(
    outcome,  " ~ c*", predictor, " + b*", mediator,
    " + age + years_diagnosed + comorbidity_count\n",
    mediator, " ~ a*", predictor,
    " + age + years_diagnosed + comorbidity_count\n",
    "indirect := a * b\n",
    "total    := c + (a * b)\n"
  )
  fit <- tryCatch(
    lavaan::sem(model, data = data, missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit)) model_parameters(fit) |> display() |> print()
}

# Model 3a: Physical activity (log-transformed - right-skewed)
run_mediation("iot_c", "MET_total_log", "pcosqol_total", df,
              "IoT -> PA (MET log) -> PCOSQOL")

# Model 3b: Sleep quality (ISI - higher = worse insomnia)
run_mediation("iot_c", "isi_total", "pcosqol_total", df,
              "IoT -> Sleep (ISI) -> PCOSQOL")

# Model 3c: Eating behaviours (EDE-Q7) *** KEY MODEL ***
run_mediation("iot_c", "ede_q7_global", "pcosqol_total", df,
              "IoT -> Eating (EDE-Q7) -> PCOSQOL")

# Model 3d: Mental wellbeing (SWEMWBS - higher = better wellbeing)
run_mediation("iot_c", "swemwbs_total", "pcosqol_total", df,
              "IoT -> Wellbeing (SWEMWBS) -> PCOSQOL")


# ----------------------------------------------------------------------------
# 4. Sensitivity: raw MET (untransformed)
# ----------------------------------------------------------------------------

run_mediation("iot_c", "MET_total", "pcosqol_total", df,
              "IoT -> PA (MET raw) -> PCOSQOL [sensitivity]")


# ----------------------------------------------------------------------------
# 5. Sensitivity: PA sub-components as mediators
# ----------------------------------------------------------------------------
# MVPA = moderate + vigorous MET
# sitting_hours = sedentary behaviour (higher = more sitting)

run_mediation("iot_c", "MVPA",         "pcosqol_total", df,
              "IoT -> MVPA -> PCOSQOL")
run_mediation("iot_c", "MET_walk",     "pcosqol_total", df,
              "IoT -> Walking -> PCOSQOL")
run_mediation("iot_c", "sitting_hours","pcosqol_total", df,
              "IoT -> Sitting -> PCOSQOL")


# ----------------------------------------------------------------------------
# 6. Subscale analysis: Engagement vs Self-Monitoring as predictors
# ----------------------------------------------------------------------------
# Engagement (items 1-5): browsing, forums, social media, sharing, AI
# Self-Monitoring (items 6-10): apps, trackers, sleep/nutrition/PA monitors

cat("\n\n--- Engagement subscale as predictor ---\n")
run_mediation("iot_engage_c", "MET_total_log", "pcosqol_total", df,
              "Engagement -> PA (MET log) -> PCOSQOL")
run_mediation("iot_engage_c", "isi_total",     "pcosqol_total", df,
              "Engagement -> Sleep (ISI) -> PCOSQOL")
run_mediation("iot_engage_c", "ede_q7_global", "pcosqol_total", df,
              "Engagement -> Eating (EDE-Q7) -> PCOSQOL")
run_mediation("iot_engage_c", "swemwbs_total", "pcosqol_total", df,
              "Engagement -> Wellbeing (SWEMWBS) -> PCOSQOL")

cat("\n\n--- Self-Monitoring subscale as predictor ---\n")
run_mediation("iot_selfmon_c", "MET_total_log", "pcosqol_total", df,
              "Self-Monitoring -> PA (MET log) -> PCOSQOL")
run_mediation("iot_selfmon_c", "isi_total",     "pcosqol_total", df,
              "Self-Monitoring -> Sleep (ISI) -> PCOSQOL")
run_mediation("iot_selfmon_c", "ede_q7_global", "pcosqol_total", df,
              "Self-Monitoring -> Eating (EDE-Q7) -> PCOSQOL")
run_mediation("iot_selfmon_c", "swemwbs_total", "pcosqol_total", df,
              "Self-Monitoring -> Wellbeing (SWEMWBS) -> PCOSQOL")


# ----------------------------------------------------------------------------
# 7. Multiple mediator model - all 4 primary mediators simultaneously
# ----------------------------------------------------------------------------
# Tests the unique indirect effect of each pathway when all mediators
# compete. EDE-Q7 remaining significant here is the key robustness check.
# Covariances between mediators are freely estimated (~~).

cat("\n\n--- Multiple mediator model (all 4 primary mediators) ---\n")

model_multi <- '
  pcosqol_total ~ c*iot_c +
                  b1*ede_q7_global +
                  b2*swemwbs_total +
                  b3*isi_total +
                  b4*MET_total_log +
                  age + years_diagnosed + comorbidity_count

  ede_q7_global ~ a1*iot_c + age + years_diagnosed + comorbidity_count
  swemwbs_total ~ a2*iot_c + age + years_diagnosed + comorbidity_count
  isi_total     ~ a3*iot_c + age + years_diagnosed + comorbidity_count
  MET_total_log ~ a4*iot_c + age + years_diagnosed + comorbidity_count

  ede_q7_global ~~ swemwbs_total
  ede_q7_global ~~ isi_total
  ede_q7_global ~~ MET_total_log
  swemwbs_total ~~ isi_total
  swemwbs_total ~~ MET_total_log
  isi_total     ~~ MET_total_log

  indirect_eating    := a1 * b1
  indirect_wellbeing := a2 * b2
  indirect_sleep     := a3 * b3
  indirect_PA        := a4 * b4
  total_indirect     := (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4)
  total              := c + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4)
'

fit_multi <- lavaan::sem(model_multi, data = df, missing = "fiml",
                         estimator = "MLR")

model_parameters(fit_multi) |> display()

cat("\nModel fit indices:\n")
lavaan::fitmeasures(fit_multi,
                    fit.measures = c("cfi", "rmsea", "srmr", "tli")) %>%
  round(3) %>%
  print()

cat("\nScript 4 complete. Next: Script5_ModeratedMediation_FINAL.R\n")

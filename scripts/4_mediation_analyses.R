# ============================================================================
# PCOS IoT Study - Script 4: Mediation Analysis
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Test whether disordered eating, insomnia, mental wellbeing and
#          physical activity mediate the association between digital health
#          technology use and PCOSQOL (Figures 3-4; Supplemental Material 4).
#          iot_engagement = information-seeking subscale (items 1-5)
#          iot_selfmon    = self-monitoring subscale (items 6-10)
# Method:  lavaan::sem(), MLR estimator, FIML for missing data
#          (Field, 2026, Ch. 10; Hayes, 2018)
#
# Input:   data-processed/cleaned_data.rds (from Script 1)
# Output:  results/Script4_indirect_effects.csv   (Figure 4)
#          results/Script4_primary_paths.csv      (Figure 8, manuscript Fig 3)
#
# Run order: Script 1 -> 2 -> 3 -> THIS SCRIPT -> Script 5
# ============================================================================

library(tidyverse)
library(lavaan)
library(parameters)
library(semTools)

# ----------------------------------------------------------------------------
# Monte Carlo CIs for indirect effects
# ----------------------------------------------------------------------------
# lavaan's default CI for a := parameter is the delta method, which assumes
# a*b is normally distributed. The product of two normals is skewed, so that
# assumption does not hold. Monte Carlo CIs draw from the estimated parameter
# distribution and take the empirical percentiles instead (Preacher & Selig,
# 2012). 100,000 draws; seeded so the CIs are reproducible.
# If a model returns no usable standard errors, the CI is skipped rather than
# allowed to stop the run.

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
# 1. Load data and centre predictors
# ----------------------------------------------------------------------------
# Mean-centring throughout for consistency with Script 5.
# B coefficients are per 1 IoT point, not per 1 SD.

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

df <- df %>%
  mutate(
    iot_c         = iot_total      - mean(iot_total,      na.rm = TRUE),
    iot_engage_c  = iot_engagement - mean(iot_engagement, na.rm = TRUE),
    iot_selfmon_c = iot_selfmon    - mean(iot_selfmon,    na.rm = TRUE),
    # Untransformed physical activity in hundreds of MET-min/week, so that its
    # variance is on a similar scale to the other model variables
    MET_total_h = MET_total / 100,
    MVPA_h      = (MET_moderate + MET_vigorous) / 100,
    walk_h      = MET_walk / 100
  )

# Centring check - all should be ~0
cat("\nCentring check:\n")
cat("  iot_c:        ", round(mean(df$iot_c,         na.rm = TRUE), 4), "\n")
cat("  iot_engage_c: ", round(mean(df$iot_engage_c,  na.rm = TRUE), 4), "\n")
cat("  iot_selfmon_c:", round(mean(df$iot_selfmon_c, na.rm = TRUE), 4), "\n\n")


# ----------------------------------------------------------------------------
# 2. Total effect - IoT -> PCOSQOL (no mediator)
# ----------------------------------------------------------------------------
# Total effect (c), reported for reference.

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
  if (!is.null(fit)) {
    model_parameters(fit) |> display() |> print()
    mc_ci(fit)
  }
  invisible(fit)
}

# Model 3a: Physical activity (log-transformed - right-skewed)
fit_met_total <- run_mediation("iot_c", "MET_total_log", "pcosqol_total", df,
                               "IoT -> PA (MET log) -> PCOSQOL")

# Model 3b: Sleep quality (ISI - higher = worse insomnia)
fit_isi_total <- run_mediation("iot_c", "isi_total", "pcosqol_total", df,
                               "IoT -> Sleep (ISI) -> PCOSQOL")

# Model 3c: Disordered eating (EDE-Q7) - primary model
fit_ede_total <- run_mediation("iot_c", "ede_q7_global", "pcosqol_total", df,
                               "IoT -> Eating (EDE-Q7) -> PCOSQOL")

# Model 3d: Mental wellbeing (SWEMWBS - higher = better wellbeing)
fit_swemwbs_total <- run_mediation("iot_c", "swemwbs_total", "pcosqol_total", df,
                                   "IoT -> Wellbeing (SWEMWBS) -> PCOSQOL")


# ----------------------------------------------------------------------------
# 4. Sensitivity: untransformed total physical activity (100 MET-min/week)
# ----------------------------------------------------------------------------

fit_met_untransformed <- run_mediation("iot_c", "MET_total_h", "pcosqol_total", df,
                                       "IoT -> PA (MET, untransformed) -> PCOSQOL [sensitivity]")


# ----------------------------------------------------------------------------
# 5. Sensitivity: PA sub-components as mediators
# ----------------------------------------------------------------------------
# MVPA = moderate + vigorous MET
# sitting_hours = sedentary behaviour (higher = more sitting)

run_mediation("iot_c", "MVPA_h",       "pcosqol_total", df,
              "IoT -> MVPA -> PCOSQOL")
run_mediation("iot_c", "walk_h",       "pcosqol_total", df,
              "IoT -> Walking -> PCOSQOL")
run_mediation("iot_c", "sitting_hours","pcosqol_total", df,
              "IoT -> Sitting -> PCOSQOL")

# ----------------------------------------------------------------------------
# 5b. Sensitivity: covariate counting all reported conditions (incl. free text)
# ----------------------------------------------------------------------------
# Primary analyses adjust for the 18 listed conditions (comorbidity_count).
# Here the primary EDE-Q7 models are re-run with comorbidity_count_all.

model_allcond <- '
  pcosqol_total ~ c*iot_c + b*ede_q7_global + age + years_diagnosed + comorbidity_count_all
  ede_q7_global ~ a*iot_c + age + years_diagnosed + comorbidity_count_all
  indirect := a * b
  total    := c + (a * b)
'

cat("\n--- Sensitivity: all reported conditions, total score ---\n")
fit_allcond <- lavaan::sem(model_allcond, data = df, missing = "fiml",
                           estimator = "MLR")
model_parameters(fit_allcond) |> display() |> print()
mc_ci(fit_allcond)

cat("\n--- Sensitivity: all reported conditions, information-seeking subscale ---\n")
model_allcond_eng <- gsub("iot_c", "iot_engage_c", model_allcond, fixed = TRUE)
fit_allcond_eng <- lavaan::sem(model_allcond_eng, data = df, missing = "fiml",
                               estimator = "MLR")
model_parameters(fit_allcond_eng) |> display() |> print()
mc_ci(fit_allcond_eng)

# ----------------------------------------------------------------------------
# 6. Subscale analysis: Engagement vs Self-Monitoring as predictors
# ----------------------------------------------------------------------------
# Single-mediator models with each subscale as the predictor.

cat("\n\n--- Engagement subscale as predictor ---\n")
run_mediation("iot_engage_c", "MET_total_log", "pcosqol_total", df,
              "Engagement -> PA (MET log) -> PCOSQOL")
run_mediation("iot_engage_c", "isi_total",     "pcosqol_total", df,
              "Engagement -> Sleep (ISI) -> PCOSQOL")
fit_eng_ede <- run_mediation("iot_engage_c", "ede_q7_global",
                             "pcosqol_total", df,
                             "Engagement -> Eating (EDE-Q7) -> PCOSQOL")
fit_eng_swemwbs <- run_mediation("iot_engage_c", "swemwbs_total", 
                                 "pcosqol_total", df, 
                                 "Engagement -> Wellbeing (SWEMWBS) -> PCOSQOL")

cat("\n\n--- Self-Monitoring subscale as predictor ---\n")
run_mediation("iot_selfmon_c", "MET_total_log", "pcosqol_total", df,
              "Self-Monitoring -> PA (MET log) -> PCOSQOL")
run_mediation("iot_selfmon_c", "isi_total",     "pcosqol_total", df,
              "Self-Monitoring -> Sleep (ISI) -> PCOSQOL")
fit_sm_ede <- run_mediation("iot_selfmon_c", "ede_q7_global", 
                            "pcosqol_total", df,
                            "Self-Monitoring -> Eating (EDE-Q7) -> PCOSQOL")
run_mediation("iot_selfmon_c", "swemwbs_total", "pcosqol_total", df,
              "Self-Monitoring -> Wellbeing (SWEMWBS) -> PCOSQOL")


# ----------------------------------------------------------------------------
# 7. Multiple mediator model - all 4 primary mediators simultaneously
# ----------------------------------------------------------------------------
# Unique indirect effect of each mediator when all four are modelled together.

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
mc_ci(fit_multi)

# The model is saturated (df = 0), so fit indices are not informative
# (Field, 2026, p. 581). The degrees of freedom are printed as a check.
cat("\nModel degrees of freedom:", lavaan::fitmeasures(fit_multi, "df"), "\n")

# ----------------------------------------------------------------------------
# 8. Monte Carlo stability check
# ----------------------------------------------------------------------------
# Two indirect effects have upper bounds close enough to zero that simulation
# noise could move them across it. Repeating the draw under ten seeds shows
# whether the bound is a property of the data or an artefact of the sampling.

mc_stability <- function(fit, label, seeds = 1:10, reps = MC_REPS) {
  cat("\n---", label, "---\n")
  for (s in seeds) {
    set.seed(s)
    out <- semTools::monteCarloCI(fit, nRep = reps)
    cat(sprintf("  seed %2d:  est %8.4f   95%% CI [%8.4f, %8.4f]\n",
                s, out["indirect", "est"],
                out["indirect", "ci.lower"], out["indirect", "ci.upper"]))
  }
}

cat("\n\n--- Monte Carlo stability check (10 seeds, 100,000 draws) ---\n")
mc_stability(fit_eng_swemwbs, "Engagement -> SWEMWBS -> PCOSQOL")
mc_stability(fit_sm_ede,      "Self-Monitoring -> EDE-Q7 -> PCOSQOL")


# ----------------------------------------------------------------------------
# 9. Multiple mediator model - subscales as predictors
# ----------------------------------------------------------------------------
# Mirrors Section 7 with each subscale in place of IoT total. Tests whether
# EDE-Q7 and SWEMWBS each carry unique variance when they compete directly.

cat("\n\n--- Multiple mediator model (Engagement subscale) ---\n")
model_multi_eng <- gsub("iot_c", "iot_engage_c", model_multi, fixed = TRUE)
fit_multi_eng <- lavaan::sem(model_multi_eng, data = df, missing = "fiml",
                             estimator = "MLR")
model_parameters(fit_multi_eng) |> display()
mc_ci(fit_multi_eng)

cat("\n\n--- Multiple mediator model (Self-Monitoring subscale) ---\n")
model_multi_sm <- gsub("iot_c", "iot_selfmon_c", model_multi, fixed = TRUE)
fit_multi_sm <- lavaan::sem(model_multi_sm, data = df, missing = "fiml",
                            estimator = "MLR")
model_parameters(fit_multi_sm) |> display()
mc_ci(fit_multi_sm)


# ----------------------------------------------------------------------------
# 10. Export for Script 7
# ----------------------------------------------------------------------------
# Values for Figures 3 and 4 are written to file so that Script 7 reads them
# directly; Monte Carlo CIs use the same seed as above.

dir.create("results", showWarnings = FALSE)

cat("\n\n--- Export ---\n")

# Indirect effects (Figure 4)
mc_row <- function(fit, param, model_id, predictor, mediator) {
  set.seed(MC_SEED)
  out <- semTools::monteCarloCI(fit, nRep = MC_REPS)
  data.frame(
    model_id  = model_id,
    predictor = predictor,
    mediator  = mediator,
    B         = round(unname(out[param, "est"]),      4),
    ci_lo     = round(unname(out[param, "ci.lower"]), 4),
    ci_hi     = round(unname(out[param, "ci.upper"]), 4)
  )
}

indirect_effects <- bind_rows(
  mc_row(fit_ede_total,     "indirect",        "ede_total",
         "IoT total",           "EDE-Q7"),
  mc_row(fit_eng_ede,       "indirect",        "ede_infoseek",
         "Information seeking", "EDE-Q7"),
  mc_row(fit_multi,         "indirect_eating", "ede_multi",
         "IoT total",           "EDE-Q7 (multiple mediator)"),
  mc_row(fit_eng_swemwbs,   "indirect",        "swemwbs_infoseek",
         "Information seeking", "SWEMWBS"),
  mc_row(fit_isi_total,     "indirect",        "isi_total",
         "IoT total",           "ISI"),
  mc_row(fit_swemwbs_total, "indirect",        "swemwbs_total",
         "IoT total",           "SWEMWBS"),
  mc_row(fit_met_total,     "indirect",        "met_total",
         "IoT total",           "log(MET + 1)")
)

# An interval is treated as excluding zero when both bounds share a sign.
indirect_effects$sig <- with(indirect_effects, ci_hi < 0 | ci_lo > 0)

print(indirect_effects)
write_csv(indirect_effects, "results/Script4_indirect_effects.csv")
cat("Saved: results/Script4_indirect_effects.csv\n")

# Primary model paths (Figure 8, and manuscript Figure 3)
pe_row <- function(fit, lbl, path_name) {
  pe <- lavaan::parameterEstimates(fit)
  r  <- pe[pe$label == lbl, ][1, ]
  data.frame(
    path = path_name,
    B    = round(r$est,    4),
    SE   = round(r$se,     4),
    z    = round(r$z,      4),
    p    = signif(r$pvalue, 4)
  )
}

primary_paths <- bind_rows(
  pe_row(fit_total,     "c_total",  "total effect (c)"),
  pe_row(fit_ede_total, "a",        "a: IoT -> EDE-Q7"),
  pe_row(fit_ede_total, "b",        "b: EDE-Q7 -> PCOSQOL"),
  pe_row(fit_ede_total, "c",        "c': direct effect")
)

print(primary_paths)
write_csv(primary_paths, "results/Script4_primary_paths.csv")
cat("Saved: results/Script4_primary_paths.csv\n")


cat("\nScript 4 complete. Next: 5_moderated_mediation.R\n")

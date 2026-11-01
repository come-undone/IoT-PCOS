# ============================================================================
# PCOS IoT Study - Script 9: Post Hoc Power Analysis for Moderated Mediation
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Power for the age x use interaction on path a (a3: use x age ->
#          EDE-Q7) from the age-moderated mediation in Script 5, and the
#          sample size needed for 80% power (Supplemental Material 5, 5.2).
#
# Unlike the indirect effect in Script 8, the interaction coefficient has an
# approximately normal sampling distribution, so analytic power calculation
# is appropriate (Cohen, 1988). A simulation-based check is also run to
# corroborate the analytic result.
#
# Note: power4mome v0.2.1 does not implement a dedicated moderated mediation
# power function. The approach here follows standard practice for interaction
# terms in regression (Fairchild & MacKinnon, 2009, Prevention Science).
#
# Input:   data-processed/cleaned_data.rds (from Script 1)
# Run order: Script 1 -> 5 -> THIS SCRIPT
# ============================================================================

library(dplyr)
library(lavaan)
library(pwr)


# ----------------------------------------------------------------------------
# 1. Refit moderated mediation model and extract standardised coefficients
# ----------------------------------------------------------------------------
# Reproduces the age-moderated mediation from Script 5 exactly.

df <- readRDS("data-processed/cleaned_data.rds")

df <- df %>%
  dplyr::mutate(
    iot_c     = iot_total - mean(iot_total, na.rm = TRUE),
    age_c     = age       - mean(age,       na.rm = TRUE),
    iot_x_age = iot_c * age_c
  )

# Same model as the EDE-Q7 x Age model in Script 5 (Hayes Model 8)
model_modmed_age <- '
  ede_q7_global ~ a1*iot_c + a2*age_c + a3*iot_x_age + years_diagnosed + comorbidity_count
  pcosqol_total ~ b*ede_q7_global + c1*iot_c + c2*age_c + c3*iot_x_age + years_diagnosed + comorbidity_count
'

fit_modmed_age <- lavaan::sem(model_modmed_age, data = df,
                              missing = "fiml", estimator = "MLR")

# Check against Script 5: unstandardised a3 should match Script5_moderation_a3.csv
pe_age <- lavaan::parameterEstimates(fit_modmed_age)
print(round(unlist(pe_age[pe_age$label == "a3", c("est", "se", "pvalue")]), 4))

std_all    <- lavaan::standardizedsolution(fit_modmed_age)
std_modmed <- std_all[std_all$label %in% c("a1", "a2", "a3", "b", "c1"),
                      c("label", "est.std", "se")]
print(std_modmed)

r2_ede <- lavaan::inspect(fit_modmed_age, "r2")["ede_q7_global"]
cat("R-squared for EDE-Q7 equation:", round(r2_ede, 4), "\n")


# ----------------------------------------------------------------------------
# 2. Analytic power for the interaction term (a3) using pwr
# ----------------------------------------------------------------------------
# Cohen's f-squared for a single term is the R-squared it adds (Cohen, 1988):
#   f2 = (R2_full - R2_reduced) / (1 - R2_full)
# The shortcut beta^2 / (1 - R2) is exact only when the term is uncorrelated
# with the other predictors, which an interaction term is not, so the
# R-squared change is computed directly. Both regressions use the same 89
# cases as the lavaan model (the two respondents without a condition count
# are excluded; there are no other missing values in these variables).

mod_vars <- c("ede_q7_global", "iot_c", "age_c", "iot_x_age",
              "years_diagnosed", "comorbidity_count")
df_cc <- na.omit(df[, mod_vars])
cat("Cases used:", nrow(df_cc), "\n")

lm_full    <- lm(ede_q7_global ~ iot_c + age_c + iot_x_age +
                   years_diagnosed + comorbidity_count, data = df_cc)
lm_reduced <- lm(ede_q7_global ~ iot_c + age_c +
                   years_diagnosed + comorbidity_count, data = df_cc)

r2_full    <- summary(lm_full)$r.squared
r2_reduced <- summary(lm_reduced)$r.squared
f2_a3      <- (r2_full - r2_reduced) / (1 - r2_full)

cat("R-squared, full model:   ", round(r2_full, 4), "\n")
cat("R-squared, without a3:   ", round(r2_reduced, 4), "\n")
cat("Cohen's f-squared for a3:", round(f2_a3, 4), "\n")

# Check: the lm estimate of a3 should equal the lavaan estimate above
cat("a3 from lm:", round(coef(lm_full)["iot_x_age"], 4), "\n")

power_a3_analytic <- pwr::pwr.f2.test(
  u   = 1,                    # numerator df: ONE term (a3) is being tested
  v   = nrow(df_cc) - 5 - 1,  # denominator df: n - 5 predictors - 1
  f2  = f2_a3,
  sig.level = 0.05
)

print(power_a3_analytic)


# ----------------------------------------------------------------------------
# 3. Minimum n for 80% power for the interaction term
# ----------------------------------------------------------------------------
# Searches for the n that achieves 80% power for a3, holding f2 constant.
# This answers: how many participants would this interaction require?

power_a3_n80 <- pwr::pwr.f2.test(
  u         = 1,
  f2        = f2_a3,
  sig.level = 0.05,
  power     = 0.80
)

cat("\nMinimum n for 80% power (interaction term a3):\n")
cat("v (denominator df) =", ceiling(power_a3_n80$v), "\n")
cat("n =", ceiling(power_a3_n80$v) + 5 + 1, "\n")


# ----------------------------------------------------------------------------
# 4. Simulation-based corroboration
# ----------------------------------------------------------------------------
# Simulates the EDE-Q7 equation 5,000 times at n = 89, keeping the observed
# predictor values (so their correlations are preserved) and generating
# EDE-Q7 from the estimated coefficients plus normal error with the
# estimated residual SD. Counts how often a3 is significant (p < .05).
# This corroborates the analytic result in Step 2.

set.seed(2025)
nrep   <- 5000
X      <- model.matrix(lm_full)
beta   <- coef(lm_full)
sd_res <- summary(lm_full)$sigma
sig_a3 <- 0

for (i in seq_len(nrep)) {
  ede_sim <- as.vector(X %*% beta) + rnorm(nrow(X), sd = sd_res)
  fit_sim <- lm(ede_sim ~ X - 1)
  p_a3    <- summary(fit_sim)$coefficients["Xiot_x_age", "Pr(>|t|)"]
  if (p_a3 < 0.05) sig_a3 <- sig_a3 + 1
}

power_sim <- sig_a3 / nrep
cat("\n--- Simulation-based power for a3 interaction (n = 89) ---\n")
cat("Replications:", nrep, "\n")
cat("Empirical power:", round(power_sim, 3), "\n")

# Package versions, for the README
writeLines(capture.output(sessionInfo()), "results/Script9_sessionInfo.txt")

# Verified output (21 Oct 2026):
#   a3 = -0.0053 (SE 0.0025, p = 0.0329), matching Script 5
#   Standardised a3 = -0.162 (SE 0.076)
#   R-squared, EDE-Q7 equation: 0.2058; without a3: 0.1821
#   Cohen's f-squared for a3: 0.0298
#   Analytic power at n = 89: 0.350
#   Minimum n for 80% power: 270 (v = 264)
#   Simulation-based power at n = 89: 0.358 (5,000 replications, seed 2025)
# Both methods agree: power for the a3 interaction was well below 80% at
# n = 89, so the moderated mediation result is treated as exploratory.
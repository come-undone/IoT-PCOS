# ============================================================================
# Script 8: Post Hoc Power Analysis for Primary Mediation
# ============================================================================
# The a priori power calculation (G*Power, n = 154) was designed for a
# bivariate comparison. The primary inferential finding is a mediation, and
# the indirect effect (product of paths a and b) has a non-normal sampling
# distribution. Monte Carlo simulation is therefore the appropriate method
# (Schoemann, Boulton & Short, 2017, Psychological Methods), using the
# observed standardised path coefficients and their SEs as population inputs.
#
# Package: power4mome v0.2.1 (Cheung, 2024)
# CRAN: https://cran.r-project.org/package=power4mome
# ============================================================================

library(dplyr)
library(lavaan)
library(power4mome)


# ----------------------------------------------------------------------------
# 1. Refit primary mediation model and extract standardised coefficients
# ----------------------------------------------------------------------------
# Reproduces Script 4 primary model exactly.
# power4mome requires standardised coefficients because it simulates data
# on a common scale.

df <- readRDS("data-processed/cleaned_data.rds")

df <- df %>%
  mutate(
    MVPA  = MET_moderate + MET_vigorous,
    iot_c = iot_total - mean(iot_total, na.rm = TRUE)
  )

model_primary <- '
  ede_q7_global ~ a*iot_c + age + years_diagnosed + comorbidity_count
  pcosqol_total ~ b*ede_q7_global + c*iot_c + age + years_diagnosed + comorbidity_count
  indirect := a * b
  total    := c + (a * b)
'

fit_primary <- lavaan::sem(model_primary, data = df,
                           missing = "fiml", estimator = "MLR")

std_paths <- lavaan::standardizedsolution(fit_primary)[
  lavaan::standardizedsolution(fit_primary)$label %in% c("a", "b", "c"),
  c("label", "est.std", "se")
]

print(std_paths)

# 95% CI for path a — lower bound used as primary sensitivity value in Step 4
print(lavaan::standardizedsolution(fit_primary)[
  lavaan::standardizedsolution(fit_primary)$label == "a",
  c("label", "est.std", "se", "ci.lower", "ci.upper")
])
# Verified: ci.lower = 0.141, ci.upper = 0.485

# Verified output:
#   label  est.std    se
#       a    0.313  0.088   path a: IoT -> EDE-Q7
#       b   -0.346  0.078   path b: EDE-Q7 -> PCOSQOL
#       c   -0.252  0.071   direct effect: IoT -> PCOSQOL


# ----------------------------------------------------------------------------
# 2. Post hoc power at achieved sample size
# ----------------------------------------------------------------------------
# Recruited n = 91; n = 89 used in the mediation model after listwise
# deletion of 2 cases with missing data on comorbidity_count (consistent
# with Script 4). All other covariates were complete across the full sample.
# Answers: what empirical power did the achieved sample provide?

set.seed(2025)

power_at_n89 <- q_power_mediation_simple(
  a            =  0.313,
  b            = -0.346,
  cp           = -0.252,
  n            = 89,
  ci_type      = "mc",
  R            = 5000,
  seed         = 2025,
  mode         = "power",
  target_power = 0.80
)

print(power_at_n89)

# Verified output:
#   Empirical power: 0.792 (95% CI: 0.750-0.829)
#   All replications valid (p.v = 1.000)
#   Mean indirect effect across replications: -0.108


# ----------------------------------------------------------------------------
# 3. Minimum n for 80% power
# ----------------------------------------------------------------------------
# Answers: what sample size would this indirect effect formally require?
# Search range: n = 50 to 2000 (probabilistic bisection algorithm).

set.seed(2025)

n_for_80 <- q_power_mediation_simple(
  a            =  0.313,
  b            = -0.346,
  cp           = -0.252,
  ci_type      = "mc",
  R            = 5000,
  seed         = 2025,
  mode         = "n",
  target_power = 0.80
)

print(n_for_80)

# Verified output:
#   Minimum n for 80% power: 91
#   Estimated power at n = 91: 0.784 (95% CI: 0.654-0.875)
#   Power exceeds 0.80 from approximately n = 92 onwards


# ----------------------------------------------------------------------------
# 4. Sensitivity analysis: power across a range of path a assumptions
# ----------------------------------------------------------------------------
# The observed path a (beta = 0.313) is estimated from n = 89. Small samples
# are susceptible to effect size inflation: only effects large enough to reach
# significance are observed, so the true population effect may be smaller
# (Schafer & Schwarz, 2019, Frontiers in Psychology).
#
# Path a is varied across four principled values while path b is held at the
# observed value (beta = -0.346). Path b is held fixed because it represents
# a well-established clinical association (eating concerns -> QoL) with
# stronger prior support than the novel IoT predictor.
#
# Four values of path a are tested:
#   0.313 = observed estimate (baseline, matches Steps 2-3)
#   0.235 = 75% of observed (modest attenuation)
#   0.157 = 50% of observed (conservative attenuation)
#   0.141 = lower bound of 95% CI for path a (statistically principled
#           minimum: smallest value consistent with the data at 95% confidence)
#
# The CI lower bound is the primary sensitivity value. The attenuation
# scenarios provide additional context.
#
# Extraction method: sig values pulled directly from replication list using
# sapply(); Wilson (1927) CI calculated from rejection rate and n replications.
# ----------------------------------------------------------------------------

wilson_ci <- function(p, n) {
  z      <- 1.96
  centre <- (p + z^2/(2*n)) / (1 + z^2/n)
  margin <- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / (1 + z^2/n)
  c(lower = round(centre - margin, 3),
    upper = round(centre + margin, 3))
}

a_values   <- c(0.313, 0.235, 0.157, 0.141)
a_labels   <- c("observed", "attenuated_75pct", "conservative_50pct", "ci_lower_bound")
b_fixed    <- -0.346
cp_fixed   <- -0.252
n_analysis <- 89

sensitivity_results <- data.frame(
  scenario        = a_labels,
  a_value         = a_values,
  indirect_effect = round(a_values * b_fixed, 3),
  power           = NA_real_,
  ci_lower        = NA_real_,
  ci_upper        = NA_real_
)

cat("Sensitivity analysis: power at n = 89 across path a assumptions\n\n")

for (i in seq_along(a_values)) {
  
  set.seed(2025)
  
  res_i <- q_power_mediation_simple(
    a            = a_values[i],
    b            = b_fixed,
    cp           = cp_fixed,
    n            = n_analysis,
    ci_type      = "mc",
    R            = 5000,
    seed         = 2025,
    mode         = "power",
    target_power = 0.80
  )
  
  sig_i   <- sapply(res_i$power4test$test_all[[1]], function(x) x$test["sig"])
  pwr_i   <- mean(sig_i)
  ci_i    <- wilson_ci(pwr_i, length(sig_i))
  
  sensitivity_results$power[i]    <- round(pwr_i, 3)
  sensitivity_results$ci_lower[i] <- ci_i["lower"]
  sensitivity_results$ci_upper[i] <- ci_i["upper"]
  
  cat("Scenario:", a_labels[i], "\n")
  cat("  Path a:", a_values[i], "| Indirect effect:", round(a_values[i] * b_fixed, 3), "\n")
  cat("  Power:", round(pwr_i, 3), "| 95% CI:", ci_i["lower"], "-", ci_i["upper"], "\n\n")
}

cat("Summary table:\n")
print(sensitivity_results)

# ----------------------------------------------------------------------------
# VERIFIED OUTPUTS - sensitivity analysis
# ----------------------------------------------------------------------------
# Scenario            | Path a | Indirect | Power | 95% CI
# observed            | 0.313  | -0.108   | 0.792 | [0.750, 0.829]
# attenuated_75pct    | 0.235  | -0.081   | 0.568 | [0.519, 0.615]
# conservative_50pct  | 0.157  | -0.054   | 0.295 | [0.252, 0.341]
# ci_lower_bound      | 0.141  | -0.049   | 0.240 | [0.201, 0.284]

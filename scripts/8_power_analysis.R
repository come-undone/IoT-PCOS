# ============================================================================
# PCOS IoT Study - Script 8: Post Hoc Power Analysis for Primary Mediation
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Monte Carlo power for the primary indirect effect
#          (use -> EDE-Q7 -> PCOSQOL), the sample size needed for 80% power,
#          and power under smaller assumed values of path a
#          (Supplemental Material 5).
# Package: power4mome v0.2.1 (Cheung et al., 2025)
# CRAN:    https://cran.r-project.org/package=power4mome
#
# Input:   data-processed/cleaned_data.rds (from Script 1)
# Output:  results/Script8_power_sensitivity.csv
#
# Run order: Script 1 -> 4 -> THIS SCRIPT
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
  mutate(iot_c = iot_total - mean(iot_total, na.rm = TRUE))

model_primary <- '
  ede_q7_global ~ a*iot_c + age + years_diagnosed + comorbidity_count
  pcosqol_total ~ b*ede_q7_global + c*iot_c + age + years_diagnosed + comorbidity_count
  indirect := a * b
  total    := c + (a * b)
'

fit_primary <- lavaan::sem(model_primary, data = df,
                           missing = "fiml", estimator = "MLR")

std_all   <- lavaan::standardizedsolution(fit_primary)
std_paths <- std_all[std_all$label %in% c("a", "b", "c", "indirect"),
                     c("label", "est.std", "se", "ci.lower", "ci.upper")]
print(std_paths)

# Verified output (21 Oct 2026):
#   a = 0.308 (SE 0.088; 95% CI 0.137 to 0.480); b = -0.341 (SE 0.078);
#   c' = -0.247 (SE 0.072); indirect = -0.105 (95% CI -0.175 to -0.035)

# Standardised paths used in Steps 2-4, taken from the fitted model rather
# than typed, and rounded to 3 dp as reported in Supplemental Material 5.
# The lower 95% CI bound of path a is the primary sensitivity value (Step 4).
a_std  <- round(std_paths$est.std[std_paths$label == "a"], 3)
b_std  <- round(std_paths$est.std[std_paths$label == "b"], 3)
cp_std <- round(std_paths$est.std[std_paths$label == "c"], 3)
a_low  <- round(std_paths$ci.lower[std_paths$label == "a"], 3)

cat("Path a:", a_std, "| path b:", b_std, "| direct c':", cp_std,
    "| path a, lower 95% CI:", a_low, "\n")


# ----------------------------------------------------------------------------
# 2. Post hoc power at achieved sample size
# ----------------------------------------------------------------------------
# Recruited n = 91; n = 89 used in the mediation model after listwise
# deletion of 2 cases with missing data on comorbidity_count (consistent
# with Script 4). All other covariates were complete across the full sample.
# Answers: what empirical power did the achieved sample provide?

set.seed(2025)

power_at_n89 <- q_power_mediation_simple(
  a            = a_std,
  b            = b_std,
  cp           = cp_std,
  n            = 89,
  ci_type      = "mc",
  R            = 5000,
  seed         = 2025,
  mode         = "power",
  target_power = 0.80
)

print(power_at_n89)
# Verified output (21 Oct 2026): power 0.767 (95% CI 0.724 to 0.806);
#   400 simulated datasets, all valid


# ----------------------------------------------------------------------------
# 3. Minimum n for 80% power
# ----------------------------------------------------------------------------
# Answers: what sample size would this indirect effect formally require?
# Search range: n = 50 to 2000 (probabilistic bisection algorithm).

set.seed(2025)

n_for_80 <- q_power_mediation_simple(
  a            = a_std,
  b            = b_std,
  cp           = cp_std,
  ci_type      = "mc",
  R            = 5000,
  seed         = 2025,
  mode         = "n",
  target_power = 0.80
)

print(n_for_80)
# Verified output (21 Oct 2026): n = 97, estimated power 0.824
#   (95% CI 0.697 to 0.904)


# ----------------------------------------------------------------------------
# 4. Sensitivity analysis: power across a range of path a assumptions
# ----------------------------------------------------------------------------
# The observed path a is estimated from n = 89. Small samples
# are susceptible to effect size inflation: only effects large enough to reach
# significance are observed, so the true population effect may be smaller
# (Schafer & Schwarz, 2019, Frontiers in Psychology).
#
# Path a is varied across four principled values while path b is held at the
# observed value. Path b is held fixed because it represents
# a well-established clinical association (eating concerns -> QoL) with
# stronger prior support than the novel IoT predictor.
#
# Four values of path a are tested:
#   observed estimate (baseline, matches Steps 2-3)
#   75% of observed (modest attenuation)
#   50% of observed (conservative attenuation)
#   lower bound of 95% CI for path a (statistically principled)
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

a_values   <- round(c(a_std, 0.75 * a_std, 0.50 * a_std, a_low), 3)
a_labels   <- c("observed", "attenuated_75pct", "conservative_50pct", "ci_lower_bound")
b_fixed    <- b_std
cp_fixed   <- cp_std
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

dir.create("results", showWarnings = FALSE)
write.csv(sensitivity_results, "results/Script8_power_sensitivity.csv",
          row.names = FALSE)
cat("Saved: results/Script8_power_sensitivity.csv\n")

# Verified output (21 Oct 2026):
# Scenario            | Path a | Indirect | Power | 95% CI
# observed            | 0.308  | -0.105   | 0.767 | [0.724, 0.806]
# attenuated_75pct    | 0.231  | -0.079   | 0.557 | [0.509, 0.605]
# conservative_50pct  | 0.154  | -0.053   | 0.278 | [0.236, 0.323]
# ci_lower_bound      | 0.137  | -0.047   | 0.230 | [0.191, 0.274]

# Package versions, for the README (power values depend on them)
writeLines(capture.output(sessionInfo()), "results/Script8_sessionInfo.txt")

# ============================================================================
# Script 9: Post Hoc Power Analysis for Moderated Mediation
# ============================================================================
# The moderated mediation tested whether age moderated path a (IoT use ->
# eating concerns / EDE-Q7). The key parameter is the interaction term a3
# (IoT x age -> EDE-Q7), which was significant in Script 5
# (unstandardised: a3 = -0.0054, SE = 0.0025, p = .032;
#  standardised: beta = -0.166, SE = 0.077).
#
# Unlike the indirect effect in Script 8, the interaction coefficient has an
# approximately normal sampling distribution, so analytic power calculation
# is appropriate (Cohen, 1988). A simulation-based check is also run to
# corroborate the analytic result.
#
# Note: power4mome v0.2.1 does not implement a dedicated moderated mediation
# power function. The approach here follows standard practice for interaction
# terms in regression (Fairchild & MacKinnon, 2009, Prevention Science).
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
    MVPA      = MET_moderate + MET_vigorous,
    iot_c     = iot_total         - mean(iot_total,          na.rm = TRUE),
    age_c     = age               - mean(age,                na.rm = TRUE),
    years_c   = years_diagnosed   - mean(years_diagnosed,    na.rm = TRUE),
    comor_c   = comorbidity_count - mean(comorbidity_count,  na.rm = TRUE),
    iot_x_age = iot_c * age_c
  )

model_modmed_age <- '
  ede_q7_global ~ a1*iot_c + a2*age_c + a3*iot_x_age + years_c + comor_c
  pcosqol_total ~ b*ede_q7_global + c*iot_c + age_c + years_c + comor_c
  indirect := a1 * b
  total    := c + (a1 * b)
'

fit_modmed_age <- lavaan::sem(model_modmed_age, data = df,
                              missing = "fiml", estimator = "MLR")

std_modmed <- lavaan::standardizedsolution(fit_modmed_age)[
  lavaan::standardizedsolution(fit_modmed_age)$label %in%
    c("a1", "a2", "a3", "b", "c"),
  c("label", "est.std", "se")
]

print(std_modmed)

# Verified output:
#   label  est.std    se
#      a1    0.346  0.085   main effect: IoT -> EDE-Q7 (at mean age)
#      a2   -0.154  0.114   main effect: age -> EDE-Q7
#      a3   -0.166  0.077   interaction: IoT x age -> EDE-Q7 (KEY TERM)
#       b   -0.346  0.078   path b: EDE-Q7 -> PCOSQOL
#       c   -0.252  0.071   direct effect: IoT -> PCOSQOL


# ----------------------------------------------------------------------------
# 2. Analytic power for the interaction term (a3) using pwr
# ----------------------------------------------------------------------------
# The a3 interaction (beta = -0.166) is a standardised regression coefficient.
# pwr::pwr.f2.test() calculates power for a single predictor in a multiple
# regression model using Cohen's f-squared effect size.
#
# f-squared for a single predictor = beta^2 / (1 - R^2_full)
# We use the model R-squared for the EDE-Q7 equation as the denominator.
# This is extracted directly from the fitted lavaan model.

# Extract R-squared for the EDE-Q7 (mediator) equation
r2_ede <- lavaan::inspect(fit_modmed_age, "r2")["ede_q7_global"]
cat("R-squared for EDE-Q7 equation:", round(r2_ede, 4), "\n")

# Cohen's f-squared for the a3 interaction term
# beta_a3 = -0.166 (standardised); f2 = beta^2 / (1 - R2)
beta_a3 <- -0.166
f2_a3   <- beta_a3^2 / (1 - r2_ede)
cat("Cohen's f-squared for a3 interaction:", round(f2_a3, 4), "\n")

# Number of predictors in the EDE-Q7 equation:
# iot_c, age_c, iot_x_age, years_c, comor_c = 5 predictors (u = 5)
# n = 89 (analysis sample, consistent with Scripts 4 and 5)

power_a3_analytic <- pwr::pwr.f2.test(
  u   = 5,       # numerator df = number of predictors in the equation
  v   = 89 - 5 - 1,  # denominator df = n - u - 1
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
  u         = 5,
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
# Simulates the moderated mediation dataset nrep times at n = 89 and counts
# how often the a3 interaction is detected as significant (p < .05).
# Uses the standardised coefficients as population parameters.
# This corroborates the analytic result in Step 2.

set.seed(2025)
nrep    <- 5000
n_sim   <- 89
sig_a3  <- 0

# Population parameters (standardised)
a1 <- 0.346; a2 <- -0.154; a3 <- -0.166
b  <- -0.346; cp <- -0.252

# Residual SDs derived from model variances:
# Var(.m) = 0.902 -> SD = sqrt(0.902); Var(.y) = 0.762 -> SD = sqrt(0.762)
sd_m <- sqrt(0.902)
sd_y <- sqrt(0.762)

for (i in seq_len(nrep)) {
  
  # Simulate standardised predictors
  x     <- rnorm(n_sim)
  w     <- rnorm(n_sim)   # age (standardised)
  xw    <- x * w          # interaction term
  years <- rnorm(n_sim)   # years diagnosed
  comor <- rnorm(n_sim)   # comorbidity count
  
  # Generate mediator (EDE-Q7)
  m <- a1*x + a2*w + a3*xw + 0.1*years + 0.1*comor + rnorm(n_sim, sd = sd_m)
  
  # Generate outcome (PCOSQOL)
  y <- b*m + cp*x + 0.1*w + 0.1*years + 0.1*comor + rnorm(n_sim, sd = sd_y)
  
  # Fit regression for path a (mediator equation)
  fit_sim <- lm(m ~ x + w + xw + years + comor)
  p_a3    <- summary(fit_sim)$coefficients["xw", "Pr(>|t|)"]
  
  if (!is.na(p_a3) && p_a3 < 0.05) sig_a3 <- sig_a3 + 1
}

power_sim <- sig_a3 / nrep
cat("\n--- Simulation-based power for a3 interaction (n = 89) ---\n")
cat("Replications:", nrep, "\n")
cat("Empirical power:", round(power_sim, 3), "\n")

# ----------------------------------------------------------------------------
# VERIFIED OUTPUTS
# ----------------------------------------------------------------------------
# Step 2 - R-squared (EDE-Q7 equation): 0.2036
#           Cohen's f-squared (a3):      0.0346 (small effect)
#           Analytic power at n = 89:    0.215
#
# Step 3 - Minimum n for 80% power (analytic): 377
#           (v = 371, u = 5, f2 = 0.0346, alpha = 0.05)
#
# Step 4 - Simulation-based power at n = 89:   0.337
#           (5000 replications, seed = 2025)
#
# Both methods agree: power for the a3 interaction was well below 80%
# at n = 89. The moderated mediation result is treated as exploratory
# in the manuscript.

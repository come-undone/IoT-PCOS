# ============================================================================
# PCOS IoT Study - Script 2: Descriptive Statistics and Scale Reliability
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Produce all descriptive statistics and internal consistency
#          estimates reported in the manuscript (Tables 1-3).
#
# Input:   data-processed/cleaned_data.rds (from Script_1_DataCleaning.R)
# Output:  results/table1_summary.csv
# ============================================================================

library(tidyverse)
library(psych)


# ----------------------------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------------------------

df <- readRDS("data-processed/cleaned_data.rds")
cat("n =", nrow(df), "\n")


# ----------------------------------------------------------------------------
# 2. Scale reliability (Cronbach's alpha)
# ----------------------------------------------------------------------------
# Reported in the Methods/Results section alongside scale descriptions.
# alpha >= .70 acceptable; >= .80 good; >= .90 excellent

pcosqol_cols <- paste0("pcosqol_", 1:35)
iot_cols     <- paste0("iot_",     1:10)
ede_cols     <- paste0("ede_",     1:7)
isi_cols     <- paste0("isi_",     1:7)
swemwbs_cols <- paste0("swemwbs_", 1:7)
cm_cols      <- paste0("cm_", 1:6)
cat(sprintf("Complementary medicine (6 items): %.3f\n",
            psych::alpha(df[, cm_cols])$total$raw_alpha))
cat("Note: CM alpha < .70 is expected — items tap distinct practices,\n")
cat("not a unidimensional construct.\n")

# PCOSQOL domain alphas
pcosqol_impact_items <- paste0("pcosqol_", c(4,6,10,13,15,16,17,18,19,20,27,28,29,32,33,35))
pcosqol_infert_items <- paste0("pcosqol_", c(1,8,9,23,25,31,34))
pcosqol_hirsu_items  <- paste0("pcosqol_", c(7,21,22,24,26,30))
pcosqol_mood_items   <- paste0("pcosqol_", c(2,3,5,11,12,14))

cat(sprintf("  PCOSQOL Impact (16 items):       %.3f\n", psych::alpha(df[, pcosqol_impact_items])$total$raw_alpha))
cat(sprintf("  PCOSQOL Infertility (7 items):   %.3f\n", psych::alpha(df[, pcosqol_infert_items])$total$raw_alpha))
cat(sprintf("  PCOSQOL Hirsutism (6 items):     %.3f\n", psych::alpha(df[, pcosqol_hirsu_items])$total$raw_alpha))
cat(sprintf("  PCOSQOL Mood (6 items):          %.3f\n", psych::alpha(df[, pcosqol_mood_items])$total$raw_alpha))

cat("\nCronbach's alpha:\n")
cat(sprintf("  PCOSQOL (35 items):  %.3f\n", psych::alpha(df[, pcosqol_cols])$total$raw_alpha))
cat(sprintf("  IoT (10 items):      %.3f\n", psych::alpha(df[, iot_cols])$total$raw_alpha))
cat(sprintf("  ISI (7 items):       %.3f\n", psych::alpha(df[, isi_cols])$total$raw_alpha))
cat(sprintf("  EDE-Q7 (7 items):    %.3f\n", psych::alpha(df[, ede_cols])$total$raw_alpha))
cat(sprintf("  SWEMWBS (7 items):   %.3f\n", psych::alpha(df[, swemwbs_cols])$total$raw_alpha))


# ----------------------------------------------------------------------------
# 3. Sample characteristics (Table 1)
# ----------------------------------------------------------------------------

cat("\n--- Age ---\n")
cat("Mean (SD):", round(mean(df$age), 2), "(", round(sd(df$age), 2), ")\n")
cat("Median [IQR]:", median(df$age), "[",
    quantile(df$age, 0.25), "-", quantile(df$age, 0.75), "]\n")
cat("Range:", min(df$age), "-", max(df$age), "\n")

cat("\n--- Years since PCOS diagnosis ---\n")
cat("Median [IQR]:", median(df$years_diagnosed),
    "[", quantile(df$years_diagnosed, 0.25), "-",
    quantile(df$years_diagnosed, 0.75), "]\n")
cat("Mean (SD):", round(mean(df$years_diagnosed), 2),
    "(", round(sd(df$years_diagnosed), 2), ")\n")

cat("\n--- Geographic distribution (nation) ---\n")
nation_tbl <- table(df$nation, useNA = "ifany")
print(nation_tbl)
round(prop.table(nation_tbl) * 100, 1)

cat("\n--- Urban vs Rural ---\n")
urb_tbl <- table(df$urban_rural, useNA = "ifany")
print(urb_tbl)
round(prop.table(urb_tbl) * 100, 1)

cat("\n--- Ethnicity ---\n")
eth_tbl <- table(df$ethnicity_cat, useNA = "ifany")
print(eth_tbl)
round(prop.table(eth_tbl) * 100, 1)

cat("\n--- Relationship status (grouped) ---\n")
rel_tbl <- table(df$relationship_grouped, useNA = "ifany")
print(rel_tbl)
round(prop.table(rel_tbl) * 100, 1)

cat("\n--- Has children ---\n")
ch_tbl <- table(df$has_children, useNA = "ifany")
print(ch_tbl)

cat("\n--- Highest level of education (grouped for table) ---\n")
cat("  Postgraduate or higher (PG + Doctorate): n =",
    sum(df$education_cat %in% c("Postgraduate", "Doctorate"), na.rm = TRUE), "\n")
cat("  Undergraduate: n =",
    sum(df$education_cat == "Undergraduate", na.rm = TRUE), "\n")
cat("  Secondary/College: n =",
    sum(df$education_cat %in% c("Secondary", "College"), na.rm = TRUE), "\n")
cat("  Other: n =",
    sum(df$education_cat == "Other", na.rm = TRUE), "\n")

cat("\n--- Occupational status (multiple selections permitted) ---\n")
cat("  Note: 5 respondents selected more than one category\n")
cat("  Full-time employed:  n =", sum(df$occ_fulltime,     na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_fulltime,     na.rm = TRUE) * 100))
cat("  Part-time employed:  n =", sum(df$occ_parttime,     na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_parttime,     na.rm = TRUE) * 100))
cat("  Unemployed:          n =", sum(df$occ_unemployed,   na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_unemployed,   na.rm = TRUE) * 100))
cat("  Student:             n =", sum(df$occ_student,      na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_student,      na.rm = TRUE) * 100))
cat("  Self-employed:       n =", sum(df$occ_selfemployed, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_selfemployed, na.rm = TRUE) * 100))
cat("  House person:        n =", sum(df$occ_houseperson,  na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_houseperson,  na.rm = TRUE) * 100))
cat("  Stay at home mother: n =", sum(df$occ_sahm,         na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_sahm,         na.rm = TRUE) * 100))
cat("  Disabled:            n =", sum(df$occ_disabled,     na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_disabled,     na.rm = TRUE) * 100))
cat("  Prefer not to answer:n =", sum(df$occ_prefernot,    na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$occ_prefernot,    na.rm = TRUE) * 100))

cat("\n--- Comorbidity count (18 pre-coded terms, used in analyses) ---\n")
summary(df$comorbidity_count)
cat("SD:", round(sd(df$comorbidity_count, na.rm = TRUE), 1), "\n")
cat("Missing:", sum(is.na(df$comorbidity_count)), "\n")

cat("\n--- Comorbidity distribution (all self-reported, for Table 2) ---\n")
cat("  None:          n =", sum(df$comorbidity_count_all == 0, na.rm = TRUE), "\n")
cat("  1 condition:   n =", sum(df$comorbidity_count_all == 1, na.rm = TRUE), "\n")
cat("  2 conditions:  n =", sum(df$comorbidity_count_all == 2, na.rm = TRUE), "\n")
cat("  3+ conditions: n =", sum(df$comorbidity_count_all >= 3, na.rm = TRUE), "\n")
cat("  Prefer not/NA: n =", sum(is.na(df$comorbidity_count_all)), "\n")
# Denominator = the 89 respondents who disclosed their conditions.
# The 2 who declined are unknown, not negative, so they are excluded.
n_disclosed <- sum(!is.na(df$comorbidity_count_all))
cat("  Disclosed conditions: n =", n_disclosed, "\n")

n_at_least_one <- sum(df$comorbidity_count_all >= 1, na.rm = TRUE)
cat(sprintf("  At least one condition: n = %d (%.1f%%)\n",
            n_at_least_one, 100 * n_at_least_one / n_disclosed))
n_multiple <- sum(df$comorbidity_count_all >= 2, na.rm = TRUE)
cat(sprintf("  Multiple conditions (2+): n = %d (%.1f%%)\n",
            n_multiple, 100 * n_multiple / n_disclosed))

cat("\n--- Specific condition prevalence (for Table 2) ---\n")
cat("  Note: has_depression and has_anxiety use case-insensitive matching\n")
cat("  and include free-text variants (e.g. Prenatal depression).\n")
cat("  Denominator = 89 respondents who disclosed their conditions.\n\n")

cat(sprintf("  %-35s n = %d (%.1f%%)\n", "Anxiety disorder",
            sum(df$has_anxiety     == 1, na.rm = TRUE),
            100 * sum(df$has_anxiety     == 1, na.rm = TRUE) / n_disclosed))
cat(sprintf("  %-35s n = %d (%.1f%%)\n", "Depression (all variants)",
            sum(df$has_depression  == 1, na.rm = TRUE),
            100 * sum(df$has_depression  == 1, na.rm = TRUE) / n_disclosed))
cat(sprintf("  %-35s n = %d (%.1f%%)\n", "Insulin resistance",
            sum(df$has_insulin_res == 1, na.rm = TRUE),
            100 * sum(df$has_insulin_res == 1, na.rm = TRUE) / n_disclosed))

cat("\n--- Alcohol use frequency ---\n")
table(df$alcohol_Q1, useNA = "ifany")

cat("\n--- Tobacco use ---\n")
table(df$tobacco_Q1, useNA = "ifany")

cat("\n--- Vaping ---\n")
table(df$tobacco_Q2, useNA = "ifany")


# ----------------------------------------------------------------------------
# 4. IoT usage patterns (Table 2)
# ----------------------------------------------------------------------------

cat("\n--- IoT total score (0-30) ---\n")
summary(df$iot_total)
cat("SD:", round(sd(df$iot_total, na.rm = TRUE), 2), "\n")

cat("\n--- IoT Engagement subscale (0-15) ---\n")
summary(df$iot_engagement)
cat("SD:", round(sd(df$iot_engagement, na.rm = TRUE), 2), "\n")

cat("\n--- IoT Self-Monitoring subscale (0-15) ---\n")
summary(df$iot_selfmon)
cat("SD:", round(sd(df$iot_selfmon, na.rm = TRUE), 2), "\n")

cat("\n--- IoT group distribution ---\n")
print(table(df$iot_category, useNA = "ifany"))
round(prop.table(table(df$iot_category)) * 100, 1)

cat("\n--- Individual IoT items: mean (SD) and frequency breakdown ---\n")
iot_labels <- c(
  "Browse PCOS websites",
  "Online PCOS forums",
  "PCOS info on social media",
  "Share PCOS experiences on social media",
  "AI/LLMs for PCOS advice",
  "Apps: monitor PCOS symptoms",
  "Wearable fitness/activity trackers",
  "Apps: monitor sleep",
  "Apps: monitor nutrition",
  "Apps: monitor physical activity"
)

for (i in seq_along(iot_cols)) {
  x <- df[[iot_cols[i]]]
  cat(sprintf("\n  Item %d: %s\n", i, iot_labels[i]))
  cat(sprintf("    Mean (SD): %.2f (%.2f)\n", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
  freq <- table(x)
  pct  <- round(prop.table(freq) * 100, 1)
  cat(sprintf("    Never:%d(%.1f%%)  Seldom:%d(%.1f%%)  Often:%d(%.1f%%)  Regularly:%d(%.1f%%)\n",
              freq["0"], pct["0"], freq["1"], pct["1"],
              freq["2"], pct["2"], freq["3"], pct["3"]))
}


# ----------------------------------------------------------------------------
# 5. Health outcomes (Table 3)
# ----------------------------------------------------------------------------

cat("\n--- Physical activity: MET-min/week (n=80, 11 missing) ---\n")
summary(df$MET_total)
cat("SD:", round(sd(df$MET_total, na.rm = TRUE), 1), "\n")
print(table(df$PA_category, useNA = "ifany"))

cat("\n--- Sitting hours/weekday (n=76, 15 missing) ---\n")
summary(df$sitting_hours)
cat("SD:", round(sd(df$sitting_hours, na.rm = TRUE), 1), "\n")

cat("\n--- ISI total (0-28, higher = worse insomnia) ---\n")
summary(df$isi_total)
cat("SD:", round(sd(df$isi_total, na.rm = TRUE), 1), "\n")
print(table(df$isi_category, useNA = "ifany"))

cat("\n--- EDE-Q7 global mean score (0-6) ---\n")
summary(df$ede_q7_global)
cat("SD:", round(sd(df$ede_q7_global, na.rm = TRUE), 2), "\n")
cat("Above clinical threshold (>3.64): n =",
    sum(df$ede_q7_global > 3.64, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$ede_q7_global > 3.64, na.rm = TRUE) * 100))

cat("\n--- SWEMWBS total (7-35, higher = better wellbeing) ---\n")
summary(df$swemwbs_total)
cat("SD:", round(sd(df$swemwbs_total, na.rm = TRUE), 1), "\n")
# Note: observed range is 11-34; no participant scored below 11 in this sample

cat("\n--- PCOSQOL total (35-245, higher = better QoL) ---\n")
summary(df$pcosqol_total)
cat("SD:", round(sd(df$pcosqol_total, na.rm = TRUE), 1), "\n")

cat("\n--- PCOSQOL domain scores ---\n")
for (v in c("pcosqol_impact", "pcosqol_infertility",
            "pcosqol_hirsutism", "pcosqol_mood")) {
  cat(sprintf("  %-25s  Median [IQR]: %.1f [%.1f-%.1f]  Mean (SD): %.1f (%.1f)\n",
              v,
              median(df[[v]], na.rm = TRUE),
              quantile(df[[v]], 0.25, na.rm = TRUE),
              quantile(df[[v]], 0.75, na.rm = TRUE),
              mean(df[[v]], na.rm = TRUE),
              sd(df[[v]], na.rm = TRUE)))
}


# ----------------------------------------------------------------------------
# 6. Distributional checks
# ----------------------------------------------------------------------------
# Descriptive only. The rank-based tests in Script 3 assume no distributional
# form, and Scripts 4-5 use the MLR estimator, whose standard errors and test
# statistic are corrected for non-normality. These summaries describe the data;
# they do not select the tests.

dist_vars <- c("iot_total", "MET_total", "MET_total_log",
               "isi_total", "ede_q7_global", "swemwbs_total", "pcosqol_total")

# Guard: log(MET + 1) must change the shape. If this stops the script,
# cleaned_data.rds is stale and Script 1 needs re-running.
stopifnot(!isTRUE(all.equal(df$MET_total, df$MET_total_log)))

dist_checks <- psych::describe(as.data.frame(df[, dist_vars]))[
  , c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")]

dist_checks <- cbind(dist_checks, t(sapply(dist_vars, function(v) {
  s <- shapiro.test(df[[v]][!is.na(df[[v]])])
  c(shapiro_W = unname(s$statistic), shapiro_p = unname(s$p.value))
})))

cat("\n--- Distribution of continuous variables ---\n")
print(round(dist_checks, 3))
write.csv(round(dist_checks, 3), "results/Script2_distribution_checks.csv")

# Q-Q plots: visual assessment, preferred over a single normality test
png("results/Script2_qqplots.png", width = 1200, height = 700, res = 120)
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (v in dist_vars) {
  x <- df[[v]][!is.na(df[[v]])]
  qqnorm(x, main = v, pch = 16, cex = 0.5,
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
  qqline(x, col = "grey40", lwd = 1.5)
}
dev.off()

# ----------------------------------------------------------------------------
# 7. Spearman correlations: IoT vs outcomes
# ----------------------------------------------------------------------------

cat("\n--- IoT total vs outcomes ---\n")
outcomes <- c("MET_total", "isi_total", "ede_q7_global",
              "swemwbs_total", "pcosqol_total")

for (v in outcomes) {
  ct <- cor.test(df$iot_total, df[[v]], method = "spearman", exact = FALSE)
  cat(sprintf("  IoT total vs %-15s  rho = %6.3f  p = %.3f\n",
              v, ct$estimate, ct$p.value))
}

cat("\n--- IoT Engagement vs outcomes ---\n")
for (v in outcomes) {
  ct <- cor.test(df$iot_engagement, df[[v]], method = "spearman", exact = FALSE)
  cat(sprintf("  Engagement vs %-15s  rho = %6.3f  p = %.3f\n",
              v, ct$estimate, ct$p.value))
}

cat("\n--- IoT Self-Monitoring vs outcomes ---\n")
for (v in outcomes) {
  ct <- cor.test(df$iot_selfmon, df[[v]], method = "spearman", exact = FALSE)
  cat(sprintf("  Self-Mon vs %-15s  rho = %6.3f  p = %.3f\n",
              v, ct$estimate, ct$p.value))
}

# ----------------------------------------------------------------------------
# 7b. FDR-corrected correlations: IoT Total vs outcomes
# ----------------------------------------------------------------------------
# Benjamini-Hochberg FDR correction across the 5 outcome tests for the
# IoT Total predictor. This mirrors exactly the correction already applied
# to the IoT subscales in Script 3 (Section 5), so that all three predictor
# families are treated identically. Both uncorrected and corrected p-values
# are retained so Table 6 can report each.

cat("\n--- IoT Total vs outcomes: BH FDR corrected (5 tests) ---\n\n")

cor_total <- data.frame()

for (v in outcomes) {
  idx <- !is.na(df$iot_total) & !is.na(df[[v]])
  ct  <- cor.test(df$iot_total[idx], df[[v]][idx],
                  method = "spearman", exact = FALSE)
  cor_total <- bind_rows(cor_total, data.frame(
    Predictor = "IoT Total",
    Outcome   = v,
    rho       = round(ct$estimate, 3),
    p_raw     = round(ct$p.value, 4),
    n         = sum(idx)
  ))
}

cor_total$p_fdr   <- round(p.adjust(cor_total$p_raw, method = "BH"), 4)
cor_total$Sig_raw <- ifelse(cor_total$p_raw < .001, "***",
                            ifelse(cor_total$p_raw < .01,  "**",
                                   ifelse(cor_total$p_raw < .05,  "*", "ns")))
cor_total$Sig_fdr <- ifelse(cor_total$p_fdr < .001, "***",
                            ifelse(cor_total$p_fdr < .01,  "**",
                                   ifelse(cor_total$p_fdr < .05,  "*", "ns")))

print(as.data.frame(cor_total))

write_csv(cor_total, "results/Script2_iot_total_vs_outcomes_FDR.csv")
cat("\nSaved: results/Script2_iot_total_vs_outcomes_FDR.csv\n")

# ----------------------------------------------------------------------------
# 8. Comorbidity effects on key outcomes (supplementary checks)
# ----------------------------------------------------------------------------

cat("\n--- PCOSQOL by depression status ---\n")
print(tapply(df$pcosqol_total, df$has_depression, median, na.rm = TRUE))
wilcox.test(pcosqol_total ~ has_depression, data = df)

cat("\n--- ISI by anxiety status ---\n")
print(tapply(df$isi_total, df$has_anxiety, median, na.rm = TRUE))
wilcox.test(isi_total ~ has_anxiety, data = df)


# ----------------------------------------------------------------------------
# 9. Distribution plots (visual check)
# ----------------------------------------------------------------------------

par(mfrow = c(2, 3))
hist(df$iot_total,     main = "IoT Total",    xlab = "Score (0-30)",  col = "lightblue",   breaks = 15)
hist(df$MET_total,     main = "MET-min/week", xlab = "MET-min/week",  col = "lightgreen",  breaks = 15)
hist(df$MET_total_log, main = "MET (log)",    xlab = "log(MET+1)",    col = "lightgreen",  breaks = 15)
hist(df$pcosqol_total, main = "PCOSQOL",      xlab = "Score (35-245)",col = "lightyellow", breaks = 15)
hist(df$ede_q7_global, main = "EDE-Q7",       xlab = "Score (0-6)",   col = "lightyellow", breaks = 15)
hist(df$isi_total,     main = "ISI",          xlab = "Score (0-28)",  col = "lightyellow", breaks = 15)
par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 10. Save summary table
# ----------------------------------------------------------------------------

cont_vars <- list(
  c("age",               "Age (years)"),
  c("years_diagnosed",   "Years since diagnosis"),
  c("comorbidity_count", "Comorbidity count"),
  c("iot_total",         "IoT total (0-30)"),
  c("iot_engagement",    "IoT engagement (0-15)"),
  c("iot_selfmon",       "IoT self-monitoring (0-15)"),
  c("MET_total",         "MET-min/week"),
  c("sitting_hours",     "Sitting hours/weekday"),
  c("isi_total",         "ISI total (0-28)"),
  c("ede_q7_global",     "EDE-Q7 global mean (0-6)"),
  c("swemwbs_total",     "SWEMWBS total (7-35)"),
  c("pcosqol_total",     "PCOSQOL total (35-245)")
)

summary_table <- map_dfr(cont_vars, function(item) {
  x <- df[[item[1]]]
  tibble(
    Variable   = item[2],
    N          = sum(!is.na(x)),
    Missing    = sum(is.na(x)),
    Mean_SD    = sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
    Median_IQR = sprintf("%.1f [%.1f-%.1f]",
                         median(x, na.rm = TRUE),
                         quantile(x, 0.25, na.rm = TRUE),
                         quantile(x, 0.75, na.rm = TRUE)),
    Range      = sprintf("%.1f-%.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  )
})

write_csv(summary_table, "results/table1_summary.csv")
cat("\nDone. Summary table saved to results/table1_summary.csv\n")

# Next: Script_3_GroupComparisons.R


# ----------------------------------------------------------------------------
# Formal test of the subscale contrast
# ----------------------------------------------------------------------------
# The paper claims the Engagement-PCOSQOL association is stronger than the
# Self-Monitoring-PCOSQOL association. Comparing "significant" with
# "non-significant" is not a test of that difference (Gelman & Stern, 2006).
# Steiger's test compares two dependent, overlapping correlations.

library(cocor)

d <- df[complete.cases(df$pcosqol_total, df$iot_engagement, df$iot_selfmon), ]

cat("Engagement x Self-Monitoring intercorrelation (Spearman):\n")
print(cor.test(d$iot_engagement, d$iot_selfmon, method = "spearman", exact = FALSE))

cocor.dep.groups.overlap(
  r.jk = cor(d$pcosqol_total, d$iot_engagement, method = "spearman"),
  r.jh = cor(d$pcosqol_total, d$iot_selfmon,    method = "spearman"),
  r.kh = cor(d$iot_engagement, d$iot_selfmon,   method = "spearman"),
  n = nrow(d), alternative = "two.sided", test = "steiger1980"
)









cocor.dep.groups.overlap(
  r.jk = cor(d$pcosqol_total, d$iot_engagement),
  r.jh = cor(d$pcosqol_total, d$iot_selfmon),
  r.kh = cor(d$iot_engagement, d$iot_selfmon),
  n = nrow(d), alternative = "two.sided", test = "steiger1980"
)








cocor.dep.groups.overlap(
  r.jk = cor(d$ede_q7_global, d$iot_engagement, method = "spearman"),
  r.jh = cor(d$ede_q7_global, d$iot_selfmon,    method = "spearman"),
  r.kh = cor(d$iot_engagement, d$iot_selfmon,   method = "spearman"),
  n = nrow(d), alternative = "two.sided", test = "steiger1980"
)



cocor.dep.groups.overlap(
  r.jk = cor(d$swemwbs_total, d$iot_engagement, method = "spearman"),
  r.jh = cor(d$swemwbs_total, d$iot_selfmon,    method = "spearman"),
  r.kh = cor(d$iot_engagement, d$iot_selfmon,   method = "spearman"),
  n = nrow(d), alternative = "two.sided", test = "steiger1980"
)







c(raw = shapiro.test(df$MET_total)$statistic,
  log = shapiro.test(df$MET_total_log)$statistic)


p.adjust(c(0.0008, 0.2861, 0.0124), method = "BH")

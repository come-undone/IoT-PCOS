# ============================================================================
# PCOS IoT Study - Script 3: Group Comparisons
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Stage 2 analyses - compare health outcomes across IoT tertile
#          groups, and test whether associations differ between the
#          Engagement and Self-Monitoring subscales.
#
# Sections:
#   1. IoT total tertile groups vs outcomes (Kruskal-Wallis + Dunn)
#   2. Descriptive table by IoT group
#   3. Engagement subscale tertile groups vs outcomes
#   4. Self-Monitoring subscale tertile groups vs outcomes
#   5. FDR-corrected subscale correlations
#   6. Export
#
# Scale directions:
#   PCOSQOL  - higher = BETTER quality of life
#   EDE-Q7   - higher = MORE eating concerns
#   ISI      - higher = WORSE insomnia
#   SWEMWBS  - higher = BETTER mental wellbeing
#   MET      - higher = MORE physical activity
#
# Input:  data-processed/cleaned_data.rds
# Output: results/Script3_KW_results.csv
#         results/Script3_posthoc.csv
#         results/Script3_descriptives_by_group.csv
#         results/Script3_subscale_correlations.csv
# ============================================================================

library(tidyverse)
library(dunn.test)


# ----------------------------------------------------------------------------
# 0. Load data
# ----------------------------------------------------------------------------

df <- readRDS("data-processed/cleaned_data.rds")
cat("n =", nrow(df), "\n\n")

# Confirm IoT group distribution and cut-points from Script 1
cat("IoT tertile groups:\n")
print(table(df$iot_category, useNA = "always"))
cat("\nTertile cut-points:\n")
print(quantile(df$iot_total, probs = c(1/3, 2/3), na.rm = TRUE))


# ----------------------------------------------------------------------------
# 1. IoT total tertile groups vs outcomes
# ----------------------------------------------------------------------------
# Kruskal-Wallis H with epsilon-squared effect size
# Post-hoc: Dunn's test with Bonferroni correction
# Effect size benchmarks: >= .01 small, >= .06 medium, >= .14 large

cat("\n--- Section 1: IoT total tertile groups vs outcomes ---\n\n")

outcomes <- c("pcosqol_total", "ede_q7_global", "isi_total",
              "swemwbs_total", "MET_total")

kw_rows   <- list()
dunn_rows <- list()

for (v in outcomes) {

  df_sub  <- df %>% filter(!is.na(.data[[v]]), !is.na(iot_category))
  n_total <- nrow(df_sub)
  k       <- length(levels(df_sub$iot_category))

  cat(sprintf("Outcome: %s\n", v))

  # Descriptives by group
  df_sub %>%
    group_by(iot_category) %>%
    summarise(
      n      = n(),
      Median = round(median(.data[[v]], na.rm = TRUE), 2),
      IQR    = round(IQR(.data[[v]], na.rm = TRUE), 2),
      Mean   = round(mean(.data[[v]], na.rm = TRUE), 2),
      SD     = round(sd(.data[[v]], na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    print()

  # Kruskal-Wallis
  kw     <- kruskal.test(as.formula(paste(v, "~ iot_category")), data = df_sub)
  eps_sq <- max(0, (kw$statistic - k + 1) / (n_total - k))
  sig    <- ifelse(kw$p.value < .001, "***",
             ifelse(kw$p.value < .01,  "**",
             ifelse(kw$p.value < .05,  "*", "ns")))

  cat(sprintf("KW: H(%d) = %.3f, p = %.4f %s, epsilon2 = %.3f\n\n",
              kw$parameter, kw$statistic, kw$p.value, sig, eps_sq))

  kw_rows[[v]] <- data.frame(
    Outcome    = v,
    H_df       = kw$parameter,
    H_stat     = round(kw$statistic, 3),
    p_KW       = round(kw$p.value, 4),
    Sig        = sig,
    epsilon_sq = round(eps_sq, 3),
    n          = n_total
  )

  # Dunn post-hoc (always run for the five outcomes - saves results)
  cat("Dunn post-hoc (Bonferroni):\n")
  dunn <- dunn.test::dunn.test(
    x      = df_sub[[v]],
    g      = df_sub$iot_category,
    method = "bonferroni",
    alpha  = 0.05,
    altp   = TRUE,
    list   = FALSE,
    table  = TRUE
  )
  cat("\n")

  dunn_rows[[v]] <- data.frame(
    Outcome    = v,
    Comparison = dunn$comparisons,
    Z          = round(dunn$Z, 3),
    p_adj      = round(dunn$altP.adjusted, 4),
    Sig        = ifelse(dunn$altP.adjusted < .001, "***",
                 ifelse(dunn$altP.adjusted < .01,  "**",
                 ifelse(dunn$altP.adjusted < .05,  "*", "ns")))
  )
}


# ----------------------------------------------------------------------------
# 2. Descriptive table by IoT group (for manuscript)
# ----------------------------------------------------------------------------

cat("\n--- Section 2: Descriptive table by IoT group ---\n")
cat("PCOSQOL/SWEMWBS/MET: higher = better; ISI/EDE-Q7: higher = worse\n\n")

desc_rows <- list()

for (v in outcomes) {
  df_sub <- df %>% filter(!is.na(.data[[v]]), !is.na(iot_category))

  row <- df_sub %>%
    group_by(iot_category) %>%
    summarise(
      n      = n(),
      Median = round(median(.data[[v]], na.rm = TRUE), 2),
      Q1     = round(quantile(.data[[v]], 0.25, na.rm = TRUE), 2),
      Q3     = round(quantile(.data[[v]], 0.75, na.rm = TRUE), 2),
      val    = sprintf("%.2f [%.2f\u2013%.2f]", Median, Q1, Q3),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = iot_category, values_from = c(val, n)) %>%
    mutate(Variable = v) %>%
    select(Variable,
           Low = val_Low, n_Low,
           Medium = val_Medium, n_Medium,
           High = val_High, n_High)

  desc_rows[[v]] <- row
}

desc_table <- bind_rows(desc_rows)
print(as.data.frame(desc_table))


# ----------------------------------------------------------------------------
# 3. Engagement subscale tertile groups vs outcomes
# ----------------------------------------------------------------------------

cat("\n--- Section 3: Engagement tertile groups vs outcomes ---\n\n")

eng_cuts <- quantile(df$iot_engagement, probs = c(1/3, 2/3), na.rm = TRUE)
df <- df %>%
  mutate(
    eng_group = cut(iot_engagement,
                    breaks = c(-Inf, eng_cuts, Inf),
                    labels = c("Low", "Medium", "High"),
                    ordered_result = TRUE)
  )

cat("Engagement groups:\n")
print(table(df$eng_group, useNA = "always"))
cat("Cut-points:", eng_cuts, "\n\n")

kw_eng <- list()

for (v in outcomes) {

  df_sub  <- df %>% filter(!is.na(.data[[v]]), !is.na(eng_group))
  n_total <- nrow(df_sub)
  k       <- length(levels(df_sub$eng_group))

  cat(sprintf("Outcome: %s\n", v))

  df_sub %>%
    group_by(eng_group) %>%
    summarise(
      n      = n(),
      Q1     = round(quantile(.data[[v]], 0.25, na.rm = TRUE), 2),
      Median = round(median(.data[[v]], na.rm = TRUE), 2),
      Q3     = round(quantile(.data[[v]], 0.75, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    print()

  kw     <- kruskal.test(as.formula(paste(v, "~ eng_group")), data = df_sub)
  eps_sq <- max(0, (kw$statistic - k + 1) / (n_total - k))
  sig    <- ifelse(kw$p.value < .001, "***",
             ifelse(kw$p.value < .01,  "**",
             ifelse(kw$p.value < .05,  "*", "ns")))

  cat(sprintf("KW: H(%d) = %.3f, p = %.4f %s, epsilon2 = %.3f\n\n",
              kw$parameter, kw$statistic, kw$p.value, sig, eps_sq))

  if (kw$p.value < .05) {
    cat("Dunn post-hoc (Bonferroni):\n")
    dunn.test::dunn.test(df_sub[[v]], df_sub$eng_group,
                         method = "bonferroni", alpha = 0.05, altp = TRUE,
                         list = FALSE, table = TRUE)
    cat("\n")
  }

  kw_eng[[v]] <- data.frame(
    Outcome    = v,
    Group      = "Engagement",
    H_df       = kw$parameter,
    H_stat     = round(kw$statistic, 3),
    p_KW       = round(kw$p.value, 4),
    Sig        = sig,
    epsilon_sq = round(eps_sq, 3)
  )
}


# ----------------------------------------------------------------------------
# 4. Self-Monitoring subscale tertile groups vs outcomes
# ----------------------------------------------------------------------------

cat("\n--- Section 4: Self-Monitoring tertile groups vs outcomes ---\n\n")

sm_cuts <- quantile(df$iot_selfmon, probs = c(1/3, 2/3), na.rm = TRUE)
df <- df %>%
  mutate(
    sm_group = cut(iot_selfmon,
                   breaks = c(-Inf, sm_cuts, Inf),
                   labels = c("Low", "Medium", "High"),
                   ordered_result = TRUE)
  )

cat("Self-Monitoring groups:\n")
print(table(df$sm_group, useNA = "always"))
cat("Cut-points:", sm_cuts, "\n\n")

kw_sm <- list()

for (v in outcomes) {

  df_sub  <- df %>% filter(!is.na(.data[[v]]), !is.na(sm_group))
  n_total <- nrow(df_sub)
  k       <- length(levels(df_sub$sm_group))

  cat(sprintf("Outcome: %s\n", v))

  df_sub %>%
    group_by(sm_group) %>%
    summarise(
      n      = n(),
      Q1     = round(quantile(.data[[v]], 0.25, na.rm = TRUE), 2),
      Median = round(median(.data[[v]], na.rm = TRUE), 2),
      Q3     = round(quantile(.data[[v]], 0.75, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    print()

  kw     <- kruskal.test(as.formula(paste(v, "~ sm_group")), data = df_sub)
  eps_sq <- max(0, (kw$statistic - k + 1) / (n_total - k))
  sig    <- ifelse(kw$p.value < .001, "***",
             ifelse(kw$p.value < .01,  "**",
             ifelse(kw$p.value < .05,  "*", "ns")))

  cat(sprintf("KW: H(%d) = %.3f, p = %.4f %s, epsilon2 = %.3f\n\n",
              kw$parameter, kw$statistic, kw$p.value, sig, eps_sq))

  if (kw$p.value < .05) {
    cat("Dunn post-hoc (Bonferroni):\n")
    dunn.test::dunn.test(df_sub[[v]], df_sub$sm_group,
                         method = "bonferroni", alpha = 0.05, altp = TRUE,
                         list = FALSE, table = TRUE)
    cat("\n")
  }

  kw_sm[[v]] <- data.frame(
    Outcome    = v,
    Group      = "Self-Monitoring",
    H_df       = kw$parameter,
    H_stat     = round(kw$statistic, 3),
    p_KW       = round(kw$p.value, 4),
    Sig        = sig,
    epsilon_sq = round(eps_sq, 3)
  )
}


# ----------------------------------------------------------------------------
# 5. FDR-corrected subscale correlations (Benjamini-Hochberg)
# ----------------------------------------------------------------------------
# One BH family per subscale (5 comparisons each).
# Both raw and FDR-corrected p-values are reported.

cat("\n--- Section 5: Subscale correlations (FDR corrected) ---\n\n")

subscales <- c("iot_engagement", "iot_selfmon")
cor_results <- data.frame()

for (sub_v in subscales) {

  cat(sprintf("Predictor: %s\n", sub_v))
  family <- data.frame()

  for (v in outcomes) {
    idx <- !is.na(df[[sub_v]]) & !is.na(df[[v]])
    ct  <- cor.test(df[[sub_v]][idx], df[[v]][idx],
                    method = "spearman", exact = FALSE)

    family <- bind_rows(family, data.frame(
      Predictor = sub_v,
      Outcome   = v,
      rho       = round(ct$estimate, 3),
      p_raw     = round(ct$p.value, 4),
      n         = sum(idx)
    ))
  }

  family$p_fdr   <- round(p.adjust(family$p_raw, method = "BH"), 4)
  family$Sig_raw <- ifelse(family$p_raw < .001, "***",
                    ifelse(family$p_raw < .01,  "**",
                    ifelse(family$p_raw < .05,  "*", "ns")))
  family$Sig_fdr <- ifelse(family$p_fdr < .001, "***",
                    ifelse(family$p_fdr < .01,  "**",
                    ifelse(family$p_fdr < .05,  "*", "ns")))

  print(as.data.frame(family))
  cat("\n")

  cor_results <- bind_rows(cor_results, family)
}


# ----------------------------------------------------------------------------
# 6. Export
# ----------------------------------------------------------------------------

write_csv(bind_rows(kw_rows),   "results/Script3_KW_results.csv")
write_csv(bind_rows(dunn_rows), "results/Script3_posthoc.csv")
write_csv(desc_table,           "results/Script3_descriptives_by_group.csv")
write_csv(cor_results,          "results/Script3_subscale_correlations.csv")

cat("Results saved to results/\n")
cat("Next: Script4_Mediation_FINAL.R\n")

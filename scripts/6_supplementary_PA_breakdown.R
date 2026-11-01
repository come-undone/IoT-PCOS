# ============================================================================
# PCOS IoT Study - Script 6 (Supplementary): PA Sub-Component Breakdown
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Supplementary analyses examining IoT use (total and subscales)
#          against PA sub-components and sedentary behaviour.
#          Reported in Supplemental Material 3 (Table 3.1).
#
# Rationale: PCOS guidelines specifically recommend MVPA. Sitting time is
#          an independent cardiometabolic risk factor in PCOS. These
#          analyses extend the primary MET_total findings in Script 3 to
#          examine whether associations are driven by specific PA types.
#
# Sections:
#   1. Load data and create MVPA variable
#   2. IoT total vs PA breakdown (Spearman + FDR, then KW)
#   3. IoT subscales vs PA breakdown (Spearman + FDR, then KW)
#
# PA variables:
#   MVPA_total    = MET_moderate + MET_vigorous (MET-min/week)
#   MET_walk      = Walking MET-min/week
#   sitting_hours = Sitting hours per weekday (higher = more sedentary)
#
# Input:  data-processed/cleaned_data.rds (from Script 1)
# Output: results/Script6_iot_total_vs_PA.csv
#         results/Script6_subscales_vs_PA.csv
#
# Run order: Script 1 -> 2 -> 3 -> 4 -> 5 -> THIS SCRIPT (supplementary)
# ============================================================================

library(tidyverse)
library(dunn.test)


# ----------------------------------------------------------------------------
# 1. Load data and create MVPA variable
# ----------------------------------------------------------------------------

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n\n")

# MVPA = moderate + vigorous MET-min/week (standard IPAQ-SF MVPA metric)
df <- df %>%
  mutate(MVPA_total = MET_moderate + MET_vigorous)

cat("MVPA_total (MET_moderate + MET_vigorous):\n")
cat(sprintf("  n valid = %d  |  Mean = %.1f (SD %.1f)  |  Range = %.0f-%.0f\n\n",
            sum(!is.na(df$MVPA_total)),
            mean(df$MVPA_total, na.rm = TRUE),
            sd(df$MVPA_total,   na.rm = TRUE),
            min(df$MVPA_total,  na.rm = TRUE),
            max(df$MVPA_total,  na.rm = TRUE)))

# Subscale tertile groups (consistent with Script 3)
eng_cuts    <- quantile(df$iot_engagement, probs = c(1/3, 2/3), na.rm = TRUE)
selfmon_cuts <- quantile(df$iot_selfmon,   probs = c(1/3, 2/3), na.rm = TRUE)

df <- df %>%
  mutate(
    eng_group = cut(iot_engagement,
                    breaks = c(-Inf, eng_cuts, Inf),
                    labels = c("Low", "Medium", "High"),
                    ordered_result = TRUE),
    selfmon_group = cut(iot_selfmon,
                        breaks = c(-Inf, selfmon_cuts, Inf),
                        labels = c("Low", "Medium", "High"),
                        ordered_result = TRUE)
  )

pa_vars   <- c("MVPA_total", "MET_walk", "sitting_hours")
pa_labels <- c("MVPA MET-min/week", "Walking MET-min/week",
               "Sitting hours/weekday")


# ----------------------------------------------------------------------------
# 2. IoT total vs PA breakdown
# ----------------------------------------------------------------------------

cat("--- Section 2: IoT total vs PA sub-components ---\n\n")

# 2a: Spearman correlations with BH FDR correction (3 tests, 1 family)
cat("Spearman correlations (BH FDR, 3 tests):\n\n")

cor_total <- data.frame()

for (i in seq_along(pa_vars)) {
  idx <- !is.na(df$iot_total) & !is.na(df[[pa_vars[i]]])
  ct  <- cor.test(df$iot_total[idx], df[[pa_vars[i]]][idx],
                  method = "spearman", exact = FALSE)
  cor_total <- bind_rows(cor_total, data.frame(
    Predictor = "IoT total",
    Outcome   = pa_labels[i],
    rho       = round(unname(ct$estimate), 3),
    p_raw     = ct$p.value,
    n         = sum(idx)
  ))
}

cor_total$p_fdr   <- round(p.adjust(cor_total$p_raw, method = "BH"), 4)
cor_total$p_raw   <- round(cor_total$p_raw, 4)
cor_total$Sig_raw <- ifelse(cor_total$p_raw < .001, "***",
                     ifelse(cor_total$p_raw < .01,  "**",
                     ifelse(cor_total$p_raw < .05,  "*",
                     ifelse(cor_total$p_raw < .10,  "t", "ns"))))
cor_total$Sig_fdr <- ifelse(cor_total$p_fdr < .001, "***",
                     ifelse(cor_total$p_fdr < .01,  "**",
                     ifelse(cor_total$p_fdr < .05,  "*",
                     ifelse(cor_total$p_fdr < .10,  "t", "ns"))))

print(as.data.frame(cor_total))
cat("\n")

# 2b: Kruskal-Wallis by IoT tertile group
cat("Kruskal-Wallis by IoT tertile group:\n\n")

for (i in seq_along(pa_vars)) {
  df_sub <- df %>% filter(!is.na(.data[[pa_vars[i]]]), !is.na(iot_category))
  n_total <- nrow(df_sub)
  k <- length(levels(df_sub$iot_category))

  df_sub %>%
    group_by(iot_category) %>%
    summarise(
      n      = n(),
      Median = round(median(.data[[pa_vars[i]]], na.rm = TRUE), 1),
      IQR    = round(IQR(.data[[pa_vars[i]]],   na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    print()

  kw     <- kruskal.test(as.formula(paste(pa_vars[i], "~ iot_category")),
                         data = df_sub)
  # Eta-squared based on H (Tomczak & Tomczak, 2014); negative values set to 0
  eta2_H <- max(0, (kw$statistic - k + 1) / (n_total - k))
  sig    <- ifelse(kw$p.value < .001, "***",
             ifelse(kw$p.value < .01,  "**",
             ifelse(kw$p.value < .05,  "*",
             ifelse(kw$p.value < .10,  "t", "ns"))))

  cat(sprintf("%s: H(%d) = %.3f, p = %.4f %s, eta2_H = %.3f\n\n",
              pa_labels[i], kw$parameter, kw$statistic,
              kw$p.value, sig, eta2_H))

  if (kw$p.value < .10) {
    cat("Dunn post-hoc (Bonferroni):\n")
    dunn.test::dunn.test(df_sub[[pa_vars[i]]], df_sub$iot_category,
                         method = "bonferroni", alpha = 0.05,
                         list = FALSE, table = TRUE)
    cat("\n")
  }
}


# ----------------------------------------------------------------------------
# 3. IoT subscales vs PA breakdown
# ----------------------------------------------------------------------------

cat("--- Section 3: IoT subscales vs PA sub-components (exploratory) ---\n")
cat("BH FDR correction applied within each predictor (3 PA outcomes per predictor).\n\n")

subscales       <- c("iot_engagement", "iot_selfmon")
subscale_labels <- c("Information-seeking use", "Self-monitoring use")

# 3a: Spearman correlations
cor_subscale <- data.frame()

for (j in seq_along(subscales)) {
  for (i in seq_along(pa_vars)) {
    idx <- !is.na(df[[subscales[j]]]) & !is.na(df[[pa_vars[i]]])
    ct  <- cor.test(df[[subscales[j]]][idx], df[[pa_vars[i]]][idx],
                    method = "spearman", exact = FALSE)
    cor_subscale <- bind_rows(cor_subscale, data.frame(
      Predictor = subscale_labels[j],
      Outcome   = pa_labels[i],
      rho       = round(unname(ct$estimate), 3),
      p_raw     = ct$p.value,
      n         = sum(idx)
    ))
  }
}

# BH FDR applied WITHIN each predictor (3 PA outcomes per predictor),
# matching the per-predictor family used in Scripts 2 and 3.
cor_subscale$p_fdr <- round(
  ave(cor_subscale$p_raw, cor_subscale$Predictor,
      FUN = function(p) p.adjust(p, method = "BH")), 4)
cor_subscale$p_raw <- round(cor_subscale$p_raw, 4)
cor_subscale$Sig_raw <- ifelse(cor_subscale$p_raw < .001, "***",
                        ifelse(cor_subscale$p_raw < .01,  "**",
                        ifelse(cor_subscale$p_raw < .05,  "*",
                        ifelse(cor_subscale$p_raw < .10,  "t", "ns"))))
cor_subscale$Sig_fdr <- ifelse(cor_subscale$p_fdr < .001, "***",
                        ifelse(cor_subscale$p_fdr < .01,  "**",
                        ifelse(cor_subscale$p_fdr < .05,  "*",
                        ifelse(cor_subscale$p_fdr < .10,  "t", "ns"))))

print(as.data.frame(cor_subscale))
cat("\n")

# 3b: Kruskal-Wallis by subscale tertile groups
for (group_v in c("eng_group", "selfmon_group")) {
  g_label <- ifelse(group_v == "eng_group",
                    "Information-seeking use tertile",
                    "Self-monitoring use tertile")
  cat(sprintf("Kruskal-Wallis: %s vs PA sub-components:\n\n", g_label))

  for (i in seq_along(pa_vars)) {
    df_sub  <- df %>% filter(!is.na(.data[[pa_vars[i]]]),
                              !is.na(.data[[group_v]]))
    n_total <- nrow(df_sub)
    k       <- length(levels(df_sub[[group_v]]))

    df_sub %>%
      group_by(.data[[group_v]]) %>%
      summarise(
        n      = n(),
        Median = round(median(.data[[pa_vars[i]]], na.rm = TRUE), 1),
        IQR    = round(IQR(.data[[pa_vars[i]]],   na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      print()

    kw     <- kruskal.test(as.formula(paste(pa_vars[i], "~", group_v)),
                           data = df_sub)
    eta2_H <- max(0, (kw$statistic - k + 1) / (n_total - k))
    sig    <- ifelse(kw$p.value < .001, "***",
               ifelse(kw$p.value < .01,  "**",
               ifelse(kw$p.value < .05,  "*",
               ifelse(kw$p.value < .10,  "t", "ns"))))

    cat(sprintf("%s: H(%d) = %.3f, p = %.4f %s, eta2_H = %.3f\n\n",
                pa_labels[i], kw$parameter, kw$statistic,
                kw$p.value, sig, eta2_H))

    if (kw$p.value < .10) {
      cat("Dunn post-hoc (Bonferroni):\n")
      dunn.test::dunn.test(df_sub[[pa_vars[i]]], df_sub[[group_v]],
                           method = "bonferroni", alpha = 0.05,
                           list = FALSE, table = TRUE)
      cat("\n")
    }
  }
}


# ----------------------------------------------------------------------------
# Export
# ----------------------------------------------------------------------------

write_csv(cor_total,    "results/Script6_iot_total_vs_PA.csv")
write_csv(cor_subscale, "results/Script6_subscales_vs_PA.csv")

cat("Saved: results/Script6_iot_total_vs_PA.csv\n")
cat("Saved: results/Script6_subscales_vs_PA.csv\n")
cat("\nScript 6 (supplementary) complete.\n")

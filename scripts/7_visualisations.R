# ============================================================================
# PCOS IoT Study - Script 7: Figures for Manuscript
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Produce all figures for journal manuscript submission.
#          Values are read from the result files written by Scripts 3-5, or
#          computed from cleaned_data.rds. Nothing is transcribed by hand.
#
# Manuscript figures:
#   Figure 1 - Conceptual framework                        [PowerPoint - no R code]
#   Figure 2 - Digital health technology use by item (grouped bar)
#   Figure 3 - Mediation model: use -> EDE-Q7 -> PCOSQOL   [PowerPoint - no R code]
#   Figure 4 - Forest plot: indirect effects across all mediator models
#   Figure 5 - Conditional indirect effects by age (moderated mediation)
#
# Additional figures (not submitted with the manuscript; retained for the thesis):
#   Figure 6 - Health outcomes by use tertile group (raincloud plots)
#   Figure 7 - Correlation heatmap: predictors x outcomes (3 x 10)
#   Figure 8 - Mediation path diagram (R rendering of manuscript Figure 3)
#   Figure 9 - Coexisting health conditions reported alongside PCOS
#
# All figures saved as PNG (300 DPI), PDF, and TIFF (600 DPI) to figures/.
#
# Scale directions (critical - never reverse):
#   PCOSQOL  - higher = BETTER quality of life
#   EDE-Q7   - higher = MORE disordered eating
#   ISI      - higher = WORSE insomnia
#   SWEMWBS  - higher = BETTER mental wellbeing
#
# Input:  data-processed/cleaned_data.rds                 (Script 1)
#         results/Script3_KW_results.csv                  (Script 3)
#         results/Script3_posthoc.csv                     (Script 3)
#         results/Script4_indirect_effects.csv            (Script 4)
#         results/Script4_primary_paths.csv               (Script 4)
#         results/Script5_conditional_indirect_age.csv    (Script 5)
#         results/Script5_moderation_a3.csv               (Script 5)
#
# Run order: Script 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> THIS SCRIPT
# ============================================================================

library(tidyverse)
library(ggdist)
library(patchwork)
library(scales)
library(readxl)
library(ragg)
library(here)

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

dir.create("figures", showWarnings = FALSE)

# Raw survey export. Only Figure 9 needs it, because the coexisting-conditions
# free-text item is not carried into cleaned_data.rds. Set once here so the
# path is edited in a single place when the script is run elsewhere.
RAW_DATA <- here::here("raw_data", "Digital_Health_Tech_PMOS_UK92_deidentified.xlsx")

# Fails loudly rather than silently falling back to stale values.
read_results <- function(file) {
  path <- file.path("results", file)
  if (!file.exists(path)) {
    stop("Missing ", path,
         "\n  Run the script that produces it before running Script 7.",
         call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE)
}


# ---- Shared colour palette (consistent across all figures) -----------------

COL_TEAL  <- "#028090"
COL_CORAL <- "#E05C5C"
COL_SLATE <- "#50808E"
COL_GOLD  <- "#D4A843"
COL_GREY  <- "#7A8C8E"
COL_DARK  <- "#1A2B2C"
COL_LIGHT <- "#EBF5F7"

# Sampled from the PowerPoint figures so R figures match Figures 1 and 3
COL_RED   <- "#B03020"   # mediator role
COL_GREEN <- "#2C5F2D"   # outcome role

# Use tertile group colours
# Low = light teal (lower risk), High = coral (draws attention)
COL_LOW  <- "#BDD5D8"
COL_MED  <- "#028090"
COL_HIGH <- "#E05C5C"

TERTILE_COLS <- c("Low"    = COL_LOW,
                  "Medium" = COL_MED,
                  "High"   = COL_HIGH)


# ---- Shared theme ----------------------------------------------------------

theme_pcos <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(colour = COL_DARK),
      plot.title       = element_text(face = "bold", size = base_size + 1,
                                      colour = COL_TEAL, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 2, colour = COL_GREY,
                                      hjust = 0, margin = margin(b = 8)),
      plot.caption     = element_text(size = base_size - 3, colour = COL_GREY,
                                      hjust = 0, face = "italic"),
      axis.title       = element_text(size = base_size - 1),
      axis.text        = element_text(size = base_size - 2),
      panel.grid.major = element_line(colour = "#E8ECEC", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      strip.text       = element_text(face = "bold", colour = COL_TEAL,
                                      size = base_size - 1),
      strip.background = element_rect(fill = COL_LIGHT, colour = NA),
      plot.margin      = margin(14, 14, 10, 14)
    )
}

save_fig <- function(name, plot = last_plot(), width = 10, height = 7) {
  ggsave(paste0("figures/", name, ".png"), plot,
         width = width, height = height, dpi = 300, bg = "white")
  ggsave(paste0("figures/", name, ".pdf"), plot,
         width = width, height = height, bg = "white")
  ggsave(paste0("figures/", name, ".tiff"), plot,
         width = width, height = height, dpi = 600, bg = "white",
         compression = "lzw")
  message("Saved: figures/", name)
}

# p values without the leading zero, matching the manuscript
fmt_p <- function(p, digits = 3) {
  ifelse(p < 0.001, "< .001",
         paste("=", sub("^0", "", sprintf(paste0("%.", digits, "f"), p))))
}

stars <- function(p) {
  ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
}

# Cohen (1988) bands for eta-squared (eta2_H): >= .01 small, >= .06 medium, >= .14 large
eta_band <- function(e) {
  ifelse(e >= .14, "large",
         ifelse(e >= .06, "medium",
                ifelse(e >= .01, "small", "negligible")))
}


# ############################################################################
# PART 1 - MANUSCRIPT FIGURES
# ############################################################################


# ============================================================================
# FIGURE 1 - Conceptual framework guiding the analyses
# ============================================================================
# Drawn in PowerPoint, not in R. Placeholder so the numbering in this script
# matches the numbering in the manuscript.
# Source file: figures/Figure1_Conceptual_Framework.png
# ============================================================================


# ============================================================================
# FIGURE 2 - Reported frequency of digital health technology use by item
# ============================================================================
# Frequency of use for each of the 10 items, grouped by subscale and sorted by
# mean score within subscale so the most-used items appear first.
# Item means and response percentages are computed from cleaned_data.rds,
# reproducing Script 2 Section 4.
# No title, subtitle or caption: the legend sits in the manuscript text.
# ============================================================================

message("Drawing Figure 2: Digital health technology use profile")

item_labels <- c(
  iot_1  = "Browse websites",
  iot_2  = "Online forums",
  iot_3  = "Search on social media",
  iot_4  = "Share experiences\non social media",
  iot_5  = "Use AI / LLMs for advice",
  iot_6  = "Monitor symptoms",
  iot_7  = "Fitness / activity tracker",
  iot_8  = "Monitor sleep",
  iot_9  = "Monitor nutrition",
  iot_10 = "Monitor physical activity"
)

response_levels <- c("Never", "Seldom", "Often", "Regularly")

# One row per item x response, with the percentage of respondents choosing it
freq_data <- bind_rows(imap(item_labels, function(lab, var) {
  x   <- df[[var]]
  tab <- table(factor(x, levels = 0:3))
  tibble(
    var      = var,
    item     = lab,
    subscale = if (as.integer(sub("iot_", "", var)) <= 5)
      "Information seeking (items 1-5)" else
        "Self-monitoring (items 6-10)",
    mean_score = mean(x, na.rm = TRUE),
    response   = response_levels,
    pct        = round(100 * as.numeric(tab) / sum(tab), 1)
  )
}))

# Levels set explicitly; otherwise R sorts them alphabetically and the sort
# documented in the figure legend is not applied.
item_order <- freq_data %>%
  distinct(item, subscale, mean_score) %>%
  arrange(subscale, desc(mean_score)) %>%
  pull(item)

freq_data <- freq_data %>%
  mutate(
    response = factor(response, levels = response_levels),
    item     = factor(item, levels = rev(item_order))
  )

response_cols <- c("Never"     = "#D6E8EA",
                   "Seldom"    = "#80B4BB",
                   "Often"     = "#028090",
                   "Regularly" = "#014D56")

fig2 <- ggplot(freq_data, aes(x = pct, y = item, fill = response)) +
  geom_col(position = position_dodge(width = 0.8),
           width    = 0.75) +
  facet_wrap(~ subscale, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = response_cols,
                    name   = "Frequency of use",
                    guide  = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "",
    subtitle = "",
    x        = "Proportion of respondents (%)",
    y        = NULL,
    caption  = ""
  ) +
  theme_pcos(11) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 14),
    legend.text     = element_text(size = 13),
    strip.text      = element_text(size = 15, face = "bold", colour = COL_DARK),
    axis.text.y     = element_text(size = 15),
    axis.text.x     = element_text(size = 13),
    axis.title.x    = element_text(size = 14, margin = margin(t = 12))
  )

save_fig("Figure2_Digital_Health_Technologies_Use_Profile", fig2,
         width = 10, height = 11)


# ============================================================================
# FIGURE 3 - Mediation model: disordered eating mediates the association
#            between the use of digital health technologies and quality of life
# ============================================================================
# Drawn in PowerPoint, not in R, so that it matches the visual conventions of
# Figure 1 (box colours by role, brace for the indirect effect).
# The R rendering of the same model is retained below as Figure 8, and takes
# its coefficients from results/Script4_primary_paths.csv.
# Source file: figures/Figure3_Mediation_Model.png
# ============================================================================


# ============================================================================
# FIGURE 4 - Forest plot: indirect effects across all mediation models
# ============================================================================
# Indirect effect (B) and 95% Monte Carlo CI for each mediator, for the total
# use score, the information-seeking subscale, and the multiple-mediator model.
# Values read from results/Script4_indirect_effects.csv, where an effect is
# flagged significant when the interval excludes zero.
# No title, subtitle or caption: the legend sits in the manuscript text.
# ============================================================================

message("Drawing Figure 4: Forest plot of indirect effects")

indirect_effects <- read_results("Script4_indirect_effects.csv")

# Display labels and row order live here, not in Script 4, so the analysis
# script stays free of presentation decisions.
forest_labels <- c(
  ede_total        = "EDE-Q7\n(disordered eating)",
  ede_infoseek     = "EDE-Q7\n(information seeking as predictor)",
  ede_multi        = "EDE-Q7\n(multiple mediator model)",
  swemwbs_infoseek = "SWEMWBS\n(information seeking as predictor)",
  isi_total        = "ISI\n(insomnia severity)",
  swemwbs_total    = "SWEMWBS\n(mental wellbeing)",
  met_total        = "log(MET + 1)\n(physical activity)"
)

# label_x below is a fixed-length vector, so the rows must match one to one
stopifnot(setequal(indirect_effects$model_id, names(forest_labels)))

forest_dat <- indirect_effects %>%
  arrange(match(model_id, names(forest_labels))) %>%
  mutate(
    label   = forest_labels[model_id],
    colour  = ifelse(sig, COL_RED, COL_GREY),
    label   = factor(label, levels = rev(label)),
    # Horizontal position of the "B = ..." text only. Estimates close to zero
    # would otherwise print on top of the dashed reference line.
    # Text only; the diamonds and intervals are unaffected.
    label_x = B + c(0, 0, 0, 0, -0.20, -0.20, 0.18)
  )

fig4 <- ggplot(forest_dat,
               aes(x = B, y = label, colour = I(colour))) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.8) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi,
                    linewidth = I(ifelse(sig, 1.2, 0.8))),
                orientation = "y", width = 0.25) +
  geom_point(aes(size = I(ifelse(sig, 4.5, 3.5))), shape = 18) +
  # geom_text size is in mm, not points; 4.2 renders at about 12 pt.
  geom_text(aes(x = label_x, label = sprintf("B = %.2f", B)),
            nudge_y = 0.34, size = 4.2, fontface = "bold",
            colour = COL_DARK) +
  scale_x_continuous(limits = c(-3.0, 0.8),
                     breaks = seq(-3.0, 0.5, 0.5)) +
  labs(
    title    = "",
    subtitle = "",
    x        = "Indirect effect (unstandardised B)",
    y        = NULL,
    caption  = ""
  ) +
  theme_pcos(11) +
  theme(
    axis.text.y        = element_text(size = 14, lineheight = 1.1),
    axis.text.x        = element_text(size = 13),
    axis.title.x       = element_text(size = 14, margin = margin(t = 12)),
    panel.grid.major.y = element_line(colour = "#F0F4F4", linewidth = 0.4)
  )

save_fig("Figure4_Forest_Plot_Mediation", fig4, width = 10, height = 8)


# ============================================================================
# FIGURE 5 - Conditional indirect effects by age
# ============================================================================
# Indirect effect of digital health technology use -> EDE-Q7 -> PCOSQOL at
# three age levels (M-1SD, M, M+1SD).
# Values and the age levels shown on the x axis are read from
# results/Script5_conditional_indirect_age.csv.
# No title, subtitle or caption: the legend sits in the manuscript text.
# ============================================================================

message("Drawing Figure 5: Conditional indirect effects by age")

conditional_age <- read_results("Script5_conditional_indirect_age.csv")
moderation_a3   <- read_results("Script5_moderation_a3.csv")

# Quoted in the Results text and the Figure 5 legend
message(sprintf("  a3 interaction: B = %.4f, SE = %.4f, p %s",
                moderation_a3$B, moderation_a3$SE, fmt_p(moderation_a3$p)))

age_label_fmt <- c(low  = "Younger\n(-1 SD, ~%d years)",
                   mean = "Mean age\n(~%d years)",
                   high = "Older\n(+1 SD, ~%d years)")

modmed_dat <- conditional_age %>%
  arrange(match(age_level, c("low", "mean", "high"))) %>%
  mutate(label = sprintf(age_label_fmt[age_level], as.integer(age_value))) %>%
  mutate(label = factor(label, levels = label))

fig5 <- ggplot(modmed_dat,
               aes(x = label, y = B,
                   colour = sig, fill = sig)) +
  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.8) +
  # CI error bars
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.15, linewidth = 1.2) +
  # Point estimates
  geom_point(size = 6, shape = 21, stroke = 2) +
  # B value labels
  geom_text(aes(label = sprintf("B = %.2f", B),
                vjust = ifelse(B < 0, 2.5, -1.5)),
            nudge_x = 0.16, size = 4.2, fontface = "bold", colour = COL_DARK) +
  # 95% CI labels below the interval
  geom_text(aes(y = ci_lo - 0.18,
                label = sprintf("[%.2f, %.2f]", ci_lo, ci_hi)),
            size = 4, fontface = "italic") +
  # Trend line connecting the three points
  geom_line(aes(group = 1), colour = COL_TEAL,
            linewidth = 0.9, linetype = "solid") +
  
  scale_colour_manual(values = c("TRUE" = COL_RED, "FALSE" = COL_GREY)) +
  scale_fill_manual(  values = c("TRUE" = COL_RED, "FALSE" = COL_GREY)) +
  scale_y_continuous(limits = c(-2.7, 0.5), breaks = seq(-2.5, 0.5, 0.5)) +
  labs(
    title    = "",
    subtitle = "",
    x        = NULL,
    y        = "Indirect effect (B)",
    caption  = ""
  ) +
  theme_pcos(12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14, face = "bold"),
    axis.text.y     = element_text(size = 13),
    axis.title.y    = element_text(size = 14, margin = margin(r = 12))
  )

save_fig("Figure5_Moderated_Mediation", fig5, width = 10, height = 7)


# ############################################################################
# PART 2 - ADDITIONAL FIGURES
# Not submitted with the manuscript. Retained for the thesis.
# These keep their titles and captions inside the plot, since they are not
# accompanied by a journal figure legend.
# ############################################################################


# ============================================================================
# FIGURE 6 - Health outcomes by use tertile group (raincloud plots)
# ============================================================================
# Distribution, individual data points, and group summary for PCOSQOL and
# EDE-Q7, the two outcomes with significant Kruskal-Wallis results.
# Test statistics read from results/Script3_KW_results.csv and
# results/Script3_posthoc.csv.
# PCOSQOL: higher = better QoL. EDE-Q7: higher = more disordered eating.
# ============================================================================

message("Drawing Figure 6: Raincloud plots")

kw      <- read_results("Script3_KW_results.csv")
posthoc <- read_results("Script3_posthoc.csv")

# Kruskal-Wallis annotation for one outcome
kw_label <- function(outcome) {
  r <- kw[kw$Outcome == outcome, ][1, ]
  sprintf("H(%d) = %.2f, p %s%s\n\u03b7\u00b2H = %s (%s effect)",
          as.integer(r$H_df), r$H_stat, fmt_p(r$p_KW), stars(r$p_KW),
          sub("^0", "", sprintf("%.3f", r$eta2_H)),
          eta_band(r$eta2_H))
}

# Dunn pairwise p for one outcome. Matched on both group names so the label
# does not depend on how dunn.test formats the separator.
dunn_label <- function(outcome, g1, g2) {
  d   <- posthoc[posthoc$Outcome == outcome, ]
  hit <- grepl(g1, d$Comparison, fixed = TRUE) &
    grepl(g2, d$Comparison, fixed = TRUE)
  stopifnot(sum(hit) == 1)
  p <- d$p_adj[hit]
  sprintf("%s vs %s: p %s%s", g1, g2, fmt_p(p), stars(p))
}

group_n <- table(df$iot_category)

df_plot <- df %>%
  mutate(
    iot_group = factor(iot_category,
                       levels = c("Low", "Medium", "High"),
                       labels = c(sprintf("Low use\n(n=%d)",    as.integer(group_n[["Low"]])),
                                  sprintf("Medium use\n(n=%d)", as.integer(group_n[["Medium"]])),
                                  sprintf("High use\n(n=%d)",   as.integer(group_n[["High"]]))))
  )

# Panel A: PCOSQOL (higher = better QoL)
p_pcosqol <- ggplot(df_plot %>% filter(!is.na(pcosqol_total)),
                    aes(x = iot_group, y = pcosqol_total,
                        fill = iot_category, colour = iot_category)) +
  ggdist::stat_halfeye(
    adjust = 0.7, width = 0.5, .width = 0,
    justification = -0.25, point_colour = NA, alpha = 0.7
  ) +
  geom_boxplot(
    width = 0.18, outlier.shape = NA,
    colour = COL_DARK, fill = "white", alpha = 0.9, linewidth = 0.6
  ) +
  ggdist::stat_dots(
    side = "left", dotsize = 0.9,
    binwidth = unit(0.025, "npc"),
    colour = NA, alpha = 0.55
  ) +
  scale_fill_manual(values   = TERTILE_COLS, guide = "none") +
  scale_colour_manual(values = TERTILE_COLS, guide = "none") +
  annotate("text", x = 2, y = 230,
           label = kw_label("pcosqol_total"),
           colour = COL_CORAL, size = 3, fontface = "italic") +
  annotate("segment", x = 1, xend = 3, y = 208, yend = 208,
           colour = COL_CORAL, linewidth = 0.6) +
  annotate("text", x = 2, y = 214,
           label = dunn_label("pcosqol_total", "High", "Low"),
           colour = COL_CORAL, size = 2.8, fontface = "italic") +
  labs(
    title    = "A.  PCOSQOL total",
    subtitle = "(higher score = better quality of life)",
    x        = NULL,
    y        = "PCOSQOL score (35-245)"
  ) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"))

# Panel B: EDE-Q7 (higher = more concerns)
p_edq7 <- ggplot(df_plot %>% filter(!is.na(ede_q7_global)),
                 aes(x = iot_group, y = ede_q7_global,
                     fill = iot_category, colour = iot_category)) +
  ggdist::stat_halfeye(
    adjust = 0.7, width = 0.5, .width = 0,
    justification = -0.25, point_colour = NA, alpha = 0.7
  ) +
  geom_boxplot(
    width = 0.18, outlier.shape = NA,
    colour = COL_DARK, fill = "white", alpha = 0.9, linewidth = 0.6
  ) +
  ggdist::stat_dots(
    side = "left", dotsize = 0.9,
    binwidth = unit(0.025, "npc"),
    colour = NA, alpha = 0.55
  ) +
  # Clinical threshold (Bang et al., 2023)
  geom_hline(yintercept = 3.64, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.7) +
  annotate("text", x = 0.55, y = 3.72,
           label = "Clinical threshold = 3.64",
           colour = COL_GREY, size = 2.7, fontface = "italic", hjust = 0) +
  scale_fill_manual(values   = TERTILE_COLS, guide = "none") +
  scale_colour_manual(values = TERTILE_COLS, guide = "none") +
  annotate("text", x = 2, y = 7.05,
           label = kw_label("ede_q7_global"),
           colour = COL_TEAL, size = 3, fontface = "italic") +
  annotate("segment", x = 1, xend = 3, y = 6.25, yend = 6.25,
           colour = COL_TEAL, linewidth = 0.6) +
  annotate("text", x = 2, y = 6.6,
           label = dunn_label("ede_q7_global", "High", "Low"),
           colour = COL_TEAL, size = 2.8, fontface = "italic") +
  labs(
    title    = "B.  EDE-Q7 global mean",
    subtitle = "(higher score = more disordered eating)",
    x        = NULL,
    y        = "EDE-Q7 global mean (0-6)"
  ) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"))

fig6 <- (p_pcosqol | p_edq7) +
  plot_annotation(
    title   = "Figure 6 - Health outcomes by digital health technology use group",
    subtitle = "Raincloud plots: half-violin density (right) + boxplot + individual data points (left). Groups defined by tertile split.",
    caption  = "PCOSQOL: higher = better QoL. EDE-Q7: higher = more disordered eating.\nISI, SWEMWBS, and MET were not significantly different across groups (all p > .15).\nDunn's pairwise tests with Bonferroni correction. Dashed line = EDE-Q7 clinical threshold (Bang et al., 2023).",
    theme    = theme(
      plot.title    = element_text(face = "bold", colour = COL_TEAL, size = 13),
      plot.subtitle = element_text(colour = COL_GREY, size = 9),
      plot.caption  = element_text(colour = COL_GREY, size = 8, face = "italic"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

save_fig("Figure6_Outcomes_by_Use_Group", fig6, width = 12, height = 7)


# ============================================================================
# FIGURE 7 - Correlation heatmap: predictors x outcomes (3 x 10)
# ============================================================================
# Columns: total use score, information seeking, self-monitoring.
# Rows: PCOSQOL total + 4 subscales, EDE-Q7, ISI, SWEMWBS, MET, sitting.
# Coral = negative rho (use up = outcome down); teal = positive.
# Bold rho in significant cells, faded in non-significant.
# All values computed from the data.
# ============================================================================

message("Drawing Figure 7: Correlation heatmap")

iot_vars <- c("iot_total", "iot_engagement", "iot_selfmon")

iot_labels_map <- c(
  "iot_total"      = "Total use",
  "iot_engagement" = "Information seeking",
  "iot_selfmon"    = "Self-monitoring"
)

outcome_vars <- c(
  "pcosqol_total",
  "pcosqol_mood",
  "pcosqol_impact",
  "pcosqol_hirsutism",
  "pcosqol_infertility",
  "ede_q7_global",
  "isi_total",
  "swemwbs_total",
  "MET_total",
  "sitting_hours"
)

# Arrows communicate scoring direction
outcome_labels_map <- c(
  "pcosqol_total"      = "PCOSQOL \u2014 Total  (\u2191 = better QoL)",
  "pcosqol_mood"       = "  \u2514 Mood / Emotional",
  "pcosqol_impact"     = "  \u2514 Symptom Impact",
  "pcosqol_hirsutism"  = "  \u2514 Body Hair (Hirsutism)",
  "pcosqol_infertility"= "  \u2514 Infertility",
  "ede_q7_global"      = "EDE-Q7  (\u2191 = more disordered eating)",
  "isi_total"          = "ISI  (\u2191 = worse insomnia)",
  "swemwbs_total"      = "SWEMWBS  (\u2191 = better wellbeing)",
  "MET_total"          = "Physical activity  (MET-min/week)",
  "sitting_hours"      = "Sitting time  (hours/weekday)"
)

rho_mat <- matrix(NA, nrow = length(outcome_vars), ncol = length(iot_vars),
                  dimnames = list(outcome_vars, iot_vars))
p_mat   <- rho_mat

for (ov in outcome_vars) {
  for (iv in iot_vars) {
    idx <- complete.cases(df[[ov]], df[[iv]])
    ct  <- suppressWarnings(
      cor.test(df[[ov]][idx], df[[iv]][idx],
               method = "spearman", exact = FALSE)
    )
    rho_mat[ov, iv] <- round(ct$estimate, 2)
    p_mat[ov, iv]   <- ct$p.value
  }
}

# n for the subtitle, computed rather than fixed
n_met     <- sum(!is.na(df$MET_total))
n_sitting <- sum(!is.na(df$sitting_hours))

plot_dat <- expand.grid(outcome = outcome_vars, predictor = iot_vars,
                        stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    rho     = rho_mat[outcome, predictor],
    p_value = p_mat[outcome, predictor],
    is_sig  = p_value < .05,
    cell_label = ifelse(is_sig,
                        paste0(sprintf("%.2f", rho), stars(p_value)),
                        sprintf("%.2f", rho)),
    outcome_label   = outcome_labels_map[outcome],
    predictor_label = iot_labels_map[predictor]
  ) %>%
  ungroup() %>%
  mutate(
    outcome_label   = factor(outcome_label,
                             levels = rev(outcome_labels_map)),
    predictor_label = factor(predictor_label,
                             levels = iot_labels_map)
  )

fig7 <- ggplot(plot_dat, aes(x = predictor_label, y = outcome_label,
                             fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(data = filter(plot_dat,  is_sig),
            aes(label = cell_label),
            size = 3.3, fontface = "bold", colour = COL_DARK) +
  geom_text(data = filter(plot_dat, !is_sig),
            aes(label = cell_label),
            size = 3.0, fontface = "plain", colour = "#AABFC2") +
  # Separates PCOSQOL rows from the other outcomes
  geom_hline(yintercept = 5.5, linetype = "dashed", colour = COL_GREY, linewidth = 0.6) +
  scale_fill_gradientn(
    colours = c(COL_CORAL, "#F5CECE", "white", "#C5E4E9", COL_TEAL),
    values  = scales::rescale(c(-0.65, -0.25, 0, 0.25, 0.65)),
    limits  = c(-0.65, 0.65),
    oob     = scales::squish,
    name    = "Spearman \u03c1",
    guide   = guide_colorbar(barwidth = 0.8, barheight = 11,
                             title.position = "top", title.hjust = 0.5)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Figure 7 - Spearman's rank-order correlations:\ndigital health technology use and health outcomes",
    subtitle = sprintf(paste0("Bold = p < .05 (uncorrected; see Tables 3 and 4 for FDR-corrected values).\nDashed line separates PCOSQOL subscales (above) from other outcomes (below).\n",
                              "Coral = negative correlation; teal = positive. n = %d (MET: n = %d; sitting: n = %d)."),
                       nrow(df), as.integer(n_met), as.integer(n_sitting)),
    caption  = "PCOSQOL: higher = better QoL; negative rho indicates worse QoL with higher use.\n* p < .05  ** p < .01  *** p < .001",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", colour = COL_TEAL,
                                    size = 12, hjust = 0),
    plot.subtitle    = element_text(colour = COL_GREY, size = 8.5,
                                    hjust = 0, lineheight = 1.4,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(colour = COL_GREY, size = 8,
                                    hjust = 0, face = "italic"),
    axis.text.x      = element_text(size = 10, face = "bold",
                                    colour = COL_DARK, angle = 0, hjust = 0.5),
    axis.text.y      = element_text(size = 9, colour = COL_DARK, hjust = 1),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position  = "right",
    plot.margin      = margin(14, 14, 12, 14)
  )

save_fig("Figure7_Correlation_Heatmap", fig7, width = 9, height = 10)


# ============================================================================
# FIGURE 8 - Mediation path diagram (R rendering of manuscript Figure 3)
# ============================================================================
# The version submitted with the manuscript is the PowerPoint rendering; this
# one is kept as an alternative.
# Coefficients read from results/Script4_primary_paths.csv and
# results/Script4_indirect_effects.csv.
# ============================================================================

message("Drawing Figure 8: Mediation path diagram")

primary_paths <- read_results("Script4_primary_paths.csv")

path_of <- function(name) primary_paths[primary_paths$path == name, ][1, ]
p_a     <- path_of("a: IoT -> EDE-Q7")
p_b     <- path_of("b: EDE-Q7 -> PCOSQOL")
p_c     <- path_of("c': direct effect")
p_tot   <- path_of("total effect (c)")
ind     <- indirect_effects[indirect_effects$model_id == "ede_total", ][1, ]

EXPOSURE_LAB <- "Digital health\ntechnology use"

NODE_HW <- 0.11
NODE_HH <- 0.075
Y_LINE  <- 0.50

nodes <- data.frame(
  label = c(paste0(EXPOSURE_LAB, "\n(Predictor)"),
            "Disordered Eating\n(EDE-Q7)\n(Mediator)",
            "PCOSQOL\n(Outcome)"),
  x     = c(0.14, 0.50, 0.86),
  y     = Y_LINE,
  fill  = c(COL_LIGHT, "#FFF0EE", COL_LIGHT),
  col   = c(COL_TEAL,  COL_CORAL, COL_TEAL)
)

# Arrows end at the box edges. Nodes are drawn before the arrows so the
# arrowheads are never covered.
edges <- data.frame(
  x    = c(0.14 + NODE_HW, 0.50 + NODE_HW),
  xend = c(0.50 - NODE_HW, 0.86 - NODE_HW),
  y    = Y_LINE, yend = Y_LINE
)

# Direct path c': arc from the base of the predictor box to the base of the
# outcome box, so the arrowhead touches the box
arc <- data.frame(t = seq(0, 1, length.out = 150)) %>%
  mutate(
    x = 0.14 + 0.72 * t,
    y = (Y_LINE - NODE_HH) - 0.17 * (1 - 4 * (t - 0.5)^2)
  )

fig8 <- ggplot() +
  geom_rect(data = nodes,
            aes(xmin = x - NODE_HW, xmax = x + NODE_HW,
                ymin = y - NODE_HH, ymax = y + NODE_HH,
                fill = I(fill), colour = I(col)),
            linewidth = 1.4) +
  geom_segment(data = edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = COL_CORAL, linewidth = 1.4,
               arrow = arrow(length = unit(9, "pt"), type = "closed")) +
  geom_path(data = arc, aes(x = x, y = y),
            colour = COL_TEAL, linewidth = 1.2,
            arrow = arrow(length = unit(9, "pt"), type = "closed")) +
  # Node text last, so it sits above everything
  geom_text(data = nodes,
            aes(x = x, y = y, label = label),
            size = 3.4, fontface = "bold", lineheight = 0.95,
            colour = COL_DARK) +
  annotate("label", x = 0.32, y = 0.71,
           label = sprintf("path a\nB = %+.2f, SE = %.2f\np %s %s",
                           p_a$B, p_a$SE, fmt_p(p_a$p), stars(p_a$p)),
           colour = COL_CORAL, fill = "white", size = 2.9,
           fontface = "bold", lineheight = 1.05) +
  annotate("label", x = 0.68, y = 0.71,
           label = sprintf("path b\nB = %.2f, SE = %.2f\np %s %s",
                           p_b$B, p_b$SE, fmt_p(p_b$p), stars(p_b$p)),
           colour = COL_CORAL, fill = "white", size = 2.9,
           fontface = "bold", lineheight = 1.05) +
  annotate("label", x = 0.50, y = 0.17,
           label = sprintf("path c'  (direct effect)\nB = %.2f, SE = %.2f,  p %s %s",
                           p_c$B, p_c$SE, fmt_p(p_c$p), stars(p_c$p)),
           colour = COL_TEAL, fill = "white", size = 2.9,
           fontface = "bold", lineheight = 1.05) +
  annotate("label", x = 0.50, y = 0.90,
           label = sprintf("Indirect effect (a x b): B = %.2f  [95%% Monte Carlo CI: %.2f, %.2f]",
                           ind$B, ind$ci_lo, ind$ci_hi),
           colour = COL_CORAL, fill = "#FFF0EE", size = 3.1,
           fontface = "bold") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.05, 1.0), clip = "off") +
  labs(
    title    = "Figure 8 - Mediation model: disordered eating mediates the association between digital health technology use and QoL",
    subtitle = "MLR-SEM with FIML. Covariates: age, years diagnosed, coexisting health condition count. n = 89 (2 cases excluded due to missing covariate data).",
    caption  = sprintf(paste0("Total effect (digital health technology use -> PCOSQOL, adjusted for the same covariates): B = %.2f, SE = %.2f, p %s.",
                              "\nPartial mediation: direct effect remains significant after introducing the mediator.",
                              "\nHigher PCOSQOL = better QoL; higher EDE-Q7 = more disordered eating."),
                       p_tot$B, p_tot$SE, fmt_p(p_tot$p))
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", colour = COL_TEAL,
                                   size = 12, margin = margin(b = 4)),
    plot.subtitle   = element_text(colour = COL_GREY, size = 9,
                                   margin = margin(b = 8)),
    plot.caption    = element_text(colour = COL_GREY, size = 8, face = "italic"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(15, 20, 12, 20)
  )

save_fig("Figure8_Mediation_Diagram", fig8, width = 12, height = 6.5)


# ============================================================================
# FIGURE 9 - Coexisting health conditions reported alongside PCOS
# ============================================================================
# Two panels:
#   A - number of coexisting conditions per respondent (None / 1 / 2 / 3+)
#   B - prevalence of each specific condition reported
# The conditions sit in a free-text survey item that is not carried into
# cleaned_data.rds, so this figure reads the raw export (RAW_DATA, set at the
# top of this script) and counts them case-insensitively. Unlike the Script 1
# covariate (the 18 listed options only), these counts include conditions
# typed under "Other" (for example, endometriosis and hypothyroidism).
#
# Denominator is the respondents who disclosed their conditions, matching
# Script 2 and Table 1. Those who declined are unknown, not negative.
# ============================================================================

message("Drawing Figure 9: Coexisting health conditions")

raw <- read_excel(RAW_DATA, sheet = "Full with IPAQ-SF", na = "")

# Located by question text rather than column position, so a change to the
# export layout fails here instead of silently plotting the wrong column.
cond_col <- grep("^In addition to PCOS", names(raw))
stopifnot(length(cond_col) == 1)
conditions_raw <- raw[[cond_col]]

# Panel B - specific conditions (search term -> display label)
condition_terms <- c(
  "Anxiety"              = "anxiety",
  "Depression"           = "depression",
  "Insulin resistance"   = "insulin resistance",
  "Asthma"               = "asthma",
  "High blood pressure"  = "high blood pressure",
  "High LDL cholesterol" = "high ldl",
  "Type 2 diabetes"      = "type 2 diabetes",
  "Chronic pain"         = "chronic pain",
  "Insomnia"             = "insomnia",
  "Pre-diabetes"         = "pre-diabetes",
  "Endometriosis"        = "endometriosis",
  "Hypothyroidism"       = "hypothyroid"
)

# Number of conditions per respondent; 'prefer not' treated as missing
count_conditions <- function(x) {
  if (str_detect(coalesce(x, ""), regex("prefer not", ignore_case = TRUE))) return(NA_integer_)
  if (is.na(x) || str_trim(x) == "") return(0L)
  parts <- str_trim(str_split(x, ";")[[1]])
  parts <- parts[parts != "" & !str_detect(parts, regex("^none$", ignore_case = TRUE))]
  length(parts)
}
n_conditions <- map_int(conditions_raw, count_conditions)

n_total     <- length(n_conditions)
n_disclosed <- sum(!is.na(n_conditions))
cat("Respondents disclosing coexisting conditions: n =", n_disclosed,
    "of", n_total, "\n")

cond_dat <- tibble(
  label = names(condition_terms),
  n     = map_int(condition_terms,
                  ~ sum(str_detect(coalesce(conditions_raw, ""),
                                   regex(.x, ignore_case = TRUE))))
) %>%
  mutate(
    pct   = 100 * n / n_disclosed,
    lab   = sprintf("%d (%.1f%%)", n, pct),
    label = factor(label, levels = rev(names(condition_terms)))
  )

count_dat <- tibble(
  label = c("None", "1 condition", "2 conditions", "3+ conditions"),
  n     = c(sum(n_conditions == 0, na.rm = TRUE),
            sum(n_conditions == 1, na.rm = TRUE),
            sum(n_conditions == 2, na.rm = TRUE),
            sum(n_conditions >= 3, na.rm = TRUE))
) %>%
  mutate(
    pct   = 100 * n / n_disclosed,
    lab   = sprintf("%d (%.1f%%)", n, pct),
    label = factor(label, levels = rev(label))
  )

p_count <- ggplot(count_dat, aes(x = pct, y = label)) +
  geom_col(fill = COL_TEAL, width = 0.68) +
  geom_text(aes(label = lab), hjust = -0.1, size = 3.2, colour = COL_DARK) +
  scale_x_continuous(limits = c(0, 55), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Panel a) - Number of coexisting health conditions",
       x = "Respondents (%)", y = NULL) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"),
        panel.grid.major.y = element_blank())

p_cond <- ggplot(cond_dat, aes(x = pct, y = label)) +
  geom_col(fill = COL_TEAL, width = 0.68) +
  geom_text(aes(label = lab), hjust = -0.1, size = 3.2, colour = COL_DARK) +
  scale_x_continuous(limits = c(0, 55), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Panel b) - Specific health conditions reported",
       x = "Respondents (%)", y = NULL) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"),
        panel.grid.major.y = element_blank())

fig9 <- (p_count / p_cond) +
  plot_layout(heights = c(4, 12)) +
  plot_annotation(
    title   = "Figure 9 - Coexisting health conditions reported alongside PMOS (PCOS)",
    caption = sprintf(paste0("Percentages are of the %d respondents who disclosed their conditions; %d of %d preferred not to disclose and are excluded from both panels.",
                             "\nParticipants could report more than one condition. Panel b shows each condition reported by five or more respondents,\nincluding conditions typed under 'Other'. LDL = low-density lipoprotein."),
                      as.integer(n_disclosed), as.integer(n_total - n_disclosed), as.integer(n_total)),
    theme   = theme(
      plot.title      = element_text(face = "bold", colour = COL_TEAL, size = 13),
      plot.caption    = element_text(colour = COL_GREY, size = 8, face = "italic"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

save_fig("Figure9_Coexisting_Conditions", fig9, width = 8, height = 8)


# ----------------------------------------------------------------------------
# Session record
# ----------------------------------------------------------------------------
# Package versions used to produce the figures, for the repository.

dir.create("results", showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), "results/Script7_sessionInfo.txt")
message("Saved: results/Script7_sessionInfo.txt")


message("\n--- Script 7 complete ---")
message("Manuscript figures saved to figures/:")
message("  Figure2_Digital_Health_Technologies_Use_Profile")
message("  Figure4_Forest_Plot_Mediation")
message("  Figure5_Moderated_Mediation")
message("  (Figures 1 and 3 are drawn in PowerPoint)")
message("Additional figures saved to figures/:")
message("  Figure6_Outcomes_by_Use_Group")
message("  Figure7_Correlation_Heatmap")
message("  Figure8_Mediation_Diagram")
message("  Figure9_Coexisting_Conditions")
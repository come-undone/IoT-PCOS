# ============================================================================
# PCOS IoT Study - Script 7: Figures for Manuscript
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Produce all figures for journal manuscript submission.
#          All values are taken directly from verified script outputs
#          (Scripts 2-5). Nothing is hardcoded without a source.
#
# Figures produced:
#   Figure 1 - IoT technology use profile (item-level stacked bar)
#   Figure 2 - Health outcomes by IoT tertile group (raincloud plots)
#   Figure 3 - Correlation heatmap: IoT predictors x outcomes (3 x 10)
#   Figure 4 - Mediation path diagram: IoT -> EDE-Q7 -> PCOSQOL
#   Figure 5 - Forest plot: indirect effects across all mediator models
#   Figure 6 - Conditional indirect effects by age (moderated mediation)
#
# All figures saved as PNG (300 DPI) and PDF to figures/ folder.
#
# Scale directions (critical - never reverse):
#   PCOSQOL  - higher = BETTER quality of life
#   EDE-Q7   - higher = MORE eating concerns
#   ISI      - higher = WORSE insomnia
#   SWEMWBS  - higher = BETTER mental wellbeing
#
# Input:  data-processed/cleaned_data.rds (from Script 1)
# Run order: Script 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> THIS SCRIPT
# ============================================================================

library(tidyverse)
library(ggdist)
library(patchwork)
library(scales)

df <- readRDS("data-processed/cleaned_data.rds")
cat("N =", nrow(df), "\n")

dir.create("figures", showWarnings = FALSE)


# ---- Shared colour palette (consistent across all figures) -----------------

COL_TEAL  <- "#028090"
COL_CORAL <- "#E05C5C"
COL_SLATE <- "#50808E"
COL_GOLD  <- "#D4A843"
COL_GREY  <- "#7A8C8E"
COL_DARK  <- "#1A2B2C"
COL_LIGHT <- "#EBF5F7"

# IoT tertile group colours
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
  message("Saved: figures/", name)
}


# ============================================================================
# FIGURE 1 - IoT technology use profile
# ============================================================================
# Shows frequency of use for each of the 10 IoT items, grouped by subscale.
# Sorted by mean score within subscale so the most-used items appear first.
# Source: Script 2 Section 4 verified output.
# ============================================================================

message("Drawing Figure 1: IoT use profile")

# Item means from Script 2 verified output
iot_items <- data.frame(
  item = factor(c(
    "Browse PCOS websites",
    "PCOS info on social media",
    "Online PCOS forums",
    "Share PCOS experiences\n(social media)",
    "AI / LLMs for PCOS advice",
    "Monitor physical activity\n(app)",
    "Fitness / activity tracker",
    "Monitor PCOS symptoms\n(app)",
    "Monitor sleep\n(app/watch)",
    "Monitor nutrition\n(app)"
  )),
  subscale = c(rep("Engagement\n(items 1-5)", 5),
               rep("Self-Monitoring\n(items 6-10)", 5)),
  mean_score = c(1.93, 1.85, 1.16, 0.93, 0.48,
                 1.88, 1.81, 1.33, 1.32, 1.03)
)

# Frequency breakdown from Script 2 verified output
# Columns: Never (0), Seldom (1), Often (2), Regularly (3)
freq_data <- data.frame(
  item = rep(iot_items$item, each = 4),
  subscale = rep(iot_items$subscale, each = 4),
  response = rep(c("Never", "Seldom", "Often", "Regularly"), 10),
  pct = c(
    # Engagement items (sorted by mean score within subscale)
    7.7,  20.9, 41.8, 29.7,  # Browse PCOS websites (mean 1.93)
    12.1, 23.1, 33.0, 31.9,  # PCOS info social media (mean 1.85)
    34.1, 30.8, 19.8, 15.4,  # Online forums (mean 1.16)
    47.3, 24.2, 16.5, 12.1,  # Share social media (mean 0.93)
    68.1, 17.6, 12.1,  2.2,  # AI/LLMs (mean 0.48)
    # Self-Monitoring items (sorted by mean score within subscale)
    23.1,  8.8, 25.3, 42.9,  # Monitor PA app (mean 1.88)
    27.5,  8.8, 18.7, 45.1,  # Fitness/activity tracker (mean 1.81)
    40.7, 14.3, 16.5, 28.6,  # Monitor PCOS symptoms (mean 1.33)
    39.6, 18.7, 12.1, 29.7,  # Monitor sleep (mean 1.32)
    47.3, 18.7, 17.6, 16.5   # Monitor nutrition (mean 1.03)
  )
)

freq_data$response <- factor(freq_data$response,
                              levels = c("Never", "Seldom", "Often", "Regularly"))
freq_data$item     <- factor(freq_data$item,
                              levels = rev(levels(iot_items$item)))

response_cols <- c("Never"     = "#D6E8EA",
                   "Seldom"    = "#80B4BB",
                   "Often"     = "#028090",
                   "Regularly" = "#014D56")

fig1 <- ggplot(freq_data, aes(x = pct, y = item, fill = response)) +
  geom_col(position = "stack", width = 0.72) +
  facet_wrap(~ subscale, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = response_cols,
                    name   = "Frequency of use") +
  scale_x_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Figure 1. IoT technology use by item",
    subtitle = "Proportion of participants by frequency of use in the past 6 months (n = 91)",
    x        = "Proportion of respondents (%)",
    y        = NULL,
    caption  = "Items sorted by mean score within subscale (highest use first).\nEngagement = browsing, forums, social media, AI use. Self-Monitoring = apps and wearables."
  ) +
  theme_pcos(11) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 10, face = "bold", colour = COL_DARK),
    axis.text.y     = element_text(size = 9)
  )

save_fig("Figure1_IoT_Use_Profile", fig1, width = 10, height = 8)


# ============================================================================
# FIGURE 2 - Health outcomes by IoT tertile group (raincloud plots)
# ============================================================================
# Shows distribution, individual data points, and group summary for
# PCOSQOL and EDE-Q7 — the two outcomes with significant KW results.
# Source: Script 3 verified output. Group sizes: Low=34, Med=32, High=25.
# PCOSQOL: higher = BETTER QoL. KW H(2)=13.271, p=.001, eps2=.128.
# EDE-Q7:  higher = MORE concerns. KW H(2)=9.531, p=.009, eps2=.086.
# ============================================================================

message("Drawing Figure 2: Raincloud plots")

df_plot <- df %>%
  mutate(
    iot_group = factor(iot_category,
                       levels = c("Low", "Medium", "High"),
                       labels = c("Low IoT\n(n=34)", "Medium IoT\n(n=32)",
                                  "High IoT\n(n=25)"))
  )

# Panel A: PCOSQOL (higher = BETTER QoL - label this clearly)
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
  # Significance annotation - only High vs Low is significant
  annotate("text", x = 2, y = 210,
           label = "H(2) = 13.27, p = .001**\n\u03b5\u00b2 = .128 (medium effect)",
           colour = COL_CORAL, size = 3, fontface = "italic") +
  annotate("segment", x = 1, xend = 3, y = 200, yend = 200,
           colour = COL_CORAL, linewidth = 0.6) +
  annotate("text", x = 2, y = 204,
           label = "High vs Low: p = .0005***",
           colour = COL_CORAL, size = 2.8, fontface = "italic") +
  labs(
    title    = "A.  PCOSQOL total",
    subtitle = "(higher score = better quality of life)",
    x        = NULL,
    y        = "PCOSQOL score (35-245)"
  ) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"))

# Panel B: EDE-Q7 (higher = MORE concerns)
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
  # Clinical threshold line (Bang et al., 2023)
  geom_hline(yintercept = 3.64, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.7) +
  annotate("text", x = 0.55, y = 3.72,
           label = "Clinical threshold = 3.64",
           colour = COL_GREY, size = 2.7, fontface = "italic", hjust = 0) +
  scale_fill_manual(values   = TERTILE_COLS, guide = "none") +
  scale_colour_manual(values = TERTILE_COLS, guide = "none") +
  annotate("text", x = 2, y = 6.8,
           label = "H(2) = 9.53, p = .009**\n\u03b5\u00b2 = .086 (medium effect)",
           colour = COL_TEAL, size = 3, fontface = "italic") +
  annotate("segment", x = 1, xend = 3, y = 6.5, yend = 6.5,
           colour = COL_TEAL, linewidth = 0.6) +
  annotate("text", x = 1.5, y = 6.6,
           label = "p = .004**",
           colour = COL_TEAL, size = 2.8, fontface = "italic") +
  annotate("text", x = 2.5, y = 6.6,
           label = "p = .037*",
           colour = COL_TEAL, size = 2.8, fontface = "italic") +
  labs(
    title    = "B.  EDE-Q7 global mean",
    subtitle = "(higher score = more eating concerns)",
    x        = NULL,
    y        = "EDE-Q7 global mean (0-6)"
  ) +
  theme_pcos(11) +
  theme(plot.title = element_text(size = 11, colour = COL_DARK, face = "bold"))

fig2 <- (p_pcosqol | p_edq7) +
  plot_annotation(
    title   = "Figure 2. Health outcomes by IoT use group",
    subtitle = "Raincloud plots: half-violin density (right) + boxplot + individual data points (left). Groups defined by tertile split.",
    caption  = "Low n=34, Medium n=32, High n=25. PCOSQOL: higher = better QoL. EDE-Q7: higher = more eating concerns.\nISI, SWEMWBS, and MET were not significantly different across groups (all p > .15).\nDunn's pairwise tests with Bonferroni correction. Dashed line = EDE-Q7 clinical threshold (Bang et al., 2023).",
    theme    = theme(
      plot.title    = element_text(face = "bold", colour = COL_TEAL, size = 13),
      plot.subtitle = element_text(colour = COL_GREY, size = 9),
      plot.caption  = element_text(colour = COL_GREY, size = 8, face = "italic"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

save_fig("Figure2_Outcomes_by_IoT_Group", fig2, width = 12, height = 7)


# ============================================================================
# FIGURE 3 - Correlation heatmap: IoT predictors x outcomes (3 x 10)
# ============================================================================
# Rectangular geom_tile heatmap.
# Columns: IoT Total, Engagement, Self-Monitoring.
# Rows: PCOSQOL total + 4 subscales, EDE-Q7, ISI, SWEMWBS, MET, Sitting.
# Colour: coral = negative rho (IoT up = outcome down), teal = positive.
# Bold rho values shown in significant cells; faded in non-significant.
# All values computed from the data - not hardcoded.
# ============================================================================

message("Drawing Figure 3: Correlation heatmap")

iot_vars <- c("iot_total", "iot_engagement", "iot_selfmon")

iot_labels_map <- c(
  "iot_total"      = "IoT Total",
  "iot_engagement" = "IoT Engagement",
  "iot_selfmon"    = "IoT Self-Monitoring"
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

# Readable labels - arrows communicate scoring direction
outcome_labels_map <- c(
  "pcosqol_total"      = "PCOSQOL \u2014 Total  (\u2191 = better QoL)",
  "pcosqol_mood"       = "  \u2514 Mood / Emotional",
  "pcosqol_impact"     = "  \u2514 Symptom Impact",
  "pcosqol_hirsutism"  = "  \u2514 Body Hair (Hirsutism)",
  "pcosqol_infertility"= "  \u2514 Infertility",
  "ede_q7_global"      = "EDE-Q7  (\u2191 = more eating concerns)",
  "isi_total"          = "ISI  (\u2191 = worse insomnia)",
  "swemwbs_total"      = "SWEMWBS  (\u2191 = better wellbeing)",
  "MET_total"          = "Physical activity  (MET-min/week)",
  "sitting_hours"      = "Sitting time  (hours/day)"
)

# Compute all correlations from the data
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

# Build long data frame for ggplot
plot_dat <- expand.grid(outcome = outcome_vars, predictor = iot_vars,
                        stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    rho     = rho_mat[outcome, predictor],
    p_value = p_mat[outcome, predictor],
    is_sig  = p_value < .05,
    stars   = case_when(p_value < .001 ~ "***",
                        p_value < .01  ~ "**",
                        p_value < .05  ~ "*",
                        TRUE           ~ ""),
    cell_label = ifelse(is_sig,
                        paste0(sprintf("%.2f", rho), stars),
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

fig3 <- ggplot(plot_dat, aes(x = predictor_label, y = outcome_label,
                              fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(data = filter(plot_dat,  is_sig),
            aes(label = cell_label),
            size = 3.3, fontface = "bold", colour = COL_DARK) +
  geom_text(data = filter(plot_dat, !is_sig),
            aes(label = cell_label),
            size = 3.0, fontface = "plain", colour = "#AABFC2") +
  # Dashed line separating PCOSQOL rows from other outcomes
  annotate("segment",
           x = 0.5, xend = 3.5, y = 5.5, yend = 5.5,
           colour = COL_GREY, linewidth = 0.6, linetype = "dashed") +
  scale_fill_gradientn(
    colours = c(COL_CORAL, "#F5CECE", "white", "#C5E4E9", COL_TEAL),
    values  = rescale(c(-0.65, -0.25, 0, 0.25, 0.65)),
    limits  = c(-0.65, 0.65),
    oob     = squish,
    name    = "Spearman \u03c1",
    guide   = guide_colorbar(barwidth = 0.8, barheight = 11,
                             title.position = "top", title.hjust = 0.5)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Figure 3. Spearman rank correlations: IoT use and health outcomes",
    subtitle = "Bold = significant (p < .05). Dashed line separates PCOSQOL subscales (above) from other outcomes (below).\nCoral = negative correlation; teal = positive. n = 91 (MET: n = 80; sitting: n = 76).",
    caption  = "PCOSQOL: higher = better QoL; negative rho indicates worse QoL with higher IoT use.\n* p < .05  ** p < .01  *** p < .001",
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

save_fig("Figure3_Correlation_Heatmap", fig3, width = 9, height = 10)


# ============================================================================
# FIGURE 4 - Mediation path diagram: IoT -> EDE-Q7 -> PCOSQOL
# ============================================================================
# Built with ggplot2 geom_segment + annotate.
# All coefficients from Script 4 verified output (single mediator model):
#   Path a (IoT -> EDE-Q7):   B = +0.08, SE = 0.02, p < .001
#   Path b (EDE-Q7 -> PCOSQOL): B = -10.10, SE = 2.41, p < .001
#   Direct c' (IoT -> PCOSQOL): B = -1.85, SE = 0.55, p < .001
#   Indirect (a x b):          B = -0.80 [95% CI: -1.34, -0.25], p = .004
#   Total effect:              B = -2.65, p < .001
# ============================================================================

message("Drawing Figure 4: Mediation path diagram")

# Node positions
nodes <- data.frame(
  id    = c("IoT", "EDE", "QoL"),
  label = c("IoT Use\n(Predictor)", "Eating Concerns\n(EDE-Q7)", "PCOSQOL\n(Outcome)"),
  x     = c(0.10, 0.50, 0.90),
  y     = c(0.20, 0.75, 0.20),
  fill  = c(COL_LIGHT, "#FFF0EE", COL_LIGHT),
  col   = c(COL_TEAL,  COL_CORAL, COL_TEAL)
)

# Edge coordinates (slightly inset from node centres)
edges <- data.frame(
  x    = c(0.19, 0.60, 0.19),
  y    = c(0.25, 0.68, 0.20),
  xend = c(0.42, 0.82, 0.81),
  yend = c(0.68, 0.27, 0.20),
  col  = c(COL_CORAL, COL_CORAL, COL_TEAL),
  lwd  = c(1.4, 1.4, 1.2),
  lty  = c(1, 1, 1)
)

# Indirect effect arc (series of points forming a curve above the mediator)
arc <- data.frame(
  t = seq(0, 1, length.out = 80)
) %>%
  mutate(
    x = 0.10 + 0.80 * t,
    y = 0.94 - 4 * 0.12 * (t - 0.5)^2
  )

fig4 <- ggplot() +
  # Indirect arc
  geom_path(data = arc, aes(x = x, y = y),
            colour = COL_CORAL, linewidth = 1.3,
            arrow = arrow(length = unit(7, "pt"), type = "closed",
                          ends = "last")) +
  # Direct path
  geom_segment(data = edges[3, ],
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = COL_TEAL, linewidth = 1.2,
               arrow = arrow(length = unit(7, "pt"), type = "closed")) +
  # Path a and b arrows
  geom_segment(data = edges[1:2, ],
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour = col, linewidth = I(lwd)),
               arrow = arrow(length = unit(7, "pt"), type = "closed"),
               show.legend = FALSE) +
  scale_colour_identity() +
  # Node boxes
  geom_tile(data = nodes,
            aes(x = x, y = y, fill = I(fill)),
            width = 0.20, height = 0.15, colour = NA) +
  geom_rect(data = nodes,
            aes(xmin = x - 0.10, xmax = x + 0.10,
                ymin = y - 0.075, ymax = y + 0.075,
                colour = I(col)),
            fill = NA, linewidth = 1.4) +
  geom_text(data = nodes,
            aes(x = x, y = y, label = label),
            size = 3.5, fontface = "bold", lineheight = 0.9,
            colour = COL_DARK) +
  # Coefficient label boxes
  # Path a label
  annotate("label", x = 0.285, y = 0.535,
           label = "path a\nB = +0.08, SE = 0.02\np < .001 ***",
           colour = COL_CORAL, fill = "white", size = 2.9,
           fontface = "bold", label.size = 0.4) +
  # Path b label
  annotate("label", x = 0.715, y = 0.535,
           label = "path b\nB = -10.10, SE = 2.41\np < .001 ***",
           colour = COL_CORAL, fill = "white", size = 2.9,
           fontface = "bold", label.size = 0.4) +
  # Direct effect label
  annotate("label", x = 0.50, y = 0.09,
           label = "path c'  (direct effect)\nB = -1.85, SE = 0.55,  p < .001 ***",
           colour = COL_TEAL, fill = "white", size = 2.9,
           fontface = "bold", label.size = 0.4) +
  # Indirect effect label (on the arc)
  annotate("label", x = 0.50, y = 0.975,
           label = "Indirect effect: B = -0.80  [95% CI: -1.34, -0.25]  p = .004 **",
           colour = COL_CORAL, fill = "#FFF0EE", size = 3.0,
           fontface = "bold", label.size = 0.5) +
  # Path labels in small italic
  annotate("text", x = 0.30, y = 0.65,
           label = "path a", colour = COL_GREY, size = 3, fontface = "italic") +
  annotate("text", x = 0.70, y = 0.65,
           label = "path b", colour = COL_GREY, size = 3, fontface = "italic") +
  annotate("text", x = 0.50, y = 0.23,
           label = "path c'", colour = COL_GREY, size = 3, fontface = "italic") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.05), clip = "off") +
  labs(
    title    = "Figure 4. Mediation model: eating concerns mediate the association between IoT use and QoL",
    subtitle = "MLR-SEM with FIML. Covariates: age, years diagnosed, comorbidity count. n = 89 (2 cases excluded due to missing covariate data).",
    caption  = "Total effect (IoT -> PCOSQOL, unadjusted): B = -2.65, SE = 0.56, p < .001.\nPartial mediation: direct effect remains significant after introducing mediator.\nHigher PCOSQOL = better QoL; higher EDE-Q7 = more eating concerns."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", colour = COL_TEAL,
                                    size = 12, margin = margin(b = 4)),
    plot.subtitle    = element_text(colour = COL_GREY, size = 9,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(colour = COL_GREY, size = 8,
                                    face = "italic"),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(15, 20, 12, 20)
  )

save_fig("Figure4_Mediation_Diagram", fig4, width = 12, height = 7)


# ============================================================================
# FIGURE 5 - Forest plot: indirect effects across all mediation models
# ============================================================================
# Shows the indirect effect (B) and 95% CI for each of the four mediators
# plus the Engagement subscale model.
# Source: Script 4 verified output.
# The key message: only EDE-Q7 has a CI entirely below zero.
# Non-significant mediators are plotted in grey to show they were tested.
# ============================================================================

message("Drawing Figure 5: Forest plot of indirect effects")

# All values from Script 4 verified output
# (single mediator models, IoT total as predictor)
forest_dat <- data.frame(
  label    = c("EDE-Q7\n(eating concerns)",
               "EDE-Q7 — Engagement\nsubscale as predictor",
               "EDE-Q7\n(multiple mediator model)",
               "ISI\n(insomnia severity)",
               "SWEMWBS\n(mental wellbeing)",
               "log(MET + 1)\n(physical activity)"),
  B        = c(-0.80, -1.37, -0.69, -0.23, -0.08, 0.11),
  ci_lo    = c(-1.34, -2.35, -1.18, -0.61, -0.48, -0.18),
  ci_hi    = c(-0.25, -0.38, -0.19,  0.15,  0.32,  0.40),
  p_value  = c(0.004, 0.007, 0.007,  0.230,  0.689,  0.456),
  model    = c("Primary", "Subscale", "Multiple",
               "Null", "Null", "Null"),
  stringsAsFactors = FALSE
)

forest_dat <- forest_dat %>%
  mutate(
    sig   = p_value < .05,
    colour = case_when(
      model == "Primary"  ~ COL_CORAL,
      model == "Subscale" ~ COL_GOLD,
      model == "Multiple" ~ COL_SLATE,
      TRUE                ~ COL_GREY
    ),
    label = factor(label, levels = rev(label))
  )

fig5 <- ggplot(forest_dat,
               aes(x = B, y = label, colour = I(colour))) +
  # Zero reference line
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.8) +
  # CI bars
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi,
                     linewidth = I(ifelse(sig, 1.2, 0.8))),
                 height = 0.25) +
  # Point estimates
  geom_point(aes(size = I(ifelse(sig, 4.5, 3.5)),
                 shape = I(ifelse(sig, 18, 18)))) +
  # B value labels for significant results
  geom_text(data = filter(forest_dat, sig),
            aes(label = sprintf("B = %.2f\np = .%s",
                                B,
                                formatC(p_value * 1000, digits = 0,
                                        format = "d", flag = "0"))),
            nudge_x = -0.15, size = 2.8, fontface = "bold",
            hjust = 1, lineheight = 0.9) +
  # Significance stars
  geom_text(data = filter(forest_dat, sig),
            aes(x = ci_hi + 0.04,
                label = ifelse(p_value < .01, "**", "*")),
            size = 4, fontface = "bold") +
  # Panel annotation
  annotate("label", x = -2.6, y = "EDE-Q7\n(eating concerns)",
           label = "Only EDE-Q7 shows a\nsignificant indirect pathway",
           colour = COL_CORAL, fill = "#FFF0EE", size = 2.8,
           fontface = "italic", hjust = 0, label.size = 0.3) +
  scale_x_continuous(limits = c(-3.0, 0.7),
                     breaks = seq(-3.0, 0.5, 0.5)) +
  labs(
    title    = "Figure 5. Indirect effects of IoT use on PCOSQOL via each mediator",
    subtitle = "Points = unstandardised B; error bars = 95% CI. Negative values = harmful indirect pathway (more IoT -> worse QoL).",
    x        = "Indirect effect (unstandardised B)",
    y        = NULL,
    caption  = "All models: MLR-SEM with FIML, covariates: age, years diagnosed, comorbidity count.\nEngagement model uses IoT Engagement subscale as predictor instead of IoT total.\nMultiple mediator model: unique indirect effect of EDE-Q7 when all four mediators compete simultaneously."
  ) +
  theme_pcos(11) +
  theme(
    axis.text.y  = element_text(size = 9, lineheight = 1.1),
    panel.grid.major.y = element_line(colour = "#F0F4F4", linewidth = 0.4)
  )

save_fig("Figure5_Forest_Plot_Mediation", fig5, width = 11, height = 7)


# ============================================================================
# FIGURE 6 - Conditional indirect effects by age
# ============================================================================
# Shows how the indirect effect of IoT -> EDE-Q7 -> PCOSQOL varies
# across three age levels (M-1SD, M, M+1SD).
# Source: Script 5 verified output.
#   a3 interaction = -0.0054, SE = 0.0025, p = .032
#   Younger (~27 yrs, M-1SD): B = -1.40 [CI: -2.19, -0.60] p < .001
#   Mean age (~35 yrs):       B = -0.92 [CI: -1.48, -0.36] p = .001
#   Older (~43 yrs, M+1SD):   B = -0.44 [CI: -1.10, +0.22] p = .190 ns
# ============================================================================

message("Drawing Figure 6: Conditional indirect effects by age")

modmed_dat <- data.frame(
  age_level = factor(
    c("Younger\n(-1 SD, ~27 yrs)", "Mean age\n(~35 yrs)", "Older\n(+1 SD, ~43 yrs)"),
    levels = c("Younger\n(-1 SD, ~27 yrs)", "Mean age\n(~35 yrs)",
               "Older\n(+1 SD, ~43 yrs)")
  ),
  B     = c(-1.40, -0.92, -0.44),
  ci_lo = c(-2.19, -1.48, -1.10),
  ci_hi = c(-0.60, -0.36,  0.22),
  p     = c(0.0001, 0.001, 0.190),
  sig   = c(TRUE,   TRUE,  FALSE)
)

fig6 <- ggplot(modmed_dat,
               aes(x = age_level, y = B,
                   colour = sig, fill = sig)) +
  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = COL_GREY, linewidth = 0.8) +
  # Shaded CI band
  geom_rect(aes(xmin = as.numeric(age_level) - 0.30,
                xmax = as.numeric(age_level) + 0.30,
                ymin = ci_lo, ymax = ci_hi),
            alpha = 0.15, colour = NA) +
  # CI error bars
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.15, linewidth = 1.2) +
  # Point estimates
  geom_point(size = 6, shape = 21, stroke = 2) +
  # B value labels
  geom_text(aes(label = sprintf("B = %.2f", B),
                vjust = ifelse(B < 0, 2.5, -1.5)),
            size = 4, fontface = "bold", colour = COL_DARK) +
  # p-value labels below CI
  geom_text(data = filter(modmed_dat,  sig),
            aes(y = ci_lo - 0.15,
                label = ifelse(p < .001, "p < .001 ***", "p = .001 **")),
            size = 3, fontface = "italic", colour = COL_CORAL) +
  geom_text(data = filter(modmed_dat, !sig),
            aes(y = ci_hi + 0.12, label = "p = .190 ns"),
            size = 3, fontface = "italic", colour = COL_GREY) +
  # Trend line connecting the three points
  geom_line(aes(group = 1), colour = COL_TEAL,
            linewidth = 0.9, linetype = "solid") +
  # Annotation box explaining the moderation
  annotate("label", x = 2, y = -2.4,
           label = paste0("Age moderates path a\n",
                          "(IoT -> eating concerns)\n",
                          "a3 = -0.0054, SE = 0.0025, p = .032*"),
           colour = COL_TEAL, fill = COL_LIGHT, size = 3,
           fontface = "italic", label.size = 0.4) +
  scale_colour_manual(values = c("TRUE" = COL_CORAL, "FALSE" = COL_GREY)) +
  scale_fill_manual(  values = c("TRUE" = COL_CORAL, "FALSE" = COL_GREY)) +
  scale_y_continuous(limits = c(-2.7, 0.5), breaks = seq(-2.5, 0.5, 0.5)) +
  labs(
    title    = "Figure 6. Conditional indirect effects: age moderates the eating-concerns mediation pathway",
    subtitle = "Indirect effect of IoT use on PCOSQOL via eating concerns (EDE-Q7) at three levels of age.",
    x        = NULL,
    y        = "Indirect effect (B)",
    caption  = "Error bars = 95% CI. Negative B = harmful indirect pathway (more IoT -> more eating concerns -> worse QoL).\nCovariates: years diagnosed, comorbidity count. Moderated mediation at path a. MLR-SEM with FIML."
  ) +
  theme_pcos(12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 11, face = "bold")
  )

save_fig("Figure6_Moderated_Mediation", fig6, width = 10, height = 7)


message("\n--- Script 7 complete ---")
message("Six figures saved to figures/:")
message("  Figure1_IoT_Use_Profile")
message("  Figure2_Outcomes_by_IoT_Group")
message("  Figure3_Correlation_Heatmap")
message("  Figure4_Mediation_Diagram")
message("  Figure5_Forest_Plot_Mediation")
message("  Figure6_Moderated_Mediation")

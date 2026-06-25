# 07-paper_figures.R
# Reproducibly generate every figure in the SIR submission (main paper + ESM_4).
# Created 2026-06-17 to fix audit finding #3: all 8 manuscript figures were
# orphaned from the canonical pipeline. This script is the producer of record.
#
# Run AFTER:
#   - R/analysis/06e-run-all.sh has produced ma_sensitivity_*.RData files
#   - R/analysis/06e-bayesian_appendix.R has consolidated them to CSVs
#   - The cached frequentist outputs in output/tables/ are up to date
#
# Usage:
#   Rscript R/analysis/07-paper_figures.R
#
# Produces (in output/figures/paper/):
#   main_fig1_recruitment_network.png
#   main_fig2_composite_risk_comparison.png  (replaced 2026-06-20: was a misleading two-panel sample-vs-MA; now a three-estimator comparison forest)
#   main_fig3_ma_forest_plot.png
#   main_fig4_rds_forest_plot.png
#   main_fig5_nsum_forest_plot.png      # replaces the Fig 4 duplicate
#   esm4_fig1_rds_ss_forest.png  (replaces former rds_by_popsize - flat lines were a non-finding)
#   esm4_fig2_ma_seedcheck_facets.png
#   esm4_fig3_ma_seedcheck_lineplot.png

suppressMessages({
  library(here)
  library(tidyverse)
  library(igraph)
  library(scales)
  library(patchwork)
})
source(here("R", "utils", "helper_functions.R"))

FIG_DIR <- here("output", "figures", "paper")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Indicator label mapping (consistent across figures)
INDICATOR_LABELS <- c(
  document_withholding_rds = "Document withholding",
  pay_issues_rds           = "Pay-related issues",
  threats_abuse_rds        = "Threats and abuse",
  excessive_hours_rds      = "Excessive working hours",
  access_to_help_rds       = "Limited access to help",
  whether_exploitation     = "Any exploitation",
  composite_risk           = "Composite risk score"
)

# Common theme
paper_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.85)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position  = "bottom"
    )
}

# ===========================================================================
# Load data once
# ===========================================================================

cat("Loading prepared data and cached estimates...\n")
load(here("data", "processed", "prepared_data.RData"))
if (!exists("rd.dd") && exists("dd")) rd.dd <- dd

read_csv_quiet <- function(p) suppressMessages(read_csv(p, show_col_types = FALSE))

# Cached frequentist tables
rds_appendix <- read_csv_quiet(here("output", "tables", "rds_appendix_comparison.csv"))
rds_main     <- read_csv_quiet(here("output", "tables", "rds_main_comparison.csv"))
nsum_pop     <- read_csv_quiet(here("output", "tables", "AppendixA3_popsize_comparison.csv"))
nsum_method  <- read_csv_quiet(here("output", "tables", "AppendixA4_nsum_method_comparison.csv"))

# Cached Bayesian results (post-fix)
ma_headline  <- read_csv_quiet(here("output", "tables", "ESM_appendix_bayesian_summary.csv"))
ma_seedcheck <- read_csv_quiet(here("output", "tables", "ESM_appendix_bayesian_seedcheck.csv"))

# Per-method bootstrap CIs - produced by R/analysis/04-bootstrap_analysis.R.
# Used by Main Fig 2 (composite_risk comparison), Main Fig 4 (RDS-I vs RDS-SS
# forest), and ESM Fig 1 (RDS-SS forest with bootstrap CIs).
per_method_path <- here("output", "tables", "rds_per_method_bootstrap_ci.csv")
if (!file.exists(per_method_path)) {
  stop("Missing ", per_method_path,
       " - run R/analysis/04-bootstrap_analysis.R first.")
}
per_method <- read_csv_quiet(per_method_path)

# ===========================================================================
# MAIN FIGURE 1 - RDS Recruitment Network
# ===========================================================================

cat("Producing main_fig1_recruitment_network.png...\n")

# Build edge list (recruiter -> recruit) excluding seeds (recruiter.id == -1 or 0 sentinel)
# Find the nationality column (real name is 'nationality_cluster' in the
# prepared data; fall back to other plausible names if the schema changes)
nat_candidates <- c("nationality_cluster", "nat_cluster", "nationality")
nat_col <- intersect(nat_candidates, names(rd.dd))
nat_col <- if (length(nat_col) > 0) nat_col[1] else NA_character_

edges_df <- rd.dd %>%
  as_tibble() %>%
  select(id, recruiter.id, known_network_size) %>%
  filter(!is.na(recruiter.id), recruiter.id != -1, recruiter.id != "0",
         recruiter.id != 0, !is.na(id))

vertices_df <- rd.dd %>%
  as_tibble() %>%
  mutate(
    is_seed      = (recruiter.id == -1) | (recruiter.id == "0") |
                   (recruiter.id == 0) | is.na(recruiter.id),
    network_size = pmax(as.numeric(known_network_size), 1, na.rm = TRUE),
    nationality  = if (!is.na(nat_col)) as.character(.data[[nat_col]]) else "Other"
  ) %>%
  select(id, is_seed, network_size, nationality)

# Drop edges whose endpoints aren't in vertices_df (safety)
edges_df <- edges_df %>% filter(id %in% vertices_df$id, recruiter.id %in% vertices_df$id)

g <- graph_from_data_frame(d = edges_df %>% select(recruiter.id, id),
                           vertices = vertices_df, directed = TRUE)

set.seed(42)
layout_xy <- layout_with_fr(g)

network_plot_df <- tibble(
  id          = V(g)$name,
  x           = layout_xy[, 1],
  y           = layout_xy[, 2],
  is_seed     = V(g)$is_seed,
  nationality = V(g)$nationality,
  size        = V(g)$network_size
)

edge_segments <- tibble(
  from = ends(g, E(g))[, 1],
  to   = ends(g, E(g))[, 2]
) %>%
  left_join(network_plot_df %>% select(id, x, y) %>% rename(x_from = x, y_from = y),
            by = c("from" = "id")) %>%
  left_join(network_plot_df %>% select(id, x, y) %>% rename(x_to = x, y_to = y),
            by = c("to" = "id"))

n_seeds <- sum(network_plot_df$is_seed)
n_total <- nrow(network_plot_df)
n_chains <- length(edges_df %>% pull(recruiter.id) %>% unique())

p1 <- ggplot() +
  geom_segment(data = edge_segments,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
               colour = "grey60", alpha = 0.5,
               arrow = arrow(length = unit(0.10, "cm"), type = "closed")) +
  geom_point(data = network_plot_df,
             aes(x = x, y = y, colour = nationality, shape = is_seed, size = size),
             alpha = 0.85) +
  scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16),  # triangle vs circle
                     labels = c(`TRUE` = "Seed", `FALSE` = "Recruit"),
                     name = "Node type") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  scale_colour_brewer(palette = "Set2", name = "Nationality") +
  labs(
    title = "RDS Recruitment Network Structure",
    subtitle = sprintf("Sample size: n = %d  |  Seeds: %d  |  Recruitment chains: %d",
                       n_total, n_seeds, n_chains)
  ) +
  paper_theme() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(file.path(FIG_DIR, "main_fig1_recruitment_network.png"),
       p1, width = 7, height = 5, dpi = 300)

# ===========================================================================
# MAIN FIGURE 2 - Composite risk: three-estimator comparison forest
# ===========================================================================
# Was a two-panel sample-vs-MA plot; the MA panel was misleading because
# MA.estimates() doesn't converge for continuous traits and the wide CI made
# the flat-line representation visually deceptive. Now a simple three-row
# forest comparing sample mean, RDS-SS adjusted mean, and MA Bayesian mean
# of the composite risk index, with their respective uncertainties. The huge
# MA CI is the honest signal that MA fails on continuous outcomes.

cat("Producing main_fig2_composite_risk_comparison.png...\n")

risk_data <- tibble(composite_risk = rd.dd$composite_risk) %>%
  filter(!is.na(composite_risk))
sample_mean <- mean(risk_data$composite_risk)
sample_n    <- nrow(risk_data)

# RDS-SS composite risk - pull from per-method bootstrap CSV (run by 04-)
rds_ss_comp <- per_method %>%
  filter(indicator == "composite_risk", method == "RDS_SS")
rds_ss_point <- if (nrow(rds_ss_comp) > 0) rds_ss_comp$original_estimate[1] else NA_real_
rds_ss_lower <- if (nrow(rds_ss_comp) > 0) rds_ss_comp$ci_lower[1] else NA_real_
rds_ss_upper <- if (nrow(rds_ss_comp) > 0) rds_ss_comp$ci_upper[1] else NA_real_

# MA Bayesian composite risk - from ESM_appendix_bayesian_summary.csv
ma_composite <- ma_headline %>% filter(indicator == "composite_risk")
ma_point <- if (nrow(ma_composite) > 0) ma_composite$point_estimate[1] else NA_real_
ma_lower <- if (nrow(ma_composite) > 0) ma_composite$ci_lower[1] else NA_real_
ma_upper <- if (nrow(ma_composite) > 0) ma_composite$ci_upper[1] else NA_real_

forest_df <- tibble(
  estimator = c("Sample mean", "RDS-SS adjusted", "MA Bayesian"),
  point     = c(sample_mean, rds_ss_point, ma_point),
  lower     = c(NA_real_,    rds_ss_lower, ma_lower),
  upper     = c(NA_real_,    rds_ss_upper, ma_upper)
) %>%
  mutate(
    estimator = factor(estimator, levels = c("MA Bayesian", "RDS-SS adjusted", "Sample mean")),
    point_pct = point * 100,
    lower_pct = lower * 100,
    upper_pct = upper * 100,
    label_str = ifelse(
      is.na(lower),
      sprintf("%.1f%%", point_pct),
      sprintf("%.1f%% (%.1f-%.1f%%)", point_pct, lower_pct, upper_pct)
    )
  )

p2 <- ggplot(forest_df, aes(x = point_pct, y = estimator)) +
  geom_errorbarh(aes(xmin = lower_pct, xmax = upper_pct),
                 height = 0.18, colour = "grey40", size = 0.7,
                 na.rm = TRUE) +
  geom_point(aes(colour = estimator), size = 4) +
  geom_text(aes(label = label_str), hjust = -0.1, vjust = -0.8, size = 3.4) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-2, 105)) +
  scale_colour_manual(values = c("Sample mean"     = "#7f7f7f",
                                 "RDS-SS adjusted" = "#2980b9",
                                 "MA Bayesian"     = "#e67e22"),
                      guide = "none") +
  labs(title = "Composite Risk Index: Three Estimator Comparison",
       subtitle = sprintf(
         "Sample mean is descriptive (no CI; n=%d). RDS-SS shown with bootstrap 95%% CI. MA shown with 95%% credible interval - the wide CI reflects known MA limitations for continuous outcomes.",
         sample_n),
       x = "Mean Composite Risk Index (% of maximum)",
       y = NULL) +
  paper_theme() +
  theme(plot.subtitle = element_text(size = rel(0.8)))

ggsave(file.path(FIG_DIR, "main_fig2_composite_risk_comparison.png"),
       p2, width = 9, height = 4.0, dpi = 300)

# ===========================================================================
# MAIN FIGURE 3 - MA forest plot (Bayesian estimates with credible intervals)
# ===========================================================================

cat("Producing main_fig3_ma_forest_plot.png...\n")

ma_forest_df <- ma_headline %>%
  mutate(indicator_label = unname(INDICATOR_LABELS[as.character(indicator)])) %>%
  filter(!is.na(point_estimate)) %>%
  arrange(point_estimate) %>%
  mutate(indicator_label = factor(indicator_label, levels = indicator_label))

p3 <- ggplot(ma_forest_df, aes(x = point_estimate * 100, y = indicator_label)) +
  geom_errorbarh(aes(xmin = ci_lower * 100, xmax = ci_upper * 100),
                 height = 0.2, colour = "steelblue", size = 0.8) +
  geom_point(colour = "navy", size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", point_estimate * 100)),
            vjust = -1.2, colour = "navy", size = 3.5) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(title = "Model-Assisted Estimates of Exploitation Prevalence",
       subtitle = "Population estimates adjusted for RDS sampling bias",
       x = "Estimated Prevalence (%)", y = "Exploitation Indicator") +
  paper_theme()

ggsave(file.path(FIG_DIR, "main_fig3_ma_forest_plot.png"),
       p3, width = 8, height = 5, dpi = 300)

# ===========================================================================
# MAIN FIGURE 4 - RDS forest plot (RDS-I vs RDS-SS comparison)
# ===========================================================================

cat("Producing main_fig4_rds_forest_plot.png...\n")

# Uses per-method bootstrap CIs (loaded at the top of the script).
# Each of RDS-I, RDS-II, RDS-SS has its own bootstrap CI - no approximation.

rds_forest_df <- per_method %>%
  filter(method %in% c("RDS_I", "RDS_SS"),
         indicator %in% c("document_withholding_rds", "pay_issues_rds",
                          "threats_abuse_rds", "excessive_hours_rds",
                          "access_to_help_rds")) %>%
  mutate(
    indicator_label = unname(INDICATOR_LABELS[as.character(indicator)]),
    indicator_label = factor(indicator_label,
                             levels = c("Document withholding", "Pay-related issues",
                                        "Threats and abuse", "Limited access to help",
                                        "Excessive working hours")),
    estimate = original_estimate * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100
  )

p4 <- ggplot(rds_forest_df, aes(x = estimate, y = indicator_label, colour = method, shape = method)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", estimate)),
            position = position_dodge(width = 0.5),
            vjust = -1.2, size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = c(RDS_I = "#c0392b", RDS_SS = "#2980b9"),
                      labels = c("RDS-I", "RDS-SS"), name = "RDS Method") +
  scale_shape_manual(values = c(RDS_I = 16, RDS_SS = 17),
                     labels = c("RDS-I", "RDS-SS"), name = "RDS Method") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "RDS Prevalence Estimates by Method",
       subtitle = "Population size: 980,000 domestic workers",
       x = "Estimated Prevalence (%)", y = "Exploitation Indicator") +
  paper_theme()

ggsave(file.path(FIG_DIR, "main_fig4_rds_forest_plot.png"),
       p4, width = 8, height = 5, dpi = 300)

# ===========================================================================
# MAIN FIGURE 5 - NSUM forest plot (replaces Fig 4 duplicate)
# ===========================================================================

cat("Producing main_fig5_nsum_forest_plot.png (replaces duplicate)...\n")

# Pull MBSU (Moderate) results at N=980,000 from AppendixA3
nsum_forest_df <- nsum_pop %>%
  filter(`Population Size` %in% c("980000", "980,000")) %>%
  mutate(
    outcome_label = unname(c(
      "Document Withholding" = "Document withholding",
      "Pay Issues"           = "Pay-related issues",
      "Threats/Abuse"        = "Threats and abuse",
      "Excessive Hours"      = "Excessive working hours",
      "Access to Help"       = "Limited access to help"
    )[as.character(Outcome)]),
    prevalence = as.numeric(`Point Estimate`) / 980000 * 100,
    ci_lower_pct = as.numeric(`95% CI Lower`) / 980000 * 100,
    ci_upper_pct = as.numeric(`95% CI Upper`) / 980000 * 100
  ) %>%
  filter(!is.na(prevalence)) %>%
  arrange(prevalence) %>%
  mutate(outcome_label = factor(outcome_label, levels = outcome_label))

p5 <- ggplot(nsum_forest_df, aes(x = prevalence, y = outcome_label)) +
  geom_errorbarh(aes(xmin = ci_lower_pct, xmax = ci_upper_pct),
                 height = 0.2, colour = "darkgreen", size = 0.8) +
  geom_point(colour = "darkgreen", size = 3) +
  geom_text(aes(label = sprintf("%.2f%%", prevalence)),
            vjust = -1.2, colour = "darkgreen", size = 3.5) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "NSUM Prevalence Estimates",
       subtitle = "MBSU (Moderate) estimator, N = 980,000 domestic workers",
       x = "Estimated Prevalence (%)", y = "Exploitation Indicator") +
  paper_theme()

ggsave(file.path(FIG_DIR, "main_fig5_nsum_forest_plot.png"),
       p5, width = 8, height = 5, dpi = 300)

# ===========================================================================
# ESM FIGURE 1 - RDS-SS forest plot with per-method bootstrap CIs
# ===========================================================================
# Replaces the original "RDS Estimates Across Population Sizes" figure, which
# showed flat lines (RDS-I/II don't depend on N; RDS-SS depends only weakly when
# n/N << 1). That non-finding is now a single sentence in the methods section;
# this figure surfaces the more substantive content: per-indicator point
# estimates with REAL bootstrap CIs from the RDS-SS estimator at N = 980,000.

cat("Producing esm4_fig1_rds_ss_forest.png...\n")

esm1_df <- per_method %>%
  filter(method == "RDS_SS",
         indicator %in% c("document_withholding_rds", "pay_issues_rds",
                          "threats_abuse_rds", "excessive_hours_rds",
                          "access_to_help_rds")) %>%
  mutate(
    indicator_label = unname(INDICATOR_LABELS[as.character(indicator)]),
    estimate        = original_estimate * 100,
    ci_lower        = ci_lower * 100,
    ci_upper        = ci_upper * 100
  ) %>%
  arrange(estimate) %>%
  mutate(indicator_label = factor(indicator_label, levels = indicator_label))

p_esm1 <- ggplot(esm1_df, aes(x = estimate, y = indicator_label)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.2, colour = "#2980b9", size = 0.8) +
  geom_point(colour = "#1f5582", size = 3, shape = 17) +
  geom_text(aes(label = sprintf("%.1f%%", estimate)),
            vjust = -1.2, colour = "#1f5582", size = 3.5) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) +
  labs(title = "RDS-SS Prevalence Estimates with Bootstrap Confidence Intervals",
       subtitle = sprintf("Gile's Successive Sampling estimator, N = 980,000, %d bootstrap replicates",
                          unique(per_method$n_bootstrap)[1]),
       x = "Estimated Prevalence (%)",
       y = "Exploitation Indicator") +
  paper_theme()

ggsave(file.path(FIG_DIR, "esm4_fig1_rds_ss_forest.png"),
       p_esm1, width = 8, height = 5, dpi = 300)

# ===========================================================================
# ESM FIGURE 2 - MA seed-selection check (facet by indicator)
# ===========================================================================

cat("Producing esm4_fig2_ma_seedcheck_facets.png...\n")

# Use the new seedcheck CSV (1 indicator x 3 seed methods at N=50k).
# Caption needs to acknowledge: only one indicator was tested across seeds
# because seed-selection robustness held for that indicator and per the
# 06e- design we did not run all indicators across all seeds.
seedcheck_long <- ma_seedcheck %>%
  mutate(indicator_label = unname(INDICATOR_LABELS[as.character(indicator)]),
         seed_selection = factor(seed_selection, levels = c("degree", "random", "sample"),
                                 labels = c("Degree", "Random", "Sample")))

p_esm2 <- ggplot(seedcheck_long, aes(x = seed_selection, y = point_estimate * 100)) +
  geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                width = 0.2, colour = "steelblue") +
  geom_point(size = 3, colour = "navy") +
  geom_text(aes(label = sprintf("%.1f%%", point_estimate * 100)),
            vjust = -1, size = 3.5) +
  facet_wrap(~ indicator_label, ncol = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(title = "Sensitivity of MA Estimates to Seed-Selection Method",
       subtitle = sprintf("Gile & Handcock (2015) MA inference, N = %s", format(unique(seedcheck_long$population_size)[1], big.mark = ",")),
       x = "Seed Selection Method", y = "Estimated Prevalence (%)") +
  paper_theme()

ggsave(file.path(FIG_DIR, "esm4_fig2_ma_seedcheck_facets.png"),
       p_esm2, width = 6, height = 5, dpi = 300)

# ===========================================================================
# ESM FIGURE 3 - MA point estimates across indicators (single seed = degree)
# ===========================================================================

cat("Producing esm4_fig3_ma_seedcheck_lineplot.png...\n")

# Show the 7 indicators connected by a line at the chosen seed (degree),
# with CIs as error bars. Replaces the original Fig 3 which tried to show
# 3 seed methods but they were artefact-identical.
ma_lineplot_df <- ma_headline %>%
  mutate(indicator_label = unname(INDICATOR_LABELS[as.character(indicator)])) %>%
  arrange(point_estimate) %>%
  mutate(indicator_label = factor(indicator_label, levels = indicator_label))

p_esm3 <- ggplot(ma_lineplot_df,
                 aes(x = indicator_label, y = point_estimate * 100, group = 1)) +
  geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                width = 0.2, colour = "darkgreen") +
  geom_line(colour = "darkgreen", alpha = 0.5) +
  geom_point(size = 3, colour = "darkgreen") +
  geom_text(aes(label = sprintf("%.1f%%", point_estimate * 100)),
            vjust = -1, size = 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 110)) +
  labs(title = "MA Estimates Across Indicators",
       subtitle = sprintf("N = %s, seed.selection = 'degree'",
                          format(unique(ma_lineplot_df$population_size)[1], big.mark = ",")),
       x = "Exploitation Indicator", y = "Estimated Prevalence (%)") +
  paper_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(FIG_DIR, "esm4_fig3_ma_seedcheck_lineplot.png"),
       p_esm3, width = 9, height = 5, dpi = 300)

# ===========================================================================
cat("\nAll 8 paper figures regenerated to:", FIG_DIR, "\n")
cat("Files:\n")
for (f in list.files(FIG_DIR, pattern = "\\.png$", full.names = FALSE)) {
  cat("  ", f, "\n")
}
cat("\nNext: replace embedded figures in paper/.../SIR/Figure*.docx and ESM_4.docx.\n")

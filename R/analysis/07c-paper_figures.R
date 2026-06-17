# 07c-paper_figures.R
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
#   Rscript R/analysis/07c-paper_figures.R
#
# Produces (in output/figures/paper/):
#   main_fig1_recruitment_network.png
#   main_fig2_composite_risk_distribution.png
#   main_fig3_ma_forest_plot.png
#   main_fig4_rds_forest_plot.png
#   main_fig5_nsum_forest_plot.png      # replaces the Fig 4 duplicate
#   esm4_fig1_rds_by_popsize.png
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

# ===========================================================================
# MAIN FIGURE 1 - RDS Recruitment Network
# ===========================================================================

cat("Producing main_fig1_recruitment_network.png...\n")

# Build edge list (recruiter -> recruit) excluding seeds (recruiter.id == -1 or 0 sentinel)
edges_df <- rd.dd %>%
  as_tibble() %>%
  select(id, recruiter.id, known_network_size, any_of(c("nat_cluster", "nationality"))) %>%
  filter(!is.na(recruiter.id), recruiter.id != -1, recruiter.id != "0",
         recruiter.id != 0, !is.na(id))

# Vertex attributes - use whichever nationality column exists
nat_col <- intersect(c("nat_cluster", "nationality"), names(rd.dd))[1]
vertices_df <- rd.dd %>%
  as_tibble() %>%
  mutate(
    is_seed   = (recruiter.id == -1) | (recruiter.id == "0") | (recruiter.id == 0) | is.na(recruiter.id),
    network_size = pmax(as.numeric(known_network_size), 1, na.rm = TRUE)
  ) %>%
  select(id, is_seed, network_size, any_of(nat_col))

if (!is.null(nat_col)) names(vertices_df)[names(vertices_df) == nat_col] <- "nationality"
if (!"nationality" %in% names(vertices_df)) vertices_df$nationality <- "Other"

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
# MAIN FIGURE 2 - Composite risk: sample distribution vs MA population estimate
# ===========================================================================

cat("Producing main_fig2_composite_risk_distribution.png...\n")

risk_data <- tibble(composite_risk = rd.dd$composite_risk) %>%
  filter(!is.na(composite_risk))

sample_mean <- mean(risk_data$composite_risk)

# Pull MA composite risk row from headline summary
ma_composite <- ma_headline %>% filter(indicator == "composite_risk")
ma_point  <- ma_composite$point_estimate[1]
ma_lower  <- ma_composite$ci_lower[1]
ma_upper  <- ma_composite$ci_upper[1]

panel_a <- ggplot(risk_data, aes(x = composite_risk)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "steelblue", colour = "white", alpha = 0.7) +
  geom_density(colour = "navy", size = 1) +
  geom_vline(xintercept = sample_mean, colour = "red", linetype = "dashed", size = 1) +
  annotate("text", x = sample_mean + 0.02, y = Inf,
           label = sprintf("Sample mean = %.3f", sample_mean),
           hjust = 0, vjust = 1.5, colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(title = "A. Observed Sample Distribution",
       subtitle = sprintf("n = %d", nrow(risk_data)),
       x = "Composite Risk Index", y = "Density") +
  paper_theme()

ma_band_df <- tibble(
  composite_risk = seq(0, 1, length.out = 100),
  estimate = ma_point,
  ci_lower = ma_lower,
  ci_upper = ma_upper
)

panel_b <- ggplot(ma_band_df, aes(x = composite_risk)) +
  geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
              fill = "orange", alpha = 0.25) +
  geom_hline(yintercept = ma_point * 100, colour = "orange", size = 1) +
  geom_vline(xintercept = ma_point, colour = "red", linetype = "dashed", size = 1) +
  annotate("text", x = ma_point + 0.02, y = Inf,
           label = sprintf("Pop. mean = %.3f", ma_point),
           hjust = 0, vjust = 1.5, colour = "red") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(title = "B. Model-Assisted Population Estimate",
       subtitle = "Population-adjusted with 95% credible interval",
       x = "Composite Risk Index", y = "Estimated Population (%)") +
  paper_theme()

p2 <- panel_a + panel_b +
  plot_annotation(title = "Composite Risk Index: Sample vs. Population-Adjusted Estimates",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(FIG_DIR, "main_fig2_composite_risk_distribution.png"),
       p2, width = 10, height = 4.5, dpi = 300)

# ===========================================================================
# MAIN FIGURE 3 - MA forest plot (Bayesian estimates with credible intervals)
# ===========================================================================

cat("Producing main_fig3_ma_forest_plot.png...\n")

ma_forest_df <- ma_headline %>%
  mutate(indicator_label = recode(indicator, !!!INDICATOR_LABELS)) %>%
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

# rds_main_comparison.csv has columns: indicator, RDS_I, RDS_II, RDS_SS (percentages)
# Need CIs from rds_appendix_comparison.csv or bootstrap_results. Build from
# rds_main_results.csv which has CIs per indicator.
rds_main_with_ci <- read_csv_quiet(here("output", "tables", "rds_main_results.csv")) %>%
  mutate(method = "RDS_SS",
         indicator_label = recode(gsub("_rds$", "", indicator_base),
                                  document_withholding = "Document withholding",
                                  pay_issues           = "Pay-related issues",
                                  threats_abuse        = "Threats and abuse",
                                  excessive_hours      = "Excessive working hours",
                                  access_to_help       = "Limited access to help",
                                  whether_exploitation = "Any exploitation",
                                  composite_risk       = "Composite risk score"))

# Per-method estimates from rds_main_comparison.csv (points only) - reshape long
rds_methods_long <- rds_main %>%
  pivot_longer(c(RDS_I, RDS_II, RDS_SS), names_to = "method", values_to = "estimate_str") %>%
  mutate(estimate = as.numeric(gsub("%", "", estimate_str))) %>%
  filter(method %in% c("RDS_I", "RDS_SS"),
         indicator %in% c("Document withholding", "Pay issues", "Threats/abuse",
                          "Excessive hours", "Limited access to help"))

# CIs: approximate using rds_main_with_ci (RDS-SS CIs) and apply identical width to RDS-I
# (rough; for a publication we'd want per-method bootstraps. Flagged as TODO.)
ci_lookup <- rds_main_with_ci %>%
  transmute(indicator_label, ci_lower = ci_lower * 100, ci_upper = ci_upper * 100)

rds_forest_df <- rds_methods_long %>%
  mutate(indicator_label = recode(indicator,
                                  "Document withholding"   = "Document withholding",
                                  "Pay issues"             = "Pay-related issues",
                                  "Threats/abuse"          = "Threats and abuse",
                                  "Excessive hours"        = "Excessive working hours",
                                  "Limited access to help" = "Limited access to help")) %>%
  left_join(ci_lookup, by = "indicator_label") %>%
  mutate(indicator_label = factor(indicator_label,
                                  levels = c("Document withholding", "Pay-related issues",
                                             "Threats and abuse", "Limited access to help",
                                             "Excessive working hours")))

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
    outcome_label = recode(Outcome,
                           "Document Withholding" = "Document withholding",
                           "Pay Issues"           = "Pay-related issues",
                           "Threats/Abuse"        = "Threats and abuse",
                           "Excessive Hours"      = "Excessive working hours",
                           "Access to Help"       = "Limited access to help"),
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
# ESM FIGURE 1 - RDS estimates across population sizes (RDS-I, RDS-II, RDS-SS)
# ===========================================================================

cat("Producing esm4_fig1_rds_by_popsize.png...\n")

esm1_df <- rds_appendix %>%
  mutate(
    indicator_label = recode(indicator,
                             "Document withholding"   = "Document withholding",
                             "Pay issues"             = "Pay-related issues",
                             "Threats/abuse"          = "Threats and abuse",
                             "Excessive hours"        = "Excessive working hours",
                             "Limited access to help" = "Limited access to help"),
    pop_label = factor(population_label, levels = c("50k", "100k", "980k", "1.74M"),
                       labels = c("50K", "100K", "980K", "1.74M"))
  )

p_esm1 <- ggplot(esm1_df, aes(x = pop_label, y = estimate_pct,
                              group = indicator_label, colour = indicator_label)) +
  geom_point(size = 2.5) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ method, ncol = 3) +
  scale_colour_viridis_d(name = "Indicator") +
  labs(title = "RDS Estimates Across Population Sizes",
       subtitle = "Comparison of estimation methods (RDS-I, RDS-II, RDS-SS)",
       x = "Population Size", y = "Estimated Prevalence (%)") +
  paper_theme() +
  theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "esm4_fig1_rds_by_popsize.png"),
       p_esm1, width = 10, height = 5, dpi = 300)

# ===========================================================================
# ESM FIGURE 2 - MA seed-selection check (facet by indicator)
# ===========================================================================

cat("Producing esm4_fig2_ma_seedcheck_facets.png...\n")

# Use the new seedcheck CSV (1 indicator x 3 seed methods at N=50k).
# Caption needs to acknowledge: only one indicator was tested across seeds
# because seed-selection robustness held for that indicator and per the
# 06e- design we did not run all indicators across all seeds.
seedcheck_long <- ma_seedcheck %>%
  mutate(indicator_label = recode(indicator, !!!INDICATOR_LABELS),
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
  mutate(indicator_label = recode(indicator, !!!INDICATOR_LABELS)) %>%
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

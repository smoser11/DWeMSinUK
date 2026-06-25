# 05b-nsum_tau_sensitivity.R
# Continuous-tau sensitivity analysis for the NSUM Modified Basic Scale-Up
# (MBSU) estimator. Addresses the IJOPM reviewer's "wide 34%-84% range"
# concern by showing per-indicator NSUM prevalence as a continuous function
# of the visibility / true-positive-rate adjustment factor tau_F.
#
# The current paper reports three discrete (delta, tau) combinations:
#   "No Adjustment":   delta=1.0,  tau=1.0
#   "Moderate":        delta=0.9,  tau=0.85
#   "Conservative":    delta=0.8,  tau=0.70
#
# This script keeps delta fixed at 0.9 and sweeps tau across a fine grid
# (0.40 to 1.00 in 0.05 steps = 13 values). For each (indicator, tau) it
# computes the MBSU population count + bootstrap 95% CI, then converts to
# prevalence (count / N_F).
#
# Implementation note: this project's NSUM uses known_network_size directly
# as the degree variable (per the design note in 05-nsum_estimation.R: "NO
# traditional probe questions - uses direct network size from RDS"). The
# adjustment factors are applied multiplicatively post-hoc:
#   adjusted_count = basic_count * (1/delta) * (1/tau) * eta
# This matches Feehan & Salganik (2016, Eq. 24) with the network-size-based
# basic estimate substituting for the probe-question-based basic estimate.
#
# Outputs:
#   output/tables/nsum_tau_sensitivity.csv
#   output/figures/paper/nsum_tau_sensitivity.png
#
# Created 2026-06-20. Reworked 2026-06-20 to use the project-native NSUM
# call pattern (degree_var = known_network_size, no probe questions).

suppressMessages({
  library(here)
  library(tidyverse)
  library(scales)
})

source(here("R", "utils", "helper_functions.R"))
source(here("R", "analysis", "nsum_core_estimators.R"))

if (!exists("dd") || !exists("dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

nsum_tau_config <- list(
  N_F      = 980000,
  delta_F  = 0.9,
  eta      = 1.0,
  tau_grid = seq(0.40, 1.00, by = 0.05),

  # NSUM-side indicators: respondents' reports about exploitation in their
  # alter network (paired with the _rds ego-self-report indicators).
  indicators_nsum = c(
    "document_withholding_nsum",
    "pay_issues_nsum",
    "threats_abuse_nsum",
    "excessive_hours_nsum",
    "access_to_help_nsum"
  ),

  # Degree variable (Q13: number of domestic workers respondent knows).
  degree_var = "known_network_size",

  # RDS-SS weight column for the main population size (per project convention)
  weight_var = "wt.SS_980k",

  # Bootstrap
  n_bootstrap      = 500,
  confidence_level = 0.95,
  seed             = 12345
)

cat("=== NSUM continuous-tau sensitivity ===\n")
cat(sprintf("Indicators:        %d\n", length(nsum_tau_config$indicators_nsum)))
cat(sprintf("Tau grid:          %d values from %.2f to %.2f\n",
            length(nsum_tau_config$tau_grid),
            min(nsum_tau_config$tau_grid), max(nsum_tau_config$tau_grid)))
cat(sprintf("delta_F:           %.2f  eta: %.2f\n",
            nsum_tau_config$delta_F, nsum_tau_config$eta))
cat(sprintf("N_F:               %s\n", format(nsum_tau_config$N_F, big.mark = ",")))
cat(sprintf("Bootstrap reps:    %d\n", nsum_tau_config$n_bootstrap))
cat(sprintf("Degree variable:   %s\n", nsum_tau_config$degree_var))
cat(sprintf("Weight variable:   %s\n", nsum_tau_config$weight_var))
cat("\n")

# Sanity checks + diagnostics (added 2026-06-20 - if 05b- silently fails this
# block tells you exactly why)
cat("Data object check:\n")
cat(sprintf("  nrow(dd):                 %d\n", nrow(dd)))
cat(sprintf("  degree_var in dd:         %s\n", nsum_tau_config$degree_var %in% names(dd)))
cat(sprintf("  weight_var in dd:         %s\n", nsum_tau_config$weight_var %in% names(dd)))
for (ind in nsum_tau_config$indicators_nsum) {
  cat(sprintf("  indicator '%s' in dd: %s\n", ind, ind %in% names(dd)))
}
cat("\n")

if (!nsum_tau_config$degree_var %in% names(dd)) {
  stop("degree_var '", nsum_tau_config$degree_var, "' not found in dd")
}
weights <- if (nsum_tau_config$weight_var %in% names(dd)) {
  dd[[nsum_tau_config$weight_var]]
} else {
  cat("WARN: weight_var '", nsum_tau_config$weight_var,
      "' not found; falling back to unweighted MBSU.\n", sep = "")
  NULL
}
method_name <- if (is.null(weights)) "basic" else "weighted"
scheme_name <- if (is.null(weights)) "unweighted" else "SS"

# Smoke test: try one call BEFORE the sweep, with verbose=TRUE so any
# downstream error surfaces immediately rather than 65x silently.
cat("Smoke test: single MBSU call with verbose=TRUE...\n")
smoke <- tryCatch(
  estimate_nsum_population(
    data = dd,
    weights = weights,
    hidden_connections_var = nsum_tau_config$indicators_nsum[1],
    degree_var = nsum_tau_config$degree_var,
    total_population_size = nsum_tau_config$N_F,
    method = method_name,
    weighting_scheme = scheme_name,
    verbose = TRUE
  ),
  error = function(e) list(error = conditionMessage(e))
)
if ((!is.null(smoke$error) && !is.na(smoke$error)) ||
    is.null(smoke$N_H_estimate) || is.na(smoke$N_H_estimate)) {
  cat("\nSMOKE TEST FAILED:\n  ",
      if (is.null(smoke$error) || is.na(smoke$error)) "N_H_estimate missing" else smoke$error,
      "\n")
  cat("All subsequent (indicator, tau) combinations will also fail.\n")
  stop("05b- smoke test failed - see diagnostics above")
}
cat("Smoke test OK. N_H_estimate =",
    if (is.null(smoke$N_H_estimate)) "NULL" else format(round(smoke$N_H_estimate)), "\n\n")

# --------------------------------------------------------------------------
# Helper: one MBSU call with manual (delta, tau, eta) adjustment
# --------------------------------------------------------------------------

mbsu_adjusted <- function(data, ind, weights_vec, tau,
                          delta = nsum_tau_config$delta_F,
                          eta = nsum_tau_config$eta,
                          N_F = nsum_tau_config$N_F) {
  basic <- tryCatch(
    estimate_nsum_population(
      data = data,
      weights = weights_vec,
      hidden_connections_var = ind,
      degree_var = nsum_tau_config$degree_var,
      total_population_size = N_F,
      method = method_name,
      weighting_scheme = scheme_name,
      verbose = FALSE
    ),
    error = function(e) list(error = e$message)
  )
    # estimate_mbsu() returns `error = NA` as a list field on SUCCESS (always
  # present), so we cannot check `"error" %in% names(basic)`. Check whether
  # error is non-NA (actual failure) OR estimate is missing.
  if ((!is.null(basic$error) && !is.na(basic$error)) ||
      is.null(basic$N_H_estimate) || is.na(basic$N_H_estimate)) {
    return(NA_real_)
  }
  basic$N_H_estimate * (1 / delta) * (1 / tau) * eta
}

# --------------------------------------------------------------------------
# Sweep
# --------------------------------------------------------------------------

set.seed(nsum_tau_config$seed)
qs <- c((1 - nsum_tau_config$confidence_level) / 2,
        1 - (1 - nsum_tau_config$confidence_level) / 2)

results <- list()
for (ind in nsum_tau_config$indicators_nsum) {
  if (!(ind %in% names(dd))) {
    cat("SKIP - indicator missing from dd:", ind, "\n")
    next
  }
  for (tau in nsum_tau_config$tau_grid) {
    cat(sprintf("  %s  tau=%.2f", ind, tau))
    point_count <- mbsu_adjusted(dd, ind, weights, tau)
    if (is.na(point_count)) {
      cat("  FAIL (point estimate)\n"); next
    }

    # Bootstrap: simple node-level resample (consistent with the per-method
    # RDS bootstrap in 04-bootstrap_analysis.R)
    boot_counts <- numeric(nsum_tau_config$n_bootstrap)
    for (b in seq_len(nsum_tau_config$n_bootstrap)) {
      idx <- sample(seq_len(nrow(dd)), replace = TRUE)
      boot_data <- dd[idx, ]
      class(boot_data) <- class(dd)
      attributes(boot_data) <- attributes(dd)
      boot_w <- if (!is.null(weights)) weights[idx] else NULL
      boot_counts[b] <- mbsu_adjusted(boot_data, ind, boot_w, tau)
    }
    ci <- quantile(boot_counts, qs, na.rm = TRUE)
    if (any(!is.finite(ci))) ci <- c(NA_real_, NA_real_)

    results[[paste(ind, tau, sep = "__")]] <- tibble(
      indicator        = ind,
      tau              = tau,
      delta            = nsum_tau_config$delta_F,
      eta              = nsum_tau_config$eta,
      point_count      = point_count,
      point_prevalence = point_count / nsum_tau_config$N_F,
      ci_lower_count   = unname(ci[1]),
      ci_upper_count   = unname(ci[2]),
      ci_lower_prev    = unname(ci[1]) / nsum_tau_config$N_F,
      ci_upper_prev    = unname(ci[2]) / nsum_tau_config$N_F,
      n_bootstrap      = nsum_tau_config$n_bootstrap,
      N_F              = nsum_tau_config$N_F,
      weight_scheme    = scheme_name
    )

    cat(sprintf("  -> %.2f%% (%.2f-%.2f%%)\n",
                100 * point_count / nsum_tau_config$N_F,
                100 * unname(ci[1]) / nsum_tau_config$N_F,
                100 * unname(ci[2]) / nsum_tau_config$N_F))
  }
}

results_df <- bind_rows(results)
if (nrow(results_df) == 0) {
  stop("No NSUM tau sensitivity results were produced. Check the configuration.")
}

# --------------------------------------------------------------------------
# Write CSV
# --------------------------------------------------------------------------

out_csv <- here("output", "tables", "nsum_tau_sensitivity.csv")
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write_csv(results_df, out_csv)
cat("\nSaved CSV:", out_csv, "\n")

# --------------------------------------------------------------------------
# Figure
# --------------------------------------------------------------------------

INDICATOR_LABELS <- c(
  document_withholding_nsum = "Document withholding",
  pay_issues_nsum           = "Pay-related issues",
  threats_abuse_nsum        = "Threats and abuse",
  excessive_hours_nsum      = "Excessive working hours",
  access_to_help_nsum       = "Limited access to help"
)

plot_df <- results_df %>%
  mutate(indicator_label = recode(indicator, !!!INDICATOR_LABELS))

p <- ggplot(plot_df,
            aes(x = tau, y = point_prevalence * 100,
                colour = indicator_label, fill = indicator_label,
                group = indicator_label)) +
  geom_ribbon(aes(ymin = ci_lower_prev * 100,
                  ymax = ci_upper_prev * 100),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(0.4, 1.0, 0.1),
                     labels = function(x) format(x, nsmall = 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_viridis_d(name = "Indicator") +
  scale_fill_viridis_d(name = "Indicator") +
  labs(title = "NSUM (MBSU) Prevalence as a Continuous Function of Visibility tau",
       subtitle = sprintf(
         "delta fixed at %.2f; %s weights; bootstrap 95%% CI (B = %d); N_F = %s",
         nsum_tau_config$delta_F, scheme_name,
         nsum_tau_config$n_bootstrap,
         format(nsum_tau_config$N_F, big.mark = ",")),
       x = "Visibility / true-positive-rate adjustment tau",
       y = "Estimated prevalence (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40", size = rel(0.85)),
        legend.position = "bottom")

out_png <- here("output", "figures", "paper", "nsum_tau_sensitivity.png")
dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
ggsave(out_png, p, width = 9, height = 5.5, dpi = 300)
cat("Saved figure:", out_png, "\n\n")
cat("Done.\n")

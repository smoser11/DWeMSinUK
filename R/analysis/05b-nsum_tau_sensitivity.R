# 05b-nsum_tau_sensitivity.R
# Continuous-τ sensitivity analysis for the NSUM Modified Basic Scale-Up (MBSU)
# estimator. Addresses the IJOPM reviewer's "wide 34%-84% range" concern by
# showing per-indicator NSUM prevalence as a continuous function of the
# visibility / true-positive-rate adjustment factor τ_F.
#
# The current paper reports three discrete (δ, τ) combinations:
#   "No Adjustment":   δ=1.0, τ=1.0
#   "Moderate":        δ=0.9, τ=0.85
#   "Conservative":    δ=0.8, τ=0.70
#
# This script keeps δ fixed at 0.9 and sweeps τ across a fine grid (0.40 - 1.00
# in 0.05 steps = 13 values). For each (indicator, τ) it computes:
#   - MBSU population count
#   - MBSU prevalence (count / N_F)
#   - Bootstrap 95% CI (B = nsum_tau_config$n_bootstrap replicates of the
#     NSUM-aware neighborhood bootstrap)
#
# Outputs:
#   output/tables/nsum_tau_sensitivity.csv
#   output/figures/paper/nsum_tau_sensitivity.png
#
# Created 2026-06-20.

suppressMessages({
  library(here)
  library(tidyverse)
  library(scales)
})

# Project setup + NSUM machinery
source(here("R", "utils", "helper_functions.R"))
source(here("R", "analysis", "nsum_core_estimators.R"))
source(here("R", "analysis", "nsum_robust_adjustment.R"))
source(here("R", "analysis", "nsum_bootstrap.R"))

# Load prepared RDS data
if (!exists("rd.dd") || !exists("dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

nsum_tau_config <- list(
  # Population size for prevalence calculations
  N_F = 980000,

  # Fixed delta (degree ratio). Moderate value; main paper sensitivity already
  # covers delta variation across the discrete (No / Moderate / Conservative) tiers.
  delta_F = 0.9,

  # Tau sweep: visibility / true-positive-rate
  tau_grid = seq(0.40, 1.00, by = 0.05),

  # Indicators (NSUM-side names; one per binary ILO indicator)
  indicators_nsum = c(
    "document_withholding_nsum",
    "pay_issues_nsum",
    "threats_abuse_nsum",
    "excessive_hours_nsum",
    "access_to_help_nsum"
  ),

  # Probe questions (used to estimate d_F,F = average degree in the frame
  # population from respondents' reports about known groups of known size)
  probe_sizes = list(
    # Reasonable defaults; if 02-data_preparation.R defines probe_sizes globally
    # we should pick those up. Documented in the codebook.
    "q_probe_brothers"  = 1.31,    # placeholder; replace with actual probe sizes
    "q_probe_sisters"   = 1.40,
    "q_probe_doctors"   = 0.003
  ),

  # Weighting scheme for NSUM (SS = Sequential Sampling = RDS-SS weights)
  weighting_scheme = "SS",
  weight_var = "wt.SS",

  # Bootstrap
  n_bootstrap = 500,
  confidence_level = 0.95,
  seed = 12345
)

cat("=== NSUM continuous-tau sensitivity ===\n")
cat("Indicators:", length(nsum_tau_config$indicators_nsum), "\n")
cat("Tau grid:  ", length(nsum_tau_config$tau_grid),
    "values from", min(nsum_tau_config$tau_grid),
    "to", max(nsum_tau_config$tau_grid), "\n")
cat("delta_F:   ", nsum_tau_config$delta_F, "\n")
cat("N_F:       ", format(nsum_tau_config$N_F, big.mark = ","), "\n")
cat("Bootstrap: ", nsum_tau_config$n_bootstrap, "replicates\n\n")

# --------------------------------------------------------------------------
# Sanity-check probe variables present in data
# --------------------------------------------------------------------------

degree_vars <- intersect(names(rd.dd), names(nsum_tau_config$probe_sizes))
if (length(degree_vars) == 0) {
  # Fall back: search for any q_probe_* variables in the data
  candidate <- grep("^q_probe_|^probe_|^known_", names(rd.dd), value = TRUE)
  if (length(candidate) > 0) {
    cat("WARN: configured probe names not found; using these instead:",
        paste(candidate, collapse = ", "), "\n")
    degree_vars <- candidate
  } else {
    stop("No probe-question variables found in rd.dd. Required for NSUM degree estimation. ",
         "Edit nsum_tau_config$probe_sizes to match the actual probe-variable names in ",
         "data/processed/prepared_data.RData.")
  }
}
probe_sizes <- nsum_tau_config$probe_sizes[degree_vars]

# --------------------------------------------------------------------------
# Core sweep: for each indicator, for each tau, run MBSU and bootstrap CI
# --------------------------------------------------------------------------

set.seed(nsum_tau_config$seed)
quantiles <- c((1 - nsum_tau_config$confidence_level) / 2,
               1 - (1 - nsum_tau_config$confidence_level) / 2)

results <- list()
for (ind in nsum_tau_config$indicators_nsum) {

  if (!(ind %in% names(rd.dd))) {
    cat("SKIP - indicator missing from rd.dd:", ind, "\n")
    next
  }

  for (tau in nsum_tau_config$tau_grid) {

    cat(sprintf("  %s  tau=%.2f", ind, tau))

    # Point estimate via the existing robust-NSUM function
    pt <- tryCatch(
      calculate_robust_nsum(
        data = rd.dd,
        nsum_var = ind,
        degree_vars = degree_vars,
        probe_sizes = probe_sizes,
        weight_var = if (nsum_tau_config$weight_var %in% names(rd.dd))
                       nsum_tau_config$weight_var else NULL,
        N_F = nsum_tau_config$N_F,
        degree_ratio = nsum_tau_config$delta_F,
        true_positive_rate = tau,
        precision = 1.0,
        scheme_name = nsum_tau_config$weighting_scheme
      ),
      error = function(e) list(error = e$message)
    )

    if ("error" %in% names(pt)) {
      cat("  ERROR:", pt$error, "\n")
      next
    }

    point_count   <- pt$adjusted_estimate
    point_prev    <- point_count / nsum_tau_config$N_F

    # Bootstrap CI via the existing NSUM bootstrap (passes adjustment factors)
    boot_pts <- numeric(nsum_tau_config$n_bootstrap)
    for (b in seq_len(nsum_tau_config$n_bootstrap)) {
      boot_indices <- sample(seq_len(nrow(rd.dd)), replace = TRUE)
      boot_data <- rd.dd[boot_indices, ]
      class(boot_data) <- class(rd.dd)
      attributes(boot_data) <- attributes(rd.dd)

      boot_pts[b] <- tryCatch({
        r <- calculate_robust_nsum(
          data = boot_data,
          nsum_var = ind,
          degree_vars = degree_vars,
          probe_sizes = probe_sizes,
          weight_var = if (nsum_tau_config$weight_var %in% names(boot_data))
                         nsum_tau_config$weight_var else NULL,
          N_F = nsum_tau_config$N_F,
          degree_ratio = nsum_tau_config$delta_F,
          true_positive_rate = tau,
          precision = 1.0,
          scheme_name = nsum_tau_config$weighting_scheme
        )
        if ("error" %in% names(r)) NA_real_ else r$adjusted_estimate
      }, error = function(e) NA_real_)
    }
    ci <- quantile(boot_pts, quantiles, na.rm = TRUE)

    results[[paste(ind, tau, sep = "__")]] <- tibble(
      indicator        = ind,
      tau              = tau,
      delta            = nsum_tau_config$delta_F,
      point_count      = point_count,
      point_prevalence = point_prev,
      ci_lower_count   = unname(ci[1]),
      ci_upper_count   = unname(ci[2]),
      ci_lower_prev    = unname(ci[1]) / nsum_tau_config$N_F,
      ci_upper_prev    = unname(ci[2]) / nsum_tau_config$N_F,
      n_bootstrap      = nsum_tau_config$n_bootstrap,
      N_F              = nsum_tau_config$N_F
    )

    cat(sprintf("  -> %.2f%% (%.2f%%, %.2f%%)\n",
                100 * point_prev,
                100 * unname(ci[1]) / nsum_tau_config$N_F,
                100 * unname(ci[2]) / nsum_tau_config$N_F))
  }
}

results_df <- bind_rows(results)

# --------------------------------------------------------------------------
# Write CSV
# --------------------------------------------------------------------------

out_csv <- here("output", "tables", "nsum_tau_sensitivity.csv")
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write_csv(results_df, out_csv)
cat("Saved CSV:", out_csv, "\n")

# --------------------------------------------------------------------------
# Figure: prevalence vs tau, one line per indicator, CI ribbon
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
  labs(title = "NSUM (MBSU) Prevalence as a Continuous Function of Visibility τ",
       subtitle = sprintf(
         "delta fixed at %.2f; bootstrap 95%% CI; N_F = %s; %d bootstrap replicates",
         nsum_tau_config$delta_F,
         format(nsum_tau_config$N_F, big.mark = ","),
         nsum_tau_config$n_bootstrap),
       x = expression("Visibility / true-positive-rate adjustment τ"),
       y = "Estimated prevalence (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40", size = rel(0.85)),
        legend.position = "bottom")

out_png <- here("output", "figures", "paper", "nsum_tau_sensitivity.png")
dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
ggsave(out_png, p, width = 9, height = 5.5, dpi = 300)
cat("Saved figure:", out_png, "\n")
cat("\nDone.\n")

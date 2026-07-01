# 08c-loo_chain_analysis.R
# Leave-One-Out chain sensitivity analysis: exclude the chain rooted at seed #55
# (length 12) and recompute RDS-SS estimates for all five comparable indicators.
#
# The manuscript (line ~365) and Appendix Section D.8 claim that removing this
# disproportionately long chain does not change the rank ordering of indicators.
# This script verifies that claim and produces the figure and table referenced
# in the Appendix.
#
# Chain identification: BFS from seed #55 following recruiter.id links in dd.
# Seeds in dd have recruiter.id == "0" after 02-data_preparation.R.
#
# Outputs:
#   output/tables/loo_chain_sensitivity.csv
#   output/figures/paper/figA5_loo_chain.png
#
# Created 2026-06-30.

if (exists("skip_execution") && !skip_execution) {
  cat("=== LOO Chain Sensitivity (seed #55) ===\n")
} else if (!exists("skip_execution")) {
  cat("=== LOO Chain Sensitivity (seed #55) ===\n")
} else {
  # skip_execution == TRUE: just define helpers, don't run body
}

if (!exists("skip_execution") || !skip_execution) {

suppressMessages({
  library(here)
  library(tidyverse)
  library(RDS)
  library(ggplot2)
})

source(here("R", "utils", "helper_functions.R"))
source(here("R", "config.R"))
global_config <- get_global_config()

# --------------------------------------------------------------------------
# Load data
# --------------------------------------------------------------------------

if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared_data.RData\n")
}

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

loo_config <- list(
  seed_id        = "55",        # root of the chain to exclude
  N_F            = 980000,      # main population size
  n_bootstrap    = 500,
  boot_seed      = 42,
  indicators     = c(
    "document_withholding_rds",
    "pay_issues_rds",
    "threats_abuse_rds",
    "excessive_hours_rds",
    "access_to_help_rds"
  ),
  indicator_labels = c(
    document_withholding_rds = "Document withholding",
    pay_issues_rds           = "Pay/debt issues",
    threats_abuse_rds        = "Threats and abuse",
    excessive_hours_rds      = "Excessive hours",
    access_to_help_rds       = "Limited access to help"
  )
)

cat(sprintf("Seed to exclude:   %s\n", loo_config$seed_id))
cat(sprintf("N_F:               %s\n", format(loo_config$N_F, big.mark = ",")))
cat(sprintf("Bootstrap reps:    %d\n", loo_config$n_bootstrap))
cat(sprintf("Indicators:        %d\n", length(loo_config$indicators)))
cat("\n")

# --------------------------------------------------------------------------
# BFS to find chain members
# --------------------------------------------------------------------------

find_chain <- function(dd, root_id) {
  # dd must have columns: id (character), recruiter.id (character)
  # Seeds have recruiter.id == "0"; root_id is character
  chain <- as.character(root_id)
  frontier <- as.character(root_id)
  repeat {
    next_gen <- as.character(dd$id[dd$recruiter.id %in% frontier])
    new_members <- setdiff(next_gen, chain)
    if (length(new_members) == 0) break
    chain <- c(chain, new_members)
    frontier <- new_members
  }
  chain
}

chain_ids <- find_chain(dd, loo_config$seed_id)
cat(sprintf("Chain rooted at seed #%s: %d members\n",
            loo_config$seed_id, length(chain_ids)))
cat(sprintf("Full sample n:     %d\n", nrow(dd)))
cat(sprintf("LOO sample n:      %d\n", nrow(dd) - length(chain_ids)))
cat("\n")

if (!loo_config$seed_id %in% as.character(dd$id)) {
  stop("Seed #", loo_config$seed_id, " not found in dd. ",
       "Check that recruiter.id == '0' identifies seeds correctly.")
}

# --------------------------------------------------------------------------
# Build LOO dataset and rds.data.frame
# --------------------------------------------------------------------------

dd_loo <- dd[!as.character(dd$id) %in% chain_ids, ]

# Recruits whose recruiter is now absent become new seeds (recruiter.id -> "0")
# so the rds.data.frame remains valid.
dd_loo$recruiter.id <- ifelse(
  as.character(dd_loo$recruiter.id) %in% chain_ids,
  "0",
  as.character(dd_loo$recruiter.id)
)

# Re-construct rds.data.frame with the same parameters as 02-data_preparation.R
rd_loo <- as.rds.data.frame(
  dd_loo,
  id              = "id",
  recruiter.id    = "recruiter.id",
  network.size    = "known_network_size",
  max.coupons     = 5,
  check.valid     = FALSE
)

cat(sprintf("LOO rds.data.frame: %d rows\n\n", nrow(rd_loo)))

# --------------------------------------------------------------------------
# RDS-SS estimation helper
# --------------------------------------------------------------------------

run_rds_ss <- function(rds_obj, indicators, N, n_boot, boot_seed) {
  set.seed(boot_seed)
  results <- list()
  for (ind in indicators) {
    if (!ind %in% names(rds_obj)) {
      cat("  SKIP (column missing):", ind, "\n"); next
    }
    cat("  Estimating:", ind, "... ")
    est <- tryCatch(
      RDS.SS.estimates(
        rds.data    = rds_obj,
        outcome.variable = ind,
        N           = N,
        se.method   = "bootstrap",
        number.of.bootstrap.iterations = n_boot
      ),
      error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
    )
    if (is.null(est)) next

    # RDS.SS.estimates returns a list; extract point estimate and CI
    pt   <- est$estimate
    ci_l <- tryCatch(est$confidence.interval[1], error = function(e) NA_real_)
    ci_u <- tryCatch(est$confidence.interval[2], error = function(e) NA_real_)

    results[[ind]] <- tibble(
      indicator   = ind,
      estimate    = as.numeric(pt),
      ci_lower    = as.numeric(ci_l),
      ci_upper    = as.numeric(ci_u)
    )
    cat(sprintf("%.3f (%.3f-%.3f)\n", as.numeric(pt),
                as.numeric(ci_l), as.numeric(ci_u)))
  }
  bind_rows(results)
}

# --------------------------------------------------------------------------
# Run full-sample and LOO estimates
# --------------------------------------------------------------------------

cat("Full sample (n =", nrow(rd.dd), "):\n")
full_results <- run_rds_ss(
  rd.dd,
  loo_config$indicators,
  loo_config$N_F,
  loo_config$n_bootstrap,
  loo_config$boot_seed
)
full_results$sample <- "Full"
full_results$n      <- nrow(rd.dd)

cat("\nLOO sample (n =", nrow(rd_loo), "):\n")
loo_results <- run_rds_ss(
  rd_loo,
  loo_config$indicators,
  loo_config$N_F,
  loo_config$n_bootstrap,
  loo_config$boot_seed
)
loo_results$sample <- "LOO (excl. seed #55 chain)"
loo_results$n      <- nrow(rd_loo)

combined <- bind_rows(full_results, loo_results) %>%
  mutate(
    label = recode(indicator, !!!loo_config$indicator_labels),
    sample = factor(sample, levels = c("Full", "LOO (excl. seed #55 chain)"))
  )

# --------------------------------------------------------------------------
# Rank-ordering check
# --------------------------------------------------------------------------

check_ranks <- function(results_df) {
  results_df %>%
    arrange(estimate) %>%
    pull(label)
}

full_rank <- check_ranks(full_results %>%
  mutate(label = recode(indicator, !!!loo_config$indicator_labels)))
loo_rank  <- check_ranks(loo_results %>%
  mutate(label = recode(indicator, !!!loo_config$indicator_labels)))

cat("\nRank ordering (lowest to highest):\n")
cat("  Full sample:", paste(full_rank, collapse = " < "), "\n")
cat("  LOO sample: ", paste(loo_rank,  collapse = " < "), "\n")
cat("  Rank ordering preserved:", identical(full_rank, loo_rank), "\n\n")

# --------------------------------------------------------------------------
# Save CSV
# --------------------------------------------------------------------------

out_csv <- here("output", "tables", "loo_chain_sensitivity.csv")
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write_csv(combined, out_csv)
cat("Saved CSV:", out_csv, "\n")

# --------------------------------------------------------------------------
# Figure
# --------------------------------------------------------------------------

p <- ggplot(combined,
            aes(x = reorder(label, estimate),
                y = estimate * 100,
                colour = sample,
                shape  = sample)) +
  geom_pointrange(
    aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
    position = position_dodge(width = 0.45),
    linewidth = 0.6,
    size = 0.7
  ) +
  coord_flip() +
  scale_colour_manual(
    name   = "Sample",
    values = c("Full" = "#2166ac", "LOO (excl. seed #55 chain)" = "#d6604d")
  ) +
  scale_shape_manual(
    name   = "Sample",
    values = c("Full" = 16, "LOO (excl. seed #55 chain)" = 17)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Leave-One-Out Chain Sensitivity: RDS-SS Estimates",
    subtitle = sprintf(
      "Full sample (n = %d) vs. LOO sample excluding seed #%s's chain (n = %d); N = %s",
      nrow(rd.dd), loo_config$seed_id, nrow(rd_loo),
      format(loo_config$N_F, big.mark = ",")),
    x = NULL,
    y = "RDS-SS prevalence estimate (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40", size = rel(0.85)),
    legend.position = "bottom"
  )

out_png <- here("output", "figures", "paper", "figA5_loo_chain.png")
dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
ggsave(out_png, p, width = 7.5, height = 4.5, dpi = 300)
cat("Saved figure:", out_png, "\n\n")
cat("Done.\n")

} # end skip_execution guard

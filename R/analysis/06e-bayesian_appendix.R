# 06e-bayesian_appendix.R
# Consolidate the appendix-only Bayesian MA results into a summary table
# for ESM_4. Created 2026-06-11 as a successor to 06d-consolidate_existing_results.R
# tailored to the minimal appendix-only Bayesian approach.
#
# Run AFTER R/analysis/06e-run-all.sh has produced the individual
# ma_sensitivity_*.RData files in output/.
#
# Produces:
#   output/tables/ESM_appendix_bayesian_summary.csv    # 8 indicators x (point, CI lower, CI upper) at N=50k, seed=degree
#   output/tables/ESM_appendix_bayesian_seedcheck.csv  # 1 indicator x 3 seed methods, showing posterior convergence
#
# Old script kept at R/analysis/06d-consolidate_existing_results.R for reference.

suppressMessages({
  library(here)
  library(tidyverse)
})
source(here("R", "utils", "helper_functions.R"))

# --------------- Configuration (mirrors 06e-run-all.sh) ---------------
POPSIZE <- 50000
HEADLINE_INDICATORS <- c(
  "document_withholding_rds",
  "pay_issues_rds",
  "threats_abuse_rds",
  "excessive_hours_rds",
  "access_to_help_rds",
  "whether_exploitation",
  "composite_risk",
  "sum_categories"
)
SEED_CHECK_INDICATOR <- "document_withholding_rds"
SEED_CHECK_METHODS   <- c("degree", "random", "sample")  # all three for the seed-check table

# --------------- Helpers ---------------

load_ma <- function(indicator, popsize, seed_selection) {
  combination_id <- paste(indicator, format(popsize, scientific = FALSE), seed_selection, sep = "_")
  result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))
  if (!file.exists(result_file)) {
    warning("MISSING: ", result_file)
    return(NULL)
  }
  env <- new.env()
  load(result_file, envir = env)
  if (!exists("ma_result", envir = env)) {
    warning("ma_result not found in ", result_file)
    return(NULL)
  }
  ma_result <- env$ma_result

  if (is.null(ma_result$estimate) || !inherits(ma_result$estimate, "rds.interval.estimate")) {
    warning("estimate missing or wrong class in ", result_file)
    return(NULL)
  }
  interval <- ma_result$estimate$interval
  # interval is a matrix; row "1" of a binary trait gives the prevalence of category "1"
  list(
    indicator       = indicator,
    population_size = popsize,
    seed_selection  = seed_selection,
    point_estimate  = interval[2],  # row 2, col 1: point estimate for category "1"
    ci_lower        = interval[4],  # row 2, col 2: 95% lower
    ci_upper        = interval[6],  # row 2, col 3: 95% upper
    se              = if(length(interval) >= 3) interval[3] else NA_real_
  )
}

format_pct_ci <- function(p, lo, hi) {
  sprintf("%.1f%% (%.1f%%, %.1f%%)", 100 * p, 100 * lo, 100 * hi)
}

# --------------- Headline table: 8 indicators @ N=50k, seed=degree ---------------

cat("=== Building headline appendix table (8 indicators @ N=", POPSIZE, ", seed=degree) ===\n", sep = "")
headline_rows <- map(HEADLINE_INDICATORS, ~ load_ma(.x, POPSIZE, "degree"))
headline_rows <- compact(headline_rows)
headline_df <- map_df(headline_rows, as_tibble)
if (nrow(headline_df) > 0) {
  headline_df <- headline_df %>%
    mutate(estimate_with_ci = format_pct_ci(point_estimate, ci_lower, ci_upper)) %>%
    select(indicator, population_size, seed_selection, point_estimate, ci_lower, ci_upper, se, estimate_with_ci)
}
print(headline_df)

dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
out_headline <- here("output", "tables", "ESM_appendix_bayesian_summary.csv")
write_csv(headline_df, out_headline)
cat("Saved:", out_headline, "\n")

# --------------- Seed-selection check: 1 indicator across 3 methods ---------------

cat("\n=== Building seed-selection check table (indicator=", SEED_CHECK_INDICATOR, ", N=", POPSIZE, ") ===\n", sep = "")
seedcheck_rows <- map(SEED_CHECK_METHODS, ~ load_ma(SEED_CHECK_INDICATOR, POPSIZE, .x))
seedcheck_rows <- compact(seedcheck_rows)
seedcheck_df <- map_df(seedcheck_rows, as_tibble)
if (nrow(seedcheck_df) > 0) {
  seedcheck_df <- seedcheck_df %>%
    mutate(estimate_with_ci = format_pct_ci(point_estimate, ci_lower, ci_upper)) %>%
    select(indicator, population_size, seed_selection, point_estimate, ci_lower, ci_upper, se, estimate_with_ci)
}
print(seedcheck_df)

out_seedcheck <- here("output", "tables", "ESM_appendix_bayesian_seedcheck.csv")
write_csv(seedcheck_df, out_seedcheck)
cat("Saved:", out_seedcheck, "\n")

# --------------- Brief on-screen interpretation ---------------

cat("\n=== Interpretation ===\n")
if (nrow(seedcheck_df) > 0) {
  unique_points <- length(unique(round(seedcheck_df$point_estimate, 6)))
  if (unique_points == 1) {
    cat("Seed-selection check: all 3 methods produced IDENTICAL point estimates -",
        "evidence the model is robust to seed-selection initialisation.\n")
  } else {
    cat("Seed-selection check: methods produced DIFFERENT point estimates -",
        "report variation in the appendix and discuss.\n")
  }
}
cat("Done.\n")

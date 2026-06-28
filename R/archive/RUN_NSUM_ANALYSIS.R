#!/usr/bin/env Rscript
# ==============================================================================
# RUNNER: Complete NSUM Analysis
# ==============================================================================
#
# This script runs the complete NSUM estimation workflow with:
# - All 5 NSUM indicators
# - Neighborhood bootstrap resampling
# - Gile's Sequential Sampling (SS) weights
# - 95% bootstrap confidence intervals
#
# Simply run: Rscript R/analysis/RUN_NSUM_ANALYSIS.R
# Or source: source("R/analysis/RUN_NSUM_ANALYSIS.R")
#
# ==============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║         NSUM POPULATION SIZE ESTIMATION WORKFLOW              ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Check if running interactively or from command line
if (!interactive()) {
  cat("Running in batch mode...\n\n")
}

# Source the complete workflow
workflow_file <- here::here("R", "analysis", "NSUM_complete_workflow.R")

if (!file.exists(workflow_file)) {
  stop("Workflow file not found: ", workflow_file)
}

cat("Loading workflow from:", workflow_file, "\n\n")

# Run the workflow
results <- source(workflow_file)$value

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                  ANALYSIS COMPLETE                            ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Results stored in: output/nsum_complete/\n")
cat("\n")

# Return results invisibly
invisible(results)

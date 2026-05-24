# Quick test of NSUM workflow with B=50 bootstrap samples

library(here)

# Load workflow with modified config (small B for testing)
source(here("R", "analysis", "NSUM_complete_workflow.R"))

# Override config for quick test
config$bootstrap_samples <- 50
config$use_parallel <- FALSE
config$output_dir <- here("output", "nsum_test")
config$verbose <- TRUE

cat("\n=== TESTING WITH B=50 BOOTSTRAP SAMPLES ===\n\n")

# Re-run steps with test config...
# (The main workflow already ran with default config=1000)
# This is just for validation

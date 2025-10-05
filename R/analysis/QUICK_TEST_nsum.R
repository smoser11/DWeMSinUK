#!/usr/bin/env Rscript
# ==============================================================================
# QUICK TEST: NSUM Workflow (B=50 bootstrap samples)
# ==============================================================================
#
# Fast test version of the complete NSUM workflow
# Uses only 50 bootstrap samples for quick validation
#
# Run: Rscript R/analysis/QUICK_TEST_nsum.R
#
# ==============================================================================

library(here)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║              QUICK TEST: NSUM WORKFLOW (B=50)                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Load the workflow script
workflow_script <- here("R", "analysis", "NSUM_complete_workflow.R")

# Read the script
workflow_code <- readLines(workflow_script)

# Modify config for quick test
workflow_code <- gsub("bootstrap_samples = 1000", "bootstrap_samples = 50", workflow_code)
workflow_code <- gsub('output_dir = here\\("output", "nsum_complete"\\)',
                     'output_dir = here("output", "nsum_quick_test")',
                     workflow_code)

# Create temporary file
temp_file <- tempfile(fileext = ".R")
writeLines(workflow_code, temp_file)

# Run modified workflow
cat("Running workflow with B=50 bootstrap samples...\n\n")
source(temp_file)

# Clean up
unlink(temp_file)

cat("\n✓ Quick test completed!\n")
cat("  Results saved to: output/nsum_quick_test/\n\n")

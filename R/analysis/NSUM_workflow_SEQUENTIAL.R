# ==============================================================================
# NSUM WORKFLOW - SEQUENTIAL VERSION (No Parallel Processing)
# ==============================================================================
#
# This is the same workflow as NSUM_complete_workflow.R but with parallel
# processing DISABLED. Use this if you experience issues with parallel clusters.
#
# ==============================================================================

library(here)

# Load the main workflow
workflow_script <- here("R", "analysis", "NSUM_complete_workflow.R")
workflow_code <- readLines(workflow_script)

# Disable parallel processing
workflow_code <- gsub("use_parallel = TRUE", "use_parallel = FALSE", workflow_code)

# Create temporary file
temp_file <- tempfile(fileext = ".R")
writeLines(workflow_code, temp_file)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║         NSUM WORKFLOW (Sequential - No Parallel)              ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Running workflow without parallel processing...\n")
cat("(This will be slower but more stable)\n\n")

# Run workflow
source(temp_file)

# Clean up
unlink(temp_file)

cat("\n✓ Sequential workflow completed!\n\n")

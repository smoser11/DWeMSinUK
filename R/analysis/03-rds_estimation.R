# 03-rds_estimation.R
# RDS Estimation Coordinator Script
# This script coordinates the modular RDS analysis components

cat("=== RDS Analysis Pipeline Coordinator ===\n")
cat("Domestic Worker Exploitation and Modern Slavery in UK\n")
cat("Modular RDS estimation using comparable indicators\n\n")

# Load required libraries
library(here)

# Configuration for RDS analysis
rds_config <- list(
  # Analysis components to run
  run_basic_rds = TRUE,
  run_model_assisted = FALSE,  # Set to FALSE by default (computationally expensive)
  run_population_size = FALSE, # Set to FALSE by default (computationally expensive)
  run_convergence = TRUE,
  run_bootstrap = FALSE,       # Set to FALSE by default (computationally expensive)
  
  # Basic parameters
  outcome_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds"),
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  pop_sizes = c(50000, 100000, 980000, 1740000),
  
  # Computational parameters
  force_recompute = FALSE,
  parallel_cores = 4,
  bootstrap_samples = 100
)

cat("RDS Analysis Configuration:\n")
for (component in names(rds_config)[1:6]) {
  cat("-", component, ":", rds_config[[component]], "\n")
}
cat("\n")

# Set execution flag to prevent individual scripts from running
skip_execution <- TRUE

# Component 1: Basic RDS Estimation (RDS-I, RDS-II, RDS-SS)
if (rds_config$run_basic_rds) {
  cat("Step 1: Basic RDS estimation (RDS-I, RDS-II, RDS-SS)...\n")
  
  tryCatch({
    source(here("R", "analysis", "03a-rds_basic_estimation.R"))
    basic_results <- run_basic_rds_estimation(
      outcome_vars = rds_config$outcome_vars,
      legacy_vars = rds_config$legacy_vars,
      pop_sizes = rds_config$pop_sizes,
      force_recompute = rds_config$force_recompute
    )
    cat("✓ Basic RDS estimation completed\n\n")
  }, error = function(e) {
    cat("✗ Basic RDS estimation failed:", e$message, "\n\n")
  })
}

# Component 2: Model-Assisted Estimation (expensive)
if (rds_config$run_model_assisted) {
  cat("Step 2: Model-Assisted RDS estimation...\n")
  cat("Warning: This is computationally expensive and will take significant time.\n")
  
  response <- readline(prompt = "Continue with Model-Assisted estimation? (y/N): ")
  if (tolower(response) == "y") {
    tryCatch({
      source(here("R", "analysis", "03b-rds_model_assisted.R"))
      ma_results <- run_model_assisted_estimation(
        outcome_vars = rds_config$outcome_vars,
        legacy_vars = rds_config$legacy_vars,
        pop_sizes = c(980000),  # Conservative: just baseline
        parallel_cores = rds_config$parallel_cores,
        force_recompute = rds_config$force_recompute
      )
      cat("✓ Model-Assisted estimation completed\n\n")
    }, error = function(e) {
      cat("✗ Model-Assisted estimation failed:", e$message, "\n\n")
    })
  } else {
    cat("Model-Assisted estimation skipped by user\n\n")
  }
}

# Component 3: Population Size Estimation (expensive)
if (rds_config$run_population_size) {
  cat("Step 3: Population size estimation (SS-PSE)...\n")
  cat("Warning: This is computationally expensive.\n")
  
  response <- readline(prompt = "Continue with Population Size estimation? (y/N): ")
  if (tolower(response) == "y") {
    tryCatch({
      source(here("R", "analysis", "03c-rds_population_size.R"))
      popsize_results <- run_population_size_estimation(
        prior_sizes = c(980000),
        force_recompute = rds_config$force_recompute
      )
      cat("✓ Population size estimation completed\n\n")
    }, error = function(e) {
      cat("✗ Population size estimation failed:", e$message, "\n\n")
    })
  } else {
    cat("Population size estimation skipped by user\n\n")
  }
}

# Component 4: Convergence Diagnostics
if (rds_config$run_convergence) {
  cat("Step 4: Convergence diagnostics...\n")
  
  tryCatch({
    source(here("R", "analysis", "03d-rds_convergence.R"))
    convergence_results <- run_convergence_diagnostics(
      outcome_vars = rds_config$outcome_vars,
      legacy_vars = rds_config$legacy_vars
    )
    diagnostic_plots <- generate_diagnostic_plots()
    cat("✓ Convergence diagnostics completed\n\n")
  }, error = function(e) {
    cat("✗ Convergence diagnostics failed:", e$message, "\n\n")
  })
}

# Component 5: Bootstrap Uncertainty (very expensive)
if (rds_config$run_bootstrap) {
  cat("Step 5: Bootstrap uncertainty estimation...\n")
  cat("Warning: This is very computationally expensive and will take substantial time.\n")
  
  response <- readline(prompt = "Continue with Bootstrap analysis? (y/N): ")
  if (tolower(response) == "y") {
    tryCatch({
      source(here("R", "analysis", "03e-rds_bootstrap.R"))
      bootstrap_results <- run_neighborhood_bootstrap(
        n_bootstrap = rds_config$bootstrap_samples,
        force_recompute = rds_config$force_recompute
      )
      bootstrap_summary <- create_bootstrap_summary()
      cat("✓ Bootstrap uncertainty estimation completed\n\n")
    }, error = function(e) {
      cat("✗ Bootstrap uncertainty estimation failed:", e$message, "\n\n")
    })
  } else {
    cat("Bootstrap analysis skipped by user\n\n")
  }
}

# Summary and Results
cat("=== RDS ANALYSIS PIPELINE COMPLETED ===\n")
cat("Results available in:\n")
cat("- Database: output/rds_results_database.RDS\n")
cat("- Tables: output/tables/\n") 
cat("- Figures: output/figures/\n")
cat("\nTo run individual components:\n")
cat("- source('R/analysis/03a-rds_basic_estimation.R')\n")
cat("- source('R/analysis/03b-rds_model_assisted.R')     # Expensive\n")
cat("- source('R/analysis/03c-rds_population_size.R')    # Expensive\n") 
cat("- source('R/analysis/03d-rds_convergence.R')\n")
cat("- source('R/analysis/03e-rds_bootstrap.R')          # Very expensive\n")
cat("\nTo access results programmatically:\n")
cat("- results_db <- readRDS('output/rds_results_database.RDS')\n")
cat("- basic_results <- get_basic_rds_results()\n")
cat("- convergence <- load_convergence_results()\n")

# Clean up
rm(skip_execution)
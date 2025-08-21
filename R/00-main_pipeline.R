# 00-main_pipeline.R
# Main Analysis Pipeline for DWeMSinUK Project
# Domestic Worker Exploitation and Modern Slavery in UK
# Network Scale-Up Method (NSUM) and Respondent-Driven Sampling (RDS)

cat("=== DWeMSinUK Analysis Pipeline ===\n")
cat("Domestic Worker Exploitation and Modern Slavery in UK\n") 
cat("Using RDS and NSUM statistical methods\n\n")

# Load required libraries
library(here)

# Source utility functions first
cat("Loading utility functions...\n")
source(here("R", "utils", "helper_functions.R"))

# Initialize project environment
setup_project_environment()

# Pipeline configuration
pipeline_config <- list(
  run_data_cleaning = TRUE,
  run_data_preparation = TRUE, 
  run_rds_estimation = TRUE,
  run_bootstrap_analysis = TRUE,
  run_nsum_estimation = TRUE,
  
  # Analysis parameters (consolidated structure)
  preferred_rds_method = "RDS_SS",
  preferred_nsum_method = "weighted",
  include_bootstrap = TRUE,
  n_bootstrap = 1000,
  population_sizes = c(50000, 100000, 980000, 1740000),
  main_population_size = 980000,
  
  # Output options
  save_intermediate_results = TRUE,
  create_summary_report = TRUE,
  generate_comparison_tables = TRUE
)

# Set execution flag to prevent scripts from running when sourced
skip_execution <- TRUE

cat("Pipeline configuration:\n")
for (step in names(pipeline_config)[1:5]) {
  cat("-", step, ":", pipeline_config[[step]], "\n")
}
cat("- Preferred models: RDS-SS,", pipeline_config$preferred_nsum_method, "NSUM\n")
cat("- Main population size:", format(pipeline_config$main_population_size, big.mark = ","), "\n")
cat("- Bootstrap samples:", pipeline_config$n_bootstrap, "\n\n")

# Step 1: Data Cleaning and Import
if (pipeline_config$run_data_cleaning) {
  cat("Step 1: Data cleaning and import...\n")
  
  tryCatch({
    source(here("R", "data_processing", "01-data_cleaning.R"))
    cat("✓ Data cleaning completed\n")
  }, error = function(e) {
    cat("✗ Data cleaning failed:", e$message, "\n")
    return(FALSE)
  })
}

# Step 2: Data Preparation and Indicator Creation
if (pipeline_config$run_data_preparation) {
  cat("\nStep 2: Data preparation and CE's comparable indicators...\n")
  
  tryCatch({
    source(here("R", "data_processing", "02-data_preparation.R"))
    cat("✓ Data preparation completed\n")
  }, error = function(e) {
    cat("✗ Data preparation failed:", e$message, "\n")
    return(FALSE)
  })
}

# Step 3: RDS Estimation Analysis
if (pipeline_config$run_rds_estimation) {
  cat("\nStep 3: RDS estimation analysis...\n")
  
  tryCatch({
    source(here("R", "analysis", "03-rds_estimation.R"))
    cat("✓ RDS estimation completed\n")
  }, error = function(e) {
    cat("✗ RDS estimation failed:", e$message, "\n")
    return(FALSE)
  })
}

# Step 4: Bootstrap Analysis (Confidence Intervals)
if (pipeline_config$run_bootstrap_analysis && pipeline_config$include_bootstrap) {
  cat("\nStep 4: Bootstrap analysis for confidence intervals...\n")
  
  tryCatch({
    source(here("R", "analysis", "04-bootstrap_analysis.R"))
    cat("✓ Bootstrap analysis completed\n")
  }, error = function(e) {
    cat("✗ Bootstrap analysis failed:", e$message, "\n")
    return(FALSE)
  })
}

# Step 5: NSUM Estimation Analysis  
if (pipeline_config$run_nsum_estimation) {
  cat("\nStep 5: NSUM estimation analysis...\n")
  
  tryCatch({
    source(here("R", "analysis", "05-nsum_estimation.R"))
    cat("✓ NSUM estimation completed\n")
  }, error = function(e) {
    cat("✗ NSUM estimation failed:", e$message, "\n")
    return(FALSE)
  })
}

# Generate summary report
if (pipeline_config$create_summary_report) {
  cat("\nGenerating summary report...\n")
  
  # Create a simple text summary
  report_file <- here("output", "reports", paste0("pipeline_summary_", Sys.Date(), ".txt"))
  
  cat("DWeMSinUK Analysis Pipeline Summary", 
      paste0("Generated: ", Sys.time()),
      "",
      "Files processed:",
      paste("- Cleaned data:", ifelse(file.exists(here("data", "processed", "cleaned_data.RData")), "✓", "✗")),
      paste("- Prepared data:", ifelse(file.exists(here("data", "processed", "prepared_data.RData")), "✓", "✗")),
      paste("- RDS results:", ifelse(file.exists(here("output", "rds_estimation_results.RData")), "✓", "✗")),
      paste("- Bootstrap results:", ifelse(file.exists(here("output", "bootstrap_results.RData")), "✓", "✗")),
      paste("- NSUM results:", ifelse(file.exists(here("output", "nsum_estimation_results.RData")), "✓", "✗")),
      "",
      "Analysis parameters:",
      paste("- Preferred models:", pipeline_config$preferred_rds_method, "&", pipeline_config$preferred_nsum_method, "NSUM"),
      paste("- Main population size:", format(pipeline_config$main_population_size, big.mark = ",")),
      paste("- Population scenarios:", paste(pipeline_config$population_sizes, collapse = ", ")),
      paste("- Bootstrap samples:", pipeline_config$n_bootstrap),
      "",
      "Output locations:",
      "- Tables: output/tables/",
      "- Figures: output/figures/",  
      "- Results: output/",
      "",
      "Next steps:",
      "1. Review results in output/ directory",
      "2. Check figures in output/figures/",
      "3. Examine summary tables in output/tables/",
      "4. Run individual analysis scripts for specific investigations",
      "",
      sep = "\n",
      file = report_file
  )
  
  cat("✓ Summary report saved to:", report_file, "\n")
}

# Clean up environment
rm(skip_execution)

cat("\n=== PIPELINE COMPLETED SUCCESSFULLY ===\n")
cat("Results available in:\n")
cat("- Data: data/processed/\n")  
cat("- Results: output/\n")
cat("- Tables: output/tables/\n")
cat("- Figures: output/figures/\n")
cat("- Reports: output/reports/\n")
cat("\nTo run individual analyses:\n")
cat("- source('R/analysis/03-rds_estimation.R')    # RDS-SS estimation + sensitivity\n")
cat("- source('R/analysis/04-bootstrap_analysis.R') # Confidence intervals\n") 
cat("- source('R/analysis/05-nsum_estimation.R')   # NSUM estimation + RDS comparison\n")
cat("\nSee CLAUDE.md for detailed documentation.\n")
# 06b-bayesian_estimators.R
# Bayesian RDS Estimators (MA.estimates) with Built-in Credible Intervals
# PRODUCTION VERSION: Clean, comprehensive Bayesian analysis
#
# INCLUDES:
# - MA.estimates() with enhanced parameters for numeric/ordinal variables
# - Different parameter sets for binary vs numeric/ordinal indicators
# - Built-in Bayesian credible intervals (no bootstrap needed)
# - All comparable indicators + composite risk + sum_categories
# - Single population size (980K baseline)
# - Publication-ready outputs

cat("=== BAYESIAN RDS ESTIMATORS (Production) ===\n")
cat("Method: MA.estimates with enhanced parameters for numeric traits\n")
cat("Indicators: All comparable indicators + composite risk + sum_categories\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
                                                                                                                           
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data (with weights)
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data with weights\n")
}

# ============================================================================
# BAYESIAN CONFIGURATION WITH ENHANCED PARAMETERS
# ============================================================================

bayesian_config <- list(
  # Bayesian method only
  methods = c("MA_estimates"),
  
  # Population size (single baseline)
  population_size = 980000,  # EU baseline estimate
  
  # All indicators (binary + numeric/ordinal)
  binary_indicators = get_comparable_indicators()$rds_vars,
  numeric_indicators = c("composite_risk", "sum_categories"),
  additional_indicators = c("whether_exploitation"),  # Binary but important
  
  # Enhanced parameters for numeric/ordinal variables (YOUR PROVEN SETTINGS)
  enhanced_params = list(
    number.of.iterations = 3,        # Enhanced: 3 iterations for stability
    M1 = 75,                         # Enhanced: larger network samples  
    M2 = 50,                         # Enhanced: more RDS samples
    MPLE.samplesize = 150000,        # Enhanced: better initialization
    SAN.maxit = 25,                  # Enhanced: more annealing steps
    SAN.nsteps = 2^22,               # Enhanced: 8x longer burn-in
    sim.interval = 25000,            # Enhanced: more spacing
    seed.selection = "random",       # Enhanced: may work better for numeric
    parallel = 1,
    verbose = TRUE,
    full.output = TRUE,
    seed = 42
  ),
  
  # Standard parameters for binary variables
  standard_params = list(
    number.of.iterations = 2,        # Standard: 2 iterations sufficient for binary
    M1 = 50,                         # Standard: default network samples
    M2 = 25,                         # Standard: default RDS samples
    MPLE.samplesize = 50000,         # Standard: default initialization
    SAN.maxit = 10,                  # Standard: default annealing
    SAN.nsteps = 2^19,               # Standard: default burn-in
    sim.interval = 10000,            # Standard: default spacing
    seed.selection = "degree",       # Standard: default seed selection
    parallel = 1,
    verbose = TRUE,
    full.output = TRUE,
    seed = 42
  ),
  
  # Output control
  save_detailed_results = TRUE,
  create_comparison_tables = TRUE,
  save_individual_files = TRUE
)

# Get all indicators to process
all_indicators <- c(
  bayesian_config$binary_indicators,
  bayesian_config$numeric_indicators, 
  bayesian_config$additional_indicators
)

cat("Bayesian configuration:\n")
cat("- Method: MA.estimates\n")
cat("- Population size:", format(bayesian_config$population_size, big.mark = ","), "\n")
cat("- Binary indicators:", length(bayesian_config$binary_indicators), "\n")
cat("- Numeric/ordinal indicators:", length(bayesian_config$numeric_indicators), "\n")
cat("- Total indicators:", length(all_indicators), "\n")
cat("- Enhanced parameters for:", paste(bayesian_config$numeric_indicators, collapse = ", "), "\n\n")

# ============================================================================
# PARAMETER SELECTION FUNCTION
# ============================================================================

get_ma_parameters <- function(indicator) {
  # Use enhanced parameters for numeric/ordinal variables
  if (indicator %in% bayesian_config$numeric_indicators) {
    params <- bayesian_config$enhanced_params
    param_type <- "enhanced"
    cat("  Using ENHANCED parameters for numeric/ordinal variable\n")
  } else {
    params <- bayesian_config$standard_params
    param_type <- "standard"
    cat("  Using STANDARD parameters for binary variable\n")
  }
  
  params$param_type <- param_type
  return(params)
}

# ============================================================================
# BAYESIAN ESTIMATION FUNCTION
# ============================================================================

estimate_ma_bayesian <- function(indicator, population_size) {
  tryCatch({
    cat("Processing MA.estimates for:", indicator, "\n")
    cat("  Population size:", format(population_size, big.mark = ","), "\n")
    
    # Get appropriate parameters for this indicator type
    params <- get_ma_parameters(indicator)
    
    cat("  Parameters:", params$param_type, "- Iterations:", params$number.of.iterations, 
        "M1/M2:", params$M1, "/", params$M2, "\n")
    
    # Run MA.estimates with appropriate parameters
    ma_result <- MA.estimates(
      rd.dd, 
      trait.variable = indicator,
      N = population_size,
      number.of.iterations = params$number.of.iterations,
      M1 = params$M1,
      M2 = params$M2,
      MPLE.samplesize = params$MPLE.samplesize,
      SAN.maxit = params$SAN.maxit,
      SAN.nsteps = params$SAN.nsteps,
      sim.interval = params$sim.interval,
      seed.selection = params$seed.selection,
      parallel = params$parallel,
      verbose = params$verbose,
      full.output = params$full.output,
      seed = params$seed
    )
    
    # Extract values using your proven direct array indexing method
    if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
      point_estimate <- ma_result$estimate$interval[2]  # Point estimate
      ci_lower <- ma_result$estimate$interval[4]        # Lower 95% CI
      ci_upper <- ma_result$estimate$interval[6]        # Upper 95% CI
      bayesian_se <- if(length(ma_result$estimate$interval) >= 3) ma_result$estimate$interval[3] else NA
      
      cat("  Estimate:", sprintf("%.1f%% (%.1f–%.1f)", 
                                point_estimate * 100, ci_lower * 100, ci_upper * 100), "\n")
    } else {
      point_estimate <- ci_lower <- ci_upper <- bayesian_se <- NA
      cat("  ERROR: Could not extract estimate\n")
    }
    
    # Save individual MA result object for diagnostics
    if (bayesian_config$save_individual_files) {
      ma_file <- here("output", paste0("ma_result_", indicator, ".RData"))
      save(ma_result, file = ma_file)
    }
    
    return(list(
      method = "MA_estimates",
      indicator = indicator,
      estimate = point_estimate,
      se = bayesian_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = population_size,
      uncertainty_method = "bayesian_credible_interval",
      method_type = "bayesian",
      
      # Parameter information
      ma_parameters = params,
      parameter_type = params$param_type,
      iterations = params$number.of.iterations,
      M1 = params$M1,
      M2 = params$M2,
      
      # Metadata
      run_timestamp = Sys.time(),
      convergence_status = if(!is.na(point_estimate)) "success" else "failed"
    ))
    
  }, error = function(e) {
    cat("  ERROR in MA.estimates for", indicator, ":", e$message, "\n")
    return(list(
      method = "MA_estimates", 
      indicator = indicator,
      estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
      error = paste("MA.estimates failed:", e$message), 
      population_size = population_size, 
      method_type = "bayesian"
    ))
  })
}

# ============================================================================
# MAIN BAYESIAN ANALYSIS
# ============================================================================

run_bayesian_analysis <- function() {
  
  cat("=== Running Bayesian MA.estimates Analysis ===\n")
  
  all_results <- list()
  result_count <- 0
  
  for (indicator in all_indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    result_count <- result_count + 1
    result <- estimate_ma_bayesian(indicator, bayesian_config$population_size)
    all_results[[result_count]] <- result
    
    cat("\n")
  }
  
  cat("Completed", result_count, "Bayesian estimations\n")
  return(all_results)
}

# Process results into data frame
process_bayesian_results <- function(results_list) {
  
  # Helper function to safely extract values
  safe_extract <- function(result, field, default = NA) {
    if (is.null(result) || !is.list(result)) return(default)
    value <- result[[field]]
    if (is.null(value) || length(value) == 0) return(default)
    return(value)
  }
  
  results_df <- map_dfr(results_list, function(result) {
    if (is.null(result) || !is.list(result)) {
      return(data.frame())
    }
    
    data.frame(
      indicator = safe_extract(result, "indicator", "unknown"),
      method = safe_extract(result, "method", NA),
      method_type = safe_extract(result, "method_type", NA),
      estimate = safe_extract(result, "estimate", NA),
      se = safe_extract(result, "se", NA),
      ci_lower = safe_extract(result, "ci_lower", NA),
      ci_upper = safe_extract(result, "ci_upper", NA),
      population_size = safe_extract(result, "population_size", NA),
      uncertainty_method = safe_extract(result, "uncertainty_method", NA),
      parameter_type = safe_extract(result, "parameter_type", NA),
      iterations = safe_extract(result, "iterations", NA),
      M1 = safe_extract(result, "M1", NA),
      M2 = safe_extract(result, "M2", NA),
      convergence_status = safe_extract(result, "convergence_status", NA),
      error_msg = safe_extract(result, "error", NA),
      stringsAsFactors = FALSE
    )
  })
  
  # Add indicator labels and formatting
  indicators_info <- get_comparable_indicators()
  results_df <- results_df %>%
    mutate(
      indicator_clean = case_when(
        str_detect(indicator, "_rds$") ~ indicators_info$labels[str_remove(indicator, "_rds$")],
        indicator == "composite_risk" ~ "Composite risk score",
        indicator == "whether_exploitation" ~ "Overall exploitation indicator",
        indicator == "sum_categories" ~ "Risk exposure scale (ordinal)",
        TRUE ~ indicator
      ),
      method_clean = "MA.estimates",
      uncertainty_clean = "Bayesian CI",
      estimate_pct = estimate * 100,
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      se_pct = se * 100,
      estimate_with_ci = ifelse(
        !is.na(ci_lower) & !is.na(ci_upper),
        sprintf("%.1f%% (%.1f–%.1f)", estimate_pct, ci_lower_pct, ci_upper_pct),
        sprintf("%.1f%% (CI not available)", estimate_pct)
      ),
      parameter_description = case_when(
        parameter_type == "enhanced" ~ paste0("Enhanced (", iterations, " iter, M1=", M1, ", M2=", M2, ")"),
        parameter_type == "standard" ~ paste0("Standard (", iterations, " iter, M1=", M1, ", M2=", M2, ")"),
        TRUE ~ "Unknown"
      )
    ) %>%
    arrange(desc(parameter_type), desc(estimate_pct))  # Enhanced parameters first, then by estimate
  
  return(results_df)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Run analysis
cat("Starting Bayesian analysis...\n")
bayesian_results <- run_bayesian_analysis()

# Process results
cat("Processing results...\n")
bayesian_df <- process_bayesian_results(bayesian_results)

# Save results
if (bayesian_config$save_detailed_results) {
  save(bayesian_results, bayesian_df, bayesian_config, 
       file = here("output", "bayesian_estimators_results.RData"))
  write.csv(bayesian_df, here("output", "tables", "bayesian_estimators.csv"), row.names = FALSE)
  cat("Results saved to output/bayesian_estimators_results.RData\n")
  
  # Save enhanced parameters for reuse
  enhanced_params <- bayesian_config$enhanced_params
  enhanced_params$saved_timestamp <- Sys.time()
  enhanced_params$source_script <- "06b-bayesian_estimators.R"
  enhanced_params$validation_status <- "proven_for_numeric_traits"
  save(enhanced_params, file = here("output", "enhanced_ma_parameters.RData"))
  cat("Enhanced parameters saved for reuse\n")
}

# Summary
cat("\n=== BAYESIAN ANALYSIS COMPLETE ===\n")
cat("Total estimations:", nrow(bayesian_df), "\n")
cat("Method: MA.estimates\n")
cat("Population size:", format(bayesian_config$population_size, big.mark = ","), "\n")
cat("Indicators:", length(unique(bayesian_df$indicator_clean)), "\n")

# Show parameter usage
param_summary <- bayesian_df %>%
  filter(!is.na(estimate)) %>%
  count(parameter_type, name = "n_successful") %>%
  arrange(desc(n_successful))

cat("\nParameter usage:\n")
for(i in 1:nrow(param_summary)) {
  row <- param_summary[i,]
  cat("-", row$parameter_type, "parameters:", row$n_successful, "successful estimations\n")
}

# Show results by indicator type
cat("\nResults by indicator type:\n")

# Enhanced parameter results (numeric/ordinal)
enhanced_results <- bayesian_df %>%
  filter(parameter_type == "enhanced", !is.na(estimate)) %>%
  select(indicator_clean, estimate_with_ci, parameter_description)

if(nrow(enhanced_results) > 0) {
  cat("\nNumeric/Ordinal indicators (Enhanced parameters):\n")
  for(i in 1:nrow(enhanced_results)) {
    row <- enhanced_results[i,]
    cat(sprintf("%-30s: %s\n", row$indicator_clean, row$estimate_with_ci))
  }
}

# Standard parameter results (binary)
standard_results <- bayesian_df %>%
  filter(parameter_type == "standard", !is.na(estimate)) %>%
  select(indicator_clean, estimate_with_ci, parameter_description) %>%
  slice_head(n = 6)  # Show first 6

if(nrow(standard_results) > 0) {
  cat("\nBinary indicators (Standard parameters - sample):\n")
  for(i in 1:nrow(standard_results)) {
    row <- standard_results[i,]
    cat(sprintf("%-30s: %s\n", row$indicator_clean, row$estimate_with_ci))
  }
}

cat("\nBayesian analysis ready for comparison with frequentist methods!\n")
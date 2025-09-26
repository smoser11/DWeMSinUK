# 06c-bayesian_sensitivity.R
# Bayesian MA.estimates Sensitivity Analysis: Population Size & Seed Selection
# PRODUCTION VERSION: Comprehensive sensitivity analysis for MA.estimates
#
# SENSITIVITY DIMENSIONS:
# - Population sizes: 50K, 100K, 980K, 1.74M
# - Seed selection methods: random, degree, sample
# - Parameter types: Enhanced (numeric/ordinal) vs Standard (binary)
# - All indicators: Binary + numeric/ordinal
#
# BUILDS ON: 06b-bayesian_estimators.R (core analysis)
# EXTENDS WITH: Population and seed selection sensitivity

cat("=== BAYESIAN MA.ESTIMATES SENSITIVITY ANALYSIS ===\n")
cat("Sensitivity dimensions: Population size × Seed selection × Parameter type\n")
cat("Method: MA.estimates with comprehensive parameter exploration\n\n")

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

# Load enhanced parameters (from 06b if available)
enhanced_ma_params <- tryCatch({
  param_file <- here("output", "enhanced_ma_parameters.RData")
  if (file.exists(param_file)) {
    load(param_file)
    cat("Loaded enhanced MA parameters from 06b analysis\n")
    enhanced_params
  } else {
    cat("No enhanced parameters found, using built-in defaults\n")
    NULL
  }
}, error = function(e) {
  cat("Error loading enhanced parameters:", e$message, "\n")
  NULL
})

# ============================================================================
# SENSITIVITY ANALYSIS CONFIGURATION
# ============================================================================

sensitivity_config <- list(
  # Sensitivity dimensions
  population_sizes = c(50000, 100000, 980000, 1740000),
  population_labels = c("50K", "100K", "980K", "1.74M"),
  seed_selection_methods = c("random", "degree", "sample"),
  
  # Indicator categories for parameter selection
  binary_indicators = get_comparable_indicators()$rds_vars,
  numeric_indicators = c("composite_risk", "sum_categories"), 
  additional_indicators = c("whether_exploitation"),
  
  # Enhanced parameters (from proven settings or defaults)
  enhanced_params = if(!is.null(enhanced_ma_params)) enhanced_ma_params else list(
    number.of.iterations = 3,        # Enhanced: 3 iterations for stability
    M1 = 75,                         # Enhanced: larger network samples  
    M2 = 50,                         # Enhanced: more RDS samples
    MPLE.samplesize = 150000,        # Enhanced: better initialization
    SAN.maxit = 25,                  # Enhanced: more annealing steps
    SAN.nsteps = 2^22,               # Enhanced: 8x longer burn-in
    sim.interval = 25000,            # Enhanced: more spacing
    parallel = 1,
    verbose = FALSE,                 # Reduce output for batch runs
    full.output = TRUE,
    seed = 42
  ),
  
  # Standard parameters for comparison
  standard_params = list(
    number.of.iterations = 2,        # Standard: 2 iterations
    M1 = 50,                         # Standard: default network samples
    M2 = 25,                         # Standard: default RDS samples
    MPLE.samplesize = 50000,         # Standard: default initialization
    SAN.maxit = 10,                  # Standard: default annealing
    SAN.nsteps = 2^19,               # Standard: default burn-in
    sim.interval = 10000,            # Standard: default spacing
    parallel = 1,
    verbose = FALSE,                 # Reduce output for batch runs
    full.output = TRUE,
    seed = 42
  ),
  
  # Computational control
  max_parallel_jobs = 1,             # Keep single-core for stability
  save_individual_results = TRUE,
  batch_size = 5,                    # Process in batches to manage memory
  
  # Output control
  save_detailed_results = TRUE,
  create_sensitivity_tables = TRUE
)

# Get all indicators to process
all_indicators <- c(
  sensitivity_config$binary_indicators,
  sensitivity_config$numeric_indicators,
  sensitivity_config$additional_indicators
)

# Calculate total combinations
total_combinations <- length(sensitivity_config$population_sizes) * 
                     length(sensitivity_config$seed_selection_methods) * 
                     length(all_indicators)

cat("Sensitivity analysis configuration:\n")
cat("- Population sizes:", length(sensitivity_config$population_sizes), "\n")
cat("- Seed selection methods:", length(sensitivity_config$seed_selection_methods), "\n")
cat("- Total indicators:", length(all_indicators), "\n")
cat("- Binary indicators:", length(sensitivity_config$binary_indicators), "\n")
cat("- Numeric/ordinal indicators:", length(sensitivity_config$numeric_indicators), "\n")
cat("- Total combinations:", total_combinations, "\n")
cat("- Enhanced parameters available:", !is.null(enhanced_ma_params), "\n\n")

# ============================================================================
# PARAMETER SELECTION WITH SENSITIVITY
# ============================================================================

get_sensitivity_parameters <- function(indicator, seed_selection_method) {
  # Base parameters depend on indicator type
  if (indicator %in% sensitivity_config$numeric_indicators) {
    base_params <- sensitivity_config$enhanced_params
    param_type <- "enhanced"
  } else {
    base_params <- sensitivity_config$standard_params
    param_type <- "standard"
  }
  
  # Override seed selection method for sensitivity analysis
  params <- base_params
  params$seed.selection <- seed_selection_method
  params$param_type <- param_type
  params$seed_method <- seed_selection_method
  
  return(params)
}

# ============================================================================
# UTILITY FUNCTIONS FOR EXISTING RESULTS
# ============================================================================

check_existing_result <- function(indicator, population_size, seed_selection) {
  combination_id <- paste(indicator, format(population_size, scientific = FALSE), seed_selection, sep = "_")
  result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))
  return(file.exists(result_file))
}

load_existing_result <- function(indicator, population_size, seed_selection) {
  combination_id <- paste(indicator, format(population_size, scientific = FALSE), seed_selection, sep = "_")
  result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))

  if (file.exists(result_file)) {
    tryCatch({
      load(result_file)

      # Extract values from loaded ma_result
      if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
        point_estimate <- ma_result$estimate$interval[2]  # Point estimate
        ci_lower <- ma_result$estimate$interval[4]        # Lower 95% CI
        ci_upper <- ma_result$estimate$interval[6]        # Upper 95% CI
        bayesian_se <- if(length(ma_result$estimate$interval) >= 3) ma_result$estimate$interval[3] else NA
      } else {
        point_estimate <- ci_lower <- ci_upper <- bayesian_se <- NA
      }

      # Determine parameter type based on indicator
      params <- get_sensitivity_parameters(indicator, seed_selection)

      return(list(
        method = "MA_estimates",
        indicator = indicator,
        population_size = population_size,
        pop_label = sensitivity_config$population_labels[which(sensitivity_config$population_sizes == population_size)],
        seed_selection = seed_selection,
        estimate = point_estimate,
        se = bayesian_se,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        uncertainty_method = "bayesian_credible_interval",
        method_type = "bayesian",

        # Sensitivity parameters
        parameter_type = params$param_type,
        iterations = params$number.of.iterations,
        M1 = params$M1,
        M2 = params$M2,
        MPLE_samplesize = params$MPLE.samplesize,
        SAN_maxit = params$SAN.maxit,
        SAN_nsteps = params$SAN.nsteps,

        # Metadata
        combination_id = combination_id,
        run_timestamp = file.mtime(result_file),
        convergence_status = if(!is.na(point_estimate)) "success" else "failed",
        loaded_from_file = TRUE
      ))

    }, error = function(e) {
      cat("    ERROR loading existing result for", combination_id, ":", e$message, "\n")
      return(NULL)
    })
  } else {
    return(NULL)
  }
}

# ============================================================================
# SENSITIVITY ESTIMATION FUNCTION
# ============================================================================

estimate_ma_sensitivity <- function(indicator, population_size, seed_selection) {

  combination_id <- paste(indicator, format(population_size, scientific = FALSE), seed_selection, sep = "_")

  # Check if result already exists
  if (check_existing_result(indicator, population_size, seed_selection)) {
    cat("  SKIPPING (exists):", indicator, "- Pop:", format(population_size, big.mark = ","),
        "Seed:", seed_selection, "\n")
    existing_result <- load_existing_result(indicator, population_size, seed_selection)
    if (!is.null(existing_result)) {
      if (!is.na(existing_result$estimate)) {
        cat("    Loaded result:", sprintf("%.1f%% (%.1f–%.1f)",
                                        existing_result$estimate * 100,
                                        existing_result$ci_lower * 100,
                                        existing_result$ci_upper * 100), "\n")
      }
      return(existing_result)
    }
  }

  tryCatch({
    # Get appropriate parameters
    params <- get_sensitivity_parameters(indicator, seed_selection)
    
    if (params$param_type == "enhanced") {
      cat("  Enhanced params for", indicator, "- Pop:", format(population_size, big.mark = ","), 
          "Seed:", seed_selection, "\n")
    } else {
      cat("  Standard params for", indicator, "- Pop:", format(population_size, big.mark = ","), 
          "Seed:", seed_selection, "\n")
    }
    
    # Run MA.estimates with sensitivity parameters
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
    
    # Extract values using proven direct array indexing method
    if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
      point_estimate <- ma_result$estimate$interval[2]  # Point estimate
      ci_lower <- ma_result$estimate$interval[4]        # Lower 95% CI
      ci_upper <- ma_result$estimate$interval[6]        # Upper 95% CI
      bayesian_se <- if(length(ma_result$estimate$interval) >= 3) ma_result$estimate$interval[3] else NA
      
      if (!is.na(point_estimate)) {
        cat("    Result:", sprintf("%.1f%% (%.1f–%.1f)", 
                                  point_estimate * 100, ci_lower * 100, ci_upper * 100), "\n")
      }
    } else {
      point_estimate <- ci_lower <- ci_upper <- bayesian_se <- NA
      cat("    ERROR: Could not extract estimate\n")
    }
    
    # Save individual result for detailed analysis
    if (sensitivity_config$save_individual_results) {
      result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))
      save(ma_result, file = result_file)
    }
    
    return(list(
      method = "MA_estimates",
      indicator = indicator,
      population_size = population_size,
      pop_label = sensitivity_config$population_labels[which(sensitivity_config$population_sizes == population_size)],
      seed_selection = seed_selection,
      estimate = point_estimate,
      se = bayesian_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      uncertainty_method = "bayesian_credible_interval",
      method_type = "bayesian",
      
      # Sensitivity parameters
      parameter_type = params$param_type,
      iterations = params$number.of.iterations,
      M1 = params$M1,
      M2 = params$M2,
      MPLE_samplesize = params$MPLE.samplesize,
      SAN_maxit = params$SAN.maxit,
      SAN_nsteps = params$SAN.nsteps,
      
      # Metadata
      combination_id = combination_id,
      run_timestamp = Sys.time(),
      convergence_status = if(!is.na(point_estimate)) "success" else "failed"
    ))
    
  }, error = function(e) {
    cat("    ERROR in sensitivity analysis for", combination_id, ":", e$message, "\n")
    return(list(
      method = "MA_estimates",
      indicator = indicator,
      population_size = population_size,
      seed_selection = seed_selection,
      estimate = NA, se = NA, ci_lower = NA, ci_upper = NA,
      error = paste("MA.estimates failed:", e$message),
      method_type = "bayesian",
      combination_id = combination_id,
      convergence_status = "failed"
    ))
  })
}

# ============================================================================
# MAIN SENSITIVITY ANALYSIS
# ============================================================================

run_sensitivity_analysis <- function() {

  cat("=== Running MA.estimates Sensitivity Analysis ===\n")
  cat("Processing", total_combinations, "combinations...\n")

  # Count existing results
  existing_count <- 0
  for (indicator in all_indicators) {
    if (!(indicator %in% names(rd.dd))) next
    for (pop_size in sensitivity_config$population_sizes) {
      for (seed_method in sensitivity_config$seed_selection_methods) {
        if (check_existing_result(indicator, pop_size, seed_method)) {
          existing_count <- existing_count + 1
        }
      }
    }
  }

  cat("Found", existing_count, "existing results that will be loaded\n")
  cat("Need to compute", total_combinations - existing_count, "new combinations\n\n")

  all_results <- list()
  result_count <- 0
  successful_count <- 0
  skipped_count <- 0
  computed_count <- 0

  for (indicator in all_indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }

    cat("Processing indicator:", indicator, "\n")

    for (pop_size in sensitivity_config$population_sizes) {
      for (seed_method in sensitivity_config$seed_selection_methods) {

        result_count <- result_count + 1

        # Check if we're loading existing or computing new
        if (check_existing_result(indicator, pop_size, seed_method)) {
          skipped_count <- skipped_count + 1
        } else {
          computed_count <- computed_count + 1
        }

        result <- estimate_ma_sensitivity(indicator, pop_size, seed_method)
        all_results[[result_count]] <- result

        if (!is.na(result$estimate)) {
          successful_count <- successful_count + 1
        }

        # Memory management and progress reporting
        if (result_count %% 10 == 0) {
          gc()  # Garbage collection every 10 results
          cat("  Progress:", result_count, "/", total_combinations,
              "- Successful:", successful_count,
              "Skipped:", skipped_count,
              "Computed:", computed_count, "\n")
        }
      }
    }
    cat("\n")
  }

  cat("Sensitivity analysis completed:\n")
  cat("- Total combinations:", result_count, "\n")
  cat("- Successful estimations:", successful_count, "\n")
  cat("- Skipped (existing):", skipped_count, "\n")
  cat("- Newly computed:", computed_count, "\n")
  cat("- Success rate:", round(successful_count/result_count*100, 1), "%\n\n")

  return(all_results)
}

# Process sensitivity results
process_sensitivity_results <- function(results_list) {
  
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
      population_size = safe_extract(result, "population_size", NA),
      pop_label = safe_extract(result, "pop_label", NA),
      seed_selection = safe_extract(result, "seed_selection", NA),
      method = safe_extract(result, "method", NA),
      method_type = safe_extract(result, "method_type", NA),
      estimate = safe_extract(result, "estimate", NA),
      se = safe_extract(result, "se", NA),
      ci_lower = safe_extract(result, "ci_lower", NA),
      ci_upper = safe_extract(result, "ci_upper", NA),
      uncertainty_method = safe_extract(result, "uncertainty_method", NA),
      parameter_type = safe_extract(result, "parameter_type", NA),
      iterations = safe_extract(result, "iterations", NA),
      M1 = safe_extract(result, "M1", NA),
      M2 = safe_extract(result, "M2", NA),
      convergence_status = safe_extract(result, "convergence_status", NA),
      combination_id = safe_extract(result, "combination_id", NA),
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
      sensitivity_combo = paste0(pop_label, "_", seed_selection, "_", parameter_type)
    ) %>%
    arrange(indicator_clean, population_size, seed_selection, parameter_type)
  
  return(results_df)
}

# ============================================================================
# SENSITIVITY ANALYSIS FUNCTIONS
# ============================================================================

# Create population sensitivity table
create_population_sensitivity_table <- function(sensitivity_df) {
  
  cat("Creating population sensitivity table...\n")
  
  pop_sensitivity <- sensitivity_df %>%
    filter(!is.na(estimate), seed_selection == "random") %>%  # Use random seed as baseline
    select(indicator_clean, pop_label, parameter_type, estimate_with_ci) %>%
    pivot_wider(names_from = pop_label, values_from = estimate_with_ci) %>%
    arrange(indicator_clean, parameter_type)
  
  return(pop_sensitivity)
}

# Create seed selection sensitivity table
create_seed_sensitivity_table <- function(sensitivity_df) {
  
  cat("Creating seed selection sensitivity table...\n")
  
  seed_sensitivity <- sensitivity_df %>%
    filter(!is.na(estimate), pop_label == "980K") %>%  # Use 980K as baseline
    select(indicator_clean, seed_selection, parameter_type, estimate_with_ci) %>%
    pivot_wider(names_from = seed_selection, values_from = estimate_with_ci) %>%
    arrange(indicator_clean, parameter_type)
  
  return(seed_sensitivity)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Run sensitivity analysis
cat("Starting sensitivity analysis...\n")
sensitivity_results <- run_sensitivity_analysis()

# Process results
cat("Processing sensitivity results...\n")
sensitivity_df <- process_sensitivity_results(sensitivity_results)

# Create sensitivity tables
pop_sensitivity_table <- create_population_sensitivity_table(sensitivity_df)
seed_sensitivity_table <- create_seed_sensitivity_table(sensitivity_df)

# Save results
if (sensitivity_config$save_detailed_results) {
  save(sensitivity_results, sensitivity_df, sensitivity_config,
       pop_sensitivity_table, seed_sensitivity_table,
       file = here("output", "bayesian_sensitivity_results.RData"))
  
  write.csv(sensitivity_df, here("output", "tables", "bayesian_sensitivity_full.csv"), row.names = FALSE)
  write.csv(pop_sensitivity_table, here("output", "tables", "population_sensitivity.csv"), row.names = FALSE)
  write.csv(seed_sensitivity_table, here("output", "tables", "seed_sensitivity.csv"), row.names = FALSE)
  
  cat("Results saved to output/bayesian_sensitivity_results.RData\n")
}

# Summary analysis
cat("\n=== SENSITIVITY ANALYSIS COMPLETE ===\n")
cat("Total combinations:", nrow(sensitivity_df), "\n")
cat("Successful estimations:", sum(!is.na(sensitivity_df$estimate)), "\n")

# Parameter type performance
param_performance <- sensitivity_df %>%
  filter(!is.na(estimate)) %>%
  count(parameter_type, name = "successful_runs") %>%
  arrange(desc(successful_runs))

cat("\nParameter type performance:\n")
for(i in 1:nrow(param_performance)) {
  row <- param_performance[i,]
  cat("-", row$parameter_type, "parameters:", row$successful_runs, "successful runs\n")
}

# Population size effects (for enhanced parameters)
if(any(!is.na(sensitivity_df$estimate) & sensitivity_df$parameter_type == "enhanced")) {
  cat("\nPopulation size effects (enhanced parameters, numeric indicators):\n")
  
  pop_effects <- sensitivity_df %>%
    filter(!is.na(estimate), parameter_type == "enhanced", 
           seed_selection == "random", 
           indicator %in% sensitivity_config$numeric_indicators) %>%
    select(indicator_clean, pop_label, estimate_with_ci) %>%
    slice_head(n = 8)  # Show first few results
  
  for(i in 1:nrow(pop_effects)) {
    row <- pop_effects[i,]
    cat(sprintf("%-25s %6s: %s\n", row$indicator_clean, row$pop_label, row$estimate_with_ci))
  }
}

# Seed selection effects
if(any(!is.na(sensitivity_df$estimate))) {
  cat("\nSeed selection effects (980K population, sample indicators):\n")
  
  seed_effects <- sensitivity_df %>%
    filter(!is.na(estimate), pop_label == "980K") %>%
    select(indicator_clean, seed_selection, parameter_type, estimate_with_ci) %>%
    slice_head(n = 6)  # Show first few results
  
  for(i in 1:nrow(seed_effects)) {
    row <- seed_effects[i,]
    cat(sprintf("%-25s %-8s %s: %s\n", row$indicator_clean, row$seed_selection, 
                row$parameter_type, row$estimate_with_ci))
  }
}

cat("\n=== SENSITIVITY ANALYSIS READY FOR COMPREHENSIVE COMPARISON ===\n")
cat("Files available:\n")
cat("- bayesian_sensitivity_results.RData (full results)\n") 
cat("- tables/population_sensitivity.csv (population effects)\n")
cat("- tables/seed_sensitivity.csv (seed selection effects)\n")
cat("- tables/bayesian_sensitivity_full.csv (complete data)\n\n")

cat("Ready for Step 3: Comprehensive comparison across all methods!\n")
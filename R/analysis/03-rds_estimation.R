# 03-rds_estimation.R
# Comprehensive RDS Estimation Analysis
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# Focus: RDS-SS as preferred method for main text
# Includes: Basic estimation, model-assisted, sensitivity analysis for appendices

cat("=== RDS Estimation Analysis ===\n")
cat("Preferred model: RDS-SS with comparable indicators\n")
cat("Robustness analysis for appendices\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(parallel)
library(ggplot2)
library(kableExtra)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Analysis configuration
rds_config <- list(
  # Main analysis (for main text)
  preferred_method = "RDS_SS",
  main_population_size = 980000,  # EU baseline
  
  # Methods to run
  run_basic_estimation = TRUE,
  run_model_assisted = FALSE,     # Computationally expensive - set TRUE if needed
  run_population_size_sensitivity = TRUE,
  run_convergence_diagnostics = TRUE,
  run_method_comparison = TRUE,   # For appendices
  
  # Variables (CE's comparable indicators)
  outcome_vars = get_comparable_indicators()$rds_vars,
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  
  # Population scenarios for sensitivity
  population_sizes = get_population_parameters()$sizes,
  
  # Computational parameters
  force_recompute = FALSE,
  parallel_cores = 4,
  
  # Output control
  save_tables = TRUE,
  save_plots = TRUE,
  create_appendix_materials = TRUE
)

cat("Configuration loaded:\n")
cat("- Preferred method:", rds_config$preferred_method, "\n")
cat("- Main population size:", format(rds_config$main_population_size, big.mark = ","), "\n")
cat("- Outcome variables:", length(rds_config$outcome_vars), "comparable indicators\n")
cat("- Population scenarios:", length(rds_config$population_sizes), "sizes\n\n")

# Initialize results database
results_db <- load_rds_results_database()

# ============================================================================
# BASIC RDS ESTIMATION (RDS-I, RDS-II, RDS-SS)
# ============================================================================

run_basic_rds_estimation <- function(outcome_vars, population_sizes, force_recompute = FALSE) {
  
  cat("=== Basic RDS Estimation ===\n")
  cat("Methods: RDS-I, RDS-II, RDS-SS\n")
  
  all_results <- list()
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(rd.dd))) {
      cat("Warning: Variable", outcome_var, "not found in data\n")
      next
    }
    
    var_results <- list()
    
    for (pop_size in population_sizes) {
      
      # Create parameter ID for caching
      param_id <- create_parameter_id("BASIC", outcome_var, pop_size)
      
      if (!force_recompute && param_id %in% names(results_db)) {
        cat("  Using cached results for", param_id, "\n")
        var_results[[as.character(pop_size)]] <- results_db[[param_id]]
        next
      }
      
      tryCatch({
        
        # RDS-I estimation
        rds_I <- RDS.I.estimates(rds.data = rd.dd, 
                                outcome.variable = outcome_var,
                                N = pop_size)
        
        # RDS-II estimation  
        rds_II <- RDS.II.estimates(rds.data = rd.dd,
                                  outcome.variable = outcome_var,
                                  N = pop_size)
        
        # RDS-SS estimation (preferred)
        rds_SS <- RDS.SS.estimates(rds.data = rd.dd,
                                  outcome.variable = outcome_var, 
                                  N = pop_size)
        
        # Compile results
        result <- list(
          variable = outcome_var,
          population_size = pop_size,
          RDS_I = rds_I$estimate,
          RDS_II = rds_II$estimate, 
          RDS_SS = rds_SS$estimate,
          
          # Store full objects for bootstrap later
          rds_I_object = rds_I,
          rds_II_object = rds_II,
          rds_SS_object = rds_SS,
          
          # Metadata
          sample_size = nrow(rd.dd),
          timestamp = Sys.time()
        )
        
        var_results[[as.character(pop_size)]] <- result
        
        # Cache result
        results_db <- save_to_rds_database(results_db, result, param_id)
        
        cat("  Completed:", param_id, "\n")
        
      }, error = function(e) {
        cat("  Error in", param_id, ":", e$message, "\n")
        var_results[[as.character(pop_size)]] <- list(
          variable = outcome_var,
          population_size = pop_size,
          error = e$message
        )
      })
    }
    
    all_results[[outcome_var]] <- var_results
  }
  
  cat("Basic RDS estimation completed\n\n")
  return(all_results)
}

# ============================================================================
# PREFERRED MODEL RESULTS (MAIN TEXT)
# ============================================================================

extract_preferred_results <- function(basic_results, preferred_method = "RDS_SS", 
                                    main_pop_size = 980000) {
  
  cat("=== Extracting Preferred Model Results ===\n")
  cat("Method:", preferred_method, "at population size:", format(main_pop_size, big.mark = ","), "\n")
  
  preferred_results <- list()
  
  comparable_indicators <- get_comparable_indicators()
  
  for (outcome_var in names(basic_results)) {
    
    if (as.character(main_pop_size) %in% names(basic_results[[outcome_var]])) {
      
      result <- basic_results[[outcome_var]][[as.character(main_pop_size)]]
      
      if (preferred_method %in% names(result)) {
        preferred_results[[outcome_var]] <- list(
          estimate = result[[preferred_method]],
          variable_label = comparable_indicators$labels[outcome_var],
          confidence_level = comparable_indicators$confidence_levels[outcome_var],
          population_size = main_pop_size,
          method = preferred_method
        )
      }
    }
  }
  
  # Create main results table
  main_table <- data.frame(
    indicator = names(preferred_results),
    estimate = sapply(preferred_results, function(x) x$estimate),
    confidence = sapply(preferred_results, function(x) x$confidence_level),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estimate_pct = estimate * 100,
      indicator_clean = get_comparable_indicators()$labels[indicator]
    ) %>%
    arrange(desc(estimate))
  
  cat("Preferred results extracted for", nrow(main_table), "indicators\n\n")
  
  return(list(
    results = preferred_results,
    main_table = main_table
  ))
}

# ============================================================================
# CONVERGENCE DIAGNOSTICS
# ============================================================================

run_convergence_diagnostics <- function(outcome_vars, save_plots = TRUE) {
  
  cat("=== Convergence Diagnostics ===\n")
  
  diagnostics <- list()
  
  for (outcome_var in outcome_vars) {
    
    if (!(outcome_var %in% names(rd.dd))) {
      next
    }
    
    cat("Convergence analysis for:", outcome_var, "\n")
    
    tryCatch({
      
      # Generate convergence plots
      conv_plot <- convergence.plot(rd.dd, outcome.variable = outcome_var)
      
      diagnostics[[outcome_var]] <- conv_plot
      
      if (save_plots) {
        filename <- here("output", "figures", paste0("convergence_", outcome_var, ".png"))
        ggsave(filename, conv_plot, width = 8, height = 6, dpi = 300)
      }
      
    }, error = function(e) {
      cat("  Error in convergence for", outcome_var, ":", e$message, "\n")
    })
  }
  
  cat("Convergence diagnostics completed\n\n")
  return(diagnostics)
}

# ============================================================================
# SENSITIVITY ANALYSIS (APPENDICES)
# ============================================================================

run_sensitivity_analysis <- function(basic_results, focus_vars = NULL) {
  
  cat("=== Sensitivity Analysis for Appendices ===\n")
  
  if (is.null(focus_vars)) {
    focus_vars <- names(basic_results)[1:3]  # Focus on top 3 indicators
  }
  
  sensitivity_results <- list()
  
  # 1. Population size sensitivity
  pop_sensitivity <- list()
  
  for (outcome_var in focus_vars) {
    
    var_results <- basic_results[[outcome_var]]
    
    # Extract RDS-SS estimates across population sizes
    estimates_by_pop <- sapply(var_results, function(x) {
      if ("RDS_SS" %in% names(x)) x$RDS_SS else NA
    })
    
    estimates_by_pop <- estimates_by_pop[!is.na(estimates_by_pop)]
    
    if (length(estimates_by_pop) > 0) {
      pop_sensitivity[[outcome_var]] <- list(
        population_sizes = as.numeric(names(estimates_by_pop)),
        estimates = estimates_by_pop,
        relative_change = (max(estimates_by_pop) - min(estimates_by_pop)) / min(estimates_by_pop)
      )
    }
  }
  
  # 2. Method comparison (all methods)
  method_comparison <- list()
  
  main_pop <- rds_config$main_population_size
  
  for (outcome_var in focus_vars) {
    
    if (as.character(main_pop) %in% names(basic_results[[outcome_var]])) {
      
      result <- basic_results[[outcome_var]][[as.character(main_pop)]]
      
      method_comparison[[outcome_var]] <- list(
        RDS_I = result$RDS_I,
        RDS_II = result$RDS_II,
        RDS_SS = result$RDS_SS
      )
    }
  }
  
  sensitivity_results$population_sensitivity = pop_sensitivity
  sensitivity_results$method_comparison = method_comparison
  
  cat("Sensitivity analysis completed\n\n")
  return(sensitivity_results)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

main_rds_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting comprehensive RDS analysis...\n\n")
  
  # Step 1: Basic RDS estimation
  if (rds_config$run_basic_estimation) {
    basic_results <- run_basic_rds_estimation(
      outcome_vars = rds_config$outcome_vars,
      population_sizes = rds_config$population_sizes,
      force_recompute = rds_config$force_recompute
    )
  } else {
    cat("Skipping basic estimation (disabled in config)\n")
    return(invisible(NULL))
  }
  
  # Step 2: Extract preferred model results (main text)
  preferred_results <- extract_preferred_results(
    basic_results = basic_results,
    preferred_method = rds_config$preferred_method,
    main_pop_size = rds_config$main_population_size
  )
  
  # Step 3: Convergence diagnostics
  if (rds_config$run_convergence_diagnostics) {
    convergence_results <- run_convergence_diagnostics(
      outcome_vars = rds_config$outcome_vars,
      save_plots = rds_config$save_plots
    )
  }
  
  # Step 4: Sensitivity analysis (appendices)
  if (rds_config$run_method_comparison) {
    sensitivity_results <- run_sensitivity_analysis(
      basic_results = basic_results,
      focus_vars = rds_config$outcome_vars[1:3]  # Top 3 indicators
    )
  }
  
  # Step 5: Save main results
  final_results <- list(
    basic_results = basic_results,
    preferred_results = preferred_results,
    sensitivity_results = if(exists("sensitivity_results")) sensitivity_results else NULL,
    convergence_results = if(exists("convergence_results")) convergence_results else NULL,
    
    config = rds_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      preferred_method = rds_config$preferred_method,
      main_population_size = rds_config$main_population_size
    )
  )
  
  # Save results
  save(final_results, file = here("output", "rds_estimation_results.RData"))
  
  # Save main table for easy access
  if (rds_config$save_tables) {
    write.csv(preferred_results$main_table, 
              here("output", "tables", "rds_main_results.csv"),
              row.names = FALSE)
  }
  
  cat("=== RDS Analysis Complete ===\n")
  cat("Main results (", rds_config$preferred_method, "):\n")
  print(preferred_results$main_table)
  cat("\nResults saved to: output/rds_estimation_results.RData\n")
  cat("Main table saved to: output/tables/rds_main_results.csv\n\n")
  
  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  # Check if this is being run from the pipeline
  if (exists("pipeline_config")) {
    cat("Running as part of main pipeline\n")
  } else {
    cat("Running standalone RDS analysis\n")
  }
  
  # Run analysis
  rds_results <- main_rds_analysis()
  
} else {
  cat("RDS estimation script loaded (execution skipped)\n")
}
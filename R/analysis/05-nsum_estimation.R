# 05-nsum_estimation.R
# Network Scale-Up Method (NSUM) Estimation
# Domestic Worker Exploitation and Modern Slavery in UK
#
# Estimates hidden population sizes using alter-centric network questions
# Focus: One preferred NSUM model for main text, sensitivity analysis for appendices

cat("=== NSUM Estimation Analysis ===\n")
cat("Network Scale-Up Method for hidden population estimation\n")
cat("Using CE's comparable indicators and RDS weights\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(here)
library(parallel)  # For Monte Carlo simulation
library(boot)      # For bootstrap confidence intervals

# Load NSUM-specific packages from CRAN
tryCatch({
  library(networkscaleup)  # Modern Bayesian NSUM with Stan backend
  nsum_packages_available <- TRUE
}, error = function(e) {
  cat("Warning: networkscaleup package not available. Using custom implementation.\n")
  nsum_packages_available <- FALSE
})

# Source helper functions
source(here("R", "utils", "helper_functions.R"))
source(here("R", "config.R"))
global_config <- get_global_config()

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# CONFIGURATION
# ============================================================================

nsum_config <- list(
  # Focus on comparable indicators (NSUM versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,
  
  # Network size variables and known population probes
  network_size_var = "known_network_size",  # Q13 2f
  degree_vars = c("q13_a", "q13_b", "q13_c", "q13_d", "q13_e"),  # Network size probes
  
  # Known population sizes for scaling
  probe_sizes = c(
    # These would be UK population estimates for reference groups
    # Placeholder values - should be replaced with actual UK demographics
    q13_a = 150000,   # Example: specific occupation group
    q13_b = 250000,   # Example: specific demographic group  
    q13_c = 500000,   # Example: specific activity group
    q13_d = 100000,   # Example: specific characteristic group
    q13_e = 75000     # Example: specific location group
  ),
  
  # Population scenarios
  total_population_sizes = global_config$population_sizes,
  preferred_population_size = global_config$main_population_size,  # EU baseline
  
  # Multiple weighting schemes from data preparation
  weighting_schemes = list(
    "unweighted" = NULL,
    "rds_I" = "wt.RDS1_",  # Variable-specific RDS-I weights
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k", 
    "vh_050k" = "wt.vh_050k",
    "vh_1740k" = "wt.vh_1740k",
    "ss_980k" = "wt.SS_980k",
    "ss_100k" = "wt.SS_100k",
    "ss_050k" = "wt.SS_050k", 
    "ss_1740k" = "wt.SS_1740k"
  ),
  
  # Analysis options
  use_rds_weights = TRUE,
  run_sensitivity_analysis = TRUE,
  create_comparison_with_rds = TRUE,
  run_monte_carlo = TRUE,
  
  # Monte Carlo parameters
  mc_iterations = 1000,  # Number of Monte Carlo iterations
  mc_confidence_level = 0.95,
  mc_parallel_cores = 4,
  
  # Estimation methods
  estimation_methods = c("basic", "weighted", "stratified", "monte_carlo"),
  preferred_method = global_config$preferred_nsum_method,
  
  # Output
  save_detailed_results = TRUE,
  create_plots = TRUE,
  save_all_weighting_results = TRUE
)

cat("NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n") 
cat("- Network probes:", length(nsum_config$degree_vars), "reference groups\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Preferred method:", nsum_config$preferred_method, "\n")
cat("- Preferred population:", format(nsum_config$preferred_population_size, big.mark = ","), "\n\n")

# ============================================================================
# CORE NSUM ESTIMATION FUNCTION (Enhanced with Multiple Weighting Support)
# ============================================================================

estimate_nsum_population <- function(data, weights = NULL, hidden_connections_var, 
                                   degree_vars, probe_sizes, 
                                   total_population_size = 980000,
                                   method = "basic", weighting_scheme = "unweighted") {
  
  # Input validation
  if (length(degree_vars) != length(probe_sizes)) {
    stop("degree_vars and probe_sizes must have the same length")
  }
  
  # Handle weights based on weighting scheme
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
    cat("  Using unweighted estimation\n")
  } else {
    cat("  Using", weighting_scheme, "weights\n")
  }
  
  # Validate weights
  if (length(weights) != nrow(data)) {
    warning("Weight vector length doesn't match data rows. Using unweighted.")
    weights <- rep(1, nrow(data))
  }
  
  # Remove missing values for this estimation
  complete_cases <- complete.cases(data[[hidden_connections_var]], weights)
  data_complete <- data[complete_cases, ]
  weights_complete <- weights[complete_cases]
  
  if (nrow(data_complete) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No complete cases available"
    ))
  }
  
  cat("  Complete cases:", nrow(data_complete), "of", nrow(data), "\n")
  
  # Calculate weighted average number of hidden population members known (y_F,H)
  y_connections <- data_complete[[hidden_connections_var]]
  
  if (method == "weighted" && !is.null(weights)) {
    # Weighted estimate
    y_F_H <- sum(y_connections * weights_complete, na.rm = TRUE) / sum(weights_complete, na.rm = TRUE)
  } else {
    # Unweighted estimate
    y_F_H <- mean(y_connections, na.rm = TRUE)
  }
  
  # Get frame population (all respondents)
  frame_data <- data_complete
  frame_weights <- weights_complete
  
  # Calculate average personal network size (d_F,F) using probe questions
  network_sizes <- list()
  
  for (i in seq_along(degree_vars)) {
    degree_var <- degree_vars[i]
    probe_size <- probe_sizes[i]
    
    if (degree_var %in% names(frame_data)) {
      
      if (method == "weighted" && !is.null(weights)) {
        # Weighted average
        avg_connections <- sum(frame_data[[degree_var]] * frame_weights, na.rm = TRUE) / 
                          sum(frame_weights, na.rm = TRUE)
      } else {
        # Unweighted average
        avg_connections <- mean(frame_data[[degree_var]], na.rm = TRUE)
      }
      
      # Scale by known population size
      network_sizes[[degree_var]] <- avg_connections / probe_size * total_population_size
    }
  }
  
  # Average across all probes to get d_F,F
  valid_network_sizes <- unlist(network_sizes)
  valid_network_sizes <- valid_network_sizes[is.finite(valid_network_sizes)]
  
  if (length(valid_network_sizes) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No valid network size estimates"
    ))
  }
  
  d_F_F <- mean(valid_network_sizes)
  
  # Calculate N_H estimate using basic NSUM formula
  # N_H = (y_F,H / d_F,F) * N_F
  # Where N_F is the frame population size (total_population_size)
  
  N_H_estimate <- (y_F_H / d_F_F) * total_population_size
  
  # Additional diagnostics
  result <- list(
    N_H_estimate = N_H_estimate,
    
    # Components
    y_F_H = y_F_H,
    d_F_F = d_F_F,
    total_population_size = total_population_size,
    
    # Diagnostics
    sample_size = nrow(data_complete),
    hidden_connections_mean = y_F_H,
    network_size_estimates = network_sizes,
    average_network_size = d_F_F,
    
    # Metadata
    method = method,
    weighting_scheme = weighting_scheme,
    hidden_variable = hidden_connections_var,
    degree_variables = degree_vars,
    probe_sizes = probe_sizes
  )
  
  return(result)
}

# ============================================================================
# MULTIPLE WEIGHTING SCHEMES NSUM ESTIMATION
# ============================================================================

get_weights_for_scheme <- function(data, scheme_name, outcome_var = NULL) {
  
  if (scheme_name == "unweighted") {
    return(NULL)
  }
  
  # Variable-specific RDS-I weights
  if (scheme_name == "rds_I" && !is.null(outcome_var)) {
    # Convert NSUM variable to RDS variable name
    rds_var <- gsub("_nsum", "_rds", outcome_var)
    weight_var <- paste0("wt.RDS1_", rds_var)
    
    if (weight_var %in% names(data)) {
      return(data[[weight_var]])
    } else {
      # Fallback to generic RDS-I weights if variable-specific not available
      weight_var <- paste0("wt.RDS1_", gsub("_nsum", "", outcome_var))
      if (weight_var %in% names(data)) {
        return(data[[weight_var]])
      }
    }
  }
  
  # Population-based weights
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]
  if (weight_var %in% names(data)) {
    return(data[[weight_var]])
  }
  
  # If weight variable not found, return NULL (unweighted)
  cat("Warning: Weight variable", weight_var, "not found. Using unweighted.\n")
  return(NULL)
}

run_comprehensive_nsum_estimation <- function(outcome_vars, population_sizes,
                                              method = nsum_config$preferred_method,
                                              weighting_schemes = names(nsum_config$weighting_schemes)) {
  
  cat("=== Comprehensive NSUM Estimation ===\n")
  cat("Indicators:", length(outcome_vars), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Weighting schemes:", length(weighting_schemes), "\n")
  cat("Total combinations:", length(outcome_vars) * length(population_sizes) * length(weighting_schemes), "\n\n")
  
  all_results <- list()
  scheme_summaries <- list()
  
  # Get RDS weights if using weighted method
  if (method == "weighted" && nsum_config$use_rds_weights) {
    # Use RDS-SS weights (preferred method)
    if ("rds_weights" %in% names(rd.dd)) {
      weights <- rd.dd$rds_weights
    } else {
      # Calculate RDS-SS weights
      cat("Calculating RDS-SS weights...\n")
      rds_ss_result <- RDS.SS.estimates(rds.data = rd.dd, 
                                       outcome.variable = outcome_vars[1],
                                       N = nsum_config$preferred_population_size)
      weights <- rds_ss_result$weights
    }
  } else {
    weights <- NULL
  }
  
  for (outcome_var in outcome_vars) {
    
    cat("\nProcessing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(rd.dd))) {
      cat("Warning: Variable", outcome_var, "not found in data\n")
      next
    }
    
    var_results <- list()
    
    for (pop_size in population_sizes) {
      
      cat("  Population size:", format(pop_size, big.mark = ","), "\n")
      
      tryCatch({
        
        result <- estimate_nsum_population(
          data = rd.dd,
          weights = weights,
          hidden_connections_var = outcome_var,
          degree_vars = nsum_config$degree_vars,
          probe_sizes = nsum_config$probe_sizes,
          total_population_size = pop_size,
          method = method
        )
        
        # Add metadata
        result$variable = outcome_var
        result$population_size = pop_size
        result$timestamp = Sys.time()
        
        var_results[[as.character(pop_size)]] <- result
        
        if (!is.na(result$N_H_estimate)) {
          cat("  Estimate:", format(round(result$N_H_estimate), big.mark = ","), "\n")
        }
        
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
        var_results[[as.character(pop_size)]] <- list(
          variable = outcome_var,
          population_size = pop_size,
          error = e$message
        )
      })
    }
    
    all_results[[outcome_var]] <- var_results
  }
  
  cat("\nNSUM estimation completed\n\n")
  return(all_results)
}

# Backwards-compatible wrapper expected by the pipeline
run_nsum_estimation <- function(outcome_vars, population_sizes, method = "weighted") {
  run_comprehensive_nsum_estimation(
    outcome_vars = outcome_vars,
    population_sizes = population_sizes,
    method = method
  )
}

# ============================================================================
# EXTRACT PREFERRED RESULTS
# ============================================================================

extract_preferred_nsum_results <- function(nsum_results, preferred_method = "weighted",
                                          main_pop_size = 980000) {
  
  cat("=== Extracting Preferred NSUM Results ===\n")
  cat("Method:", preferred_method, "at population size:", format(main_pop_size, big.mark = ","), "\n")
  
  preferred_results <- list()
  comparable_indicators <- get_comparable_indicators()
  
  for (outcome_var in names(nsum_results)) {
    
    if (as.character(main_pop_size) %in% names(nsum_results[[outcome_var]])) {
      
      result <- nsum_results[[outcome_var]][[as.character(main_pop_size)]]
      
      if (!"error" %in% names(result) && !is.na(result$N_H_estimate)) {
        
        # Convert to prevalence rate
        prevalence_rate <- result$N_H_estimate / main_pop_size
        
        preferred_results[[outcome_var]] <- list(
          estimate = prevalence_rate,
          absolute_estimate = result$N_H_estimate,
          variable_label = comparable_indicators$labels[gsub("_nsum", "_rds", outcome_var)],
          confidence_level = comparable_indicators$confidence_levels[gsub("_nsum", "_rds", outcome_var)],
          population_size = main_pop_size,
          method = preferred_method,
          
          # NSUM components
          y_F_H = result$y_F_H,
          d_F_F = result$d_F_F,
          sample_size = result$sample_size
        )
      }
    }
  }
  
  # Create main results table
  main_table <- data.frame(
    indicator = names(preferred_results),
    estimate = sapply(preferred_results, function(x) x$estimate),
    absolute_estimate = sapply(preferred_results, function(x) x$absolute_estimate),
    confidence = sapply(preferred_results, function(x) x$confidence_level),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estimate_pct = estimate * 100,
      indicator_clean = sapply(indicator, function(x) {
        rds_var <- gsub("_nsum", "_rds", x)
        get_comparable_indicators()$labels[rds_var]
      }),
      absolute_formatted = format(round(absolute_estimate), big.mark = ",")
    ) %>%
    arrange(desc(estimate))
  
  cat("Preferred NSUM results extracted for", nrow(main_table), "indicators\n\n")
  
  return(list(
    results = preferred_results,
    main_table = main_table
  ))
}

# ============================================================================
# COMPARISON WITH RDS RESULTS
# ============================================================================

create_rds_nsum_comparison <- function(nsum_results, rds_results_file = NULL) {
  
  cat("=== Creating RDS vs NSUM Comparison ===\n")
  
  # Load RDS results if available
  if (is.null(rds_results_file)) {
    rds_results_file <- here("output", "rds_estimation_results.RData")
  }
  
  if (!file.exists(rds_results_file)) {
    cat("Warning: RDS results not found. Run RDS analysis first.\n")
    return(NULL)
  }
  
  load(rds_results_file)  # Should load 'final_results'
  rds_main_table <- final_results$preferred_results$main_table
  
  # Match indicators
  comparison_table <- data.frame()
  
  for (i in 1:nrow(rds_main_table)) {
    
    rds_indicator <- rds_main_table$indicator[i]
    nsum_indicator <- paste0(gsub("_rds", "_nsum", rds_indicator))
    
    if (nsum_indicator %in% names(nsum_results$results)) {
      
      rds_est <- rds_main_table$estimate[i]
      nsum_est <- nsum_results$results[[nsum_indicator]]$estimate
      
      comparison_row <- data.frame(
        indicator = rds_main_table$indicator_clean[i],
        rds_estimate = rds_est,
        nsum_estimate = nsum_est,
        difference = nsum_est - rds_est,
        ratio = nsum_est / rds_est,
        
        rds_estimate_pct = rds_est * 100,
        nsum_estimate_pct = nsum_est * 100,
        difference_pct = (nsum_est - rds_est) * 100,
        
        confidence_level = rds_main_table$confidence[i],
        
        stringsAsFactors = FALSE
      )
      
      comparison_table <- rbind(comparison_table, comparison_row)
    }
  }
  
  if (nrow(comparison_table) > 0) {
    comparison_table <- comparison_table[order(-comparison_table$rds_estimate), ]
  }
  
  cat("RDS vs NSUM comparison created for", nrow(comparison_table), "indicators\n\n")
  
  return(comparison_table)
}

# ============================================================================
# MAIN NSUM ANALYSIS
# ============================================================================

main_nsum_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting NSUM analysis for comparable indicators...\n\n")
  
  # Step 1: Run NSUM estimation
  nsum_results <- run_nsum_estimation(
    outcome_vars = nsum_config$outcome_vars,
    population_sizes = nsum_config$total_population_sizes,
    method = nsum_config$preferred_method
  )
  
  # Step 2: Extract preferred results (main text)
  preferred_results <- extract_preferred_nsum_results(
    nsum_results = nsum_results,
    preferred_method = nsum_config$preferred_method,
    main_pop_size = nsum_config$preferred_population_size
  )
  
  # Step 3: Create comparison with RDS (if available)
  if (nsum_config$create_comparison_with_rds) {
    comparison_table <- create_rds_nsum_comparison(
      nsum_results = preferred_results
    )
  } else {
    comparison_table <- NULL
  }
  
  # Step 4: Compile final results
  final_results <- list(
    nsum_results = nsum_results,
    preferred_results = preferred_results,
    rds_nsum_comparison = comparison_table,
    
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      preferred_method = nsum_config$preferred_method,
      main_population_size = nsum_config$preferred_population_size
    )
  )
  
  # Save results
  save(final_results, file = here("output", "nsum_estimation_results.RData"))
  
  # Save main table
  if (nsum_config$save_detailed_results) {
    write.csv(preferred_results$main_table, 
              here("output", "tables", "nsum_main_results.csv"),
              row.names = FALSE)
    
    if (!is.null(comparison_table)) {
      write.csv(comparison_table, 
                here("output", "tables", "rds_nsum_comparison.csv"),
                row.names = FALSE)
    }
  }
  
  cat("=== NSUM Analysis Complete ===\n")
  cat("Main results (", nsum_config$preferred_method, "):\n")
  print(preferred_results$main_table[c("indicator_clean", "estimate_pct", "absolute_formatted")])
  
  if (!is.null(comparison_table)) {
    cat("\nRDS vs NSUM comparison:\n")
    print(comparison_table[c("indicator", "rds_estimate_pct", "nsum_estimate_pct", "difference_pct")])
  }
  
  cat("\nResults saved to: output/nsum_estimation_results.RData\n")
  cat("Main table saved to: output/tables/nsum_main_results.csv\n\n")
  
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
    cat("Running standalone NSUM analysis\n")
  }
  
  # Run analysis
  nsum_results <- main_nsum_analysis()
  
} else {
  cat("NSUM estimation script loaded (execution skipped)\n")
}

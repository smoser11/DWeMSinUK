# 05-nsum_estimation_fixed.R
# Enhanced Network Scale-Up Method (NSUM) Estimation - FIXED VERSION
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# FIXES: Simplified Monte Carlo without parallel processing scoping issues

cat("=== Enhanced NSUM Estimation Analysis (FIXED) ===\n")
cat("Network Scale-Up Method with multiple weighting approaches\n")
cat("Monte Carlo simulation (serial processing for reliability)\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(here)
library(boot)      # For bootstrap confidence intervals

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# SIMPLIFIED CONFIGURATION (FIXED)
# ============================================================================

nsum_config <- list(
  # Focus on comparable indicators (NSUM versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,
  
  # Network size variables (actual variables available in data)
  network_size_var = "known_network_size",  # Q13 2f
  degree_vars = c("q13"),  # Network size probe (actual variable available)
  
  # Known population sizes for scaling
  probe_sizes = c(
    q13 = 200000   # Domestic workers reference population for network scaling
  ),
  
  # Population scenarios for Monte Carlo
  total_population_sizes = get_population_parameters()$sizes,
  preferred_population_size = 980000,  # EU baseline
  
  # Multiple weighting schemes (verified available in data)
  weighting_schemes = list(
    "unweighted" = NULL,
    "rds_I_document_withholding" = "wt.RDS1_document_withholding",
    "rds_I_pay_issues" = "wt.RDS1_pay_issues",
    "rds_I_threats_abuse" = "wt.RDS1_threats_abuse",
    "rds_I_excessive_hours" = "wt.RDS1_excessive_hours", 
    "rds_I_access_to_help" = "wt.RDS1_access_to_help",
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k",
    "vh_050k" = "wt.vh_050k",
    "ss_980k" = "wt.SS_980k"
  ),
  
  # Monte Carlo parameters (simplified)
  mc_iterations = 100,  # Reduced for demonstration
  mc_confidence_level = 0.95,
  mc_sample_fraction = 0.8,
  
  # Analysis options
  run_monte_carlo = TRUE,
  save_detailed_results = TRUE
)

cat("Fixed NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n") 
cat("- Network probes:", length(nsum_config$degree_vars), "reference groups\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Weighting schemes:", length(nsum_config$weighting_schemes), "approaches\n")
cat("- Monte Carlo iterations:", nsum_config$mc_iterations, "(reduced for reliability)\n\n")

# ============================================================================
# WEIGHTING SCHEME MANAGEMENT (FIXED)
# ============================================================================

get_weights_for_scheme_fixed <- function(scheme_name, outcome_var = NULL) {
  
  if (scheme_name == "unweighted") {
    return(NULL)
  }
  
  # Get weight variable name from scheme
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]
  
  if (is.null(weight_var)) {
    return(NULL)
  }
  
  # Check if weight exists in dd
  if (weight_var %in% names(dd)) {
    return(dd[[weight_var]])
  }
  
  cat("Warning: Weight variable", weight_var, "not found. Using unweighted.\n")
  return(NULL)
}

# ============================================================================
# CORE NSUM ESTIMATION FUNCTION (SIMPLIFIED)
# ============================================================================

estimate_nsum_population_fixed <- function(data, weights = NULL, hidden_connections_var, 
                                          degree_vars, probe_sizes, 
                                          total_population_size = 980000,
                                          weighting_scheme = "unweighted") {
  
  # Input validation
  if (length(degree_vars) != length(probe_sizes)) {
    stop("degree_vars and probe_sizes must have the same length")
  }
  
  # Handle weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  } else {
    if (length(weights) != nrow(data)) {
      warning("Weight vector length doesn't match data rows. Using unweighted.")
      weights <- rep(1, nrow(data))
    }
  }
  
  # Remove missing values
  complete_cases <- complete.cases(data[[hidden_connections_var]], weights)
  data_complete <- data[complete_cases, ]
  weights_complete <- weights[complete_cases]
  
  if (nrow(data_complete) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No complete cases available"
    ))
  }
  
  # Calculate weighted average number of hidden population members known (y_F,H)
  y_connections <- data_complete[[hidden_connections_var]]
  y_F_H <- sum(y_connections * weights_complete, na.rm = TRUE) / sum(weights_complete, na.rm = TRUE)
  
  # Calculate average personal network size (d_F,F) using probe questions
  network_sizes <- list()
  
  for (i in seq_along(degree_vars)) {
    degree_var <- degree_vars[i]
    probe_size <- probe_sizes[i]
    
    if (degree_var %in% names(data_complete)) {
      # Weighted average connections to this probe population
      avg_connections <- sum(data_complete[[degree_var]] * weights_complete, na.rm = TRUE) / 
                        sum(weights_complete, na.rm = TRUE)
      
      # Scale by known population size to get network size estimate
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
  N_H_estimate <- (y_F_H / d_F_F) * total_population_size
  
  # Return comprehensive results
  return(list(
    N_H_estimate = N_H_estimate,
    prevalence_rate = N_H_estimate / total_population_size,
    y_F_H = y_F_H,
    d_F_F = d_F_F,
    total_population_size = total_population_size,
    sample_size = nrow(data_complete),
    weighting_scheme = weighting_scheme,
    hidden_variable = hidden_connections_var,
    timestamp = Sys.time()
  ))
}

# ============================================================================
# SIMPLIFIED MONTE CARLO NSUM ESTIMATION
# ============================================================================

run_monte_carlo_nsum_fixed <- function(data, outcome_var, weights, population_sizes, 
                                      weighting_scheme, iterations = 100, 
                                      confidence_level = 0.95) {
  
  cat("    Running Monte Carlo simulation with", iterations, "iterations (serial processing)\n")
  
  mc_results <- list()
  
  for (pop_size in population_sizes) {
    
    cat("      Population size:", format(pop_size, big.mark = ","), "\n")
    
    # Initialize result vectors
    all_estimates <- numeric(iterations)
    all_prevalences <- numeric(iterations)
    valid_count <- 0
    
    # Run Monte Carlo iterations (serial)
    for (i in 1:iterations) {
      
      tryCatch({
        # Bootstrap sample
        n <- nrow(data)
        sample_size <- floor(nsum_config$mc_sample_fraction * n)
        boot_indices <- sample(1:n, size = sample_size, replace = TRUE)
        
        boot_data <- data[boot_indices, ]
        boot_weights <- if (is.null(weights)) NULL else weights[boot_indices]
        
        # Run NSUM estimation on bootstrap sample
        result <- estimate_nsum_population_fixed(
          data = boot_data,
          weights = boot_weights,
          hidden_connections_var = outcome_var,
          degree_vars = nsum_config$degree_vars,
          probe_sizes = nsum_config$probe_sizes,
          total_population_size = pop_size,
          weighting_scheme = weighting_scheme
        )
        
        if (!is.na(result$N_H_estimate)) {
          all_estimates[i] <- result$N_H_estimate
          all_prevalences[i] <- result$prevalence_rate
          valid_count <- valid_count + 1
        }
        
      }, error = function(e) {
        # Skip failed iterations
      })
    }
    
    # Extract valid results
    valid_estimates <- all_estimates[!is.na(all_estimates) & all_estimates > 0]
    valid_prevalences <- all_prevalences[!is.na(all_prevalences) & all_prevalences > 0]
    
    if (length(valid_estimates) < iterations * 0.1) {
      cat("        Warning: Only", length(valid_estimates), "valid estimates from", iterations, "iterations\n")
    }
    
    # Calculate confidence intervals
    alpha <- 1 - confidence_level
    if (length(valid_estimates) >= 10) {
      estimate_ci <- quantile(valid_estimates, c(alpha/2, 1-alpha/2), na.rm = TRUE)
      prevalence_ci <- quantile(valid_prevalences, c(alpha/2, 1-alpha/2), na.rm = TRUE)
    } else {
      estimate_ci <- c(NA, NA)
      prevalence_ci <- c(NA, NA)
    }
    
    mc_results[[as.character(pop_size)]] <- list(
      population_size = pop_size,
      iterations = iterations,
      valid_iterations = length(valid_estimates),
      
      # Point estimates
      mean_estimate = mean(valid_estimates, na.rm = TRUE),
      median_estimate = median(valid_estimates, na.rm = TRUE),
      mean_prevalence = mean(valid_prevalences, na.rm = TRUE),
      
      # Uncertainty measures
      estimate_sd = sd(valid_estimates, na.rm = TRUE),
      estimate_ci_lower = estimate_ci[1],
      estimate_ci_upper = estimate_ci[2],
      prevalence_ci_lower = prevalence_ci[1],
      prevalence_ci_upper = prevalence_ci[2],
      
      # Metadata
      confidence_level = confidence_level,
      weighting_scheme = weighting_scheme
    )
    
    if (length(valid_estimates) >= 10) {
      cat("        Mean estimate:", format(round(mean(valid_estimates)), big.mark = ","), 
          "CI: [", format(round(estimate_ci[1]), big.mark = ","), "-", 
          format(round(estimate_ci[2]), big.mark = ","), "]\n")
    } else {
      cat("        Insufficient valid estimates for CI calculation\n")
    }
  }
  
  return(mc_results)
}

# ============================================================================
# COMPREHENSIVE ANALYSIS (SIMPLIFIED AND FIXED)
# ============================================================================

run_comprehensive_nsum_fixed <- function(outcome_vars = nsum_config$outcome_vars, 
                                        population_sizes = nsum_config$total_population_sizes,
                                        weighting_schemes = names(nsum_config$weighting_schemes)) {
  
  cat("=== Comprehensive NSUM Estimation (FIXED) ===\n")
  cat("Indicators:", length(outcome_vars), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Weighting schemes:", length(weighting_schemes), "\n")
  cat("Monte Carlo:", ifelse(nsum_config$run_monte_carlo, "YES", "NO"), "\n")
  cat("Total combinations:", length(outcome_vars) * length(population_sizes) * length(weighting_schemes), "\n\n")
  
  all_results <- list()
  summary_table <- data.frame()
  monte_carlo_results <- list()
  
  for (scheme in weighting_schemes) {
    
    cat("\n=== Weighting Scheme:", scheme, "===\n")
    scheme_results <- list()
    
    # Get weights for this scheme
    scheme_weights <- get_weights_for_scheme_fixed(scheme)
    
    for (outcome_var in outcome_vars) {
      
      cat("\nProcessing:", outcome_var, "with", scheme, "weights\n")
      
      if (!(outcome_var %in% names(rd.dd))) {
        cat("Warning: Variable", outcome_var, "not found in data\n")
        next
      }
      
      var_results <- list()
      
      # Standard estimation for each population size
      for (pop_size in population_sizes) {
        
        cat("  Population size:", format(pop_size, big.mark = ","), "\n")
        
        tryCatch({
          
          result <- estimate_nsum_population_fixed(
            data = rd.dd,
            weights = scheme_weights,
            hidden_connections_var = outcome_var,
            degree_vars = nsum_config$degree_vars,
            probe_sizes = nsum_config$probe_sizes,
            total_population_size = pop_size,
            weighting_scheme = scheme
          )
          
          var_results[[as.character(pop_size)]] <- result
          
          if (!is.na(result$N_H_estimate)) {
            cat("    Estimate:", format(round(result$N_H_estimate), big.mark = ","), 
                "(", round(result$prevalence_rate * 100, 2), "%)\n")
            
            # Add to summary table
            summary_row <- data.frame(
              variable = outcome_var,
              population_size = pop_size,
              weighting_scheme = scheme,
              estimate = result$N_H_estimate,
              prevalence_pct = result$prevalence_rate * 100,
              y_F_H = result$y_F_H,
              d_F_F = result$d_F_F,
              sample_size = result$sample_size,
              stringsAsFactors = FALSE
            )
            summary_table <- rbind(summary_table, summary_row)
          }
          
        }, error = function(e) {
          cat("    Error:", e$message, "\n")
        })
      }
      
      scheme_results[[outcome_var]] <- var_results
      
      # Run Monte Carlo simulation if enabled
      if (nsum_config$run_monte_carlo && length(var_results) > 0) {
        cat("  Running Monte Carlo simulation...\n")
        
        mc_key <- paste(scheme, outcome_var, sep = "_")
        monte_carlo_results[[mc_key]] <- run_monte_carlo_nsum_fixed(
          data = rd.dd,
          outcome_var = outcome_var,
          weights = scheme_weights,
          population_sizes = population_sizes,
          weighting_scheme = scheme,
          iterations = nsum_config$mc_iterations,
          confidence_level = nsum_config$mc_confidence_level
        )
      }
    }
    
    all_results[[scheme]] <- scheme_results
  }
  
  cat("\n=== Comprehensive NSUM Estimation (FIXED) Completed ===\n\n")
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_table,
    monte_carlo_results = monte_carlo_results,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      schemes_run = weighting_schemes,
      outcomes_analyzed = outcome_vars,
      population_sizes = population_sizes
    )
  ))
}

# ============================================================================
# MAIN ANALYSIS FUNCTION (FIXED)
# ============================================================================

main_enhanced_nsum_analysis_fixed <- function() {
  
  setup_project_environment()
  
  cat("Starting FIXED enhanced NSUM analysis...\n\n")
  
  # Run comprehensive estimation
  comprehensive_results <- run_comprehensive_nsum_fixed()
  
  # Save results
  save(comprehensive_results, file = here("output", "enhanced_nsum_results_fixed.RData"))
  
  # Save summary table
  if (nrow(comprehensive_results$summary_table) > 0) {
    write.csv(comprehensive_results$summary_table, 
              here("output", "tables", "nsum_summary_fixed.csv"),
              row.names = FALSE)
  }
  
  cat("=== FIXED NSUM Analysis Complete ===\n")
  cat("Results saved to: output/enhanced_nsum_results_fixed.RData\n")
  cat("Summary table saved to: output/tables/nsum_summary_fixed.csv\n\n")
  
  # Show preview results
  if (nrow(comprehensive_results$summary_table) > 0) {
    cat("Preview of results:\n")
    preview_data <- comprehensive_results$summary_table %>%
      filter(population_size == nsum_config$preferred_population_size) %>%
      select(variable, weighting_scheme, prevalence_pct) %>%
      arrange(variable, desc(prevalence_pct))
    
    print(head(preview_data, 10))
  }
  
  return(comprehensive_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  cat("Running FIXED enhanced NSUM analysis\n")
  
  # Run analysis
  fixed_results <- main_enhanced_nsum_analysis_fixed()
  
} else {
  cat("FIXED enhanced NSUM estimation script loaded (execution skipped)\n")
}
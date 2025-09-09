# 05-nsum_estimation_simple.R
# Enhanced Network Scale-Up Method (NSUM) Estimation - SIMPLE RELIABLE VERSION
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# FOCUS: Reliable basic functionality without complex Monte Carlo

cat("=== Enhanced NSUM Estimation Analysis (SIMPLE & RELIABLE) ===\n")
cat("Network Scale-Up Method with multiple weighting approaches\n")
cat("Focus: Reliable base estimates without Monte Carlo complexity\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# SIMPLE CONFIGURATION
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
  
  # Population scenarios
  total_population_sizes = get_population_parameters()$sizes,
  preferred_population_size = 980000,  # EU baseline
  
  # Available weighting schemes (verified in data)
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
  
  # Analysis options
  save_detailed_results = TRUE,
  create_comparison_with_rds = TRUE
)

cat("Simple NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n") 
cat("- Network probes:", length(nsum_config$degree_vars), "reference groups\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Weighting schemes:", length(nsum_config$weighting_schemes), "approaches\n\n")

# ============================================================================
# SIMPLE WEIGHTING MANAGEMENT
# ============================================================================

get_weights_simple <- function(scheme_name) {
  
  if (scheme_name == "unweighted") {
    return(rep(1, nrow(rd.dd)))
  }
  
  # Get weight variable name from scheme
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]
  
  if (is.null(weight_var)) {
    cat("Warning: No weight variable for scheme", scheme_name, ". Using unweighted.\n")
    return(rep(1, nrow(rd.dd)))
  }
  
  # Check if weight exists in dd
  if (weight_var %in% names(dd)) {
    return(dd[[weight_var]])
  }
  
  cat("Warning: Weight variable", weight_var, "not found. Using unweighted.\n")
  return(rep(1, nrow(rd.dd)))
}

# ============================================================================
# CORE NSUM ESTIMATION FUNCTION (SIMPLE)
# ============================================================================

estimate_nsum_simple <- function(data, weights, hidden_var, degree_vars, probe_sizes, pop_size, scheme_name) {
  
  # Input validation
  if (length(degree_vars) != length(probe_sizes)) {
    return(list(N_H_estimate = NA, error = "degree_vars and probe_sizes length mismatch"))
  }
  
  if (!(hidden_var %in% names(data))) {
    return(list(N_H_estimate = NA, error = paste("Hidden variable", hidden_var, "not found")))
  }
  
  # Ensure weights are correct length
  if (length(weights) != nrow(data)) {
    weights <- rep(1, nrow(data))
  }
  
  # Complete cases analysis
  complete_cases <- complete.cases(data[[hidden_var]], weights)
  data_clean <- data[complete_cases, ]
  weights_clean <- weights[complete_cases]
  
  if (nrow(data_clean) == 0) {
    return(list(N_H_estimate = NA, error = "No complete cases"))
  }
  
  cat("      Processing", nrow(data_clean), "complete cases\n")
  
  # Calculate y_F_H (weighted average connections to hidden population)
  hidden_connections <- data_clean[[hidden_var]]
  y_F_H <- sum(hidden_connections * weights_clean, na.rm = TRUE) / sum(weights_clean, na.rm = TRUE)
  
  # Calculate d_F_F (average network size) using probe questions
  network_size_estimates <- c()
  
  for (i in seq_along(degree_vars)) {
    degree_var <- degree_vars[i]
    probe_size <- probe_sizes[i]
    
    if (degree_var %in% names(data_clean)) {
      # Weighted average connections to probe population
      avg_connections <- sum(data_clean[[degree_var]] * weights_clean, na.rm = TRUE) / 
                        sum(weights_clean, na.rm = TRUE)
      
      # Scale to get network size estimate
      network_size_est <- (avg_connections / probe_size) * pop_size
      network_size_estimates <- c(network_size_estimates, network_size_est)
    }
  }
  
  if (length(network_size_estimates) == 0) {
    return(list(N_H_estimate = NA, error = "No valid network size estimates"))
  }
  
  # Average network size
  d_F_F <- mean(network_size_estimates, na.rm = TRUE)
  
  # Basic NSUM formula
  N_H_estimate <- (y_F_H / d_F_F) * pop_size
  
  # Return results
  return(list(
    N_H_estimate = N_H_estimate,
    prevalence_rate = N_H_estimate / pop_size,
    y_F_H = y_F_H,
    d_F_F = d_F_F,
    sample_size = nrow(data_clean),
    weighting_scheme = scheme_name,
    variable = hidden_var,
    population_size = pop_size,
    timestamp = Sys.time()
  ))
}

# ============================================================================
# COMPREHENSIVE ANALYSIS (SIMPLE)
# ============================================================================

run_comprehensive_nsum_simple <- function() {
  
  cat("=== Comprehensive NSUM Estimation (SIMPLE) ===\n")
  
  outcome_vars <- nsum_config$outcome_vars
  population_sizes <- nsum_config$total_population_sizes
  weighting_schemes <- names(nsum_config$weighting_schemes)
  
  cat("Processing:\n")
  cat("- Indicators:", length(outcome_vars), "\n")
  cat("- Population sizes:", length(population_sizes), "\n") 
  cat("- Weighting schemes:", length(weighting_schemes), "\n")
  cat("- Total combinations:", length(outcome_vars) * length(population_sizes) * length(weighting_schemes), "\n\n")
  
  all_results <- list()
  summary_table <- data.frame()
  
  for (scheme in weighting_schemes) {
    
    cat("\n=== Weighting Scheme:", scheme, "===\n")
    
    # Get weights for this scheme
    scheme_weights <- get_weights_simple(scheme)
    
    scheme_results <- list()
    
    for (outcome_var in outcome_vars) {
      
      cat("\nVariable:", outcome_var, "\n")
      
      if (!(outcome_var %in% names(rd.dd))) {
        cat("  Warning: Variable not found in data\n")
        next
      }
      
      var_results <- list()
      
      for (pop_size in population_sizes) {
        
        cat("  Population:", format(pop_size, big.mark = ","), "- ")
        
        # Run NSUM estimation
        result <- estimate_nsum_simple(
          data = rd.dd,
          weights = scheme_weights,
          hidden_var = outcome_var,
          degree_vars = nsum_config$degree_vars,
          probe_sizes = nsum_config$probe_sizes,
          pop_size = pop_size,
          scheme_name = scheme
        )
        
        var_results[[as.character(pop_size)]] <- result
        
        if (!is.na(result$N_H_estimate)) {
          cat("Estimate:", format(round(result$N_H_estimate), big.mark = ","), 
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
        } else {
          cat("Error:", result$error, "\n")
        }
      }
      
      scheme_results[[outcome_var]] <- var_results
    }
    
    all_results[[scheme]] <- scheme_results
  }
  
  cat("\n=== Simple NSUM Analysis Completed ===\n\n")
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_table,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      schemes_analyzed = weighting_schemes,
      variables_analyzed = outcome_vars,
      population_sizes = population_sizes
    )
  ))
}

# ============================================================================
# RDS COMPARISON (SIMPLE)
# ============================================================================

create_rds_nsum_comparison_simple <- function(nsum_results, main_pop_size = 980000) {
  
  cat("=== Creating RDS vs NSUM Comparison ===\n")
  
  # Load RDS results if available
  rds_file <- here("output", "rds_estimation_results.RData")
  
  if (!file.exists(rds_file)) {
    cat("Warning: RDS results not found. Skipping comparison.\n")
    return(NULL)
  }
  
  load(rds_file)  # Should load 'final_results'
  
  if (!exists("final_results")) {
    cat("Warning: Could not load RDS results. Skipping comparison.\n")
    return(NULL)
  }
  
  # Get NSUM results for main population size
  nsum_summary <- nsum_results$summary_table %>%
    filter(population_size == main_pop_size)
  
  # Get RDS results 
  rds_table <- final_results$preferred_results$main_table
  
  # Create comparison
  comparison_table <- data.frame()
  
  for (i in 1:nrow(rds_table)) {
    
    rds_indicator <- rds_table$indicator[i]
    nsum_indicator <- gsub("_rds", "_nsum", rds_indicator)
    
    # Find matching NSUM results across all schemes
    nsum_matches <- nsum_summary[nsum_summary$variable == nsum_indicator, ]
    
    if (nrow(nsum_matches) > 0) {
      
      for (j in 1:nrow(nsum_matches)) {
        
        rds_est <- rds_table$estimate_pct[i]
        nsum_est <- nsum_matches$prevalence_pct[j]
        
        comparison_row <- data.frame(
          indicator = rds_table$indicator_clean[i],
          rds_estimate_pct = rds_est,
          nsum_estimate_pct = nsum_est,
          nsum_weighting_scheme = nsum_matches$weighting_scheme[j],
          difference_pct = nsum_est - rds_est,
          ratio = nsum_est / rds_est,
          stringsAsFactors = FALSE
        )
        
        comparison_table <- rbind(comparison_table, comparison_row)
      }
    }
  }
  
  if (nrow(comparison_table) > 0) {
    comparison_table <- comparison_table %>%
      arrange(indicator, nsum_weighting_scheme)
  }
  
  cat("RDS vs NSUM comparison created for", length(unique(comparison_table$indicator)), 
      "indicators across", length(unique(comparison_table$nsum_weighting_scheme)), "schemes\n\n")
  
  return(comparison_table)
}

# ============================================================================
# MAIN ANALYSIS FUNCTION (SIMPLE)
# ============================================================================

main_nsum_analysis_simple <- function() {
  
  setup_project_environment()
  
  cat("Starting simple, reliable NSUM analysis...\n\n")
  
  # Run comprehensive estimation
  comprehensive_results <- run_comprehensive_nsum_simple()
  
  # Create RDS comparison if possible
  rds_comparison <- NULL
  if (nsum_config$create_comparison_with_rds) {
    tryCatch({
      rds_comparison <- create_rds_nsum_comparison_simple(comprehensive_results)
    }, error = function(e) {
      cat("Could not create RDS comparison:", e$message, "\n")
    })
  }
  
  # Compile final results
  final_results <- list(
    nsum_results = comprehensive_results,
    rds_nsum_comparison = rds_comparison,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      total_estimates = nrow(comprehensive_results$summary_table),
      analysis_type = "simple_reliable"
    )
  )
  
  # Save results
  save(final_results, file = here("output", "nsum_results_simple.RData"))
  cat("Results saved to: output/nsum_results_simple.RData\n")
  
  # Save summary table
  if (nrow(comprehensive_results$summary_table) > 0) {
    write.csv(comprehensive_results$summary_table, 
              here("output", "tables", "nsum_summary_simple.csv"),
              row.names = FALSE)
    cat("Summary table saved to: output/tables/nsum_summary_simple.csv\n")
  }
  
  # Save comparison table
  if (!is.null(rds_comparison) && nrow(rds_comparison) > 0) {
    write.csv(rds_comparison, 
              here("output", "tables", "rds_nsum_comparison_simple.csv"),
              row.names = FALSE)
    cat("Comparison table saved to: output/tables/rds_nsum_comparison_simple.csv\n")
  }
  
  cat("\n=== Simple NSUM Analysis Complete ===\n")
  cat("Total estimates generated:", nrow(comprehensive_results$summary_table), "\n")
  
  # Show preview of results
  if (nrow(comprehensive_results$summary_table) > 0) {
    cat("\nPreview of results (980K population):\n")
    preview_data <- comprehensive_results$summary_table %>%
      filter(population_size == 980000) %>%
      select(variable, weighting_scheme, prevalence_pct) %>%
      arrange(variable, desc(prevalence_pct)) %>%
      head(10)
    
    print(preview_data)
  }
  
  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  cat("Running simple, reliable NSUM analysis\n")
  
  # Run analysis
  simple_results <- main_nsum_analysis_simple()
  
} else {
  cat("Simple NSUM estimation script loaded (execution skipped)\n")
}
# 05-nsum_estimation_corrected.R
# CORRECTED Network Scale-Up Method (NSUM) Estimation
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# FIXED: Proper NSUM formula implementation

cat("=== CORRECTED NSUM Estimation Analysis ===\n")
cat("Network Scale-Up Method with CORRECTED network size calculation\n")
cat("Formula: N_H = (y_F_H / d_F_F) * N_F\n\n")

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
# CORRECTED CONFIGURATION
# ============================================================================

nsum_config <- list(
  # Focus on comparable indicators (NSUM versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,
  
  # Network size variables (CORRECTED interpretation)
  network_size_var = "known_network_size",  # Q13 2f - total domestic workers known
  degree_var = "q13",  # Network size - total DW connections per respondent
  
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

cat("Corrected NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n") 
cat("- Network size variable:", nsum_config$degree_var, "(domestic workers known per person)\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Weighting schemes:", length(nsum_config$weighting_schemes), "approaches\n\n")

# ============================================================================
# CORRECTED WEIGHTING MANAGEMENT
# ============================================================================

get_weights_corrected <- function(scheme_name) {
  
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
# CORRECTED NSUM ESTIMATION FUNCTION
# ============================================================================

estimate_nsum_corrected <- function(data, weights, hidden_var, degree_var, pop_size, scheme_name) {
  
  if (!(hidden_var %in% names(data))) {
    return(list(N_H_estimate = NA, error = paste("Hidden variable", hidden_var, "not found")))
  }
  
  if (!(degree_var %in% names(data))) {
    return(list(N_H_estimate = NA, error = paste("Degree variable", degree_var, "not found")))
  }
  
  # Ensure weights are correct length
  if (length(weights) != nrow(data)) {
    weights <- rep(1, nrow(data))
  }
  
  # Complete cases analysis - require both hidden variable and network size
  complete_cases <- complete.cases(data[[hidden_var]], data[[degree_var]], weights) &
                   data[[degree_var]] > 0  # Exclude people with zero network size
  
  data_clean <- data[complete_cases, ]
  weights_clean <- weights[complete_cases]
  
  if (nrow(data_clean) == 0) {
    return(list(N_H_estimate = NA, error = "No complete cases with positive network size"))
  }
  
  cat("      Processing", nrow(data_clean), "complete cases with positive network size\n")
  
  # CORRECTED NSUM CALCULATION
  # y_F_H: weighted average number of hidden population members known per person
  hidden_connections <- data_clean[[hidden_var]]
  y_F_H <- sum(hidden_connections * weights_clean, na.rm = TRUE) / sum(weights_clean, na.rm = TRUE)
  
  # d_F_F: weighted average total network size (domestic workers known per person) 
  network_sizes <- data_clean[[degree_var]]
  d_F_F <- sum(network_sizes * weights_clean, na.rm = TRUE) / sum(weights_clean, na.rm = TRUE)
  
  # Basic NSUM formula: N_H = (y_F_H / d_F_F) * N_F
  # Where N_F is the total domestic worker population size
  if (d_F_F <= 0) {
    return(list(N_H_estimate = NA, error = "Average network size is zero or negative"))
  }
  
  # The key insight: we're estimating the hidden population as a fraction of the total DW population
  N_H_estimate <- (y_F_H / d_F_F) * pop_size
  
  # Additional validation: prevalence should be reasonable
  prevalence_rate <- N_H_estimate / pop_size
  
  # Flag potentially problematic estimates
  warning_msg <- NULL
  if (prevalence_rate > 0.5) {
    warning_msg <- "High prevalence estimate (>50%) - check data and assumptions"
  } else if (prevalence_rate < 0.001) {
    warning_msg <- "Very low prevalence estimate (<0.1%) - check data sensitivity"
  }
  
  # Return comprehensive results
  return(list(
    N_H_estimate = N_H_estimate,
    prevalence_rate = prevalence_rate,
    y_F_H = y_F_H,
    d_F_F = d_F_F,
    sample_size = nrow(data_clean),
    total_sample = nrow(data),
    weighting_scheme = scheme_name,
    variable = hidden_var,
    population_size = pop_size,
    warning = warning_msg,
    
    # Additional diagnostics
    hidden_connections_sum = sum(hidden_connections * weights_clean, na.rm = TRUE),
    network_size_sum = sum(network_sizes * weights_clean, na.rm = TRUE),
    weight_sum = sum(weights_clean, na.rm = TRUE),
    
    timestamp = Sys.time()
  ))
}

# ============================================================================
# COMPREHENSIVE ANALYSIS (CORRECTED)
# ============================================================================

run_comprehensive_nsum_corrected <- function() {
  
  cat("=== Comprehensive NSUM Estimation (CORRECTED) ===\n")
  
  outcome_vars <- nsum_config$outcome_vars
  population_sizes <- nsum_config$total_population_sizes
  weighting_schemes <- names(nsum_config$weighting_schemes)
  
  cat("Processing:\n")
  cat("- Indicators:", length(outcome_vars), "\n")
  cat("- Population sizes:", length(population_sizes), "\n") 
  cat("- Weighting schemes:", length(weighting_schemes), "\n")
  cat("- Total combinations:", length(outcome_vars) * length(population_sizes) * length(weighting_schemes), "\n\n")
  
  # First, let's examine the data to understand the scaling
  cat("Data examination:\n")
  cat("- Average hidden connections (excessive_hours_nsum):", round(mean(rd.dd$excessive_hours_nsum, na.rm=TRUE), 3), "\n")
  cat("- Average network size (q13):", round(mean(rd.dd$q13, na.rm=TRUE), 3), "\n")
  cat("- Network size range:", range(rd.dd$q13[rd.dd$q13 > 0]), "\n")
  cat("- Sample with positive network size:", sum(rd.dd$q13 > 0), "out of", nrow(rd.dd), "\n\n")
  
  all_results <- list()
  summary_table <- data.frame()
  
  for (scheme in weighting_schemes) {
    
    cat("\n=== Weighting Scheme:", scheme, "===\n")
    
    # Get weights for this scheme
    scheme_weights <- get_weights_corrected(scheme)
    
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
        
        # Run corrected NSUM estimation
        result <- estimate_nsum_corrected(
          data = rd.dd,
          weights = scheme_weights,
          hidden_var = outcome_var,
          degree_var = nsum_config$degree_var,
          pop_size = pop_size,
          scheme_name = scheme
        )
        
        var_results[[as.character(pop_size)]] <- result
        
        if (!is.na(result$N_H_estimate)) {
          cat("Estimate:", format(round(result$N_H_estimate), big.mark = ","), 
              "(", round(result$prevalence_rate * 100, 2), "%)")
          
          if (!is.null(result$warning)) {
            cat(" [WARNING:", result$warning, "]")
          }
          cat("\n")
          
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
            warning = ifelse(is.null(result$warning), "", result$warning),
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
  
  cat("\n=== Corrected NSUM Analysis Completed ===\n\n")
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_table,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      schemes_analyzed = weighting_schemes,
      variables_analyzed = outcome_vars,
      population_sizes = population_sizes,
      formula_used = "N_H = (y_F_H / d_F_F) * N_F"
    )
  ))
}

# ============================================================================
# MAIN ANALYSIS FUNCTION (CORRECTED)
# ============================================================================

main_nsum_analysis_corrected <- function() {
  
  setup_project_environment()
  
  cat("Starting CORRECTED NSUM analysis with proper network size calculation...\n\n")
  
  # Run comprehensive estimation
  comprehensive_results <- run_comprehensive_nsum_corrected()
  
  # Create RDS comparison if possible
  rds_comparison <- NULL
  if (nsum_config$create_comparison_with_rds) {
    tryCatch({
      # Simple RDS comparison (reuse from previous version)
      rds_file <- here("output", "rds_estimation_results.RData")
      if (file.exists(rds_file)) {
        load(rds_file)
        if (exists("final_results")) {
          rds_table <- final_results$preferred_results$main_table
          nsum_summary <- comprehensive_results$summary_table %>%
            filter(population_size == 980000)
          
          comparison_table <- data.frame()
          for (i in 1:nrow(rds_table)) {
            rds_indicator <- rds_table$indicator[i]
            nsum_indicator <- gsub("_rds", "_nsum", rds_indicator)
            nsum_matches <- nsum_summary[nsum_summary$variable == nsum_indicator, ]
            
            if (nrow(nsum_matches) > 0) {
              for (j in 1:nrow(nsum_matches)) {
                comparison_row <- data.frame(
                  indicator = rds_table$indicator_clean[i],
                  rds_estimate_pct = rds_table$estimate_pct[i],
                  nsum_estimate_pct = nsum_matches$prevalence_pct[j],
                  nsum_weighting_scheme = nsum_matches$weighting_scheme[j],
                  difference_pct = nsum_matches$prevalence_pct[j] - rds_table$estimate_pct[i],
                  stringsAsFactors = FALSE
                )
                comparison_table <- rbind(comparison_table, comparison_row)
              }
            }
          }
          rds_comparison <- comparison_table
        }
      }
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
      analysis_type = "corrected_nsum_formula",
      key_fix = "Proper network size interpretation: q13 as total DW network size"
    )
  )
  
  # Save results
  save(final_results, file = here("output", "nsum_results_corrected.RData"))
  cat("Results saved to: output/nsum_results_corrected.RData\n")
  
  # Save summary table
  if (nrow(comprehensive_results$summary_table) > 0) {
    write.csv(comprehensive_results$summary_table, 
              here("output", "tables", "nsum_summary_corrected.csv"),
              row.names = FALSE)
    cat("Summary table saved to: output/tables/nsum_summary_corrected.csv\n")
  }
  
  # Save comparison table
  if (!is.null(rds_comparison) && nrow(rds_comparison) > 0) {
    write.csv(rds_comparison, 
              here("output", "tables", "rds_nsum_comparison_corrected.csv"),
              row.names = FALSE)
    cat("Comparison table saved to: output/tables/rds_nsum_comparison_corrected.csv\n")
  }
  
  cat("\n=== Corrected NSUM Analysis Complete ===\n")
  cat("Total estimates generated:", nrow(comprehensive_results$summary_table), "\n")
  
  # Show preview of results with sanity check
  if (nrow(comprehensive_results$summary_table) > 0) {
    cat("\nPreview of CORRECTED results (980K population):\n")
    preview_data <- comprehensive_results$summary_table %>%
      filter(population_size == 980000) %>%
      select(variable, weighting_scheme, prevalence_pct, warning) %>%
      arrange(variable, desc(prevalence_pct)) %>%
      head(10)
    
    print(preview_data)
    
    # Sanity check
    max_prev <- max(preview_data$prevalence_pct, na.rm = TRUE)
    cat("\nSanity check - Maximum prevalence:", round(max_prev, 2), "%")
    if (max_prev > 50) {
      cat(" [STILL TOO HIGH - NEEDS FURTHER INVESTIGATION]")
    } else if (max_prev < 10) {
      cat(" [REASONABLE RANGE]")
    } else {
      cat(" [CHECK IF REASONABLE]")
    }
    cat("\n")
  }
  
  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  cat("Running CORRECTED NSUM analysis\n")
  
  # Run analysis
  corrected_results <- main_nsum_analysis_corrected()
  
} else {
  cat("Corrected NSUM estimation script loaded (execution skipped)\n")
}
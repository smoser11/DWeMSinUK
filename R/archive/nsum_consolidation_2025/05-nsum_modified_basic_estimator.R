# 05-nsum_modified_basic_estimator.R
# CORRECTED: Modified Basic Scale-Up Estimator (Feehan & Salganik 2016)
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# Uses Modified Basic Scale-Up Estimator for single RDS sample from frame population
# Formula: N_H = (y_F,H / d_F,F) * N_F * (1/δ_F) * (1/τ_F)

cat("=== Modified Basic Scale-Up Estimator (Corrected Implementation) ===\n")
cat("Based on Feehan & Salganik (2016) Equation 23\n")
cat("Formula: N_H = (y_F,H / d_F,F) * N_F\n")
cat("For single RDS sample from frame population\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(boot)
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
# CONFIGURATION FOR MODIFIED BASIC SCALE-UP ESTIMATOR
# ============================================================================

basic_nsum_config <- list(
  # All comparable indicator pairs (CE's specifications)
  indicators = list(
    document_withholding = list(
      rds_var = "document_withholding_rds",
      nsum_var = "document_withholding_nsum",
      confidence = "highest"
    ),
    pay_issues = list(
      rds_var = "pay_issues_rds", 
      nsum_var = "pay_issues_nsum",
      confidence = "high"
    ),
    threats_abuse = list(
      rds_var = "threats_abuse_rds",
      nsum_var = "threats_abuse_nsum", 
      confidence = "high"
    ),
    excessive_hours = list(
      rds_var = "excessive_hours_rds",
      nsum_var = "excessive_hours_nsum",
      confidence = "medium"
    ),
    access_to_help = list(
      rds_var = "access_to_help_rds",
      nsum_var = "access_to_help_nsum", 
      confidence = "lowest"
    )
  ),
  
  # Network size variable for frame population degree estimation
  degree_var = "known_network_size",  # Q13 2f - total domestic workers known
  
  # Multiple RDS weighting schemes for robustness
  # NOTE: All weights now properly normalized to address scaling differences
  weight_schemes = list(
    "unweighted" = NULL,
    "rds_I_document_withholding" = "wt.RDS1_document_withholding",
    "rds_I_pay_issues" = "wt.RDS1_pay_issues", 
    "rds_I_threats_abuse" = "wt.RDS1_threats_abuse",
    "rds_I_excessive_hours" = "wt.RDS1_excessive_hours",
    "rds_I_access_to_help" = "wt.RDS1_access_to_help",
    "vh_980k" = "wt.vh_980k",  # RESTORED: Now with proper normalization
    "vh_100k" = "wt.vh_100k",
    "vh_050k" = "wt.vh_050k",
    "ss_980k" = "wt.SS_980k"
  ),
  
  # Multiple frame population sizes for robustness
  frame_population_sizes = c(50000, 100000, 980000, 1740000),
  preferred_size = 980000,  # EU baseline
  
  # Adjustment factors for bias correction (sensitivity analysis)
  adjustment_factors = list(
    # δ_F = degree ratio (hidden pop degree / frame pop degree)
    degree_ratios = c(0.5, 0.75, 1.0, 1.25, 1.5),
    
    # τ_F = true positive rate (reporting accuracy)  
    true_positive_rates = c(0.6, 0.7, 0.8, 0.9, 1.0),
    
    # Default values for main analysis
    default_degree_ratio = 1.0,  # Assume same degree
    default_true_positive_rate = 1.0  # Assume perfect reporting
  ),
  
  # Bootstrap settings
  bootstrap_samples = 1000,
  confidence_level = 0.95
)

cat("Modified Basic Scale-Up Configuration:\n")
cat("- Indicators:", length(basic_nsum_config$indicators), "\n")
cat("- Weight schemes:", length(basic_nsum_config$weight_schemes), "\n") 
cat("- Population sizes:", length(basic_nsum_config$frame_population_sizes), "\n")
cat("- Degree ratio scenarios:", length(basic_nsum_config$adjustment_factors$degree_ratios), "\n")
cat("- True positive rate scenarios:", length(basic_nsum_config$adjustment_factors$true_positive_rates), "\n\n")

# ============================================================================
# MODIFIED BASIC SCALE-UP ESTIMATOR FUNCTION
# ============================================================================

modified_basic_nsum <- function(data, rds_var, nsum_var, degree_var, weight_var, N_F,
                                degree_ratio = 1.0, true_positive_rate = 1.0,
                                scheme_name = "unknown") {
  
  # Input validation
  required_vars <- c(rds_var, nsum_var, degree_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name,
      N_F = N_F,
      error = paste("Missing variables:", paste(missing_vars, collapse = ", "))
    ))
  }
  
  # Get weights and normalize them
  if (is.null(weight_var) || !weight_var %in% names(data)) {
    weights <- rep(1, nrow(data))
    if (!is.null(weight_var)) {
      cat("Warning: Weight variable", weight_var, "not found. Using unweighted.\n")
    }
  } else {
    raw_weights <- data[[weight_var]]
    # Normalize weights to sample size to ensure consistent scaling
    # This addresses the issue where VH/SS weights sum to population size
    # while RDS-I weights sum to 1
    weights <- raw_weights * nrow(data) / sum(raw_weights, na.rm = TRUE)
  }
  
  # Ensure weights are valid
  valid_weight_cases <- !is.na(weights) & weights > 0 & is.finite(weights)
  
  # ==== STEP 1: Estimate Out-Reports y_F,H ====
  # Total reports from frame population about hidden population
  out_reports <- data[[nsum_var]]
  valid_out_cases <- !is.na(out_reports) & valid_weight_cases
  
  if (sum(valid_out_cases) == 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name, 
      N_F = N_F,
      error = "No valid out-report cases"
    ))
  }
  
  # Weighted total out-reports from frame population
  y_FH <- sum(out_reports[valid_out_cases] * weights[valid_out_cases])
  
  # ==== STEP 2: Estimate Average Degree d_F,F ====
  # Average network size within frame population
  degrees <- data[[degree_var]]
  valid_degree_cases <- !is.na(degrees) & degrees > 0 & valid_weight_cases
  
  if (sum(valid_degree_cases) == 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name,
      N_F = N_F, 
      error = "No valid degree cases"
    ))
  }
  
  # Weighted average degree in frame population
  d_FF <- sum(degrees[valid_degree_cases] * weights[valid_degree_cases]) / 
          sum(weights[valid_degree_cases])
  
  if (d_FF <= 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name,
      N_F = N_F,
      error = "Average degree is zero or negative"
    ))
  }
  
  # ==== STEP 3: Modified Basic Scale-Up Estimator ====
  # Basic estimator: N_H = (y_F,H / d_F,F) * N_F
  basic_estimate <- (y_FH / d_FF) * N_F
  
  # With adjustment factors: N_H = basic * (1/δ_F) * (1/τ_F)
  adjusted_estimate <- basic_estimate * (1 / degree_ratio) * (1 / true_positive_rate)
  
  # ==== STEP 4: RDS Prevalence Estimate (for comparison) ====
  rds_cases <- !is.na(data[[rds_var]]) & valid_weight_cases
  
  if (sum(rds_cases) == 0) {
    rds_prevalence <- NA
    rds_estimate <- NA
  } else {
    rds_prevalence <- sum(data[[rds_var]][rds_cases] * weights[rds_cases]) / 
                      sum(weights[rds_cases])
    rds_estimate <- rds_prevalence * N_F
  }
  
  # ==== STEP 5: Additional Diagnostics ====
  # Estimate degree ratio empirically from data if possible
  hidden_cases <- !is.na(data[[rds_var]]) & data[[rds_var]] == 1 & 
                  !is.na(degrees) & degrees > 0 & valid_weight_cases
  
  empirical_degree_ratio <- NA
  if (sum(hidden_cases) > 0) {
    d_HH <- sum(degrees[hidden_cases] * weights[hidden_cases]) / 
            sum(weights[hidden_cases])
    empirical_degree_ratio <- d_HH / d_FF
  }
  
  # ==== STEP 6: Return Results ====
  return(list(
    indicator = rds_var,
    scheme = scheme_name,
    N_F = N_F,
    
    # Basic NSUM components
    y_FH = y_FH,
    d_FF = d_FF,
    basic_estimate = basic_estimate,
    basic_prevalence = basic_estimate / N_F,
    
    # Adjusted estimates
    degree_ratio = degree_ratio,
    true_positive_rate = true_positive_rate,
    adjusted_estimate = adjusted_estimate,
    adjusted_prevalence = adjusted_estimate / N_F,
    
    # RDS components  
    rds_prevalence = rds_prevalence,
    rds_estimate = rds_estimate,
    
    # Diagnostics
    empirical_degree_ratio = empirical_degree_ratio,
    
    # Sample sizes
    n_total = nrow(data),
    n_out_reports = sum(valid_out_cases),
    n_degree = sum(valid_degree_cases), 
    n_hidden = sum(hidden_cases, na.rm = TRUE),
    n_rds = sum(rds_cases),
    
    # Ratios for interpretation
    basic_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0, 
                            basic_estimate / rds_estimate, NA),
    adjusted_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0, 
                               adjusted_estimate / rds_estimate, NA)
  ))
}

# ============================================================================
# BOOTSTRAP FUNCTION FOR CONFIDENCE INTERVALS 
# ============================================================================

bootstrap_basic_nsum <- function(data, rds_var, nsum_var, degree_var, weight_var, N_F,
                                 degree_ratio, true_positive_rate, scheme_name, 
                                 n_bootstrap = 1000) {
  
  cat("  Bootstrapping", scheme_name, "with", n_bootstrap, "samples...\n")
  
  bootstrap_estimates <- replicate(n_bootstrap, {
    # Resample with replacement
    boot_indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    boot_data <- data[boot_indices, ]
    
    # Get point estimates
    result <- modified_basic_nsum(boot_data, rds_var, nsum_var, degree_var, 
                                 weight_var, N_F, degree_ratio, true_positive_rate,
                                 scheme_name)
    
    if (is.null(result$error)) {
      c(basic = result$basic_estimate, 
        adjusted = result$adjusted_estimate,
        rds = result$rds_estimate)
    } else {
      c(basic = NA, adjusted = NA, rds = NA)
    }
  }, simplify = TRUE)
  
  # Calculate confidence intervals
  alpha <- 1 - basic_nsum_config$confidence_level
  
  basic_ci <- quantile(bootstrap_estimates["basic", ], c(alpha/2, 1-alpha/2), na.rm = TRUE)
  adjusted_ci <- quantile(bootstrap_estimates["adjusted", ], c(alpha/2, 1-alpha/2), na.rm = TRUE)
  rds_ci <- quantile(bootstrap_estimates["rds", ], c(alpha/2, 1-alpha/2), na.rm = TRUE)
  
  return(list(
    basic_ci_lower = basic_ci[1],
    basic_ci_upper = basic_ci[2],
    adjusted_ci_lower = adjusted_ci[1],
    adjusted_ci_upper = adjusted_ci[2], 
    rds_ci_lower = rds_ci[1], 
    rds_ci_upper = rds_ci[2],
    n_valid_bootstrap = sum(!is.na(bootstrap_estimates["basic", ])),
    bootstrap_estimates = bootstrap_estimates
  ))
}

# ============================================================================
# MAIN ANALYSIS: MODIFIED BASIC SCALE-UP ESTIMATOR
# ============================================================================

cat("Starting Modified Basic Scale-Up Estimator analysis...\n\n")

# Initialize results storage
all_basic_results <- list()
result_counter <- 1

# Main analysis loop (using default adjustment factors)
for (indicator_name in names(basic_nsum_config$indicators)) {
  
  indicator_info <- basic_nsum_config$indicators[[indicator_name]]
  cat("=== Processing:", indicator_name, "(", indicator_info$confidence, "confidence ) ===\n")
  
  for (scheme_name in names(basic_nsum_config$weight_schemes)) {
    
    weight_var <- basic_nsum_config$weight_schemes[[scheme_name]]
    cat("  Weight scheme:", scheme_name, "\n")
    
    for (N_F in basic_nsum_config$frame_population_sizes) {
      
      cat("    Population size:", format(N_F, scientific = FALSE), "\n")
      
      # Point estimates with default adjustment factors
      result <- modified_basic_nsum(
        data = dd,
        rds_var = indicator_info$rds_var,
        nsum_var = indicator_info$nsum_var, 
        degree_var = basic_nsum_config$degree_var,
        weight_var = weight_var,
        N_F = N_F,
        degree_ratio = basic_nsum_config$adjustment_factors$default_degree_ratio,
        true_positive_rate = basic_nsum_config$adjustment_factors$default_true_positive_rate,
        scheme_name = scheme_name
      )
      
      # Add metadata
      result$indicator_name <- indicator_name
      result$confidence_level <- indicator_info$confidence
      
      # Bootstrap confidence intervals (for preferred population size only)
      if (N_F == basic_nsum_config$preferred_size) {
        bootstrap_result <- bootstrap_basic_nsum(
          data = dd,
          rds_var = indicator_info$rds_var,
          nsum_var = indicator_info$nsum_var,
          degree_var = basic_nsum_config$degree_var, 
          weight_var = weight_var,
          N_F = N_F,
          degree_ratio = basic_nsum_config$adjustment_factors$default_degree_ratio,
          true_positive_rate = basic_nsum_config$adjustment_factors$default_true_positive_rate,
          scheme_name = scheme_name,
          n_bootstrap = basic_nsum_config$bootstrap_samples
        )
        
        result <- c(result, bootstrap_result)
      }
      
      # Store result
      all_basic_results[[result_counter]] <- result
      result_counter <- result_counter + 1
    }
  }
  cat("\n")
}

cat("Basic analysis completed! Total results:", length(all_basic_results), "\n\n")

# ============================================================================
# RESULTS COMPILATION AND SUMMARY
# ============================================================================

cat("Compiling results into summary tables...\n")

# Convert to data frame
basic_results_df <- map_dfr(all_basic_results, function(r) {
  tibble(
    indicator_name = r$indicator_name %||% "unknown",
    indicator = r$indicator,
    confidence_level = r$confidence_level %||% "unknown", 
    scheme = r$scheme,
    N_F = r$N_F,
    
    # Basic NSUM results
    y_FH = r$y_FH %||% NA,
    d_FF = r$d_FF %||% NA,
    basic_estimate = r$basic_estimate %||% NA,
    basic_prevalence = r$basic_prevalence %||% NA,
    
    # Adjusted results
    degree_ratio = r$degree_ratio %||% NA,
    true_positive_rate = r$true_positive_rate %||% NA,
    adjusted_estimate = r$adjusted_estimate %||% NA,
    adjusted_prevalence = r$adjusted_prevalence %||% NA,
    
    # RDS results
    rds_prevalence = r$rds_prevalence %||% NA,
    rds_estimate = r$rds_estimate %||% NA,
    
    # Diagnostics
    empirical_degree_ratio = r$empirical_degree_ratio %||% NA,
    
    # Comparison ratios
    basic_rds_ratio = r$basic_rds_ratio %||% NA,
    adjusted_rds_ratio = r$adjusted_rds_ratio %||% NA,
    
    # Sample sizes
    n_total = r$n_total %||% NA,
    n_out_reports = r$n_out_reports %||% NA,
    n_degree = r$n_degree %||% NA,
    n_hidden = r$n_hidden %||% NA,
    n_rds = r$n_rds %||% NA,
    
    # Bootstrap CIs (if available)
    basic_ci_lower = r$basic_ci_lower %||% NA,
    basic_ci_upper = r$basic_ci_upper %||% NA,
    adjusted_ci_lower = r$adjusted_ci_lower %||% NA,
    adjusted_ci_upper = r$adjusted_ci_upper %||% NA,
    rds_ci_lower = r$rds_ci_lower %||% NA,
    rds_ci_upper = r$rds_ci_upper %||% NA,
    
    # Error tracking
    error = r$error %||% NA
  )
})

# Filter out errored results
valid_basic_results <- basic_results_df %>% filter(is.na(error))

cat("Valid results:", nrow(valid_basic_results), "out of", nrow(basic_results_df), "\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save comprehensive results
save(all_basic_results, basic_results_df, valid_basic_results, basic_nsum_config,
     file = here("output", "modified_basic_nsum_results.RData"))

# Save summary table
write_csv(valid_basic_results, here("output", "tables", "modified_basic_nsum_summary.csv"))

# Quick preview of key results
cat("\n=== PREVIEW: Modified Basic Scale-Up Estimator Results (980K population) ===\n")
preview <- valid_basic_results %>%
  filter(N_F == 980000) %>%
  select(indicator_name, scheme, basic_estimate, adjusted_estimate, rds_estimate, 
         basic_rds_ratio, empirical_degree_ratio) %>%
  arrange(indicator_name, scheme)

print(preview, n = 20)

cat("\n=== Analysis Complete ===\n")
cat("Results saved to: output/modified_basic_nsum_results.RData\n")
cat("Summary saved to: output/tables/modified_basic_nsum_summary.csv\n")
cat("\nKey Formula Used: N_H = (y_F,H / d_F,F) * N_F\n")
cat("With adjustment factors for bias correction available\n")
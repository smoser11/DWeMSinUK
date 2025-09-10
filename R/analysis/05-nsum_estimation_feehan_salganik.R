# 05-nsum_estimation_feehan_salganik.R
# Proper NSUM Estimation Following Feehan & Salganik (2016) Framework
# Domestic Worker Exploitation and Modern Slavery in UK
# 
# Using RDS sample from FRAME population to estimate HIDDEN population size
# via visibility estimation from ex post identified hidden population members

cat("=== NSUM vs RDS Comparison: Feehan & Salganik (2016) Framework ===\n")
cat("Estimating hidden population size using visibility from RDS frame sample\n")
cat("Formula: N_H = (y_F,H / v_H,F) where v_H,F estimated from RDS weights\n\n")

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
# CONFIGURATION FOR ROBUST NSUM vs RDS COMPARISON
# ============================================================================

comparison_config <- list(
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
  
  # Network size variable for visibility estimation
  degree_var = "known_network_size",  # Q13 2f
  
  # Multiple RDS weighting schemes for robustness  
  weight_schemes = list(
    "unweighted" = NULL,
    "rds_I_document_withholding" = "wt.RDS1_document_withholding",
    "rds_I_pay_issues" = "wt.RDS1_pay_issues", 
    "rds_I_threats_abuse" = "wt.RDS1_threats_abuse",
    "rds_I_excessive_hours" = "wt.RDS1_excessive_hours",
    "rds_I_access_to_help" = "wt.RDS1_access_to_help",
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k",
    "vh_050k" = "wt.vh_050k",
    "ss_980k" = "wt.SS_980k",  # Fixed: uppercase SS
    "ss_100k" = "wt.SS_100k",  # Added additional SS weights  
    "ss_050k" = "wt.SS_050k",
    "ss_1740k" = "wt.SS_1740k"
  ),
  
  # Multiple frame population sizes for robustness
  frame_population_sizes = c(50000, 100000, 980000, 1740000),
  preferred_size = 980000,  # EU baseline
  
  # Bootstrap settings
  bootstrap_samples = 1000,
  confidence_level = 0.95
)

cat("Configuration loaded:\n")
cat("- Indicators:", length(comparison_config$indicators), "\n")
cat("- Weight schemes:", length(comparison_config$weight_schemes), "\n") 
cat("- Population sizes:", length(comparison_config$frame_population_sizes), "\n")
cat("- Bootstrap samples:", comparison_config$bootstrap_samples, "\n")
cat("- Total combinations:", length(comparison_config$indicators) * length(comparison_config$weight_schemes) * length(comparison_config$frame_population_sizes), "\n\n")

# ============================================================================
# CORE NSUM vs RDS COMPARISON FUNCTION
# ============================================================================

compare_nsum_rds <- function(data, rds_var, nsum_var, degree_var, weight_var, N_F, 
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
  
  # Get weights
  if (is.null(weight_var) || !weight_var %in% names(data)) {
    weights <- rep(1, nrow(data))
    if (!is.null(weight_var)) {
      cat("Warning: Weight variable", weight_var, "not found. Using unweighted.\n")
    }
  } else {
    weights <- data[[weight_var]]
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
  
  y_FH <- sum(out_reports[valid_out_cases] * weights[valid_out_cases])
  
  # ==== STEP 2: Estimate Visibility v_H,F ====
  # Average visibility of hidden population to frame, weighted by RDS weights
  hidden_pop_cases <- !is.na(data[[rds_var]]) & data[[rds_var]] == 1 & 
                      !is.na(data[[degree_var]]) & data[[degree_var]] > 0 &
                      valid_weight_cases
  
  if (sum(hidden_pop_cases) == 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name,
      N_F = N_F, 
      error = "No valid hidden population cases for visibility estimation"
    ))
  }
  
  # Feehan & Salganik visibility estimator (Equation 7 adapted)
  # v_H,F = sum(degree_i / w_i) / sum(1 / w_i) where w_i are RDS weights
  degrees <- data[[degree_var]][hidden_pop_cases]
  weights_hidden <- weights[hidden_pop_cases]
  
  # Note: RDS weights are typically 1/pi_i, so we use them directly
  numerator <- sum(degrees * weights_hidden)
  denominator <- sum(weights_hidden)
  v_HF <- numerator / denominator
  
  if (v_HF <= 0) {
    return(list(
      indicator = rds_var,
      scheme = scheme_name,
      N_F = N_F,
      error = "Invalid visibility estimate (<=0)"
    ))
  }
  
  # ==== STEP 3: Generalized NSUM Estimator ====
  # N_H = y_F,H / v_H,F 
  NSUM_estimate <- y_FH / v_HF
  
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
  
  # ==== STEP 5: Return Results ====
  return(list(
    indicator = rds_var,
    scheme = scheme_name,
    N_F = N_F,
    
    # NSUM components
    y_FH = y_FH,
    v_HF = v_HF,
    NSUM_estimate = NSUM_estimate,
    NSUM_prevalence = NSUM_estimate / N_F,
    
    # RDS components  
    rds_prevalence = rds_prevalence,
    rds_estimate = rds_estimate,
    
    # Sample sizes
    n_total = nrow(data),
    n_out_reports = sum(valid_out_cases),
    n_hidden_visibility = sum(hidden_pop_cases),
    n_rds = sum(rds_cases),
    
    # Ratio for interpretation
    nsum_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0, 
                           NSUM_estimate / rds_estimate, NA)
  ))
}

# ============================================================================
# BOOTSTRAP FUNCTION FOR CONFIDENCE INTERVALS 
# ============================================================================

bootstrap_comparison <- function(data, rds_var, nsum_var, degree_var, weight_var, N_F,
                                scheme_name, n_bootstrap = 1000) {
  
  cat("  Bootstrapping", scheme_name, "with", n_bootstrap, "samples...\n")
  
  bootstrap_estimates <- replicate(n_bootstrap, {
    # Resample with replacement
    boot_indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    boot_data <- data[boot_indices, ]
    
    # Get point estimates
    result <- compare_nsum_rds(boot_data, rds_var, nsum_var, degree_var, 
                              weight_var, N_F, scheme_name)
    
    if (is.null(result$error)) {
      c(NSUM = result$NSUM_estimate, RDS = result$rds_estimate)
    } else {
      c(NSUM = NA, RDS = NA)
    }
  }, simplify = TRUE)
  
  # Calculate confidence intervals
  alpha <- 1 - comparison_config$confidence_level
  
  nsum_ci <- quantile(bootstrap_estimates["NSUM", ], c(alpha/2, 1-alpha/2), na.rm = TRUE)
  rds_ci <- quantile(bootstrap_estimates["RDS", ], c(alpha/2, 1-alpha/2), na.rm = TRUE)
  
  return(list(
    nsum_ci_lower = nsum_ci[1],
    nsum_ci_upper = nsum_ci[2],
    rds_ci_lower = rds_ci[1], 
    rds_ci_upper = rds_ci[2],
    n_valid_bootstrap = sum(!is.na(bootstrap_estimates["NSUM", ])),
    bootstrap_estimates = bootstrap_estimates
  ))
}

# ============================================================================
# MAIN ANALYSIS: COMPREHENSIVE NSUM vs RDS COMPARISON
# ============================================================================

cat("Starting comprehensive NSUM vs RDS comparison analysis...\n\n")

# Initialize results storage
all_results <- list()
result_counter <- 1

# Progress tracking is displayed in configuration section above

# Main analysis loop
for (indicator_name in names(comparison_config$indicators)) {
  
  indicator_info <- comparison_config$indicators[[indicator_name]]
  cat("=== Processing:", indicator_name, "(", indicator_info$confidence, "confidence ) ===\n")
  
  for (scheme_name in names(comparison_config$weight_schemes)) {
    
    weight_var <- comparison_config$weight_schemes[[scheme_name]]
    cat("  Weight scheme:", scheme_name, "\n")
    
    for (N_F in comparison_config$frame_population_sizes) {
      
      cat("    Population size:", format(N_F, scientific = FALSE), "\n")
      
      # Point estimates
      result <- compare_nsum_rds(
        data = dd,
        rds_var = indicator_info$rds_var,
        nsum_var = indicator_info$nsum_var, 
        degree_var = comparison_config$degree_var,
        weight_var = weight_var,
        N_F = N_F,
        scheme_name = scheme_name
      )
      
      # Add metadata
      result$indicator_name <- indicator_name
      result$confidence_level <- indicator_info$confidence
      
      # Bootstrap confidence intervals (for preferred population size only to save time)
      if (N_F == comparison_config$preferred_size) {
        bootstrap_result <- bootstrap_comparison(
          data = dd,
          rds_var = indicator_info$rds_var,
          nsum_var = indicator_info$nsum_var,
          degree_var = comparison_config$degree_var, 
          weight_var = weight_var,
          N_F = N_F,
          scheme_name = scheme_name,
          n_bootstrap = comparison_config$bootstrap_samples
        )
        
        result <- c(result, bootstrap_result)
      }
      
      # Store result
      all_results[[result_counter]] <- result
      result_counter <- result_counter + 1
    }
  }
  cat("\n")
}

cat("Analysis completed! Total results:", length(all_results), "\n\n")

# ============================================================================
# RESULTS COMPILATION AND SUMMARY
# ============================================================================

cat("Compiling results into summary tables...\n")

# Convert to data frame
results_df <- map_dfr(all_results, function(r) {
  tibble(
    indicator_name = r$indicator_name %||% "unknown",
    indicator = r$indicator,
    confidence_level = r$confidence_level %||% "unknown", 
    scheme = r$scheme,
    N_F = r$N_F,
    
    # NSUM results
    y_FH = r$y_FH %||% NA,
    v_HF = r$v_HF %||% NA,
    NSUM_estimate = r$NSUM_estimate %||% NA,
    NSUM_prevalence = r$NSUM_prevalence %||% NA,
    
    # RDS results
    rds_prevalence = r$rds_prevalence %||% NA,
    rds_estimate = r$rds_estimate %||% NA,
    
    # Comparison
    nsum_rds_ratio = r$nsum_rds_ratio %||% NA,
    
    # Sample sizes
    n_total = r$n_total %||% NA,
    n_out_reports = r$n_out_reports %||% NA,
    n_hidden_visibility = r$n_hidden_visibility %||% NA,
    n_rds = r$n_rds %||% NA,
    
    # Bootstrap CIs (if available)
    nsum_ci_lower = r$nsum_ci_lower %||% NA,
    nsum_ci_upper = r$nsum_ci_upper %||% NA,
    rds_ci_lower = r$rds_ci_lower %||% NA,
    rds_ci_upper = r$rds_ci_upper %||% NA,
    
    # Error tracking
    error = r$error %||% NA
  )
})

# Filter out errored results for main summary
valid_results <- results_df %>% filter(is.na(error))

cat("Valid results:", nrow(valid_results), "out of", nrow(results_df), "\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save comprehensive results
save(all_results, results_df, valid_results, comparison_config,
     file = here("output", "nsum_rds_comparison_feehan_salganik.RData"))

# Save summary table
write_csv(valid_results, here("output", "tables", "nsum_rds_comparison_summary.csv"))

# Quick preview of key results
cat("\n=== PREVIEW: NSUM vs RDS Comparison (980K population) ===\n")
preview <- valid_results %>%
  filter(N_F == 980000) %>%
  select(indicator_name, scheme, NSUM_estimate, rds_estimate, nsum_rds_ratio) %>%
  arrange(indicator_name, scheme)

print(preview, n = 20)

cat("\n=== Analysis Complete ===\n")
cat("Results saved to: output/nsum_rds_comparison_feehan_salganik.RData\n")
cat("Summary saved to: output/tables/nsum_rds_comparison_summary.csv\n")
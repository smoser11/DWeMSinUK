# Modified Basic Scale-Up Estimator with Adjustment Factors
# Following Feehan & Salganik (2016) Equation 15 and 24
# 
# N_H = (y_F,H / d_F,F) * N_F * (1/δ_F) * (1/τ_F) * η_F
# 
# Where:
# - y_F,H: total out-reports from frame population
# - d_F,F: average degree within frame population  
# - N_F: frame population size
# - δ_F: degree ratio (hidden pop degree / frame pop degree)
# - τ_F: true positive rate (reporting accuracy)
# - η_F: precision of out-reports (1 if no false positives)

library(tidyverse)
library(RDS)
library(boot)
library(here)

# Load data (assuming dd is available)
# load(here("data", "processed", "prepared_data.RData"))

# ============================================================================
# CONFIGURATION FOR NSUM WITH ADJUSTMENT FACTORS
# ============================================================================

nsum_config <- list(
  # Comparable indicators (as defined in your data prep)
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
  
  # Network degree variable
  degree_var = "known_network_size",  # Q13
  
  # Weight schemes for robustness testing (prioritizing SS per user preference)
  weight_schemes = list(
    # SS weights (user preference)
    "ss_980k" = "wt.SS_980k",
    "ss_100k" = "wt.SS_100k",
    "ss_050k" = "wt.SS_050k", 
    "ss_1740k" = "wt.SS_1740k",
    
    # VH weights (alternative)
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k", 
    "vh_050k" = "wt.vh_050k",
    "vh_1740k" = "wt.vh_1740k",
    
    # RDS-I weights (for specific indicators)
    "rds_I_document" = "wt.RDS1_document_withholding",
    "rds_I_pay" = "wt.RDS1_pay_issues",
    "rds_I_threats" = "wt.RDS1_threats_abuse",
    
    # Unweighted (baseline)
    "unweighted" = NULL
  ),
  
  # Frame population sizes for sensitivity analysis
  frame_sizes = c(50000, 100000, 500000, 980000, 1500000, 1740000, 2000000),
  
  # Adjustment factor ranges for sensitivity analysis
  adjustment_factors = list(
    # Degree ratio: hidden pop degree / frame pop degree
    degree_ratio = seq(0.5, 2.0, by = 0.25),  # Hidden pop has 50% to 200% of frame degree
    
    # True positive rate: reporting accuracy (1 = perfect, <1 = underreporting)
    true_positive_rate = seq(0.3, 1.0, by = 0.1),  # 30% to 100% accurate reporting
    
    # Precision of out-reports (typically 1, but can test for false positives)
    precision = c(0.8, 0.9, 1.0)  # 80% to 100% precision (no false positives = 1.0)
  ),
  
  # Default "best guess" adjustment factors
  default_adjustments = list(
    degree_ratio = 1.0,      # Assume same average degree
    true_positive_rate = 0.7, # Assume 70% reporting accuracy (conservative)
    precision = 1.0          # Assume no false positives
  )
)

cat("NSUM Configuration loaded:\n")
cat("- Indicators:", length(nsum_config$indicators), "\n")
cat("- Weight schemes:", length(nsum_config$weight_schemes), "\n")
cat("- Frame sizes:", length(nsum_config$frame_sizes), "\n")
cat("- Degree ratios:", length(nsum_config$adjustment_factors$degree_ratio), "\n")
cat("- True positive rates:", length(nsum_config$adjustment_factors$true_positive_rate), "\n\n")

# ============================================================================
# MODIFIED BASIC SCALE-UP ESTIMATOR WITH ADJUSTMENT FACTORS
# ============================================================================

calculate_nsum_with_adjustments <- function(data, rds_var, nsum_var, degree_var, 
                                          weight_var = NULL, N_F, 
                                          degree_ratio = 1.0, 
                                          true_positive_rate = 1.0, 
                                          precision = 1.0,
                                          scheme_name = "unknown") {
  
  # Input validation
  required_vars <- c(rds_var, nsum_var, degree_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  
  if (length(missing_vars) > 0) {
    return(list(
      error = paste("Missing variables:", paste(missing_vars, collapse = ", ")),
      scheme = scheme_name, N_F = N_F, degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate, precision = precision
    ))
  }
  
  # Set up weights
  if (is.null(weight_var) || !weight_var %in% names(data)) {
    weights <- rep(1, nrow(data))
  } else {
    weights <- data[[weight_var]]
    weights[is.na(weights) | weights <= 0] <- 1  # Handle invalid weights
  }
  
  valid_cases <- !is.na(weights) & weights > 0 & is.finite(weights)
  
  # ====================================================================
  # STEP 1: Calculate y_F,H (total out-reports)
  # ====================================================================
  
  out_reports <- data[[nsum_var]]
  valid_out_cases <- !is.na(out_reports) & valid_cases
  
  if (sum(valid_out_cases) == 0) {
    return(list(
      error = "No valid out-report cases",
      scheme = scheme_name, N_F = N_F, degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate, precision = precision
    ))
  }
  
  # Convert binary indicators to counts if needed
  if (all(out_reports[valid_out_cases] %in% c(0, 1), na.rm = TRUE)) {
    # Binary indicator: sum weighted reports
    y_FH <- sum(out_reports[valid_out_cases] * weights[valid_out_cases])
  } else {
    # Count data: sum weighted counts
    y_FH <- sum(out_reports[valid_out_cases] * weights[valid_out_cases])
  }
  
  # ====================================================================
  # STEP 2: Calculate d_F,F (average degree within frame population)
  # ====================================================================
  
  degrees <- data[[degree_var]]
  valid_degree_cases <- !is.na(degrees) & degrees >= 0 & valid_cases
  
  if (sum(valid_degree_cases) == 0) {
    return(list(
      error = "No valid degree cases",
      scheme = scheme_name, N_F = N_F, degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate, precision = precision
    ))
  }
  
  # Weighted average degree
  d_FF <- sum(degrees[valid_degree_cases] * weights[valid_degree_cases]) / 
          sum(weights[valid_degree_cases])
  
  if (d_FF <= 0) {
    return(list(
      error = "Invalid average degree (<=0)",
      scheme = scheme_name, N_F = N_F, degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate, precision = precision
    ))
  }
  
  # ====================================================================
  # STEP 3: Apply Modified Basic Scale-Up with Adjustment Factors
  # ====================================================================
  
  # Feehan & Salganik (2016) Equation 24:
  # N_H = (y_F,H / d_F,F) * N_F * (1/δ_F) * (1/τ_F) * η_F
  
  basic_estimate <- (y_FH / d_FF) * N_F
  
  # Apply adjustment factors
  adjusted_estimate <- basic_estimate * (1 / degree_ratio) * (1 / true_positive_rate) * precision
  
  # ====================================================================
  # STEP 4: Calculate RDS prevalence for comparison
  # ====================================================================
  
  rds_cases <- !is.na(data[[rds_var]]) & valid_cases
  
  if (sum(rds_cases) > 0) {
    rds_prevalence <- sum(data[[rds_var]][rds_cases] * weights[rds_cases]) / 
                      sum(weights[rds_cases])
    rds_estimate <- rds_prevalence * N_F
  } else {
    rds_prevalence <- NA
    rds_estimate <- NA
  }
  
  # ====================================================================
  # STEP 5: Return comprehensive results
  # ====================================================================
  
  return(list(
    # Identification
    indicator = rds_var,
    scheme = scheme_name,
    N_F = N_F,
    
    # Adjustment factors used
    degree_ratio = degree_ratio,
    true_positive_rate = true_positive_rate,
    precision = precision,
    
    # NSUM components
    y_FH = y_FH,
    d_FF = d_FF,
    basic_estimate = basic_estimate,
    adjusted_estimate = adjusted_estimate,
    nsum_prevalence = adjusted_estimate / N_F,
    
    # RDS comparison
    rds_prevalence = rds_prevalence,
    rds_estimate = rds_estimate,
    
    # Ratios for interpretation
    nsum_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0, 
                           adjusted_estimate / rds_estimate, NA),
    adjustment_impact = adjusted_estimate / basic_estimate,
    
    # Sample sizes
    n_total = nrow(data),
    n_out_reports = sum(valid_out_cases),
    n_degree_valid = sum(valid_degree_cases),
    n_rds = sum(rds_cases, na.rm = TRUE)
  ))
}

# ============================================================================
# COMPREHENSIVE SENSITIVITY ANALYSIS
# ============================================================================

run_sensitivity_analysis <- function(data, save_results = TRUE) {
  
  cat("Starting comprehensive NSUM sensitivity analysis...\n\n")
  
  # Initialize results storage
  all_results <- list()
  result_counter <- 1
  
  # Calculate total combinations for progress tracking
  total_combinations <- length(nsum_config$indicators) * 
                       length(nsum_config$weight_schemes) * 
                       length(nsum_config$frame_sizes) * 
                       length(nsum_config$adjustment_factors$degree_ratio) * 
                       length(nsum_config$adjustment_factors$true_positive_rate) * 
                       length(nsum_config$adjustment_factors$precision)
  
  cat("Total combinations to test:", format(total_combinations, big.mark = ","), "\n")
  cat("This may take several minutes...\n\n")
  
  # Progress counter
  progress_counter <- 0
  
  # Main analysis loop
  for (indicator_name in names(nsum_config$indicators)) {
    
    indicator_info <- nsum_config$indicators[[indicator_name]]
    cat("=== Processing:", indicator_name, "===\n")
    
    for (scheme_name in names(nsum_config$weight_schemes)) {
      
      weight_var <- nsum_config$weight_schemes[[scheme_name]]
      
      for (N_F in nsum_config$frame_sizes) {
        
        for (delta_F in nsum_config$adjustment_factors$degree_ratio) {
          
          for (tau_F in nsum_config$adjustment_factors$true_positive_rate) {
            
            for (eta_F in nsum_config$adjustment_factors$precision) {
              
              progress_counter <- progress_counter + 1
              
              # Run estimation
              result <- calculate_nsum_with_adjustments(
                data = data,
                rds_var = indicator_info$rds_var,
                nsum_var = indicator_info$nsum_var,
                degree_var = nsum_config$degree_var,
                weight_var = weight_var,
                N_F = N_F,
                degree_ratio = delta_F,
                true_positive_rate = tau_F,
                precision = eta_F,
                scheme_name = scheme_name
              )
              
              # Add metadata
              result$indicator_name <- indicator_name
              result$confidence_level <- indicator_info$confidence
              result$progress <- progress_counter
              
              # Store result
              all_results[[result_counter]] <- result
              result_counter <- result_counter + 1
            }
          }
        }
      }
      
      # Progress update every 100 combinations
      if (progress_counter %% 100 == 0) {
        pct_complete <- round(100 * progress_counter / total_combinations, 1)
        cat("  Progress:", progress_counter, "/", total_combinations, 
            "(", pct_complete, "%)\n")
      }
    }
  }
  
  cat("\nAnalysis completed! Total results:", length(all_results), "\n\n")
  
  # ========================================================================
  # Convert to data frame for analysis
  # ========================================================================
  
  cat("Converting results to data frame...\n")
  
  results_df <- map_dfr(all_results, function(r) {
    tibble(
      # Identification
      indicator_name = r$indicator_name %||% "unknown",
      indicator = r$indicator %||% "unknown",
      confidence_level = r$confidence_level %||% "unknown",
      scheme = r$scheme %||% "unknown",
      N_F = r$N_F %||% NA,
      
      # Adjustment factors
      degree_ratio = r$degree_ratio %||% NA,
      true_positive_rate = r$true_positive_rate %||% NA,
      precision = r$precision %||% NA,
      
      # NSUM results
      y_FH = r$y_FH %||% NA,
      d_FF = r$d_FF %||% NA,
      basic_estimate = r$basic_estimate %||% NA,
      adjusted_estimate = r$adjusted_estimate %||% NA,
      nsum_prevalence = r$nsum_prevalence %||% NA,
      
      # RDS comparison
      rds_prevalence = r$rds_prevalence %||% NA,
      rds_estimate = r$rds_estimate %||% NA,
      
      # Ratios and impact
      nsum_rds_ratio = r$nsum_rds_ratio %||% NA,
      adjustment_impact = r$adjustment_impact %||% NA,
      
      # Sample sizes
      n_total = r$n_total %||% NA,
      n_out_reports = r$n_out_reports %||% NA,
      n_degree_valid = r$n_degree_valid %||% NA,
      n_rds = r$n_rds %||% NA,
      
      # Error handling
      error = r$error %||% NA
    )
  })
  
  # Filter valid results
  valid_results <- results_df %>% filter(is.na(error))
  
  cat("Valid results:", nrow(valid_results), "out of", nrow(results_df), "\n")
  
  # ========================================================================
  # Save results
  # ========================================================================
  
  if (save_results) {
    save(all_results, results_df, valid_results, nsum_config,
         file = here("output", "nsum_sensitivity_analysis.RData"))
    
    write_csv(valid_results, here("output", "tables", "nsum_sensitivity_results.csv"))
    
    cat("Results saved to:\n")
    cat("- output/nsum_sensitivity_analysis.RData\n")
    cat("- output/tables/nsum_sensitivity_results.csv\n")
  }
  
  return(list(
    all_results = all_results,
    results_df = results_df,
    valid_results = valid_results
  ))
}

# ============================================================================
# SUMMARY ANALYSIS FUNCTIONS
# ============================================================================

analyze_sensitivity <- function(results_df) {
  
  cat("\n=== SENSITIVITY ANALYSIS SUMMARY ===\n\n")
  
  # Filter to valid results only
  valid_data <- results_df %>% filter(is.na(error))
  
  if (nrow(valid_data) == 0) {
    cat("No valid results to analyze!\n")
    return(NULL)
  }
  
  # 1. Range of estimates by indicator
  cat("1. ESTIMATE RANGES BY INDICATOR:\n")
  estimate_ranges <- valid_data %>%
    group_by(indicator_name) %>%
    summarise(
      min_estimate = min(adjusted_estimate, na.rm = TRUE),
      max_estimate = max(adjusted_estimate, na.rm = TRUE),
      median_estimate = median(adjusted_estimate, na.rm = TRUE),
      range_ratio = max_estimate / min_estimate,
      .groups = 'drop'
    ) %>%
    arrange(desc(range_ratio))
  
  print(estimate_ranges)
  
  # 2. Impact of adjustment factors
  cat("\n2. ADJUSTMENT FACTOR IMPACTS:\n")
  
  # Effect of degree ratio
  degree_impact <- valid_data %>%
    group_by(degree_ratio) %>%
    summarise(
      mean_adjustment_impact = mean(adjustment_impact, na.rm = TRUE),
      median_estimate = median(adjusted_estimate, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Degree ratio effects:\n")
  print(degree_impact)
  
  # Effect of true positive rate
  tpr_impact <- valid_data %>%
    group_by(true_positive_rate) %>%
    summarise(
      mean_adjustment_impact = mean(adjustment_impact, na.rm = TRUE),
      median_estimate = median(adjusted_estimate, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("\nTrue positive rate effects:\n")
  print(tpr_impact)
  
  # 3. Weight scheme comparison
  cat("\n3. WEIGHT SCHEME COMPARISON:\n")
  weight_comparison <- valid_data %>%
    filter(N_F == 980000, degree_ratio == 1.0, true_positive_rate == 0.7) %>%
    group_by(scheme, indicator_name) %>%
    summarise(
      estimate = mean(adjusted_estimate, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = scheme, values_from = estimate)
  
  print(weight_comparison)
  
  # 4. Population size sensitivity
  cat("\n4. POPULATION SIZE SENSITIVITY:\n")
  pop_sensitivity <- valid_data %>%
    filter(degree_ratio == 1.0, true_positive_rate == 0.7, scheme == "vh_980k") %>%
    group_by(N_F, indicator_name) %>%
    summarise(
      prevalence = mean(nsum_prevalence, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = indicator_name, values_from = prevalence)
  
  print(pop_sensitivity)
  
  return(list(
    estimate_ranges = estimate_ranges,
    degree_impact = degree_impact,
    tpr_impact = tpr_impact,
    weight_comparison = weight_comparison,
    pop_sensitivity = pop_sensitivity
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("\n=== EXAMPLE: Single estimation with default parameters ===\n")

# Example single calculation (assuming 'dd' data is loaded)
if (exists("dd")) {
  example_result <- calculate_nsum_with_adjustments(
    data = dd,
    rds_var = "document_withholding_rds",
    nsum_var = "document_withholding_nsum", 
    degree_var = "known_network_size",
    weight_var = "wt.vh_980k",
    N_F = 980000,
    degree_ratio = nsum_config$default_adjustments$degree_ratio,
    true_positive_rate = nsum_config$default_adjustments$true_positive_rate,
    precision = nsum_config$default_adjustments$precision,
    scheme_name = "vh_980k_default"
  )
  
  if (is.null(example_result$error)) {
    cat("NSUM estimate (document withholding):", 
        format(round(example_result$adjusted_estimate), big.mark = ","), "\n")
    cat("RDS estimate:", 
        format(round(example_result$rds_estimate), big.mark = ","), "\n")
    cat("NSUM/RDS ratio:", round(example_result$nsum_rds_ratio, 2), "\n")
  } else {
    cat("Error in example calculation:", example_result$error, "\n")
  }
} else {
  cat("Data 'dd' not found. Load prepared data first.\n")
}

cat("\n=== To run full sensitivity analysis, use: ===\n")
cat("sensitivity_results <- run_sensitivity_analysis(dd)\n")
cat("summary_analysis <- analyze_sensitivity(sensitivity_results$valid_results)\n")
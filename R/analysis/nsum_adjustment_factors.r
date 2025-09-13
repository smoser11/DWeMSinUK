# Robust NSUM with RDS Weights and Survey Bootstrap
# Following Feehan & Salganik (2016) and using surveybootstrap package
# for proper variance estimation with complex survey designs

library(tidyverse)
library(here)
library(surveybootstrap)

# ============================================================================
# RDS-AWARE NSUM CONFIGURATION
# ============================================================================

robust_nsum_config <- list(
  
  # Comparable indicators (CE's specifications 2024-11-05)
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
  degree_var = "known_network_size",  # Q13: network size with contact details
  
  # RDS weights for inclusion probabilities
  rds_weights = list(
    # SS weights (user preference for different population sizes)
    "ss_980k" = list(weight_var = "wt.SS_980k", N_F = 980000),
    "ss_100k" = list(weight_var = "wt.SS_100k", N_F = 100000), 
    "ss_050k" = list(weight_var = "wt.SS_050k", N_F = 50000),
    "ss_1740k" = list(weight_var = "wt.SS_1740k", N_F = 1740000),
    
    # VH weights (Volz-Heckathorn)
    "vh_980k" = list(weight_var = "wt.vh_980k", N_F = 980000),
    "vh_100k" = list(weight_var = "wt.vh_100k", N_F = 100000),
    "vh_050k" = list(weight_var = "wt.vh_050k", N_F = 50000), 
    "vh_1740k" = list(weight_var = "wt.vh_1740k", N_F = 1740000),
    
    # RDS-I weights (indicator-specific)
    "rds_I_document" = list(weight_var = "wt.RDS1_document_withholding", N_F = 980000),
    "rds_I_pay" = list(weight_var = "wt.RDS1_pay_issues", N_F = 980000),
    "rds_I_threats" = list(weight_var = "wt.RDS1_threats_abuse", N_F = 980000)
  ),
  
  # Adjustment factor ranges (Feehan & Salganik 2016)
  adjustment_factors = list(
    # δ_F: degree ratio (hidden pop degree / frame pop degree)  
    degree_ratio = seq(0.5, 2.0, by = 0.25),
    
    # τ_F: true positive rate (reporting accuracy)
    true_positive_rate = seq(0.3, 1.0, by = 0.1),
    
    # η_F: precision (1 - false positive rate)
    precision = c(0.8, 0.9, 1.0)
  ),
  
  # Bootstrap parameters
  bootstrap = list(
    n_reps = 1000,           # Number of bootstrap replications
    parallel = TRUE,         # Use parallel processing
    use_psu_var = "id",      # Primary sampling unit variable (individual level)
    use_strata_var = NULL    # No explicit stratification in RDS
  )
)

cat("Robust NSUM configuration loaded:\n")
cat("- Indicators:", length(robust_nsum_config$indicators), "\n")
cat("- RDS weight schemes:", length(robust_nsum_config$rds_weights), "\n") 
cat("- Adjustment factor combinations:", 
    length(robust_nsum_config$adjustment_factors$degree_ratio) *
    length(robust_nsum_config$adjustment_factors$true_positive_rate) *
    length(robust_nsum_config$adjustment_factors$precision), "\n\n")

# ============================================================================
# INCLUSION PROBABILITY CALCULATION
# ============================================================================

calculate_inclusion_probabilities <- function(data, weight_var, N_F) {
  
  if (is.null(weight_var) || !weight_var %in% names(data)) {
    cat("Warning: Weight variable", weight_var, "not found, using equal probabilities\n")
    return(rep(1/nrow(data), nrow(data)))
  }
  
  # Get RDS weights
  weights <- data[[weight_var]]
  
  # Handle missing or invalid weights
  weights[is.na(weights) | weights <= 0] <- median(weights, na.rm = TRUE)
  
  # Convert RDS weights to inclusion probabilities 
  # RDS weights are π_i^{-1}, so π_i = 1/weights (up to scaling)
  pi_i_raw <- 1 / weights
  
  # Rescale so that sum(π_i) = 1 (as required for NSUM)
  pi_i <- pi_i_raw / sum(pi_i_raw, na.rm = TRUE)
  
  # Validate
  if (abs(sum(pi_i, na.rm = TRUE) - 1) > 1e-6) {
    warning("Inclusion probabilities do not sum to 1: ", sum(pi_i, na.rm = TRUE))
  }
  
  return(pi_i)
}

# ============================================================================
# ROBUST NSUM ESTIMATOR WITH INCLUSION PROBABILITIES
# ============================================================================

calculate_robust_nsum <- function(data, rds_var, nsum_var, degree_var, 
                                  weight_var, N_F, 
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
      scheme = scheme_name, N_F = N_F
    ))
  }
  
  # Calculate inclusion probabilities from RDS weights
  pi_i <- calculate_inclusion_probabilities(data, weight_var, N_F)
  
  # Add to data for easier handling
  data$pi_i <- pi_i
  
  # Filter valid cases
  valid_cases <- !is.na(data[[rds_var]]) & !is.na(data[[nsum_var]]) & 
                 !is.na(data[[degree_var]]) & !is.na(pi_i) & pi_i > 0
  
  if (sum(valid_cases) == 0) {
    return(list(
      error = "No valid cases after filtering",
      scheme = scheme_name, N_F = N_F
    ))
  }
  
  valid_data <- data[valid_cases, ]
  
  # ========================================================================
  # STEP 1: Calculate weighted proportion of out-reports (y_F,H)
  # Using inclusion probabilities as specified in Feehan & Salganik
  # ========================================================================
  
  # Horvitz-Thompson estimator for out-report proportion
  out_reports <- valid_data[[nsum_var]]
  pi_i_valid <- valid_data$pi_i
  
  # y_F,H = sum(Y_i / π_i) / sum(1 / π_i) where Y_i is out-report indicator
  numerator <- sum(out_reports / pi_i_valid, na.rm = TRUE)
  denominator <- sum(1 / pi_i_valid, na.rm = TRUE) 
  
  y_FH_proportion <- numerator / denominator
  
  # ========================================================================
  # STEP 2: Calculate weighted average degree (d_F,F)
  # ========================================================================
  
  degrees <- valid_data[[degree_var]]
  
  # Weighted average degree using inclusion probabilities
  d_FF <- sum(degrees / pi_i_valid, na.rm = TRUE) / sum(1 / pi_i_valid, na.rm = TRUE)
  
  if (d_FF <= 0) {
    return(list(
      error = "Invalid average degree (<=0)", 
      scheme = scheme_name, N_F = N_F
    ))
  }
  
  # ========================================================================
  # STEP 3: Calculate basic NSUM estimate
  # ========================================================================
  
  # Basic NSUM: N_H = (y_F,H / d_F,F) * N_F
  basic_estimate <- (y_FH_proportion / d_FF) * N_F
  
  # ========================================================================
  # STEP 4: Apply adjustment factors (Feehan & Salganik 2016, Eq. 24)
  # ========================================================================
  
  # Adjusted NSUM: N_H = basic * (1/δ_F) * (1/τ_F) * η_F
  adjusted_estimate <- basic_estimate * (1/degree_ratio) * (1/true_positive_rate) * precision
  
  # ========================================================================
  # STEP 5: Calculate RDS prevalence for comparison
  # ========================================================================
  
  rds_cases <- valid_data[[rds_var]]
  rds_prevalence <- sum(rds_cases / pi_i_valid, na.rm = TRUE) / sum(1 / pi_i_valid, na.rm = TRUE)
  rds_estimate <- rds_prevalence * N_F
  
  # ========================================================================
  # STEP 6: Return comprehensive results
  # ========================================================================
  
  return(list(
    # Identification
    indicator = rds_var,
    scheme = scheme_name,
    N_F = N_F,
    
    # Adjustment factors
    degree_ratio = degree_ratio,
    true_positive_rate = true_positive_rate, 
    precision = precision,
    
    # NSUM components
    y_FH = y_FH_proportion,
    d_FF = d_FF,
    basic_estimate = basic_estimate,
    adjusted_estimate = adjusted_estimate,
    nsum_prevalence = adjusted_estimate / N_F,
    
    # RDS comparison
    rds_prevalence = rds_prevalence,
    rds_estimate = rds_estimate,
    
    # Inclusion probability info
    pi_i_mean = mean(pi_i_valid),
    pi_i_min = min(pi_i_valid),
    pi_i_max = max(pi_i_valid),
    
    # Ratios for interpretation
    nsum_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0,
                           adjusted_estimate / rds_estimate, NA),
    adjustment_impact = adjusted_estimate / basic_estimate,
    
    # Sample sizes
    n_total = nrow(data), 
    n_valid = sum(valid_cases),
    n_out_reports = sum(out_reports, na.rm = TRUE),
    n_rds_positive = sum(rds_cases, na.rm = TRUE)
  ))
}

# ============================================================================
# SURVEY BOOTSTRAP FOR NSUM CONFIDENCE INTERVALS  
# ============================================================================

robust_nsum_bootstrap <- function(data, rds_var, nsum_var, degree_var,
                                  weight_var, N_F,
                                  degree_ratio = 1.0,
                                  true_positive_rate = 1.0,
                                  precision = 1.0,
                                  n_boot = 1000) {

  cat("Running survey bootstrap for", rds_var, "with", n_boot, "replications...\n")

  # First debug: check the input data
  valid_input <- !is.na(data[[rds_var]]) & !is.na(data[[nsum_var]]) &
                 !is.na(data[[degree_var]]) & !is.na(data[[weight_var]])

  cat("Input data validation:\n")
  cat("- Total observations:", nrow(data), "\n")
  cat("- Valid observations:", sum(valid_input), "\n")
  cat("- Missing", rds_var, ":", sum(is.na(data[[rds_var]])), "\n")
  cat("- Missing", nsum_var, ":", sum(is.na(data[[nsum_var]])), "\n")
  cat("- Missing", degree_var, ":", sum(is.na(data[[degree_var]])), "\n")
  cat("- Missing", weight_var, ":", sum(is.na(data[[weight_var]])), "\n")

  if (sum(valid_input) < 10) {
    cat("ERROR: Insufficient valid observations for bootstrap\n")
    return(list(
      nsum_ci_lower = NA, nsum_ci_upper = NA,
      rds_ci_lower = NA, rds_ci_upper = NA,
      n_valid_boot = 0, boot_estimates = NULL
    ))
  }

  # Work with valid data only
  data_valid <- data[valid_input, ]

  # Prepare survey design for bootstrap
  # For RDS, each individual is a PSU (primary sampling unit)
  data_valid$psu_id <- 1:nrow(data_valid)

  # Add inclusion probabilities
  pi_i <- calculate_inclusion_probabilities(data_valid, weight_var, N_F)
  data_valid$survey_weight <- 1 / pi_i  # Survey weights = 1/π_i

  cat("Inclusion probabilities: min =", round(min(pi_i), 6),
      "max =", round(max(pi_i), 6),
      "mean =", round(mean(pi_i), 6), "\n")

  # Define survey design formula
  # weight ~ psu_vars (no strata for basic RDS)
  survey_design <- survey_weight ~ psu_id

  # Debug: Check surveybootstrap package structure
  cat("Attempting rescaled bootstrap with", nrow(data_valid), "observations...\n")

  # Draw bootstrap samples using rescaled bootstrap
  boot_samples <- tryCatch({
    rescaled.bootstrap.sample(
      survey.data = data_valid,
      survey.design = survey_design,
      num.reps = n_boot,
      parallel = robust_nsum_config$bootstrap$parallel
    )
  }, error = function(e) {
    cat("Bootstrap sampling failed:", e$message, "\n")
    return(NULL)
  })

  if (is.null(boot_samples)) {
    cat("ERROR: Bootstrap sampling failed\n")
    return(list(
      nsum_ci_lower = NA, nsum_ci_upper = NA,
      rds_ci_lower = NA, rds_ci_upper = NA,
      n_valid_boot = 0, boot_estimates = NULL
    ))
  }

  cat("Generated", length(boot_samples), "bootstrap samples\n")

  # Debug: Check structure of first bootstrap sample
  if (length(boot_samples) > 0) {
    cat("Bootstrap sample structure:\n")
    cat("- Names:", names(boot_samples[[1]]), "\n")
    cat("- Sample 1 length:", nrow(boot_samples[[1]]), "\n")
    cat("- Sample 1 columns:", names(boot_samples[[1]]), "\n")
  }
  
  # Calculate NSUM estimates for each bootstrap sample
  boot_estimates <- map_dfr(1:length(boot_samples), function(b) {

    boot_data <- boot_samples[[b]]

    # Debug bootstrap sample structure
    if (b <= 3) {  # Only debug first 3 samples
      cat("Bootstrap sample", b, ":\n")
      cat("- Columns:", paste(names(boot_data), collapse = ", "), "\n")
      cat("- Rows:", nrow(boot_data), "\n")
      if ("index" %in% names(boot_data)) {
        cat("- Index range:", range(boot_data$index), "\n")
      }
      if ("weight.scale" %in% names(boot_data)) {
        cat("- Weight.scale range:", round(range(boot_data$weight.scale), 4), "\n")
      }
    }

    # Merge bootstrap info back with original data
    if (!"index" %in% names(boot_data)) {
      cat("ERROR: No 'index' column in bootstrap sample", b, "\n")
      return(tibble(nsum_est = NA, rds_est = NA))
    }

    boot_merged <- data_valid[boot_data$index, ]
    boot_merged$boot_weight <- data_valid$survey_weight[boot_data$index] * boot_data$weight.scale
    
    # Calculate NSUM estimate with bootstrap weights
    result <- tryCatch({

      # Use bootstrap-adjusted weights
      pi_i_boot <- 1 / boot_merged$boot_weight
      pi_i_boot <- pi_i_boot / sum(pi_i_boot, na.rm = TRUE)  # Rescale to sum to 1

      # Apply NSUM calculation with bootstrap weights
      valid_cases <- !is.na(boot_merged[[rds_var]]) & !is.na(boot_merged[[nsum_var]]) &
                    !is.na(boot_merged[[degree_var]]) & !is.na(pi_i_boot) & pi_i_boot > 0

      if (b <= 3) {  # Debug first few samples
        cat("  Valid cases in sample", b, ":", sum(valid_cases), "out of", length(valid_cases), "\n")
        cat("  Missing in bootstrap sample:\n")
        cat("  -", rds_var, ":", sum(is.na(boot_merged[[rds_var]])), "\n")
        cat("  -", nsum_var, ":", sum(is.na(boot_merged[[nsum_var]])), "\n")
        cat("  -", degree_var, ":", sum(is.na(boot_merged[[degree_var]])), "\n")
        cat("  - pi_i_boot:", sum(is.na(pi_i_boot) | pi_i_boot <= 0), "\n")
      }

      if (sum(valid_cases) < 10) {
        if (b <= 3) cat("  Insufficient valid cases (<10) in sample", b, "\n")
        return(tibble(nsum_est = NA, rds_est = NA))
      }

      valid_boot <- boot_merged[valid_cases, ]
      pi_boot_valid <- pi_i_boot[valid_cases]

      # NSUM calculation
      out_reports <- valid_boot[[nsum_var]]
      degrees <- valid_boot[[degree_var]]

      y_FH_prop <- sum(out_reports / pi_boot_valid, na.rm = TRUE) / sum(1 / pi_boot_valid, na.rm = TRUE)
      d_FF <- sum(degrees / pi_boot_valid, na.rm = TRUE) / sum(1 / pi_boot_valid, na.rm = TRUE)

      if (b <= 3) {
        cat("  y_FH_prop =", round(y_FH_prop, 4), ", d_FF =", round(d_FF, 4), "\n")
      }

      if (d_FF <= 0) {
        if (b <= 3) cat("  d_FF <= 0, returning NA\n")
        return(tibble(nsum_est = NA, rds_est = NA))
      }

      basic_est <- (y_FH_prop / d_FF) * N_F
      nsum_est <- basic_est * (1/degree_ratio) * (1/true_positive_rate) * precision

      # RDS estimate
      rds_cases <- valid_boot[[rds_var]]
      rds_prev <- sum(rds_cases / pi_boot_valid, na.rm = TRUE) / sum(1 / pi_boot_valid, na.rm = TRUE)
      rds_est <- rds_prev * N_F

      if (b <= 3) {
        cat("  Final estimates: NSUM =", round(nsum_est), ", RDS =", round(rds_est), "\n")
      }

      tibble(nsum_est = nsum_est, rds_est = rds_est)

    }, error = function(e) {
      if (b <= 3) cat("  Error in sample", b, ":", e$message, "\n")
      tibble(nsum_est = NA, rds_est = NA)
    })
    
    result$boot_rep <- b
    return(result)
  })
  
  # Calculate confidence intervals
  nsum_valid <- boot_estimates$nsum_est[!is.na(boot_estimates$nsum_est)]
  rds_valid <- boot_estimates$rds_est[!is.na(boot_estimates$rds_est)]
  
  if (length(nsum_valid) > 10) {
    nsum_ci <- quantile(nsum_valid, c(0.025, 0.975), na.rm = TRUE)
  } else {
    nsum_ci <- c(NA, NA)
  }
  
  if (length(rds_valid) > 10) {
    rds_ci <- quantile(rds_valid, c(0.025, 0.975), na.rm = TRUE)  
  } else {
    rds_ci <- c(NA, NA)
  }
  
  cat("Bootstrap completed:", length(nsum_valid), "valid NSUM estimates,", 
      length(rds_valid), "valid RDS estimates\n")
  
  return(list(
    nsum_ci_lower = nsum_ci[1],
    nsum_ci_upper = nsum_ci[2], 
    rds_ci_lower = rds_ci[1],
    rds_ci_upper = rds_ci[2],
    n_valid_boot = length(nsum_valid),
    boot_estimates = boot_estimates
  ))
}

# ============================================================================
# COMPREHENSIVE ROBUST NSUM ANALYSIS
# ============================================================================

run_robust_nsum_analysis <- function(data, save_results = TRUE) {
  
  cat("=== Starting Robust NSUM Analysis with Survey Bootstrap ===\n\n")
  
  all_results <- list()
  result_counter <- 1
  
  # Calculate total combinations
  total_combinations <- length(robust_nsum_config$indicators) * 
                       length(robust_nsum_config$rds_weights) *
                       length(robust_nsum_config$adjustment_factors$degree_ratio) * 
                       length(robust_nsum_config$adjustment_factors$true_positive_rate) *
                       length(robust_nsum_config$adjustment_factors$precision)
  
  cat("Total analysis combinations:", format(total_combinations, big.mark = ","), "\n")
  cat("This analysis uses proper survey bootstrap and may take longer...\n\n")
  
  progress <- 0
  
  # Main analysis loop
  for (indicator_name in names(robust_nsum_config$indicators)) {
    
    indicator_info <- robust_nsum_config$indicators[[indicator_name]]
    cat("=== Processing indicator:", indicator_name, "===\n")
    
    for (scheme_name in names(robust_nsum_config$rds_weights)) {
      
      scheme_info <- robust_nsum_config$rds_weights[[scheme_name]]
      weight_var <- scheme_info$weight_var
      N_F <- scheme_info$N_F
      
      for (delta_F in robust_nsum_config$adjustment_factors$degree_ratio) {
        for (tau_F in robust_nsum_config$adjustment_factors$true_positive_rate) {
          for (eta_F in robust_nsum_config$adjustment_factors$precision) {
            
            progress <- progress + 1
            
            # Calculate point estimate
            result <- calculate_robust_nsum(
              data = data,
              rds_var = indicator_info$rds_var,
              nsum_var = indicator_info$nsum_var,
              degree_var = robust_nsum_config$degree_var,
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
            result$progress <- progress
            
            # Store result
            all_results[[result_counter]] <- result
            result_counter <- result_counter + 1
            
            # Progress update
            if (progress %% 50 == 0) {
              pct_complete <- round(100 * progress / total_combinations, 1)
              cat("  Progress:", progress, "/", total_combinations, "(", pct_complete, "%)\n")
            }
          }
        }
      }
    }
  }
  
  cat("\nPoint estimation completed! Total results:", length(all_results), "\n")
  
  # Convert to data frame
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
      
      # Inclusion probability info
      pi_i_mean = r$pi_i_mean %||% NA,
      pi_i_min = r$pi_i_min %||% NA, 
      pi_i_max = r$pi_i_max %||% NA,
      
      # Ratios and impact
      nsum_rds_ratio = r$nsum_rds_ratio %||% NA,
      adjustment_impact = r$adjustment_impact %||% NA,
      
      # Sample info
      n_total = r$n_total %||% NA,
      n_valid = r$n_valid %||% NA,
      n_out_reports = r$n_out_reports %||% NA,
      n_rds_positive = r$n_rds_positive %||% NA,
      
      # Error handling
      error = r$error %||% NA
    )
  })
  
  # Filter valid results
  valid_results <- results_df %>% filter(is.na(error))
  
  cat("Valid results:", nrow(valid_results), "out of", nrow(results_df), "\n\n")
  
  # Save results
  if (save_results) {
    save(all_results, results_df, valid_results, robust_nsum_config,
         file = here("output", "robust_nsum_analysis.RData"))
    
    write_csv(valid_results, here("output", "tables", "robust_nsum_results.csv"))
    
    cat("Results saved to:\n")
    cat("- output/robust_nsum_analysis.RData\n") 
    cat("- output/tables/robust_nsum_results.csv\n\n")
  }
  
  return(list(
    all_results = all_results,
    results_df = results_df,
    valid_results = valid_results
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("\n=== Example: Robust NSUM with inclusion probabilities ===\n")

if (exists("dd")) {
  example_result <- calculate_robust_nsum(
    data = dd,
    rds_var = "document_withholding_rds",
    nsum_var = "document_withholding_nsum",
    degree_var = "known_network_size", 
    weight_var = "wt.vh_980k",
    N_F = 980000,
    scheme_name = "vh_980k_example"
  )
  
  if (is.null(example_result$error)) {
    cat("Robust NSUM estimate:", format(round(example_result$adjusted_estimate), big.mark = ","), "\n")
    cat("RDS estimate:", format(round(example_result$rds_estimate), big.mark = ","), "\n") 
    cat("NSUM/RDS ratio:", round(example_result$nsum_rds_ratio, 2), "\n")
    cat("Inclusion prob range:", round(example_result$pi_i_min, 4), "to", round(example_result$pi_i_max, 4), "\n")
  } else {
    cat("Error in example:", example_result$error, "\n")
  }
} else {
  cat("Data 'dd' not found. Load prepared data first.\n")
}

cat("\n=== To run full robust analysis with survey bootstrap, use: ===\n")
cat("robust_results <- run_robust_nsum_analysis(dd)\n\n")
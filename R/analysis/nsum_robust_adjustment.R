# nsum_robust_adjustment.R
# Robust NSUM with Adjustment Factors
# Following Feehan & Salganik (2016) AOAS
#
# Extends core NSUM estimation with adjustment factors for:
# - Degree ratio (δ_F): hidden pop degree / frame pop degree
# - True positive rate (τ_F): reporting accuracy
# - Precision (η_F): 1 - false positive rate
#
# For bootstrap confidence intervals, see nsum_bootstrap.R

cat("=== Robust NSUM with Adjustment Factors ===\n")
cat("Following Feehan & Salganik (2016)\n\n")

# Load required libraries
library(tidyverse)
library(here)

# Source core NSUM functions
source(here("R", "analysis", "05-nsum_estimation.R"))

# ============================================================================
# CONFIGURATION
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

  # Adjustment factor ranges (Feehan & Salganik 2016 Table 2)
  adjustment_factors = list(
    # δ_F: degree ratio (hidden pop degree / frame pop degree)
    # Values < 1 indicate hidden pop has smaller networks
    degree_ratio = c(0.5, 0.75, 1.0, 1.5, 2.0),

    # τ_F: true positive rate (reporting accuracy)
    # Lower values indicate under-reporting
    true_positive_rate = c(0.5, 0.7, 0.8, 0.9, 1.0),

    # η_F: precision (1 - false positive rate)
    # Lower values indicate over-reporting
    precision = c(0.9, 0.95, 1.0)
  ),

  # RDS weighting schemes to test
  rds_weight_schemes = c("unweighted", "ss_980k", "vh_980k"),

  # Population sizes to test
  population_sizes = c(980000),  # Focus on main population for robustness checks

  # Focus on key scenarios to reduce computational burden
  run_full_sensitivity = FALSE  # Set to TRUE for exhaustive sensitivity analysis
)

cat("Robust NSUM configuration:\n")
cat("- Indicators:", length(robust_nsum_config$indicators), "\n")
cat("- Degree ratios:", length(robust_nsum_config$adjustment_factors$degree_ratio), "\n")
cat("- TPR values:", length(robust_nsum_config$adjustment_factors$true_positive_rate), "\n")
cat("- Precision values:", length(robust_nsum_config$adjustment_factors$precision), "\n")
cat("- Total adjustment combinations:",
    length(robust_nsum_config$adjustment_factors$degree_ratio) *
    length(robust_nsum_config$adjustment_factors$true_positive_rate) *
    length(robust_nsum_config$adjustment_factors$precision), "\n\n")

# ============================================================================
# INCLUSION PROBABILITY CALCULATION
# ============================================================================

calculate_inclusion_probabilities <- function(data, weight_var, N_F) {

  # Validate inputs
  if (is.null(weight_var) || !weight_var %in% names(data)) {
    warning("Weight variable '", weight_var, "' not found, using equal probabilities")
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
# ROBUST NSUM ESTIMATOR WITH ADJUSTMENT FACTORS
# ============================================================================

calculate_robust_nsum <- function(data,
                                  nsum_var,
                                  rds_var = NULL,
                                  degree_vars,
                                  probe_sizes,
                                  weight_var = NULL,
                                  N_F = 980000,
                                  degree_ratio = 1.0,
                                  true_positive_rate = 1.0,
                                  precision = 1.0,
                                  scheme_name = "unweighted") {

  # Step 1: Get basic NSUM estimate using core function
  weights <- if (!is.null(weight_var) && weight_var %in% names(data)) {
    data[[weight_var]]
  } else {
    NULL
  }

  basic_result <- estimate_nsum_population(
    data = data,
    weights = weights,
    hidden_connections_var = nsum_var,
    degree_vars = degree_vars,
    probe_sizes = probe_sizes,
    total_population_size = N_F,
    method = if (is.null(weights)) "basic" else "weighted",
    weighting_scheme = scheme_name,
    verbose = FALSE
  )

  # Check for errors in basic estimation
  if ("error" %in% names(basic_result)) {
    return(list(
      error = basic_result$error,
      scheme = scheme_name,
      N_F = N_F
    ))
  }

  basic_estimate <- basic_result$N_H_estimate
  y_FH <- basic_result$y_F_H
  d_FF <- basic_result$d_F_F

  # Step 2: Apply adjustment factors (Feehan & Salganik 2016, Eq. 24)
  # Adjusted NSUM: N_H = basic × (1/δ_F) × (1/τ_F) × η_F
  # Where:
  #   δ_F = degree_ratio: ratio of hidden to frame network sizes
  #   τ_F = true_positive_rate: probability of accurate reporting
  #   η_F = precision: 1 - false positive rate

  adjusted_estimate <- basic_estimate * (1/degree_ratio) * (1/true_positive_rate) * precision

  # Step 3: Calculate RDS prevalence for comparison (if rds_var provided)
  if (!is.null(rds_var) && rds_var %in% names(data)) {

    # Calculate inclusion probabilities if weights available
    if (!is.null(weight_var) && weight_var %in% names(data)) {
      pi_i <- calculate_inclusion_probabilities(data, weight_var, N_F)

      # Horvitz-Thompson estimator for RDS prevalence
      valid_cases <- !is.na(data[[rds_var]]) & !is.na(pi_i) & pi_i > 0
      if (sum(valid_cases) > 0) {
        rds_cases <- data[[rds_var]][valid_cases]
        pi_valid <- pi_i[valid_cases]

        rds_prevalence <- sum(rds_cases / pi_valid, na.rm = TRUE) /
                         sum(1 / pi_valid, na.rm = TRUE)
        rds_estimate <- rds_prevalence * N_F

        pi_i_mean <- mean(pi_valid)
        pi_i_min <- min(pi_valid)
        pi_i_max <- max(pi_valid)
      } else {
        rds_prevalence <- NA
        rds_estimate <- NA
        pi_i_mean <- NA
        pi_i_min <- NA
        pi_i_max <- NA
      }
    } else {
      # Unweighted RDS estimate
      rds_prevalence <- mean(data[[rds_var]], na.rm = TRUE)
      rds_estimate <- rds_prevalence * N_F
      pi_i_mean <- 1 / nrow(data)
      pi_i_min <- 1 / nrow(data)
      pi_i_max <- 1 / nrow(data)
    }
  } else {
    rds_prevalence <- NA
    rds_estimate <- NA
    pi_i_mean <- NA
    pi_i_min <- NA
    pi_i_max <- NA
  }

  # Step 4: Return comprehensive results
  return(list(
    # Identification
    nsum_indicator = nsum_var,
    rds_indicator = rds_var,
    scheme = scheme_name,
    N_F = N_F,

    # Adjustment factors
    degree_ratio = degree_ratio,
    true_positive_rate = true_positive_rate,
    precision = precision,

    # NSUM estimates
    basic_estimate = basic_estimate,
    adjusted_estimate = adjusted_estimate,
    nsum_prevalence = adjusted_estimate / N_F,

    # NSUM components
    y_FH = y_FH,
    d_FF = d_FF,
    n_probes_used = basic_result$n_probes_used,

    # RDS comparison
    rds_prevalence = rds_prevalence,
    rds_estimate = rds_estimate,

    # Inclusion probability info (for weighted schemes)
    pi_i_mean = pi_i_mean,
    pi_i_min = pi_i_min,
    pi_i_max = pi_i_max,

    # Derived metrics
    nsum_rds_ratio = ifelse(!is.na(rds_estimate) && rds_estimate > 0,
                           adjusted_estimate / rds_estimate, NA),
    adjustment_impact = adjusted_estimate / basic_estimate,

    # Sample info
    sample_size = basic_result$sample_size,
    n_missing = basic_result$n_missing
  ))
}

# ============================================================================
# COMPREHENSIVE ROBUST NSUM ANALYSIS
# ============================================================================

run_robust_nsum_analysis <- function(data = rd.dd,
                                    indicators = robust_nsum_config$indicators,
                                    weight_schemes = robust_nsum_config$rds_weight_schemes,
                                    population_sizes = robust_nsum_config$population_sizes,
                                    adjustment_factors = robust_nsum_config$adjustment_factors,
                                    run_full_sensitivity = robust_nsum_config$run_full_sensitivity,
                                    save_results = TRUE) {

  cat("=== Starting Robust NSUM Analysis ===\n\n")

  # Determine scenarios to run
  if (run_full_sensitivity) {
    cat("Running FULL sensitivity analysis (all adjustment factor combinations)\n")
    delta_F_values <- adjustment_factors$degree_ratio
    tau_F_values <- adjustment_factors$true_positive_rate
    eta_F_values <- adjustment_factors$precision
  } else {
    cat("Running KEY scenarios only (reduced computational burden)\n")
    # Focus on key scenarios: neutral (1.0, 1.0, 1.0) and realistic ranges
    delta_F_values <- c(0.75, 1.0, 1.5)  # Modest degree differences
    tau_F_values <- c(0.7, 0.9, 1.0)     # Under-reporting to accurate
    eta_F_values <- c(0.95, 1.0)         # High precision
  }

  total_combinations <- length(indicators) * length(weight_schemes) *
                       length(population_sizes) *
                       length(delta_F_values) * length(tau_F_values) * length(eta_F_values)

  cat("Total analysis combinations:", format(total_combinations, big.mark = ","), "\n\n")

  all_results <- list()
  result_counter <- 1
  progress <- 0

  # Main analysis loop
  for (indicator_name in names(indicators)) {

    indicator_info <- indicators[[indicator_name]]
    cat("=== Processing:", indicator_name, "===\n")

    for (N_F in population_sizes) {

      cat("  Population:", format(N_F, big.mark = ","), "\n")

      for (scheme_name in weight_schemes) {

        cat("    Scheme:", scheme_name, "\n")

        # Get weight variable name
        weight_var <- if (scheme_name == "unweighted") {
          NULL
        } else {
          nsum_config$weighting_schemes[[scheme_name]]
        }

        for (delta_F in delta_F_values) {
          for (tau_F in tau_F_values) {
            for (eta_F in eta_F_values) {

              progress <- progress + 1

              # Calculate robust NSUM with adjustments
              result <- calculate_robust_nsum(
                data = data,
                nsum_var = indicator_info$nsum_var,
                rds_var = indicator_info$rds_var,
                degree_vars = nsum_config$degree_vars,
                probe_sizes = nsum_config$probe_sizes,
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

              # Progress update every 20 iterations
              if (progress %% 20 == 0) {
                pct_complete <- round(100 * progress / total_combinations, 1)
                cat("      Progress:", progress, "/", total_combinations,
                    "(", pct_complete, "%)\n")
              }
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
      nsum_indicator = r$nsum_indicator %||% "unknown",
      rds_indicator = r$rds_indicator %||% "unknown",
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

      # Derived metrics
      nsum_rds_ratio = r$nsum_rds_ratio %||% NA,
      adjustment_impact = r$adjustment_impact %||% NA,

      # Sample info
      sample_size = r$sample_size %||% NA,
      n_missing = r$n_missing %||% NA,
      n_probes_used = r$n_probes_used %||% NA,

      # Error handling
      error = r$error %||% NA
    )
  })

  # Filter valid results
  valid_results <- results_df %>% filter(is.na(error))

  cat("Valid results:", nrow(valid_results), "out of", nrow(results_df), "\n\n")

  # Extract key scenarios for main results
  main_results <- valid_results %>%
    filter(
      degree_ratio == 1.0,
      true_positive_rate == 1.0,
      precision == 1.0,
      scheme %in% c("ss_980k", "unweighted")
    ) %>%
    select(indicator_name, scheme, N_F, adjusted_estimate, nsum_prevalence,
           rds_estimate, rds_prevalence, nsum_rds_ratio, sample_size)

  cat("Main results (neutral adjustments):\n")
  print(main_results)
  cat("\n")

  # Save results
  if (save_results) {
    output_file <- here("output", "robust_nsum_adjustment_results.RData")
    save(all_results, results_df, valid_results, main_results, robust_nsum_config,
         file = output_file)
    cat("Saved:", output_file, "\n")

    csv_file <- here("output", "tables", "robust_nsum_adjustment_results.csv")
    write_csv(valid_results, csv_file)
    cat("Saved:", csv_file, "\n\n")
  }

  return(list(
    all_results = all_results,
    results_df = results_df,
    valid_results = valid_results,
    main_results = main_results
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("\n=== Example: Robust NSUM with adjustment factors ===\n")

if (exists("rd.dd")) {
  example_result <- calculate_robust_nsum(
    data = rd.dd,
    nsum_var = "document_withholding_nsum",
    rds_var = "document_withholding_rds",
    degree_vars = nsum_config$degree_vars,
    probe_sizes = nsum_config$probe_sizes,
    weight_var = "wt.SS_980k",
    N_F = 980000,
    degree_ratio = 0.75,      # Hidden pop has 75% network size of frame
    true_positive_rate = 0.7, # 70% reporting accuracy
    precision = 0.95,         # 95% precision (5% false positives)
    scheme_name = "ss_980k"
  )

  if (is.null(example_result$error)) {
    cat("Basic NSUM estimate:", format(round(example_result$basic_estimate), big.mark = ","), "\n")
    cat("Adjusted NSUM estimate:", format(round(example_result$adjusted_estimate), big.mark = ","), "\n")
    cat("Adjustment impact:", round(example_result$adjustment_impact, 2), "×\n")
    if (!is.na(example_result$rds_estimate)) {
      cat("RDS estimate:", format(round(example_result$rds_estimate), big.mark = ","), "\n")
      cat("NSUM/RDS ratio:", round(example_result$nsum_rds_ratio, 2), "\n")
    }
  } else {
    cat("Error in example:", example_result$error, "\n")
  }
} else {
  cat("Data 'rd.dd' not found. Load prepared data first.\n")
}

cat("\n=== To run full robust analysis, use: ===\n")
cat("robust_results <- run_robust_nsum_analysis(rd.dd)\n\n")

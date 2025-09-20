# ==============================================================================
# STEP 3: NSUM ESTIMATION ON RESAMPLED AND REWEIGHTED DATA
# ==============================================================================
#
# This script implements various NSUM estimators for bootstrap samples from
# Step 2 (reweighted RDS data). Each estimator takes a weighted bootstrap
# replicate and produces a hidden population size estimate.
#
# Key estimators implemented:
# 1. Modified Basic Scale-Up (MBSU) - with adjustment factors δ, τ, ρ
# 2. Generalized NSUM (GNSUM) - standard weighted NSUM
# 3. GNSUM with Symmetric Visibility - incorporates in-reports from hidden members
# 4. Model-Based NSUM - framework for Bayesian approaches
#
# Author: NSUM-RDS Bootstrap Team
# Date: 2025-01-XX
# ==============================================================================

library(tidyverse)
library(here)

# Set execution control
SKIP_EXECUTION <- FALSE
if (exists("skip_step3") && skip_step3) SKIP_EXECUTION <- TRUE

if (!SKIP_EXECUTION) {
  cat("=== Loading Step 3: NSUM Estimation Functions ===\n")
}

# ==============================================================================
# MODIFIED BASIC SCALE-UP (MBSU) ESTIMATOR
# ==============================================================================
#
# Implements the MBSU estimator from the existing nsum_adjustment_factors.r
# with three key adjustment factors:
# - δ (delta): Transmission bias - probability respondent aware of alter's status
# - τ (tau): Barrier effect - social mixing between hidden and frame population
# - ρ (rho): Popularity bias - relative visibility of hidden population members
#
# Formula: N_H = (y_F,H / d_F,F) * N_F * (1/δ) * (1/τ) * ρ
# ==============================================================================

estimate_mbsu <- function(data,
                         outcome_variable,
                         degree_variable,
                         frame_size,
                         weight_column = "inclusion_prob",
                         adjustment_factors = list(
                           delta = 1.0,    # Transmission bias (default: no bias)
                           tau = 1.0,      # Barrier effect (default: no barriers)
                           rho = 1.0       # Popularity bias (default: no bias)
                         ),
                         validate_inputs = TRUE,
                         verbose = FALSE) {

  if (verbose) {
    cat("=== MBSU Estimation ===\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
    cat("Adjustment factors: δ =", adjustment_factors$delta,
        ", τ =", adjustment_factors$tau,
        ", ρ =", adjustment_factors$rho, "\n")
  }

  # Input validation
  if (validate_inputs) {
    required_vars <- c(outcome_variable, degree_variable, weight_column)
    missing_vars <- required_vars[!required_vars %in% names(data)]

    if (length(missing_vars) > 0) {
      stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
    }

    if (frame_size <= 0) {
      stop("Frame size must be positive")
    }

    if (any(c(adjustment_factors$delta, adjustment_factors$tau, adjustment_factors$rho) <= 0)) {
      stop("All adjustment factors must be positive")
    }
  }

  # Extract required variables
  y_iH <- data[[outcome_variable]]  # Out-reports to hidden population
  d_i <- data[[degree_variable]]    # Network degrees
  pi_i <- data[[weight_column]]     # Inclusion probabilities

  # Filter valid cases
  valid_cases <- !is.na(y_iH) & !is.na(d_i) & !is.na(pi_i) & pi_i > 0

  if (sum(valid_cases) == 0) {
    warning("No valid cases for MBSU estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = NA,
      d_FF_average = NA,
      basic_estimate = NA,
      adjustment_impact = NA,
      n_valid = 0,
      method = "MBSU",
      error = "No valid cases"
    ))
  }

  # Work with valid data
  y_valid <- y_iH[valid_cases]
  d_valid <- d_i[valid_cases]
  pi_valid <- pi_i[valid_cases]

  # STEP 1: Calculate weighted proportion of out-reports (y_F,H)
  # Using Horvitz-Thompson estimator: y_F,H = Σ(Y_i / π_i) / Σ(1 / π_i)
  numerator_y <- sum(y_valid / pi_valid, na.rm = TRUE)
  denominator <- sum(1 / pi_valid, na.rm = TRUE)
  y_FH_proportion <- numerator_y / denominator

  # STEP 2: Calculate weighted average degree (d_F,F)
  numerator_d <- sum(d_valid / pi_valid, na.rm = TRUE)
  d_FF_average <- numerator_d / denominator

  if (d_FF_average <= 0) {
    warning("Invalid average degree (≤0) in MBSU estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = y_FH_proportion,
      d_FF_average = d_FF_average,
      basic_estimate = NA,
      adjustment_impact = NA,
      n_valid = sum(valid_cases),
      method = "MBSU",
      error = "Invalid average degree"
    ))
  }

  # STEP 3: Calculate basic NSUM estimate
  basic_estimate <- (y_FH_proportion / d_FF_average) * frame_size

  # STEP 4: Apply MBSU adjustment factors
  # Formula: N_H = basic * (1/δ) * (1/τ) * ρ
  delta_adjustment <- 1 / adjustment_factors$delta
  tau_adjustment <- 1 / adjustment_factors$tau
  rho_adjustment <- adjustment_factors$rho

  adjusted_estimate <- basic_estimate * delta_adjustment * tau_adjustment * rho_adjustment

  # Calculate adjustment impact
  total_adjustment <- delta_adjustment * tau_adjustment * rho_adjustment

  if (verbose) {
    cat("y_F,H proportion:", round(y_FH_proportion, 4), "\n")
    cat("d_F,F average:", round(d_FF_average, 2), "\n")
    cat("Basic estimate:", format(round(basic_estimate), big.mark = ","), "\n")
    cat("Total adjustment factor:", round(total_adjustment, 3), "\n")
    cat("Adjusted estimate:", format(round(adjusted_estimate), big.mark = ","), "\n")
  }

  return(list(
    N_hat = adjusted_estimate,
    y_FH_proportion = y_FH_proportion,
    d_FF_average = d_FF_average,
    basic_estimate = basic_estimate,
    adjustment_impact = total_adjustment,
    delta_factor = delta_adjustment,
    tau_factor = tau_adjustment,
    rho_factor = rho_adjustment,
    n_valid = sum(valid_cases),
    method = "MBSU",
    error = NA
  ))
}

# ==============================================================================
# GENERALIZED NSUM (GNSUM) ESTIMATOR
# ==============================================================================
#
# Standard GNSUM estimator as described in Feehan & Salganik (2016)
# Formula: N_H = (Σ(y_i,H / π_i) / Σ(d_i / π_i)) * N_F
# ==============================================================================

estimate_gnsum <- function(data,
                          outcome_variable,
                          degree_variable,
                          frame_size,
                          weight_column = "inclusion_prob",
                          validate_inputs = TRUE,
                          verbose = FALSE) {

  if (verbose) {
    cat("=== GNSUM Estimation ===\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
  }

  # Input validation
  if (validate_inputs) {
    required_vars <- c(outcome_variable, degree_variable, weight_column)
    missing_vars <- required_vars[!required_vars %in% names(data)]

    if (length(missing_vars) > 0) {
      stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
    }

    if (frame_size <= 0) {
      stop("Frame size must be positive")
    }
  }

  # Extract required variables
  y_iH <- data[[outcome_variable]]  # Out-reports to hidden population
  d_i <- data[[degree_variable]]    # Network degrees
  pi_i <- data[[weight_column]]     # Inclusion probabilities

  # Filter valid cases
  valid_cases <- !is.na(y_iH) & !is.na(d_i) & !is.na(pi_i) & pi_i > 0

  if (sum(valid_cases) == 0) {
    warning("No valid cases for GNSUM estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = NA,
      d_FF_average = NA,
      n_valid = 0,
      method = "GNSUM",
      error = "No valid cases"
    ))
  }

  # Work with valid data
  y_valid <- y_iH[valid_cases]
  d_valid <- d_i[valid_cases]
  pi_valid <- pi_i[valid_cases]

  # Calculate weighted sums
  numerator_y <- sum(y_valid / pi_valid, na.rm = TRUE)
  numerator_d <- sum(d_valid / pi_valid, na.rm = TRUE)

  if (numerator_d <= 0) {
    warning("Invalid degree sum (≤0) in GNSUM estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = NA,
      d_FF_average = NA,
      n_valid = sum(valid_cases),
      method = "GNSUM",
      error = "Invalid degree sum"
    ))
  }

  # GNSUM formula: N_H = (Σ(y_i,H / π_i) / Σ(d_i / π_i)) * N_F
  N_hat <- (numerator_y / numerator_d) * frame_size

  # For consistency with MBSU output, also calculate proportions
  denominator <- sum(1 / pi_valid, na.rm = TRUE)
  y_FH_proportion <- numerator_y / denominator
  d_FF_average <- numerator_d / denominator

  if (verbose) {
    cat("Weighted y sum:", round(numerator_y, 2), "\n")
    cat("Weighted d sum:", round(numerator_d, 2), "\n")
    cat("Estimate:", format(round(N_hat), big.mark = ","), "\n")
  }

  return(list(
    N_hat = N_hat,
    y_FH_proportion = y_FH_proportion,
    d_FF_average = d_FF_average,
    numerator_y = numerator_y,
    numerator_d = numerator_d,
    n_valid = sum(valid_cases),
    method = "GNSUM",
    error = NA
  ))
}

# ==============================================================================
# GNSUM WITH SYMMETRIC VISIBILITY
# ==============================================================================
#
# Enhanced GNSUM that incorporates "in-reports" from hidden population members
# under the symmetric visibility assumption.
#
# Formula: N_H = (Σ(y_i,H + I_H(i) * y_i^in) / π_i) / Σ(d_i / π_i)) * N_F
#
# Where:
# - I_H(i) = 1 if respondent i is in hidden population, 0 otherwise
# - y_i^in = number of other hidden members who report knowing i
# ==============================================================================

estimate_gnsum_symmetric <- function(data,
                                    outcome_variable,
                                    degree_variable,
                                    frame_size,
                                    hidden_member_indicator,
                                    in_reports_variable = NULL,
                                    weight_column = "inclusion_prob",
                                    validate_inputs = TRUE,
                                    verbose = FALSE) {

  if (verbose) {
    cat("=== GNSUM with Symmetric Visibility ===\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
  }

  # Input validation
  if (validate_inputs) {
    required_vars <- c(outcome_variable, degree_variable, weight_column, hidden_member_indicator)
    missing_vars <- required_vars[!required_vars %in% names(data)]

    if (length(missing_vars) > 0) {
      stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
    }

    if (frame_size <= 0) {
      stop("Frame size must be positive")
    }
  }

  # Extract required variables
  y_iH <- data[[outcome_variable]]      # Out-reports to hidden population
  d_i <- data[[degree_variable]]        # Network degrees
  pi_i <- data[[weight_column]]         # Inclusion probabilities
  I_H <- data[[hidden_member_indicator]] # Hidden member indicator (1 if hidden, 0 if not)

  # Handle in-reports
  if (!is.null(in_reports_variable) && in_reports_variable %in% names(data)) {
    y_in <- data[[in_reports_variable]]
  } else {
    # If no in-reports variable provided, estimate based on network structure
    # This is a simplified approach - in practice, this would require
    # more sophisticated network analysis
    if (verbose) {
      cat("No in-reports variable provided, using simplified estimation\n")
    }
    y_in <- rep(0, nrow(data))  # Conservative: assume no in-reports
  }

  # Filter valid cases
  valid_cases <- !is.na(y_iH) & !is.na(d_i) & !is.na(pi_i) & !is.na(I_H) &
                 !is.na(y_in) & pi_i > 0

  if (sum(valid_cases) == 0) {
    warning("No valid cases for symmetric GNSUM estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = NA,
      d_FF_average = NA,
      n_valid = 0,
      n_hidden_members = 0,
      method = "GNSUM_Symmetric",
      error = "No valid cases"
    ))
  }

  # Work with valid data
  y_valid <- y_iH[valid_cases]
  d_valid <- d_i[valid_cases]
  pi_valid <- pi_i[valid_cases]
  I_H_valid <- I_H[valid_cases]
  y_in_valid <- y_in[valid_cases]

  # Calculate enhanced y values incorporating in-reports
  # y_enhanced = y_i,H + I_H(i) * y_i^in
  y_enhanced <- y_valid + I_H_valid * y_in_valid

  # Calculate weighted sums
  numerator_y <- sum(y_enhanced / pi_valid, na.rm = TRUE)
  numerator_d <- sum(d_valid / pi_valid, na.rm = TRUE)

  if (numerator_d <= 0) {
    warning("Invalid degree sum (≤0) in symmetric GNSUM estimation")
    return(list(
      N_hat = NA,
      y_FH_proportion = NA,
      d_FF_average = NA,
      n_valid = sum(valid_cases),
      n_hidden_members = sum(I_H_valid),
      method = "GNSUM_Symmetric",
      error = "Invalid degree sum"
    ))
  }

  # Symmetric GNSUM formula
  N_hat <- (numerator_y / numerator_d) * frame_size

  # Calculate proportions for consistency
  denominator <- sum(1 / pi_valid, na.rm = TRUE)
  y_FH_proportion <- numerator_y / denominator
  d_FF_average <- numerator_d / denominator

  # Calculate contribution of in-reports
  in_reports_contribution <- sum(I_H_valid * y_in_valid / pi_valid, na.rm = TRUE)
  out_reports_contribution <- sum(y_valid / pi_valid, na.rm = TRUE)

  if (verbose) {
    cat("Out-reports contribution:", round(out_reports_contribution, 2), "\n")
    cat("In-reports contribution:", round(in_reports_contribution, 2), "\n")
    cat("Total weighted y sum:", round(numerator_y, 2), "\n")
    cat("Weighted d sum:", round(numerator_d, 2), "\n")
    cat("Hidden members in sample:", sum(I_H_valid), "\n")
    cat("Estimate:", format(round(N_hat), big.mark = ","), "\n")
  }

  return(list(
    N_hat = N_hat,
    y_FH_proportion = y_FH_proportion,
    d_FF_average = d_FF_average,
    numerator_y = numerator_y,
    numerator_d = numerator_d,
    out_reports_contribution = out_reports_contribution,
    in_reports_contribution = in_reports_contribution,
    in_reports_fraction = in_reports_contribution / numerator_y,
    n_valid = sum(valid_cases),
    n_hidden_members = sum(I_H_valid),
    method = "GNSUM_Symmetric",
    error = NA
  ))
}

# ==============================================================================
# MODEL-BASED NSUM FRAMEWORK
# ==============================================================================
#
# Placeholder for Bayesian NSUM implementations
# (e.g., using rstanarm, brms, or custom MCMC)
# ==============================================================================

estimate_model_based_nsum <- function(data,
                                     outcome_variable,
                                     degree_variable,
                                     frame_size,
                                     weight_column = "inclusion_prob",
                                     model_type = "simple_bayesian",
                                     prior_params = list(),
                                     mcmc_params = list(
                                       n_iter = 2000,
                                       n_warmup = 1000,
                                       n_chains = 4
                                     ),
                                     verbose = FALSE) {

  if (verbose) {
    cat("=== Model-Based NSUM Estimation ===\n")
    cat("Model type:", model_type, "\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
  }

  # For now, return placeholder result
  # In full implementation, this would use:
  # - rstanarm for Stan-based Bayesian NSUM
  # - brms for more flexible Bayesian modeling
  # - Custom MCMC implementations

  warning("Model-based NSUM not yet implemented - returning placeholder")

  return(list(
    N_hat = NA,
    N_hat_median = NA,
    N_hat_mean = NA,
    credible_interval = c(NA, NA),
    posterior_samples = NULL,
    convergence_diagnostics = NULL,
    n_valid = sum(!is.na(data[[outcome_variable]])),
    method = paste0("ModelBased_", model_type),
    error = "Not yet implemented"
  ))
}

# ==============================================================================
# UNIFIED NSUM ESTIMATION FUNCTION
# ==============================================================================
#
# Main function that dispatches to appropriate estimator based on method
#
# Parameters:
# - data: Bootstrap sample with RDS weights from Step 2
# - nsum_method: "mbsu", "gnsum", "gnsum_symmetric", "model_bayes"
# - outcome_variable: Column name for out-reports (e.g., "document_withholding_nsum")
# - degree_variable: Column name for network degrees (default: "known_network_size")
# - frame_size: Total frame population size (e.g., 980000 for UK domestic workers)
# - weight_column: RDS weight column from Step 2 ("weight_vh", "weight_rds_i",
#                  "weight_rds_ii", "weight_rds_ss", or "inclusion_prob")
# - adjustment_factors: For MBSU method only - list with:
#   * delta: Transmission bias (0 < δ ≤ 1, default 1.0 = no bias)
#   * tau: Barrier effect (0 < τ ≤ 1, default 1.0 = no barriers)
#   * rho: Popularity bias (ρ > 0, default 1.0 = no bias)
# ==============================================================================

estimate_nsum <- function(data,
                         nsum_method = "mbsu",
                         outcome_variable,
                         degree_variable = "known_network_size",
                         frame_size,
                         weight_column = "inclusion_prob",
                         hidden_member_indicator = NULL,
                         in_reports_variable = NULL,
                         adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0),
                         model_params = list(),
                         validate_inputs = TRUE,
                         verbose = FALSE) {

  if (verbose) {
    cat("=== NSUM Estimation ===\n")
    cat("Method:", nsum_method, "\n")
    cat("Outcome variable:", outcome_variable, "\n")
    cat("Sample size:", nrow(data), "\n")
  }

  # Dispatch to appropriate estimator
  result <- switch(tolower(nsum_method),
    "mbsu" = estimate_mbsu(
      data = data,
      outcome_variable = outcome_variable,
      degree_variable = degree_variable,
      frame_size = frame_size,
      weight_column = weight_column,
      adjustment_factors = adjustment_factors,
      validate_inputs = validate_inputs,
      verbose = verbose
    ),

    "gnsum" = estimate_gnsum(
      data = data,
      outcome_variable = outcome_variable,
      degree_variable = degree_variable,
      frame_size = frame_size,
      weight_column = weight_column,
      validate_inputs = validate_inputs,
      verbose = verbose
    ),

    "gnsum_symmetric" = estimate_gnsum_symmetric(
      data = data,
      outcome_variable = outcome_variable,
      degree_variable = degree_variable,
      frame_size = frame_size,
      hidden_member_indicator = hidden_member_indicator,
      in_reports_variable = in_reports_variable,
      weight_column = weight_column,
      validate_inputs = validate_inputs,
      verbose = verbose
    ),

    "model_bayes" = estimate_model_based_nsum(
      data = data,
      outcome_variable = outcome_variable,
      degree_variable = degree_variable,
      frame_size = frame_size,
      weight_column = weight_column,
      model_type = model_params$model_type %||% "simple_bayesian",
      prior_params = model_params$prior_params %||% list(),
      mcmc_params = model_params$mcmc_params %||% list(),
      verbose = verbose
    ),

    stop("Unknown NSUM method: ", nsum_method, ". Supported methods: mbsu, gnsum, gnsum_symmetric, model_bayes")
  )

  # Add common metadata
  result$frame_size <- frame_size
  result$outcome_variable <- outcome_variable
  result$degree_variable <- degree_variable
  result$weight_column <- weight_column
  result$sample_size <- nrow(data)

  return(result)
}

# ==============================================================================
# BATCH PROCESSING FOR BOOTSTRAP SAMPLES
# ==============================================================================
#
# Process multiple bootstrap samples from Step 2
# ==============================================================================

estimate_nsum_batch <- function(boot_samples,
                               nsum_method = "mbsu",
                               outcome_variable,
                               degree_variable = "known_network_size",
                               frame_size,
                               weight_column = "inclusion_prob",
                               adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0),
                               parallel = TRUE,
                               n_cores = NULL,
                               verbose = FALSE) {

  if (!is.list(boot_samples)) {
    stop("boot_samples must be a list of data frames")
  }

  n_samples <- length(boot_samples)

  if (verbose) {
    cat("=== Batch NSUM Estimation ===\n")
    cat("Method:", nsum_method, "\n")
    cat("Bootstrap samples:", n_samples, "\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
    cat("Parallel processing:", parallel, "\n")
  }

  # Set up parallel processing if requested
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- min(4, parallel::detectCores() - 1)
    }

    if (verbose) cat("Using", n_cores, "cores\n")

    # Create cluster
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export required functions and variables
    parallel::clusterExport(cl, c(
      "estimate_nsum", "estimate_mbsu", "estimate_gnsum",
      "estimate_gnsum_symmetric", "estimate_model_based_nsum",
      "nsum_method", "outcome_variable", "degree_variable",
      "frame_size", "weight_column", "adjustment_factors"
    ), envir = environment())

    # Load required packages on workers
    parallel::clusterEvalQ(cl, {
      library(tidyverse)
    })

    # Process samples in parallel
    results <- parallel::parLapply(cl, 1:n_samples, function(i) {
      tryCatch({
        estimate_nsum(
          data = boot_samples[[i]],
          nsum_method = nsum_method,
          outcome_variable = outcome_variable,
          degree_variable = degree_variable,
          frame_size = frame_size,
          weight_column = weight_column,
          adjustment_factors = adjustment_factors,
          validate_inputs = FALSE,  # Skip validation for speed
          verbose = FALSE
        )
      }, error = function(e) {
        list(
          N_hat = NA,
          method = nsum_method,
          error = as.character(e),
          bootstrap_sample = i
        )
      })
    })
  } else {
    # Sequential processing
    results <- vector("list", n_samples)

    for (i in 1:n_samples) {
      if (verbose && i %% 100 == 0) {
        cat("Processing sample", i, "of", n_samples, "\n")
      }

      results[[i]] <- tryCatch({
        estimate_nsum(
          data = boot_samples[[i]],
          nsum_method = nsum_method,
          outcome_variable = outcome_variable,
          degree_variable = degree_variable,
          frame_size = frame_size,
          weight_column = weight_column,
          adjustment_factors = adjustment_factors,
          validate_inputs = FALSE,
          verbose = FALSE
        )
      }, error = function(e) {
        list(
          N_hat = NA,
          method = nsum_method,
          error = as.character(e),
          bootstrap_sample = i
        )
      })
    }
  }

  # Extract key results
  N_hat_estimates <- sapply(results, function(x) x$N_hat %||% NA)
  valid_estimates <- N_hat_estimates[!is.na(N_hat_estimates)]

  if (verbose) {
    cat("Completed batch processing:\n")
    cat("- Valid estimates:", length(valid_estimates), "/", n_samples, "\n")
    cat("- Mean estimate:", format(round(mean(valid_estimates, na.rm = TRUE)), big.mark = ","), "\n")
    cat("- Range:", format(round(range(valid_estimates, na.rm = TRUE)), big.mark = ","), "\n")
  }

  # Calculate summary statistics
  if (length(valid_estimates) > 0) {
    ci_95 <- quantile(valid_estimates, c(0.025, 0.975), na.rm = TRUE)
    ci_90 <- quantile(valid_estimates, c(0.05, 0.95), na.rm = TRUE)
  } else {
    ci_95 <- c(NA, NA)
    ci_90 <- c(NA, NA)
  }

  return(list(
    estimates = N_hat_estimates,
    valid_estimates = valid_estimates,
    n_bootstrap = n_samples,
    n_valid = length(valid_estimates),
    mean_estimate = mean(valid_estimates, na.rm = TRUE),
    median_estimate = median(valid_estimates, na.rm = TRUE),
    ci_95 = ci_95,
    ci_90 = ci_90,
    method = nsum_method,
    outcome_variable = outcome_variable,
    frame_size = frame_size,
    detailed_results = results
  ))
}

# ==============================================================================
# TESTING AND VALIDATION FUNCTIONS
# ==============================================================================

test_nsum_estimation <- function(verbose = TRUE) {

  if (verbose) cat("=== Testing NSUM Estimation Functions ===\n")

  # Create test data
  set.seed(12345)
  n_test <- 50

  test_data <- data.frame(
    id = 1:n_test,
    document_withholding_nsum = rbinom(n_test, 3, 0.1),  # Out-reports
    known_network_size = rpois(n_test, 25) + 5,          # Degrees
    inclusion_prob = runif(n_test, 0.001, 0.01),         # Weights
    hidden_member = rbinom(n_test, 1, 0.15),             # Hidden status
    in_reports = rbinom(n_test, 2, 0.05)                 # In-reports
  )

  if (verbose) {
    cat("Created test dataset:\n")
    cat("- Sample size:", nrow(test_data), "\n")
    cat("- Mean out-reports:", round(mean(test_data$document_withholding_nsum), 2), "\n")
    cat("- Mean degree:", round(mean(test_data$known_network_size), 2), "\n")
    cat("- Hidden members:", sum(test_data$hidden_member), "\n")
  }

  # Test each method
  frame_size <- 980000
  outcome_var <- "document_withholding_nsum"

  # Test MBSU
  if (verbose) cat("\nTesting MBSU...\n")
  mbsu_result <- estimate_nsum(
    data = test_data,
    nsum_method = "mbsu",
    outcome_variable = outcome_var,
    frame_size = frame_size,
    adjustment_factors = list(delta = 0.8, tau = 0.9, rho = 1.1),
    verbose = verbose
  )

  # Test GNSUM
  if (verbose) cat("\nTesting GNSUM...\n")
  gnsum_result <- estimate_nsum(
    data = test_data,
    nsum_method = "gnsum",
    outcome_variable = outcome_var,
    frame_size = frame_size,
    verbose = verbose
  )

  # Test Symmetric GNSUM
  if (verbose) cat("\nTesting Symmetric GNSUM...\n")
  gnsum_sym_result <- estimate_nsum(
    data = test_data,
    nsum_method = "gnsum_symmetric",
    outcome_variable = outcome_var,
    frame_size = frame_size,
    hidden_member_indicator = "hidden_member",
    in_reports_variable = "in_reports",
    verbose = verbose
  )

  # Test batch processing
  if (verbose) cat("\nTesting batch processing...\n")
  boot_samples <- list(test_data, test_data, test_data)  # Simple test

  batch_result <- estimate_nsum_batch(
    boot_samples = boot_samples,
    nsum_method = "mbsu",
    outcome_variable = outcome_var,
    frame_size = frame_size,
    parallel = FALSE,
    verbose = verbose
  )

  if (verbose) {
    cat("\n=== Test Results Summary ===\n")
    cat("MBSU estimate:", format(round(mbsu_result$N_hat), big.mark = ","), "\n")
    cat("GNSUM estimate:", format(round(gnsum_result$N_hat), big.mark = ","), "\n")
    cat("Symmetric GNSUM estimate:", format(round(gnsum_sym_result$N_hat), big.mark = ","), "\n")
    cat("Batch processing: ", batch_result$n_valid, " valid out of ", batch_result$n_bootstrap, "\n")
  }

  return(list(
    mbsu = mbsu_result,
    gnsum = gnsum_result,
    gnsum_symmetric = gnsum_sym_result,
    batch = batch_result
  ))
}

# ==============================================================================
# EXECUTION CONTROL
# ==============================================================================

if (!SKIP_EXECUTION) {
  cat("Step 3 NSUM estimation functions loaded successfully.\n")
  cat("Key functions:\n")
  cat("- estimate_nsum(): Main NSUM estimation function\n")
  cat("- estimate_nsum_batch(): Batch processing for bootstrap samples\n")
  cat("- test_nsum_estimation(): Test all implementations\n\n")

  cat("Usage examples:\n")
  cat("# Basic MBSU with default adjustment factors (no bias)\n")
  cat("result <- estimate_nsum(data, 'mbsu', 'outcome_var', frame_size = 980000)\n\n")
  cat("# MBSU with custom adjustment factors for production run\n")
  cat("mbsu_adj <- list(delta = 0.8, tau = 0.7, rho = 1.2)  # Expert elicitation values\n")
  cat("result_adj <- estimate_nsum(data, 'mbsu', 'outcome_var', frame_size = 980000,\n")
  cat("                           weight_column = 'weight_vh', adjustment_factors = mbsu_adj)\n\n")
  cat("# Different RDS weight methods from Step 2\n")
  cat("result_vh <- estimate_nsum(data, 'gnsum', 'outcome_var', frame_size = 980000, weight_column = 'weight_vh')\n")
  cat("result_rds_ss <- estimate_nsum(data, 'gnsum', 'outcome_var', frame_size = 980000, weight_column = 'weight_rds_ss')\n\n")
  cat("# Batch processing with adjustment factors\n")
  cat("batch_results <- estimate_nsum_batch(boot_samples, 'mbsu', 'outcome_var', 980000,\n")
  cat("                                    weight_column = 'weight_vh', adjustment_factors = mbsu_adj)\n\n")
}

# Example: Using different weight methods and adjustment factors
#
# Production MBSU with expert-elicited adjustment factors:
# - delta = 0.75: Hidden workers may be less likely to disclose status (75% transmission)
# - tau = 0.8: Some social barriers between hidden and frame populations (80% mixing)
# - rho = 0.9: Hidden workers may be slightly less visible in networks (90% visibility)
mbsu_production <- list(delta = 0.75, tau = 0.8, rho = 0.9)

# result_mbsu <- estimate_nsum(weighted_bootstrap_samples[[1]], 'mbsu', 'excessive_hours_nsum',
#                             frame_size = 980000, weight_column = 'weight_vh',
#                             adjustment_factors = mbsu_production)
# result_gnsum <- estimate_nsum(weighted_bootstrap_samples[[1]], 'gnsum', 'excessive_hours_nsum',
#                              frame_size = 980000, weight_column = 'weight_rds_ss')


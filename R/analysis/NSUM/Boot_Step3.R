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
library(RDS)

# Set execution control
SKIP_EXECUTION <- FALSE
if (exists("skip_step3") && skip_step3) SKIP_EXECUTION <- TRUE

if (!SKIP_EXECUTION) {
  cat("=== Loading Step 3: NSUM Estimation Functions ===\n")
}

# ==============================================================================
# FIX MISSING RDS WEIGHTS IN BOOTSTRAP SAMPLES
# ==============================================================================

add_rds_weights_to_bootstrap_samples <- function(boot_samples, original_data = NULL) {
  cat("=== Adding Missing RDS Weights to Bootstrap Samples ===\n")

  # Handle both single data frame and list of bootstrap samples
  is_list_samples <- is.list(boot_samples) && !is.data.frame(boot_samples)

  if (is_list_samples) {
    cat("Processing list of", length(boot_samples), "bootstrap samples\n")
    first_sample <- boot_samples[[1]]
  } else {
    cat("Processing single bootstrap sample\n")
    first_sample <- boot_samples
  }

  # Check if weights already exist in first sample
  weight_cols <- c("weight_vh", "weight_rds_i", "weight_rds_ii", "weight_rds_ss")
  missing_weights <- setdiff(weight_cols, colnames(first_sample))

  if (length(missing_weights) == 0) {
    cat("All RDS weights already present in bootstrap samples.\n")
    return(boot_samples)
  }

  cat("Missing weight columns:", paste(missing_weights, collapse = ", "), "\n")

  # For RDS bootstrap samples, calculate weights directly from each sample
  if (class(first_sample)[1] == "rds.data.frame") {
    cat("Bootstrap samples are RDS objects - calculating weights directly\n")

    if (is_list_samples) {
      # Process each bootstrap sample in the list
      cat("Adding RDS weights to", length(boot_samples), "bootstrap samples...\n")

      boot_samples_with_weights <- lapply(seq_along(boot_samples), function(i) {
        if (i %% 20 == 0 || i == length(boot_samples)) {
          cat("Processing sample", i, "of", length(boot_samples), "\n")
        }

        sample <- boot_samples[[i]]

        tryCatch({
          # Calculate RDS weights for this bootstrap sample
          weights_vh <- vh.weights(sample)
          weights_rds_i <- rds.I.weights(sample)
          weights_rds_ii <- rds.II.weights(sample)
          weights_rds_ss <- compute.weights(sample, weight.type = "Gille's SS")

          # Add weights as new columns
          sample$weight_vh <- as.numeric(weights_vh)
          sample$weight_rds_i <- as.numeric(weights_rds_i)
          sample$weight_rds_ii <- as.numeric(weights_rds_ii)
          sample$weight_rds_ss <- as.numeric(weights_rds_ss)

          return(sample)
        }, error = function(e) {
          cat("Error processing sample", i, ":", e$message, "\n")
          # Fallback to simple weights based on network size
          if ("network.size" %in% colnames(sample)) {
            sample$weight_vh <- 1/sample$network.size
            sample$weight_rds_i <- 1/sample$network.size
            sample$weight_rds_ii <- 1/sample$network.size
            sample$weight_rds_ss <- 1/sample$network.size
            cat("Using simple inclusion probability weights for sample", i, "\n")
          } else {
            # Last resort: equal weights
            sample$weight_vh <- 1
            sample$weight_rds_i <- 1
            sample$weight_rds_ii <- 1
            sample$weight_rds_ss <- 1
            cat("Using equal weights for sample", i, "\n")
          }
          return(sample)
        })
      })

      cat("Successfully processed all bootstrap samples\n")
      return(boot_samples_with_weights)

    } else {
      # Single RDS sample
      cat("Calculating RDS weights for single sample...\n")
      weights_vh <- rds.i.weights(boot_samples)
      weights_rds_i <- rds.i.weights(boot_samples)
      weights_rds_ii <- rds.iI.weights(boot_samples)
      weights_rds_ss <- RDS.SS.weights(boot_samples)

      boot_samples$weight_vh <- as.numeric(weights_vh)
      boot_samples$weight_rds_i <- as.numeric(weights_rds_i)
      boot_samples$weight_rds_ii <- as.numeric(weights_rds_ii)
      boot_samples$weight_rds_ss <- as.numeric(weights_rds_ss)

      return(boot_samples)
    }
  }

  # Fallback: try to load original data for weight calculation
  if (is.null(original_data)) {
    cat("Loading original RDS data to calculate weights...\n")
    possible_files <- c(
      here("data", "processed", "prepared_data.RData"),
      here("data", "processed", "rds_object.RData"),
      here("data", "processed", "clean_data.RData")
    )

    loaded_data <- NULL
    for (file in possible_files) {
      if (file.exists(file)) {
        cat("Loading:", file, "\n")
        load(file)
        if (exists("prepared_data")) {
          loaded_data <- prepared_data
          break
        } else if (exists("rds_object")) {
          loaded_data <- rds_object
          break
        } else if (exists("clean_data")) {
          loaded_data <- clean_data
          break
        }
      }
    }

    if (is.null(loaded_data)) {
      stop("Cannot find original data file to calculate RDS weights. Please provide original_data parameter.")
    }
    original_data <- loaded_data
  }

  # Use original data approach if bootstrap samples are not RDS objects
  if (class(original_data)[1] == "rds.data.frame") {
    cat("Calculating RDS weights from original RDS object...\n")
    weights_vh <- rds.i.weights(original_data)
    weights_rds_i <- rds.i.weights(original_data)
    weights_rds_ii <- rds.iI.weights(original_data)
    weights_rds_ss <- RDS.SS.weights(original_data)

    weight_df <- data.frame(
      id = as.numeric(row.names(original_data)),
      weight_vh = as.numeric(weights_vh),
      weight_rds_i = as.numeric(weights_rds_i),
      weight_rds_ii = as.numeric(weights_rds_ii),
      weight_rds_ss = as.numeric(weights_rds_ss)
    )
  } else {
    # Simple weights from data frame
    weight_df <- original_data %>%
      select(id, network.size) %>%
      mutate(
        weight_vh = 1/network.size,
        weight_rds_i = 1/network.size,
        weight_rds_ii = 1/network.size,
        weight_rds_ss = 1/network.size
      ) %>%
      select(id, starts_with("weight_"))
  }

  # Merge with bootstrap samples
  if (is_list_samples) {
    boot_samples_with_weights <- lapply(boot_samples, function(sample) {
      if (!"id" %in% colnames(sample) && "original_id" %in% colnames(sample)) {
        sample$id <- sample$original_id
      }
      sample %>% left_join(weight_df, by = "id")
    })
  } else {
    if (!"id" %in% colnames(boot_samples) && "original_id" %in% colnames(boot_samples)) {
      boot_samples$id <- boot_samples$original_id
    }
    boot_samples_with_weights <- boot_samples %>% left_join(weight_df, by = "id")
  }

  return(boot_samples_with_weights)
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
# STANDARD GNSUM - NOT IMPLEMENTABLE WITH RDS-NSUM DATA
# ==============================================================================
#
# Standard GNSUM requires direct visibility measures between frame and hidden
# populations, which we do not have in typical RDS-NSUM studies.
#
# Standard GNSUM formula: N_H = (Σ(y_i,H / π_i) / Σ(v_i,H / π_i)) * N_F
#
# The problem: We have out-reports (y_i,H) but lack visibility measures (v_i,H)
# from frame population members to hidden population members.
#
# Instead, use 'gnsum_symmetric' which estimates visibility from hidden members'
# degrees under the symmetric visibility assumption.
# ==============================================================================

# Standard GNSUM cannot be implemented - function removed
# Use estimate_gnsum_symmetric() instead

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
# - nsum_method: "mbsu", "gnsum_symmetric", "model_bayes"
#   Note: Standard "gnsum" not implementable (lacks visibility measures from frame to hidden pop)
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

    "gnsum" = {
      stop("Standard GNSUM cannot be implemented with RDS-NSUM data because we lack direct visibility measures from frame to hidden population. Use 'gnsum_symmetric' with degree-based visibility estimation instead.")
    },

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
                               hidden_member_indicator = NULL,
                               in_reports_variable = NULL,
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
      "frame_size", "weight_column", "adjustment_factors",
      "hidden_member_indicator", "in_reports_variable"
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
          hidden_member_indicator = hidden_member_indicator,
          in_reports_variable = in_reports_variable,
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
          hidden_member_indicator = hidden_member_indicator,
          in_reports_variable = in_reports_variable,
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
# MBSU ADJUSTMENT FACTOR SWEEP
# ==============================================================================
#
# Sweep across multiple adjustment factor combinations for sensitivity analysis
# ==============================================================================

estimate_mbsu_sweep <- function(data,
                               outcome_variable,
                               degree_variable = "known_network_size",
                               frame_size,
                               weight_column = "inclusion_prob",
                               delta_range = seq(0.5, 1.0, by = 0.1),      # Transmission bias
                               tau_range = seq(0.6, 1.0, by = 0.1),        # Barrier effect
                               rho_range = seq(0.8, 1.2, by = 0.1),        # Popularity bias
                               parallel = TRUE,
                               n_cores = NULL,
                               verbose = FALSE) {

  if (verbose) {
    cat("=== MBSU Adjustment Factor Sweep ===\n")
    cat("Delta range:", paste(range(delta_range), collapse = " to "), "\n")
    cat("Tau range:", paste(range(tau_range), collapse = " to "), "\n")
    cat("Rho range:", paste(range(rho_range), collapse = " to "), "\n")
  }

  # Create all combinations
  factor_grid <- expand.grid(
    delta = delta_range,
    tau = tau_range,
    rho = rho_range,
    stringsAsFactors = FALSE
  )

  n_combinations <- nrow(factor_grid)
  if (verbose) {
    cat("Total combinations:", n_combinations, "\n")
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
      "estimate_mbsu", "data", "outcome_variable", "degree_variable",
      "frame_size", "weight_column"
    ), envir = environment())

    # Load required packages on workers
    parallel::clusterEvalQ(cl, {
      library(tidyverse)
    })

    # Process combinations in parallel
    results <- parallel::parLapply(cl, 1:n_combinations, function(i) {
      adjustment_factors <- list(
        delta = factor_grid$delta[i],
        tau = factor_grid$tau[i],
        rho = factor_grid$rho[i]
      )

      result <- tryCatch({
        estimate_mbsu(
          data = data,
          outcome_variable = outcome_variable,
          degree_variable = degree_variable,
          frame_size = frame_size,
          weight_column = weight_column,
          adjustment_factors = adjustment_factors,
          validate_inputs = FALSE,
          verbose = FALSE
        )
      }, error = function(e) {
        list(N_hat = NA, error = as.character(e))
      })

      # Add factor combination info
      result$delta <- factor_grid$delta[i]
      result$tau <- factor_grid$tau[i]
      result$rho <- factor_grid$rho[i]
      result$combination_id <- i

      return(result)
    })
  } else {
    # Sequential processing
    results <- vector("list", n_combinations)

    for (i in 1:n_combinations) {
      if (verbose && i %% 50 == 0) {
        cat("Processing combination", i, "of", n_combinations, "\n")
      }

      adjustment_factors <- list(
        delta = factor_grid$delta[i],
        tau = factor_grid$tau[i],
        rho = factor_grid$rho[i]
      )

      results[[i]] <- tryCatch({
        result <- estimate_mbsu(
          data = data,
          outcome_variable = outcome_variable,
          degree_variable = degree_variable,
          frame_size = frame_size,
          weight_column = weight_column,
          adjustment_factors = adjustment_factors,
          validate_inputs = FALSE,
          verbose = FALSE
        )

        # Add factor combination info
        result$delta <- factor_grid$delta[i]
        result$tau <- factor_grid$tau[i]
        result$rho <- factor_grid$rho[i]
        result$combination_id <- i

        return(result)
      }, error = function(e) {
        list(
          N_hat = NA,
          delta = factor_grid$delta[i],
          tau = factor_grid$tau[i],
          rho = factor_grid$rho[i],
          combination_id = i,
          error = as.character(e)
        )
      })
    }
  }

  # Extract estimates and create summary
  estimates <- sapply(results, function(x) x$N_hat %||% NA)
  valid_estimates <- estimates[!is.na(estimates)]

  if (verbose) {
    cat("Sweep completed:\n")
    cat("- Valid estimates:", length(valid_estimates), "/", n_combinations, "\n")
    cat("- Estimate range:", format(round(range(valid_estimates, na.rm = TRUE)), big.mark = ","), "\n")
    cat("- Median estimate:", format(round(median(valid_estimates, na.rm = TRUE)), big.mark = ","), "\n")
  }

  # Convert to data frame for easier analysis
  results_df <- map_dfr(results, function(r) {
    tibble(
      delta = r$delta %||% NA,
      tau = r$tau %||% NA,
      rho = r$rho %||% NA,
      N_hat = r$N_hat %||% NA,
      basic_estimate = r$basic_estimate %||% NA,
      adjustment_impact = r$adjustment_impact %||% NA,
      y_FH_proportion = r$y_FH_proportion %||% NA,
      d_FF_average = r$d_FF_average %||% NA,
      combination_id = r$combination_id %||% NA,
      error = r$error %||% NA
    )
  })

  # Calculate summary statistics
  if (length(valid_estimates) > 0) {
    summary_stats <- list(
      min_estimate = min(valid_estimates, na.rm = TRUE),
      max_estimate = max(valid_estimates, na.rm = TRUE),
      median_estimate = median(valid_estimates, na.rm = TRUE),
      mean_estimate = mean(valid_estimates, na.rm = TRUE),
      q25 = quantile(valid_estimates, 0.25, na.rm = TRUE),
      q75 = quantile(valid_estimates, 0.75, na.rm = TRUE),
      sd_estimate = sd(valid_estimates, na.rm = TRUE),
      cv_estimate = sd(valid_estimates, na.rm = TRUE) / mean(valid_estimates, na.rm = TRUE)
    )
  } else {
    summary_stats <- list(
      min_estimate = NA, max_estimate = NA, median_estimate = NA,
      mean_estimate = NA, q25 = NA, q75 = NA, sd_estimate = NA, cv_estimate = NA
    )
  }

  return(list(
    estimates = estimates,
    valid_estimates = valid_estimates,
    results_df = results_df,
    detailed_results = results,
    summary_stats = summary_stats,
    factor_grid = factor_grid,
    n_combinations = n_combinations,
    n_valid = length(valid_estimates),
    method = "MBSU_sweep"
  ))
}

# ==============================================================================
# COMPREHENSIVE 3-STEP BOOTSTRAP FOR NSUM ESTIMATION
# ==============================================================================
#
# Complete workflow combining Steps 1-3 with comprehensive parameter sweeps:
# - Multiple RDS weight methods from Step 2
# - MBSU with adjustment factor sweeps
# - GNSUM_Symmetric across different weight schemes
# - Bootstrap confidence intervals for all combinations
# ==============================================================================

run_comprehensive_nsum_bootstrap <- function(boot_samples,              # From Step 1
                                           outcome_variables,          # Vector of NSUM variables
                                           hidden_indicators,          # Corresponding RDS variables
                                           degree_variable = "known_network_size",
                                           frame_size = 980000,
                                           rds_weight_methods = c("weight_vh", "weight_rds_i", "weight_rds_ii", "weight_rds_ss"),
                                           mbsu_config = list(
                                             delta_range = seq(0.5, 1.0, by = 0.1),
                                             tau_range = seq(0.6, 1.0, by = 0.1),
                                             rho_range = seq(0.8, 1.2, by = 0.1)
                                           ),
                                           confidence_level = 0.95,
                                           parallel = TRUE,
                                           n_cores = NULL,
                                           verbose = TRUE) {

  if (verbose) {
    cat("=== Comprehensive 3-Step NSUM Bootstrap ===\n")
    cat("Bootstrap samples:", length(boot_samples), "\n")
    cat("Outcome variables:", length(outcome_variables), "\n")
    cat("RDS weight methods:", length(rds_weight_methods), "\n")
    cat("MBSU factor combinations:",
        length(mbsu_config$delta_range) * length(mbsu_config$tau_range) * length(mbsu_config$rho_range), "\n")
    cat("Frame size:", format(frame_size, big.mark = ","), "\n")
  }

  # Validate inputs
  if (length(outcome_variables) != length(hidden_indicators)) {
    stop("outcome_variables and hidden_indicators must be same length")
  }

  if (!is.list(boot_samples)) {
    stop("boot_samples must be a list of data frames from Step 1")
  }

  # Initialize results storage
  all_results <- list()
  result_counter <- 1

  # Calculate total number of combinations for progress tracking
  n_mbsu_factors <- length(mbsu_config$delta_range) * length(mbsu_config$tau_range) * length(mbsu_config$rho_range)
  total_combinations <- length(outcome_variables) * length(rds_weight_methods) * (n_mbsu_factors + 1) # +1 for GNSUM_Symmetric

  if (verbose) {
    cat("Total estimation combinations:", format(total_combinations, big.mark = ","), "\n")
    cat("Total bootstrap replications:", format(length(boot_samples) * total_combinations, big.mark = ","), "\n\n")
  }

  # Set up parallel processing if requested
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- min(6, parallel::detectCores() - 1)
    }
    if (verbose) cat("Using", n_cores, "cores for parallel processing\n")
  }

  # Main analysis loop
  for (i in seq_along(outcome_variables)) {
    outcome_var <- outcome_variables[i]
    hidden_var <- hidden_indicators[i]

    if (verbose) {
      cat("=== Processing outcome:", outcome_var, "===\n")
    }

    for (weight_method in rds_weight_methods) {
      if (verbose) {
        cat("Weight method:", weight_method, "\n")
      }

      # === MBSU WITH ADJUSTMENT FACTOR SWEEP ===
      if (verbose) cat("  Running MBSU sweep...\n")

      mbsu_estimates <- estimate_nsum_batch_sweep(
        boot_samples = boot_samples,
        nsum_method = "mbsu",
        outcome_variable = outcome_var,
        degree_variable = degree_variable,
        frame_size = frame_size,
        weight_column = weight_method,
        adjustment_factors = mbsu_config,
        parallel = parallel,
        n_cores = n_cores,
        verbose = FALSE
      )

      # Store MBSU results
      all_results[[result_counter]] <- list(
        outcome_variable = outcome_var,
        hidden_indicator = hidden_var,
        weight_method = weight_method,
        nsum_method = "MBSU",
        estimates = mbsu_estimates$estimates,
        confidence_intervals = mbsu_estimates$confidence_intervals,
        summary_stats = mbsu_estimates$summary_stats,
        parameter_grid = mbsu_estimates$parameter_grid,
        n_bootstrap = length(boot_samples)
      )
      result_counter <- result_counter + 1

      # === GNSUM SYMMETRIC ===
      if (verbose) cat("  Running GNSUM Symmetric...\n")

      gnsum_estimates <- estimate_nsum_batch(
        boot_samples = boot_samples,
        nsum_method = "gnsum_symmetric",
        outcome_variable = outcome_var,
        degree_variable = degree_variable,
        frame_size = frame_size,
        weight_column = weight_method,
        hidden_member_indicator = hidden_var,
        parallel = parallel,
        n_cores = n_cores,
        verbose = FALSE
      )

      # Calculate confidence intervals
      alpha <- 1 - confidence_level
      valid_estimates <- gnsum_estimates$valid_estimates

      if (length(valid_estimates) > 10) {
        ci_lower <- quantile(valid_estimates, alpha/2, na.rm = TRUE)
        ci_upper <- quantile(valid_estimates, 1 - alpha/2, na.rm = TRUE)
      } else {
        ci_lower <- ci_upper <- NA
      }

      # Store GNSUM_Symmetric results
      all_results[[result_counter]] <- list(
        outcome_variable = outcome_var,
        hidden_indicator = hidden_var,
        weight_method = weight_method,
        nsum_method = "GNSUM_Symmetric",
        estimates = gnsum_estimates$estimates,
        confidence_intervals = list(
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          confidence_level = confidence_level
        ),
        summary_stats = list(
          mean_estimate = gnsum_estimates$mean_estimate,
          median_estimate = gnsum_estimates$median_estimate,
          sd_estimate = sd(valid_estimates, na.rm = TRUE),
          cv_estimate = sd(valid_estimates, na.rm = TRUE) / mean(valid_estimates, na.rm = TRUE)
        ),
        n_bootstrap = length(boot_samples),
        n_valid = gnsum_estimates$n_valid
      )
      result_counter <- result_counter + 1

      if (verbose) {
        progress_pct <- round(100 * (result_counter - 1) / length(outcome_variables) / length(rds_weight_methods) / 2, 1)
        cat("  Progress:", progress_pct, "%\n")
      }
    }
  }

  if (verbose) {
    cat("\n=== Bootstrap Analysis Complete ===\n")
    cat("Total result combinations:", length(all_results), "\n")
  }

  # Create comprehensive summary
  summary_results <- create_nsum_bootstrap_summary(all_results, verbose = verbose)

  return(list(
    detailed_results = all_results,
    summary_results = summary_results,
    configuration = list(
      outcome_variables = outcome_variables,
      hidden_indicators = hidden_indicators,
      rds_weight_methods = rds_weight_methods,
      mbsu_config = mbsu_config,
      frame_size = frame_size,
      confidence_level = confidence_level,
      n_bootstrap = length(boot_samples)
    )
  ))
}

# ==============================================================================
# BATCH PROCESSING WITH ADJUSTMENT FACTOR SWEEP
# ==============================================================================

estimate_nsum_batch_sweep <- function(boot_samples,
                                     nsum_method,
                                     outcome_variable,
                                     degree_variable = "known_network_size",
                                     frame_size,
                                     weight_column,
                                     adjustment_factors = list(
                                       delta_range = seq(0.5, 1.0, by = 0.2),
                                       tau_range = seq(0.6, 1.0, by = 0.2),
                                       rho_range = seq(0.8, 1.2, by = 0.2)
                                     ),
                                     parallel = TRUE,
                                     n_cores = NULL,
                                     verbose = FALSE) {

  if (nsum_method != "mbsu") {
    stop("Adjustment factor sweep only available for MBSU method")
  }

  # Create parameter grid
  param_grid <- expand.grid(
    delta = adjustment_factors$delta_range,
    tau = adjustment_factors$tau_range,
    rho = adjustment_factors$rho_range,
    stringsAsFactors = FALSE
  )

  n_combinations <- nrow(param_grid)
  n_bootstrap <- length(boot_samples)

  if (verbose) {
    cat("MBSU batch sweep: ", n_combinations, " parameter combinations × ",
        n_bootstrap, " bootstrap samples = ",
        format(n_combinations * n_bootstrap, big.mark = ","), " total estimations\n")
  }

  # Storage for all estimates
  all_estimates <- array(NA, dim = c(n_bootstrap, n_combinations))
  colnames(all_estimates) <- paste0("delta", param_grid$delta, "_tau", param_grid$tau, "_rho", param_grid$rho)

  # Process each parameter combination
  for (i in 1:n_combinations) {
    if (verbose && i %% 10 == 0) {
      cat("Processing parameter combination", i, "of", n_combinations, "\n")
    }

    adj_factors <- list(
      delta = param_grid$delta[i],
      tau = param_grid$tau[i],
      rho = param_grid$rho[i]
    )

    # Get estimates for this parameter combination
    batch_result <- estimate_nsum_batch(
      boot_samples = boot_samples,
      nsum_method = nsum_method,
      outcome_variable = outcome_variable,
      degree_variable = degree_variable,
      frame_size = frame_size,
      weight_column = weight_column,
      adjustment_factors = adj_factors,
      parallel = parallel,
      n_cores = n_cores,
      verbose = FALSE
    )

    all_estimates[, i] <- batch_result$estimates
  }

  # Calculate confidence intervals for each parameter combination
  confidence_intervals <- apply(all_estimates, 2, function(x) {
    valid_x <- x[!is.na(x)]
    if (length(valid_x) > 10) {
      list(
        ci_lower = quantile(valid_x, 0.025, na.rm = TRUE),
        ci_upper = quantile(valid_x, 0.975, na.rm = TRUE),
        mean_estimate = mean(valid_x, na.rm = TRUE),
        median_estimate = median(valid_x, na.rm = TRUE),
        n_valid = length(valid_x)
      )
    } else {
      list(ci_lower = NA, ci_upper = NA, mean_estimate = NA, median_estimate = NA, n_valid = 0)
    }
  })

  # Calculate summary statistics across all parameter combinations
  all_valid_estimates <- as.vector(all_estimates[!is.na(all_estimates)])

  summary_stats <- if (length(all_valid_estimates) > 0) {
    list(
      overall_mean = mean(all_valid_estimates),
      overall_median = median(all_valid_estimates),
      overall_min = min(all_valid_estimates),
      overall_max = max(all_valid_estimates),
      overall_sd = sd(all_valid_estimates),
      n_total_valid = length(all_valid_estimates),
      n_total_estimates = length(all_estimates)
    )
  } else {
    list(overall_mean = NA, overall_median = NA, overall_min = NA,
         overall_max = NA, overall_sd = NA, n_total_valid = 0, n_total_estimates = length(all_estimates))
  }

  return(list(
    estimates = all_estimates,
    confidence_intervals = confidence_intervals,
    parameter_grid = param_grid,
    summary_stats = summary_stats,
    method = paste0(nsum_method, "_sweep"),
    outcome_variable = outcome_variable,
    weight_column = weight_column
  ))
}

# ==============================================================================
# RESULTS SUMMARY AND PRESENTATION
# ==============================================================================

create_nsum_bootstrap_summary <- function(results_list, verbose = TRUE) {

  if (verbose) cat("Creating comprehensive results summary...\n")

  # Convert to data frame for analysis
  summary_df <- map_dfr(results_list, function(result) {

    if (result$nsum_method == "MBSU") {
      # MBSU has multiple parameter combinations
      map_dfr(1:ncol(result$estimates), function(i) {
        param_info <- result$parameter_grid[i, ]
        ci_info <- result$confidence_intervals[[i]]

        tibble(
          outcome_variable = result$outcome_variable,
          hidden_indicator = result$hidden_indicator,
          weight_method = result$weight_method,
          nsum_method = result$nsum_method,
          delta = param_info$delta,
          tau = param_info$tau,
          rho = param_info$rho,
          mean_estimate = ci_info$mean_estimate %||% NA,
          median_estimate = ci_info$median_estimate %||% NA,
          ci_lower = ci_info$ci_lower %||% NA,
          ci_upper = ci_info$ci_upper %||% NA,
          n_valid = ci_info$n_valid %||% 0,
          n_bootstrap = result$n_bootstrap
        )
      })
    } else {
      # GNSUM_Symmetric has single result
      tibble(
        outcome_variable = result$outcome_variable,
        hidden_indicator = result$hidden_indicator,
        weight_method = result$weight_method,
        nsum_method = result$nsum_method,
        delta = NA,
        tau = NA,
        rho = NA,
        mean_estimate = result$summary_stats$mean_estimate %||% NA,
        median_estimate = result$summary_stats$median_estimate %||% NA,
        ci_lower = result$confidence_intervals$ci_lower %||% NA,
        ci_upper = result$confidence_intervals$ci_upper %||% NA,
        n_valid = result$n_valid %||% 0,
        n_bootstrap = result$n_bootstrap
      )
    }
  })

  # Calculate summary statistics by method and weight
  method_summaries <- summary_df %>%
    filter(!is.na(mean_estimate)) %>%
    group_by(outcome_variable, weight_method, nsum_method) %>%
    summarise(
      n_parameter_combinations = n(),
      estimate_range_min = min(mean_estimate, na.rm = TRUE),
      estimate_range_max = max(mean_estimate, na.rm = TRUE),
      estimate_median = median(mean_estimate, na.rm = TRUE),
      estimate_iqr = IQR(mean_estimate, na.rm = TRUE),
      avg_ci_width = mean((ci_upper - ci_lower), na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    cat("Summary completed:\n")
    cat("- Total result rows:", nrow(summary_df), "\n")
    cat("- Valid estimates:", sum(!is.na(summary_df$mean_estimate)), "\n")
    cat("- Methods analyzed:", length(unique(summary_df$nsum_method)), "\n")
    cat("- Weight methods:", length(unique(summary_df$weight_method)), "\n")
  }

  return(list(
    detailed_summary = summary_df,
    method_summaries = method_summaries,
    overall_stats = list(
      total_combinations = nrow(summary_df),
      valid_combinations = sum(!is.na(summary_df$mean_estimate)),
      outcome_variables = unique(summary_df$outcome_variable),
      weight_methods = unique(summary_df$weight_method),
      nsum_methods = unique(summary_df$nsum_method)
    )
  ))
}

# ==============================================================================
# VISUALIZATION AND COMPARISON FUNCTIONS
# ==============================================================================

create_nsum_comparison_plots <- function(summary_results,
                                        save_plots = TRUE,
                                        output_dir = here("output", "figures"),
                                        plot_width = 12,
                                        plot_height = 8) {

  require(ggplot2)
  require(dplyr)
  require(scales)

  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  detailed_df <- summary_results$detailed_summary
  plots <- list()

  # Check if there are any valid estimates
  valid_estimates <- detailed_df %>% filter(!is.na(mean_estimate))

  if (nrow(valid_estimates) == 0) {
    warning("No valid estimates found for plotting. All estimates are NA.")
    cat("Debugging information:\n")
    cat("- Total rows in summary:", nrow(detailed_df), "\n")
    cat("- Valid estimates:", sum(!is.na(detailed_df$mean_estimate)), "\n")
    cat("- Methods in data:", paste(unique(detailed_df$nsum_method), collapse = ", "), "\n")
    cat("- Weight methods:", paste(unique(detailed_df$weight_method), collapse = ", "), "\n")

    # Create placeholder plot explaining the issue
    placeholder_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "No valid NSUM estimates available for plotting.\nAll estimates returned NA.\nPlease check estimation functions and data quality.",
               size = 6, hjust = 0.5, vjust = 0.5) +
      xlim(0, 1) + ylim(0, 1) +
      labs(title = "NSUM Estimation Results: No Valid Estimates",
           subtitle = "Check data quality and estimation functions") +
      theme_void()

    return(list(
      error_message = "No valid estimates",
      placeholder_plot = placeholder_plot,
      debug_info = list(
        total_rows = nrow(detailed_df),
        valid_estimates = sum(!is.na(detailed_df$mean_estimate)),
        methods = unique(detailed_df$nsum_method),
        weight_methods = unique(detailed_df$weight_method)
      )
    ))
  }

  # 1. Forest plot comparing all estimates with confidence intervals
  p1 <- detailed_df %>%
    filter(!is.na(mean_estimate)) %>%
    mutate(
      method_param = ifelse(nsum_method == "MBSU",
                           paste0("MBSU(δ=", delta, ",τ=", tau, ",ρ=", rho, ")"),
                           "GNSUM_Symmetric"),
      combo = paste(outcome_variable, weight_method, sep = " + ")
    ) %>%
    ggplot(aes(x = mean_estimate, y = reorder(method_param, mean_estimate),
               color = weight_method)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    facet_wrap(~outcome_variable, scales = "free_x") +
    scale_x_continuous(labels = comma_format()) +
    labs(
      title = "NSUM Population Estimates with 95% Confidence Intervals",
      subtitle = "Comparison across methods, weight schemes, and parameters",
      x = "Population Estimate",
      y = "Method & Parameters",
      color = "RDS Weight Method"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

  plots$forest_plot <- p1

  if (save_plots) {
    ggsave(file.path(output_dir, "nsum_forest_plot.png"), p1,
           width = plot_width, height = plot_height, dpi = 300)
  }

  # 2. MBSU parameter sensitivity plot
  mbsu_data <- detailed_df %>%
    filter(nsum_method == "MBSU", !is.na(mean_estimate))

  if (nrow(mbsu_data) > 0) {
    p2 <- mbsu_data %>%
      ggplot(aes(x = delta, y = mean_estimate, color = weight_method)) +
      geom_point(alpha = 0.7) +
      geom_line(aes(group = interaction(tau, rho, weight_method)), alpha = 0.5) +
      facet_grid(outcome_variable ~ paste("τ =", tau, ", ρ =", rho)) +
      scale_y_continuous(labels = comma_format()) +
      labs(
        title = "MBSU Parameter Sensitivity Analysis",
        subtitle = "Population estimates across δ (transmission bias) values",
        x = "Delta (δ) - Transmission Bias Factor",
        y = "Population Estimate",
        color = "RDS Weight Method"
      ) +
      theme_minimal()

    plots$parameter_sensitivity <- p2

    if (save_plots) {
      ggsave(file.path(output_dir, "mbsu_parameter_sensitivity.png"), p2,
             width = plot_width, height = plot_height, dpi = 300)
    }
  }

  # 3. Method comparison by weight scheme
  p3 <- detailed_df %>%
    filter(!is.na(mean_estimate)) %>%
    group_by(outcome_variable, weight_method, nsum_method) %>%
    summarise(
      median_estimate = median(mean_estimate, na.rm = TRUE),
      q25 = quantile(mean_estimate, 0.25, na.rm = TRUE),
      q75 = quantile(mean_estimate, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = weight_method, y = median_estimate, fill = nsum_method)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = q25, ymax = q75),
                  position = position_dodge(width = 0.9), width = 0.2) +
    facet_wrap(~outcome_variable, scales = "free_y") +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title = "Method Comparison by RDS Weight Scheme",
      subtitle = "Median estimates with IQR error bars",
      x = "RDS Weight Method",
      y = "Median Population Estimate",
      fill = "NSUM Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plots$method_comparison <- p3

  if (save_plots) {
    ggsave(file.path(output_dir, "method_comparison.png"), p3,
           width = plot_width, height = plot_height, dpi = 300)
  }

  # 4. Confidence interval width comparison
  p4 <- detailed_df %>%
    filter(!is.na(ci_lower), !is.na(ci_upper)) %>%
    mutate(ci_width = ci_upper - ci_lower,
           rel_ci_width = ci_width / mean_estimate) %>%
    ggplot(aes(x = weight_method, y = rel_ci_width, fill = nsum_method)) +
    geom_boxplot(alpha = 0.8) +
    facet_wrap(~outcome_variable) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Confidence Interval Width Analysis",
      subtitle = "Relative CI width (CI width / point estimate)",
      x = "RDS Weight Method",
      y = "Relative CI Width",
      fill = "NSUM Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plots$ci_width_analysis <- p4

  if (save_plots) {
    ggsave(file.path(output_dir, "ci_width_analysis.png"), p4,
           width = plot_width, height = plot_height, dpi = 300)
  }

  return(plots)
}

create_nsum_summary_table <- function(summary_results, save_table = TRUE,
                                     output_dir = here("output", "tables")) {

  if (save_table && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Check for valid estimates
  valid_data <- summary_results$detailed_summary %>%
    filter(!is.na(mean_estimate))

  if (nrow(valid_data) == 0) {
    warning("No valid estimates found for summary table. All estimates are NA.")
    return(data.frame(
      Message = "No valid NSUM estimates available",
      Total_Combinations = nrow(summary_results$detailed_summary),
      Valid_Estimates = 0,
      Note = "Check estimation functions and data quality"
    ))
  }

  # Create formatted summary table
  summary_table <- valid_data %>%
    mutate(
      Method = ifelse(nsum_method == "MBSU",
                     paste0("MBSU (δ=", delta, ", τ=", tau, ", ρ=", rho, ")"),
                     "GNSUM Symmetric"),
      `Point Estimate` = format(round(mean_estimate), big.mark = ","),
      `95% CI` = paste0("[", format(round(ci_lower), big.mark = ","),
                       ", ", format(round(ci_upper), big.mark = ","), "]"),
      `CI Width` = format(round(ci_upper - ci_lower), big.mark = ","),
      `Valid Samples` = n_valid
    ) %>%
    select(outcome_variable, weight_method, Method, `Point Estimate`,
           `95% CI`, `CI Width`, `Valid Samples`) %>%
    arrange(outcome_variable, weight_method, Method)

  if (save_table) {
    write.csv(summary_table,
              file.path(output_dir, "nsum_bootstrap_summary.csv"),
              row.names = FALSE)
  }

  return(summary_table)
}

# ==============================================================================
# TESTING AND VALIDATION FUNCTIONS
# ==============================================================================

test_comprehensive_bootstrap <- function(verbose = TRUE) {

  if (verbose) cat("=== Testing Comprehensive 3-Step Bootstrap ===\n")

  # Create test bootstrap samples (simulating Step 1 output)
  n_bootstrap <- 10  # Small number for testing
  set.seed(12345)

  boot_samples <- map(1:n_bootstrap, function(i) {
    data.frame(
      id = 1:50,
      document_withholding_nsum = rbinom(50, 3, 0.1),
      document_withholding_rds = rbinom(50, 1, 0.15),
      pay_issues_nsum = rbinom(50, 2, 0.08),
      pay_issues_rds = rbinom(50, 1, 0.12),
      known_network_size = rpois(50, 25) + 5,
      weight_vh = runif(50, 0.001, 0.01),
      weight_rds_i = runif(50, 0.001, 0.01),
      weight_rds_ss = runif(50, 0.001, 0.01)
    )
  })

  if (verbose) {
    cat("Created", length(boot_samples), "bootstrap samples with", nrow(boot_samples[[1]]), "observations each\n")
  }

  # Test with minimal configuration for speed
  test_config <- list(
    delta_range = c(0.8, 1.0),
    tau_range = c(0.9, 1.0),
    rho_range = c(1.0)
  )

  if (verbose) {
    cat("Running comprehensive bootstrap with minimal configuration...\n")
    cat("MBSU factor combinations:", length(test_config$delta_range) * length(test_config$tau_range) * length(test_config$rho_range), "\n")
  }

  # Run comprehensive analysis
  results <- run_comprehensive_nsum_bootstrap(
    boot_samples = boot_samples,
    outcome_variables = c("document_withholding_nsum", "pay_issues_nsum"),
    hidden_indicators = c("document_withholding_rds", "pay_issues_rds"),
    rds_weight_methods = c("weight_vh", "weight_rds_i"),  # Reduced for testing
    mbsu_config = test_config,
    parallel = FALSE,  # Sequential for testing
    verbose = verbose
  )

  if (verbose) {
    cat("\n=== Test Results Summary ===\n")
    cat("Total result combinations:", length(results$detailed_results), "\n")
    cat("Summary rows:", nrow(results$summary_results$detailed_summary), "\n")
    cat("Valid estimates:", results$summary_results$overall_stats$valid_combinations, "\n")
    cat("Methods tested:", paste(results$summary_results$overall_stats$nsum_methods, collapse = ", "), "\n")
    cat("Weight methods tested:", paste(results$summary_results$overall_stats$weight_methods, collapse = ", "), "\n")

    # Show sample results
    valid_results <- results$summary_results$detailed_summary %>%
      filter(!is.na(mean_estimate)) %>%
      head(3)

    if (nrow(valid_results) > 0) {
      cat("\nSample estimates:\n")
      for (i in 1:nrow(valid_results)) {
        row <- valid_results[i, ]
        cat(sprintf("- %s (%s, %s): %s [CI: %s - %s]\n",
                   row$outcome_variable,
                   row$nsum_method,
                   row$weight_method,
                   format(round(row$mean_estimate), big.mark = ","),
                   format(round(row$ci_lower), big.mark = ","),
                   format(round(row$ci_upper), big.mark = ",")))
      }
    }
  }

  return(results)
}

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
  cat("- run_comprehensive_nsum_bootstrap(): Full 3-step bootstrap analysis\n")
  cat("- debug_estimation_functions(): Troubleshoot NA estimates\n")
  cat("- test_nsum_estimation(): Test all implementations\n\n")

  cat("Usage examples:\n")
  cat("# Basic MBSU with default adjustment factors (no bias)\n")
  cat("result <- estimate_nsum(data, 'mbsu', 'outcome_var', frame_size = 980000)\n\n")
  cat("# MBSU with custom adjustment factors for production run\n")
  cat("mbsu_adj <- list(delta = 0.8, tau = 0.7, rho = 1.2)  # Expert elicitation values\n")
  cat("result_adj <- estimate_nsum(data, 'mbsu', 'outcome_var', frame_size = 980000,\n")
  cat("                           weight_column = 'weight_vh', adjustment_factors = mbsu_adj)\n\n")
  cat("# GNSUM with symmetric visibility (the implementable version)\n")
  cat("result_gnsum <- estimate_nsum(data, 'gnsum_symmetric', 'outcome_var', frame_size = 980000,\n")
  cat("                             hidden_member_indicator = 'rds_var', weight_column = 'weight_vh')\n\n")
  cat("# MBSU sensitivity analysis across multiple adjustment factors\n")
  cat("sweep_results <- estimate_mbsu_sweep(data, 'outcome_var', frame_size = 980000,\n")
  cat("                                    weight_column = 'weight_vh',\n")
  cat("                                    delta_range = seq(0.5, 1.0, by = 0.1),\n")
  cat("                                    tau_range = seq(0.6, 1.0, by = 0.1),\n")
  cat("                                    rho_range = seq(0.8, 1.2, by = 0.1))\n\n")
  cat("# Comprehensive 3-step bootstrap analysis\n")
  cat("bootstrap_results <- run_comprehensive_nsum_bootstrap(\n")
  cat("  boot_samples = boot_samples,\n")
  cat("  outcome_variables = c('document_withholding_nsum', 'pay_issues_nsum'),\n")
  cat("  hidden_indicators = c('document_withholding_rds', 'pay_issues_rds'),\n")
  cat("  rds_weight_methods = c('weight_vh', 'weight_rds_i', 'weight_rds_ss'),\n")
  cat("  mbsu_config = list(\n")
  cat("    delta_range = seq(0.5, 1.0, by = 0.1),\n")
  cat("    tau_range = seq(0.6, 1.0, by = 0.1),\n")
  cat("    rho_range = seq(0.8, 1.2, by = 0.1)\n")
  cat("  )\n")
  cat(")\n\n")

  cat("# Create comprehensive visualizations and summary tables\n")
  cat("plots <- create_nsum_comparison_plots(bootstrap_results$summary_results)\n")
  cat("summary_table <- create_nsum_summary_table(bootstrap_results$summary_results)\n\n")

  cat("# View specific visualization outputs\n")
  cat("print(plots$forest_plot)              # All estimates with 95% CIs\n")
  cat("print(plots$parameter_sensitivity)    # MBSU parameter sensitivity\n")
  cat("print(plots$method_comparison)        # Method comparison by weight scheme\n")
  cat("print(plots$ci_width_analysis)        # CI width comparison\n\n")

  cat("# Access structured results for further analysis\n")
  cat("detailed_df <- bootstrap_results$summary_results$detailed_summary\n")
  cat("# detailed_df contains: outcome_variable, weight_method, nsum_method,\n")
  cat("# delta, tau, rho, mean_estimate, ci_lower, ci_upper, n_valid\n\n")

  cat("# Example: Filter to specific combinations for detailed analysis\n")
  cat("high_precision <- detailed_df %>% filter(ci_upper - ci_lower < 50000)\n")
  cat("View(high_precision)  # Interactive table view\n\n")
}

# ==============================================================================
# DEBUGGING FUNCTIONS
# ==============================================================================

debug_estimation_functions <- function(boot_samples, verbose = TRUE) {
  if (verbose) cat("=== Debugging NSUM Estimation Functions ===\n")

  # Use first bootstrap sample for testing
  test_data <- boot_samples[[1]]
  frame_size <- 980000

  if (verbose) {
    cat("Test data dimensions:", nrow(test_data), "x", ncol(test_data), "\n")
    cat("Available columns:", paste(names(test_data), collapse = ", "), "\n")
  }

  # Check if required variables exist
  required_vars <- c("document_withholding_nsum", "known_network_size", "weight_vh")
  missing_vars <- required_vars[!required_vars %in% names(test_data)]

  if (length(missing_vars) > 0) {
    cat("ERROR: Missing required variables:", paste(missing_vars, collapse = ", "), "\n")

    # Look for weight-related variables that might exist
    weight_vars <- names(test_data)[grepl("weight|inclusion|prob", names(test_data), ignore.case = TRUE)]
    if (length(weight_vars) > 0) {
      cat("Available weight-related variables:", paste(weight_vars, collapse = ", "), "\n")
    } else {
      cat("No weight-related variables found. Bootstrap samples missing RDS weights.\n")
      cat("Expected weight variables: weight_vh, weight_rds_i, weight_rds_ii, weight_rds_ss\n")
      cat("These should have been added in Steps 1-2 of the bootstrap procedure.\n")
    }
    return(NULL)
  }

  # Test MBSU estimation
  if (verbose) cat("\n--- Testing MBSU ---\n")
  mbsu_result <- tryCatch({
    estimate_mbsu(
      data = test_data,
      outcome_variable = "document_withholding_nsum",
      degree_variable = "known_network_size",
      frame_size = frame_size,
      weight_column = "weight_vh",
      adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0)
    )
  }, error = function(e) {
    if (verbose) cat("MBSU Error:", e$message, "\n")
    list(N_hat = NA, error = e$message)
  })

  if (verbose) {
    cat("MBSU estimate:", mbsu_result$N_hat, "\n")
    if (!is.null(mbsu_result$error)) cat("MBSU error:", mbsu_result$error, "\n")
  }

  # Test GNSUM Symmetric estimation
  if (verbose) cat("\n--- Testing GNSUM Symmetric ---\n")
  gnsum_result <- tryCatch({
    estimate_gnsum_symmetric(
      data = test_data,
      outcome_variable = "document_withholding_nsum",
      degree_variable = "known_network_size",
      frame_size = frame_size,
      hidden_member_indicator = "document_withholding_rds",
      weight_column = "weight_vh"
    )
  }, error = function(e) {
    if (verbose) cat("GNSUM Error:", e$message, "\n")
    list(N_hat = NA, error = e$message)
  })

  if (verbose) {
    cat("GNSUM estimate:", gnsum_result$N_hat, "\n")
    if (!is.null(gnsum_result$error)) cat("GNSUM error:", gnsum_result$error, "\n")
  }

  # Test data quality
  if (verbose) cat("\n--- Data Quality Check ---\n")
  outcome_data <- test_data$document_withholding_nsum
  degree_data <- test_data$known_network_size
  weight_data <- test_data$weight_vh

  cat("Outcome variable summary:\n")
  print(summary(outcome_data))
  cat("Degree variable summary:\n")
  print(summary(degree_data))
  cat("Weight variable summary:\n")
  print(summary(weight_data))

  cat("Non-missing cases:", sum(!is.na(outcome_data) & !is.na(degree_data) & !is.na(weight_data)), "\n")
  cat("Zero weight cases:", sum(weight_data == 0, na.rm = TRUE), "\n")
  cat("Zero degree cases:", sum(degree_data == 0, na.rm = TRUE), "\n")

  return(list(
    mbsu_result = mbsu_result,
    gnsum_result = gnsum_result,
    data_summary = list(
      n_valid = sum(!is.na(outcome_data) & !is.na(degree_data) & !is.na(weight_data)),
      outcome_mean = mean(outcome_data, na.rm = TRUE),
      degree_mean = mean(degree_data, na.rm = TRUE),
      weight_mean = mean(weight_data, na.rm = TRUE)
    )
  ))
}

# Enhanced debugging function to trace the issue step-by-step
deep_debug_nsum <- function(boot_samples, verbose = TRUE) {
  cat("=== DEEP DEBUGGING NSUM ESTIMATION ===\n")

  # Handle both list of bootstrap samples and single data frame
  if (is.list(boot_samples) && !is.data.frame(boot_samples)) {
    cat("Boot samples is a list with", length(boot_samples), "elements\n")
    test_data <- boot_samples[[1]]
  } else if (is.data.frame(boot_samples)) {
    cat("Boot samples is a single data frame\n")
    test_data <- boot_samples
  } else {
    cat("ERROR: boot_samples must be a data frame or list of data frames\n")
    return(invisible(NULL))
  }

  # 1. Check data structure
  cat("1. DATA STRUCTURE CHECK\n")
  cat("Test data class:", class(test_data), "\n")
  cat("Dimensions:", nrow(test_data), "x", ncol(test_data), "\n")

  # 2. Check key variables
  cat("\n2. KEY VARIABLES CHECK\n")
  key_vars <- c("id", "known_network_size", "document_withholding_nsum", "weight_vh", "weight_rds_i")
  for (var in key_vars) {
    if (var %in% colnames(test_data)) {
      n_na <- sum(is.na(test_data[[var]]))
      n_zero <- sum(test_data[[var]] == 0, na.rm = TRUE)
      n_valid <- sum(!is.na(test_data[[var]]) & test_data[[var]] != 0)
      cat(sprintf("  %s: %d valid, %d zeros, %d NAs (range: %s to %s)\n",
                  var, n_valid, n_zero, n_na,
                  min(test_data[[var]], na.rm = TRUE),
                  max(test_data[[var]], na.rm = TRUE)))
    } else {
      cat(sprintf("  %s: MISSING\n", var))
    }
  }

  # 3. Check for valid estimation sample
  cat("\n3. VALID ESTIMATION SAMPLE CHECK\n")
  valid_sample <- test_data %>%
    filter(!is.na(known_network_size) & known_network_size > 0 &
           !is.na(weight_vh) & weight_vh > 0 &
           !is.na(document_withholding_nsum))

  cat("Valid sample size:", nrow(valid_sample), "out of", nrow(test_data), "\n")

  if (nrow(valid_sample) == 0) {
    cat("ERROR: No valid samples for estimation!\n")
    return(invisible(NULL))
  }

  # 4. Test basic NSUM calculation manually
  cat("\n4. MANUAL NSUM CALCULATION TEST\n")
  test_sample <- valid_sample[1:min(5, nrow(valid_sample)), ]

  cat("Test sample (first 5 valid rows):\n")

  # Handle RDS data frame indexing properly
  tryCatch({
    if ("rds.data.frame" %in% class(test_sample)) {
      # For RDS data frames, convert to regular data frame for display
      display_sample <- as.data.frame(test_sample)[, c("id", "known_network_size", "document_withholding_nsum", "weight_vh")]
      print(display_sample)
    } else {
      print(test_sample[, c("id", "known_network_size", "document_withholding_nsum", "weight_vh")])
    }
  }, error = function(e) {
    cat("Error displaying test sample:", e$message, "\n")
    cat("Sample dimensions:", nrow(test_sample), "x", ncol(test_sample), "\n")
  })


  # Basic NSUM formula: sum(y_i * w_i) / sum(d_i * w_i) * frame_size
  y_weighted <- sum(test_sample$document_withholding_nsum * test_sample$weight_vh, na.rm = TRUE)
  d_weighted <- sum(test_sample$known_network_size * test_sample$weight_vh, na.rm = TRUE)
  frame_size <- 100000

  cat("y_weighted (numerator):", y_weighted, "\n")
  cat("d_weighted (denominator):", d_weighted, "\n")
  cat("Frame size:", frame_size, "\n")

  if (d_weighted == 0) {
    cat("ERROR: Zero denominator in NSUM calculation!\n")
  } else {
    manual_estimate <- (y_weighted / d_weighted) * frame_size
    cat("Manual NSUM estimate:", manual_estimate, "\n")
  }

  # 5. Test estimate_mbsu function step by step
  cat("\n5. STEP-BY-STEP MBSU FUNCTION TEST\n")
  tryCatch({
    cat("Calling estimate_mbsu with test sample...\n")
    mbsu_result <- estimate_mbsu(
      data = test_sample,
      outcome_variable = "document_withholding_nsum",
      degree_variable = "known_network_size",
      frame_size = frame_size,
      weight_column = "weight_vh",
      adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0)
    )


    cat("MBSU function result:\n")
    if (is.list(mbsu_result)) {
      print(mbsu_result)
    } else {
      cat("  Estimate:", mbsu_result, "\n")
    }

    # Check if the main estimate is NA
    estimate_value <- if (is.list(mbsu_result) && "N_hat" %in% names(mbsu_result)) {
      mbsu_result$N_hat
    } else if (is.list(mbsu_result)) {
      mbsu_result[[1]]
    } else {
      mbsu_result
    }

    if (is.na(estimate_value)) {
      cat("MBSU returned NA - investigating inside function...\n")
    } else {
      cat("MBSU estimate extracted:", estimate_value, "\n")

    }

  }, error = function(e) {
    cat("MBSU function error:", e$message, "\n")
    cat("Full error:", toString(e), "\n")
  })

  return(invisible(list(
    test_data = test_data,
    valid_sample = valid_sample,
    test_sample = test_sample
  )))
}

# Example: Using different weight methods and adjustment factors
#
# Production MBSU with expert-elicited adjustment factors:
# - delta = 0.75: Hidden workers may be less likely to disclose status (75% transmission)
# - tau = 0.8: Some social barriers between hidden and frame populations (80% mixing)
# - rho = 0.9: Hidden workers may be slightly less visible in networks (90% visibility)
mbsu_production <- list(delta = 0.75, tau = 0.8, rho = 0.9)

# result_mbsu <- estimate_nsum(weighted_bootstrap_samples[[1]], 'mbsu', 'excessive_hours_nsum',
#                             frame_size = 980000, weight_column = 'weight_rds_ss',
#                             adjustment_factors = mbsu_production)
# result_gnsum_sym <- estimate_nsum(weighted_bootstrap_samples[[1]], 'gnsum_symmetric', 'excessive_hours_nsum',
#                                  frame_size = 980000, weight_column = 'weight_rds_ss',
#                                  hidden_member_indicator = 'excessive_hours_rds')

# Run comprehensive analysis
bootstrap_results <- run_comprehensive_nsum_bootstrap(
  boot_samples = boot_samples,              # From Step 1-2
  outcome_variables = c(                    # NSUM outcome 
      "document_withholding_nsum",
      "pay_issues_nsum",
      "threats_abuse_nsum",
      "excessive_hours_nsum",
      "access_to_help_nsum"
    ),
    hidden_indicators = c(                    # Corresponding RDS   variables  
      "document_withholding_rds",
      "pay_issues_rds",
      "threats_abuse_rds",
      "excessive_hours_rds",
      "access_to_help_rds"
    ),
    degree_variable = "known_network_size",   # Network size  variable (default)
    frame_size = 980000,                      # UK domestic worker population (default)
    rds_weight_methods = c(                   # All RDS weight   schemes
      "weight_vh",
      "weight_rds_i",
      "weight_rds_ii",
      "weight_rds_ss"
    ),
    mbsu_config = list(                       # MBSU parameter sweep ranges
      delta_range = seq(0.5, 1.0, by = 0.1), # Transmission bias factors
      tau_range = seq(0.6, 1.0, by = 0.1),   # Barrier effect   factors  
      rho_range = seq(0.8, 1.2, by = 0.1)    # Popularity bias   factors
    ),
    confidence_level = 0.95,                  # For bootstrap CIs  (default)
    parallel = TRUE,                          # Use parallel processing (default)
    n_cores = 4,                             # Number of cores (default: auto-detect)
    verbose = TRUE                           # Progress messages   (default)
  )

summary(bootstrap_results$summary_results$mean_estimate)

# Create all visualizations
plots <-
  create_nsum_comparison_plots(bootstrap_results$summary_results)
summary_table <-
  create_nsum_summary_table(bootstrap_results$summary_results)

# Assume 'plots' is a list of ggplot objects
for (i in seq_along(plots)) {
  # Create a filename for each plot
  fname <- paste0("comparison_plot_", i, ".png")
  # Save the plot
  ggsave(filename = fname, plot = plots[[i]], width = 8, height = 6, dpi = 300)
}



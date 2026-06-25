# nsum_core_estimators.R
# Core NSUM Estimation Functions for RDS+NSUM Hybrid Design
#
# These functions implement various NSUM estimators with proper inclusion
# probability (π_i) weighting following Feehan & Salganik (2016).
#
# All estimators are compatible with:
# - Point estimation (on original data)
# - Bootstrap estimation (on resampled data)

# ============================================================================
# CORE NSUM ESTIMATOR: MODIFIED BASIC SCALE-UP (MBSU)
# ============================================================================

#' Modified Basic Scale-Up Estimator with Inclusion Probabilities
#'
#' Implements the Modified Basic Scale-Up method for RDS+NSUM hybrid design.
#' Uses direct network size (not probe questions) and accounts for RDS
#' sampling design via inclusion probabilities. Includes Feehan & Salganik (2016)
#' adjustment factors for transmission bias, barriers, and popularity.
#'
#' @param data Data frame containing survey responses
#' @param hidden_var Character, name of alter report variable (binary: knows someone in hidden pop)
#' @param degree_var Character, name of network size variable (default: "known_network_size")
#' @param N_F Numeric, frame population size (total UK domestic workers)
#' @param weights Numeric vector, RDS weights (optional, calculated to π_i on-the-fly)
#' @param pi_column Character, name of column containing pre-calculated π_i (for bootstrap samples)
#' @param use_inclusion_probs Logical, use Horvitz-Thompson weighting with π_i (default: TRUE)
#' @param adjustment_factors List with delta, tau, rho adjustment factors (default: all 1.0)
#' @param verbose Logical, print progress messages
#'
#' @return List containing:
#'   - N_H_estimate / N_hat: Estimated hidden population size (adjusted)
#'   - prevalence_rate: N_H / N_F
#'   - y_F_H / y_FH_proportion: Average alter reports (weighted)
#'   - d_F_F / d_FF_average: Average network size (weighted)
#'   - basic_estimate: Unadjusted NSUM estimate
#'   - adjustment_impact: Total adjustment factor applied
#'   - delta_factor, tau_factor, rho_factor: Individual adjustment factors
#'   - n_effective: Effective sample size
#'   - method: Estimation method used
#'
#' @details
#' Basic Formula: N_H_basic = (y_F,H / d_F,F) × N_F
#' Adjusted Formula: N_H_adjusted = N_H_basic × (1/δ) × (1/τ) × ρ
#'
#' With inclusion probabilities:
#'   y_F,H = Σ(y_i / π_i) / Σ(1 / π_i)  (Horvitz-Thompson estimator)
#'   d_F,F = Σ(d_i / π_i) / Σ(1 / π_i)  (Horvitz-Thompson estimator)
#'
#' Where π_i = inclusion probability (from RDS weights or pre-calculated column)
#'
#' Adjustment factors (Feehan & Salganik 2016):
#'   - delta (δ): Transmission bias (0 < δ ≤ 1, lower = more bias)
#'   - tau (τ): Barrier effect (0 < τ ≤ 1, lower = more barriers)
#'   - rho (ρ): Popularity bias (ρ > 0, higher = hidden pop more visible)
#'
estimate_mbsu <- function(data,
                          hidden_var,
                          degree_var = "known_network_size",
                          N_F = 980000,
                          weights = NULL,
                          pi_column = NULL,
                          use_inclusion_probs = TRUE,
                          adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0),
                          verbose = FALSE) {

  # Validate inputs
  if (!hidden_var %in% names(data)) {
    stop("Hidden variable '", hidden_var, "' not found in data")
  }
  if (!degree_var %in% names(data)) {
    stop("Degree variable '", degree_var, "' not found in data")
  }

  # Extract variables
  y_i <- data[[hidden_var]]  # Alter reports
  d_i <- data[[degree_var]]  # Network sizes

  # Remove missing values
  complete_idx <- complete.cases(y_i, d_i)
  if (!is.null(weights)) {
    complete_idx <- complete_idx & !is.na(weights)
  }
  if (!is.null(pi_column) && pi_column %in% names(data)) {
    complete_idx <- complete_idx & !is.na(data[[pi_column]])
  }

  y_i <- y_i[complete_idx]
  d_i <- d_i[complete_idx]
  n <- length(y_i)

  if (n == 0) {
    stop("No complete cases available for estimation")
  }

  if (verbose) {
    cat("  Sample size:", n, "\n")
  }

  # ========================================================================
  # STEP 1: Get or calculate inclusion probabilities (π_i)
  # ========================================================================

  # Priority: pi_column > weights > unweighted
  if (!is.null(pi_column) && pi_column %in% names(data)) {
    # Option 1: Read π_i directly from data column (for bootstrap samples)
    pi_i <- data[[pi_column]][complete_idx]

    # Validate π_i values
    if (any(pi_i <= 0, na.rm = TRUE)) {
      stop("Inclusion probabilities must be positive")
    }

    method <- "MBSU (weighted, pre-calculated π_i)"

    if (verbose) {
      cat("  Using pre-calculated π_i from column:", pi_column, "\n")
      cat("  π_i range: [", round(min(pi_i, na.rm = TRUE), 4), ", ",
          round(max(pi_i, na.rm = TRUE), 4), "]\n")
      cat("  Mean π_i:", round(mean(pi_i, na.rm = TRUE), 4), "\n")
    }

  } else if (is.null(weights) || !use_inclusion_probs) {
    # Option 2: Unweighted estimation (simple random sampling assumption)
    pi_i <- rep(1, n)
    method <- "MBSU (unweighted)"

    if (verbose) cat("  Using unweighted estimation\n")

  } else {
    # Option 3: Calculate π_i from RDS weights
    weights <- weights[complete_idx]

    # Convert RDS weights to inclusion probabilities
    # RDS weights are already proportional to 1/π_i, so:
    # - Normalize weights to sum to 1
    # - π_i ∝ normalized_weight × n (higher weight = higher inclusion prob)

    w_norm <- weights / sum(weights, na.rm = TRUE)
    pi_i <- w_norm * n

    # Ensure π_i are in valid range (0, 1]
    pi_i <- pmin(pi_i, 1)  # Cap at 1
    pi_i <- pmax(pi_i, 0.001)  # Minimum to avoid division by zero

    method <- "MBSU (weighted, calculated π_i)"

    if (verbose) {
      cat("  Using weighted estimation with calculated π_i\n")
      cat("  π_i range: [", round(min(pi_i), 4), ", ", round(max(pi_i), 4), "]\n")
      cat("  Mean π_i:", round(mean(pi_i), 4), "\n")
    }
  }

  # ========================================================================
  # STEP 2: Calculate Horvitz-Thompson weighted averages
  # ========================================================================

  # Horvitz-Thompson estimator for alter reports
  # y_F,H = Σ(y_i / π_i) / Σ(1 / π_i)
  y_F_H <- sum(y_i / pi_i, na.rm = TRUE) / sum(1 / pi_i, na.rm = TRUE)

  # Horvitz-Thompson estimator for network size
  # d_F,F = Σ(d_i / π_i) / Σ(1 / pi_i)
  d_F_F <- sum(d_i / pi_i, na.rm = TRUE) / sum(1 / pi_i, na.rm = TRUE)

  if (verbose) {
    cat("  y_F,H (alter reports):", round(y_F_H, 4), "\n")
    cat("  d_F,F (network size):", round(d_F_F, 4), "\n")
  }

  # Check for zero network size
  if (d_F_F <= 0) {
    stop("Average network size is zero or negative")
  }

  # ========================================================================
  # STEP 3: Calculate basic NSUM estimate
  # ========================================================================

  # Modified Basic Scale-Up formula (unadjusted)
  basic_estimate <- (y_F_H / d_F_F) * N_F

  # ========================================================================
  # STEP 4: Apply MBSU adjustment factors (Feehan & Salganik 2016)
  # ========================================================================

  # Extract adjustment factors
  delta <- adjustment_factors$delta  # Transmission bias (0 < δ ≤ 1)
  tau <- adjustment_factors$tau      # Barrier effect (0 < τ ≤ 1)
  rho <- adjustment_factors$rho      # Popularity bias (ρ > 0)

  # Validate adjustment factors
  if (any(c(delta, tau, rho) <= 0)) {
    stop("All adjustment factors must be positive")
  }

  # Apply adjustments: N_H_adjusted = N_H_basic × (1/δ) × (1/τ) × ρ
  delta_adjustment <- 1 / delta
  tau_adjustment <- 1 / tau
  rho_adjustment <- rho

  total_adjustment <- delta_adjustment * tau_adjustment * rho_adjustment
  adjusted_estimate <- basic_estimate * total_adjustment

  # Final estimate and prevalence
  N_H <- adjusted_estimate
  prevalence <- N_H / N_F

  # Calculate effective sample size (accounts for weighting)
  # n_eff = (Σπ_i)² / Σ(π_i²)
  n_effective <- (sum(pi_i, na.rm = TRUE))^2 / sum(pi_i^2, na.rm = TRUE)

  if (verbose) {
    cat("  y_F,H:", round(y_F_H, 4), "\n")
    cat("  d_F,F:", round(d_F_F, 2), "\n")
    cat("  Basic estimate:", format(round(basic_estimate), big.mark = ","), "\n")
    cat("  Adjustment factors: δ =", delta, ", τ =", tau, ", ρ =", rho, "\n")
    cat("  Total adjustment:", round(total_adjustment, 3), "\n")
    cat("  Adjusted estimate:", format(round(adjusted_estimate), big.mark = ","), "\n")
    cat("  Prevalence:", round(prevalence * 100, 2), "%\n")
    cat("  Effective n:", round(n_effective, 1), "\n")
  }

  # ========================================================================
  # STEP 5: Return results
  # ========================================================================

  return(list(
    # Main estimates
    N_H_estimate = N_H,
    N_hat = N_H,  # Alias for backward compatibility with Boot_Step3.R
    prevalence_rate = prevalence,

    # Components
    y_F_H = y_F_H,
    y_FH_proportion = y_F_H,  # Alias for Boot_Step3.R compatibility
    d_F_F = d_F_F,
    d_FF_average = d_F_F,  # Alias for Boot_Step3.R compatibility

    # Adjustment details
    basic_estimate = basic_estimate,
    adjustment_impact = total_adjustment,
    delta_factor = delta_adjustment,
    tau_factor = tau_adjustment,
    rho_factor = rho_adjustment,

    # Sample information
    sample_size = n,
    n_valid = n,  # Alias for Boot_Step3.R compatibility
    n_effective = n_effective,
    n_missing = sum(!complete_idx),

    # Metadata
    N_F = N_F,
    method = method,
    hidden_variable = hidden_var,
    degree_variable = degree_var,
    weighted = !is.null(weights) || !is.null(pi_column),
    error = NA
  ))
}

# ============================================================================
# ALTERNATIVE ESTIMATOR: BASIC NSUM (Traditional)
# ============================================================================

#' Basic NSUM Estimator (Traditional)
#'
#' Traditional NSUM estimator without RDS adjustments.
#' Assumes simple random sampling (no inclusion probability weighting).
#' Included for comparison purposes.
#'
#' @inheritParams estimate_mbsu
#' @return List with same structure as estimate_mbsu
#'
estimate_basic_nsum <- function(data,
                                hidden_var,
                                degree_var = "known_network_size",
                                N_F = 980000,
                                verbose = FALSE) {

  # Call MBSU with no weights (forces unweighted estimation)
  result <- estimate_mbsu(
    data = data,
    hidden_var = hidden_var,
    degree_var = degree_var,
    N_F = N_F,
    weights = NULL,
    use_inclusion_probs = FALSE,
    verbose = verbose
  )

  result$method <- "Basic NSUM (unweighted)"
  return(result)
}

# ============================================================================
# ALTERNATIVE ESTIMATOR: SYMMETRIC GNSUM (Placeholder)
# ============================================================================

#' Symmetric Generalized NSUM (Placeholder)
#'
#' GNSUM variant assuming reciprocal ties (symmetric network).
#' This is a placeholder for future implementation.
#'
#' @inheritParams estimate_mbsu
#' @return List with same structure as estimate_mbsu
#'
#' @details
#' The symmetric GNSUM assumes that if person i knows person j in the hidden
#' population, then j also knows i (reciprocal ties). This may be more
#' appropriate for close relationships.
#'
#' TODO: Implement full symmetric GNSUM logic
#'
estimate_symmetric_gnsum <- function(data,
                                    hidden_var,
                                    degree_var = "known_network_size",
                                    N_F = 980000,
                                    weights = NULL,
                                    use_inclusion_probs = TRUE,
                                    verbose = FALSE) {

  # For now, call MBSU (placeholder implementation)
  # TODO: Implement symmetric correction factor

  result <- estimate_mbsu(
    data = data,
    hidden_var = hidden_var,
    degree_var = degree_var,
    N_F = N_F,
    weights = weights,
    use_inclusion_probs = use_inclusion_probs,
    verbose = verbose
  )

  result$method <- paste0(result$method, " + symmetric assumption")

  if (verbose) {
    cat("  NOTE: Symmetric GNSUM not yet fully implemented\n")
    cat("  Using MBSU as placeholder\n")
  }

  return(result)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Validate NSUM Variables
#'
#' Check that required variables exist in data
#'
#' @param data Data frame
#' @param hidden_var Character, hidden variable name
#' @param degree_var Character, degree variable name
#'
#' @return List with 'valid' (logical) and 'missing_vars' (character vector)
#'
validate_nsum_variables <- function(data, hidden_var, degree_var) {
  missing_vars <- character(0)

  if (!hidden_var %in% names(data)) {
    missing_vars <- c(missing_vars, hidden_var)
  }

  if (!degree_var %in% names(data)) {
    missing_vars <- c(missing_vars, degree_var)
  }

  if (length(missing_vars) > 0) {
    return(list(
      valid = FALSE,
      missing_vars = missing_vars,
      error = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
    ))
  }

  return(list(valid = TRUE, missing_vars = character(0)))
}

#' Convert RDS Weights to Inclusion Probabilities
#'
#' Convert normalized RDS weights to inclusion probabilities for
#' Horvitz-Thompson estimation.
#'
#' @param weights Numeric vector, RDS weights (unnormalized or normalized)
#' @param n Integer, sample size
#'
#' @return Numeric vector of inclusion probabilities (π_i)
#'
#' @details
#' RDS weights w_i are proportional to 1/π_i, so:
#' 1. Normalize: w_norm = w_i / Σw_i
#' 2. Convert: π_i = w_norm × n
#' 3. Constrain: π_i ∈ (0, 1]
#'
convert_weights_to_pi <- function(weights, n) {
  # Normalize weights
  w_norm <- weights / sum(weights, na.rm = TRUE)

  # Convert to inclusion probabilities
  pi_i <- w_norm * n

  # Constrain to valid range
  pi_i <- pmin(pi_i, 1)  # Cap at 1
  pi_i <- pmax(pi_i, 0.001)  # Minimum floor

  return(pi_i)
}

# ============================================================================
# TESTING FUNCTION
# ============================================================================

#' Test NSUM Estimators
#'
#' Quick test of all NSUM estimators on sample data
#'
#' @param data Data frame with NSUM variables
#' @param hidden_var Character, hidden variable name
#' @param degree_var Character, degree variable name
#' @param weights Numeric vector, RDS weights (optional)
#' @param N_F Numeric, frame population size
#'
#' @return Data frame comparing all estimators
#'
test_nsum_estimators <- function(data,
                                 hidden_var,
                                 degree_var = "known_network_size",
                                 weights = NULL,
                                 N_F = 980000) {

  cat("=== Testing NSUM Estimators ===\n")
  cat("Hidden variable:", hidden_var, "\n")
  cat("Degree variable:", degree_var, "\n")
  cat("Population size:", format(N_F, big.mark = ","), "\n")
  cat("Weights provided:", !is.null(weights), "\n\n")

  results <- list()

  # Test 1: Basic NSUM (unweighted)
  cat("1. Basic NSUM (unweighted)...\n")
  results$basic <- estimate_basic_nsum(
    data = data,
    hidden_var = hidden_var,
    degree_var = degree_var,
    N_F = N_F,
    verbose = TRUE
  )
  cat("\n")

  # Test 2: MBSU (unweighted)
  cat("2. MBSU (unweighted)...\n")
  results$mbsu_unweighted <- estimate_mbsu(
    data = data,
    hidden_var = hidden_var,
    degree_var = degree_var,
    N_F = N_F,
    weights = NULL,
    verbose = TRUE
  )
  cat("\n")

  # Test 3: MBSU (weighted) - only if weights provided
  if (!is.null(weights)) {
    cat("3. MBSU (weighted with π_i)...\n")
    results$mbsu_weighted <- estimate_mbsu(
      data = data,
      hidden_var = hidden_var,
      degree_var = degree_var,
      N_F = N_F,
      weights = weights,
      use_inclusion_probs = TRUE,
      verbose = TRUE
    )
    cat("\n")
  }

  # Test 4: Symmetric GNSUM (placeholder)
  if (!is.null(weights)) {
    cat("4. Symmetric GNSUM (weighted with π_i)...\n")
    results$symmetric_gnsum <- estimate_symmetric_gnsum(
      data = data,
      hidden_var = hidden_var,
      degree_var = degree_var,
      N_F = N_F,
      weights = weights,
      use_inclusion_probs = TRUE,
      verbose = TRUE
    )
    cat("\n")
  }

  # Compile comparison table
  comparison <- data.frame(
    estimator = names(results),
    N_H = sapply(results, function(x) x$N_H_estimate),
    prevalence_pct = sapply(results, function(x) x$prevalence_rate * 100),
    y_F_H = sapply(results, function(x) x$y_F_H),
    d_F_F = sapply(results, function(x) x$d_F_F),
    n_effective = sapply(results, function(x) x$n_effective),
    method = sapply(results, function(x) x$method),
    stringsAsFactors = FALSE
  )

  cat("=== Comparison Table ===\n")
  print(comparison)

  return(list(
    results = results,
    comparison = comparison
  ))
}

# ============================================================================
# NOTES FOR FUTURE IMPLEMENTATION
# ============================================================================

# TODO: Implement full Feehan & Salganik (2016) adjustments
#   - δ_F: degree ratio (hidden pop degree / frame pop degree)
#   - τ_F: true positive rate (reporting accuracy)
#   - η_F: precision (1 - false positive rate)
#
# Adjusted formula:
#   N_H_adj = N_H × (1/δ_F) × (1/τ_F) × η_F
#
# See: nsum_robust_adjustment.R for implementation

# TODO: Implement full symmetric GNSUM
#   - Account for reciprocity in network ties
#   - May require additional network data

# TODO: Add variance estimation
#   - Analytical variance formulas for MBSU
#   - Delta method for complex estimators

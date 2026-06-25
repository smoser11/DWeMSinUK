# nsum_bootstrap.R
# Bootstrap Confidence Intervals for NSUM Estimates
# Using survey bootstrap methods for complex RDS sampling
#
# Implements Rust & Rao rescaled bootstrap for RDS data
# Compatible with both core NSUM and robust NSUM with adjustments

cat("=== NSUM Bootstrap Confidence Intervals ===\n")
cat("Survey bootstrap for RDS-weighted NSUM estimates\n\n")

# Load required libraries
library(tidyverse)
library(here)
library(parallel)

# Check for surveybootstrap package
surveybootstrap_available <- requireNamespace("surveybootstrap", quietly = TRUE)
if (!surveybootstrap_available) {
  cat("WARNING: surveybootstrap package not available\n")
  cat("Install with: devtools::install_github('gregridgeway/surveybootstrap')\n")
  cat("Falling back to simple bootstrap methods\n\n")
}

# Source core NSUM and robust adjustment functions
source(here("R", "analysis", "05-nsum_estimation.R"))
source(here("R", "analysis", "nsum_robust_adjustment.R"))

# ============================================================================
# CONFIGURATION
# ============================================================================

bootstrap_config <- list(
  # Bootstrap parameters
  n_boot = 1000,
  confidence_level = 0.95,
  parallel_processing = TRUE,
  n_cores = 4,

  # Bootstrap method
  method = if (surveybootstrap_available) "survey" else "simple",

  # For survey bootstrap
  use_psu_var = "id",      # Primary sampling unit (individual level in RDS)
  use_strata_var = NULL,   # No stratification in basic RDS

  # Random seed
  seed = 12345
)

cat("Bootstrap configuration:\n")
cat("- Method:", bootstrap_config$method, "\n")
cat("- Bootstrap replications:", bootstrap_config$n_boot, "\n")
cat("- Confidence level:", bootstrap_config$confidence_level * 100, "%\n")
cat("- Parallel processing:", bootstrap_config$parallel_processing, "\n\n")

# ============================================================================
# SIMPLE BOOTSTRAP FOR NSUM
# ============================================================================

simple_nsum_bootstrap <- function(data,
                                  nsum_var,
                                  degree_vars,
                                  probe_sizes,
                                  weight_var = NULL,
                                  N_F = 980000,
                                  degree_ratio = 1.0,
                                  true_positive_rate = 1.0,
                                  precision = 1.0,
                                  n_boot = 1000,
                                  seed = 12345) {

  cat("Running simple bootstrap for", nsum_var, "with", n_boot, "replications...\n")

  set.seed(seed)

  # Get point estimate
  weights <- if (!is.null(weight_var) && weight_var %in% names(data)) {
    data[[weight_var]]
  } else {
    NULL
  }

  point_estimate <- estimate_nsum_population(
    data = data,
    weights = weights,
    hidden_connections_var = nsum_var,
    degree_vars = degree_vars,
    probe_sizes = probe_sizes,
    total_population_size = N_F,
    method = if (is.null(weights)) "basic" else "weighted",
    verbose = FALSE
  )

  if ("error" %in% names(point_estimate)) {
    return(list(
      estimate = NA,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = 0,
      error = point_estimate$error
    ))
  }

  # Bootstrap resampling
  boot_estimates <- numeric(n_boot)

  for (b in 1:n_boot) {
    # Simple bootstrap: resample with replacement
    boot_indices <- sample(1:nrow(data), replace = TRUE)
    boot_data <- data[boot_indices, ]

    # Get bootstrap weights if applicable
    boot_weights <- if (!is.null(weights)) {
      weights[boot_indices]
    } else {
      NULL
    }

    # Calculate bootstrap estimate
    boot_result <- tryCatch({
      estimate_nsum_population(
        data = boot_data,
        weights = boot_weights,
        hidden_connections_var = nsum_var,
        degree_vars = degree_vars,
        probe_sizes = probe_sizes,
        total_population_size = N_F,
        method = if (is.null(weights)) "basic" else "weighted",
        verbose = FALSE
      )
    }, error = function(e) {
      list(N_H_estimate = NA)
    })

    # Apply adjustments if not neutral
    if (!is.na(boot_result$N_H_estimate)) {
      boot_estimates[b] <- boot_result$N_H_estimate *
                          (1/degree_ratio) * (1/true_positive_rate) * precision
    } else {
      boot_estimates[b] <- NA
    }
  }

  # Remove NA values
  boot_estimates <- boot_estimates[!is.na(boot_estimates)]

  if (length(boot_estimates) < 10) {
    return(list(
      estimate = point_estimate$N_H_estimate * (1/degree_ratio) * (1/true_positive_rate) * precision,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = length(boot_estimates),
      error = "Insufficient valid bootstrap samples"
    ))
  }

  # Calculate confidence intervals
  alpha <- 1 - bootstrap_config$confidence_level
  ci_bounds <- quantile(boot_estimates, c(alpha/2, 1 - alpha/2), na.rm = TRUE)

  adjusted_estimate <- point_estimate$N_H_estimate *
                      (1/degree_ratio) * (1/true_positive_rate) * precision

  cat("Bootstrap completed:", length(boot_estimates), "valid estimates\n")

  return(list(
    estimate = adjusted_estimate,
    ci_lower = ci_bounds[1],
    ci_upper = ci_bounds[2],
    se = sd(boot_estimates, na.rm = TRUE),
    n_boot_valid = length(boot_estimates),
    boot_estimates = boot_estimates
  ))
}

# ============================================================================
# SURVEY BOOTSTRAP FOR NSUM (RESCALED BOOTSTRAP)
# ============================================================================

survey_nsum_bootstrap <- function(data,
                                  nsum_var,
                                  degree_vars,
                                  probe_sizes,
                                  weight_var,
                                  N_F = 980000,
                                  degree_ratio = 1.0,
                                  true_positive_rate = 1.0,
                                  precision = 1.0,
                                  n_boot = 1000,
                                  seed = 12345) {

  if (!surveybootstrap_available) {
    warning("surveybootstrap package not available, using simple bootstrap")
    return(simple_nsum_bootstrap(
      data, nsum_var, degree_vars, probe_sizes, weight_var,
      N_F, degree_ratio, true_positive_rate, precision, n_boot, seed
    ))
  }

  cat("Running survey bootstrap for", nsum_var, "with", n_boot, "replications...\n")

  set.seed(seed)

  # Validate required variables
  required_vars <- c(nsum_var, degree_vars, weight_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]

  if (length(missing_vars) > 0) {
    return(list(
      estimate = NA,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = 0,
      error = paste("Missing variables:", paste(missing_vars, collapse = ", "))
    ))
  }

  # Prepare data: keep only complete cases
  complete_cases <- complete.cases(data[, c(nsum_var, degree_vars, weight_var)])
  data_complete <- data[complete_cases, ]

  if (nrow(data_complete) < 10) {
    return(list(
      estimate = NA,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = 0,
      error = "Insufficient complete cases for bootstrap"
    ))
  }

  # Add PSU variable (each individual is a PSU in RDS)
  data_complete$psu_id <- 1:nrow(data_complete)

  # Calculate inclusion probabilities
  pi_i <- calculate_inclusion_probabilities(data_complete, weight_var, N_F)
  data_complete$survey_weight <- 1 / pi_i

  # Get point estimate first
  point_estimate <- estimate_nsum_population(
    data = data_complete,
    weights = data_complete[[weight_var]],
    hidden_connections_var = nsum_var,
    degree_vars = degree_vars,
    probe_sizes = probe_sizes,
    total_population_size = N_F,
    method = "weighted",
    verbose = FALSE
  )

  if ("error" %in% names(point_estimate)) {
    return(list(
      estimate = NA,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = 0,
      error = point_estimate$error
    ))
  }

  # Define survey design formula: weight ~ psu_vars
  survey_design <- survey_weight ~ psu_id

  # Draw bootstrap samples using rescaled bootstrap
  boot_samples <- tryCatch({
    surveybootstrap::rescaled.bootstrap.sample(
      survey.data = data_complete,
      survey.design = survey_design,
      num.reps = n_boot,
      parallel = bootstrap_config$parallel_processing
    )
  }, error = function(e) {
    cat("Survey bootstrap sampling failed:", e$message, "\n")
    cat("Falling back to simple bootstrap\n")
    return(NULL)
  })

  # Fall back to simple bootstrap if survey bootstrap fails
  if (is.null(boot_samples)) {
    return(simple_nsum_bootstrap(
      data_complete, nsum_var, degree_vars, probe_sizes, weight_var,
      N_F, degree_ratio, true_positive_rate, precision, n_boot, seed
    ))
  }

  cat("Generated", length(boot_samples), "bootstrap samples\n")

  # Calculate NSUM estimates for each bootstrap sample
  boot_estimates <- map_dbl(1:length(boot_samples), function(b) {

    boot_data_info <- boot_samples[[b]]

    # Validate bootstrap sample structure
    if (!"index" %in% names(boot_data_info)) {
      return(NA)
    }

    # Get bootstrap data
    boot_data <- data_complete[boot_data_info$index, ]

    # Apply bootstrap weight scaling
    if ("weight.scale" %in% names(boot_data_info)) {
      boot_weights_raw <- data_complete[[weight_var]][boot_data_info$index]
      boot_weights <- boot_weights_raw * boot_data_info$weight.scale
    } else {
      boot_weights <- data_complete[[weight_var]][boot_data_info$index]
    }

    # Calculate NSUM estimate
    boot_result <- tryCatch({
      estimate_nsum_population(
        data = boot_data,
        weights = boot_weights,
        hidden_connections_var = nsum_var,
        degree_vars = degree_vars,
        probe_sizes = probe_sizes,
        total_population_size = N_F,
        method = "weighted",
        verbose = FALSE
      )
    }, error = function(e) {
      return(list(N_H_estimate = NA))
    })

    if ("error" %in% names(boot_result) || is.na(boot_result$N_H_estimate)) {
      return(NA)
    }

    # Apply adjustments
    adjusted <- boot_result$N_H_estimate *
               (1/degree_ratio) * (1/true_positive_rate) * precision

    return(adjusted)
  })

  # Remove NA values
  boot_estimates <- boot_estimates[!is.na(boot_estimates)]

  if (length(boot_estimates) < 10) {
    return(list(
      estimate = point_estimate$N_H_estimate * (1/degree_ratio) * (1/true_positive_rate) * precision,
      ci_lower = NA,
      ci_upper = NA,
      se = NA,
      n_boot_valid = length(boot_estimates),
      error = "Insufficient valid bootstrap estimates"
    ))
  }

  # Calculate confidence intervals
  alpha <- 1 - bootstrap_config$confidence_level
  ci_bounds <- quantile(boot_estimates, c(alpha/2, 1 - alpha/2), na.rm = TRUE)

  adjusted_estimate <- point_estimate$N_H_estimate *
                      (1/degree_ratio) * (1/true_positive_rate) * precision

  cat("Survey bootstrap completed:", length(boot_estimates), "valid estimates\n")

  return(list(
    estimate = adjusted_estimate,
    ci_lower = ci_bounds[1],
    ci_upper = ci_bounds[2],
    se = sd(boot_estimates, na.rm = TRUE),
    n_boot_valid = length(boot_estimates),
    boot_estimates = boot_estimates
  ))
}

# ============================================================================
# UNIFIED BOOTSTRAP INTERFACE
# ============================================================================

calculate_nsum_bootstrap_ci <- function(data,
                                       nsum_var,
                                       degree_vars,
                                       probe_sizes,
                                       weight_var = NULL,
                                       N_F = 980000,
                                       degree_ratio = 1.0,
                                       true_positive_rate = 1.0,
                                       precision = 1.0,
                                       n_boot = bootstrap_config$n_boot,
                                       method = bootstrap_config$method,
                                       seed = bootstrap_config$seed) {

  cat("\n=== Bootstrap CI for", nsum_var, "===\n")
  cat("Method:", method, "| N_F:", format(N_F, big.mark = ","),
      "| Adjustments: δ =", degree_ratio, ", τ =", true_positive_rate,
      ", η =", precision, "\n")

  if (method == "survey" && !is.null(weight_var)) {
    result <- survey_nsum_bootstrap(
      data, nsum_var, degree_vars, probe_sizes, weight_var,
      N_F, degree_ratio, true_positive_rate, precision, n_boot, seed
    )
  } else {
    result <- simple_nsum_bootstrap(
      data, nsum_var, degree_vars, probe_sizes, weight_var,
      N_F, degree_ratio, true_positive_rate, precision, n_boot, seed
    )
  }

  # Display results
  if (!is.na(result$estimate) && !is.na(result$ci_lower)) {
    cat("Estimate:", format(round(result$estimate), big.mark = ","),
        "(", format(round(result$ci_lower), big.mark = ","), "-",
        format(round(result$ci_upper), big.mark = ","), ")\n")
  } else if ("error" %in% names(result)) {
    cat("Error:", result$error, "\n")
  }

  return(result)
}

# ============================================================================
# BOOTSTRAP CIS FOR MULTIPLE INDICATORS
# ============================================================================

calculate_nsum_bootstrap_multiple <- function(data = rd.dd,
                                              indicators = robust_nsum_config$indicators,
                                              weight_scheme = "ss_980k",
                                              N_F = 980000,
                                              adjustment_scenarios = list(
                                                neutral = list(delta = 1.0, tau = 1.0, eta = 1.0),
                                                conservative = list(delta = 0.75, tau = 0.7, eta = 0.95)
                                              ),
                                              n_boot = bootstrap_config$n_boot,
                                              save_results = TRUE) {

  cat("=== Bootstrap CIs for Multiple Indicators ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Scenarios:", length(adjustment_scenarios), "\n")
  cat("Total:", length(indicators) * length(adjustment_scenarios), "bootstrap analyses\n\n")

  # Get weight variable
  weight_var <- if (weight_scheme == "unweighted") {
    NULL
  } else {
    nsum_config$weighting_schemes[[weight_scheme]]
  }

  all_results <- list()
  result_counter <- 1

  for (indicator_name in names(indicators)) {

    indicator_info <- indicators[[indicator_name]]
    cat("\n=== ", indicator_name, " ===\n")

    for (scenario_name in names(adjustment_scenarios)) {

      scenario <- adjustment_scenarios[[scenario_name]]
      cat("  Scenario:", scenario_name, "\n")

      result <- calculate_nsum_bootstrap_ci(
        data = data,
        nsum_var = indicator_info$nsum_var,
        degree_vars = nsum_config$degree_vars,
        probe_sizes = nsum_config$probe_sizes,
        weight_var = weight_var,
        N_F = N_F,
        degree_ratio = scenario$delta,
        true_positive_rate = scenario$tau,
        precision = scenario$eta,
        n_boot = n_boot,
        method = bootstrap_config$method
      )

      # Add metadata
      result$indicator_name <- indicator_name
      result$scenario_name <- scenario_name
      result$weight_scheme <- weight_scheme
      result$N_F <- N_F

      all_results[[result_counter]] <- result
      result_counter <- result_counter + 1
    }
  }

  # Convert to data frame
  results_df <- map_dfr(all_results, function(r) {
    tibble(
      indicator_name = r$indicator_name,
      scenario = r$scenario_name,
      weight_scheme = r$weight_scheme,
      N_F = r$N_F,
      estimate = r$estimate,
      ci_lower = r$ci_lower,
      ci_upper = r$ci_upper,
      se = r$se,
      n_boot_valid = r$n_boot_valid,
      error = r$error %||% NA
    )
  })

  cat("\n=== Bootstrap Analysis Complete ===\n")
  print(results_df %>% select(indicator_name, scenario, estimate, ci_lower, ci_upper))

  # Save results
  if (save_results) {
    save(all_results, results_df, bootstrap_config,
         file = here("output", "nsum_bootstrap_results.RData"))
    cat("\nSaved: output/nsum_bootstrap_results.RData\n")

    write_csv(results_df, here("output", "tables", "nsum_bootstrap_ci.csv"))
    cat("Saved: output/tables/nsum_bootstrap_ci.csv\n")
  }

  return(list(
    all_results = all_results,
    results_df = results_df
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("\n=== Example: Bootstrap CI for single indicator ===\n")

if (exists("rd.dd")) {
  example_ci <- calculate_nsum_bootstrap_ci(
    data = rd.dd,
    nsum_var = "document_withholding_nsum",
    degree_vars = nsum_config$degree_vars,
    probe_sizes = nsum_config$probe_sizes,
    weight_var = "wt.SS_980k",
    N_F = 980000,
    degree_ratio = 1.0,
    true_positive_rate = 1.0,
    precision = 1.0,
    n_boot = 100,  # Reduced for example
    method = "simple"  # Use simple for speed in example
  )
} else {
  cat("Data 'rd.dd' not found. Load prepared data first.\n")
}

cat("\n=== To run bootstrap CIs for all indicators, use: ===\n")
cat("bootstrap_results <- calculate_nsum_bootstrap_multiple(rd.dd, n_boot = 1000)\n\n")

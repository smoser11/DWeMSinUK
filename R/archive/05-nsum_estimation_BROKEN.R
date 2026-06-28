# 05-nsum_estimation.R
# Network Scale-Up Method (NSUM) Core Estimation
# Domestic Worker Exploitation and Modern Slavery in UK
#
# Core NSUM estimation using basic and weighted methods
# For robust adjustments and bootstrap CIs, see nsum_robust_adjustment.R and nsum_bootstrap.R

cat("=== NSUM Core Estimation ===\n")
cat("Network Scale-Up Method for hidden population estimation\n")
cat("Using CE's comparable indicators and RDS weights\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(here)

# Source helper functions and configuration
source(here("R", "utils", "helper_functions.R"))
source(here("R", "config.R"))
global_config <- get_global_config()

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# CONFIGURATION
# ============================================================================

nsum_config <- list(
  # Focus on comparable indicators (NSUM versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,

  # Network size variable for NSUM respondents
  network_size_var = "known_network_size",  # Q13 2f: domestic workers with contact details

  # IMPORTANT: This study uses RDS+NSUM hybrid design (Modified Basic Scale-Up)
  # We do NOT have traditional probe questions about known populations
  # Instead, we use direct network size (q13 = known_network_size)

  # Degree variable: respondent's self-reported network size
  degree_var = "known_network_size",  # Q13: number of domestic workers known with contact details

  # For modified basic scale-up, we use the RDS network size directly
  # d_F,F is estimated as the mean of known_network_size (weighted by RDS weights)
  use_direct_network_size = TRUE,  # Use direct network size, not probe scaling

  # Population scenarios for hidden population (domestic workers)
  total_population_sizes = global_config$population_sizes,
  preferred_population_size = global_config$main_population_size,  # 980,000 (EU baseline)

  # RDS weighting schemes available in prepared data
  weighting_schemes = list(
    "unweighted" = NULL,
    "rds_I" = "wt.RDS1_",  # Variable-specific RDS-I weights
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k",
    "vh_050k" = "wt.vh_050k",
    "vh_1740k" = "wt.vh_1740k",
    "ss_980k" = "wt.SS_980k",
    "ss_100k" = "wt.SS_100k",
    "ss_050k" = "wt.SS_050k",
    "ss_1740k" = "wt.SS_1740k"
  ),

  # Analysis options
  use_rds_weights = TRUE,
  preferred_method = global_config$preferred_nsum_method,  # "weighted"
  create_comparison_with_rds = TRUE,

  # Output options
  save_detailed_results = TRUE,
  save_intermediate = FALSE
)

# Validate probe sizes
if (!nsum_config$probe_sizes_validated) {
  warning(
    "NSUM probe_sizes are PLACEHOLDERS and must be replaced with actual UK population data.\n",
    "Set nsum_config$probe_sizes_validated = TRUE after updating probe_sizes with real values.\n",
    "Current NSUM estimates will be unreliable until probe sizes are corrected."
  )
}

cat("NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n")
cat("- Network probes:", length(nsum_config$degree_vars), "reference groups\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Preferred method:", nsum_config$preferred_method, "\n")
cat("- Preferred population:", format(nsum_config$preferred_population_size, big.mark = ","), "\n")
cat("- Probe sizes validated:", nsum_config$probe_sizes_validated, "\n\n")

# ============================================================================
# INPUT VALIDATION FUNCTIONS
# ============================================================================

validate_nsum_variables <- function(data, hidden_var, degree_var) {

  missing_vars <- character(0)

  # Check hidden population variable (alter-centric)
  if (!(hidden_var %in% names(data))) {
    missing_vars <- c(missing_vars, hidden_var)
  }

  # Check network size variable (degree variable)
  if (!(degree_var %in% names(data))) {
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

# ============================================================================
# CORE NSUM ESTIMATION FUNCTION (Modified Basic Scale-Up for RDS+NSUM)
# ============================================================================
#
# This implements the "Modified Basic Scale-Up" estimator for RDS+NSUM design
# Following Feehan & Salganik (2016) but adapted for RDS sampling
#
# Formula: N_H = (y_F,H / d_F,F) × N_F
# Where:
#   y_F,H = average alter reports (how many in hidden pop does frame know)
#   d_F,F = average network size within frame (self-reported)
#   N_F = total frame population size

estimate_nsum_population <- function(data,
                                     weights = NULL,
                                     hidden_connections_var,
                                     degree_var = "known_network_size",
                                     total_population_size = 980000,
                                     method = "basic",
                                     weighting_scheme = "unweighted",
                                     verbose = TRUE) {

  # Validate inputs
  var_check <- validate_nsum_variables(data, hidden_connections_var, degree_var)
  if (!var_check$valid) {
    return(list(
      N_H_estimate = NA,
      error = var_check$error,
      missing_variables = var_check$missing_vars
    ))
  }

  # Handle weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
    if (verbose) cat("  Using unweighted estimation\n")
  } else {
    if (verbose) cat("  Using", weighting_scheme, "weights\n")
  }

  # Validate weights
  if (length(weights) != nrow(data)) {
    warning("Weight vector length doesn't match data rows. Using unweighted.")
    weights <- rep(1, nrow(data))
  }

  # Remove missing values
  complete_cases <- complete.cases(data[[hidden_connections_var]], weights)
  data_complete <- data[complete_cases, ]
  weights_complete <- weights[complete_cases]

  if (nrow(data_complete) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No complete cases available"
    ))
  }

  if (verbose) {
    cat("  Complete cases:", nrow(data_complete), "of", nrow(data), "\n")
  }

  # ========================================================================
  # STEP 1: Calculate weighted average out-reports (y_F,H)
  # ========================================================================

  y_connections <- data_complete[[hidden_connections_var]]

  if (method == "weighted" && !is.null(weights)) {
    # Weighted estimate: sum(y_i * w_i) / sum(w_i)
    y_F_H <- sum(y_connections * weights_complete, na.rm = TRUE) /
             sum(weights_complete, na.rm = TRUE)
  } else {
    # Unweighted estimate: mean(y_i)
    y_F_H <- mean(y_connections, na.rm = TRUE)
  }

  # ========================================================================
  # STEP 2: Calculate average network size (d_F,F) using probe questions
  # ========================================================================

  network_sizes <- list()

  for (i in seq_along(degree_vars)) {
    degree_var <- degree_vars[i]
    probe_size <- probe_sizes[i]

    if (degree_var %in% names(data_complete)) {

      # Get probe responses
      probe_responses <- data_complete[[degree_var]]
      valid_responses <- !is.na(probe_responses)

      if (sum(valid_responses) == 0) {
        warning("No valid responses for probe variable: ", degree_var)
        next
      }

      if (method == "weighted" && !is.null(weights)) {
        # Weighted average probe responses
        avg_connections <- sum(probe_responses[valid_responses] *
                               weights_complete[valid_responses], na.rm = TRUE) /
                          sum(weights_complete[valid_responses], na.rm = TRUE)
      } else {
        # Unweighted average
        avg_connections <- mean(probe_responses, na.rm = TRUE)
      }

      # Scale to estimate personal network size using known population
      # d_F,F = (average_connections_to_probe / probe_population_size) × total_population
      network_size_estimate <- (avg_connections / probe_size) * total_population_size

      network_sizes[[degree_var]] <- list(
        avg_connections = avg_connections,
        probe_size = probe_size,
        network_size_estimate = network_size_estimate
      )
    } else {
      warning("Probe variable not found in data: ", degree_var)
    }
  }

  # Average across all valid probes
  valid_network_estimates <- sapply(network_sizes, function(x) x$network_size_estimate)
  valid_network_estimates <- valid_network_estimates[is.finite(valid_network_estimates)]

  if (length(valid_network_estimates) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No valid network size estimates from probe questions"
    ))
  }

  d_F_F <- mean(valid_network_estimates)

  # ========================================================================
  # STEP 3: Calculate NSUM estimate
  # ========================================================================

  # Basic NSUM formula: N_H = (y_F,H / d_F,F) × N_F
  # Where:
  #   y_F,H = average number of hidden population members known per respondent
  #   d_F_F = average personal network size
  #   N_F = total frame population size

  N_H_estimate <- (y_F_H / d_F_F) * total_population_size

  # ========================================================================
  # STEP 4: Return comprehensive results
  # ========================================================================

  result <- list(
    # Main estimate
    N_H_estimate = N_H_estimate,
    prevalence_rate = N_H_estimate / total_population_size,

    # NSUM components
    y_F_H = y_F_H,
    d_F_F = d_F_F,

    # Metadata
    total_population_size = total_population_size,
    sample_size = nrow(data_complete),
    n_missing = nrow(data) - nrow(data_complete),

    # Network size details
    network_size_estimates = network_sizes,
    n_probes_used = length(valid_network_estimates),

    # Configuration
    method = method,
    weighting_scheme = weighting_scheme,
    hidden_variable = hidden_connections_var,
    degree_variables = degree_vars,
    probe_sizes = probe_sizes
  )

  return(result)
}

# ============================================================================
# WEIGHT EXTRACTION HELPER
# ============================================================================

get_weights_for_scheme <- function(data, scheme_name, outcome_var = NULL) {

  if (scheme_name == "unweighted") {
    return(NULL)
  }

  # Variable-specific RDS-I weights
  if (scheme_name == "rds_I" && !is.null(outcome_var)) {
    # Convert NSUM variable to RDS variable name
    rds_var <- gsub("_nsum", "_rds", outcome_var)
    weight_var <- paste0("wt.RDS1_", rds_var)

    if (weight_var %in% names(data)) {
      return(data[[weight_var]])
    } else {
      warning("RDS-I weight variable not found: ", weight_var, ". Using unweighted.")
      return(NULL)
    }
  }

  # Population-based weights (VH, SS)
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]

  if (!is.null(weight_var) && weight_var %in% names(data)) {
    return(data[[weight_var]])
  }

  warning("Weight variable not found: ", weight_var, ". Using unweighted.")
  return(NULL)
}

# ============================================================================
# COMPREHENSIVE NSUM ESTIMATION ACROSS SCENARIOS
# ============================================================================

run_nsum_estimation <- function(outcome_vars = nsum_config$outcome_vars,
                                population_sizes = nsum_config$total_population_sizes,
                                method = nsum_config$preferred_method,
                                weighting_schemes = c("unweighted", "ss_980k")) {

  cat("=== Comprehensive NSUM Estimation ===\n")
  cat("Indicators:", length(outcome_vars), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Weighting schemes:", length(weighting_schemes), "\n")
  cat("Total combinations:", length(outcome_vars) * length(population_sizes) *
      length(weighting_schemes), "\n\n")

  all_results <- list()

  for (outcome_var in outcome_vars) {

    cat("\nProcessing:", outcome_var, "\n")

    if (!(outcome_var %in% names(rd.dd))) {
      warning("Variable ", outcome_var, " not found in data")
      next
    }

    var_results <- list()

    for (pop_size in population_sizes) {

      cat("  Population size:", format(pop_size, big.mark = ","), "\n")

      pop_results <- list()

      for (scheme in weighting_schemes) {

        # Get weights for this scheme
        weights <- get_weights_for_scheme(rd.dd, scheme, outcome_var)

        tryCatch({

          result <- estimate_nsum_population(
            data = rd.dd,
            weights = weights,
            hidden_connections_var = outcome_var,
            degree_vars = nsum_config$degree_vars,
            probe_sizes = nsum_config$probe_sizes,
            total_population_size = pop_size,
            method = method,
            weighting_scheme = scheme,
            verbose = FALSE
          )

          # Add metadata
          result$variable = outcome_var
          result$population_size = pop_size
          result$timestamp = Sys.time()

          pop_results[[scheme]] <- result

          if (!is.na(result$N_H_estimate)) {
            cat("    ", scheme, ":", format(round(result$N_H_estimate), big.mark = ","), "\n")
          } else {
            cat("    ", scheme, ": Error -", result$error, "\n")
          }

        }, error = function(e) {
          cat("    ", scheme, ": Error -", e$message, "\n")
          pop_results[[scheme]] <- list(
            variable = outcome_var,
            population_size = pop_size,
            weighting_scheme = scheme,
            error = e$message
          )
        })
      }

      var_results[[as.character(pop_size)]] <- pop_results
    }

    all_results[[outcome_var]] <- var_results
  }

  cat("\nNSUM estimation completed\n\n")
  return(all_results)
}

# ============================================================================
# EXTRACT PREFERRED RESULTS FOR MAIN TEXT
# ============================================================================

extract_preferred_nsum_results <- function(nsum_results,
                                          preferred_method = "weighted",
                                          main_pop_size = 980000,
                                          preferred_scheme = "ss_980k") {

  cat("=== Extracting Preferred NSUM Results ===\n")
  cat("Method:", preferred_method, "| Population:", format(main_pop_size, big.mark = ","),
      "| Scheme:", preferred_scheme, "\n")

  preferred_results <- list()
  comparable_indicators <- get_comparable_indicators()

  for (outcome_var in names(nsum_results)) {

    pop_results <- nsum_results[[outcome_var]][[as.character(main_pop_size)]]

    if (is.null(pop_results)) next

    result <- pop_results[[preferred_scheme]]

    if (is.null(result)) next

    if (!"error" %in% names(result) && !is.na(result$N_H_estimate)) {

      # Get indicator label
      rds_var <- gsub("_nsum", "_rds", outcome_var)
      indicator_label <- comparable_indicators$labels[[rds_var]]
      confidence_level <- comparable_indicators$confidence_levels[[rds_var]]

      preferred_results[[outcome_var]] <- list(
        estimate = result$prevalence_rate,
        absolute_estimate = result$N_H_estimate,
        variable_label = indicator_label,
        confidence_level = confidence_level,
        population_size = main_pop_size,
        method = preferred_method,
        weighting_scheme = preferred_scheme,

        # NSUM components
        y_F_H = result$y_F_H,
        d_F_F = result$d_F_F,
        sample_size = result$sample_size,
        n_probes_used = result$n_probes_used
      )
    }
  }

  # Create summary table
  main_table <- data.frame(
    indicator = names(preferred_results),
    estimate = sapply(preferred_results, function(x) x$estimate),
    absolute_estimate = sapply(preferred_results, function(x) x$absolute_estimate),
    confidence = sapply(preferred_results, function(x) x$confidence_level),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estimate_pct = estimate * 100,
      indicator_clean = sapply(indicator, function(x) {
        rds_var <- gsub("_nsum", "_rds", x)
        get_comparable_indicators()$labels[[rds_var]]
      }),
      absolute_formatted = format(round(absolute_estimate), big.mark = ",")
    ) %>%
    arrange(desc(estimate))

  cat("Extracted preferred results for", nrow(main_table), "indicators\n\n")

  return(list(
    results = preferred_results,
    main_table = main_table
  ))
}

# ============================================================================
# RDS VS NSUM COMPARISON
# ============================================================================

create_rds_nsum_comparison <- function(nsum_results,
                                      rds_results_file = NULL) {

  cat("=== Creating RDS vs NSUM Comparison ===\n")

  # Load RDS results
  if (is.null(rds_results_file)) {
    rds_results_file <- here("output", "rds_estimation_results.RData")
  }

  if (!file.exists(rds_results_file)) {
    warning("RDS results not found at: ", rds_results_file)
    return(NULL)
  }

  load(rds_results_file)  # Should load 'final_results'

  if (!exists("final_results")) {
    warning("final_results object not found in RDS results file")
    return(NULL)
  }

  rds_main_table <- final_results$preferred_results$main_table

  # Match indicators
  comparison_table <- data.frame()

  for (i in 1:nrow(rds_main_table)) {

    rds_indicator <- rds_main_table$indicator[i]
    nsum_indicator <- gsub("_rds", "_nsum", rds_indicator)

    if (nsum_indicator %in% names(nsum_results$results)) {

      rds_est <- rds_main_table$estimate[i]
      nsum_est <- nsum_results$results[[nsum_indicator]]$estimate

      comparison_row <- data.frame(
        indicator = rds_main_table$indicator_clean[i],
        rds_estimate = rds_est,
        nsum_estimate = nsum_est,
        difference = nsum_est - rds_est,
        ratio = nsum_est / rds_est,

        rds_estimate_pct = rds_est * 100,
        nsum_estimate_pct = nsum_est * 100,
        difference_pct = (nsum_est - rds_est) * 100,

        confidence_level = rds_main_table$confidence[i],

        stringsAsFactors = FALSE
      )

      comparison_table <- rbind(comparison_table, comparison_row)
    }
  }

  if (nrow(comparison_table) > 0) {
    comparison_table <- comparison_table[order(-comparison_table$rds_estimate), ]
  }

  cat("Created comparison table for", nrow(comparison_table), "indicators\n\n")

  return(comparison_table)
}

# ============================================================================
# MAIN NSUM ANALYSIS FUNCTION
# ============================================================================

main_nsum_analysis <- function() {

  setup_project_environment()

  cat("Starting NSUM analysis for comparable indicators...\n\n")

  # Step 1: Run NSUM estimation
  nsum_results <- run_nsum_estimation(
    outcome_vars = nsum_config$outcome_vars,
    population_sizes = nsum_config$total_population_sizes,
    method = nsum_config$preferred_method,
    weighting_schemes = c("unweighted", "ss_980k", "ss_100k", "ss_050k", "ss_1740k")
  )

  # Step 2: Extract preferred results (main text)
  preferred_results <- extract_preferred_nsum_results(
    nsum_results = nsum_results,
    preferred_method = nsum_config$preferred_method,
    main_pop_size = nsum_config$preferred_population_size,
    preferred_scheme = "ss_980k"
  )

  # Step 3: Create comparison with RDS (if available)
  if (nsum_config$create_comparison_with_rds) {
    comparison_table <- create_rds_nsum_comparison(
      nsum_results = preferred_results
    )
  } else {
    comparison_table <- NULL
  }

  # Step 4: Compile final results
  final_results <- list(
    nsum_results = nsum_results,
    preferred_results = preferred_results,
    rds_nsum_comparison = comparison_table,

    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      preferred_method = nsum_config$preferred_method,
      main_population_size = nsum_config$preferred_population_size,
      probe_sizes_validated = nsum_config$probe_sizes_validated
    )
  )

  # Save results
  save(final_results, file = here("output", "nsum_estimation_results.RData"))
  cat("Saved: output/nsum_estimation_results.RData\n")

  # Save main table
  if (nsum_config$save_detailed_results) {
    write.csv(preferred_results$main_table,
              here("output", "tables", "nsum_main_results.csv"),
              row.names = FALSE)
    cat("Saved: output/tables/nsum_main_results.csv\n")

    if (!is.null(comparison_table)) {
      write.csv(comparison_table,
                here("output", "tables", "rds_nsum_comparison.csv"),
                row.names = FALSE)
      cat("Saved: output/tables/rds_nsum_comparison.csv\n")
    }
  }

  # Display results
  cat("\n=== NSUM Analysis Complete ===\n")
  cat("Main results (", nsum_config$preferred_method, "):\n")
  print(preferred_results$main_table[c("indicator_clean", "estimate_pct", "absolute_formatted")])

  if (!is.null(comparison_table)) {
    cat("\nRDS vs NSUM comparison:\n")
    print(comparison_table[c("indicator", "rds_estimate_pct", "nsum_estimate_pct", "difference_pct")])
  }

  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {

  # Check if running from pipeline
  if (exists("pipeline_config")) {
    cat("Running as part of main pipeline\n")
  } else {
    cat("Running standalone NSUM analysis\n")
  }

  # Run analysis
  nsum_results <- main_nsum_analysis()

} else {
  cat("NSUM core estimation script loaded (execution skipped)\n")
}

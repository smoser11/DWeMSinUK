# 05-nsum_estimation.R
# Network Scale-Up Method (NSUM) - RDS+NSUM Hybrid Design
# Domestic Worker Exploitation and Modern Slavery in UK
#
# Modified Basic Scale-Up estimator for RDS sampling with alter reports
# NO traditional probe questions - uses direct network size from RDS

cat("=== NSUM Estimation (RDS+NSUM Hybrid) ===\n")
cat("Modified Basic Scale-Up Method\n")
cat("Using direct network size from RDS sampling\n\n")

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
  # Comparable indicators (NSUM alter-report versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,
  
  # Network size variable (direct from RDS)
  degree_var = "known_network_size",  # Q13: number of domestic workers known
  
  # Population scenarios
  total_population_sizes = global_config$population_sizes,
  preferred_population_size = global_config$main_population_size,
  
  # RDS weighting schemes
  weighting_schemes = list(
    "unweighted" = NULL,
    "ss_980k" = "wt.SS_980k",
    "ss_100k" = "wt.SS_100k",
    "ss_050k" = "wt.SS_050k",
    "ss_1740k" = "wt.SS_1740k",
    "vh_980k" = "wt.vh_980k",
    "vh_100k" = "wt.vh_100k",
    "vh_050k" = "wt.vh_050k",
    "vh_1740k" = "wt.vh_1740k"
  ),
  
  # Analysis options
  use_rds_weights = TRUE,
  preferred_method = global_config$preferred_nsum_method,
  create_comparison_with_rds = TRUE,
  
  # Output options
  save_detailed_results = TRUE
)

cat("NSUM configuration:\n")
cat("- Indicators:", length(nsum_config$outcome_vars), "\n")
cat("- Degree variable:", nsum_config$degree_var, "\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "\n")
cat("- Preferred method:", nsum_config$preferred_method, "\n\n")

# ============================================================================
# INPUT VALIDATION
# ============================================================================

validate_nsum_variables <- function(data, hidden_var, degree_var) {
  missing_vars <- character(0)
  
  if (!(hidden_var %in% names(data))) {
    missing_vars <- c(missing_vars, hidden_var)
  }
  
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
# CORE NSUM ESTIMATION (Modified Basic Scale-Up)
# ============================================================================

estimate_nsum_population <- function(data,
                                     weights = NULL,
                                     hidden_connections_var,
                                     degree_var = "known_network_size",
                                     total_population_size = 980000,
                                     method = "basic",
                                     weighting_scheme = "unweighted",
                                     verbose = TRUE) {
  
  # Validate
  var_check <- validate_nsum_variables(data, hidden_connections_var, degree_var)
  if (!var_check$valid) {
    return(list(N_H_estimate = NA, error = var_check$error))
  }
  
  # Handle weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
    if (verbose) cat("  Unweighted\n")
  } else {
    if (verbose) cat("  Weighted (", weighting_scheme, ")\n", sep="")
  }
  
  if (length(weights) != nrow(data)) {
    warning("Weight length mismatch. Using unweighted.")
    weights <- rep(1, nrow(data))
  }
  
  # Remove missing
  complete_cases <- complete.cases(data[[hidden_connections_var]], 
                                   data[[degree_var]], weights)
  data_c <- data[complete_cases, ]
  weights_c <- weights[complete_cases]
  
  if (nrow(data_c) == 0) {
    return(list(N_H_estimate = NA, error = "No complete cases"))
  }
  
  if (verbose) cat("  Complete:", nrow(data_c), "/", nrow(data), "\n")
  
  # Calculate y_F,H (alter reports)
  y_conn <- data_c[[hidden_connections_var]]
  if (method == "weighted") {
    y_FH <- sum(y_conn * weights_c, na.rm=TRUE) / sum(weights_c, na.rm=TRUE)
  } else {
    y_FH <- mean(y_conn, na.rm=TRUE)
  }
  
  # Calculate d_F,F (network size)
  net_sizes <- data_c[[degree_var]]
  if (method == "weighted") {
    d_FF <- sum(net_sizes * weights_c, na.rm=TRUE) / sum(weights_c, na.rm=TRUE)
  } else {
    d_FF <- mean(net_sizes, na.rm=TRUE)
  }
  
  if (d_FF <= 0) {
    return(list(N_H_estimate = NA, error = "Network size <= 0"))
  }
  
  # NSUM estimate: N_H = (y_F,H / d_F,F) × N_F
  N_H <- (y_FH / d_FF) * total_population_size
  
  return(list(
    N_H_estimate = N_H,
    prevalence_rate = N_H / total_population_size,
    y_F_H = y_FH,
    d_F_F = d_FF,
    total_population_size = total_population_size,
    sample_size = nrow(data_c),
    n_missing = nrow(data) - nrow(data_c),
    method = method,
    weighting_scheme = weighting_scheme,
    hidden_variable = hidden_connections_var,
    degree_variable = degree_var
  ))
}

# ============================================================================
# WEIGHT EXTRACTION
# ============================================================================

get_weights_for_scheme <- function(data, scheme_name) {
  if (scheme_name == "unweighted") return(NULL)
  
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    return(data[[weight_var]])
  }
  
  warning("Weight variable not found: ", weight_var)
  return(NULL)
}

# ============================================================================
# COMPREHENSIVE NSUM ESTIMATION
# ============================================================================

run_nsum_estimation <- function(outcome_vars = nsum_config$outcome_vars,
                                population_sizes = nsum_config$total_population_sizes,
                                method = nsum_config$preferred_method,
                                weighting_schemes = c("unweighted", "ss_980k")) {
  
  cat("=== Running NSUM Estimation ===\n")
  cat("Indicators:", length(outcome_vars), "\n")
  cat("Populations:", length(population_sizes), "\n")
  cat("Schemes:", length(weighting_schemes), "\n\n")
  
  all_results <- list()
  
  for (outcome_var in outcome_vars) {
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(rd.dd))) {
      warning("Variable not found: ", outcome_var)
      next
    }
    
    var_results <- list()
    
    for (pop_size in population_sizes) {
      pop_results <- list()
      
      for (scheme in weighting_schemes) {
        weights <- get_weights_for_scheme(rd.dd, scheme)
        
        result <- tryCatch({
          estimate_nsum_population(
            data = rd.dd,
            weights = weights,
            hidden_connections_var = outcome_var,
            degree_var = nsum_config$degree_var,
            total_population_size = pop_size,
            method = if (is.null(weights)) "basic" else "weighted",
            weighting_scheme = scheme,
            verbose = FALSE
          )
        }, error = function(e) {
          list(N_H_estimate = NA, error = e$message)
        })
        
        result$variable <- outcome_var
        result$population_size <- pop_size
        result$timestamp <- Sys.time()
        
        pop_results[[scheme]] <- result
        
        if (!is.na(result$N_H_estimate)) {
          cat("  ", pop_size, scheme, ":", format(round(result$N_H_estimate), big.mark=","), "\n")
        }
      }
      
      var_results[[as.character(pop_size)]] <- pop_results
    }
    
    all_results[[outcome_var]] <- var_results
  }
  
  cat("\nNSUM estimation complete\n\n")
  return(all_results)
}

# ============================================================================
# EXTRACT PREFERRED RESULTS
# ============================================================================

extract_preferred_nsum_results <- function(nsum_results,
                                          preferred_method = "weighted",
                                          main_pop_size = 980000,
                                          preferred_scheme = "ss_980k") {
  
  cat("=== Extracting Preferred Results ===\n")
  
  preferred_results <- list()
  comparable_indicators <- get_comparable_indicators()
  
  for (outcome_var in names(nsum_results)) {
    pop_results <- nsum_results[[outcome_var]][[as.character(main_pop_size)]]
    
    if (is.null(pop_results)) next
    
    result <- pop_results[[preferred_scheme]]
    
    if (is.null(result)) next
    if ("error" %in% names(result) && !is.na(result$error)) next
    if (is.na(result$N_H_estimate)) next
    
    # Get base variable name (without _rds or _nsum suffix)
    base_var <- gsub("_nsum$", "", outcome_var)

    preferred_results[[outcome_var]] <- list(
      estimate = result$prevalence_rate,
      absolute_estimate = result$N_H_estimate,
      variable_label = comparable_indicators$labels[[base_var]],
      confidence_level = comparable_indicators$confidence_levels[[base_var]],
      population_size = main_pop_size,
      method = preferred_method,
      weighting_scheme = preferred_scheme,
      y_F_H = result$y_F_H,
      d_F_F = result$d_F_F,
      sample_size = result$sample_size
    )
  }
  
  if (length(preferred_results) == 0) {
    cat("No valid results found\n")
    return(list(results = list(), main_table = data.frame()))
  }
  
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
        base_var <- gsub("_nsum$", "", x)
        get_comparable_indicators()$labels[[base_var]]
      }),
      absolute_formatted = format(round(absolute_estimate), big.mark = ",")
    ) %>%
    arrange(desc(estimate))
  
  cat("Extracted", nrow(main_table), "valid results\n\n")
  
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
  
  if (is.null(rds_results_file)) {
    rds_results_file <- here("output", "rds_estimation_results.RData")
  }
  
  if (!file.exists(rds_results_file)) {
    warning("RDS results not found")
    return(NULL)
  }
  
  load(rds_results_file)
  
  if (!exists("final_results")) {
    warning("final_results not found in RDS file")
    return(NULL)
  }
  
  rds_main_table <- final_results$preferred_results$main_table
  
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
  
  cat("Created comparison for", nrow(comparison_table), "indicators\n\n")
  return(comparison_table)
}

# ============================================================================
# MAIN ANALYSIS FUNCTION
# ============================================================================

main_nsum_analysis <- function() {
  setup_project_environment()
  
  cat("Starting NSUM analysis...\n\n")
  
  # Run NSUM estimation
  nsum_results <- run_nsum_estimation(
    outcome_vars = nsum_config$outcome_vars,
    population_sizes = nsum_config$total_population_sizes,
    method = nsum_config$preferred_method,
    weighting_schemes = c("unweighted", "ss_980k", "ss_100k", "ss_050k", "ss_1740k")
  )
  
  # Extract preferred results
  preferred_results <- extract_preferred_nsum_results(
    nsum_results = nsum_results,
    preferred_method = nsum_config$preferred_method,
    main_pop_size = nsum_config$preferred_population_size,
    preferred_scheme = "ss_980k"
  )
  
  # Create comparison
  if (nsum_config$create_comparison_with_rds) {
    comparison_table <- create_rds_nsum_comparison(
      nsum_results = preferred_results
    )
  } else {
    comparison_table <- NULL
  }
  
  # Compile results
  final_results <- list(
    nsum_results = nsum_results,
    preferred_results = preferred_results,
    rds_nsum_comparison = comparison_table,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      preferred_method = nsum_config$preferred_method,
      main_population_size = nsum_config$preferred_population_size
    )
  )
  
  # Save results
  save(final_results, file = here("output", "nsum_estimation_results.RData"))
  cat("Saved: output/nsum_estimation_results.RData\n")
  
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
  if (nrow(preferred_results$main_table) > 0) {
    cat("Main results:\n")
    print(preferred_results$main_table[c("indicator_clean", "estimate_pct", "absolute_formatted")])
  }
  
  if (!is.null(comparison_table) && nrow(comparison_table) > 0) {
    cat("\nRDS vs NSUM:\n")
    print(comparison_table[c("indicator", "rds_estimate_pct", "nsum_estimate_pct", "difference_pct")])
  }
  
  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

if (!exists("skip_execution") || !skip_execution) {
  if (exists("pipeline_config")) {
    cat("Running from pipeline\n")
  } else {
    cat("Running standalone\n")
  }
  
  nsum_results <- main_nsum_analysis()
} else {
  cat("NSUM script loaded (execution skipped)\n")
}

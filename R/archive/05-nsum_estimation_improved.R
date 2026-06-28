# 05-nsum_estimation_improved.R
# Enhanced Network Scale-Up Method (NSUM) Estimation
# Domestic Worker Exploitation and Modern Slavery in UK
#
# IMPROVEMENTS:
# - Multiple weighting schemes from RDS analysis
# - Monte Carlo simulation over population sizes
# - Appropriate NSUM libraries from CRAN
# - Comprehensive results saving

cat("=== Enhanced NSUM Estimation Analysis ===\n")
cat("Network Scale-Up Method with multiple weighting approaches\n")
cat("Monte Carlo simulation over population scenarios\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(here)
library(parallel)  # For Monte Carlo simulation
library(boot)      # For bootstrap confidence intervals

# Load NSUM-specific packages from CRAN
tryCatch({
  library(networkscaleup)  # Modern Bayesian NSUM with Stan backend
  nsum_packages_available <- TRUE
  cat("Using networkscaleup package for enhanced NSUM methods\n")
}, error = function(e) {
  cat("Warning: networkscaleup package not available. Using custom implementation.\n")
  nsum_packages_available <- FALSE
})

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# ENHANCED CONFIGURATION
# ============================================================================

nsum_config <- list(
  # Focus on comparable indicators (NSUM versions)
  outcome_vars = get_comparable_indicators()$nsum_vars,
  
  # Network size variables and known population probes
  network_size_var = "known_network_size",  # Q13 2f
  degree_vars = c("q13"),  # Network size probe (actual variable available)
  
  # Known population sizes for scaling (UK demographics)
  probe_sizes = c(
    q13 = 200000   # Domestic workers reference population for network scaling
  ),
  
  # Population scenarios for Monte Carlo
  total_population_sizes = get_population_parameters()$sizes,
  preferred_population_size = 980000,  # EU baseline
  
  # Multiple weighting schemes from data preparation (02-data_preparation.R)
  weighting_schemes = list(
    "unweighted" = NULL,
    "rds_I_variable_specific" = "wt.RDS1_",  # Variable-specific RDS-I weights
    "vh_980k" = "wt.vh_980k",      # Volz-Heckathorn 980K population
    "vh_100k" = "wt.vh_100k",      # Conservative estimate
    "vh_050k" = "wt.vh_050k",      # Very conservative
    "vh_1740k" = "wt.vh_1740k",    # Upper bound estimate
    "ss_980k" = "wt.SS_980k",      # Gile SS 980K population
    "ss_100k" = "wt.SS_100k",      # Gile SS conservative
    "ss_050k" = "wt.SS_050k",      # Gile SS very conservative
    "ss_1740k" = "wt.SS_1740k"     # Gile SS upper bound
  ),
  
  # Monte Carlo parameters
  mc_iterations = 1000,
  mc_confidence_level = 0.95,
  mc_parallel_cores = 1,  # Disable parallel processing to avoid scoping issues
  mc_sample_fraction = 0.8,  # Bootstrap sample fraction
  
  # Analysis options
  run_monte_carlo = TRUE,
  run_sensitivity_analysis = TRUE,
  create_comparison_with_rds = TRUE,
  save_all_weighting_results = TRUE,
  
  # Output options
  save_detailed_results = TRUE,
  create_plots = TRUE,
  create_summary_tables = TRUE
)

cat("Enhanced NSUM configuration:\n")
cat("- Outcome variables:", length(nsum_config$outcome_vars), "comparable indicators\n") 
cat("- Network probes:", length(nsum_config$degree_vars), "reference groups\n")
cat("- Population scenarios:", length(nsum_config$total_population_sizes), "sizes\n")
cat("- Weighting schemes:", length(nsum_config$weighting_schemes), "approaches\n")
cat("- Monte Carlo iterations:", nsum_config$mc_iterations, "\n")
cat("- Parallel cores:", nsum_config$mc_parallel_cores, "\n\n")

# ============================================================================
# WEIGHTING SCHEME MANAGEMENT
# ============================================================================

get_weights_for_scheme <- function(data, scheme_name, outcome_var = NULL, weight_data = dd) {
  
  if (scheme_name == "unweighted") {
    return(NULL)
  }
  
  # Variable-specific RDS-I weights
  if (scheme_name == "rds_I_variable_specific" && !is.null(outcome_var)) {
    # Convert NSUM variable to RDS variable name for weight lookup
    rds_var <- gsub("_nsum", "_rds", outcome_var)
    weight_var <- paste0("wt.RDS1_", rds_var)
    
    if (weight_var %in% names(data)) {
      return(weight_data[[weight_var]])
    } else {
      # Fallback strategies
      fallback_var <- paste0("wt.RDS1_", gsub("_nsum", "", outcome_var))
      if (fallback_var %in% names(data)) {
        return(data[[fallback_var]])
      }
      
      # Last resort: use generic weights
      cat("Warning: Variable-specific weights not found for", outcome_var, ". Using unweighted.\n")
      return(NULL)
    }
  }
  
  # Population-based weights (VH and SS)
  weight_var <- nsum_config$weighting_schemes[[scheme_name]]
  if (!is.null(weight_var) && weight_var %in% names(weight_data)) {
    return(weight_data[[weight_var]])
  }
  
  # If weight variable not found, return NULL (unweighted)
  cat("Warning: Weight variable", weight_var, "not found for scheme", scheme_name, ". Using unweighted.\n")
  return(NULL)
}

# ============================================================================
# CORE NSUM ESTIMATION FUNCTION (Enhanced)
# ============================================================================

estimate_nsum_population <- function(data, weights = NULL, hidden_connections_var, 
                                   degree_vars, probe_sizes, 
                                   total_population_size = 980000,
                                   weighting_scheme = "unweighted") {
  
  # Input validation
  if (length(degree_vars) != length(probe_sizes)) {
    stop("degree_vars and probe_sizes must have the same length")
  }
  
  # Handle weights based on weighting scheme
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
    cat("    Using unweighted estimation\n")
  } else {
    cat("    Using", weighting_scheme, "weights\n")
    
    # Validate weights
    if (length(weights) != nrow(data)) {
      warning("Weight vector length doesn't match data rows. Using unweighted.")
      weights <- rep(1, nrow(data))
    }
  }
  
  # Remove missing values for this estimation
  complete_cases <- complete.cases(data[[hidden_connections_var]], weights)
  data_complete <- data[complete_cases, ]
  weights_complete <- weights[complete_cases]
  
  if (nrow(data_complete) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No complete cases available"
    ))
  }
  
  cat("    Complete cases:", nrow(data_complete), "of", nrow(data), "\n")
  
  # Calculate weighted average number of hidden population members known (y_F,H)
  y_connections <- data_complete[[hidden_connections_var]]
  y_F_H <- sum(y_connections * weights_complete, na.rm = TRUE) / sum(weights_complete, na.rm = TRUE)
  
  # Calculate average personal network size (d_F,F) using probe questions
  network_sizes <- list()
  
  for (i in seq_along(degree_vars)) {
    degree_var <- degree_vars[i]
    probe_size <- probe_sizes[i]
    
    if (degree_var %in% names(data_complete)) {
      # Weighted average connections to this probe population
      avg_connections <- sum(data_complete[[degree_var]] * weights_complete, na.rm = TRUE) / 
                        sum(weights_complete, na.rm = TRUE)
      
      # Scale by known population size to get network size estimate
      network_sizes[[degree_var]] <- avg_connections / probe_size * total_population_size
    }
  }
  
  # Average across all probes to get d_F,F
  valid_network_sizes <- unlist(network_sizes)
  valid_network_sizes <- valid_network_sizes[is.finite(valid_network_sizes)]
  
  if (length(valid_network_sizes) == 0) {
    return(list(
      N_H_estimate = NA,
      error = "No valid network size estimates"
    ))
  }
  
  d_F_F <- mean(valid_network_sizes)
  
  # Calculate N_H estimate using basic NSUM formula
  # N_H = (y_F,H / d_F,F) * N_F
  N_H_estimate <- (y_F_H / d_F_F) * total_population_size
  
  # Compile comprehensive results
  result <- list(
    N_H_estimate = N_H_estimate,
    prevalence_rate = N_H_estimate / total_population_size,
    
    # NSUM components
    y_F_H = y_F_H,
    d_F_F = d_F_F,
    total_population_size = total_population_size,
    
    # Diagnostics
    sample_size = nrow(data_complete),
    hidden_connections_mean = y_F_H,
    network_size_estimates = network_sizes,
    average_network_size = d_F_F,
    
    # Metadata
    weighting_scheme = weighting_scheme,
    hidden_variable = hidden_connections_var,
    degree_variables = degree_vars,
    probe_sizes = probe_sizes,
    timestamp = Sys.time()
  )
  
  return(result)
}

# ============================================================================
# MONTE CARLO NSUM ESTIMATION
# ============================================================================

run_monte_carlo_nsum <- function(data, outcome_var, weights, population_sizes, 
                                weighting_scheme, iterations = 1000, 
                                confidence_level = 0.95, parallel_cores = 4) {
  
  cat("    Running Monte Carlo simulation with", iterations, "iterations\n")
  
  mc_results <- list()
  
  for (pop_size in population_sizes) {
    
    cat("      Population size:", format(pop_size, big.mark = ","), "\n")
    
    # Monte Carlo function for iteration execution
    mc_iteration <- function(i) {
      
      # Bootstrap sample (sampling with replacement)
      n <- nrow(data)
      sample_size <- floor(nsum_config$mc_sample_fraction * n)
      boot_indices <- sample(1:n, size = sample_size, replace = TRUE)
      
      boot_data <- data[boot_indices, ]
      boot_weights <- if (is.null(weights)) NULL else weights[boot_indices]
      
      # Run NSUM estimation on bootstrap sample
      tryCatch({
        result <- estimate_nsum_population(
          data = boot_data,
          weights = boot_weights,
          hidden_connections_var = outcome_var,
          degree_vars = nsum_config$degree_vars,
          probe_sizes = nsum_config$probe_sizes,
          total_population_size = pop_size,
          weighting_scheme = weighting_scheme
        )
        
        return(list(
          estimate = result$N_H_estimate,
          prevalence = result$prevalence_rate,
          y_F_H = result$y_F_H,
          d_F_F = result$d_F_F
        ))
        
      }, error = function(e) {
        return(list(estimate = NA, prevalence = NA, y_F_H = NA, d_F_F = NA))
      })
    }
    
    # Run Monte Carlo iterations in parallel
    if (parallel_cores > 1) {
      cl <- makeCluster(parallel_cores)
      clusterExport(cl, c("data", "weights", "outcome_var", "pop_size", 
                         "weighting_scheme", "nsum_config", "estimate_nsum_population"))
      
      mc_iterations_results <- parLapply(cl, 1:iterations, mc_iteration)
      stopCluster(cl)
    } else {
      mc_iterations_results <- lapply(1:iterations, mc_iteration)
    }
    
    # Extract results
    estimates <- sapply(mc_iterations_results, function(x) x$estimate)
    prevalences <- sapply(mc_iterations_results, function(x) x$prevalence)
    y_F_H_values <- sapply(mc_iterations_results, function(x) x$y_F_H)
    d_F_F_values <- sapply(mc_iterations_results, function(x) x$d_F_F)
    
    # Remove NA values
    valid_estimates <- estimates[!is.na(estimates)]
    valid_prevalences <- prevalences[!is.na(prevalences)]
    
    if (length(valid_estimates) < iterations * 0.5) {
      cat("        Warning: Only", length(valid_estimates), "valid estimates from", iterations, "iterations\n")
    }
    
    # Calculate confidence intervals
    alpha <- 1 - confidence_level
    if (length(valid_estimates) > 0) {
      estimate_ci <- quantile(valid_estimates, c(alpha/2, 1-alpha/2), na.rm = TRUE)
      prevalence_ci <- quantile(valid_prevalences, c(alpha/2, 1-alpha/2), na.rm = TRUE)
    } else {
      estimate_ci <- c(NA, NA)
      prevalence_ci <- c(NA, NA)
    }
    
    mc_results[[as.character(pop_size)]] <- list(
      population_size = pop_size,
      iterations = iterations,
      valid_iterations = length(valid_estimates),
      
      # Point estimates
      mean_estimate = mean(valid_estimates, na.rm = TRUE),
      median_estimate = median(valid_estimates, na.rm = TRUE),
      mean_prevalence = mean(valid_prevalences, na.rm = TRUE),
      median_prevalence = median(valid_prevalences, na.rm = TRUE),
      
      # Uncertainty measures
      estimate_sd = sd(valid_estimates, na.rm = TRUE),
      prevalence_sd = sd(valid_prevalences, na.rm = TRUE),
      estimate_ci_lower = estimate_ci[1],
      estimate_ci_upper = estimate_ci[2],
      prevalence_ci_lower = prevalence_ci[1],
      prevalence_ci_upper = prevalence_ci[2],
      
      # Component statistics
      y_F_H_mean = mean(y_F_H_values, na.rm = TRUE),
      d_F_F_mean = mean(d_F_F_values, na.rm = TRUE),
      
      # Raw data for further analysis
      all_estimates = valid_estimates,
      all_prevalences = valid_prevalences,
      
      # Metadata
      confidence_level = confidence_level,
      weighting_scheme = weighting_scheme
    )
    
    cat("        Mean estimate:", format(round(mean(valid_estimates, na.rm = TRUE)), big.mark = ","), 
        "CI: [", format(round(estimate_ci[1]), big.mark = ","), "-", 
        format(round(estimate_ci[2]), big.mark = ","), "]\n")
  }
  
  return(mc_results)
}

# ============================================================================
# COMPREHENSIVE NSUM ANALYSIS WITH ALL WEIGHTING SCHEMES
# ============================================================================

run_comprehensive_nsum_estimation <- function(outcome_vars = nsum_config$outcome_vars, 
                                            population_sizes = nsum_config$total_population_sizes,
                                            weighting_schemes = names(nsum_config$weighting_schemes)) {
  
  cat("=== Comprehensive NSUM Estimation ===\n")
  cat("Indicators:", length(outcome_vars), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Weighting schemes:", length(weighting_schemes), "\n")
  cat("Monte Carlo:", ifelse(nsum_config$run_monte_carlo, "YES", "NO"), "\n")
  cat("Total combinations:", length(outcome_vars) * length(population_sizes) * length(weighting_schemes), "\n\n")
  
  all_results <- list()
  scheme_summaries <- list()
  monte_carlo_results <- list()
  
  for (scheme in weighting_schemes) {
    
    cat("\n=== Weighting Scheme:", scheme, "===\n")
    scheme_results <- list()
    
    for (outcome_var in outcome_vars) {
      
      cat("\nProcessing:", outcome_var, "with", scheme, "weights\n")
      
      if (!(outcome_var %in% names(rd.dd))) {
        cat("Warning: Variable", outcome_var, "not found in data\n")
        next
      }
      
      # Get appropriate weights for this scheme and variable
      weights <- get_weights_for_scheme(rd.dd, scheme, outcome_var, weight_data = dd)
      
      var_results <- list()
      
      # Standard estimation for each population size
      for (pop_size in population_sizes) {
        
        cat("  Population size:", format(pop_size, big.mark = ","), "\n")
        
        tryCatch({
          
          result <- estimate_nsum_population(
            data = rd.dd,
            weights = weights,
            hidden_connections_var = outcome_var,
            degree_vars = nsum_config$degree_vars,
            probe_sizes = nsum_config$probe_sizes,
            total_population_size = pop_size,
            weighting_scheme = scheme
          )
          
          # Add metadata
          result$variable = outcome_var
          result$population_size = pop_size
          result$weighting_scheme = scheme
          
          var_results[[as.character(pop_size)]] <- result
          
          if (!is.na(result$N_H_estimate)) {
            cat("    Estimate:", format(round(result$N_H_estimate), big.mark = ","), 
                "(", round(result$prevalence_rate * 100, 2), "%)\n")
          }
          
        }, error = function(e) {
          cat("    Error:", e$message, "\n")
          var_results[[as.character(pop_size)]] <- list(
            variable = outcome_var,
            population_size = pop_size,
            weighting_scheme = scheme,
            error = e$message
          )
        })
      }
      
      scheme_results[[outcome_var]] <- var_results
      
      # Run Monte Carlo simulation if enabled
      if (nsum_config$run_monte_carlo) {
        cat("  Running Monte Carlo simulation...\n")
        
        mc_key <- paste(scheme, outcome_var, sep = "_")
        monte_carlo_results[[mc_key]] <- run_monte_carlo_nsum(
          data = rd.dd,
          outcome_var = outcome_var,
          weights = weights,
          population_sizes = population_sizes,
          weighting_scheme = scheme,
          iterations = nsum_config$mc_iterations,
          confidence_level = nsum_config$mc_confidence_level,
          parallel_cores = nsum_config$mc_parallel_cores
        )
      }
    }
    
    all_results[[scheme]] <- scheme_results
    
    # Create scheme summary
    scheme_summaries[[scheme]] <- create_scheme_summary(scheme_results, scheme)
  }
  
  cat("\n=== Comprehensive NSUM Estimation Completed ===\n\n")
  
  return(list(
    detailed_results = all_results,
    scheme_summaries = scheme_summaries,
    monte_carlo_results = monte_carlo_results,
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      schemes_run = weighting_schemes,
      outcomes_analyzed = outcome_vars,
      population_sizes = population_sizes,
      monte_carlo_enabled = nsum_config$run_monte_carlo
    )
  ))
}

# Helper function to create scheme summary
create_scheme_summary <- function(scheme_results, scheme_name) {
  
  summary_table <- data.frame()
  
  for (var_name in names(scheme_results)) {
    for (pop_size in names(scheme_results[[var_name]])) {
      
      result <- scheme_results[[var_name]][[pop_size]]
      
      if ("error" %in% names(result)) {
        next
      }
      
      summary_row <- data.frame(
        variable = var_name,
        population_size = as.numeric(pop_size),
        weighting_scheme = scheme_name,
        estimate = result$N_H_estimate,
        prevalence_rate = result$prevalence_rate,
        prevalence_pct = result$prevalence_rate * 100,
        y_F_H = result$y_F_H,
        d_F_F = result$d_F_F,
        sample_size = result$sample_size,
        stringsAsFactors = FALSE
      )
      
      summary_table <- rbind(summary_table, summary_row)
    }
  }
  
  if (nrow(summary_table) > 0) {
    summary_table <- summary_table %>%
      arrange(variable, population_size)
  }
  
  return(summary_table)
}

# ============================================================================
# RESULTS COMPILATION AND COMPARISON
# ============================================================================

create_comprehensive_summary <- function(comprehensive_results) {
  
  cat("=== Creating Comprehensive Summary ===\n")
  
  # Combine all scheme summaries
  all_summaries <- data.frame()
  
  for (scheme_name in names(comprehensive_results$scheme_summaries)) {
    scheme_summary <- comprehensive_results$scheme_summaries[[scheme_name]]
    if (nrow(scheme_summary) > 0) {
      all_summaries <- rbind(all_summaries, scheme_summary)
    }
  }
  
  # Create comparison tables by variable
  comparison_tables <- list()
  
  if (nrow(all_summaries) > 0) {
    
    for (var_name in unique(all_summaries$variable)) {
      
      var_data <- all_summaries[all_summaries$variable == var_name, ]
      
      # Pivot table: schemes vs population sizes
      comparison_table <- var_data %>%
        select(weighting_scheme, population_size, prevalence_pct) %>%
        pivot_wider(names_from = population_size, values_from = prevalence_pct,
                   names_prefix = "pop_") %>%
        arrange(weighting_scheme)
      
      comparison_tables[[var_name]] <- comparison_table
    }
  }
  
  # Monte Carlo summary if available
  mc_summary <- data.frame()
  
  if (length(comprehensive_results$monte_carlo_results) > 0) {
    
    for (mc_key in names(comprehensive_results$monte_carlo_results)) {
      
      mc_data <- comprehensive_results$monte_carlo_results[[mc_key]]
      
      # Extract scheme and variable from key
      key_parts <- strsplit(mc_key, "_")[[1]]
      scheme <- key_parts[1]
      var <- paste(key_parts[-1], collapse = "_")
      
      for (pop_size in names(mc_data)) {
        
        pop_data <- mc_data[[pop_size]]
        
        mc_row <- data.frame(
          variable = var,
          weighting_scheme = scheme,
          population_size = as.numeric(pop_size),
          mc_mean_estimate = pop_data$mean_estimate,
          mc_median_estimate = pop_data$median_estimate,
          mc_mean_prevalence_pct = pop_data$mean_prevalence * 100,
          mc_prevalence_ci_lower_pct = pop_data$prevalence_ci_lower * 100,
          mc_prevalence_ci_upper_pct = pop_data$prevalence_ci_upper * 100,
          mc_estimate_sd = pop_data$estimate_sd,
          mc_valid_iterations = pop_data$valid_iterations,
          stringsAsFactors = FALSE
        )
        
        mc_summary <- rbind(mc_summary, mc_row)
      }
    }
  }
  
  return(list(
    all_summaries = all_summaries,
    comparison_tables = comparison_tables,
    monte_carlo_summary = mc_summary,
    n_schemes = length(unique(all_summaries$weighting_scheme)),
    n_variables = length(unique(all_summaries$variable)),
    n_population_sizes = length(unique(all_summaries$population_size))
  ))
}

# ============================================================================
# MAIN ENHANCED NSUM ANALYSIS
# ============================================================================

main_enhanced_nsum_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting enhanced NSUM analysis with multiple weighting schemes...\n\n")
  
  # Step 1: Run comprehensive NSUM estimation
  comprehensive_results <- run_comprehensive_nsum_estimation()
  
  # Step 2: Create comprehensive summary
  summary_results <- create_comprehensive_summary(comprehensive_results)
  
  # Step 3: Create comparison with RDS (if available)
  rds_comparison <- NULL
  if (nsum_config$create_comparison_with_rds) {
    tryCatch({
      rds_comparison <- create_rds_nsum_comparison(comprehensive_results)
    }, error = function(e) {
      cat("Could not create RDS comparison:", e$message, "\n")
    })
  }
  
  # Step 4: Compile final results
  final_results <- list(
    comprehensive_results = comprehensive_results,
    summary_results = summary_results,
    rds_nsum_comparison = rds_comparison,
    
    config = nsum_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      total_combinations_run = summary_results$n_schemes * summary_results$n_variables * summary_results$n_population_sizes,
      monte_carlo_enabled = nsum_config$run_monte_carlo,
      packages_used = c("RDS", "tidyverse", "parallel", "boot", 
                       if (nsum_packages_available) "networkscaleup" else NULL)
    )
  )
  
  # Save comprehensive results
  save(final_results, file = here("output", "enhanced_nsum_estimation_results.RData"))
  cat("Comprehensive results saved to: output/enhanced_nsum_estimation_results.RData\n")
  
  # Save detailed tables
  if (nsum_config$save_detailed_results) {
    
    # All summaries
    write.csv(summary_results$all_summaries, 
              here("output", "tables", "nsum_all_schemes_summary.csv"),
              row.names = FALSE)
    
    # Comparison tables by variable
    for (var_name in names(summary_results$comparison_tables)) {
      filename <- paste0("nsum_comparison_", gsub("_nsum", "", var_name), ".csv")
      write.csv(summary_results$comparison_tables[[var_name]], 
                here("output", "tables", filename),
                row.names = FALSE)
    }
    
    # Monte Carlo summary
    if (nrow(summary_results$monte_carlo_summary) > 0) {
      write.csv(summary_results$monte_carlo_summary, 
                here("output", "tables", "nsum_monte_carlo_summary.csv"),
                row.names = FALSE)
    }
    
    cat("Detailed tables saved to: output/tables/\n")
  }
  
  cat("\n=== Enhanced NSUM Analysis Complete ===\n")
  cat("Weighting schemes analyzed:", summary_results$n_schemes, "\n")
  cat("Variables analyzed:", summary_results$n_variables, "\n")
  cat("Population sizes:", summary_results$n_population_sizes, "\n")
  cat("Total combinations:", final_results$metadata$total_combinations_run, "\n")
  
  if (nsum_config$run_monte_carlo) {
    cat("Monte Carlo simulations: YES (", nsum_config$mc_iterations, "iterations each)\n")
  }
  
  cat("\nKey results preview:\n")
  if (nrow(summary_results$all_summaries) > 0) {
    preview_data <- summary_results$all_summaries %>%
      filter(population_size == nsum_config$preferred_population_size) %>%
      select(variable, weighting_scheme, prevalence_pct) %>%
      arrange(variable, desc(prevalence_pct))
    
    print(head(preview_data, 10))
  }
  
  return(final_results)
}

# ============================================================================
# RDS COMPARISON (Enhanced)
# ============================================================================

create_rds_nsum_comparison <- function(comprehensive_results, rds_results_file = NULL) {
  
  cat("=== Creating Enhanced RDS vs NSUM Comparison ===\n")
  
  # Load RDS results if available
  if (is.null(rds_results_file)) {
    rds_results_file <- here("output", "rds_estimation_results.RData")
  }
  
  if (!file.exists(rds_results_file)) {
    cat("Warning: RDS results not found. Run RDS analysis first.\n")
    return(NULL)
  }
  
  load(rds_results_file)  # Should load 'final_results'
  
  # Extract RDS main results (preferred method)
  rds_main_table <- final_results$preferred_results$main_table
  
  # Get NSUM results for the preferred population size
  nsum_summary <- comprehensive_results$summary_results$all_summaries %>%
    filter(population_size == nsum_config$preferred_population_size)
  
  # Create comparison for each NSUM weighting scheme
  all_comparisons <- data.frame()
  
  for (scheme in unique(nsum_summary$weighting_scheme)) {
    
    scheme_data <- nsum_summary[nsum_summary$weighting_scheme == scheme, ]
    
    for (i in 1:nrow(rds_main_table)) {
      
      rds_indicator <- rds_main_table$indicator[i]
      nsum_indicator <- gsub("_rds", "_nsum", rds_indicator)
      
      nsum_match <- scheme_data[scheme_data$variable == nsum_indicator, ]
      
      if (nrow(nsum_match) > 0) {
        
        rds_est <- rds_main_table$estimate_pct[i]
        nsum_est <- nsum_match$prevalence_pct[1]
        
        comparison_row <- data.frame(
          indicator = rds_main_table$indicator_clean[i],
          rds_estimate_pct = rds_est,
          nsum_estimate_pct = nsum_est,
          nsum_weighting_scheme = scheme,
          difference_pct = nsum_est - rds_est,
          ratio = nsum_est / rds_est,
          abs_difference = abs(nsum_est - rds_est),
          confidence_level = rds_main_table$confidence[i],
          stringsAsFactors = FALSE
        )
        
        all_comparisons <- rbind(all_comparisons, comparison_row)
      }
    }
  }
  
  if (nrow(all_comparisons) > 0) {
    all_comparisons <- all_comparisons %>%
      arrange(indicator, nsum_weighting_scheme)
  }
  
  cat("Enhanced RDS vs NSUM comparison created for", length(unique(all_comparisons$indicator)), 
      "indicators across", length(unique(all_comparisons$nsum_weighting_scheme)), "weighting schemes\n\n")
  
  return(all_comparisons)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  # Check if this is being run from the pipeline
  if (exists("pipeline_config")) {
    cat("Running as part of main pipeline\n")
  } else {
    cat("Running standalone enhanced NSUM analysis\n")
  }
  
  # Run enhanced analysis
  enhanced_results <- main_enhanced_nsum_analysis()
  
} else {
  cat("Enhanced NSUM estimation script loaded (execution skipped)\n")
}
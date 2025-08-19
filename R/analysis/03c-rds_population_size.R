# 03c-rds_population_size.R
# Population size estimation using SS-PSE (posteriorsize and impute.visibility)
# Modular component of RDS analysis pipeline

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(parallel)
library(here)

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Source the results database functions
source(here("R", "analysis", "03a-rds_basic_estimation.R"))
skip_execution <- FALSE  # Prevent basic script from running

# Create unique ID for population size estimation configuration
create_popsize_parameter_id <- function(prior_size, visibility_dist = "nbinom", 
                                       parallel_cores = 4, burnin = 15, samplesize = 4, 
                                       interval = 1, seed = NULL) {
  components <- c("SS_PSE", paste0("prior", prior_size), visibility_dist, 
                  paste0("cores", parallel_cores), paste0("burn", burnin), 
                  paste0("samp", samplesize), paste0("int", interval))
  if (!is.null(seed)) components <- c(components, paste0("seed", seed))
  return(paste(components, collapse = "_"))
}

# Main function: Population Size Estimation
run_population_size_estimation <- function(
  prior_sizes = c(50000, 100000, 980000, 1740000),
  visibility_distributions = c("nbinom"),  # Start with just nbinom
  parallel_cores = min(4, detectCores()),  # Reduce parallel cores
  parallel_type = "PSOCK",
  burnin_samples = c(50, 1000),  # More conservative burnin
  mcmc_samples = c(100, 500),    # Reasonable sample sizes
  mcmc_intervals = c(5, 10),     # Conservative intervals
  force_recompute = FALSE
) {
  
  cat("Starting population size estimation using SS-PSE...\n")
  
  # Validate RDS data for population size estimation
  if (!exists("rd.dd") || nrow(rd.dd) < 10) {
    stop("Insufficient RDS data for population size estimation. Need at least 10 observations.")
  }
  
  # Check for essential variables
  essential_vars <- c("degree", "recruiter.id")
  missing_vars <- essential_vars[!essential_vars %in% names(rd.dd)]
  if (length(missing_vars) > 0) {
    cat("Warning: Missing variables for SS-PSE:", paste(missing_vars, collapse = ", "), "\n")
  }
  
  # Load existing results
  results_db <- load_rds_results_database()
  
  # Track computations
  new_results <- list()
  skipped_count <- 0
  computed_count <- 0
  
  # Create parameter combinations
  param_combinations <- expand.grid(
    prior_size = prior_sizes,
    visibility_dist = visibility_distributions,
    burnin = burnin_samples,
    samplesize = mcmc_samples,
    interval = mcmc_intervals,
    stringsAsFactors = FALSE
  )
  
  cat("Total parameter combinations:", nrow(param_combinations), "\n")
  
  for (i in 1:nrow(param_combinations)) {
    params <- param_combinations[i, ]
    
    # Create unique ID for this configuration
    config_id <- create_popsize_parameter_id(
      params$prior_size, params$visibility_dist, parallel_cores,
      params$burnin, params$samplesize, params$interval
    )
    
    # Check if already computed
    if (!force_recompute && 
        "population_size" %in% names(results_db) && 
        config_id %in% names(results_db$population_size)) {
      cat("  Skipping", config_id, "(already computed)\n")
      skipped_count <- skipped_count + 1
      next
    }
    
    # Compute population size estimate
    cat("  Computing", config_id, "\n")
    cat("    Prior:", params$prior_size, "| Dist:", params$visibility_dist, 
        "| MCMC:", params$burnin, "/", params$samplesize, "/", params$interval, "\n")
    
    start_time <- Sys.time()
    
    tryCatch({
      
      # Run posteriorsize estimation with error handling
      cat("Engaging warp drive using", parallel_type, "...\n")
      
      # First try with parallel processing
      ss_pse_result <- tryCatch({
        posteriorsize(
          rd.dd,
          mean.prior.size = params$prior_size,
          visibilitydistribution = params$visibility_dist,
          burnin = params$burnin,
          samplesize = params$samplesize,
          interval = params$interval,
          K = FALSE,
          priorsizedistribution = "beta",
          parallel = parallel_cores,
          parallel.type = parallel_type,
          verbose = FALSE
        )
      }, error = function(e) {
        # If parallel fails, try sequential
        cat("    Parallel failed, trying sequential...\n")
        posteriorsize(
          rd.dd,
          mean.prior.size = params$prior_size,
          visibilitydistribution = params$visibility_dist,
          burnin = params$burnin,
          samplesize = params$samplesize,
          interval = params$interval,
          K = FALSE,
          priorsizedistribution = "beta",
          parallel = 1,  # Sequential
          verbose = FALSE
        )
      })
      
      end_time <- Sys.time()
      computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      # Store result with metadata
      new_results[[config_id]] <- list(
        method = "SS_PSE",
        prior_size = params$prior_size,
        visibility_distribution = params$visibility_dist,
        parallel_cores = parallel_cores,
        parallel_type = parallel_type,
        burnin = params$burnin,
        samplesize = params$samplesize,
        interval = params$interval,
        estimate = ss_pse_result,
        config_id = config_id,
        computation_time_mins = computation_time,
        n_observations = nrow(dd)
      )
      
      cat("    Completed in", round(computation_time, 2), "minutes\n")
      computed_count <- computed_count + 1
      
      # Save intermediate results every few computations
      if (computed_count %% 3 == 0) {
        temp_db <- load_rds_results_database()
        temp_db <- save_to_rds_database(temp_db, new_results, "population_size")
        new_results <- list()  # Reset after saving
        cat("    Intermediate save completed (", computed_count, "total)\n")
      }
      
    }, error = function(e) {
      end_time <- Sys.time()
      computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      cat("    Error after", round(computation_time, 2), "minutes:", e$message, "\n")
      new_results[[config_id]] <- list(
        method = "SS_PSE",
        prior_size = params$prior_size,
        visibility_distribution = params$visibility_dist,
        parallel_cores = parallel_cores,
        parallel_type = parallel_type,
        burnin = params$burnin,
        samplesize = params$samplesize,
        interval = params$interval,
        error = e$message,
        config_id = config_id,
        computation_time_mins = computation_time,
        n_observations = nrow(dd)
      )
    })
  }
  
  # Save any remaining new results to database
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "population_size")
  }
  
  # Create summary table for this run
  if (computed_count > 0) {
    summary_table <- create_popsize_summary(results_db$population_size)
    write.csv(summary_table, here("output", "tables", "population_size_estimates_summary.csv"), row.names = FALSE)
    cat("Summary table saved: output/tables/population_size_estimates_summary.csv\n")
  }
  
  cat("Population size estimation completed!\n")
  cat("- New computations:", computed_count, "\n")
  cat("- Skipped (already computed):", skipped_count, "\n")
  cat("- Total population size database entries:", length(results_db$population_size %||% list()), "\n")
  
  return(results_db)
}

# Function: Impute visibility for existing estimates
run_visibility_imputation <- function(
  basic_rds_results = NULL,
  force_recompute = FALSE
) {
  
  cat("Starting visibility imputation...\n")
  
  # Load RDS results if not provided
  if (is.null(basic_rds_results)) {
    results_db <- load_rds_results_database()
    if (!"basic_rds" %in% names(results_db)) {
      stop("No basic RDS results found. Run 03a-rds_basic_estimation.R first.")
    }
    basic_rds_results <- results_db$basic_rds
  }
  
  # Load existing results
  results_db <- load_rds_results_database()
  new_results <- list()
  
  for (config_id in names(basic_rds_results)) {
    basic_result <- basic_rds_results[[config_id]]
    
    # Create visibility imputation ID
    vis_config_id <- paste0("VIS_", config_id)
    
    # Check if already computed
    if (!force_recompute && 
        "visibility_imputation" %in% names(results_db) && 
        vis_config_id %in% names(results_db$visibility_imputation)) {
      next
    }
    
    # Skip if basic result has error
    if (!is.null(basic_result$error)) {
      next
    }
    
    cat("  Computing visibility imputation for:", config_id, "\n")
    
    tryCatch({
      
      # Use impute.visibility function
      vis_result <- impute.visibility(
        rd.dd,
        trait.variable = basic_result$outcome_variable
      )
      
      new_results[[vis_config_id]] <- list(
        method = "visibility_imputation",
        based_on_config = config_id,
        outcome_variable = basic_result$outcome_variable,
        population_size = basic_result$population_size,
        visibility_result = vis_result,
        config_id = vis_config_id,
        n_observations = nrow(dd)
      )
      
    }, error = function(e) {
      cat("    Error:", e$message, "\n")
      new_results[[vis_config_id]] <- list(
        method = "visibility_imputation",
        based_on_config = config_id,
        outcome_variable = basic_result$outcome_variable,
        population_size = basic_result$population_size,
        error = e$message,
        config_id = vis_config_id,
        n_observations = nrow(dd)
      )
    })
  }
  
  # Save results
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "visibility_imputation")
  }
  
  cat("Visibility imputation completed! New entries:", length(new_results), "\n")
  return(results_db)
}

# Create summary table from population size results
create_popsize_summary <- function(popsize_results_list) {
  
  if (length(popsize_results_list) == 0) {
    return(data.frame())
  }
  
  summary_rows <- list()
  
  for (config_id in names(popsize_results_list)) {
    result <- popsize_results_list[[config_id]]
    
    # Extract population size estimate (structure varies)
    pop_estimate <- NA
    pop_ci_lower <- NA
    pop_ci_upper <- NA
    
    if (!is.null(result$estimate) && is.list(result$estimate)) {
      if ("pop.size" %in% names(result$estimate)) {
        pop_estimate <- mean(result$estimate$pop.size, na.rm = TRUE)
        pop_ci_lower <- quantile(result$estimate$pop.size, 0.025, na.rm = TRUE)
        pop_ci_upper <- quantile(result$estimate$pop.size, 0.975, na.rm = TRUE)
      } else if ("N" %in% names(result$estimate)) {
        pop_estimate <- mean(result$estimate$N, na.rm = TRUE)
        pop_ci_lower <- quantile(result$estimate$N, 0.025, na.rm = TRUE)
        pop_ci_upper <- quantile(result$estimate$N, 0.975, na.rm = TRUE)
      }
    }
    
    summary_rows <- append(summary_rows, list(data.frame(
      config_id = config_id,
      method = result$method,
      prior_size = result$prior_size %||% NA,
      visibility_distribution = result$visibility_distribution %||% NA,
      burnin = result$burnin %||% NA,
      samplesize = result$samplesize %||% NA,
      interval = result$interval %||% NA,
      population_estimate = pop_estimate,
      ci_lower = pop_ci_lower,
      ci_upper = pop_ci_upper,
      computation_time_mins = result$computation_time_mins %||% NA,
      n_observations = result$n_observations,
      has_error = !is.null(result$error),
      stringsAsFactors = FALSE
    )))
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame())
  }
}

# Execute population size estimation if running this script directly
if (!exists("skip_execution")) {
  cat("Starting population size estimation...\n")
  cat("Using fast settings by default. Modify parameters for thorough analysis.\n")
  
  # Run with fast settings by default
  popsize_results <- run_population_size_estimation(
    prior_sizes = c(980000),  # Just baseline
    visibility_distributions = c("nbinom"),  # Faster than CMP
    burnin_samples = c(15),   # Fast setting
    mcmc_samples = c(4),      # Fast setting
    mcmc_intervals = c(1)     # Fast setting
  )
  
  # Also run visibility imputation
  vis_results <- run_visibility_imputation()
}
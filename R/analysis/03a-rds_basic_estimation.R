# 03a-rds_basic_estimation.R
# Basic RDS estimation methods: RDS-I, RDS-II, RDS-SS
# Modular component of RDS analysis pipeline

# Load required libraries
library(tidyverse)
library(RDS)
library(here)

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Initialize or load results database
load_rds_results_database <- function() {
  results_file <- here("output", "rds_results_database.RDS")
  
  if (file.exists(results_file)) {
    cat("Loading existing RDS results database...\n")
    return(readRDS(results_file))
  } else {
    cat("Creating new RDS results database...\n")
    return(list())
  }
}

# Save results to database
save_to_rds_database <- function(results_db, new_results, result_type) {
  results_file <- here("output", "rds_results_database.RDS")
  
  # Add timestamp and type
  for (id in names(new_results)) {
    new_results[[id]]$timestamp <- Sys.time()
    new_results[[id]]$result_type <- result_type
  }
  
  # Merge with existing results
  results_db[[result_type]] <- c(results_db[[result_type]], new_results)
  
  # Save updated database
  saveRDS(results_db, results_file)
  cat("Results saved to database:", result_type, "-", length(new_results), "new entries\n")
  
  return(results_db)
}

# Create unique ID for parameter configuration
create_parameter_id <- function(method, outcome_var, pop_size, seed = NULL) {
  components <- c(method, outcome_var, pop_size)
  if (!is.null(seed)) components <- c(components, paste0("seed", seed))
  return(paste(components, collapse = "_"))
}

# Main function: Basic RDS estimation (RDS-I, RDS-II, RDS-SS)
run_basic_rds_estimation <- function(
  outcome_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds"),
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  pop_sizes = c(50000, 100000, 980000, 1740000),
  force_recompute = FALSE
) {
  
  cat("Starting basic RDS estimation (RDS-I, RDS-II, RDS-SS)...\n")
  
  # Load existing results
  results_db <- load_rds_results_database()
  
  # Combine all variables to analyze
  all_vars <- c(outcome_vars, legacy_vars)
  
  # Track new computations
  new_results <- list()
  skipped_count <- 0
  computed_count <- 0
  
  for (var in all_vars) {
    cat("Processing variable:", var, "\n")
    
    # Skip if variable doesn't exist or has no variation
    if (!var %in% names(dd) || length(unique(na.omit(dd[[var]]))) < 2) {
      cat("  Skipping", var, "(missing or no variation)\n")
      next
    }
    
    for (pop_size in pop_sizes) {
      for (method in c("RDS_I", "RDS_II", "RDS_SS")) {
        
        # Create unique ID for this configuration
        config_id <- create_parameter_id(method, var, pop_size)
        
        # Check if already computed
        if (!force_recompute && 
            "basic_rds" %in% names(results_db) && 
            config_id %in% names(results_db$basic_rds)) {
          cat("    Skipping", method, "for", var, "at N =", pop_size, "(already computed)\n")
          skipped_count <- skipped_count + 1
          next
        }
        
        # Compute estimate
        cat("    Computing", method, "for", var, "at N =", pop_size, "\n")
        tryCatch({
          
          estimate_result <- switch(method,
            "RDS_I" = RDS.I.estimates(rd.dd, outcome.variable = var, N = pop_size),
            "RDS_II" = RDS.II.estimates(rd.dd, outcome.variable = var, N = pop_size),
            "RDS_SS" = RDS.SS.estimates(rd.dd, outcome.variable = var, N = pop_size)
          )
          
          # Store result with metadata
          new_results[[config_id]] <- list(
            method = method,
            outcome_variable = var,
            population_size = pop_size,
            estimate = estimate_result,
            config_id = config_id,
            n_observations = nrow(dd)
          )
          
          computed_count <- computed_count + 1
          
        }, error = function(e) {
          cat("      Error:", e$message, "\n")
          new_results[[config_id]] <- list(
            method = method,
            outcome_variable = var,
            population_size = pop_size,
            error = e$message,
            config_id = config_id,
            n_observations = nrow(dd)
          )
        })
      }
    }
  }
  
  # Save new results to database
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "basic_rds")
  }
  
  # Create summary table for this run
  if (computed_count > 0) {
    summary_table <- create_basic_rds_summary(new_results)
    write.csv(summary_table, here("output", "tables", "basic_rds_estimates_summary.csv"), row.names = FALSE)
    cat("Summary table saved: output/tables/basic_rds_estimates_summary.csv\n")
  }
  
  cat("Basic RDS estimation completed!\n")
  cat("- New computations:", computed_count, "\n")
  cat("- Skipped (already computed):", skipped_count, "\n")
  cat("- Total database entries:", length(results_db$basic_rds %||% list()), "\n")
  
  return(results_db)
}

# Create summary table from results
create_basic_rds_summary <- function(results_list) {
  
  summary_rows <- list()
  
  for (config_id in names(results_list)) {
    result <- results_list[[config_id]]
    
    if (!is.null(result$estimate) && "estimate" %in% names(result$estimate)) {
      summary_rows <- append(summary_rows, list(data.frame(
        config_id = config_id,
        method = result$method,
        variable = result$outcome_variable,
        population_size = result$population_size,
        estimate = result$estimate$estimate,
        std_error = ifelse("std.error" %in% names(result$estimate), result$estimate$std.error, NA),
        ci_lower = ifelse(!is.null(result$estimate$confidence.interval), result$estimate$confidence.interval[1], NA),
        ci_upper = ifelse(!is.null(result$estimate$confidence.interval), result$estimate$confidence.interval[2], NA),
        n_observations = result$n_observations,
        stringsAsFactors = FALSE
      )))
    }
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame(
      config_id = character(), method = character(), variable = character(),
      population_size = numeric(), estimate = numeric(), std_error = numeric(),
      ci_lower = numeric(), ci_upper = numeric(), n_observations = numeric()
    ))
  }
}

# Utility function: Get existing results for a specific configuration
get_basic_rds_results <- function(method = NULL, variable = NULL, population_size = NULL) {
  results_db <- load_rds_results_database()
  
  if (!"basic_rds" %in% names(results_db)) {
    return(list())
  }
  
  basic_results <- results_db$basic_rds
  
  # Filter by criteria if specified
  if (!is.null(method) || !is.null(variable) || !is.null(population_size)) {
    filtered_results <- list()
    for (config_id in names(basic_results)) {
      result <- basic_results[[config_id]]
      if ((!is.null(method) && result$method != method) ||
          (!is.null(variable) && result$outcome_variable != variable) ||
          (!is.null(population_size) && result$population_size != population_size)) {
        next
      }
      filtered_results[[config_id]] <- result
    }
    return(filtered_results)
  }
  
  return(basic_results)
}

# Execute basic RDS estimation if running this script directly
if (!exists("skip_execution")) {
  rds_basic_results <- run_basic_rds_estimation()
}
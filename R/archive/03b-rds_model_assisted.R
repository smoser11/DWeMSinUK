# 03b-rds_model_assisted.R
# Model-assisted RDS estimation (computationally expensive - cached results)
# Modular component of RDS analysis pipeline

# Load required libraries
library(tidyverse)
library(RDS)
library(parallel)
library(here)

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Source the results database functions
source(here("R", "analysis", "03a-rds_basic_estimation.R"))
skip_execution <- FALSE  # Prevent basic script from running

# Create unique ID for MA parameter configuration
create_ma_parameter_id <- function(outcome_var, pop_size, seed_selection = "degree", 
                                  parallel_cores = 4, seed = NULL) {
  components <- c("MA", outcome_var, pop_size, seed_selection, 
                  paste0("cores", parallel_cores))
  if (!is.null(seed)) components <- c(components, paste0("seed", seed))
  return(paste(components, collapse = "_"))
}

# Main function: Model-Assisted RDS estimation
run_model_assisted_estimation <- function(
  outcome_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds"),
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  pop_sizes = c(50000, 100000, 980000, 1740000),
  seed_methods = c("sample", "random", "degree"),
  parallel_cores = min(4, detectCores()),
  force_recompute = FALSE,
  preferred_method_only = TRUE  # Focus on RDS-SS (preferred method from 03a)
) {
  
  cat("Starting Model-Assisted RDS estimation...\n")
  cat("Warning: This is computationally expensive and may take significant time.\n")
  
  # Load existing results
  results_db <- load_rds_results_database()
  
  # Combine all variables to analyze
  all_vars <- c(outcome_vars, legacy_vars)
  
  # Track computations
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
      for (seed_method in seed_methods) {
        
        # Create unique ID for this configuration
        config_id <- create_ma_parameter_id(var, pop_size, seed_method, 
                                          parallel_cores)
        
        # Check if already computed
        if (!force_recompute && 
            "model_assisted" %in% names(results_db) && 
            config_id %in% names(results_db$model_assisted)) {
          cat("    Skipping MA for", var, "at N =", pop_size, 
              "with", seed_method, "seeds (already computed)\n")
          skipped_count <- skipped_count + 1
          next
        }
        
        # Compute MA estimate
        cat("    Computing MA for", var, "at N =", pop_size, 
            "with", seed_method, "seeds and", parallel_cores, "cores\n")
        cat("      This may take several minutes...\n")
        
        start_time <- Sys.time()
        
        tryCatch({
          
          ma_result <- MA.estimates(
            rd.dd,
            trait.variable = var,
            N = pop_size,
            seed.selection = seed_method,
            parallel = parallel_cores
          )
          
          end_time <- Sys.time()
          computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
          
          # Store result with metadata
          new_results[[config_id]] <- list(
            method = "MA",
            outcome_variable = var,
            population_size = pop_size,
            seed_selection = seed_method,
            parallel_cores = parallel_cores,
            estimate = ma_result,
            config_id = config_id,
            computation_time_mins = computation_time,
            n_observations = nrow(dd)
          )
          
          cat("      Completed in", round(computation_time, 2), "minutes\n")
          computed_count <- computed_count + 1
          
          # Save intermediate results (in case of crash)
          if (computed_count %% 5 == 0) {
            temp_db <- load_rds_results_database()
            temp_db <- save_to_rds_database(temp_db, new_results, "model_assisted")
            new_results <- list()  # Reset after saving
            cat("    Intermediate save completed (", computed_count, "total)\n")
          }
          
        }, error = function(e) {
          end_time <- Sys.time()
          computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
          
          cat("      Error after", round(computation_time, 2), "minutes:", e$message, "\n")
          new_results[[config_id]] <- list(
            method = "MA",
            outcome_variable = var,
            population_size = pop_size,
            seed_selection = seed_method,
            parallel_cores = parallel_cores,
            error = e$message,
            config_id = config_id,
            computation_time_mins = computation_time,
            n_observations = nrow(dd)
          )
        })
      }
    }
  }
  
  # Save any remaining new results to database
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "model_assisted")
  }
  
  # Create summary table for this run
  if (computed_count > 0) {
    summary_table <- create_ma_summary(results_db$model_assisted)
    write.csv(summary_table, here("output", "tables", "model_assisted_estimates_summary.csv"), row.names = FALSE)
    cat("Summary table saved: output/tables/model_assisted_estimates_summary.csv\n")
  }
  
  cat("Model-Assisted RDS estimation completed!\n")
  cat("- New computations:", computed_count, "\n")
  cat("- Skipped (already computed):", skipped_count, "\n")
  cat("- Total MA database entries:", length(results_db$model_assisted %||% list()), "\n")
  
  return(results_db)
}

# Create summary table from MA results
create_ma_summary <- function(ma_results_list) {
  
  if (length(ma_results_list) == 0) {
    return(data.frame())
  }
  
  summary_rows <- list()
  
  for (config_id in names(ma_results_list)) {
    result <- ma_results_list[[config_id]]
    
    if (!is.null(result$estimate)) {
      # Extract key MA estimate components (structure may vary)
      estimate_val <- NA
      std_error_val <- NA
      
      if (is.list(result$estimate)) {
        if ("estimate" %in% names(result$estimate)) {
          estimate_val <- result$estimate$estimate
        } else if ("proportion.mean" %in% names(result$estimate)) {
          estimate_val <- result$estimate$proportion.mean
        }
        
        if ("std.error" %in% names(result$estimate)) {
          std_error_val <- result$estimate$std.error
        } else if ("proportion.sd" %in% names(result$estimate)) {
          std_error_val <- result$estimate$proportion.sd
        }
      }
      
      summary_rows <- append(summary_rows, list(data.frame(
        config_id = config_id,
        method = result$method,
        variable = result$outcome_variable,
        population_size = result$population_size,
        seed_selection = result$seed_selection %||% NA,
        parallel_cores = result$parallel_cores %||% NA,
        prior_distribution = result$prior_distribution %||% NA,
        estimate = estimate_val,
        std_error = std_error_val,
        computation_time_mins = result$computation_time_mins %||% NA,
        n_observations = result$n_observations,
        has_error = !is.null(result$error),
        stringsAsFactors = FALSE
      )))
    }
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame())
  }
}

# Utility function: Get MA results
get_ma_results <- function(variable = NULL, population_size = NULL, seed_selection = NULL) {
  results_db <- load_rds_results_database()
  
  if (!"model_assisted" %in% names(results_db)) {
    return(list())
  }
  
  ma_results <- results_db$model_assisted
  
  # Filter by criteria if specified
  if (!is.null(variable) || !is.null(population_size) || !is.null(seed_selection)) {
    filtered_results <- list()
    for (config_id in names(ma_results)) {
      result <- ma_results[[config_id]]
      if ((!is.null(variable) && result$outcome_variable != variable) ||
          (!is.null(population_size) && result$population_size != population_size) ||
          (!is.null(seed_selection) && result$seed_selection != seed_selection)) {
        next
      }
      filtered_results[[config_id]] <- result
    }
    return(filtered_results)
  }
  
  return(ma_results)
}

# Execute model-assisted estimation if running this script directly
if (!exists("skip_execution")) {
  cat("Starting Model-Assisted estimation...\n")
  cat("This will take significant time. Consider running with specific parameters.\n")
  
  # Run with conservative settings by default
  ma_results <- run_model_assisted_estimation(
    pop_sizes = c(980000),  # Just baseline
    seed_methods = c("degree"),  # Most stable method
    parallel_cores = min(4, detectCores())  # Conservative parallelism
  )
}

# 03e-rds_bootstrap.R
# Bootstrap uncertainty estimation for RDS methods
# Loads previous RDS estimation results and computes bootstrap confidence intervals
# Modular component of RDS analysis pipeline

# Load required libraries
library(tidyverse)
library(RDS)
library(Neighboot)
library(RDStreeboot)
library(parallel)
library(here)

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Source the results database functions
source(here("R", "analysis", "03a-rds_basic_estimation.R"))
skip_execution <- TRUE  # Prevent basic script from running

# Create unique ID for bootstrap configuration
create_bootstrap_parameter_id <- function(base_config_id, bootstrap_method, n_bootstrap, 
                                         quantiles = c(0.025, 0.975), method = "percentile", seed = NULL) {
  components <- c("BOOT", base_config_id, bootstrap_method, paste0("B", n_bootstrap), 
                  paste0("q", paste(quantiles, collapse = "-")), method)
  if (!is.null(seed)) components <- c(components, paste0("seed", seed))
  return(paste(components, collapse = "_"))
}

# Prepare network data for bootstrap packages
prepare_bootstrap_data <- function() {
  
  cat("Preparing network data for bootstrap analysis...\n")
  
  # Create edge list (relationships between recruiters and recruits)
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  # Renumber nodes sequentially for bootstrap packages
  node_map <- setNames(1:length(rd.dd$id), rd.dd$id)
  
  # Prepare traits data frame (all outcome variables)
  # Focus on CE's comparable indicators and preferred method results
  outcome_vars <- c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds", "zQ36", "zQ80", 
                   "sum_categories_factor", "composite_risk")
  
  # Filter to existing variables
  available_vars <- outcome_vars[outcome_vars %in% names(dd)]
  traits_df <- dd[, available_vars, drop = FALSE]
  
  # Handle factor variables (convert to numeric for bootstrap)
  for (var in names(traits_df)) {
    if (is.factor(traits_df[[var]])) {
      traits_df[[var]] <- as.numeric(traits_df[[var]]) - 1  # Convert to 0-based
    }
  }
  
  rds_bootstrap_data <- list(
    nodes = 1:length(node_map),
    edges = data.frame(
      from = node_map[as.character(edges_df$from)],
      to = node_map[as.character(edges_df$to)]
    ),
    traits = traits_df,
    degree = setNames(rd.dd$network.size, 1:length(rd.dd$network.size))
  )
  
  cat("Bootstrap data prepared:\n")
  cat("- Nodes:", length(rds_bootstrap_data$nodes), "\n")
  cat("- Edges:", nrow(rds_bootstrap_data$edges), "\n")
  cat("- Traits:", ncol(rds_bootstrap_data$traits), "\n")
  cat("- Available variables:", paste(available_vars, collapse = ", "), "\n")
  
  return(rds_bootstrap_data)
}

# Main function: Neighborhood Bootstrap Analysis
run_neighborhood_bootstrap <- function(
  target_configs = NULL,  # Specific RDS configurations to bootstrap
  n_bootstrap = 1000,
  quantiles = c(0.025, 0.975),
  method = "percentile",
  force_recompute = FALSE,
  preferred_method_priority = TRUE  # Prioritize RDS-SS (preferred method)
) {
  
  cat("Starting Neighborhood Bootstrap analysis...\n")
  
  # Load existing RDS results
  results_db <- load_rds_results_database()
  
  if (!"basic_rds" %in% names(results_db) || length(results_db$basic_rds) == 0) {
    stop("No basic RDS results found. Run 03a-rds_basic_estimation.R first.")
  }
  
  # Prepare bootstrap data
  bootstrap_data <- prepare_bootstrap_data()
  
  # Determine which configurations to bootstrap
  if (is.null(target_configs)) {
    if (preferred_method_priority) {
      # Prioritize RDS-SS configurations (preferred method from 03a analysis)
      target_configs <- names(results_db$basic_rds)[
        sapply(results_db$basic_rds, function(x) {
          is.null(x$error) && !is.null(x$method) && x$method == "RDS_SS"
        })
      ]
      cat("Prioritizing preferred method (RDS-SS) for bootstrap analysis\n")
    } else {
      # Bootstrap all successful basic RDS results
      target_configs <- names(results_db$basic_rds)[
        sapply(results_db$basic_rds, function(x) is.null(x$error))
      ]
    }
  }
  
  cat("Target configurations for bootstrap:", length(target_configs), "\n")
  
  # Track computations
  new_results <- list()
  skipped_count <- 0
  computed_count <- 0
  
  for (config_id in target_configs) {
    basic_result <- results_db$basic_rds[[config_id]]
    
    if (is.null(basic_result) || !is.null(basic_result$error)) {
      cat("  Skipping", config_id, "(error in basic result)\n")
      next
    }
    
    # Create bootstrap configuration ID
    boot_config_id <- create_bootstrap_parameter_id(config_id, "neighborhood", 
                                                    n_bootstrap, quantiles, method)
    
    # Check if already computed
    if (!force_recompute && 
        "neighborhood_bootstrap" %in% names(results_db) && 
        boot_config_id %in% names(results_db$neighborhood_bootstrap)) {
      cat("  Skipping", config_id, "(bootstrap already computed)\n")
      skipped_count <- skipped_count + 1
      next
    }
    
    # Check if variable exists in bootstrap data
    var_name <- basic_result$outcome_variable
    if (!var_name %in% names(bootstrap_data$traits)) {
      cat("  Skipping", config_id, "(variable not in bootstrap data)\n")
      next
    }
    
    # Run neighborhood bootstrap
    cat("  Computing neighborhood bootstrap for:", config_id, "\n")
    cat("    Variable:", var_name, "| Bootstrap samples:", n_bootstrap, "\n")
    
    start_time <- Sys.time()
    
    tryCatch({
      
      # Run neighborhood bootstrap
      neighb_result <- neighb(
        bootstrap_data,
        quant = quantiles,
        method = method,
        B = n_bootstrap
      )
      
      end_time <- Sys.time()
      computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      # Extract results for this specific variable
      var_index <- which(names(bootstrap_data$traits) == var_name)
      if (length(var_index) == 1) {
        var_bootstrap_result <- neighb_result[var_index, , drop = FALSE]
      } else {
        var_bootstrap_result <- neighb_result
      }
      
      # Store result
      new_results[[boot_config_id]] <- list(
        method = "neighborhood_bootstrap",
        base_config_id = config_id,
        bootstrap_method = "neighborhood",
        outcome_variable = var_name,
        population_size = basic_result$population_size,
        n_bootstrap = n_bootstrap,
        quantiles = quantiles,
        confidence_method = method,
        bootstrap_result = var_bootstrap_result,
        base_estimate = basic_result$estimate,
        config_id = boot_config_id,
        computation_time_mins = computation_time,
        n_observations = nrow(dd)
      )
      
      cat("    Completed in", round(computation_time, 2), "minutes\n")
      computed_count <- computed_count + 1
      
      # Save intermediate results every few computations
      if (computed_count %% 10 == 0) {
        temp_db <- load_rds_results_database()
        temp_db <- save_to_rds_database(temp_db, new_results, "neighborhood_bootstrap")
        new_results <- list()  # Reset after saving
        cat("    Intermediate save completed (", computed_count, "total)\n")
      }
      
    }, error = function(e) {
      end_time <- Sys.time()
      computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      cat("    Error after", round(computation_time, 2), "minutes:", e$message, "\n")
      new_results[[boot_config_id]] <- list(
        method = "neighborhood_bootstrap",
        base_config_id = config_id,
        bootstrap_method = "neighborhood",
        outcome_variable = var_name,
        population_size = basic_result$population_size,
        error = e$message,
        config_id = boot_config_id,
        computation_time_mins = computation_time,
        n_observations = nrow(dd)
      )
    })
  }
  
  # Save any remaining new results
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "neighborhood_bootstrap")
  }
  
  cat("Neighborhood bootstrap completed!\n")
  cat("- New computations:", computed_count, "\n")
  cat("- Skipped (already computed):", skipped_count, "\n")
  
  return(results_db)
}

# Function: Tree Bootstrap Analysis
run_tree_bootstrap <- function(
  target_configs = NULL,
  n_bootstrap = 500,  # Generally more intensive than neighborhood
  quantiles = c(0.025, 0.975),
  method = "percentile",
  force_recompute = FALSE
) {
  
  cat("Starting Tree Bootstrap analysis...\n")
  cat("Note: Tree bootstrap is more computationally intensive.\n")
  
  # Load existing results
  results_db <- load_rds_results_database()
  
  if (!"basic_rds" %in% names(results_db)) {
    stop("No basic RDS results found. Run 03a-rds_basic_estimation.R first.")
  }
  
  # Prepare bootstrap data
  bootstrap_data <- prepare_bootstrap_data()
  
  # Check network connectivity for tree bootstrap
  if (nrow(bootstrap_data$edges) == 0) {
    cat("Warning: No edges found in network. Tree bootstrap may not be applicable.\n")
    return(results_db)
  }
  
  # Determine target configurations
  if (is.null(target_configs)) {
    target_configs <- names(results_db$basic_rds)[
      sapply(results_db$basic_rds, function(x) is.null(x$error))
    ]
  }
  
  new_results <- list()
  computed_count <- 0
  skipped_count <- 0
  
  for (config_id in target_configs) {
    basic_result <- results_db$basic_rds[[config_id]]
    
    if (is.null(basic_result) || !is.null(basic_result$error)) {
      next
    }
    
    # Create tree bootstrap configuration ID
    boot_config_id <- create_bootstrap_parameter_id(config_id, "tree", 
                                                    n_bootstrap, quantiles, method)
    
    # Check if already computed
    if (!force_recompute && 
        "tree_bootstrap" %in% names(results_db) && 
        boot_config_id %in% names(results_db$tree_bootstrap)) {
      skipped_count <- skipped_count + 1
      next
    }
    
    var_name <- basic_result$outcome_variable
    if (!var_name %in% names(bootstrap_data$traits)) {
      next
    }
    
    cat("  Computing tree bootstrap for:", config_id, "\n")
    
    start_time <- Sys.time()
    
    tryCatch({
      
      # Run tree bootstrap
      tree_result <- treeboot(
        bootstrap_data,
        quant = quantiles,
        method = method,
        B = n_bootstrap
      )
      
      end_time <- Sys.time()
      computation_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      # Extract results for specific variable
      var_index <- which(names(bootstrap_data$traits) == var_name)
      if (length(var_index) == 1) {
        var_bootstrap_result <- tree_result[var_index, , drop = FALSE]
      } else {
        var_bootstrap_result <- tree_result
      }
      
      new_results[[boot_config_id]] <- list(
        method = "tree_bootstrap",
        base_config_id = config_id,
        bootstrap_method = "tree",
        outcome_variable = var_name,
        population_size = basic_result$population_size,
        n_bootstrap = n_bootstrap,
        quantiles = quantiles,
        confidence_method = method,
        bootstrap_result = var_bootstrap_result,
        base_estimate = basic_result$estimate,
        config_id = boot_config_id,
        computation_time_mins = computation_time,
        n_observations = nrow(dd)
      )
      
      cat("    Completed in", round(computation_time, 2), "minutes\n")
      computed_count <- computed_count + 1
      
    }, error = function(e) {
      cat("    Tree bootstrap failed for", config_id, ":", e$message, "\n")
      # Tree bootstrap may fail if network structure is unsuitable
    })
  }
  
  # Save results
  if (length(new_results) > 0) {
    results_db <- save_to_rds_database(results_db, new_results, "tree_bootstrap")
  }
  
  cat("Tree bootstrap completed!\n")
  cat("- New computations:", computed_count, "\n")
  cat("- Skipped:", skipped_count, "\n")
  
  return(results_db)
}

# Create summary of bootstrap results
create_bootstrap_summary <- function() {
  
  results_db <- load_rds_results_database()
  
  bootstrap_types <- c("neighborhood_bootstrap", "tree_bootstrap")
  summary_data <- list()
  
  for (boot_type in bootstrap_types) {
    if (boot_type %in% names(results_db) && length(results_db[[boot_type]]) > 0) {
      
      boot_results <- results_db[[boot_type]]
      
      for (config_id in names(boot_results)) {
        result <- boot_results[[config_id]]
        
        # Extract bootstrap confidence intervals
        ci_lower <- NA
        ci_upper <- NA
        std_error <- NA
        
        if (!is.null(result$bootstrap_result) && is.matrix(result$bootstrap_result)) {
          if (ncol(result$bootstrap_result) >= 2) {
            std_error <- result$bootstrap_result[1, 1]  # First column usually SE
            ci_lower <- result$bootstrap_result[1, 2]   # Second column usually lower CI
            ci_upper <- result$bootstrap_result[1, 3]   # Third column usually upper CI
          }
        }
        
        # Get base estimate
        base_estimate <- NA
        if (!is.null(result$base_estimate) && "estimate" %in% names(result$base_estimate)) {
          base_estimate <- result$base_estimate$estimate
        }
        
        summary_data <- append(summary_data, list(data.frame(
          config_id = config_id,
          bootstrap_method = result$bootstrap_method,
          base_config_id = result$base_config_id,
          variable = result$outcome_variable,
          population_size = result$population_size,
          base_estimate = base_estimate,
          bootstrap_se = std_error,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          n_bootstrap = result$n_bootstrap %||% NA,
          computation_time_mins = result$computation_time_mins %||% NA,
          has_error = !is.null(result$error),
          stringsAsFactors = FALSE
        )))
      }
    }
  }
  
  if (length(summary_data) > 0) {
    combined_summary <- do.call(rbind, summary_data)
    write.csv(combined_summary, here("output", "tables", "bootstrap_results_summary.csv"), row.names = FALSE)
    return(combined_summary)
  } else {
    return(data.frame())
  }
}

# Execute bootstrap analysis if running this script directly
if (!exists("skip_execution")) {
  cat("Starting bootstrap uncertainty analysis...\n")
  cat("This will take significant time. Consider running with specific configurations.\n")
  
  # Run neighborhood bootstrap first (generally more stable)
  neighborhood_results <- run_neighborhood_bootstrap(
    n_bootstrap = 100  # Conservative default
  )
  
  # Create and save summary
  bootstrap_summary <- create_bootstrap_summary()
  
  cat("Bootstrap analysis completed! Check output/tables/bootstrap_results_summary.csv\n")
}
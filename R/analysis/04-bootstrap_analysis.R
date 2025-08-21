# 04-bootstrap_analysis.R
# Bootstrap Confidence Intervals for RDS Estimates  
# Domestic Worker Exploitation and Modern Slavery in UK
#
# Combines neighborhood bootstrap and tree bootstrap methods
# Provides uncertainty estimates for preferred RDS-SS model

cat("=== Bootstrap Analysis for RDS Estimates ===\n")
cat("Methods: Neighborhood bootstrap and Tree bootstrap\n")
cat("Focus: Confidence intervals for preferred model\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(Neighboot)
library(RDStreeboot)
library(parallel)
library(igraph)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# ============================================================================
# CONFIGURATION
# ============================================================================

bootstrap_config <- list(
  # Focus on comparable indicators + risk variables
  outcome_vars = c(get_comparable_indicators()$rds_vars, 
                   "composite_risk", "whether_exploitation"),
  preferred_method = "RDS_SS",
  main_population_size = 980000,
  
  # Bootstrap parameters
  n_bootstrap = 1000,
  confidence_level = 0.95,
  quantiles = c(0.025, 0.975),
  
  # Methods to use
  use_neighborhood_bootstrap = TRUE,  # Enable neighb() from Neighboot package
  use_tree_bootstrap = TRUE,         # Enable treeboot.RDS() from RDStreeboot package
  
  # Computational
  parallel_cores = 4,
  seed = 12345,
  
  # Output
  save_detailed_results = TRUE,
  create_ci_plots = TRUE
)

cat("Bootstrap configuration:\n")
cat("- Methods: Neighborhood +", ifelse(bootstrap_config$use_tree_bootstrap, "Tree", ""), "\n")
cat("- Bootstrap samples:", bootstrap_config$n_bootstrap, "\n") 
cat("- Confidence level:", bootstrap_config$confidence_level * 100, "%\n")
cat("- Variables:", length(bootstrap_config$outcome_vars), "comparable indicators\n\n")

# ============================================================================
# SIMPLE BOOTSTRAP USING BUILT-IN RDS FUNCTIONS
# ============================================================================

get_simple_bootstrap_ci <- function(outcome_vars, n_bootstrap = 1000, 
                                   confidence_level = 0.95, seed = 12345) {
  
  cat("=== Simple Bootstrap Confidence Intervals ===\n")
  
  set.seed(seed)
  
  bootstrap_results <- list()
  alpha <- 1 - confidence_level
  quantiles <- c(alpha/2, 1 - alpha/2)
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(rd.dd))) {
      cat("Warning: Variable", outcome_var, "not found in RDS data\n")
      next
    }
    
    tryCatch({
      
      # Get original RDS estimate using RDS-II (doesn't require population size)
      rds_estimate <- RDS.II.estimates(rd.dd, outcome.variable = outcome_var)
      original_estimate <- rds_estimate$estimate
      
      # Bootstrap resampling
      boot_estimates <- numeric(n_bootstrap)
      
      for (b in 1:n_bootstrap) {
        # Simple bootstrap: resample with replacement
        boot_indices <- sample(1:nrow(rd.dd), replace = TRUE)
        boot_data <- rd.dd[boot_indices, ]
        
        # Maintain RDS structure
        class(boot_data) <- class(rd.dd)
        attributes(boot_data) <- attributes(rd.dd)
        
        # Get bootstrap estimate
        boot_est <- tryCatch({
          boot_rds <- RDS.II.estimates(boot_data, outcome.variable = outcome_var)
          boot_rds$estimate
        }, error = function(e) {
          original_estimate  # Use original if bootstrap fails
        })
        
        boot_estimates[b] <- boot_est
      }
      
      # Calculate confidence interval
      ci_bounds <- quantile(boot_estimates, quantiles, na.rm = TRUE)
      bootstrap_se <- sd(boot_estimates, na.rm = TRUE)
      
      result <- list(
        variable = outcome_var,
        method = "simple_bootstrap",
        original_estimate = original_estimate,
        bootstrap_se = bootstrap_se,
        ci_lower = ci_bounds[1],
        ci_upper = ci_bounds[2],
        confidence_level = confidence_level,
        n_bootstrap = n_bootstrap,
        boot_estimates = boot_estimates
      )
      
      bootstrap_results[[outcome_var]] <- result
      
      cat("  Completed - Estimate:", round(result$original_estimate, 4), 
          "CI: [", round(result$ci_lower, 4), ",", round(result$ci_upper, 4), "]\n")
      
    }, error = function(e) {
      cat("  Error in bootstrap for", outcome_var, ":", e$message, "\n")
      bootstrap_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "simple_bootstrap", 
        error = e$message
      )
    })
  }
  
  cat("Simple bootstrap completed\n\n")
  return(bootstrap_results)
}

# ============================================================================
# CONVERT RDS DATA TO NEIGHBOOT FORMAT
# ============================================================================

convert_rds_to_neighboot_format <- function() {
  
  cat("=== Converting RDS Data to Neighboot Format ===\n")
  
  # Create nodes vector (sequential node IDs)
  nodes <- 1:nrow(rd.dd)
  
  # Create traits data frame with comparable indicators + risk variables
  traits_df <- data.frame(
    document_withholding_rds = rd.dd$document_withholding_rds,
    pay_issues_rds = rd.dd$pay_issues_rds,
    threats_abuse_rds = rd.dd$threats_abuse_rds,
    excessive_hours_rds = rd.dd$excessive_hours_rds,
    access_to_help_rds = rd.dd$access_to_help_rds,
    composite_risk = rd.dd$composite_risk,
    whether_exploitation = rd.dd$whether_exploitation
  )
  
  # Create edges data frame from recruitment relationships
  edges_list <- list()
  edge_count <- 0
  
  # Map original IDs to sequential positions  
  id_to_pos <- setNames(1:nrow(rd.dd), rd.dd$id)
  
  # Check for valid recruitment relationships
  valid_recruiters <- sum(rd.dd$recruiter.id != "-1" & !is.na(rd.dd$recruiter.id), na.rm = TRUE)
  
  for (i in 1:nrow(rd.dd)) {
    recruiter_id <- rd.dd$recruiter.id[i] 
    # Check for valid recruiter ID (not -1, not NA, not empty)
    if (!is.na(recruiter_id) && 
        recruiter_id != "-1" && 
        recruiter_id != "" && 
        recruiter_id %in% names(id_to_pos)) {
      
      recruiter_pos <- id_to_pos[recruiter_id]
      recruit_pos <- i
      
      # Only add edge if recruiter_pos is not NA
      if (!is.na(recruiter_pos)) {
        edge_count <- edge_count + 1
        edges_list[[edge_count]] <- data.frame(
          node1 = recruiter_pos,
          node2 = recruit_pos
        )
      }
    }
  }
  
  # Combine edges into data frame
  if (length(edges_list) > 0) {
    edges_df <- do.call(rbind, edges_list)
  } else {
    edges_df <- data.frame(node1 = integer(0), node2 = integer(0))
  }
  
  # Use network size as degree (this is our best approximation)
  degree <- rd.dd$network.size
  
  # Create the RDS sample structure expected by neighb
  rds_sample <- list(
    nodes = nodes,
    edges = edges_df,
    degree = degree,
    traits = traits_df
  )
  
  cat("RDS sample prepared: ", length(rds_sample$nodes), " nodes, ", 
      nrow(rds_sample$edges), " recruitment edges, ", 
      ncol(rds_sample$traits), " traits\n\n")
  
  return(rds_sample)
}

# ============================================================================
# NEIGHBORHOOD BOOTSTRAP
# ============================================================================

run_neighborhood_bootstrap <- function(outcome_vars, n_bootstrap = 1000, 
                                     confidence_level = 0.95, seed = 12345) {
  
  cat("=== Neighborhood Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  # Convert RDS data to neighb format
  rds_sample <- convert_rds_to_neighboot_format()
  
  neighborhood_results <- list()
  alpha <- 1 - confidence_level
  quantiles <- c(alpha/2, 1 - alpha/2)
  
  tryCatch({
    
    cat("Running neighborhood bootstrap for all indicators (n =", n_bootstrap, ")...\n")
    
    # Run neighborhood bootstrap using Neighboot package
    neighboot_result <- neighb(
      RDS.data = rds_sample,
      quant = quantiles,
      method = "percentile",
      B = n_bootstrap
    )
    
    cat("Neighb bootstrap result:\n")
    print(neighboot_result)
    
    # Extract results for each outcome variable
    for (outcome_var in outcome_vars) {
      
      cat("Extracting results for:", outcome_var, "\n")
      
      if (is.matrix(neighboot_result) && outcome_var %in% rownames(neighboot_result)) {
        
        var_results <- neighboot_result[outcome_var, ]
        
        result <- list(
          variable = outcome_var,
          method = "neighborhood_bootstrap",
          original_estimate = mean(rds_sample$traits[[outcome_var]], na.rm = TRUE),
          bootstrap_se = var_results["SE"],
          ci_lower = var_results[as.character(quantiles[1])],
          ci_upper = var_results[as.character(quantiles[2])],
          confidence_level = confidence_level,
          n_bootstrap = n_bootstrap
        )
        
        neighborhood_results[[outcome_var]] <- result
        
        cat("  Completed - Estimate:", round(result$original_estimate, 4), 
            "CI: [", round(result$ci_lower, 4), ",", round(result$ci_upper, 4), "]\n")
        
      } else {
        cat("  Warning: Results not found for", outcome_var, "\n")
        if (is.matrix(neighboot_result)) {
          cat("  Available row names:", rownames(neighboot_result), "\n")
        }
      }
    }
    
  }, error = function(e) {
    cat("  Error in neighborhood bootstrap:", e$message, "\n")
    for (outcome_var in outcome_vars) {
      neighborhood_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "neighborhood_bootstrap", 
        error = e$message
      )
    }
  })
  
  cat("Neighborhood bootstrap completed\n\n")
  return(neighborhood_results)
}

# ============================================================================
# TREE BOOTSTRAP  
# ============================================================================

run_tree_bootstrap <- function(outcome_vars, n_bootstrap = 1000,
                              confidence_level = 0.95, seed = 12345) {
  
  cat("=== Tree Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  # Convert RDS data to the format expected by treeboot.RDS
  rds_sample <- convert_rds_to_neighboot_format()
  
  tree_results <- list()
  alpha <- 1 - confidence_level
  quantiles <- c(alpha/2, 1 - alpha/2)
  
  tryCatch({
    
    cat("Running tree bootstrap for all indicators (n =", n_bootstrap, ")...\n")
    
    # Run tree bootstrap using RDStreeboot package
    tree_result <- treeboot.RDS(
      samp = rds_sample,
      quant = quantiles,
      B = n_bootstrap
    )
    
    cat("Tree bootstrap result:\n")
    print(tree_result)
    
    # Extract results for each outcome variable
    for (outcome_var in outcome_vars) {
      
      cat("Extracting results for:", outcome_var, "\n")
      
      if (is.matrix(tree_result) && outcome_var %in% rownames(tree_result)) {
        
        var_results <- tree_result[outcome_var, ]
        
        result <- list(
          variable = outcome_var,
          method = "tree_bootstrap",
          original_estimate = mean(rds_sample$traits[[outcome_var]], na.rm = TRUE),
          bootstrap_se = if("SE" %in% names(var_results)) var_results["SE"] else NA,
          ci_lower = var_results[as.character(quantiles[1])],
          ci_upper = var_results[as.character(quantiles[2])],
          confidence_level = confidence_level,
          n_bootstrap = n_bootstrap
        )
        
        tree_results[[outcome_var]] <- result
        
        cat("  Completed - Estimate:", round(result$original_estimate, 4), 
            "CI: [", round(result$ci_lower, 4), ",", round(result$ci_upper, 4), "]\n")
        
      } else {
        cat("  Warning: Results not found for", outcome_var, "\n")
        if (is.matrix(tree_result)) {
          cat("  Available row names:", rownames(tree_result), "\n")
        }
      }
    }
    
  }, error = function(e) {
    cat("  Error in tree bootstrap:", e$message, "\n")
    for (outcome_var in outcome_vars) {
      tree_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "tree_bootstrap",
        error = e$message
      )
    }
  })
  
  cat("Tree bootstrap completed\n\n")
  return(tree_results)
}

# ============================================================================
# COMBINE BOOTSTRAP METHODS
# ============================================================================

combine_bootstrap_results <- function(simple_results, neighborhood_results, tree_results) {
  
  cat("=== Combining Bootstrap Results ===\n")
  
  combined_results <- list()
  all_vars <- unique(c(names(simple_results), names(neighborhood_results), names(tree_results)))
  
  for (outcome_var in all_vars) {
    
    var_result <- list(variable = outcome_var)
    
    # Add simple bootstrap results (always available)
    if (outcome_var %in% names(simple_results)) {
      sb_result <- simple_results[[outcome_var]]
      var_result$simple_bootstrap <- sb_result
    }
    
    # Add neighborhood bootstrap results
    if (outcome_var %in% names(neighborhood_results)) {
      nb_result <- neighborhood_results[[outcome_var]]
      var_result$neighborhood_bootstrap <- nb_result
    }
    
    # Add tree bootstrap results  
    if (outcome_var %in% names(tree_results)) {
      tb_result <- tree_results[[outcome_var]]
      var_result$tree_bootstrap <- tb_result
    }
    
    # Create summary comparison using available methods
    available_methods <- list()
    if (outcome_var %in% names(simple_results) && !"error" %in% names(simple_results[[outcome_var]])) {
      available_methods$simple <- simple_results[[outcome_var]]
    }
    if (outcome_var %in% names(neighborhood_results) && !"error" %in% names(neighborhood_results[[outcome_var]])) {
      available_methods$neighborhood <- neighborhood_results[[outcome_var]]
    }
    if (outcome_var %in% names(tree_results) && !"error" %in% names(tree_results[[outcome_var]])) {
      available_methods$tree <- tree_results[[outcome_var]]
    }
    
    if (length(available_methods) > 0) {
      # Use simple bootstrap estimate as primary (most reliable)
      primary_estimate <- if ("simple" %in% names(available_methods)) {
        available_methods$simple$original_estimate
      } else {
        available_methods[[1]]$original_estimate
      }
      
      # Collect all CIs
      all_ci_lower <- sapply(available_methods, function(x) x$ci_lower)
      all_ci_upper <- sapply(available_methods, function(x) x$ci_upper)
      
      var_result$comparison <- list(
        primary_estimate = primary_estimate,
        methods_available = names(available_methods),
        
        # Individual method CIs
        simple_ci = if("simple" %in% names(available_methods)) c(available_methods$simple$ci_lower, available_methods$simple$ci_upper) else NULL,
        neighborhood_ci = if("neighborhood" %in% names(available_methods)) c(available_methods$neighborhood$ci_lower, available_methods$neighborhood$ci_upper) else NULL,
        tree_ci = if("tree" %in% names(available_methods)) c(available_methods$tree$ci_lower, available_methods$tree$ci_upper) else NULL,
        
        # Conservative combined CI (widest bounds)
        combined_ci_lower = min(all_ci_lower, na.rm = TRUE),
        combined_ci_upper = max(all_ci_upper, na.rm = TRUE)
      )
    }
    
    combined_results[[outcome_var]] <- var_result
  }
  
  cat("Bootstrap methods combined for", length(combined_results), "variables\n\n")
  return(combined_results)
}

# ============================================================================
# CREATE BOOTSTRAP SUMMARY TABLE
# ============================================================================

create_bootstrap_summary_table <- function(combined_results) {
  
  cat("=== Creating Bootstrap Summary Table ===\n")
  
  summary_rows <- list()
  
  for (outcome_var in names(combined_results)) {
    
    result <- combined_results[[outcome_var]]
    
    if ("comparison" %in% names(result)) {
      
      comp <- result$comparison
      
      summary_rows[[outcome_var]] <- data.frame(
        indicator = outcome_var,
        # Fix indicator_clean mapping - remove _rds suffix and match with labels
        indicator_clean = get_comparable_indicators()$labels[str_remove(outcome_var, "_rds$")],
        point_estimate = comp$primary_estimate,
        methods_available = paste(comp$methods_available, collapse = ", "),
        
        # Simple bootstrap (if available)
        simple_ci_lower = if(!is.null(comp$simple_ci)) comp$simple_ci[1] else NA,
        simple_ci_upper = if(!is.null(comp$simple_ci)) comp$simple_ci[2] else NA,
        
        # Neighborhood bootstrap (if available)
        nb_ci_lower = if(!is.null(comp$neighborhood_ci)) comp$neighborhood_ci[1] else NA,
        nb_ci_upper = if(!is.null(comp$neighborhood_ci)) comp$neighborhood_ci[2] else NA,
        
        # Tree bootstrap (if available)
        tb_ci_lower = if(!is.null(comp$tree_ci)) comp$tree_ci[1] else NA,
        tb_ci_upper = if(!is.null(comp$tree_ci)) comp$tree_ci[2] else NA,
        
        # Combined (conservative)
        combined_ci_lower = comp$combined_ci_lower,
        combined_ci_upper = comp$combined_ci_upper,
        combined_ci_width = comp$combined_ci_upper - comp$combined_ci_lower,
        
        # Formatted for publication
        estimate_with_ci = format_estimate_with_ci(
          comp$primary_estimate, 
          comp$combined_ci_lower, 
          comp$combined_ci_upper
        ),
        
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    rownames(summary_table) <- NULL
    
    # Order by estimate size
    summary_table <- summary_table[order(-summary_table$point_estimate), ]
    
  } else {
    summary_table <- data.frame(message = "No bootstrap results available")
  }
  
  cat("Bootstrap summary table created with", nrow(summary_table), "indicators\n\n")
  
  return(summary_table)
}

# ============================================================================
# CREATE SIMPLE BOOTSTRAP SUMMARY TABLE
# ============================================================================

create_simple_bootstrap_summary_table <- function(simple_results) {
  
  cat("=== Creating Simple Bootstrap Summary Table ===\n")
  
  summary_rows <- list()
  
  for (outcome_var in names(simple_results)) {
    
    result <- simple_results[[outcome_var]]
    
    if (!"error" %in% names(result)) {
      
      summary_rows[[outcome_var]] <- data.frame(
        indicator = outcome_var,
        # Fix indicator_clean mapping - remove _rds suffix and match with labels
        indicator_clean = get_comparable_indicators()$labels[str_remove(outcome_var, "_rds$")],
        point_estimate = result$original_estimate,
        bootstrap_se = result$bootstrap_se,
        ci_lower = result$ci_lower,
        ci_upper = result$ci_upper,
        ci_width = result$ci_upper - result$ci_lower,
        method = result$method,
        n_bootstrap = result$n_bootstrap,
        confidence_level = result$confidence_level,
        
        # Formatted for publication
        estimate_with_ci = format_estimate_with_ci(
          result$original_estimate, 
          result$ci_lower, 
          result$ci_upper
        ),
        
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    rownames(summary_table) <- NULL
    
    # Order by estimate size
    summary_table <- summary_table[order(-summary_table$point_estimate), ]
    
  } else {
    summary_table <- data.frame(message = "No bootstrap results available")
  }
  
  cat("Simple bootstrap summary table created with", nrow(summary_table), "indicators\n\n")
  
  return(summary_table)
}

# ============================================================================
# MAIN BOOTSTRAP ANALYSIS
# ============================================================================

main_bootstrap_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting bootstrap analysis for RDS estimates...\n\n")
  
  # Step 1: Simple bootstrap (working implementation)
  simple_results <- get_simple_bootstrap_ci(
    outcome_vars = bootstrap_config$outcome_vars,
    n_bootstrap = bootstrap_config$n_bootstrap,
    confidence_level = bootstrap_config$confidence_level,
    seed = bootstrap_config$seed
  )
  
  # Step 2: Neighborhood bootstrap (if enabled)
  if (bootstrap_config$use_neighborhood_bootstrap) {
    neighborhood_results <- run_neighborhood_bootstrap(
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    neighborhood_results <- list()
  }
  
  # Step 3: Tree bootstrap (if enabled)  
  if (bootstrap_config$use_tree_bootstrap) {
    tree_results <- run_tree_bootstrap(
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    tree_results <- list()
  }
  
  # Step 4: Combine results from all methods
  combined_results <- combine_bootstrap_results(simple_results, neighborhood_results, tree_results)
  
  # Step 5: Create summary table
  if (exists("combined_results") && length(combined_results) > 0) {
    summary_table <- create_bootstrap_summary_table(combined_results)
  } else {
    summary_table <- create_simple_bootstrap_summary_table(simple_results)
  }
  
  # Step 6: Compile final results
  final_results <- list(
    simple_results = simple_results,
    neighborhood_results = neighborhood_results,
    tree_results = tree_results,
    combined_results = if(exists("combined_results")) combined_results else list(),
    summary_table = summary_table,
    
    config = bootstrap_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd)
    )
  )
  
  # Save results
  save(final_results, file = here("output", "bootstrap_results.RData"))
  
  # Save summary table
  write.csv(summary_table, 
            here("output", "tables", "bootstrap_confidence_intervals.csv"),
            row.names = FALSE)
  
  cat("=== Bootstrap Analysis Complete ===\n")
  cat("Confidence intervals (", bootstrap_config$confidence_level * 100, "%):\n")
  
  # Only try to print specific columns if they exist
  if ("indicator_clean" %in% names(summary_table) && "estimate_with_ci" %in% names(summary_table)) {
    print(summary_table[c("indicator_clean", "estimate_with_ci")])
  } else {
    print(summary_table)
  }
  
  cat("\nResults saved to: output/bootstrap_results.RData\n")
  cat("Summary table saved to: output/tables/bootstrap_confidence_intervals.csv\n\n")
  
  return(final_results)
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
    cat("Running standalone bootstrap analysis\n")
  }
  
  # Run analysis
  bootstrap_results <- main_bootstrap_analysis()
  
} else {
  cat("Bootstrap analysis script loaded (execution skipped)\n")
}


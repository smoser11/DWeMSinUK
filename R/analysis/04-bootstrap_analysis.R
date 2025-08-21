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
  # Focus on comparable indicators and preferred method
  outcome_vars = get_comparable_indicators()$rds_vars,
  preferred_method = "RDS_SS",
  main_population_size = 980000,
  
  # Bootstrap parameters
  n_bootstrap = 1000,
  confidence_level = 0.95,
  quantiles = c(0.025, 0.975),
  
  # Methods to use
  use_neighborhood_bootstrap = TRUE,
  use_tree_bootstrap = TRUE,
  
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
# NETWORK PREPARATION
# ============================================================================

prepare_network_for_bootstrap <- function() {
  
  cat("=== Preparing Network Structure ===\n")
  
  # Create edges data frame (exclude seeds with recruiter.id == -1)
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  cat("Network edges:", nrow(edges_df), "\n")
  
  # Create node mapping to avoid duplicate vertex names
  all_nodes <- unique(c(edges_df$from, edges_df$to, rd.dd$id))
  node_map <- setNames(1:length(all_nodes), all_nodes)
  
  # Remap edges with sequential node IDs
  edges_mapped <- data.frame(
    from = node_map[as.character(edges_df$from)],
    to = node_map[as.character(edges_df$to)]
  )
  
  # Create igraph object
  network_graph <- graph_from_data_frame(edges_mapped, directed = TRUE)
  
  # Prepare traits data frame using CE's comparable indicators
  traits_df <- data.frame(
    node_id = 1:length(rd.dd$id),
    document_withholding = rd.dd$document_withholding_rds,
    pay_issues = rd.dd$pay_issues_rds,
    threats_abuse = rd.dd$threats_abuse_rds,
    excessive_hours = rd.dd$excessive_hours_rds,
    access_to_help = rd.dd$access_to_help_rds
  )
  
  cat("Network prepared with", vcount(network_graph), "nodes and", ecount(network_graph), "edges\n\n")
  
  return(list(
    network = network_graph,
    edges = edges_mapped,
    traits = traits_df,
    node_map = node_map
  ))
}

# ============================================================================
# NEIGHBORHOOD BOOTSTRAP
# ============================================================================

run_neighborhood_bootstrap <- function(network_data, outcome_vars, n_bootstrap = 1000, 
                                     confidence_level = 0.95, seed = 12345) {
  
  cat("=== Neighborhood Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  neighborhood_results <- list()
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(network_data$traits))) {
      cat("Warning: Variable", outcome_var, "not found in traits data\n")
      next
    }
    
    # Extract trait vector
    trait_values <- network_data$traits[[outcome_var]]
    
    # Remove NA values
    complete_cases <- !is.na(trait_values)
    trait_clean <- trait_values[complete_cases]
    
    if (length(trait_clean) == 0) {
      cat("Warning: No complete cases for", outcome_var, "\n")
      next
    }
    
    tryCatch({
      
      # Run neighborhood bootstrap
      cat("  Running neighborhood bootstrap (n =", n_bootstrap, ")...\n")
      
      # Use Neighboot package
      neighboot_result <- neighb.boot(
        graph = network_data$network,
        trait = trait_clean,
        B = n_bootstrap
      )
      
      # Calculate confidence intervals
      alpha <- 1 - confidence_level
      quantiles <- c(alpha/2, 1 - alpha/2)
      
      bootstrap_estimates <- neighboot_result$bootstrap_estimates
      
      ci <- quantile(bootstrap_estimates, probs = quantiles, na.rm = TRUE)
      
      result <- list(
        variable = outcome_var,
        method = "neighborhood_bootstrap",
        original_estimate = mean(trait_clean),
        bootstrap_mean = mean(bootstrap_estimates, na.rm = TRUE),
        bootstrap_sd = sd(bootstrap_estimates, na.rm = TRUE),
        ci_lower = ci[1],
        ci_upper = ci[2],
        confidence_level = confidence_level,
        n_bootstrap = n_bootstrap,
        bootstrap_samples = bootstrap_estimates
      )
      
      neighborhood_results[[outcome_var]] <- result
      
      cat("  Completed - CI: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")
      
    }, error = function(e) {
      cat("  Error in neighborhood bootstrap for", outcome_var, ":", e$message, "\n")
      neighborhood_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "neighborhood_bootstrap", 
        error = e$message
      )
    })
  }
  
  cat("Neighborhood bootstrap completed\n\n")
  return(neighborhood_results)
}

# ============================================================================
# TREE BOOTSTRAP  
# ============================================================================

run_tree_bootstrap <- function(network_data, outcome_vars, n_bootstrap = 1000,
                              confidence_level = 0.95, seed = 12345) {
  
  cat("=== Tree Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  tree_results <- list()
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(network_data$traits))) {
      cat("Warning: Variable", outcome_var, "not found in traits data\n")
      next
    }
    
    # Extract trait vector
    trait_values <- network_data$traits[[outcome_var]]
    
    # Remove NA values
    complete_cases <- !is.na(trait_values)
    trait_clean <- trait_values[complete_cases]
    
    if (length(trait_clean) == 0) {
      cat("Warning: No complete cases for", outcome_var, "\n")
      next
    }
    
    tryCatch({
      
      # Run tree bootstrap using RDStreeboot package
      cat("  Running tree bootstrap (n =", n_bootstrap, ")...\n")
      
      # Create RDS data object for tree bootstrap
      rds_data_subset <- rd.dd[complete_cases, ]
      
      tree_result <- RDStreeboot(
        rds.data = rds_data_subset,
        outcome.variable = outcome_var,
        B = n_bootstrap,
        seed = seed
      )
      
      # Extract bootstrap estimates
      bootstrap_estimates <- tree_result$bootstrap_estimates
      
      # Calculate confidence intervals
      alpha <- 1 - confidence_level
      quantiles <- c(alpha/2, 1 - alpha/2)
      
      ci <- quantile(bootstrap_estimates, probs = quantiles, na.rm = TRUE)
      
      result <- list(
        variable = outcome_var,
        method = "tree_bootstrap",
        original_estimate = mean(trait_clean),
        bootstrap_mean = mean(bootstrap_estimates, na.rm = TRUE),
        bootstrap_sd = sd(bootstrap_estimates, na.rm = TRUE),
        ci_lower = ci[1],
        ci_upper = ci[2],
        confidence_level = confidence_level,
        n_bootstrap = n_bootstrap,
        bootstrap_samples = bootstrap_estimates
      )
      
      tree_results[[outcome_var]] <- result
      
      cat("  Completed - CI: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")
      
    }, error = function(e) {
      cat("  Error in tree bootstrap for", outcome_var, ":", e$message, "\n")
      tree_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "tree_bootstrap",
        error = e$message
      )
    })
  }
  
  cat("Tree bootstrap completed\n\n")
  return(tree_results)
}

# ============================================================================
# COMBINE BOOTSTRAP METHODS
# ============================================================================

combine_bootstrap_results <- function(neighborhood_results, tree_results) {
  
  cat("=== Combining Bootstrap Results ===\n")
  
  combined_results <- list()
  all_vars <- unique(c(names(neighborhood_results), names(tree_results)))
  
  for (outcome_var in all_vars) {
    
    var_result <- list(variable = outcome_var)
    
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
    
    # Create summary comparison
    if (outcome_var %in% names(neighborhood_results) && 
        outcome_var %in% names(tree_results) &&
        !"error" %in% names(neighborhood_results[[outcome_var]]) &&
        !"error" %in% names(tree_results[[outcome_var]])) {
      
      nb <- neighborhood_results[[outcome_var]]
      tb <- tree_results[[outcome_var]]
      
      var_result$comparison <- list(
        original_estimate = nb$original_estimate,  # Should be same for both
        
        neighborhood_ci = c(nb$ci_lower, nb$ci_upper),
        tree_ci = c(tb$ci_lower, tb$ci_upper),
        
        ci_width_neighborhood = nb$ci_upper - nb$ci_lower,
        ci_width_tree = tb$ci_upper - tb$ci_lower,
        
        # Conservative approach: use wider CI
        combined_ci_lower = min(nb$ci_lower, tb$ci_lower),
        combined_ci_upper = max(nb$ci_upper, tb$ci_upper)
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
        indicator_clean = get_comparable_indicators()$labels[outcome_var],
        point_estimate = comp$original_estimate,
        
        # Neighborhood bootstrap
        nb_ci_lower = result$neighborhood_bootstrap$ci_lower,
        nb_ci_upper = result$neighborhood_bootstrap$ci_upper,
        nb_ci_width = comp$ci_width_neighborhood,
        
        # Tree bootstrap
        tb_ci_lower = result$tree_bootstrap$ci_lower,
        tb_ci_upper = result$tree_bootstrap$ci_upper,
        tb_ci_width = comp$ci_width_tree,
        
        # Combined (conservative)
        combined_ci_lower = comp$combined_ci_lower,
        combined_ci_upper = comp$combined_ci_upper,
        combined_ci_width = comp$combined_ci_upper - comp$combined_ci_lower,
        
        # Formatted for publication
        estimate_with_ci = format_estimate_with_ci(
          comp$original_estimate, 
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
# MAIN BOOTSTRAP ANALYSIS
# ============================================================================

main_bootstrap_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting bootstrap analysis for RDS estimates...\n\n")
  
  # Step 1: Prepare network structure
  network_data <- prepare_network_for_bootstrap()
  
  # Step 2: Neighborhood bootstrap
  if (bootstrap_config$use_neighborhood_bootstrap) {
    neighborhood_results <- run_neighborhood_bootstrap(
      network_data = network_data,
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    neighborhood_results <- list()
  }
  
  # Step 3: Tree bootstrap
  if (bootstrap_config$use_tree_bootstrap) {
    tree_results <- run_tree_bootstrap(
      network_data = network_data,
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    tree_results <- list()
  }
  
  # Step 4: Combine results
  combined_results <- combine_bootstrap_results(neighborhood_results, tree_results)
  
  # Step 5: Create summary table
  summary_table <- create_bootstrap_summary_table(combined_results)
  
  # Step 6: Compile final results
  final_results <- list(
    neighborhood_results = neighborhood_results,
    tree_results = tree_results,
    combined_results = combined_results,
    summary_table = summary_table,
    
    config = bootstrap_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      network_nodes = vcount(network_data$network),
      network_edges = ecount(network_data$network)
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
  print(summary_table[c("indicator_clean", "estimate_with_ci")])
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




# ============================================================================
# CONFIGURATION
# ============================================================================

bootstrap_config <- list(
  # Focus on comparable indicators and preferred method
  outcome_vars = get_comparable_indicators()$rds_vars,
  preferred_method = "RDS_SS",
  main_population_size = 980000,
  
  # Bootstrap parameters
  n_bootstrap = 1000,
  confidence_level = 0.95,
  quantiles = c(0.025, 0.975),
  
  # Methods to use
  use_neighborhood_bootstrap = TRUE,
  use_tree_bootstrap = TRUE,
  
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
# NETWORK PREPARATION
# ============================================================================

prepare_network_for_bootstrap <- function() {
  
  cat("=== Preparing Network Structure ===\n")
  
  # Create edges data frame (exclude seeds with recruiter.id == -1)
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  cat("Network edges:", nrow(edges_df), "\n")
  
  # Create node mapping to avoid duplicate vertex names
  all_nodes <- unique(c(edges_df$from, edges_df$to, rd.dd$id))
  node_map <- setNames(1:length(all_nodes), all_nodes)
  
  # Remap edges with sequential node IDs
  edges_mapped <- data.frame(
    from = node_map[as.character(edges_df$from)],
    to = node_map[as.character(edges_df$to)]
  )
  
  # Create igraph object
  network_graph <- graph_from_data_frame(edges_mapped, directed = TRUE)
  
  # Prepare traits data frame using CE's comparable indicators
  traits_df <- data.frame(
    node_id = 1:length(rd.dd$id),
    document_withholding = rd.dd$document_withholding_rds,
    pay_issues = rd.dd$pay_issues_rds,
    threats_abuse = rd.dd$threats_abuse_rds,
    excessive_hours = rd.dd$excessive_hours_rds,
    access_to_help = rd.dd$access_to_help_rds
  )
  
  cat("Network prepared with", vcount(network_graph), "nodes and", ecount(network_graph), "edges\n\n")
  
  return(list(
    network = network_graph,
    edges = edges_mapped,
    traits = traits_df,
    node_map = node_map
  ))
}

# ============================================================================
# NEIGHBORHOOD BOOTSTRAP
# ============================================================================

run_neighborhood_bootstrap <- function(network_data, outcome_vars, n_bootstrap = 1000, 
                                     confidence_level = 0.95, seed = 12345) {
  
  cat("=== Neighborhood Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  neighborhood_results <- list()
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(network_data$traits))) {
      cat("Warning: Variable", outcome_var, "not found in traits data\n")
      next
    }
    
    # Extract trait vector
    trait_values <- network_data$traits[[outcome_var]]
    
    # Remove NA values
    complete_cases <- !is.na(trait_values)
    trait_clean <- trait_values[complete_cases]
    
    if (length(trait_clean) == 0) {
      cat("Warning: No complete cases for", outcome_var, "\n")
      next
    }
    
    tryCatch({
      
      # Run neighborhood bootstrap
      cat("  Running neighborhood bootstrap (n =", n_bootstrap, ")...\n")
      
      # Use Neighboot package
      neighboot_result <- neighb.boot(
        graph = network_data$network,
        trait = trait_clean,
        B = n_bootstrap
      )
      
      # Calculate confidence intervals
      alpha <- 1 - confidence_level
      quantiles <- c(alpha/2, 1 - alpha/2)
      
      bootstrap_estimates <- neighboot_result$bootstrap_estimates
      
      ci <- quantile(bootstrap_estimates, probs = quantiles, na.rm = TRUE)
      
      result <- list(
        variable = outcome_var,
        method = "neighborhood_bootstrap",
        original_estimate = mean(trait_clean),
        bootstrap_mean = mean(bootstrap_estimates, na.rm = TRUE),
        bootstrap_sd = sd(bootstrap_estimates, na.rm = TRUE),
        ci_lower = ci[1],
        ci_upper = ci[2],
        confidence_level = confidence_level,
        n_bootstrap = n_bootstrap,
        bootstrap_samples = bootstrap_estimates
      )
      
      neighborhood_results[[outcome_var]] <- result
      
      cat("  Completed - CI: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")
      
    }, error = function(e) {
      cat("  Error in neighborhood bootstrap for", outcome_var, ":", e$message, "\n")
      neighborhood_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "neighborhood_bootstrap", 
        error = e$message
      )
    })
  }
  
  cat("Neighborhood bootstrap completed\n\n")
  return(neighborhood_results)
}

# ============================================================================
# TREE BOOTSTRAP  
# ============================================================================

run_tree_bootstrap <- function(network_data, outcome_vars, n_bootstrap = 1000,
                              confidence_level = 0.95, seed = 12345) {
  
  cat("=== Tree Bootstrap Analysis ===\n")
  
  set.seed(seed)
  
  tree_results <- list()
  
  for (outcome_var in outcome_vars) {
    
    cat("Processing:", outcome_var, "\n")
    
    if (!(outcome_var %in% names(network_data$traits))) {
      cat("Warning: Variable", outcome_var, "not found in traits data\n")
      next
    }
    
    # Extract trait vector
    trait_values <- network_data$traits[[outcome_var]]
    
    # Remove NA values
    complete_cases <- !is.na(trait_values)
    trait_clean <- trait_values[complete_cases]
    
    if (length(trait_clean) == 0) {
      cat("Warning: No complete cases for", outcome_var, "\n")
      next
    }
    
    tryCatch({
      
      # Run tree bootstrap using RDStreeboot package
      cat("  Running tree bootstrap (n =", n_bootstrap, ")...\n")
      
      # Create RDS data object for tree bootstrap
      rds_data_subset <- rd.dd[complete_cases, ]
      
      tree_result <- RDStreeboot(
        rds.data = rds_data_subset,
        outcome.variable = outcome_var,
        B = n_bootstrap,
        seed = seed
      )
      
      # Extract bootstrap estimates
      bootstrap_estimates <- tree_result$bootstrap_estimates
      
      # Calculate confidence intervals
      alpha <- 1 - confidence_level
      quantiles <- c(alpha/2, 1 - alpha/2)
      
      ci <- quantile(bootstrap_estimates, probs = quantiles, na.rm = TRUE)
      
      result <- list(
        variable = outcome_var,
        method = "tree_bootstrap",
        original_estimate = mean(trait_clean),
        bootstrap_mean = mean(bootstrap_estimates, na.rm = TRUE),
        bootstrap_sd = sd(bootstrap_estimates, na.rm = TRUE),
        ci_lower = ci[1],
        ci_upper = ci[2],
        confidence_level = confidence_level,
        n_bootstrap = n_bootstrap,
        bootstrap_samples = bootstrap_estimates
      )
      
      tree_results[[outcome_var]] <- result
      
      cat("  Completed - CI: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")
      
    }, error = function(e) {
      cat("  Error in tree bootstrap for", outcome_var, ":", e$message, "\n")
      tree_results[[outcome_var]] <- list(
        variable = outcome_var,
        method = "tree_bootstrap",
        error = e$message
      )
    })
  }
  
  cat("Tree bootstrap completed\n\n")
  return(tree_results)
}

# ============================================================================
# COMBINE BOOTSTRAP METHODS
# ============================================================================

combine_bootstrap_results <- function(neighborhood_results, tree_results) {
  
  cat("=== Combining Bootstrap Results ===\n")
  
  combined_results <- list()
  all_vars <- unique(c(names(neighborhood_results), names(tree_results)))
  
  for (outcome_var in all_vars) {
    
    var_result <- list(variable = outcome_var)
    
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
    
    # Create summary comparison
    if (outcome_var %in% names(neighborhood_results) && 
        outcome_var %in% names(tree_results) &&
        !"error" %in% names(neighborhood_results[[outcome_var]]) &&
        !"error" %in% names(tree_results[[outcome_var]])) {
      
      nb <- neighborhood_results[[outcome_var]]
      tb <- tree_results[[outcome_var]]
      
      var_result$comparison <- list(
        original_estimate = nb$original_estimate,  # Should be same for both
        
        neighborhood_ci = c(nb$ci_lower, nb$ci_upper),
        tree_ci = c(tb$ci_lower, tb$ci_upper),
        
        ci_width_neighborhood = nb$ci_upper - nb$ci_lower,
        ci_width_tree = tb$ci_upper - tb$ci_lower,
        
        # Conservative approach: use wider CI
        combined_ci_lower = min(nb$ci_lower, tb$ci_lower),
        combined_ci_upper = max(nb$ci_upper, tb$ci_upper)
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
        indicator_clean = get_comparable_indicators()$labels[outcome_var],
        point_estimate = comp$original_estimate,
        
        # Neighborhood bootstrap
        nb_ci_lower = result$neighborhood_bootstrap$ci_lower,
        nb_ci_upper = result$neighborhood_bootstrap$ci_upper,
        nb_ci_width = comp$ci_width_neighborhood,
        
        # Tree bootstrap
        tb_ci_lower = result$tree_bootstrap$ci_lower,
        tb_ci_upper = result$tree_bootstrap$ci_upper,
        tb_ci_width = comp$ci_width_tree,
        
        # Combined (conservative)
        combined_ci_lower = comp$combined_ci_lower,
        combined_ci_upper = comp$combined_ci_upper,
        combined_ci_width = comp$combined_ci_upper - comp$combined_ci_lower,
        
        # Formatted for publication
        estimate_with_ci = format_estimate_with_ci(
          comp$original_estimate, 
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
# MAIN BOOTSTRAP ANALYSIS
# ============================================================================

main_bootstrap_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting bootstrap analysis for RDS estimates...\n\n")
  
  # Step 1: Prepare network structure
  network_data <- prepare_network_for_bootstrap()
  
  # Step 2: Neighborhood bootstrap
  if (bootstrap_config$use_neighborhood_bootstrap) {
    neighborhood_results <- run_neighborhood_bootstrap(
      network_data = network_data,
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    neighborhood_results <- list()
  }
  
  # Step 3: Tree bootstrap
  if (bootstrap_config$use_tree_bootstrap) {
    tree_results <- run_tree_bootstrap(
      network_data = network_data,
      outcome_vars = bootstrap_config$outcome_vars,
      n_bootstrap = bootstrap_config$n_bootstrap,
      confidence_level = bootstrap_config$confidence_level,
      seed = bootstrap_config$seed
    )
  } else {
    tree_results <- list()
  }
  
  # Step 4: Combine results
  combined_results <- combine_bootstrap_results(neighborhood_results, tree_results)
  
  # Step 5: Create summary table
  summary_table <- create_bootstrap_summary_table(combined_results)
  
  # Step 6: Compile final results
  final_results <- list(
    neighborhood_results = neighborhood_results,
    tree_results = tree_results,
    combined_results = combined_results,
    summary_table = summary_table,
    
    config = bootstrap_config,
    metadata = list(
      timestamp = Sys.time(),
      sample_size = nrow(rd.dd),
      network_nodes = vcount(network_data$network),
      network_edges = ecount(network_data$network)
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
  print(summary_table[c("indicator_clean", "estimate_with_ci")])
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
>>>>>>> 462426f7cf8f54f22bc15c0c9c01f1f4fddf363a

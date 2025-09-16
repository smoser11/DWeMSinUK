# neighborhood_bootstrap.R
# Wrapper function for the neighborhood bootstrap resampling method
# Part of Step 1 of the three-step bootstrap procedure for NSUM-RDS

# Load required libraries
library(tidyverse)
library(igraph)
library(here)

# Source the existing Neighboot package functions
source(here("CRAN", "Neighboot", "R", "neighb.R"))

# =============================================================================
# NEIGHBORHOOD BOOTSTRAP RESAMPLING WRAPPER
# =============================================================================

#' Neighborhood Bootstrap Resampling for RDS Data
#'
#' This function wraps the existing neighb() function from the Neighboot package
#' to perform neighborhood bootstrap resampling of RDS data. It handles data
#' format conversion and returns bootstrap samples for use in NSUM estimation.
#'
#' @param rds_data Data frame containing RDS sample with required variables:
#'                 id, recruiter.id, network.size (or degree), and other survey variables
#' @param n_bootstrap Integer, number of bootstrap replicates (default: 1000)
#' @param return_samples Logical, whether to return individual bootstrap samples
#'                       or just the bootstrap statistics (default: FALSE)
#' @param seed Integer, random seed for reproducibility (default: NULL)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return If return_samples = FALSE (default):
#'         List containing bootstrap statistics for each variable
#'         If return_samples = TRUE:
#'         List containing both statistics and individual bootstrap samples
#'
#' @details
#' The neighborhood bootstrap method (Yauck & Moodie, 2022) resamples by selecting
#' respondents and replacing them with their neighbors in the recruitment graph.
#' This maintains local network dependencies and simulates resampling from the
#' underlying contact network.
#'
#' The function internally:
#' 1. Converts standard RDS data frame to neighb() required format
#' 2. Performs neighborhood bootstrap using existing implementation
#' 3. Optionally extracts individual bootstrap samples for further analysis
#'
#' @examples
#' # Load processed data
#' load("data/processed/prepared_data.RData")
#'
#' # Basic neighborhood bootstrap for confidence intervals
#' nb_results <- neighborhood_bootstrap_resample(
#'   rds_data = prepared_data,
#'   n_bootstrap = 1000
#' )
#'
#' # Get bootstrap samples for NSUM estimation
#' nb_samples <- neighborhood_bootstrap_resample(
#'   rds_data = prepared_data,
#'   n_bootstrap = 100,
#'   return_samples = TRUE
#' )
#'
neighborhood_bootstrap_resample <- function(rds_data,
                                          n_bootstrap = 1000,
                                          return_samples = FALSE,
                                          seed = NULL,
                                          verbose = TRUE) {

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Record start time
  start_time <- Sys.time()

  if (verbose) {
    cat(sprintf("Starting neighborhood bootstrap resampling with %d replicates...\n", n_bootstrap))
  }

  # Validate and convert data format
  neighb_data <- convert_to_neighb_format(rds_data, verbose)

  # Get variables for bootstrap analysis
  trait_vars <- names(neighb_data$traits)

  if (verbose) {
    cat(sprintf("Bootstrap variables: %s\n", paste(trait_vars, collapse = ", ")))
  }

  # Perform neighborhood bootstrap using existing neighb() function
  # This returns confidence intervals and standard errors
  neighb_results <- neighb(
    RDS.data = neighb_data,
    quant = c(0.025, 0.975),
    method = "percentile",
    B = n_bootstrap
  )

  # Prepare results
  results <- list(
    bootstrap_statistics = neighb_results,
    method = "neighborhood",
    parameters = list(
      n_bootstrap = n_bootstrap,
      seed = seed,
      return_samples = return_samples
    ),
    metadata = list(
      original_sample_size = nrow(rds_data),
      trait_variables = trait_vars,
      start_time = start_time
    )
  )

  # Optionally extract individual bootstrap samples
  if (return_samples) {
    if (verbose) {
      cat("Extracting individual bootstrap samples...\n")
    }

    bootstrap_samples <- extract_neighb_bootstrap_samples(
      neighb_data, n_bootstrap, verbose
    )

    results$bootstrap_samples <- bootstrap_samples
  }

  # Record end time and duration
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  results$metadata$end_time <- end_time
  results$metadata$duration_seconds <- duration

  if (verbose) {
    cat(sprintf("Neighborhood bootstrap completed in %.2f seconds\n", duration))
  }

  return(results)
}

# =============================================================================
# DATA FORMAT CONVERSION FUNCTIONS
# =============================================================================

#' Convert RDS Data Frame to neighb() Format
#'
#' Converts standard RDS data frame format to the specific list structure
#' required by the neighb() function.
#'
#' @param rds_data Data frame with RDS sample
#' @param verbose Logical for progress messages
#' @return List in neighb() format with nodes, edges, traits, degree
convert_to_neighb_format <- function(rds_data, verbose = TRUE) {

  # Validate required columns
  required_cols <- c("id", "recruiter.id")
  missing_cols <- setdiff(required_cols, names(rds_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get degree information
  if ("network.size" %in% names(rds_data)) {
    degree_var <- "network.size"
  } else if ("numRef" %in% names(rds_data)) {
    degree_var <- "numRef"
  } else if ("degree" %in% names(rds_data)) {
    degree_var <- "degree"
  } else if ("q13" %in% names(rds_data)) {
    degree_var <- "q13"
  } else {
    stop("No degree variable found (network.size, numRef, degree, or q13)")
  }

  if (verbose) {
    cat(sprintf("Using %s as degree variable\n", degree_var))
  }

  # Use row indices as node IDs to avoid string matching issues
  n_nodes <- nrow(rds_data)
  node_ids <- 1:n_nodes

  # Create edge list using row indices (following working example)
  edges_list <- list()
  for (i in 1:n_nodes) {
    recruiter_id <- rds_data$recruiter.id[i]
    if (!is.na(recruiter_id) && recruiter_id != "-1") {
      # Find the row index of the recruiter
      recruiter_row <- which(rds_data$id == recruiter_id)
      if (length(recruiter_row) == 1) {
        edges_list[[length(edges_list) + 1]] <- data.frame(
          node1 = recruiter_row,
          node2 = i
        )
      }
    }
  }

  # Combine edges or create empty data frame
  if (length(edges_list) > 0) {
    edges_df <- do.call(rbind, edges_list)
  } else {
    edges_df <- data.frame(node1 = integer(0), node2 = integer(0))
  }

  # Prepare traits data frame with relevant variables
  trait_vars <- get_bootstrap_trait_variables(rds_data)
  traits_df <- rds_data[, trait_vars, drop = FALSE]

  # Handle factor variables (convert to numeric for bootstrap)
  for (var in names(traits_df)) {
    if (is.factor(traits_df[[var]])) {
      traits_df[[var]] <- as.numeric(traits_df[[var]]) - 1  # Convert to 0-based
    }
  }

  # Ensure at least 2 columns for neighb() function requirement
  if (ncol(traits_df) == 1) {
    traits_df$dummy_var <- 0  # Add dummy variable if only one trait
  }

  if (verbose) {
    cat(sprintf("Created network with %d nodes, %d edges\n",
                length(node_ids), nrow(edges_df)))
    cat(sprintf("Trait variables: %s\n", paste(trait_vars, collapse = ", ")))
  }

  # Create neighb() format list (using row-based indexing)
  neighb_data <- list(
    nodes = node_ids,
    edges = edges_df,  # Data frame format, not list
    traits = traits_df,
    degree = rds_data[[degree_var]]
  )

  # Validate the structure
  validate_neighb_format(neighb_data)

  return(neighb_data)
}

#' Get Bootstrap Trait Variables
#'
#' Identifies which variables should be included in bootstrap analysis
#'
#' @param rds_data RDS data frame
#' @return Character vector of variable names
get_bootstrap_trait_variables <- function(rds_data) {

  # Exclude structural variables
  exclude_vars <- c("id", "recruiter.id", "network.size", "numRef",
                    "degree", "q13", "wave", "seed", "recruiter")

  # Get CE's comparable indicators if they exist
  comparable_indicators <- c(
    "document_withholding_rds", "pay_issues_rds", "threats_abuse_rds",
    "excessive_hours_rds", "access_to_help_rds",
    "document_withholding_nsum", "pay_issues_nsum", "threats_abuse_nsum",
    "excessive_hours_nsum", "access_to_help_nsum",
    "known_network_size"
  )

  # Legacy indicators
  legacy_indicators <- c("zQ36", "zQ80", "sum_categories", "q36", "q80")

  # Prefer CE's indicators, fall back to legacy, then include all numeric/logical
  available_vars <- names(rds_data)

  # First priority: CE's comparable indicators
  priority_vars <- intersect(comparable_indicators, available_vars)

  # Second priority: Legacy indicators
  if (length(priority_vars) == 0) {
    priority_vars <- intersect(legacy_indicators, available_vars)
  }

  # Third priority: All numeric/logical variables (excluding structural)
  if (length(priority_vars) == 0) {
    candidate_vars <- setdiff(available_vars, exclude_vars)
    numeric_vars <- candidate_vars[sapply(rds_data[candidate_vars], function(x) {
      is.numeric(x) || is.logical(x)
    })]
    priority_vars <- numeric_vars
  }

  # Ensure we have at least one variable
  if (length(priority_vars) == 0) {
    stop("No suitable trait variables found for bootstrap analysis")
  }

  return(priority_vars)
}

#' Validate neighb() Format
#'
#' Validates that the data structure meets neighb() requirements
#'
#' @param neighb_data List in neighb() format
validate_neighb_format <- function(neighb_data) {

  required_elements <- c("nodes", "edges", "traits", "degree")
  missing_elements <- setdiff(required_elements, names(neighb_data))

  if (length(missing_elements) > 0) {
    stop("Missing required elements for neighb(): ",
         paste(missing_elements, collapse = ", "))
  }

  # Check nodes
  if (length(neighb_data$nodes) == 0) {
    stop("No nodes in data")
  }

  # Check edges format
  if (!is.data.frame(neighb_data$edges) ||
      !all(c("node1", "node2") %in% names(neighb_data$edges))) {
    stop("Edges must be data frame with columns 'node1' and 'node2'")
  }

  # Check traits
  if (!is.data.frame(neighb_data$traits) || nrow(neighb_data$traits) == 0) {
    stop("Traits must be non-empty data frame")
  }

  # Check degree
  if (length(neighb_data$degree) == 0) {
    stop("Degree vector cannot be empty")
  }

  return(TRUE)
}

# =============================================================================
# BOOTSTRAP SAMPLE EXTRACTION
# =============================================================================

#' Extract Individual Bootstrap Samples from Neighborhood Bootstrap
#'
#' This function runs the neighborhood bootstrap algorithm manually to extract
#' individual bootstrap samples, rather than just the summary statistics.
#'
#' @param neighb_data Data in neighb() format
#' @param n_bootstrap Number of bootstrap samples
#' @param verbose Progress messages
#' @return List of bootstrap samples
extract_neighb_bootstrap_samples <- function(neighb_data, n_bootstrap, verbose) {

  # Replicate the .Nb() internal function from neighb.R
  # This extracts the neighborhood resampling logic

  # Create igraph object
  RDS.gr <- igraph::graph_from_data_frame(
    neighb_data$edges,
    directed = FALSE,
    vertices = data.frame(
      id = neighb_data$nodes,
      neighb_data$traits[as.character(neighb_data$nodes), , drop = FALSE]
    )
  )

  e.deg <- igraph::degree(RDS.gr, mode = "total")
  cr <- mean(e.deg)

  # Calculate sample size for each bootstrap replicate
  sz <- round(length(neighb_data$nodes) / cr)

  bootstrap_samples <- list()

  if (verbose && n_bootstrap > 100) {
    cat("Extracting bootstrap samples: ")
  }

  for (b in 1:n_bootstrap) {
    if (verbose && b %% 100 == 0) {
      cat(sprintf("%d ", b))
    }

    # Sample initial nodes
    xx.s <- sample(1:length(neighb_data$nodes), size = sz, replace = TRUE)

    # Get their neighbors
    x.neig <- as.numeric(unlist(igraph::ego(
      RDS.gr,
      order = 1,
      nodes = xx.s,
      mode = "all",
      mindist = 1
    )))

    # Create bootstrap sample from neighbors
    if (length(x.neig) > 0) {
      bootstrap_indices <- x.neig
    } else {
      # Fallback to original sample if no neighbors found
      bootstrap_indices <- xx.s
    }

    # Extract the bootstrap sample data
    bootstrap_sample <- create_bootstrap_sample_from_indices(
      neighb_data, bootstrap_indices
    )

    bootstrap_samples[[b]] <- bootstrap_sample
  }

  if (verbose && n_bootstrap > 100) {
    cat("\n")
  }

  return(bootstrap_samples)
}

#' Create Bootstrap Sample from Indices
#'
#' Creates a bootstrap sample data frame from neighborhood indices
#'
#' @param neighb_data Original data in neighb() format
#' @param indices Vector of node indices for bootstrap sample
#' @return Bootstrap sample data frame
create_bootstrap_sample_from_indices <- function(neighb_data, indices) {

  # Get unique indices (neighbors might be duplicated)
  unique_indices <- unique(indices)

  # Map indices to actual node IDs
  sampled_nodes <- neighb_data$nodes[unique_indices]

  # Create bootstrap sample
  bootstrap_sample <- data.frame(
    id = sampled_nodes,
    stringsAsFactors = FALSE
  )

  # Add trait variables
  for (trait_var in names(neighb_data$traits)) {
    bootstrap_sample[[trait_var]] <- neighb_data$traits[as.character(sampled_nodes), trait_var]
  }

  # Add degree information
  bootstrap_sample$degree <- neighb_data$degree[as.character(sampled_nodes)]

  # Add bootstrap-specific recruiter structure (simplified)
  # This is a placeholder - real implementation would need to preserve network structure
  bootstrap_sample$recruiter.id <- rep(-1, nrow(bootstrap_sample))  # All seeds for simplicity

  return(bootstrap_sample)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Print Neighborhood Bootstrap Results
#'
#' Prints formatted results from neighborhood bootstrap analysis
#'
#' @param nb_results Results from neighborhood_bootstrap_resample()
print_neighborhood_bootstrap_results <- function(nb_results) {

  cat("=== Neighborhood Bootstrap Results ===\n")
  cat(sprintf("Method: %s\n", nb_results$method))
  cat(sprintf("Bootstrap replicates: %d\n", nb_results$parameters$n_bootstrap))
  cat(sprintf("Original sample size: %d\n", nb_results$metadata$original_sample_size))
  cat(sprintf("Duration: %.2f seconds\n", nb_results$metadata$duration_seconds))

  cat("\n--- Bootstrap Statistics ---\n")
  print(nb_results$bootstrap_statistics)

  if (!is.null(nb_results$bootstrap_samples)) {
    cat(sprintf("\nBootstrap samples available: %d\n",
                length(nb_results$bootstrap_samples)))
  }

  cat("\n")
}

#' Example Usage Function
#'
#' Demonstrates how to use the neighborhood bootstrap wrapper
example_neighborhood_bootstrap <- function() {

  cat("=== Neighborhood Bootstrap Example ===\n")

  # Load processed data
  if (file.exists(here("data", "processed", "prepared_data.RData"))) {
    load(here("data", "processed", "prepared_data.RData"))
    cat("Loaded prepared data\n")
  } else {
    cat("Prepared data not found. Run data preparation first.\n")
    return(invisible(NULL))
  }

  # Basic bootstrap for confidence intervals
  cat("\n1. Basic neighborhood bootstrap for confidence intervals:\n")
  nb_basic <- neighborhood_bootstrap_resample(
    rds_data = prepared_data,
    n_bootstrap = 100,  # Small number for example
    verbose = TRUE
  )

  print_neighborhood_bootstrap_results(nb_basic)

  # Bootstrap with samples for further analysis
  cat("2. Bootstrap with individual samples:\n")
  nb_samples <- neighborhood_bootstrap_resample(
    rds_data = prepared_data,
    n_bootstrap = 10,  # Very small for example
    return_samples = TRUE,
    verbose = TRUE
  )

  cat(sprintf("Retrieved %d bootstrap samples\n",
              length(nb_samples$bootstrap_samples)))

  # Show structure of first bootstrap sample
  if (length(nb_samples$bootstrap_samples) > 0) {
    cat("\nFirst bootstrap sample structure:\n")
    str(nb_samples$bootstrap_samples[[1]])
  }

  return(list(basic = nb_basic, with_samples = nb_samples))
}
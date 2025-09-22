# Boot_Step2.R
# Step 2: Recalculate RDS Weights for Bootstrap Samples
# NSUM-RDS Bootstrap Procedure - Weight Recalculation
#
# Purpose: Calculate inclusion probabilities (π_i) for bootstrap replicates
# from Step 1, enabling proper NSUM estimation in Step 3

library(RDS)
library(dplyr)

#' Compute RDS Weights for Bootstrap Samples
#'
#' @param resample Bootstrap sample from Step 1 (data.frame or rds.data.frame)
#' @param weight_method Weight calculation method: "VH" (Volz-Heckathorn),
#'                     "RDSII" (RDS-II), "SS" (Sequential Sampling), "RDSI" (RDS-I)
#' @param population_size Population size for VH/SS weights (required for these methods)
#' @param outcome_variable Variable name for weight calculation (required for RDS-II/SS, optional for others)
#' @param network_size_col Network size variable name (default: auto-detect)
#' @param verbose Logical, print progress messages
#' @param validate_rds Logical, validate RDS structure before weight calculation
#'
#' @return data.frame with original data plus weight columns:
#'   - weight_vh: Volz-Heckathorn weights (if computed)
#'   - weight_rds_i: RDS-I weights (if computed)
#'   - weight_rds_ii: RDS-II weights (if computed)
#'   - weight_rds_ss: RDS-SS weights (if computed)
#'   - inclusion_prob: Main inclusion probability (π_i) based on weight_method
#'
compute_rds_weights <- function(resample,
                               weight_method = c("VH", "RDSII", "SS", "RDSI"),
                               population_size = NULL,
                               outcome_variable = NULL,
                               network_size_col = NULL,
                               verbose = TRUE,
                               validate_rds = TRUE) {

  weight_method <- match.arg(weight_method)

  if (verbose) cat("Computing", weight_method, "weights for bootstrap sample...\n")

  # Convert to data.frame if needed
  if ("rds.data.frame" %in% class(resample)) {
    sample_data <- as.data.frame(resample)
    rds_data <- resample
  } else {
    sample_data <- resample
    rds_data <- NULL
  }

  # Validate inputs
  if (weight_method %in% c("VH", "SS") && is.null(population_size)) {
    stop("population_size is required for ", weight_method, " weights")
  }

  if (weight_method %in% c("RDSII", "SS") && is.null(outcome_variable)) {
    stop("outcome_variable is required for ", weight_method, " weights")
  }

  # Convert to rds.data.frame if not already
  if (is.null(rds_data)) {
    rds_data <- convert_to_rds_dataframe(sample_data,
                                        population_size = population_size,
                                        network_size_col = network_size_col,
                                        validate = validate_rds,
                                        verbose = verbose)
  }

  # Initialize weight storage (preserve all existing columns)
  sample_data$weight_vh <- NA
  sample_data$weight_rds_i <- NA
  sample_data$weight_rds_ii <- NA
  sample_data$weight_rds_ss <- NA
  sample_data$inclusion_prob <- NA

  # Calculate ALL weight methods for comparison and store in respective columns
  # inclusion_prob will contain the weights for the specified weight_method

  tryCatch({

    # Always compute VH weights (baseline method)
    if (verbose) cat("  Computing VH weights...\n")
    vh_weights <- compute_enhanced_vh_weights(rds_data, population_size, outcome_variable, verbose = FALSE)
    sample_data$weight_vh <- vh_weights

    # Always compute RDS-I weights
    if (verbose) cat("  Computing RDS-I weights...\n")
    rds_i_weights <- compute_rds_i_weights(rds_data, verbose = FALSE)
    sample_data$weight_rds_i <- rds_i_weights

    # Always compute RDS-II weights (may use fallback if insufficient data)
    if (verbose) cat("  Computing RDS-II weights...\n")
    rds_ii_weights <- tryCatch({
      compute_rds_ii_weights(rds_data, outcome_variable, verbose = FALSE)
    }, error = function(e) {
      if (verbose) cat("    RDS-II failed, using RDS-I as fallback\n")
      rds_i_weights  # Fallback to RDS-I
    })
    sample_data$weight_rds_ii <- rds_ii_weights

    # Always compute RDS-SS weights (may use fallback if insufficient data)
    if (verbose) cat("  Computing RDS-SS weights...\n")
    rds_ss_weights <- tryCatch({
      compute_rds_ss_weights(rds_data, population_size, outcome_variable, verbose = FALSE)
    }, error = function(e) {
      if (verbose) cat("    RDS-SS failed, using VH as fallback\n")
      vh_weights  # Fallback to VH
    })
    sample_data$weight_rds_ss <- rds_ss_weights

    # Set inclusion_prob to the requested method
    if (weight_method == "VH") {
      sample_data$inclusion_prob <- vh_weights
      active_method_name <- "Enhanced VH"
    } else if (weight_method == "RDSI") {
      sample_data$inclusion_prob <- rds_i_weights
      active_method_name <- "RDS-I"
    } else if (weight_method == "RDSII") {
      sample_data$inclusion_prob <- rds_ii_weights
      active_method_name <- "RDS-II"
    } else if (weight_method == "SS") {
      sample_data$inclusion_prob <- rds_ss_weights
      active_method_name <- "RDS-SS"
    } else {
      # Fallback to VH for unknown methods
      sample_data$inclusion_prob <- vh_weights
      active_method_name <- "VH (fallback)"
    }

    if (verbose) {
      cat("  ✓ Computed all RDS weight methods for", nrow(sample_data), "observations\n")
      cat("  Active method:", active_method_name, "\n")
      cat("  Active weight range: [", round(min(sample_data$inclusion_prob, na.rm = TRUE), 4),
          ", ", round(max(sample_data$inclusion_prob, na.rm = TRUE), 4), "]\n")

      # Show comparison of weight methods
      cat("  Weight method comparison:\n")
      cat("    VH:     [", round(min(sample_data$weight_vh, na.rm = TRUE), 4),
          ", ", round(max(sample_data$weight_vh, na.rm = TRUE), 4), "] sum =",
          round(sum(sample_data$weight_vh, na.rm = TRUE), 3), "\n")
      cat("    RDS-I:  [", round(min(sample_data$weight_rds_i, na.rm = TRUE), 4),
          ", ", round(max(sample_data$weight_rds_i, na.rm = TRUE), 4), "] sum =",
          round(sum(sample_data$weight_rds_i, na.rm = TRUE), 3), "\n")
      cat("    RDS-II: [", round(min(sample_data$weight_rds_ii, na.rm = TRUE), 4),
          ", ", round(max(sample_data$weight_rds_ii, na.rm = TRUE), 4), "] sum =",
          round(sum(sample_data$weight_rds_ii, na.rm = TRUE), 3), "\n")
      cat("    RDS-SS: [", round(min(sample_data$weight_rds_ss, na.rm = TRUE), 4),
          ", ", round(max(sample_data$weight_rds_ss, na.rm = TRUE), 4), "] sum =",
          round(sum(sample_data$weight_rds_ss, na.rm = TRUE), 3), "\n")

      cat("  All", ncol(sample_data), "variables preserved in weighted sample\n")
    }

  }, error = function(e) {
    warning("Error computing ", weight_method, " weights: ", e$message)
    # Fallback to uniform weights
    sample_data$inclusion_prob <- rep(1/nrow(sample_data), nrow(sample_data))
    if (verbose) cat("  Using uniform weights as fallback\n")
  })

  return(sample_data)
}

#' Convert Data Frame to rds.data.frame
#'
#' @param data Data frame to convert
#' @param population_size Population size for RDS object
#' @param network_size_col Network size column name
#' @param validate Validate RDS structure
#' @param verbose Print messages
#'
convert_to_rds_dataframe <- function(data,
                                   population_size = 100000,
                                   network_size_col = NULL,
                                   validate = TRUE,
                                   verbose = TRUE) {

  # Identify required columns
  id_col <- "id"
  recruiter_col <- "recruiter.id"

  # Auto-detect network size column if not specified
  if (is.null(network_size_col)) {
    possible_cols <- c("network.size.variable", "q13", "known_network_size",
                      "degree", "network_size")
    network_size_col <- intersect(possible_cols, names(data))[1]

    if (is.na(network_size_col)) {
      # Create default network size
      network_size_col <- "network.size.variable"
      data[[network_size_col]] <- 10  # Default value
      if (verbose) cat("  Created default network size variable\n")
    }
  }

  # Validate required columns exist
  required_cols <- c(id_col, recruiter_col, network_size_col)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Fix data types and structure
  data[[id_col]] <- as.character(data[[id_col]])
  data[[recruiter_col]] <- as.character(data[[recruiter_col]])

  # Handle network size - convert factor to numeric if needed
  if (is.factor(data[[network_size_col]])) {
    data[[network_size_col]] <- as.numeric(as.character(data[[network_size_col]]))
  } else {
    data[[network_size_col]] <- as.numeric(data[[network_size_col]])
  }

  # Handle missing network sizes
  if (any(is.na(data[[network_size_col]]))) {
    median_size <- median(data[[network_size_col]], na.rm = TRUE)
    data[[network_size_col]][is.na(data[[network_size_col]])] <- median_size
    if (verbose) cat("  Imputed", sum(is.na(data[[network_size_col]])), "missing network sizes\n")
  }

  # Convert to rds.data.frame
  tryCatch({
    rds_data <- RDS::as.rds.data.frame(data,
                                      population.size = population_size,
                                      id = id_col,
                                      recruiter.id = recruiter_col,
                                      network.size = network_size_col,
                                      check.valid = validate)

    if (verbose) cat("  Converted to rds.data.frame (N =", format(population_size, big.mark = ","), ")\n")
    return(rds_data)

  }, error = function(e) {
    warning("RDS conversion failed: ", e$message)
    if (validate) {
      # Retry without validation
      return(convert_to_rds_dataframe(data, population_size, network_size_col,
                                    validate = FALSE, verbose = verbose))
    } else {
      stop("Cannot convert to rds.data.frame: ", e$message)
    }
  })
}

#' Compute Volz-Heckathorn Weights
#'
#' Basic VH weights: π_i = d_i / (2 * m)
#' where d_i is degree and m is number of edges
#'
compute_vh_weights <- function(rds_data, population_size, verbose = TRUE) {

  # Get network sizes (degrees)
  network_size_var <- attr(rds_data, "network.size.variable")
  degrees <- rds_data[[network_size_var]]

  # Ensure degrees are numeric (handle factor conversion)
  if (is.factor(degrees)) {
    degrees <- as.numeric(as.character(degrees))
  } else {
    degrees <- as.numeric(degrees)
  }

  # Count edges in RDS sample (each recruit represents an edge)
  recruiter_col <- RDS::get.rid(rds_data)
  n_edges <- sum(recruiter_col != -1, na.rm = TRUE)

  if (n_edges == 0) {
    warning("No recruitment edges found, using uniform weights")
    return(rep(1/nrow(rds_data), nrow(rds_data)))
  }

  # VH weights: π_i = d_i / (2 * m)
  vh_weights <- degrees / (2 * n_edges)

  # Normalize to sum to 1
  vh_weights <- vh_weights / sum(vh_weights, na.rm = TRUE)

  if (verbose) cat("  VH weights: degrees/", 2 * n_edges, ", range [",
                  round(min(vh_weights, na.rm = TRUE), 4), ", ",
                  round(max(vh_weights, na.rm = TRUE), 4), "]\n")

  return(vh_weights)
}

#' Compute RDS-I Style Weights
#'
#' RDS-I (Salganik-Heckathorn) weights based on network size
#' Implementation follows Salganik & Heckathorn (2004) methodology
#'
#' The RDS-I estimator weights each individual by their reported network size
#' to account for differential recruitment probabilities. Individuals with larger
#' networks are more likely to be recruited, so they receive lower weights.
#'
compute_rds_i_weights <- function(rds_data, verbose = TRUE) {

  # Get network sizes (degrees)
  network_size_var <- attr(rds_data, "network.size.variable")
  degrees <- rds_data[[network_size_var]]

  # Ensure degrees are numeric and handle missing values
  if (is.factor(degrees)) {
    degrees <- as.numeric(as.character(degrees))
  } else {
    degrees <- as.numeric(degrees)
  }

  n_obs <- length(degrees)

  if (n_obs == 0) {
    warning("No observations for RDS-I weights")
    return(numeric(0))
  }

  # Handle missing or zero degrees
  if (any(is.na(degrees))) {
    missing_count <- sum(is.na(degrees))
    median_degree <- median(degrees, na.rm = TRUE)
    degrees[is.na(degrees)] <- median_degree
    if (verbose) cat("  Imputed", missing_count, "missing degrees with median value:", median_degree, "\n")
  }

  # Ensure minimum degree of 1 (everyone must have at least one connection to be recruited)
  degrees[degrees <= 0] <- 1
  if (verbose && any(degrees == 1)) {
    cat("  Set", sum(degrees == 1), "zero/negative degrees to 1\n")
  }

  # RDS-I weights: inversely proportional to network size (Salganik-Heckathorn estimator)
  # w_i = 1/d_i where d_i is the reported degree of individual i
  # Normalized so weights sum to 1
  weights <- 1 / degrees
  weights <- weights / sum(weights, na.rm = TRUE)

  if (verbose) {
    cat("  RDS-I weights: inverse degree (Salganik-Heckathorn), range [",
        round(min(weights, na.rm = TRUE), 4), ", ",
        round(max(weights, na.rm = TRUE), 4), "]\n")
    cat("  Degree range: [", min(degrees), ", ", max(degrees), "]\n")
  }

  return(weights)
}

#' Compute RDS-II Style Weights
#'
#' RDS-II weights account for homophily and differential recruitment
#' Implementation follows Volz & Heckathorn (2008) methodology
#'
#' RDS-II improves on RDS-I by accounting for the tendency of people to recruit
#' others similar to themselves (homophily). It uses group-specific transition
#' probabilities in the Markov chain model of RDS recruitment.
#'
#' @param outcome_variable Variable name to define groups for transition matrix
#'
compute_rds_ii_weights <- function(rds_data, outcome_variable = NULL, verbose = TRUE) {

  # Get network sizes (degrees)
  network_size_var <- attr(rds_data, "network.size.variable")
  degrees <- rds_data[[network_size_var]]

  # Ensure degrees are numeric and handle missing values
  if (is.factor(degrees)) {
    degrees <- as.numeric(as.character(degrees))
  } else {
    degrees <- as.numeric(degrees)
  }

  n_obs <- length(degrees)

  if (n_obs == 0) {
    warning("No observations for RDS-II weights")
    return(numeric(0))
  }

  # Handle missing or zero degrees
  if (any(is.na(degrees))) {
    missing_count <- sum(is.na(degrees))
    median_degree <- median(degrees, na.rm = TRUE)
    degrees[is.na(degrees)] <- median_degree
    if (verbose) cat("  Imputed", missing_count, "missing degrees with median value:", median_degree, "\n")
  }

  degrees[degrees <= 0] <- 1

  # Define groups for transition matrix
  if (!is.null(outcome_variable) && outcome_variable %in% names(rds_data)) {
    groups <- rds_data[[outcome_variable]]
    if (verbose) cat("  Using", outcome_variable, "for group definition\n")
  } else {
    # Create groups based on degree quartiles as fallback
    degree_quartiles <- quantile(degrees, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    groups <- cut(degrees, breaks = degree_quartiles, include.lowest = TRUE, labels = FALSE)
    if (verbose) cat("  Created degree-based groups (quartiles) for transition matrix\n")
  }

  # Ensure groups are factors
  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  if (n_groups <= 1) {
    # Fall back to RDS-I if only one group
    if (verbose) cat("  Only one group detected, falling back to RDS-I weights\n")
    weights <- 1 / degrees
    weights <- weights / sum(weights, na.rm = TRUE)
    return(weights)
  }

  # Build transition matrix
  recruiter_ids <- RDS::get.rid(rds_data)
  transition_matrix <- build_transition_matrix(groups, recruiter_ids, rds_data, verbose)

  # Calculate RDS-II weights using transition probabilities
  weights <- compute_rds_ii_from_transition_matrix(degrees, groups, transition_matrix, verbose)

  if (verbose) {
    cat("  RDS-II weights: transition matrix-based (Volz-Heckathorn), range [",
        round(min(weights, na.rm = TRUE), 4), ", ",
        round(max(weights, na.rm = TRUE), 4), "]\n")
    cat("  Groups:", n_groups, ", Degree range: [", min(degrees), ", ", max(degrees), "]\n")
  }

  return(weights)
}

#' Build Transition Matrix for RDS-II
#'
#' Constructs the group-to-group transition probability matrix
#'
build_transition_matrix <- function(groups, recruiter_ids, rds_data, verbose = TRUE) {

  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  # Initialize transition matrix
  transition_matrix <- matrix(0, nrow = n_groups, n_groups)
  rownames(transition_matrix) <- group_levels
  colnames(transition_matrix) <- group_levels

  # Map recruiter IDs to their groups
  id_col <- RDS::get.id(rds_data)
  id_to_group <- setNames(as.character(groups), id_col)

  # Count transitions
  for (i in seq_along(recruiter_ids)) {
    recruiter_id <- recruiter_ids[i]
    recruit_group <- as.character(groups[i])

    if (!is.na(recruiter_id) && recruiter_id != -1 && recruiter_id %in% names(id_to_group)) {
      recruiter_group <- id_to_group[as.character(recruiter_id)]

      if (!is.na(recruiter_group) && !is.na(recruit_group)) {
        recruiter_idx <- match(recruiter_group, group_levels)
        recruit_idx <- match(recruit_group, group_levels)

        if (!is.na(recruiter_idx) && !is.na(recruit_idx)) {
          transition_matrix[recruiter_idx, recruit_idx] <-
            transition_matrix[recruiter_idx, recruit_idx] + 1
        }
      }
    }
  }

  # Convert counts to probabilities (row-normalize)
  row_sums <- rowSums(transition_matrix)
  for (i in 1:n_groups) {
    if (row_sums[i] > 0) {
      transition_matrix[i, ] <- transition_matrix[i, ] / row_sums[i]
    } else {
      # If no transitions observed from this group, assume uniform
      transition_matrix[i, ] <- 1 / n_groups
    }
  }

  if (verbose) {
    cat("  Transition matrix (", n_groups, "x", n_groups, "):\n")
    print(round(transition_matrix, 3))
  }

  return(transition_matrix)
}

#' Compute RDS-II Weights from Transition Matrix
#'
#' Calculates individual weights using the transition matrix approach
#'
compute_rds_ii_from_transition_matrix <- function(degrees, groups, transition_matrix, verbose = TRUE) {

  n_obs <- length(degrees)
  group_levels <- levels(groups)

  # Calculate group sizes and proportions
  group_counts <- table(groups)
  group_props <- group_counts / sum(group_counts)

  # For each individual, calculate weight based on:
  # 1. Their degree (probability of being recruited)
  # 2. Their group's recruitment patterns (from transition matrix)
  weights <- numeric(n_obs)

  for (i in 1:n_obs) {
    individual_group <- as.character(groups[i])
    group_idx <- match(individual_group, group_levels)

    if (!is.na(group_idx)) {
      # Base weight: inverse of degree (RDS-I component)
      base_weight <- 1 / degrees[i]

      # Adjustment for homophily: how likely is this group to recruit others?
      # Higher homophily (diagonal dominance) means easier recruitment within group
      homophily_adjustment <- transition_matrix[group_idx, group_idx]

      # Combined weight accounts for both degree and homophily
      weights[i] <- base_weight * (1 + homophily_adjustment)
    } else {
      # Fallback for missing group
      weights[i] <- 1 / degrees[i]
    }
  }

  # Normalize weights to sum to 1
  weights <- weights / sum(weights, na.rm = TRUE)

  return(weights)
}

#' Compute RDS-SS Style Weights
#'
#' RDS-SS (Sequential Sampling) weights based on order of recruitment
#' Implementation follows Gile (2011) and Gile & Handcock methodology
#'
#' RDS-SS accounts for the sequential nature of RDS sampling where the sample
#' composition changes as recruitment progresses. It uses successive sampling
#' approximation to the RDS process.
#'
compute_rds_ss_weights <- function(rds_data, population_size, outcome_variable = NULL, verbose = TRUE) {

  # Get network sizes and recruitment order
  network_size_var <- attr(rds_data, "network.size.variable")
  degrees <- rds_data[[network_size_var]]

  # Ensure degrees are numeric and handle missing values
  if (is.factor(degrees)) {
    degrees <- as.numeric(as.character(degrees))
  } else {
    degrees <- as.numeric(degrees)
  }

  n_obs <- length(degrees)

  if (n_obs == 0) {
    warning("No observations for RDS-SS weights")
    return(numeric(0))
  }

  # Handle missing or zero degrees
  if (any(is.na(degrees))) {
    missing_count <- sum(is.na(degrees))
    median_degree <- median(degrees, na.rm = TRUE)
    degrees[is.na(degrees)] <- median_degree
    if (verbose) cat("  Imputed", missing_count, "missing degrees with median value:", median_degree, "\n")
  }

  degrees[degrees <= 0] <- 1

  # Get wave information (recruitment order)
  waves <- RDS::get.wave(rds_data)
  max_wave <- max(waves, na.rm = TRUE)

  # Get recruitment structure
  recruiter_ids <- RDS::get.rid(rds_data)
  id_col <- RDS::get.id(rds_data)

  # Define groups for SS estimation
  if (!is.null(outcome_variable) && outcome_variable %in% names(rds_data)) {
    groups <- rds_data[[outcome_variable]]
    if (verbose) cat("  Using", outcome_variable, "for group definition\n")
  } else {
    # Create groups based on degree tertiles as fallback
    degree_tertiles <- quantile(degrees, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
    groups <- cut(degrees, breaks = degree_tertiles, include.lowest = TRUE, labels = FALSE)
    if (verbose) cat("  Created degree-based groups (tertiles) for SS estimation\n")
  }

  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  # Calculate RDS-SS weights using successive sampling approach
  ss_weights <- compute_successive_sampling_weights(degrees, waves, groups, population_size, verbose)

  if (verbose) {
    cat("  RDS-SS weights: successive sampling (Gile), range [",
        round(min(ss_weights, na.rm = TRUE), 4), ", ",
        round(max(ss_weights, na.rm = TRUE), 4), "]\n")
    cat("  Waves: 0 -", max_wave, ", Groups:", n_groups, ", Population:", format(population_size, big.mark = ","), "\n")
  }

  return(ss_weights)
}

#' Compute Successive Sampling Weights
#'
#' Implements the RDS-SS successive sampling approximation
#'
compute_successive_sampling_weights <- function(degrees, waves, groups, population_size, verbose = TRUE) {

  n_obs <- length(degrees)
  weights <- numeric(n_obs)

  # Sort observations by wave (recruitment order)
  wave_order <- order(waves)

  # Calculate cumulative sampling probabilities
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  # Initialize group counts and probabilities
  cumulative_counts <- rep(0, n_groups)
  names(cumulative_counts) <- group_levels

  # For each observation in recruitment order, calculate inclusion probability
  for (idx in wave_order) {
    wave <- waves[idx]
    degree <- degrees[idx]
    group <- as.character(groups[idx])

    # Current sample size up to this point
    current_sample_size <- sum(cumulative_counts)

    # Group proportion in current sample
    if (current_sample_size > 0) {
      group_prop_in_sample <- cumulative_counts[group] / current_sample_size
    } else {
      group_prop_in_sample <- 0  # First observation
    }

    # SS weight calculation:
    # 1. Base probability: inverse of degree (RDS-I component)
    base_prob <- 1 / degree

    # 2. Sequential adjustment: account for changing sample composition
    # Later recruits get adjusted weights based on current sample composition
    if (current_sample_size > 0) {
      # Finite population correction
      fpc <- (population_size - current_sample_size) / population_size

      # Adjustment for group representation
      # If group is over-represented, reduce weight; if under-represented, increase weight
      expected_group_prop <- 1 / n_groups  # Assume equal groups as default
      group_adjustment <- expected_group_prop / max(group_prop_in_sample, 0.01)  # Avoid division by zero

      # Combined SS weight
      ss_adjustment <- fpc * group_adjustment
    } else {
      ss_adjustment <- 1  # First observation gets no adjustment
    }

    # Final weight
    weights[idx] <- base_prob * ss_adjustment

    # Update cumulative counts
    cumulative_counts[group] <- cumulative_counts[group] + 1
  }

  # Normalize weights to sum to 1
  weights <- weights / sum(weights, na.rm = TRUE)

  return(weights)
}

#' Compute Enhanced Volz-Heckathorn Weights
#'
#' Enhanced VH weights with finite population correction and homophily adjustment
#'
compute_enhanced_vh_weights <- function(rds_data, population_size, outcome_variable = NULL, verbose = TRUE) {

  # Get network sizes (degrees)
  network_size_var <- attr(rds_data, "network.size.variable")
  degrees <- rds_data[[network_size_var]]

  # Ensure degrees are numeric
  if (is.factor(degrees)) {
    degrees <- as.numeric(as.character(degrees))
  } else {
    degrees <- as.numeric(degrees)
  }

  n_obs <- length(degrees)

  if (n_obs == 0) {
    warning("No observations for enhanced VH weights")
    return(numeric(0))
  }

  # Handle missing or zero degrees
  if (any(is.na(degrees))) {
    missing_count <- sum(is.na(degrees))
    median_degree <- median(degrees, na.rm = TRUE)
    degrees[is.na(degrees)] <- median_degree
    if (verbose) cat("  Imputed", missing_count, "missing degrees\n")
  }

  degrees[degrees <= 0] <- 1

  # Count edges in RDS sample
  recruiter_ids <- RDS::get.rid(rds_data)
  n_edges <- sum(recruiter_ids != -1, na.rm = TRUE)

  if (n_edges == 0) {
    warning("No recruitment edges found, using uniform weights")
    return(rep(1/n_obs, n_obs))
  }

  # Basic VH weights: π_i = d_i / (2 * m)
  vh_weights <- degrees / (2 * n_edges)

  # Finite population correction if population size is specified
  if (!is.null(population_size) && population_size > n_obs) {
    fpc <- (population_size - n_obs) / population_size
    vh_weights <- vh_weights * fpc
    if (verbose) cat("  Applied finite population correction (", format(population_size, big.mark = ","), ")\n")
  }

  # Optional homophily adjustment
  if (!is.null(outcome_variable) && outcome_variable %in% names(rds_data)) {
    # Simple homophily adjustment based on outcome variable clustering
    groups <- rds_data[[outcome_variable]]
    homophily_factor <- calculate_homophily_factor(groups, recruiter_ids, rds_data)
    vh_weights <- vh_weights * homophily_factor
    if (verbose) cat("  Applied homophily adjustment\n")
  }

  # Normalize weights to sum to 1
  vh_weights <- vh_weights / sum(vh_weights, na.rm = TRUE)

  if (verbose) {
    cat("  Enhanced VH weights: range [",
        round(min(vh_weights, na.rm = TRUE), 4), ", ",
        round(max(vh_weights, na.rm = TRUE), 4), "]\n")
  }

  return(vh_weights)
}

#' Calculate Homophily Factor
#'
#' Simple homophily adjustment for VH weights
#'
calculate_homophily_factor <- function(groups, recruiter_ids, rds_data) {

  n_obs <- length(groups)
  homophily_factor <- rep(1, n_obs)

  # Map recruiter IDs to groups
  id_col <- RDS::get.id(rds_data)
  id_to_group <- setNames(as.character(groups), id_col)

  # Calculate homophily for each observation
  for (i in 1:n_obs) {
    recruiter_id <- recruiter_ids[i]
    recruit_group <- as.character(groups[i])

    if (!is.na(recruiter_id) && recruiter_id != -1 && recruiter_id %in% names(id_to_group)) {
      recruiter_group <- id_to_group[as.character(recruiter_id)]

      if (!is.na(recruiter_group)) {
        # If recruited by same group (homophily), slightly reduce weight
        # If recruited by different group, slightly increase weight
        if (recruiter_group == recruit_group) {
          homophily_factor[i] <- 0.9  # Slight down-weighting for homophily
        } else {
          homophily_factor[i] <- 1.1  # Slight up-weighting for heterophily
        }
      }
    }
  }

  return(homophily_factor)
}

#' Extract Weights from RDS Estimation Objects
#'
#' @param rds_result RDS estimation result object
#' @param method RDS method used
#' @param n_obs Number of observations (for fallback uniform weights)
#'
extract_rds_weights <- function(rds_result, method = "RDSI", n_obs = 100) {

  # Debug: Print structure of RDS result object
  if (getOption("debug_rds_weights", FALSE)) {
    cat("RDS Result Structure for", method, ":\n")
    cat("Class:", class(rds_result), "\n")
    cat("Names:", names(rds_result), "\n")
    if (is.list(rds_result) && length(rds_result) > 0) {
      for (name in names(rds_result)) {
        obj <- rds_result[[name]]
        cat("  ", name, ":", class(obj), "length:", length(obj), "\n")
      }
    }
  }

  # Try to extract weights based on actual RDS object structure
  weights <- NULL

  # For RDS-SS estimates, the object is typically a "rds.interval.estimate"
  if (method == "SS" && "rds.interval.estimate" %in% class(rds_result)) {
    # Check if there's a data component with weights
    if ("rds.data" %in% names(rds_result)) {
      rds_data <- rds_result$rds.data
      if ("rds.data.frame" %in% class(rds_data)) {
        # Calculate VH-style weights from the RDS data directly
        network_size_var <- attr(rds_data, "network.size.variable")
        if (!is.null(network_size_var) && network_size_var %in% names(rds_data)) {
          degrees <- rds_data[[network_size_var]]
          recruiter_ids <- RDS::get.rid(rds_data)
          n_edges <- sum(recruiter_ids != -1, na.rm = TRUE)

          if (n_edges > 0) {
            # Use VH-style calculation
            weights <- degrees / (2 * n_edges)
            weights <- weights / sum(weights, na.rm = TRUE)  # Normalize
          }
        }
      }
    }
  }

  # If we got weights from the SS-specific extraction, return them
  if (!is.null(weights) && length(weights) == n_obs) {
    return(weights)
  }

  # Standard weight extraction methods
  if (method == "RDSI") {
    # RDS-I objects may be "rds.I.estimate" class
    if ("rds.I.estimate" %in% class(rds_result)) {
      # For RDS-I, we need to calculate weights manually from the estimation
      # RDS-I doesn't store individual weights, so use VH-style calculation
      if ("rds.data" %in% names(rds_result)) {
        rds_data <- rds_result$rds.data
        if ("rds.data.frame" %in% class(rds_data)) {
          network_size_var <- attr(rds_data, "network.size.variable")
          if (!is.null(network_size_var) && network_size_var %in% names(rds_data)) {
            degrees <- as.numeric(rds_data[[network_size_var]])
            recruiter_ids <- RDS::get.rid(rds_data)
            n_edges <- sum(recruiter_ids != -1, na.rm = TRUE)

            if (n_edges > 0 && length(degrees) == n_obs) {
              # Use degree-weighted calculation for RDS-I
              weights <- degrees / sum(degrees, na.rm = TRUE)
              return(weights)
            }
          }
        }
      }
    }

    # Try standard weight locations
    if ("weight" %in% names(rds_result)) {
      return(rds_result$weight)
    } else if ("weights" %in% names(rds_result)) {
      return(rds_result$weights)
    }

  } else if (method == "RDSII") {
    # Similar for RDS-II
    if ("weight" %in% names(rds_result)) {
      return(rds_result$weight)
    } else if ("weights" %in% names(rds_result)) {
      return(rds_result$weights)
    }

  } else if (method == "SS") {
    # RDS-SS weights
    if ("weight" %in% names(rds_result)) {
      return(rds_result$weight)
    } else if ("weights" %in% names(rds_result)) {
      return(rds_result$weights)
    }
  }

  # Fallback to more thorough extraction
  return(extract_weights_fallback(rds_result, n_obs))
}

#' Fallback Weight Extraction
#'
#' Extract weights from RDS objects when standard methods fail
#'
extract_weights_fallback <- function(rds_result, n_obs) {

  # Try different possible weight storage locations
  if (is.list(rds_result)) {

    # Check common weight variable names
    weight_names <- c("weight", "weights", "inclusion.prob", "pi", "prob", "wt")
    for (name in weight_names) {
      if (name %in% names(rds_result) && is.numeric(rds_result[[name]])) {
        weights <- rds_result[[name]]
        if (length(weights) == n_obs) {
          return(weights)
        }
      }
    }

    # Check for rds.data component and extract weights from there
    if ("rds.data" %in% names(rds_result)) {
      rds_data <- rds_result$rds.data
      if ("rds.data.frame" %in% class(rds_data)) {
        # Try to calculate weights directly from the rds.data.frame
        network_size_var <- attr(rds_data, "network.size.variable")
        if (!is.null(network_size_var) && network_size_var %in% names(rds_data)) {
          degrees <- rds_data[[network_size_var]]

          # Ensure degrees are numeric
          if (is.factor(degrees)) {
            degrees <- as.numeric(as.character(degrees))
          } else {
            degrees <- as.numeric(degrees)
          }

          recruiter_ids <- RDS::get.rid(rds_data)
          n_edges <- sum(recruiter_ids != -1, na.rm = TRUE)

          if (n_edges > 0 && length(degrees) == n_obs && !any(is.na(degrees))) {
            # Calculate VH-style weights as fallback
            weights <- degrees / (2 * n_edges)
            weights <- weights / sum(weights, na.rm = TRUE)  # Normalize
            return(weights)
          }
        }
      }

      # Recursive search in nested rds.data
      nested_weights <- extract_weights_fallback(rds_data, n_obs)
      if (!is.null(nested_weights) && length(nested_weights) == n_obs) {
        return(nested_weights)
      }
    }

    # Check if it's an interval estimate object with point estimates
    if ("interval" %in% names(rds_result) && is.matrix(rds_result$interval)) {
      # For proportion estimates, try to derive individual weights
      interval_matrix <- rds_result$interval
      if ("n" %in% colnames(interval_matrix)) {
        total_n <- sum(interval_matrix[, "n"], na.rm = TRUE)
        if (total_n == n_obs) {
          # Create weights proportional to group sizes
          group_weights <- rep(1/n_obs, n_obs)
          return(group_weights)
        }
      }
    }

    # Check all numeric components that might be weights
    for (name in names(rds_result)) {
      obj <- rds_result[[name]]
      if (is.numeric(obj) && length(obj) == n_obs) {
        # This might be weights stored under a non-standard name
        if (all(obj > 0) && abs(sum(obj) - 1) < 1e-6) {
          # Looks like normalized weights
          return(obj)
        } else if (all(obj > 0)) {
          # Might be unnormalized weights
          normalized_weights <- obj / sum(obj)
          return(normalized_weights)
        }
      }
    }
  }

  # Ultimate fallback: uniform weights
  warning("Could not extract weights from RDS object, using uniform weights")
  return(rep(1/n_obs, n_obs))
}

#' Batch Compute Weights for Multiple Bootstrap Samples
#'
#' @param bootstrap_samples List of bootstrap samples from Step 1
#' @param weight_method Weight calculation method
#' @param population_size Population size
#' @param outcome_variable Outcome variable for weight calculation
#' @param parallel Logical, use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param verbose Print progress
#'
#' @return List of weighted bootstrap samples (preserving all covariates)
#'
compute_weights_batch <- function(bootstrap_samples,
                                 weight_method = "SS",
                                 population_size = 980000,
                                 outcome_variable = NULL,
                                 parallel = FALSE,
                                 n_cores = 4,
                                 verbose = TRUE) {

  if (verbose) {
    cat("Computing", weight_method, "weights for", length(bootstrap_samples), "bootstrap samples\n")
    cat("Population size:", format(population_size, big.mark = ","), "\n")
    cat("All covariates will be preserved in weighted samples\n")
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (verbose) cat("Using parallel processing with", n_cores, "cores\n")

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export required libraries to cluster
    parallel::clusterEvalQ(cl, {
      library(RDS)
      library(dplyr)
    })

    # Export all required functions to cluster
    parallel::clusterExport(cl, c("compute_rds_weights",
                                 "convert_to_rds_dataframe",
                                 "compute_vh_weights",
                                 "compute_enhanced_vh_weights",
                                 "compute_rds_i_weights",
                                 "compute_rds_ii_weights",
                                 "compute_rds_ss_weights",
                                 "build_transition_matrix",
                                 "compute_rds_ii_from_transition_matrix",
                                 "compute_successive_sampling_weights",
                                 "calculate_homophily_factor",
                                 "extract_rds_weights",
                                 "extract_weights_fallback"),
                           envir = environment())

    weighted_samples <- parallel::parLapply(cl, bootstrap_samples, function(sample) {
      compute_rds_weights(resample = sample,
                         weight_method = weight_method,
                         population_size = population_size,
                         outcome_variable = outcome_variable,
                         verbose = FALSE)
    })

  } else {
    # Sequential processing
    weighted_samples <- list()

    for (i in seq_along(bootstrap_samples)) {
      if (verbose && i %% 50 == 0) {
        cat("  Processed", i, "/", length(bootstrap_samples), "samples\n")
      }

      weighted_samples[[i]] <- compute_rds_weights(
        resample = bootstrap_samples[[i]],
        weight_method = weight_method,
        population_size = population_size,
        outcome_variable = outcome_variable,
        verbose = FALSE
      )
    }
  }

  if (verbose) {
    cat("Completed weight calculation for all", length(weighted_samples), "samples\n")
    if (length(weighted_samples) > 0) {
      n_vars <- ncol(weighted_samples[[1]])
      cat("Each weighted sample contains", n_vars, "variables (all covariates preserved)\n")
    }
  }

  return(weighted_samples)
}

# ============================================================================
# TESTING AND VALIDATION FUNCTIONS
# ============================================================================

#' Test Weight Computation on Sample Data
#'
test_weight_computation <- function(test_data = NULL, verbose = TRUE) {

  if (verbose) cat("=== Testing Weight Computation ===\n")

  # Use provided data or create test data
  if (is.null(test_data)) {
    # Create test dataset with multiple covariates (like from Step 1)
    test_data <- data.frame(
      id = 1:10,
      recruiter.id = c(-1, -1, 1, 1, 2, 2, 3, 4, 5, 6),
      q13 = c(5, 8, 3, 7, 4, 6, 9, 2, 5, 8),
      document_withholding_rds = c(1, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      pay_issues_rds = c(0, 1, 1, 0, 0, 1, 1, 0, 1, 0),
      threats_abuse_rds = c(1, 1, 0, 1, 0, 0, 1, 0, 1, 1),
      age = c(25, 34, 28, 45, 32, 38, 29, 41, 36, 31),
      nationality = c("Filipino", "Latinx", "Other", "Filipino", "British",
                     "Latinx", "Other", "Filipino", "British", "Other")
    )
    if (verbose) cat("Created test dataset with", nrow(test_data), "observations and",
                    ncol(test_data), "variables\n")
  }

  # Test each weight method
  methods <- c("VH", "RDSI", "RDSII", "SS")

  results <- list()

  for (method in methods) {
    if (verbose) cat("\nTesting", method, "weights...\n")

    tryCatch({
      weighted_data <- compute_rds_weights(
        resample = test_data,
        weight_method = method,
        population_size = 100000,
        outcome_variable = if (method %in% c("RDSII", "SS")) "document_withholding_rds" else NULL,
        verbose = verbose
      )

      results[[method]] <- list(
        success = TRUE,
        n_obs = nrow(weighted_data),
        n_vars = ncol(weighted_data),
        weight_sum = sum(weighted_data$inclusion_prob, na.rm = TRUE),
        weight_range = range(weighted_data$inclusion_prob, na.rm = TRUE),
        preserved_vars = all(names(test_data) %in% names(weighted_data))
      )

      if (verbose) {
        cat("  ✓ Success:", method, "weights computed\n")
        cat("  Variables preserved:", ncol(weighted_data), "(original:", ncol(test_data), ")\n")
        cat("  Weight sum:", round(results[[method]]$weight_sum, 4), "\n")
      }

    }, error = function(e) {
      results[[method]] <- list(
        success = FALSE,
        error = e$message
      )
      if (verbose) cat("  ✗ Failed:", e$message, "\n")
    })
  }

  if (verbose) cat("\n=== Weight Computation Test Complete ===\n")

  return(results)
}

# ============================================================================
# DEBUGGING HELPER FUNCTIONS
# ============================================================================

#' Enable Debug Mode for Weight Extraction
#'
#' This will print the structure of RDS result objects to help debug weight extraction
#'
enable_rds_weight_debug <- function() {
  options(debug_rds_weights = TRUE)
  cat("RDS weight debugging enabled. Weight extraction will show object structure.\n")
}

#' Disable Debug Mode for Weight Extraction
#'
disable_rds_weight_debug <- function() {
  options(debug_rds_weights = FALSE)
  cat("RDS weight debugging disabled.\n")
}

# ============================================================================
# EXAMPLE USAGE (commented out to prevent auto-execution)
# ============================================================================

# Example: Compute weights for a single bootstrap sample
# (assuming bootstrap_sample is from Boot_Step1.R)

weighted_sample <- compute_rds_weights(
  resample = boot_samples[[1]],
  weight_method = "SS",              # RDS-SS weights
  population_size = 980000,          # UK domestic worker population
  outcome_variable = "document_withholding_rds",
  verbose = TRUE
)

# Example: Batch compute weights for all bootstrap samples
# All covariates from Step 1 will be preserved
weighted_bootstrap_samples <- compute_weights_batch(
  bootstrap_samples = boot_samples,   # From Boot_Step1.R
  weight_method = "SS",
  population_size = 980000,
  outcome_variable = "document_withholding_rds",
  parallel = TRUE,
  n_cores = 4,
  verbose = TRUE
)

# Test weight computation
test_results <- test_weight_computation(verbose = TRUE)

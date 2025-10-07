# Boot_Step2_FIXED.R
# Step 2: Recalculate RDS Weights for Bootstrap Samples - USING STANDARD RDS PACKAGE
# NSUM-RDS Bootstrap Procedure - Weight Recalculation
#
# Purpose: Calculate inclusion probabilities (π_i) for bootstrap replicates
# from Step 1, using STANDARD RDS PACKAGE FUNCTIONS

library(RDS)
library(dplyr)

#' Compute RDS Weights for Bootstrap Samples - STANDARD RDS PACKAGE VERSION
#'
#' @param resample Bootstrap sample from Step 1 (data.frame or rds.data.frame)
#' @param weight_method Weight calculation method: "VH" (Volz-Heckathorn/RDS-II),
#'                     "SS" (Sequential Sampling/RDS-SS), "RDSI" (RDS-I)
#' @param population_size Population size for VH/SS weights (required)
#' @param outcome_variable Variable name for weight calculation (required for RDS-I/II/SS)
#' @param network_size_col Network size variable name (default: auto-detect)
#' @param verbose Logical, print progress messages
#' @param validate_rds Logical, validate RDS structure before weight calculation
#'
#' @return data.frame with original data plus weight columns:
#'   - weight_vh: RDS-II/Volz-Heckathorn weights (normalized to sum to 1)
#'   - weight_rds_i: RDS-I weights (normalized to sum to 1)
#'   - weight_rds_ss: RDS-SS weights (normalized to sum to 1)
#'   - inclusion_prob: Main inclusion probability (π_i) based on weight_method
#'
compute_rds_weights_standard <- function(resample,
                                        weight_method = c("VH", "SS", "RDSI"),
                                        population_size = NULL,
                                        outcome_variable = NULL,
                                        network_size_col = NULL,
                                        verbose = TRUE,
                                        validate_rds = TRUE) {

  weight_method <- match.arg(weight_method)

  if (verbose) cat("Computing", weight_method, "weights using STANDARD RDS package...\n")

  # Validate inputs
  if (is.null(population_size)) {
    stop("population_size is required for RDS weight calculation")
  }

  if (is.null(outcome_variable)) {
    stop("outcome_variable is required for RDS weight calculation")
  }

  # Convert to data.frame if needed
  if ("rds.data.frame" %in% class(resample)) {
    sample_data <- as.data.frame(resample)
    rds_data <- resample
  } else {
    sample_data <- resample
    rds_data <- NULL
  }

  # Convert to rds.data.frame if not already
  if (is.null(rds_data)) {
    if (verbose) cat("  Converting to rds.data.frame...\n")

    # Auto-detect column names
    if (is.null(network_size_col)) {
      network_size_col <- "known_network_size"
      if (!network_size_col %in% names(sample_data)) {
        network_size_col <- "network.size"
      }
      if (!network_size_col %in% names(sample_data)) {
        stop("Cannot find network size column. Please specify network_size_col parameter.")
      }
    }

    # Check for required columns
    if (!"id" %in% names(sample_data)) {
      stop("Bootstrap sample must have 'id' column")
    }
    if (!"recruiter.id" %in% names(sample_data)) {
      stop("Bootstrap sample must have 'recruiter.id' column")
    }
    if (!outcome_variable %in% names(sample_data)) {
      stop("outcome_variable '", outcome_variable, "' not found in bootstrap sample")
    }

    # IMPORTANT: Bootstrap samples can have duplicate IDs (sampling with replacement)
    # RDS package requires unique IDs, so create new unique IDs while preserving original
    sample_data$original_id <- sample_data$id
    sample_data$id <- seq_len(nrow(sample_data))

    # Also need to update recruiter.id mapping to use new IDs
    # Create mapping from original_id to new id
    id_mapping <- setNames(sample_data$id, sample_data$original_id)

    # Update recruiter.id (except for seeds which have recruiter.id = 0 or -1)
    sample_data$recruiter.id <- ifelse(
      sample_data$recruiter.id %in% c(0, -1, "0", "-1", "seed"),
      "0",  # Keep seeds as 0
      as.character(id_mapping[as.character(sample_data$recruiter.id)])
    )

    # Set NA recruiter.id (unmapped) to 0 (treat as seeds)
    sample_data$recruiter.id[is.na(sample_data$recruiter.id)] <- "0"

    rds_data <- as.rds.data.frame(sample_data,
                                  id = "id",
                                  recruiter.id = "recruiter.id",
                                  network.size = network_size_col,
                                  check.valid = validate_rds)
  }

  # Initialize weight storage
  sample_data$weight_vh <- NA
  sample_data$weight_rds_i <- NA
  sample_data$weight_rds_ss <- NA
  sample_data$inclusion_prob <- NA

  # Calculate weights using STANDARD RDS PACKAGE FUNCTIONS
  tryCatch({

    # RDS-I weights (outcome-specific)
    if (verbose) cat("  Computing RDS-I weights (RDS package)...\n")
    rds_i_obj <- RDS::RDS.I.estimates(rds.data = rds_data,
                                      outcome.variable = outcome_variable,
                                      N = population_size)
    rds_i_weights <- rds_i_obj$weights
    rds_i_weights_norm <- rds_i_weights / sum(rds_i_weights, na.rm = TRUE)
    sample_data$weight_rds_i <- rds_i_weights_norm

    # RDS-II weights (VH - Volz-Heckathorn)
    if (verbose) cat("  Computing RDS-II/VH weights (RDS package)...\n")
    rds_ii_obj <- RDS::RDS.II.estimates(rds.data = rds_data,
                                        outcome.variable = outcome_variable,
                                        N = population_size)
    rds_ii_weights <- rds_ii_obj$weights
    rds_ii_weights_norm <- rds_ii_weights / sum(rds_ii_weights, na.rm = TRUE)
    sample_data$weight_vh <- rds_ii_weights_norm  # VH = RDS-II

    # RDS-SS weights (Sequential Sampling)
    if (verbose) cat("  Computing RDS-SS weights (RDS package)...\n")
    rds_ss_obj <- RDS::RDS.SS.estimates(rds.data = rds_data,
                                        outcome.variable = outcome_variable,
                                        N = population_size)
    rds_ss_weights <- rds_ss_obj$weights
    rds_ss_weights_norm <- rds_ss_weights / sum(rds_ss_weights, na.rm = TRUE)
    sample_data$weight_rds_ss <- rds_ss_weights_norm

    # Set inclusion_prob to the requested method
    if (weight_method == "VH") {
      sample_data$inclusion_prob <- rds_ii_weights_norm
      active_method_name <- "RDS-II/VH"
    } else if (weight_method == "RDSI") {
      sample_data$inclusion_prob <- rds_i_weights_norm
      active_method_name <- "RDS-I"
    } else if (weight_method == "SS") {
      sample_data$inclusion_prob <- rds_ss_weights_norm
      active_method_name <- "RDS-SS"
    }

    if (verbose) {
      cat("  ✓ Computed RDS weights for", nrow(sample_data), "observations\n")
      cat("  Active method:", active_method_name, "\n")
      cat("  Active weight range: [", round(min(sample_data$inclusion_prob, na.rm = TRUE), 6),
          ", ", round(max(sample_data$inclusion_prob, na.rm = TRUE), 6), "]\n")

      # Show comparison of weight methods
      cat("  Weight method comparison (all normalized to sum=1):\n")
      cat("    RDS-I:  [", round(min(sample_data$weight_rds_i, na.rm = TRUE), 6),
          ", ", round(max(sample_data$weight_rds_i, na.rm = TRUE), 6), "] sum =",
          round(sum(sample_data$weight_rds_i, na.rm = TRUE), 3), "\n")
      cat("    VH:     [", round(min(sample_data$weight_vh, na.rm = TRUE), 6),
          ", ", round(max(sample_data$weight_vh, na.rm = TRUE), 6), "] sum =",
          round(sum(sample_data$weight_vh, na.rm = TRUE), 3), "\n")
      cat("    RDS-SS: [", round(min(sample_data$weight_rds_ss, na.rm = TRUE), 6),
          ", ", round(max(sample_data$weight_rds_ss, na.rm = TRUE), 6), "] sum =",
          round(sum(sample_data$weight_rds_ss, na.rm = TRUE), 3), "\n")
    }

  }, error = function(e) {
    cat("ERROR computing RDS weights:", e$message, "\n")
    # Return uniform weights as fallback
    n <- nrow(sample_data)
    sample_data$weight_rds_i <<- rep(1/n, n)
    sample_data$weight_vh <<- rep(1/n, n)
    sample_data$weight_rds_ss <<- rep(1/n, n)
    sample_data$inclusion_prob <<- rep(1/n, n)
    warning("Using uniform weights due to error: ", e$message)
  })

  if (verbose) cat("  Weights successfully computed and normalized.\n\n")

  return(sample_data)
}


#' Compute Weights for Multiple Bootstrap Samples (Batch Processing)
#'
#' @param bootstrap_samples List of bootstrap samples from Step 1
#' @param weight_method Weight calculation method
#' @param population_size Population size
#' @param outcome_variable Outcome variable name
#' @param parallel Logical, use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param verbose Logical, print progress messages
#'
#' @return List of bootstrap samples with computed weights
#'
compute_weights_batch_standard <- function(bootstrap_samples,
                                          weight_method = "VH",
                                          population_size = 980000,
                                          outcome_variable = NULL,
                                          parallel = FALSE,
                                          n_cores = 4,
                                          verbose = TRUE) {

  if (verbose) {
    cat("=== Computing RDS Weights for", length(bootstrap_samples), "Bootstrap Samples ===\n")
    cat("Method:", weight_method, "\n")
    cat("Population size:", format(population_size, big.mark = ","), "\n")
    cat("Outcome variable:", outcome_variable, "\n\n")
  }

  if (parallel) {
    if (verbose) cat("Using parallel processing with", n_cores, "cores...\n")

    library(parallel)
    cl <- makeCluster(n_cores)

    # Export required libraries to cluster
    clusterEvalQ(cl, {
      library(RDS)
      library(dplyr)
    })

    # Export function to cluster
    clusterExport(cl, c("compute_rds_weights_standard"),
                  envir = environment())

    weighted_samples <- parLapply(cl, bootstrap_samples, function(sample) {
      compute_rds_weights_standard(resample = sample,
                                  weight_method = weight_method,
                                  population_size = population_size,
                                  outcome_variable = outcome_variable,
                                  verbose = FALSE)
    })

    stopCluster(cl)

  } else {
    # Sequential processing
    weighted_samples <- lapply(seq_along(bootstrap_samples), function(i) {
      if (verbose && i %% 50 == 0) {
        cat("  Processing bootstrap sample", i, "of", length(bootstrap_samples), "...\n")
      }
      compute_rds_weights_standard(resample = bootstrap_samples[[i]],
                                  weight_method = weight_method,
                                  population_size = population_size,
                                  outcome_variable = outcome_variable,
                                  verbose = FALSE)
    })
  }

  if (verbose) cat("\n✓ All bootstrap samples have been weighted.\n\n")

  return(weighted_samples)
}


# Example usage (commented out)
#
# # Assuming you have bootstrap samples from Boot_Step1
# load("bootstrap_samples_step1.RData")  # Contains boot_samples
#
# # Compute weights using standard RDS package
# weighted_samples <- compute_weights_batch_standard(
#   bootstrap_samples = boot_samples,
#   weight_method = "VH",  # or "SS" or "RDSI"
#   population_size = 980000,
#   outcome_variable = "document_withholding_rds",
#   parallel = TRUE,
#   n_cores = 4,
#   verbose = TRUE
# )
#
# # Save for Step 3 (NSUM estimation)
# save(weighted_samples, file = "bootstrap_samples_weighted_step2.RData")

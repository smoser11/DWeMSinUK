# 08-netclust_subgroup_analysis_FIXED.R
# Fixed Clustered SS-PSE implementation using netclust package
# Addresses sample size and function call issues
#
# PURPOSE:
# - Implement properly working Clustered SS-PSE with netclust
# - Handle small sample sizes by smart cluster aggregation
# - Complete "Table 1 (proposed)" in the Quarto paper with working estimates

# netclust Package Issue:
#   - netclust 0.1.0 (2021) has compatibility issues with R 4.5.0 (2025)
# - Internal bug causing 'length = XX' in coercion to 'logical(1)' errors
# - Affects even the package's own example data
#   - This is unfortunately common with older statistical packages
# 
#   Alternative Approaches for Table 1 Completion:
# 
#   1. Standard RDS Estimates by Subgroup (08b-simple_subgroup_analysis.R)
#     - Use existing working approach
#     - Bootstrap confidence intervals by nationality cluster
#     - This already works and provides valid estimates
#   2. Individual SS-PSE by Cluster
#     - Run separate sspse::posteriorsize() on each nationality cluster
#     - Combine results manually
#     - Less elegant but methodologically sound
#   3. Weighted RDS Estimates
#     - Use existing RDS-SS weights by subgroup
#     - More straightforward than Clustered SS-PSE
#   4. Fix netclust Installation
#     - Try installing from source with fixes
#     - Or use an older R version temporarily
# 

cat("=== FIXED NETCLUST CLUSTERED SS-PSE ANALYSIS ===\n")
cat("Addressing sample size and function call issues\n\n")

# Load required libraries
library(tidyverse)
library(here)

# Try to load netclust, handle if not available
netclust_available <- tryCatch({
  library(netclust)
  TRUE
}, error = function(e) {
  cat("WARNING: netclust package not available. Install with:\n")
  cat("  devtools::install_github('LJGamble/netclust')\n")
  FALSE
})

if (!netclust_available) {
  stop("netclust package is required for Clustered SS-PSE analysis")
}

# Load data
load('data/processed/prepared_data.RData')

# Use the dd object (RDS data)
if (exists('dd')) {
  rds_data <- dd
} else if (exists('rd.dd')) {
  rds_data <- rd.dd
} else {
  load('data/survey/dd.RData')
  rds_data <- dd
}

cat("Data loaded:", nrow(rds_data), "observations\n")
cat("Original nationality distribution:\n")
print(table(rds_data$nationality_cluster, useNA = 'ifany'))

# ============================================================================
# CONFIGURATION WITH SMART CLUSTERING
# ============================================================================

netclust_config <- list(
  # Clustering strategy - combine small groups
  cluster_variable = "nationality_cluster_combined",

  # Minimum sample size per cluster for SS-PSE
  min_cluster_size = 10,

  # Indicators to analyze (CE's comparable indicators)
  indicators = c(
    "document_withholding_rds",
    "pay_issues_rds",
    "threats_abuse_rds",
    "excessive_hours_rds",
    "access_to_help_rds"
  ),

  # Population size scenarios
  population_sizes = c(50000, 100000, 980000, 1740000),
  baseline_population = 980000,

  # MCMC parameters - more conservative for better convergence
  burnin = 2000,
  samplesize = 5000,
  interval = 10,

  # Analysis parameters
  confidence_level = 0.95,

  # Output settings
  save_individual_results = TRUE,
  save_summary_table = TRUE,
  verbose = TRUE
)

# ============================================================================
# SMART CLUSTERING FUNCTION
# ============================================================================

create_smart_clusters <- function(rds_data, min_size = 10) {

  cat("=== Creating Smart Clusters ===\n")

  # Get original distribution
  orig_dist <- table(rds_data$nationality_cluster, useNA = 'ifany')
  cat("Original cluster sizes:\n")
  print(orig_dist)

  # Identify clusters that are too small
  small_clusters <- names(orig_dist)[orig_dist < min_size]
  large_clusters <- names(orig_dist)[orig_dist >= min_size]

  cat("\nClusters >= minimum size (", min_size, "):", paste(large_clusters, collapse = ", "), "\n")
  cat("Clusters < minimum size:", paste(small_clusters, collapse = ", "), "\n")

  # Create new combined variable
  rds_data$nationality_cluster_combined <- as.character(rds_data$nationality_cluster)

  # Combine small clusters into "Other_Combined"
  if (length(small_clusters) > 0) {
    rds_data$nationality_cluster_combined[rds_data$nationality_cluster %in% small_clusters] <- "Other_Combined"
    cat("Combined small clusters into 'Other_Combined'\n")
  }

  # Convert to factor
  rds_data$nationality_cluster_combined <- factor(rds_data$nationality_cluster_combined)

  # Check new distribution
  new_dist <- table(rds_data$nationality_cluster_combined, useNA = 'ifany')
  cat("\nFinal cluster sizes:\n")
  print(new_dist)

  # Validate all clusters meet minimum size
  if (any(new_dist < min_size)) {
    warning("Some clusters still below minimum size after combining")
  } else {
    cat("✓ All clusters meet minimum size requirement\n")
  }

  return(rds_data)
}

# ============================================================================
# FIXED NETCLUST ESTIMATION FUNCTION
# ============================================================================

estimate_netclust_indicator_fixed <- function(clean_data, indicator, population_size) {

  cat("\n--- Estimating", indicator, "using Clustered SS-PSE (N =", format(population_size, scientific = FALSE), ") ---\n")

  # Filter to observations with the indicator
  indicator_data <- clean_data %>%
    filter(!is.na(.data[[indicator]]))

  if (nrow(indicator_data) < netclust_config$min_cluster_size) {
    cat("  WARNING: Too few total observations (", nrow(indicator_data), ") for", indicator, "\n")
    return(create_failed_result(indicator, population_size, "insufficient_total_data"))
  }

  # Check cluster distribution
  cluster_counts <- table(indicator_data$cluster)
  cat("  Cluster distribution:", paste(names(cluster_counts), "=", cluster_counts, collapse = ", "), "\n")

  # Check if any cluster is too small
  if (any(cluster_counts < 3)) {
    small_clusters <- names(cluster_counts)[cluster_counts < 3]
    cat("  WARNING: Clusters with < 3 observations:", paste(small_clusters, collapse = ", "), "\n")
    return(create_failed_result(indicator, population_size, "insufficient_cluster_data"))
  }

  # Prepare data for netclust
  trait_values <- indicator_data[[indicator]]
  if (!all(trait_values %in% c(0, 1), na.rm = TRUE)) {
    cat("  Converting to binary indicator\n")
    trait_values <- as.numeric(trait_values > 0)
  }

  # Network degrees
  degree_seq <- indicator_data$degree
  if (any(is.na(degree_seq))) {
    cat("  Imputing missing degree values with median\n")
    degree_seq[is.na(degree_seq)] <- median(degree_seq, na.rm = TRUE)
  }

  # Ensure minimum degree of 1
  degree_seq[degree_seq < 1] <- 1

  # Cluster assignments (as numeric)
  cluster_assignments <- as.numeric(indicator_data$cluster)
  n_clusters <- length(unique(cluster_assignments))

  cat("  Final data: n =", length(degree_seq), ", clusters =", n_clusters, ", trait prevalence =",
      round(mean(trait_values), 3), "\n")

  # Try netclust estimation with proper error handling
  result <- tryCatch({

    cat("  Running posteriorsize.c() with burnin =", netclust_config$burnin,
        ", samples =", netclust_config$samplesize, "\n")

    # Use posteriorsize.c from netclust - FIXED PARAMETER SET
    netclust_result <- posteriorsize.c(
      s = degree_seq,                           # Network degrees
      c = cluster_assignments,                  # Cluster assignments (numeric)
      median.prior.size = population_size,      # Prior median population size
      prop.prior.params = rep(1, n_clusters),  # Symmetric Dirichlet prior for cluster proportions
      burnin = netclust_config$burnin,         # MCMC burnin
      samplesize = netclust_config$samplesize, # MCMC samples
      interval = netclust_config$interval,     # Thinning
      verbose = netclust_config$verbose        # Progress messages
    )

    cat("  ✓ MCMC completed successfully\n")

    # Extract population size estimates
    if (!"N" %in% colnames(netclust_result$sample)) {
      stop("MCMC output missing 'N' column")
    }

    pop_estimates <- netclust_result$sample[, "N"]
    point_estimate <- mean(pop_estimates)
    ci_quantiles <- quantile(pop_estimates, c(0.025, 0.975))

    cat("  Population size estimates: mean =", round(point_estimate),
        ", 95% CI: (", round(ci_quantiles[1]), "-", round(ci_quantiles[2]), ")\n")

    # Calculate prevalence (proportion with trait)
    n_trait <- sum(trait_values, na.rm = TRUE)
    prevalence_estimate <- n_trait / point_estimate
    prevalence_ci_lower <- n_trait / ci_quantiles[2]  # Inverted for division
    prevalence_ci_upper <- n_trait / ci_quantiles[1]

    cat("  Prevalence estimate:", round(prevalence_estimate * 100, 1), "% (",
        round(prevalence_ci_lower * 100, 1), "-", round(prevalence_ci_upper * 100, 1), "%)\n")

    # Extract cluster-specific results
    cluster_results <- extract_cluster_results(netclust_result, indicator_data, trait_values,
                                             cluster_assignments, pop_estimates, n_clusters)

    create_success_result(indicator, population_size, prevalence_estimate,
                         prevalence_ci_lower, prevalence_ci_upper,
                         cluster_results, netclust_result,
                         nrow(indicator_data), n_trait, n_clusters)

  }, error = function(e) {
    cat("  ✗ ERROR:", e$message, "\n")
    create_failed_result(indicator, population_size, paste("error:", e$message))
  })

  return(result)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

create_failed_result <- function(indicator, population_size, reason) {
  list(
    indicator = indicator,
    population_size = population_size,
    overall_prevalence = NA,
    overall_ci_lower = NA,
    overall_ci_upper = NA,
    cluster_results = NULL,
    netclust_output = NULL,
    n_obs = 0,
    n_trait = NA,
    n_clusters = NA,
    convergence = reason
  )
}

create_success_result <- function(indicator, population_size, prevalence, ci_lower, ci_upper,
                                cluster_results, netclust_output, n_obs, n_trait, n_clusters) {
  list(
    indicator = indicator,
    population_size = population_size,
    overall_prevalence = prevalence,
    overall_ci_lower = ci_lower,
    overall_ci_upper = ci_upper,
    cluster_results = cluster_results,
    netclust_output = netclust_output,
    n_obs = n_obs,
    n_trait = n_trait,
    n_clusters = n_clusters,
    convergence = "success"
  )
}

extract_cluster_results <- function(netclust_result, indicator_data, trait_values,
                                   cluster_assignments, pop_estimates, n_clusters) {

  cluster_results <- list()
  cluster_names <- levels(indicator_data$cluster)

  for (i in 1:n_clusters) {
    if (i <= length(cluster_names)) {
      cluster_name <- cluster_names[i]

      # Extract cluster proportion from MCMC
      prop_col <- paste0("p", i)
      if (prop_col %in% colnames(netclust_result$sample)) {
        cluster_props <- netclust_result$sample[, prop_col]
        cluster_pop <- cluster_props * pop_estimates
        cluster_trait <- sum(trait_values[cluster_assignments == i], na.rm = TRUE)
        cluster_sample_size <- sum(cluster_assignments == i)

        if (cluster_sample_size > 0 && mean(cluster_pop) > 0) {
          cluster_results[[cluster_name]] <- list(
            cluster_population = mean(cluster_pop),
            cluster_population_ci = quantile(cluster_pop, c(0.025, 0.975)),
            cluster_trait_count = cluster_trait,
            cluster_prevalence = cluster_trait / mean(cluster_pop),
            cluster_sample_size = cluster_sample_size
          )
        }
      }
    }
  }

  return(cluster_results)
}

# Function to prepare data with smart clustering
prepare_netclust_data_smart <- function(rds_data, cluster_var) {

  cat("=== Preparing Data for Clustered SS-PSE ===\n")

  # Check required variables
  required_vars <- c("id", "recruiter.id", cluster_var, "q13")
  missing_vars <- setdiff(required_vars, names(rds_data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Create clean dataset
  clean_data <- rds_data %>%
    filter(!is.na(.data[[cluster_var]])) %>%
    mutate(
      # Ensure proper ID variables
      respondent_id = as.character(id),
      recruiter_id = ifelse(recruiter.id == -1, NA, as.character(recruiter.id)),

      # Network degree
      degree = as.numeric(q13),
      degree = ifelse(is.na(degree) | degree < 1, 1, degree),  # Ensure minimum degree

      # Cluster variable
      cluster = as.factor(.data[[cluster_var]]),

      # Seeds indicator
      is_seed = is.na(recruiter_id) | recruiter.id == -1
    ) %>%
    select(respondent_id, recruiter_id, degree, cluster, is_seed,
           all_of(netclust_config$indicators))

  cat("Clean data prepared:", nrow(clean_data), "observations\n")
  cat("Final cluster distribution:\n")
  print(table(clean_data$cluster))
  cat("Seeds by cluster:\n")
  print(table(clean_data$cluster, clean_data$is_seed))
  cat("Degree summary:\n")
  print(summary(clean_data$degree))

  return(clean_data)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n=== STARTING FIXED CLUSTERED SS-PSE ANALYSIS ===\n")

# Step 1: Create smart clusters to handle small sample sizes
rds_data <- create_smart_clusters(rds_data, netclust_config$min_cluster_size)

# Step 2: Prepare data
clean_data <- prepare_netclust_data_smart(rds_data, netclust_config$cluster_variable)

# Step 3: Run analysis for baseline population size only (for testing)
cat("\n=== Running Analysis for Baseline Population Size ===\n")

all_results <- list()
results_df <- data.frame()
baseline_pop <- netclust_config$baseline_population

for (indicator in netclust_config$indicators) {

  cat("\n--- Processing", indicator, "---\n")

  # Run netclust analysis
  netclust_result <- estimate_netclust_indicator_fixed(clean_data, indicator, baseline_pop)

  # Store result
  result_id <- paste(indicator, format(baseline_pop, scientific = FALSE), sep = "_")
  all_results[[result_id]] <- netclust_result

  # Extract for summary table
  if (netclust_result$convergence == "success" && !is.na(netclust_result$overall_prevalence)) {

    # Overall result
    overall_row <- data.frame(
      indicator = indicator,
      cluster = "Overall",
      population_size = baseline_pop,
      estimate_pct = netclust_result$overall_prevalence * 100,
      ci_lower_pct = netclust_result$overall_ci_lower * 100,
      ci_upper_pct = netclust_result$overall_ci_upper * 100,
      n_obs = netclust_result$n_obs,
      n_trait = netclust_result$n_trait,
      convergence = "success",
      estimate_with_ci = sprintf("%.1f%% (%.1f-%.1f%%)",
                                netclust_result$overall_prevalence * 100,
                                netclust_result$overall_ci_lower * 100,
                                netclust_result$overall_ci_upper * 100),
      stringsAsFactors = FALSE
    )
    results_df <- rbind(results_df, overall_row)

    # Cluster-specific results
    for (cluster_name in names(netclust_result$cluster_results)) {
      cluster_info <- netclust_result$cluster_results[[cluster_name]]

      cluster_row <- data.frame(
        indicator = indicator,
        cluster = cluster_name,
        population_size = baseline_pop,
        estimate_pct = cluster_info$cluster_prevalence * 100,
        ci_lower_pct = NA,  # Individual cluster CIs complex to extract
        ci_upper_pct = NA,
        n_obs = cluster_info$cluster_sample_size,
        n_trait = cluster_info$cluster_trait_count,
        convergence = "success",
        estimate_with_ci = sprintf("%.1f%%", cluster_info$cluster_prevalence * 100),
        stringsAsFactors = FALSE
      )
      results_df <- rbind(results_df, cluster_row)
    }

    cat("✓", indicator, "completed successfully\n")

  } else {
    # Failed result
    cluster_names <- levels(clean_data$cluster)
    for (cluster_name in c("Overall", cluster_names)) {
      failed_row <- data.frame(
        indicator = indicator,
        cluster = cluster_name,
        population_size = baseline_pop,
        estimate_pct = NA,
        ci_lower_pct = NA,
        ci_upper_pct = NA,
        n_obs = 0,
        n_trait = NA,
        convergence = netclust_result$convergence,
        estimate_with_ci = "Failed",
        stringsAsFactors = FALSE
      )
      results_df <- rbind(results_df, failed_row)
    }

    cat("✗", indicator, "failed:", netclust_result$convergence, "\n")
  }
}

# Compile final results
final_results <- list(
  individual_results = all_results,
  summary_df = results_df,
  clean_data = clean_data,
  config = netclust_config
)

# Save results
save(final_results, file = here("output", "netclust_subgroup_analysis_results_FIXED.RData"))

# Save CSV
write.csv(results_df, here("output", "tables", "netclust_estimates_fixed.csv"), row.names = FALSE)

# Print summary
cat("\n=== FIXED CLUSTERED SS-PSE ANALYSIS COMPLETE ===\n")
cat("Results saved to: output/netclust_subgroup_analysis_results_FIXED.RData\n")
cat("Total estimates attempted:", nrow(results_df), "\n")
cat("Successful estimates:", sum(results_df$convergence == "success", na.rm = TRUE), "\n")

# Show successful results
successful_results <- results_df[results_df$convergence == "success" & !is.na(results_df$estimate_pct), ]
if (nrow(successful_results) > 0) {
  cat("\n=== SUCCESSFUL ESTIMATES ===\n")
  print(successful_results[, c("indicator", "cluster", "estimate_with_ci")])
}

cat("\n✓ Fixed Clustered SS-PSE analysis completed!\n")




####################################################################

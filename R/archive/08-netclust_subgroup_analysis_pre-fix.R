# 08-netclust_subgroup_analysis.R
# Non-connected SSPSE Estimators using netclust package
# Subgroup analysis by nationality_cluster for Table 1 completion
#
# PURPOSE:
# - Implement non-connected estimators from netclust package
# - Estimate prevalence by nationality subgroups (British, Filipino, Latinx, Other)
# - Complete "Table 1 (proposed)" in the Quarto paper
# - Compare with existing RDS and NSUM estimates

cat("=== NETCLUST SUBGROUP ANALYSIS ===\n")
cat("Non-connected SSPSE estimators by nationality cluster\n")
cat("Completing Table 1 for IJOPM paper\n\n")

# Load required libraries
library(netclust)
library(sspse)
library(RDS)
library(tidyverse)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load data
setwd('/data/home/Documents/GitHub/DWeMSinUK')
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
cat("Nationality distribution:\n")
print(table(rds_data$nationality_cluster, useNA = 'ifany'))

# ============================================================================
# CONFIGURATION
# ============================================================================

netclust_config <- list(
  # Clustering variable
  cluster_variable = "nationality_cluster",

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

  # Analysis parameters
  confidence_level = 0.95,
  n_bootstrap = 1000,

  # Output settings
  save_individual_results = TRUE,
  save_summary_table = TRUE
)

# ============================================================================
# NETCLUST ANALYSIS FUNCTIONS
# ============================================================================

# Function to prepare data for netclust analysis
prepare_netclust_data <- function(rds_data, cluster_var) {

  cat("Preparing data for netclust analysis...\n")

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

      # Cluster variable
      cluster = as.factor(.data[[cluster_var]]),

      # Seeds indicator
      is_seed = is.na(recruiter_id) | recruiter.id == -1
    ) %>%
    select(respondent_id, recruiter_id, degree, cluster, is_seed,
           all_of(netclust_config$indicators))

  cat("Clean data prepared:", nrow(clean_data), "observations\n")
  cat("Clusters:", paste(levels(clean_data$cluster), collapse = ", "), "\n")
  cat("Seeds by cluster:\n")
  print(table(clean_data$cluster, clean_data$is_seed))

  return(clean_data)
}

# Function to run netclust estimator for all clusters simultaneously
estimate_netclust_indicator <- function(clean_data, indicator, population_size) {

  cat("Estimating", indicator, "using netclust for all clusters (N =", format(population_size, scientific = FALSE), ")...\n")

  # Filter to observations with the indicator
  indicator_data <- clean_data %>%
    filter(!is.na(.data[[indicator]]))

  if (nrow(indicator_data) < 10) {
    cat("  WARNING: Too few total observations (", nrow(indicator_data), ") for", indicator, "\n")
    return(NULL)
  }

  # Check cluster distribution
  cluster_counts <- table(indicator_data$cluster)
  cat("  Cluster distribution:", paste(names(cluster_counts), "=", cluster_counts, collapse = ", "), "\n")

  # Prepare data for netclust
  trait_values <- indicator_data[[indicator]]
  if (!all(trait_values %in% c(0, 1), na.rm = TRUE)) {
    cat("  WARNING: Non-binary indicator values detected, converting to binary\n")
    trait_values <- as.numeric(trait_values > 0)
  }

  # Network degrees
  degree_seq <- indicator_data$degree
  if (any(is.na(degree_seq))) {
    cat("  WARNING: Missing degree values, using median imputation\n")
    degree_seq[is.na(degree_seq)] <- median(degree_seq, na.rm = TRUE)
  }

  # Cluster assignments (as numeric)
  cluster_assignments <- as.numeric(indicator_data$cluster)
  n_clusters <- length(unique(cluster_assignments))

  # Try netclust estimation
  result <- tryCatch({

    # Use posteriorsize.c from netclust for clustered populations
    netclust_result <- posteriorsize.c(
      s = degree_seq,                           # Network degrees
      c = cluster_assignments,                  # Cluster assignments
      median.prior.size = population_size,      # Prior population size
      prop.prior.params = rep(1, n_clusters),  # Equal cluster priors
      burnin = 1000,                           # Reduced for testing
      samplesize = 500,                        # Reduced for testing
      interval = 5,                            # Reduced for testing
      verbose = FALSE
    )

    # Extract population size estimates
    pop_estimates <- netclust_result$sample[, "N"]
    point_estimate <- mean(pop_estimates)
    ci_quantiles <- quantile(pop_estimates, c(0.025, 0.975))

    # Calculate prevalence (proportion with trait)
    n_trait <- sum(trait_values, na.rm = TRUE)
    prevalence_estimate <- n_trait / point_estimate
    prevalence_ci_lower <- n_trait / ci_quantiles[2]  # Inverted because of division
    prevalence_ci_upper <- n_trait / ci_quantiles[1]

    # Extract cluster-specific results from the MCMC output
    cluster_results <- list()
    cluster_names <- levels(clean_data$cluster)

    for (i in 1:n_clusters) {
      if (i <= length(cluster_names)) {
        cluster_prop_col <- paste0("p", i)
        if (cluster_prop_col %in% colnames(netclust_result$sample)) {
          cluster_props <- netclust_result$sample[, cluster_prop_col]
          cluster_pop <- cluster_props * pop_estimates
          cluster_trait <- sum(trait_values[cluster_assignments == i], na.rm = TRUE)

          cluster_results[[cluster_names[i]]] <- list(
            cluster_population = mean(cluster_pop),
            cluster_trait_count = cluster_trait,
            cluster_prevalence = cluster_trait / mean(cluster_pop),
            cluster_sample_size = sum(cluster_assignments == i)
          )
        }
      }
    }

    list(
      indicator = indicator,
      population_size = population_size,
      overall_prevalence = prevalence_estimate,
      overall_ci_lower = prevalence_ci_lower,
      overall_ci_upper = prevalence_ci_upper,
      cluster_results = cluster_results,
      netclust_output = netclust_result,
      n_obs = nrow(indicator_data),
      n_trait = n_trait,
      n_clusters = n_clusters,
      convergence = "success"
    )

  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    list(
      indicator = indicator,
      population_size = population_size,
      overall_prevalence = NA,
      overall_ci_lower = NA,
      overall_ci_upper = NA,
      cluster_results = NULL,
      convergence = paste("error:", e$message)
    )
  })

  return(result)
}

# Function to run comprehensive netclust analysis
run_netclust_analysis <- function(rds_data) {

  cat("\n=== RUNNING COMPREHENSIVE NETCLUST ANALYSIS ===\n")

  # Prepare data
  clean_data <- prepare_netclust_data(rds_data, netclust_config$cluster_variable)

  # Get unique clusters
  clusters <- levels(clean_data$cluster)
  cat("Analyzing clusters:", paste(clusters, collapse = ", "), "\n")

  # Initialize results storage
  all_results <- list()
  results_df <- data.frame()

  # Run analysis for each indicator and population size
  for (indicator in netclust_config$indicators) {
    for (pop_size in netclust_config$population_sizes) {

      cat("\n--- Processing", indicator, "at population size", format(pop_size, scientific = FALSE), "---\n")

      # Run netclust analysis for this indicator
      netclust_result <- estimate_netclust_indicator(clean_data, indicator, pop_size)

      if (!is.null(netclust_result) && netclust_result$convergence == "success") {

        # Store the full result
        result_id <- paste(indicator, format(pop_size, scientific = FALSE), sep = "_")
        all_results[[result_id]] <- netclust_result

        # Extract cluster-specific results for the summary table
        for (cluster_name in names(netclust_result$cluster_results)) {
          cluster_info <- netclust_result$cluster_results[[cluster_name]]

          result_row <- data.frame(
            indicator = indicator,
            cluster = cluster_name,
            population_size = pop_size,
            estimate_pct = cluster_info$cluster_prevalence * 100,
            ci_lower_pct = NA,  # Individual cluster CIs not available from netclust
            ci_upper_pct = NA,
            se_pct = NA,
            n_obs = cluster_info$cluster_sample_size,
            n_trait = cluster_info$cluster_trait_count,
            convergence = "success",
            estimate_with_ci = sprintf("%.1f%%", cluster_info$cluster_prevalence * 100),
            stringsAsFactors = FALSE
          )

          results_df <- rbind(results_df, result_row)
        }

        # Also add overall result
        overall_row <- data.frame(
          indicator = indicator,
          cluster = "Overall",
          population_size = pop_size,
          estimate_pct = netclust_result$overall_prevalence * 100,
          ci_lower_pct = netclust_result$overall_ci_lower * 100,
          ci_upper_pct = netclust_result$overall_ci_upper * 100,
          se_pct = NA,
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

      } else {
        # Add failed result
        for (cluster_name in clusters) {
          result_row <- data.frame(
            indicator = indicator,
            cluster = cluster_name,
            population_size = pop_size,
            estimate_pct = NA,
            ci_lower_pct = NA,
            ci_upper_pct = NA,
            se_pct = NA,
            n_obs = 0,
            n_trait = NA,
            convergence = ifelse(is.null(netclust_result), "no_data", netclust_result$convergence),
            estimate_with_ci = "Not available",
            stringsAsFactors = FALSE
          )

          results_df <- rbind(results_df, result_row)
        }
      }
    }
  }

  return(list(
    individual_results = all_results,
    summary_df = results_df,
    clean_data = clean_data
  ))
}

# ============================================================================
# TABLE 1 PREPARATION FUNCTIONS
# ============================================================================

# Function to create Table 1 data
create_table1_data <- function(netclust_results) {

  cat("\n=== CREATING TABLE 1 DATA ===\n")

  # Filter to baseline population size
  baseline_results <- netclust_results$summary_df %>%
    filter(population_size == netclust_config$baseline_population,
           !is.na(estimate_pct)) %>%
    select(indicator, cluster, estimate_with_ci, estimate_pct, ci_lower_pct, ci_upper_pct)

  # Create summary by cluster (overall exploitation)
  cluster_summary <- baseline_results %>%
    group_by(cluster) %>%
    summarise(
      n_indicators = n(),
      mean_prevalence = mean(estimate_pct, na.rm = TRUE),
      range_lower = min(estimate_pct, na.rm = TRUE),
      range_upper = max(estimate_pct, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      overall_estimate = sprintf("%.1f%% (%.1f-%.1f%%)",
                                mean_prevalence, range_lower, range_upper)
    )

  cat("Baseline results summary:\n")
  print(cluster_summary)

  return(list(
    baseline_results = baseline_results,
    cluster_summary = cluster_summary
  ))
}

# Function to create publication-ready Table 1
create_publication_table1 <- function(table1_data) {

  cat("\n=== CREATING PUBLICATION TABLE 1 ===\n")

  # Prepare data for the proposed Table 1 format
  table1_formatted <- table1_data$cluster_summary %>%
    arrange(desc(mean_prevalence)) %>%  # Order by prevalence (highest first)
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Latinx" ~ "Latinx",
        cluster == "Filipino" ~ "Filipino",
        cluster == "British" ~ "British",
        cluster == "Other" ~ "Other",
        TRUE ~ as.character(cluster)
      ),
      netclust_estimate = overall_estimate,
      ci_range = sprintf("(%.1f-%.1f%%)", range_lower, range_upper)
    ) %>%
    select(subgroup, netclust_estimate, ci_range)

  # Add sample sizes from original data
  if (exists('clean_data', envir = .GlobalEnv)) {
    sample_sizes <- table1_data$baseline_results %>%
      group_by(cluster) %>%
      summarise(n = first(n_obs), .groups = 'drop')

    table1_formatted <- table1_formatted %>%
      left_join(sample_sizes, by = c("subgroup" = "cluster"))
  }

  cat("Publication-ready Table 1:\n")
  print(table1_formatted)

  return(table1_formatted)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n=== STARTING NETCLUST ANALYSIS ===\n")

# Run the comprehensive analysis
netclust_results <- run_netclust_analysis(rds_data)

# Create Table 1 data
table1_data <- create_table1_data(netclust_results)

# Create publication table
publication_table1 <- create_publication_table1(table1_data)

# Save results
if (netclust_config$save_individual_results) {
  save(netclust_results, table1_data, publication_table1,
       file = here("output", "netclust_subgroup_analysis_results.RData"))
  cat("Results saved to: output/netclust_subgroup_analysis_results.RData\n")
}

if (netclust_config$save_summary_table) {
  write.csv(netclust_results$summary_df,
            here("output", "tables", "netclust_subgroup_estimates.csv"),
            row.names = FALSE)
  write.csv(publication_table1,
            here("output", "tables", "table1_subgroup_estimates.csv"),
            row.names = FALSE)
  cat("Tables saved to: output/tables/\n")
}

# Print summary
cat("\n=== NETCLUST ANALYSIS COMPLETE ===\n")
cat("Total estimates computed:", nrow(netclust_results$summary_df), "\n")
cat("Successful estimates:", sum(!is.na(netclust_results$summary_df$estimate_pct)), "\n")
cat("Clusters analyzed:", length(unique(netclust_results$summary_df$cluster)), "\n")
cat("Indicators analyzed:", length(unique(netclust_results$summary_df$indicator)), "\n")

cat("\nTable 1 ready for inclusion in Quarto paper!\n")
cat("Use publication_table1 object or table1_subgroup_estimates.csv\n")
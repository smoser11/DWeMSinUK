# 08b-simple_subgroup_analysis_FIXED.R
# PROPER RDS Subgroup Analysis with Neighborhood Bootstrap
# Fixed to use legitimate RDS estimation methods instead of naive bootstrap
#
# PURPOSE:
# - Create subgroup prevalence estimates by nationality_cluster using proper RDS methods
# - Use neighborhood bootstrap (neighboot) from Boot_Step1.r methodology
# - Apply MA.estimates() or RDS.SS.estimates() for each bootstrap replicate
# - Complete "Table 1 (proposed)" in the Quarto paper with methodologically sound estimates

cat("=== PROPER RDS SUBGROUP ANALYSIS FOR TABLE 1 ===\n")
cat("Using neighborhood bootstrap with RDS estimation methods\n")
cat("Flexible indicator specification with MA.estimates() or RDS.SS.estimates()\n\n")

# Load required libraries
library(RDS)
library(tidyverse)
library(here)
library(knitr)
library(kableExtra)

# Load data FIRST (before sourcing Boot_Step1.r)
load('data/processed/prepared_data.RData')

# Ensure both dd and rd.dd are available for different functions
if (!exists('rd.dd') || !exists('dd')) {
  stop("Required RDS data objects not found. Need both 'dd' and 'rd.dd' from prepared_data.RData")
}

# Make rd.dd available in global environment for Boot_Step1.r
assign("rd.dd", rd.dd, envir = .GlobalEnv)

# Now source bootstrap functions from Boot_Step1.r (test code will run with rd.dd available)
source(here("R", "analysis", "NSUM", "Boot_Step1.r"))

# Use rd.dd as primary (it's the rds.data.frame class)
rds_data <- rd.dd

cat("RDS data loaded:", nrow(rds_data), "observations\n")
cat("Data class:", class(rds_data), "\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

subgroup_config <- list(
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

  # RDS estimation methods
  estimation_methods = c("MA", "RDS_SS", "RDS_I", "RDS_II"),
  preferred_method = "RDS_SS",

  # Bootstrap parameters
  n_bootstrap = 500,  # Reasonable number for RDS bootstrap
  confidence_level = 0.95,

  # Population size scenarios (for RDS-SS)
  population_sizes = c(50000, 100000, 980000, 1740000),
  baseline_population = 980000,

  # Minimum sample size per cluster
  min_cluster_size = 10,

  # Output settings
  save_detailed_results = TRUE,
  verbose = TRUE
)

# ============================================================================
# SMART CLUSTER AGGREGATION (like in netclust fixed version)
# ============================================================================

create_smart_clusters <- function(rds_data, min_size = 10) {

  cat("=== Creating Smart Clusters for RDS Analysis ===\n")

  # Get original distribution
  orig_dist <- table(rds_data$nationality_cluster, useNA = 'ifany')
  cat("Original cluster sizes:\n")
  print(orig_dist)

  # Identify clusters that are too small
  small_clusters <- names(orig_dist)[orig_dist < min_size & !is.na(names(orig_dist))]
  large_clusters <- names(orig_dist)[orig_dist >= min_size & !is.na(names(orig_dist))]

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

  return(rds_data)
}

# ============================================================================
# RDS BOOTSTRAP ESTIMATION FUNCTIONS
# ============================================================================

# Function to calculate RDS estimate for a single indicator on RDS data
calculate_rds_estimate <- function(rds_data, indicator, method = "RDS_SS",
                                  population_size = 980000, verbose = FALSE) {

  if (verbose) cat("  Calculating", method, "estimate for", indicator, "\n")

  # Validate indicator exists and has sufficient data
  if (!indicator %in% names(rds_data)) {
    return(list(estimate = NA, error = "indicator not found"))
  }

  indicator_data <- rds_data[!is.na(rds_data[[indicator]]), ]
  if (nrow(indicator_data) < 5) {
    return(list(estimate = NA, error = "insufficient data"))
  }

  # Apply RDS estimation method
  result <- tryCatch({

    if (method == "MA") {
      # Model-Assisted estimates (Generalized VH)
      # Note: MA.estimates uses different parameter name
      ma_result <- MA.estimates(rds.data = indicator_data)
      # Extract point estimate
      estimate_val <- if (indicator %in% names(ma_result$estimate)) {
        as.numeric(ma_result$estimate[indicator])
      } else {
        as.numeric(ma_result$estimate[1])  # First estimate if name doesn't match
      }
      list(estimate = estimate_val, error = NULL)

    } else if (method == "RDS_SS") {
      # RDS Sequential Sampling estimates
      ss_result <- RDS.SS.estimates(rds.data = indicator_data,
                                   outcome.variable = indicator,
                                   N = population_size)
      # Extract point estimate from rds.interval.estimate object
      estimate_val <- as.numeric(ss_result$estimate[1])
      list(estimate = estimate_val, error = NULL)

    } else if (method == "RDS_I") {
      # RDS-I estimates (Salganik-Heckathorn)
      rds1_result <- RDS.I.estimates(rds.data = indicator_data,
                                    outcome.variable = indicator)
      # Extract point estimate from rds.interval.estimate object
      estimate_val <- as.numeric(rds1_result$estimate[1])
      list(estimate = estimate_val, error = NULL)

    } else if (method == "RDS_II") {
      # RDS-II estimates (Volz-Heckathorn)
      rds2_result <- RDS.II.estimates(rds.data = indicator_data,
                                     outcome.variable = indicator)
      # Extract point estimate from rds.interval.estimate object
      estimate_val <- as.numeric(rds2_result$estimate[1])
      list(estimate = estimate_val, error = NULL)

    } else {
      list(estimate = NA, error = "unknown method")
    }

  }, error = function(e) {
    if (verbose) cat("    Error in", method, ":", e$message, "\n")
    list(estimate = NA, error = e$message)
  })

  return(result)
}

# Function to perform neighborhood bootstrap RDS estimation
bootstrap_rds_subgroup_estimate <- function(rds_data, indicator, cluster_name = "Overall",
                                          method = "RDS_SS", population_size = 980000,
                                          n_bootstrap = 500, verbose = TRUE) {

  if (verbose) cat("\n--- Bootstrap RDS estimation for", indicator, "in", cluster_name, "cluster ---\n")

  # Validate data
  if (nrow(rds_data) < subgroup_config$min_cluster_size) {
    return(create_failed_rds_result(indicator, cluster_name, "insufficient_sample_size"))
  }

  # Calculate point estimate on original data
  point_result <- calculate_rds_estimate(rds_data, indicator, method, population_size, verbose)

  if (is.na(point_result$estimate)) {
    return(create_failed_rds_result(indicator, cluster_name, point_result$error))
  }

  point_estimate <- point_result$estimate
  if (verbose) cat("  Point estimate:", round(point_estimate, 4), "\n")

  # Perform neighborhood bootstrap resampling
  if (verbose) cat("  Generating", n_bootstrap, "bootstrap samples using neighborhood method...\n")

  bootstrap_samples <- tryCatch({
    bootstrap_rds_sample(
      rds_sample = rds_data,
      method = "neighboot",  # Use neighborhood bootstrap as specified
      B = n_bootstrap,
      traits = indicator,
      return_rds_df = TRUE,  # Return as rds.data.frame for RDS estimation
      verbose = FALSE
    )
  }, error = function(e) {
    if (verbose) cat("  Bootstrap sampling failed:", e$message, "\n")
    return(NULL)
  })

  if (is.null(bootstrap_samples)) {
    return(create_failed_rds_result(indicator, cluster_name, "bootstrap_sampling_failed"))
  }

  # Calculate RDS estimates for each bootstrap sample
  if (verbose) cat("  Computing RDS estimates for", length(bootstrap_samples), "bootstrap samples...\n")

  bootstrap_estimates <- numeric(length(bootstrap_samples))
  successful_estimates <- 0

  for (i in seq_along(bootstrap_samples)) {
    boot_result <- calculate_rds_estimate(bootstrap_samples[[i]], indicator, method,
                                        population_size, verbose = FALSE)

    if (!is.na(boot_result$estimate)) {
      bootstrap_estimates[i] <- boot_result$estimate
      successful_estimates <- successful_estimates + 1
    } else {
      bootstrap_estimates[i] <- NA
    }

    if (verbose && i %% 100 == 0) {
      cat("    Completed", i, "/", length(bootstrap_samples), "bootstrap estimates\n")
    }
  }

  # Calculate bootstrap confidence interval
  valid_estimates <- bootstrap_estimates[!is.na(bootstrap_estimates)]

  if (length(valid_estimates) < 50) {  # Need reasonable number for CI
    return(create_failed_rds_result(indicator, cluster_name,
                                   paste("insufficient_bootstrap_estimates:", length(valid_estimates))))
  }

  # Bootstrap percentile confidence interval
  ci_quantiles <- quantile(valid_estimates, c((1 - subgroup_config$confidence_level)/2,
                                              (1 + subgroup_config$confidence_level)/2))

  if (verbose) {
    cat("  Successful bootstrap estimates:", successful_estimates, "/", length(bootstrap_samples), "\n")
    cat("  Bootstrap CI: (", round(ci_quantiles[1], 4), ", ", round(ci_quantiles[2], 4), ")\n")
  }

  # Return comprehensive result
  return(list(
    indicator = indicator,
    cluster = cluster_name,
    method = method,
    population_size = population_size,
    point_estimate = point_estimate,
    bootstrap_mean = mean(valid_estimates),
    ci_lower = ci_quantiles[1],
    ci_upper = ci_quantiles[2],
    n_obs = nrow(rds_data),
    n_bootstrap_successful = length(valid_estimates),
    n_bootstrap_total = n_bootstrap,
    convergence = "success",

    # Formatted output
    prevalence_pct = point_estimate * 100,
    ci_lower_pct = ci_quantiles[1] * 100,
    ci_upper_pct = ci_quantiles[2] * 100,
    estimate_with_ci = sprintf("%.1f%% (%.1f-%.1f%%)",
                              point_estimate * 100,
                              ci_quantiles[1] * 100,
                              ci_quantiles[2] * 100),

    # Full bootstrap distribution for diagnostics
    bootstrap_estimates = valid_estimates
  ))
}

# Helper function for failed results
create_failed_rds_result <- function(indicator, cluster_name, reason) {
  list(
    indicator = indicator,
    cluster = cluster_name,
    point_estimate = NA,
    ci_lower = NA,
    ci_upper = NA,
    n_obs = 0,
    convergence = reason,
    estimate_with_ci = "Failed"
  )
}

# ============================================================================
# MAIN SUBGROUP ANALYSIS FUNCTION
# ============================================================================

run_rds_subgroup_analysis <- function(rds_data) {

  cat("\n=== RUNNING PROPER RDS SUBGROUP ANALYSIS ===\n")

  # Apply smart clustering
  rds_data <- create_smart_clusters(rds_data, subgroup_config$min_cluster_size)
  cluster_var <- "nationality_cluster_combined"

  # Get cluster distribution
  cluster_counts <- table(rds_data[[cluster_var]], useNA = "ifany")
  cat("Final cluster distribution for analysis:\n")
  print(cluster_counts)

  # Initialize results storage
  all_results <- list()
  results_df <- data.frame()

  # Overall analysis (all clusters combined)
  cat("\n=== Overall Analysis (All Clusters) ===\n")

  for (indicator in subgroup_config$indicators) {

    cat("\nProcessing", indicator, "for overall sample...\n")

    overall_result <- bootstrap_rds_subgroup_estimate(
      rds_data = rds_data,
      indicator = indicator,
      cluster_name = "Overall",
      method = subgroup_config$preferred_method,
      population_size = subgroup_config$baseline_population,
      n_bootstrap = subgroup_config$n_bootstrap,
      verbose = subgroup_config$verbose
    )

    # Store result
    result_id <- paste("Overall", indicator, sep = "_")
    all_results[[result_id]] <- overall_result

    # Add to summary dataframe
    results_df <- rbind(results_df, create_results_row(overall_result))

    cat("  Overall", indicator, ":", overall_result$estimate_with_ci, "\n")
  }

  # Cluster-specific analysis
  clusters <- names(cluster_counts)[!is.na(names(cluster_counts))]

  for (cluster_name in clusters) {

    cat("\n=== Analysis for", cluster_name, "cluster ===\n")

    # Filter to cluster
    cluster_data <- rds_data[rds_data[[cluster_var]] == cluster_name &
                            !is.na(rds_data[[cluster_var]]), ]

    cat("Sample size:", nrow(cluster_data), "\n")

    if (nrow(cluster_data) < subgroup_config$min_cluster_size) {
      cat("  Skipping cluster due to insufficient sample size\n")
      next
    }

    for (indicator in subgroup_config$indicators) {

      cat("\nProcessing", indicator, "for", cluster_name, "cluster...\n")

      cluster_result <- bootstrap_rds_subgroup_estimate(
        rds_data = cluster_data,
        indicator = indicator,
        cluster_name = cluster_name,
        method = subgroup_config$preferred_method,
        population_size = subgroup_config$baseline_population,
        n_bootstrap = subgroup_config$n_bootstrap,
        verbose = subgroup_config$verbose
      )

      # Store result
      result_id <- paste(cluster_name, indicator, sep = "_")
      all_results[[result_id]] <- cluster_result

      # Add to summary dataframe
      results_df <- rbind(results_df, create_results_row(cluster_result))

      cat("  ", cluster_name, indicator, ":", cluster_result$estimate_with_ci, "\n")
    }
  }

  return(list(
    individual_results = all_results,
    summary_df = results_df,
    rds_data = rds_data,
    config = subgroup_config
  ))
}

# Helper function to create results row
create_results_row <- function(result) {
  data.frame(
    indicator = result$indicator,
    cluster = result$cluster,
    method = if(is.null(result$method)) subgroup_config$preferred_method else result$method,
    population_size = if(is.null(result$population_size)) subgroup_config$baseline_population else result$population_size,
    prevalence_pct = if(is.null(result$prevalence_pct)) NA else result$prevalence_pct,
    ci_lower_pct = if(is.null(result$ci_lower_pct)) NA else result$ci_lower_pct,
    ci_upper_pct = if(is.null(result$ci_upper_pct)) NA else result$ci_upper_pct,
    n_obs = if(is.null(result$n_obs)) 0 else result$n_obs,
    convergence = result$convergence,
    estimate_with_ci = result$estimate_with_ci,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# TABLE 1 CREATION FUNCTIONS (unchanged from original)
# ============================================================================

create_table1_summary <- function(results) {

  cat("\n=== CREATING TABLE 1 SUMMARY ===\n")

  # Calculate overall exploitation prevalence by cluster
  cluster_summary <- results$summary_df %>%
    filter(convergence == "success") %>%
    group_by(cluster) %>%
    summarise(
      n_indicators = n(),
      mean_prevalence = mean(prevalence_pct, na.rm = TRUE),
      median_prevalence = median(prevalence_pct, na.rm = TRUE),
      range_lower = min(prevalence_pct, na.rm = TRUE),
      range_upper = max(prevalence_pct, na.rm = TRUE),
      sample_size = first(n_obs),
      .groups = 'drop'
    ) %>%
    mutate(
      # Create summary estimate (using median as more robust)
      summary_estimate = sprintf("%.1f%% (%.1f-%.1f%%)",
                                median_prevalence, range_lower, range_upper),

      # Order by prevalence
      prevalence_rank = rank(-median_prevalence, ties.method = "first", na.last = TRUE)
    ) %>%
    arrange(prevalence_rank)

  cat("Cluster summary:\n")
  print(cluster_summary)

  return(cluster_summary)
}

create_publication_table1 <- function(cluster_summary) {

  cat("\n=== CREATING PUBLICATION TABLE 1 ===\n")

  # Format for the paper
  table1_publication <- cluster_summary %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Other_Combined" ~ "Other nationalities",
        TRUE ~ as.character(cluster)
      ),

      # Format estimates for table
      rds_estimate = summary_estimate,
      sample_n = paste0("(n=", sample_size, ")")
    ) %>%
    select(subgroup, rds_estimate, sample_n) %>%
    arrange(case_when(
      subgroup == "Overall sample" ~ 1,
      subgroup == "Filipino" ~ 2,
      subgroup == "Other nationalities" ~ 3,
      TRUE ~ 4
    ))

  cat("Publication-ready Table 1 (basic):\n")
  print(table1_publication)

  return(table1_publication)
}

# ============================================================================
# PROFESSIONAL LATEX TABLE CREATION FUNCTIONS
# ============================================================================

create_latex_table1 <- function(cluster_summary, detailed_results = NULL) {

  cat("\n=== CREATING PROFESSIONAL LaTeX TABLE 1 ===\n")

  # Create main summary table
  table1_data <- cluster_summary %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Other_Combined" ~ "Other nationalities",
        cluster == "Other" ~ "Other nationalities",  # Handle both naming conventions
        TRUE ~ as.character(cluster)
      ),

      # Clean estimate formatting
      prevalence_range = sprintf("%.1f–%.1f", range_lower, range_upper),
      median_est = sprintf("%.1f", median_prevalence),
      sample_size_formatted = as.character(sample_size)
    ) %>%
    select(subgroup, median_est, prevalence_range, sample_size_formatted) %>%
    arrange(case_when(
      subgroup == "Overall sample" ~ 1,
      subgroup == "Filipino" ~ 2,
      subgroup == "Other nationalities" ~ 3,
      TRUE ~ 4
    ))

  # Create basic kable table
  basic_table <- table1_data %>%
    kable(format = "latex",
          booktabs = TRUE,
          col.names = c("Subgroup", "Median (%)", "Range (%)", "n"),
          caption = "Prevalence of labour exploitation among domestic workers by nationality subgroup",
          label = "tab:subgroup-prevalence",
          align = c("l", "r", "c", "r"),
          escape = FALSE) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      stripe_color = "gray!6",
      font_size = 11,
      position = "center"
    ) %>%
    add_header_above(c(" " = 1, "Labour Exploitation Prevalence" = 2, " " = 1),
                     bold = TRUE) %>%
    footnote(
      general = c(
        "Estimates based on RDS-SS methodology with neighborhood bootstrap confidence intervals (500 replicates).",
        "Prevalence represents median across five comparable indicators: document withholding, pay issues, threats/abuse, excessive hours, and access to help.",
        "Range shows minimum to maximum prevalence across indicators within each subgroup.",
        paste("Population size assumption:", format(subgroup_config$baseline_population, big.mark = ","))
      ),
      general_title = "Note:",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )

  return(basic_table)
}

create_detailed_latex_table <- function(results_df) {

  cat("\n=== CREATING DETAILED LaTeX TABLE (All Indicators) ===\n")

  # Filter to successful estimates only
  detailed_data <- results_df %>%
    filter(convergence == "success", !is.na(prevalence_pct)) %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Other_Combined" ~ "Other nationalities",
        cluster == "Other" ~ "Other nationalities",
        TRUE ~ as.character(cluster)
      ),

      # Clean indicator names
      indicator_clean = case_when(
        indicator == "document_withholding_rds" ~ "Document withholding",
        indicator == "pay_issues_rds" ~ "Pay-related issues",
        indicator == "threats_abuse_rds" ~ "Threats and abuse",
        indicator == "excessive_hours_rds" ~ "Excessive working hours",
        indicator == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ indicator
      ),

      # Format estimates with CIs
      formatted_estimate = sprintf("%.1f (%.1f–%.1f)",
                                 prevalence_pct,
                                 ci_lower_pct,
                                 ci_upper_pct),

      sample_size_clean = as.character(n_obs)
    ) %>%
    select(subgroup, indicator_clean, formatted_estimate, sample_size_clean) %>%
    arrange(
      case_when(
        subgroup == "Overall sample" ~ 1,
        subgroup == "Filipino" ~ 2,
        subgroup == "Other nationalities" ~ 3,
        TRUE ~ 4
      ),
      indicator_clean
    )

  # Create detailed kable table
  detailed_table <- detailed_data %>%
    kable(format = "latex",
          booktabs = TRUE,
          longtable = TRUE,
          col.names = c("Subgroup", "Indicator", "Prevalence (95% CI)", "n"),
          caption = "Detailed prevalence estimates by subgroup and exploitation indicator",
          label = "tab:detailed-subgroup-prevalence",
          align = c("l", "l", "r", "r"),
          escape = FALSE) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      stripe_color = "gray!6",
      font_size = 10,
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, width = "3cm") %>%
    column_spec(2, width = "4cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "1.5cm") %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
      general = c(
        "Estimates based on RDS-SS methodology with neighborhood bootstrap (500 replicates).",
        "95% confidence intervals derived from bootstrap distribution of RDS estimates.",
        paste("Population size assumption:", format(subgroup_config$baseline_population, big.mark = ","))
      ),
      general_title = "Note:",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )

  return(detailed_table)
}

create_quarto_table_code <- function(cluster_summary, results_df) {

  cat("\n=== CREATING QUARTO/LaTeX CODE FOR PAPER ===\n")

  # Generate Quarto chunk for main table
  quarto_main_table <- paste0(
    '```{r}\n',
    '#| label: tbl-subgroup-prevalence\n',
    '#| tbl-cap: "Prevalence of labour exploitation among domestic workers by nationality subgroup. Estimates based on RDS-SS methodology with neighborhood bootstrap confidence intervals."\n',
    '#| echo: false\n',
    '#| warning: false\n\n',
    'library(knitr)\n',
    'library(kableExtra)\n',
    'library(here)\n\n',
    '# Load the results data\n',
    'load(here("output", "rds_subgroup_analysis_results_FIXED.RData"))\n\n',
    '# Create and display main table\n',
    'latex_table1 <- create_latex_table1(cluster_summary)\n',
    'latex_table1\n',
    '```\n\n'
  )

  # Generate Quarto chunk for detailed table
  quarto_detailed_table <- paste0(
    '```{r}\n',
    '#| label: tbl-detailed-subgroup-prevalence\n',
    '#| tbl-cap: "Detailed prevalence estimates by subgroup and exploitation indicator. All estimates use RDS-SS methodology with bootstrap confidence intervals."\n',
    '#| echo: false\n',
    '#| warning: false\n\n',
    '# Create and display detailed table\n',
    'detailed_latex_table <- create_detailed_latex_table(rds_subgroup_results$summary_df)\n',
    'detailed_latex_table\n',
    '```\n\n'
  )

  # Save Quarto code to file
  quarto_code <- paste0(quarto_main_table, quarto_detailed_table)

  writeLines(quarto_code, here("output", "latex_tables_quarto_code.txt"))

  cat("Quarto/LaTeX table code saved to: output/latex_tables_quarto_code.txt\n")

  return(list(
    main_table_code = quarto_main_table,
    detailed_table_code = quarto_detailed_table,
    combined_code = quarto_code
  ))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n=== STARTING PROPER RDS SUBGROUP ANALYSIS ===\n")

# Run the analysis
rds_subgroup_results <- run_rds_subgroup_analysis(rds_data)

# Create summaries
cluster_summary <- create_table1_summary(rds_subgroup_results)
publication_table1 <- create_publication_table1(cluster_summary)

# Create professional LaTeX tables
latex_table1 <- create_latex_table1(cluster_summary)
detailed_latex_table <- create_detailed_latex_table(rds_subgroup_results$summary_df)
quarto_table_codes <- create_quarto_table_code(cluster_summary, rds_subgroup_results$summary_df)

# Save results
if (subgroup_config$save_detailed_results) {
  save(rds_subgroup_results, cluster_summary, publication_table1,
       latex_table1, detailed_latex_table, quarto_table_codes,
       create_latex_table1, create_detailed_latex_table,  # Include functions for later use
       file = here("output", "rds_subgroup_analysis_results_FIXED.RData"))

  # Save CSV files
  write.csv(rds_subgroup_results$summary_df,
            here("output", "tables", "rds_subgroup_detailed_results.csv"),
            row.names = FALSE)

  write.csv(publication_table1,
            here("output", "tables", "table1_rds_publication_ready.csv"),
            row.names = FALSE)

  # Save LaTeX table outputs
  writeLines(as.character(latex_table1), here("output", "tables", "latex_table1_main.tex"))
  writeLines(as.character(detailed_latex_table), here("output", "tables", "latex_table_detailed.tex"))
}

# Print summary
cat("\n=== PROPER RDS SUBGROUP ANALYSIS COMPLETE ===\n")
cat("Results saved to: output/rds_subgroup_analysis_results_FIXED.RData\n")
cat("Tables saved to: output/tables/\n")

cat("\n=== PROFESSIONAL LaTeX TABLES CREATED ===\n")
cat("✓ Main summary table: output/tables/latex_table1_main.tex\n")
cat("✓ Detailed indicator table: output/tables/latex_table_detailed.tex\n")
cat("✓ Quarto code chunks: output/latex_tables_quarto_code.txt\n")

cat("\n=== SUMMARY FINDINGS ===\n")
cat("Clusters ranked by exploitation prevalence (median across indicators):\n")
for (i in 1:nrow(cluster_summary)) {
  row <- cluster_summary[i, ]
  cat(i, ". ", row$cluster, ": ", row$summary_estimate, " (n=", row$sample_size, ")\n", sep = "")
}

successful_estimates <- sum(rds_subgroup_results$summary_df$convergence == "success")
total_estimates <- nrow(rds_subgroup_results$summary_df)

cat("\nEstimation success rate:", successful_estimates, "/", total_estimates,
    "(", round(100 * successful_estimates / total_estimates, 1), "%)\n")

cat("\n=== LaTeX TABLE PREVIEW ===\n")
cat("Main table columns: Subgroup | Median (%) | Range (%) | n\n")
cat("Detailed table columns: Subgroup | Indicator | Prevalence (95% CI) | n\n")
cat("Features: booktabs formatting, striped rows, professional footnotes\n")

cat("\n✓ Professional LaTeX tables ready for journal submission!\n")
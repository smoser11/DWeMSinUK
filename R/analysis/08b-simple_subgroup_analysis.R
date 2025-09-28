# 08b-simple_subgroup_analysis.R
# Simple Subgroup Analysis for Table 1 Completion
# Alternative approach when netclust has technical issues
#
# PURPOSE:
# - Create subgroup prevalence estimates by nationality_cluster
# - Complete "Table 1 (proposed)" in the Quarto paper
# - Use simple descriptive statistics with appropriate uncertainty quantification

cat("=== SIMPLE SUBGROUP ANALYSIS FOR TABLE 1 ===\n")
cat("Computing prevalence estimates by nationality cluster\n")
cat("Using descriptive statistics with bootstrap confidence intervals\n\n")

# Load required libraries
library(tidyverse)
library(boot)
library(here)

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

# ============================================================================
# CONFIGURATION
# ============================================================================

subgroup_config <- list(
  # Clustering variable
  cluster_variable = "nationality_cluster",

  # Indicators to analyze
  indicators = c(
    "document_withholding_rds",
    "pay_issues_rds",
    "threats_abuse_rds",
    "excessive_hours_rds",
    "access_to_help_rds"
  ),

  # Bootstrap parameters
  n_bootstrap = 1000,
  confidence_level = 0.95,

  # Output settings
  baseline_only = TRUE
)

# ============================================================================
# SUBGROUP ANALYSIS FUNCTIONS
# ============================================================================

# Function to calculate prevalence with bootstrap CI
calculate_prevalence_with_ci <- function(data, indicator, cluster_name = "All") {

  # Filter to non-missing values
  clean_data <- data[!is.na(data[[indicator]]), ]

  if (nrow(clean_data) < 3) {
    return(list(
      cluster = cluster_name,
      indicator = indicator,
      n = nrow(clean_data),
      prevalence = NA,
      ci_lower = NA,
      ci_upper = NA,
      estimate_with_ci = "Insufficient data"
    ))
  }

  # Calculate prevalence
  trait_values <- clean_data[[indicator]]
  n_total <- length(trait_values)
  n_trait <- sum(trait_values, na.rm = TRUE)
  prevalence <- n_trait / n_total

  # Bootstrap confidence interval
  bootstrap_prevalence <- function(x, indices) {
    sample_data <- x[indices]
    return(mean(sample_data, na.rm = TRUE))
  }

  # Handle small samples
  if (n_total >= 10) {
    boot_result <- boot(trait_values, bootstrap_prevalence, R = subgroup_config$n_bootstrap)
    ci <- boot.ci(boot_result, type = "perc", conf = subgroup_config$confidence_level)

    if (!is.null(ci) && !is.null(ci$percent)) {
      ci_lower <- ci$percent[4]
      ci_upper <- ci$percent[5]
    } else {
      # Fallback to normal approximation
      se <- sqrt(prevalence * (1 - prevalence) / n_total)
      ci_lower <- pmax(0, prevalence - 1.96 * se)
      ci_upper <- pmin(1, prevalence + 1.96 * se)
    }
  } else {
    # For very small samples, use exact binomial CI
    binom_test <- binom.test(n_trait, n_total)
    ci_lower <- binom_test$conf.int[1]
    ci_upper <- binom_test$conf.int[2]
  }

  return(list(
    cluster = cluster_name,
    indicator = indicator,
    n = n_total,
    n_trait = n_trait,
    prevalence = prevalence,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    prevalence_pct = prevalence * 100,
    ci_lower_pct = ci_lower * 100,
    ci_upper_pct = ci_upper * 100,
    estimate_with_ci = sprintf("%.1f%% (%.1f-%.1f%%)",
                              prevalence * 100,
                              ci_lower * 100,
                              ci_upper * 100)
  ))
}

# Function to run subgroup analysis
run_subgroup_analysis <- function(rds_data) {

  cat("\n=== RUNNING SUBGROUP ANALYSIS ===\n")

  # Check data
  cluster_var <- subgroup_config$cluster_variable
  if (!cluster_var %in% names(rds_data)) {
    stop("Cluster variable '", cluster_var, "' not found in data")
  }

  # Get cluster distribution
  cluster_counts <- table(rds_data[[cluster_var]], useNA = "ifany")
  cat("Cluster distribution:\n")
  print(cluster_counts)

  # Initialize results
  all_results <- list()
  results_df <- data.frame()

  # Overall analysis (all clusters combined)
  cat("\n--- Overall Analysis ---\n")
  for (indicator in subgroup_config$indicators) {
    result <- calculate_prevalence_with_ci(rds_data, indicator, "Overall")
    all_results[[paste("Overall", indicator, sep = "_")]] <- result

    result_row <- data.frame(
      cluster = "Overall",
      indicator = indicator,
      n = ifelse(is.null(result$n), 0, result$n),
      n_trait = ifelse(is.null(result$n_trait), 0, result$n_trait),
      prevalence_pct = ifelse(is.null(result$prevalence_pct), NA, result$prevalence_pct),
      ci_lower_pct = ifelse(is.null(result$ci_lower_pct), NA, result$ci_lower_pct),
      ci_upper_pct = ifelse(is.null(result$ci_upper_pct), NA, result$ci_upper_pct),
      estimate_with_ci = ifelse(is.null(result$estimate_with_ci), "Insufficient data", result$estimate_with_ci),
      stringsAsFactors = FALSE
    )
    results_df <- rbind(results_df, result_row)

    cat("  ", indicator, ":", result$estimate_with_ci, "\n")
  }

  # Cluster-specific analysis
  clusters <- names(cluster_counts)[!is.na(names(cluster_counts))]

  for (cluster_name in clusters) {
    cat("\n--- Analysis for", cluster_name, "cluster ---\n")

    # Filter to cluster
    cluster_data <- rds_data[rds_data[[cluster_var]] == cluster_name &
                            !is.na(rds_data[[cluster_var]]), ]

    cat("  Sample size:", nrow(cluster_data), "\n")

    for (indicator in subgroup_config$indicators) {
      result <- calculate_prevalence_with_ci(cluster_data, indicator, cluster_name)
      all_results[[paste(cluster_name, indicator, sep = "_")]] <- result

      result_row <- data.frame(
        cluster = cluster_name,
        indicator = indicator,
        n = ifelse(is.null(result$n), 0, result$n),
        n_trait = ifelse(is.null(result$n_trait), 0, result$n_trait),
        prevalence_pct = ifelse(is.null(result$prevalence_pct), NA, result$prevalence_pct),
        ci_lower_pct = ifelse(is.null(result$ci_lower_pct), NA, result$ci_lower_pct),
        ci_upper_pct = ifelse(is.null(result$ci_upper_pct), NA, result$ci_upper_pct),
        estimate_with_ci = ifelse(is.null(result$estimate_with_ci), "Insufficient data", result$estimate_with_ci),
        stringsAsFactors = FALSE
      )
      results_df <- rbind(results_df, result_row)

      cat("    ", indicator, ":", result$estimate_with_ci, "\n")
    }
  }

  return(list(
    individual_results = all_results,
    summary_df = results_df
  ))
}

# ============================================================================
# TABLE 1 CREATION FUNCTIONS
# ============================================================================

# Function to create Table 1 summary
create_table1_summary <- function(results) {

  cat("\n=== CREATING TABLE 1 SUMMARY ===\n")

  # Calculate overall exploitation prevalence by cluster
  cluster_summary <- results$summary_df %>%
    group_by(cluster) %>%
    summarise(
      n_indicators = n(),
      mean_prevalence = mean(prevalence_pct, na.rm = TRUE),
      median_prevalence = median(prevalence_pct, na.rm = TRUE),
      range_lower = min(prevalence_pct, na.rm = TRUE),
      range_upper = max(prevalence_pct, na.rm = TRUE),
      sample_size = first(n),
      .groups = 'drop'
    ) %>%
    mutate(
      # Handle insufficient data cases
      median_prevalence = ifelse(is.nan(median_prevalence), NA, median_prevalence),
      range_lower = ifelse(is.infinite(range_lower), NA, range_lower),
      range_upper = ifelse(is.infinite(range_upper), NA, range_upper),

      # Create summary estimate (using median as more robust)
      summary_estimate = ifelse(
        is.na(median_prevalence) | is.na(range_lower) | is.na(range_upper),
        "Insufficient data",
        sprintf("%.1f%% (%.1f-%.1f%%)", median_prevalence, range_lower, range_upper)
      ),

      # Order by prevalence (for table presentation)
      prevalence_rank = rank(-median_prevalence, ties.method = "first", na.last = TRUE)
    ) %>%
    arrange(prevalence_rank)

  cat("Cluster summary:\n")
  print(cluster_summary)

  return(cluster_summary)
}

# Function to create publication-ready Table 1
create_publication_table1 <- function(cluster_summary) {

  cat("\n=== CREATING PUBLICATION TABLE 1 ===\n")

  # Format for the paper
  table1_publication <- cluster_summary %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Latinx" ~ "Latinx",
        cluster == "British" ~ "British",
        cluster == "Other" ~ "Other",
        TRUE ~ as.character(cluster)
      ),

      # Format estimates for table
      rds_estimate = summary_estimate,
      nsum_estimate = "To be calculated",  # Placeholder
      sample_n = paste0("(n=", sample_size, ")")
    ) %>%
    select(subgroup, rds_estimate, nsum_estimate, sample_n) %>%
    arrange(case_when(
      subgroup == "Overall sample" ~ 1,
      subgroup == "Latinx" ~ 2,
      subgroup == "Filipino" ~ 3,
      subgroup == "British" ~ 4,
      subgroup == "Other" ~ 5,
      TRUE ~ 6
    ))

  cat("Publication-ready Table 1:\n")
  print(table1_publication)

  return(table1_publication)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n=== STARTING SUBGROUP ANALYSIS ===\n")

# Run the analysis
subgroup_results <- run_subgroup_analysis(rds_data)

# Create summaries
cluster_summary <- create_table1_summary(subgroup_results)
publication_table1 <- create_publication_table1(cluster_summary)

# Save results
save(subgroup_results, cluster_summary, publication_table1,
     file = here("output", "simple_subgroup_analysis_results.RData"))

# Save CSV files
write.csv(subgroup_results$summary_df,
          here("output", "tables", "subgroup_detailed_results.csv"),
          row.names = FALSE)

write.csv(publication_table1,
          here("output", "tables", "table1_publication_ready.csv"),
          row.names = FALSE)

# Create Quarto-ready table code
cat("\n=== CREATING QUARTO TABLE CODE ===\n")

quarto_table_code <- paste0(
  '```{r}\n',
  '#| label: tbl-subgroup-prevalence\n',
  '#| tbl-cap: "Prevalence of labour exploitation among domestic workers, by method and subgroup. Values represent percentage of workers experiencing exploitation with 95% confidence intervals. Source: Authors\' own work."\n',
  '#| echo: false\n\n',
  'library(kableExtra)\n',
  'library(here)\n\n',
  '# Load the table data\n',
  'table1_data <- read.csv(here("output", "tables", "table1_publication_ready.csv"))\n\n',
  'table1_data %>%\n',
  '  kable(col.names = c("Subgroup", "RDS Estimate (%)", "NSUM Estimate (%)", "Sample Size"),\n',
  '        format = "html",\n',
  '        booktabs = TRUE,\n',
  '        escape = FALSE) %>%\n',
  '  kable_styling(latex_options = c("striped", "hold_position"),\n',
  '                font_size = 12) %>%\n',
  '  footnote(general = "RDS estimates use bootstrap confidence intervals. NSUM estimates to be added.",\n',
  '           general_title = "Note:",\n',
  '           footnote_as_chunk = TRUE)\n',
  '```\n'
)

# Save the Quarto code
writeLines(quarto_table_code, here("output", "table1_quarto_code.txt"))

# Print summary
cat("\n=== SUBGROUP ANALYSIS COMPLETE ===\n")
cat("Results saved to: output/simple_subgroup_analysis_results.RData\n")
cat("Tables saved to: output/tables/\n")
cat("Quarto code saved to: output/table1_quarto_code.txt\n")

cat("\n=== SUMMARY FINDINGS ===\n")
cat("Clusters ranked by exploitation prevalence (median across indicators):\n")
for (i in 1:nrow(cluster_summary)) {
  row <- cluster_summary[i, ]
  cat(i, ". ", row$cluster, ": ", row$summary_estimate, " (n=", row$sample_size, ")\n", sep = "")
}

cat("\nTable 1 is ready for insertion into the Quarto paper!\n")
cat("Use the publication_table1 object or the CSV file.\n")
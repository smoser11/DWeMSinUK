# 07-comprehensive_comparison.R  
# Comprehensive Results Comparison Across All Production Estimators
# PRODUCTION VERSION: Clean comparison of 06a + 06b + 06c results
#
# COMPARES:
# - 06a: Frequentist RDS methods (RDS-I, RDS-II, RDS-SS) with bootstrap CIs
# - 06b: Core Bayesian MA.estimates with adaptive parameters  
# - 06c: Bayesian sensitivity analysis (population × seed selection)
#
# OUTPUTS:
# - Method comparison visualizations
# - Publication-ready tables with confidence intervals
# - Population sensitivity analysis
# - Parameter sensitivity analysis

cat("=== COMPREHENSIVE RESULTS COMPARISON (Production) ===\n")
cat("Comparing results from 06a (Frequentist) + 06b (Bayesian) + 06c (Sensitivity)\n")
cat("Outputs: Visualizations + Publication tables\n\n")

# Load required libraries
library(tidyverse)
library(here)
library(scales)
library(viridis)
library(RColorBrewer)
library(gridExtra)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# ============================================================================
# LOAD ALL PRODUCTION RESULTS
# ============================================================================

load_production_results <- function() {
  
  cat("=== Loading Production Results ===\n")
  
  all_results <- list()
  
  # 1. Load Frequentist Results (06a)
  freq_file <- here("output", "frequentist_estimators_results.RData")
  if (file.exists(freq_file)) {
    cat("Loading frequentist results (06a)...\n")
    freq_env <- new.env()
    load(freq_file, envir = freq_env)
    
    if (exists("frequentist_df", envir = freq_env)) {
      freq_df <- get("frequentist_df", envir = freq_env)
      freq_df$analysis_source <- "06a_frequentist"
      all_results$frequentist <- freq_df
      cat("- Frequentist:", nrow(freq_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Frequentist results not found\n")
  }
  
  # 2. Load Core Bayesian Results (06b)
  bayes_file <- here("output", "bayesian_estimators_results.RData")
  if (file.exists(bayes_file)) {
    cat("Loading core Bayesian results (06b)...\n")
    bayes_env <- new.env()
    load(bayes_file, envir = bayes_env)
    
    if (exists("bayesian_df", envir = bayes_env)) {
      bayes_df <- get("bayesian_df", envir = bayes_env)
      bayes_df$analysis_source <- "06b_bayesian_core"
      # Add missing columns for compatibility
      if (!"pop_label" %in% names(bayes_df)) bayes_df$pop_label <- "980K"
      if (!"seed_selection" %in% names(bayes_df)) bayes_df$seed_selection <- "adaptive"
      all_results$bayesian_core <- bayes_df
      cat("- Core Bayesian:", nrow(bayes_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Core Bayesian results not found\n")
  }
  
  # 3. Load Sensitivity Analysis Results (06c)
  sens_file <- here("output", "bayesian_sensitivity_results.RData")
  if (file.exists(sens_file)) {
    cat("Loading Bayesian sensitivity results (06c)...\n")
    sens_env <- new.env()
    load(sens_file, envir = sens_env)
    
    if (exists("sensitivity_df", envir = sens_env)) {
      sens_df <- get("sensitivity_df", envir = sens_env)
      sens_df$analysis_source <- "06c_bayesian_sensitivity"
      all_results$bayesian_sensitivity <- sens_df
      cat("- Sensitivity analysis:", nrow(sens_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Sensitivity analysis results not found\n")
  }
  
  cat("Production results loaded successfully!\n\n")
  return(all_results)
}

# ============================================================================
# UNIFIED DATA PROCESSING
# ============================================================================

create_unified_dataset <- function(results_list) {
  
  cat("=== Creating Unified Dataset ===\n")
  
  # Standardize column names across all datasets
  standardize_columns <- function(df, source_name) {
    
    # Ensure required columns exist
    required_cols <- c("indicator", "method", "estimate", "ci_lower", "ci_upper", 
                      "method_type", "indicator_clean", "method_clean", 
                      "estimate_pct", "ci_lower_pct", "ci_upper_pct", "estimate_with_ci")
    
    # Add missing columns with defaults
    for (col in required_cols) {
      if (!col %in% names(df)) {
        if (col %in% c("indicator_clean", "method_clean")) {
          df[[col]] <- df[[str_remove(col, "_clean")]] # Use base column if clean version missing
        } else if (col %in% c("estimate_pct", "ci_lower_pct", "ci_upper_pct")) {
          base_col <- str_remove(col, "_pct")
          if (base_col %in% names(df)) {
            df[[col]] <- df[[base_col]] * 100
          } else {
            df[[col]] <- NA
          }
        } else {
          df[[col]] <- NA
        }
      }
    }
    
    # Standardize method names
    df <- df %>%
      mutate(
        method_clean = case_when(
          str_detect(method, "RDS_I|RDS-I") ~ "RDS-I",
          str_detect(method, "RDS_II|RDS-II") ~ "RDS-II",
          str_detect(method, "RDS_SS|RDS-SS") ~ "RDS-SS",
          str_detect(method, "MA_estimates|MA.estimates") ~ "MA.estimates",
          TRUE ~ method
        ),
        method_type = case_when(
          method_clean %in% c("RDS-I", "RDS-II", "RDS-SS") ~ "frequentist",
          method_clean == "MA.estimates" ~ "bayesian",
          TRUE ~ method_type
        )
      )
    
    # Add source identifier
    df$source <- source_name
    
    return(df)
  }
  
  # Standardize all datasets
  standardized_results <- map2(results_list, names(results_list), standardize_columns)
  
  # Combine into unified dataset
  unified_df <- bind_rows(standardized_results)
  
  # Add additional derived columns
  unified_df <- unified_df %>%
    mutate(
      # Population size grouping
      pop_group = case_when(
        is.na(pop_label) ~ "Single",
        pop_label == "50K" ~ "Conservative",
        pop_label == "100K" ~ "Conservative", 
        pop_label == "980K" ~ "Baseline",
        pop_label == "1.74M" ~ "Liberal",
        TRUE ~ "Other"
      ),
      
      # Method category
      method_category = case_when(
        method_clean %in% c("RDS-I", "RDS-II", "RDS-SS") ~ "Frequentist RDS",
        method_clean == "MA.estimates" ~ "Bayesian MA",
        TRUE ~ "Other"
      ),
      
      # Parameter type (for Bayesian methods)
      param_group = case_when(
        !is.na(parameter_type) ~ parameter_type,
        source == "bayesian_core" ~ "adaptive",
        TRUE ~ "standard"
      ),
      
      # Confidence interval width
      ci_width = ci_upper_pct - ci_lower_pct,
      
      # Success indicator
      has_estimate = !is.na(estimate) & !is.na(ci_lower) & !is.na(ci_upper)
    )
  
  cat("Unified dataset created:\n")
  cat("- Total results:", nrow(unified_df), "\n")
  cat("- Methods:", paste(unique(unified_df$method_clean), collapse = ", "), "\n")
  cat("- Sources:", paste(unique(unified_df$source), collapse = ", "), "\n")
  cat("- Indicators:", length(unique(unified_df$indicator_clean)), "\n")
  cat("- Successful estimates:", sum(unified_df$has_estimate, na.rm = TRUE), "\n\n")
  
  return(unified_df)
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

# Method comparison plot
create_method_comparison_plot <- function(unified_df) {
  
  cat("Creating method comparison visualization...\n")
  
  # Filter to 980K baseline for main comparison
  plot_data <- unified_df %>%
    filter(has_estimate, 
           pop_label %in% c("980K", NA),  # Include single population results
           !is.na(indicator_clean)) %>%
    # Take one result per method-indicator combination (avoid duplicates)
    group_by(indicator_clean, method_clean) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  # Create color palette
  method_colors <- c(
    "RDS-I" = "#E31A1C",
    "RDS-II" = "#1F78B4", 
    "RDS-SS" = "#33A02C",
    "MA.estimates" = "#FF7F00"
  )
  
  # Main comparison plot
  p1 <- ggplot(plot_data, aes(x = reorder(indicator_clean, estimate_pct), 
                              y = estimate_pct, 
                              color = method_clean)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                  width = 0.2, alpha = 0.7) +
    scale_color_manual(values = method_colors) +
    theme_rds_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Method Comparison: RDS vs Bayesian Estimators",
      subtitle = "Baseline population (980K) with 95% confidence intervals",
      x = "Indicator",
      y = "Prevalence Estimate (%)",
      color = "Method"
    )
  
  return(p1)
}

# Population sensitivity plot
create_population_sensitivity_plot <- function(unified_df) {
  
  cat("Creating population sensitivity visualization...\n")
  
  # Filter to data with population variation
  plot_data <- unified_df %>%
    filter(has_estimate,
           !is.na(pop_label),
           !is.na(population_size)) %>%
    group_by(indicator_clean, method_clean, population_size) %>%
    slice_head(n = 1) %>%  # Remove duplicates
    ungroup()
  
  if (nrow(plot_data) == 0) {
    cat("No population sensitivity data available\n")
    return(NULL)
  }
  
  # Population sensitivity plot
  p2 <- ggplot(plot_data, aes(x = population_size, y = estimate_pct, 
                              color = method_clean, 
                              linetype = indicator_clean)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2) +
    scale_x_log10(labels = scales::comma_format()) +
    scale_color_manual(values = method_colors) +
    theme_rds_publication() +
    theme(legend.position = "bottom") +
    labs(
      title = "Population Size Sensitivity Analysis", 
      subtitle = "How prevalence estimates vary with assumed population size",
      x = "Population Size (log scale)",
      y = "Prevalence Estimate (%)",
      color = "Method",
      linetype = "Indicator"
    )
  
  return(p2)
}

# Parameter comparison plot (for Bayesian methods)
create_parameter_comparison_plot <- function(unified_df) {
  
  cat("Creating parameter comparison visualization...\n")
  
  # Filter to Bayesian results with parameter information
  plot_data <- unified_df %>%
    filter(has_estimate,
           method_clean == "MA.estimates",
           !is.na(param_group)) %>%
    group_by(indicator_clean, param_group) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  if (nrow(plot_data) == 0) {
    cat("No parameter comparison data available\n")
    return(NULL)
  }
  
  # Parameter comparison plot
  p3 <- ggplot(plot_data, aes(x = indicator_clean, y = estimate_pct, 
                              fill = param_group)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
                  position = position_dodge(width = 0.9), width = 0.2) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    theme_rds_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "MA.estimates Parameter Comparison",
      subtitle = "Enhanced vs Standard parameters across indicators",
      x = "Indicator", 
      y = "Prevalence Estimate (%)",
      fill = "Parameter Type"
    )
  
  return(p3)
}

# ============================================================================
# PUBLICATION TABLES
# ============================================================================

# Main comparison table
create_main_comparison_table <- function(unified_df) {
  
  cat("Creating main comparison table...\n")
  
  # Main comparison (980K baseline)
  main_table <- unified_df %>%
    filter(has_estimate,
           pop_label %in% c("980K", NA)) %>%
    group_by(indicator_clean, method_clean) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, method_clean, estimate_with_ci, method_type, 
           source, param_group) %>%
    pivot_wider(names_from = method_clean, values_from = estimate_with_ci) %>%
    arrange(indicator_clean)
  
  return(main_table)
}

# Population sensitivity table
create_population_table <- function(unified_df) {
  
  cat("Creating population sensitivity table...\n")
  
  pop_table <- unified_df %>%
    filter(has_estimate, !is.na(pop_label)) %>%
    group_by(indicator_clean, method_clean, pop_label) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, method_clean, pop_label, estimate_with_ci) %>%
    pivot_wider(names_from = pop_label, values_from = estimate_with_ci) %>%
    arrange(indicator_clean, method_clean)
  
  return(pop_table)
}

# Parameter sensitivity table (Bayesian only)
create_parameter_table <- function(unified_df) {
  
  cat("Creating parameter sensitivity table...\n")
  
  param_table <- unified_df %>%
    filter(has_estimate, 
           method_clean == "MA.estimates",
           !is.na(param_group)) %>%
    group_by(indicator_clean, param_group) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, param_group, estimate_with_ci, 
           iterations, M1, M2) %>%
    mutate(param_description = paste0(param_group, " (", iterations, " iter, M1=", M1, ", M2=", M2, ")")) %>%
    select(indicator_clean, param_description, estimate_with_ci) %>%
    pivot_wider(names_from = param_description, values_from = estimate_with_ci) %>%
    arrange(indicator_clean)
  
  return(param_table)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Load all production results
production_results <- load_production_results()

# Create unified dataset
unified_data <- create_unified_dataset(production_results)

# Create visualizations
cat("=== Creating Visualizations ===\n")

method_plot <- create_method_comparison_plot(unified_data)
population_plot <- create_population_sensitivity_plot(unified_data)
parameter_plot <- create_parameter_comparison_plot(unified_data)

# Create publication tables
cat("=== Creating Publication Tables ===\n")

main_comparison_table <- create_main_comparison_table(unified_data)
population_sensitivity_table <- create_population_table(unified_data)
parameter_sensitivity_table <- create_parameter_table(unified_data)

# Save visualizations
cat("=== Saving Outputs ===\n")

if (!is.null(method_plot)) {
  ggsave(here("output", "figures", "method_comparison.png"), 
         method_plot, width = 12, height = 8, dpi = 300)
  cat("Saved: method_comparison.png\n")
}

if (!is.null(population_plot)) {
  ggsave(here("output", "figures", "population_sensitivity.png"), 
         population_plot, width = 12, height = 8, dpi = 300)
  cat("Saved: population_sensitivity.png\n")
}

if (!is.null(parameter_plot)) {
  ggsave(here("output", "figures", "parameter_comparison.png"), 
         parameter_plot, width = 10, height = 6, dpi = 300)
  cat("Saved: parameter_comparison.png\n")
}

# Save tables
write.csv(main_comparison_table, here("output", "tables", "main_comparison.csv"), row.names = FALSE)
write.csv(population_sensitivity_table, here("output", "tables", "population_sensitivity_full.csv"), row.names = FALSE)
write.csv(parameter_sensitivity_table, here("output", "tables", "parameter_sensitivity_comparison.csv"), row.names = FALSE)
write.csv(unified_data, here("output", "tables", "unified_results.csv"), row.names = FALSE)

# Save complete results
save(production_results, unified_data, main_comparison_table, 
     population_sensitivity_table, parameter_sensitivity_table,
     method_plot, population_plot, parameter_plot,
     file = here("output", "comprehensive_comparison_results.RData"))

# Summary report
cat("\n=== COMPREHENSIVE COMPARISON COMPLETE ===\n")
cat("Results processed from:\n")
for(source in names(production_results)) {
  if(!is.null(production_results[[source]])) {
    cat("- ", source, ":", nrow(production_results[[source]]), "results\n")
  }
}

cat("\nUnified dataset:\n")
cat("- Total results:", nrow(unified_data), "\n")
cat("- Successful estimates:", sum(unified_data$has_estimate, na.rm = TRUE), "\n")
cat("- Methods:", paste(unique(unified_data$method_clean), collapse = ", "), "\n")
cat("- Indicators:", length(unique(unified_data$indicator_clean)), "\n")

cat("\nOutput files created:\n")
cat("- Visualizations: output/figures/\n")
cat("  * method_comparison.png\n")
cat("  * population_sensitivity.png\n")  
cat("  * parameter_comparison.png\n")
cat("- Tables: output/tables/\n")
cat("  * main_comparison.csv\n")
cat("  * population_sensitivity_full.csv\n")
cat("  * parameter_sensitivity_comparison.csv\n")
cat("  * unified_results.csv\n")
cat("- Complete results: comprehensive_comparison_results.RData\n\n")

cat("=== READY FOR PUBLICATION! ===\n")
cat("All method comparisons, sensitivity analyses, and publication tables complete.\n")
cat("Use the visualization plots and CSV tables for your manuscript.\n")


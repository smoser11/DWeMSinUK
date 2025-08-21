# 06-appendix_sensitivity_analysis.R
# Comprehensive RDS Model Comparison for Appendix
# Domestic Worker Exploitation and Modern Slavery in UK
#
# Systematic comparison of:
# - Models: RDS-I, RDS-II, RDS-SS, Model-Assisted Bayesian
# - Population sizes: 50K, 100K, 980K, 1.74M
# - Indicators: All comparable RDS variables + composite risk
# - Weighting schemes: Various options for weighted estimators

cat("=== Comprehensive RDS Model Comparison (Appendix) ===\n")
cat("Systematic sensitivity analysis across models, parameters, and indicators\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(parallel)
library(kableExtra)
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

appendix_config <- list(
  # Models to compare
  models = c("RDS_I", "RDS_II", "RDS_SS", "Model_Assisted"),
  
  # Population sizes for sensitivity analysis
  population_sizes = c(50000, 100000, 980000, 1740000),
  population_labels = c("50K", "100K", "980K", "1.74M"),
  
  # All indicators to test
  indicators = c(get_comparable_indicators()$rds_vars, 
                 "composite_risk", "whether_exploitation"),
  
  # Weighting schemes to test (for applicable methods)
  weighting_schemes = c("inverse_degree", "visibility", "posterior"),
  
  # Computational parameters
  n_bootstrap = 1000,  # Reduced for speed in sensitivity analysis
  confidence_level = 0.95,
  parallel_cores = 4,
  
  # Output control
  save_detailed_results = TRUE,
  create_comparison_tables = TRUE,
  save_plots = TRUE
)

cat("Appendix analysis configuration:\n")
cat("- Models:", length(appendix_config$models), "RDS methods\n")
cat("- Population sizes:", length(appendix_config$population_sizes), "scenarios\n")
cat("- Indicators:", length(appendix_config$indicators), "variables\n")
cat("- Total combinations:", length(appendix_config$models) * 
    length(appendix_config$population_sizes) * 
    length(appendix_config$indicators), "\n\n")

# ============================================================================
# RDS MODEL IMPLEMENTATIONS
# ============================================================================

# RDS-I Estimator
estimate_rds_i <- function(outcome_var, population_size = NULL) {
  tryCatch({
    result <- RDS.I.estimates(rd.dd, outcome.variable = outcome_var)
    list(
      method = "RDS_I",
      estimate = result$estimate,
      se = result$se,
      population_size = NA,
      weighting_scheme = NA
    )
  }, error = function(e) {
    list(method = "RDS_I", estimate = NA, se = NA, error = e$message)
  })
}

# RDS-II Estimator  
estimate_rds_ii <- function(outcome_var, population_size = NULL) {
  tryCatch({
    result <- RDS.II.estimates(rd.dd, outcome.variable = outcome_var)
    list(
      method = "RDS_II",
      estimate = result$estimate,
      se = result$se,
      population_size = NA,
      weighting_scheme = NA
    )
  }, error = function(e) {
    list(method = "RDS_II", estimate = NA, se = NA, error = e$message)
  })
}

# RDS-SS Estimator (requires population size)
estimate_rds_ss <- function(outcome_var, population_size) {
  tryCatch({
    result <- RDS.SS.estimates(rd.dd, outcome.variable = outcome_var, N = population_size)
    list(
      method = "RDS_SS",
      estimate = result$estimate,
      se = result$se,
      population_size = population_size,
      weighting_scheme = NA
    )
  }, error = function(e) {
    list(method = "RDS_SS", estimate = NA, se = NA, error = e$message, 
         population_size = population_size)
  })
}

# Model-Assisted Bayesian Estimator
estimate_model_assisted <- function(outcome_var, population_size, weighting_scheme = "inverse_degree") {
  tryCatch({
    # This is a placeholder - actual implementation would depend on available packages
    # For now, we'll use a modified RDS-II with different weighting
    
    # Create custom weights based on scheme
    weights <- switch(weighting_scheme,
      "inverse_degree" = 1 / rd.dd$network.size,
      "visibility" = rd.dd$network.size / mean(rd.dd$network.size, na.rm = TRUE),
      "posterior" = rep(1, nrow(rd.dd))  # Uniform for placeholder
    )
    
    # Weighted estimate (simplified approach)
    outcome_values <- rd.dd[[outcome_var]]
    weighted_mean <- weighted.mean(outcome_values, weights, na.rm = TRUE)
    weighted_se <- sqrt(wtd.var(outcome_values, weights, na.rm = TRUE) / sum(!is.na(outcome_values)))
    
    list(
      method = "Model_Assisted",
      estimate = weighted_mean,
      se = weighted_se,
      population_size = population_size,
      weighting_scheme = weighting_scheme
    )
  }, error = function(e) {
    list(method = "Model_Assisted", estimate = NA, se = NA, error = e$message,
         population_size = population_size, weighting_scheme = weighting_scheme)
  })
}

# Helper function for weighted variance
wtd.var <- function(x, weights, na.rm = FALSE) {
  if (na.rm) {
    valid <- !is.na(x) & !is.na(weights)
    x <- x[valid]
    weights <- weights[valid]
  }
  weighted_mean <- weighted.mean(x, weights)
  sum(weights * (x - weighted_mean)^2) / sum(weights)
}

# ============================================================================
# COMPREHENSIVE MODEL COMPARISON
# ============================================================================

run_comprehensive_comparison <- function() {
  
  cat("=== Running Comprehensive Model Comparison ===\n")
  
  # Initialize results storage
  all_results <- list()
  result_count <- 0
  
  # Progress tracking
  total_combinations <- length(appendix_config$indicators) * 
                       length(appendix_config$population_sizes) * 
                       length(appendix_config$models)
  
  for (indicator in appendix_config$indicators) {
    
    cat("Processing indicator:", indicator, "\n")
    
    if (!(indicator %in% names(rd.dd))) {
      cat("  Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    for (pop_size in appendix_config$population_sizes) {
      
      pop_label <- appendix_config$population_labels[which(appendix_config$population_sizes == pop_size)]
      cat("  Population size:", format(pop_size, big.mark = ","), "(", pop_label, ")\n")
      
      # RDS-I (doesn't need population size)
      if ("RDS_I" %in% appendix_config$models) {
        result_count <- result_count + 1
        result <- estimate_rds_i(indicator)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
      
      # RDS-II (doesn't need population size)  
      if ("RDS_II" %in% appendix_config$models) {
        result_count <- result_count + 1
        result <- estimate_rds_ii(indicator)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
      
      # RDS-SS (needs population size)
      if ("RDS_SS" %in% appendix_config$models) {
        result_count <- result_count + 1
        result <- estimate_rds_ss(indicator, pop_size)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
      
      # Model-Assisted with different weighting schemes
      if ("Model_Assisted" %in% appendix_config$models) {
        for (weight_scheme in appendix_config$weighting_schemes) {
          result_count <- result_count + 1
          result <- estimate_model_assisted(indicator, pop_size, weight_scheme)
          result$indicator <- indicator
          result$pop_size <- pop_size
          result$pop_label <- pop_label
          all_results[[result_count]] <- result
        }
      }
    }
  }
  
  cat("Completed", result_count, "model estimations\n\n")
  return(all_results)
}

# ============================================================================
# RESULTS PROCESSING AND TABLES
# ============================================================================

process_comparison_results <- function(all_results) {
  
  cat("=== Processing Comparison Results ===\n")
  
  # Convert results to data frame
  results_df <- map_dfr(all_results, function(result) {
    data.frame(
      indicator = result$indicator %||% NA,
      pop_size = result$pop_size %||% NA,
      pop_label = result$pop_label %||% NA,
      method = result$method %||% NA,
      weighting_scheme = result$weighting_scheme %||% NA,
      estimate = result$estimate %||% NA,
      se = result$se %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  # Add indicator labels and clean method names
  results_df <- results_df %>%
    mutate(
      indicator_clean = case_when(
        str_detect(indicator, "_rds$") ~ get_comparable_indicators()$labels[str_remove(indicator, "_rds$")],
        indicator == "composite_risk" ~ "Composite risk score",
        indicator == "whether_exploitation" ~ "Overall exploitation indicator",
        TRUE ~ indicator
      ),
      method_clean = case_when(
        method == "RDS_I" ~ "RDS-I",
        method == "RDS_II" ~ "RDS-II", 
        method == "RDS_SS" ~ "RDS-SS",
        method == "Model_Assisted" ~ paste0("Model-Assisted (", weighting_scheme, ")"),
        TRUE ~ method
      ),
      estimate_pct = estimate * 100,
      se_pct = se * 100
    )
  
  # Calculate confidence intervals
  results_df <- results_df %>%
    mutate(
      ci_lower = pmax(0, estimate - 1.96 * se),
      ci_upper = pmin(1, estimate + 1.96 * se),
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      estimate_with_ci = paste0(
        sprintf("%.1f", estimate_pct), "% (",
        sprintf("%.1f", ci_lower_pct), "–",
        sprintf("%.1f", ci_upper_pct), ")"
      )
    )
  
  cat("Processed", nrow(results_df), "results\n\n")
  return(results_df)
}

# Create comparison tables by indicator
create_indicator_comparison_tables <- function(results_df) {
  
  cat("=== Creating Indicator Comparison Tables ===\n")
  
  comparison_tables <- list()
  
  for (indicator in unique(results_df$indicator)) {
    
    indicator_data <- results_df %>%
      filter(indicator == !!indicator) %>%
      select(pop_label, method_clean, estimate_pct, ci_lower_pct, ci_upper_pct, estimate_with_ci) %>%
      arrange(pop_label, method_clean)
    
    # Create wide table format
    wide_table <- indicator_data %>%
      pivot_wider(
        names_from = pop_label,
        values_from = estimate_with_ci,
        id_cols = method_clean
      ) %>%
      arrange(method_clean)
    
    comparison_tables[[indicator]] <- wide_table
    
    # Save individual table
    indicator_clean <- unique(results_df$indicator_clean[results_df$indicator == indicator])
    filename <- paste0("appendix_comparison_", str_replace_all(tolower(indicator), "[^a-z0-9]", "_"), ".csv")
    
    write.csv(wide_table, 
              here("output", "tables", filename),
              row.names = FALSE)
    
    cat("Created comparison table for:", indicator_clean, "\n")
  }
  
  return(comparison_tables)
}

# Create master comparison table
create_master_comparison_table <- function(results_df) {
  
  cat("=== Creating Master Comparison Table ===\n")
  
  # Focus on key results for master table
  master_table <- results_df %>%
    filter(pop_size == 980000) %>%  # Use main population size
    select(indicator_clean, method_clean, estimate_pct, ci_lower_pct, ci_upper_pct, estimate_with_ci) %>%
    arrange(desc(estimate_pct), method_clean)
  
  # Save master table
  write.csv(master_table, 
            here("output", "tables", "appendix_master_comparison.csv"),
            row.names = FALSE)
  
  cat("Master comparison table created with", nrow(master_table), "results\n\n")
  return(master_table)
}

# ============================================================================
# SENSITIVITY ANALYSIS SUMMARY
# ============================================================================

create_sensitivity_summary <- function(results_df) {
  
  cat("=== Creating Sensitivity Analysis Summary ===\n")
  
  # Calculate coefficient of variation for each indicator across methods
  sensitivity_summary <- results_df %>%
    filter(!is.na(estimate)) %>%
    group_by(indicator, indicator_clean, pop_label) %>%
    summarise(
      n_methods = n(),
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      cv_estimate = sd_estimate / mean_estimate,
      min_estimate = min(estimate, na.rm = TRUE),
      max_estimate = max(estimate, na.rm = TRUE),
      range_estimate = max_estimate - min_estimate,
      .groups = "drop"
    ) %>%
    mutate(
      mean_estimate_pct = mean_estimate * 100,
      range_estimate_pct = range_estimate * 100,
      cv_percent = cv_estimate * 100
    )
  
  # Save sensitivity summary
  write.csv(sensitivity_summary,
            here("output", "tables", "appendix_sensitivity_summary.csv"),
            row.names = FALSE)
  
  cat("Sensitivity summary created for", nrow(sensitivity_summary), "indicator-population combinations\n\n")
  return(sensitivity_summary)
}

# ============================================================================
# MAIN ANALYSIS FUNCTION
# ============================================================================

main_appendix_analysis <- function() {
  
  setup_project_environment()
  
  cat("Starting comprehensive appendix analysis...\n\n")
  
  # Step 1: Run comprehensive model comparison
  all_results <- run_comprehensive_comparison()
  
  # Step 2: Process results into structured format
  results_df <- process_comparison_results(all_results)
  
  # Step 3: Create comparison tables by indicator
  comparison_tables <- create_indicator_comparison_tables(results_df)
  
  # Step 4: Create master comparison table
  master_table <- create_master_comparison_table(results_df)
  
  # Step 5: Create sensitivity analysis summary
  sensitivity_summary <- create_sensitivity_summary(results_df)
  
  # Step 6: Compile final results
  final_results <- list(
    all_results = all_results,
    results_df = results_df,
    comparison_tables = comparison_tables,
    master_table = master_table,
    sensitivity_summary = sensitivity_summary,
    
    config = appendix_config,
    metadata = list(
      timestamp = Sys.time(),
      total_estimations = length(all_results),
      sample_size = nrow(rd.dd)
    )
  )
  
  # Save results
  save(final_results, file = here("output", "appendix_sensitivity_results.RData"))
  
  cat("=== Appendix Analysis Complete ===\n")
  cat("Total estimations:", length(all_results), "\n")
  cat("Indicators analyzed:", length(unique(results_df$indicator)), "\n")
  cat("Methods compared:", length(unique(results_df$method_clean)), "\n")
  cat("Population scenarios:", length(unique(results_df$pop_label)), "\n")
  cat("\nResults saved to: output/appendix_sensitivity_results.RData\n")
  cat("Tables saved to: output/tables/appendix_*.csv\n\n")
  
  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

skip_execution = FALSE
# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  # Check if this is being run from the pipeline
  if (exists("pipeline_config")) {
    cat("Running as part of main pipeline\n")
  } else {
    cat("Running standalone appendix analysis\n")
  }
  
  # Run analysis
  appendix_results <- main_appendix_analysis()
  
} else {
  cat("Appendix sensitivity analysis script loaded (execution skipped)\n")
}

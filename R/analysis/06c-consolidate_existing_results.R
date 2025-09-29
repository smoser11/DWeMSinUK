# 06c-consolidate_existing_results.R
# Consolidate existing MA.estimates sensitivity results into bayesian_sensitivity_results.RData
# WITHOUT running new computations - just loads existing individual files

cat("=== CONSOLIDATING EXISTING MA.ESTIMATES SENSITIVITY RESULTS ===\n")

# Load required libraries
library(tidyverse)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# ============================================================================
# CONFIGURATION
# ============================================================================

# Match the configuration from 06c
sensitivity_config <- list(
  population_sizes = c(50000, 100000, 980000, 1740000),
  population_labels = c("50K", "100K", "980K", "1.74M"),
  seed_selection_methods = c("random", "degree", "sample"),
  binary_indicators = get_comparable_indicators()$rds_vars,
  numeric_indicators = c("composite_risk", "sum_categories"),
  additional_indicators = c("whether_exploitation"),
  save_detailed_results = TRUE
)

# All indicators to process
all_indicators <- c(
  sensitivity_config$binary_indicators,
  sensitivity_config$numeric_indicators,
  sensitivity_config$additional_indicators
)

cat("Configuration:\n")
cat("- Population sizes:", length(sensitivity_config$population_sizes), "\n")
cat("- Seed methods:", length(sensitivity_config$seed_selection_methods), "\n")
cat("- Total indicators:", length(all_indicators), "\n")
cat("- Total combinations:", length(all_indicators) * length(sensitivity_config$population_sizes) *
    length(sensitivity_config$seed_selection_methods), "\n\n")

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

get_sensitivity_parameters <- function(indicator, seed_selection) {
  # Determine if enhanced or standard parameters based on indicator type
  if (indicator %in% sensitivity_config$numeric_indicators) {
    return(list(
      parameter_type = "enhanced",
      number.of.iterations = 3,
      M1 = 75,
      M2 = 50,
      seed.selection = seed_selection
    ))
  } else {
    return(list(
      parameter_type = "standard",
      number.of.iterations = 2,
      M1 = 50,
      M2 = 25,
      seed.selection = seed_selection
    ))
  }
}

load_existing_result <- function(indicator, population_size, seed_selection) {
  combination_id <- paste(indicator, format(population_size, scientific = FALSE), seed_selection, sep = "_")
  result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))

  if (!file.exists(result_file)) {
    return(NULL)  # Skip if file doesn't exist
  }

  tryCatch({
    load(result_file)

    # Extract values from loaded ma_result
    if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
      point_estimate <- ma_result$estimate$interval[2]  # Point estimate
      ci_lower <- ma_result$estimate$interval[4]        # Lower 95% CI
      ci_upper <- ma_result$estimate$interval[6]        # Upper 95% CI
      bayesian_se <- if(length(ma_result$estimate$interval) >= 3) ma_result$estimate$interval[3] else NA
    } else {
      point_estimate <- ci_lower <- ci_upper <- bayesian_se <- NA
    }

    # Get parameter information
    params <- get_sensitivity_parameters(indicator, seed_selection)

    return(list(
      method = "MA_estimates",
      indicator = indicator,
      population_size = population_size,
      pop_label = sensitivity_config$population_labels[which(sensitivity_config$population_sizes == population_size)],
      seed_selection = seed_selection,
      estimate = point_estimate,
      se = bayesian_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      uncertainty_method = "bayesian_credible_interval",
      method_type = "bayesian",
      parameter_type = params$parameter_type,
      iterations = params$number.of.iterations,
      M1 = params$M1,
      M2 = params$M2,
      convergence_status = if(!is.na(point_estimate)) "success" else "failed",
      combination_id = combination_id,
      run_timestamp = Sys.time()
    ))
  }, error = function(e) {
    cat("Error loading", result_file, ":", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# CONSOLIDATE EXISTING RESULTS
# ============================================================================

cat("Loading existing sensitivity results...\n")

all_results <- list()
result_count <- 0
successful_count <- 0
failed_count <- 0

for (indicator in all_indicators) {
  cat("Processing indicator:", indicator, "\n")

  for (pop_size in sensitivity_config$population_sizes) {
    for (seed_method in sensitivity_config$seed_selection_methods) {

      result <- load_existing_result(indicator, pop_size, seed_method)

      if (!is.null(result)) {
        result_count <- result_count + 1
        all_results[[result_count]] <- result

        if (!is.na(result$estimate)) {
          successful_count <- successful_count + 1
          cat("  ✓", result$pop_label, result$seed_selection, ":",
              sprintf("%.1f%% (%.1f–%.1f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
        } else {
          failed_count <- failed_count + 1
          cat("  ✗", result$pop_label, result$seed_selection, ": FAILED\n")
        }
      }
    }
  }
  cat("\n")
}

cat("Consolidation summary:\n")
cat("- Results loaded:", result_count, "\n")
cat("- Successful:", successful_count, "\n")
cat("- Failed:", failed_count, "\n")
cat("- Success rate:", round(successful_count/result_count*100, 1), "%\n\n")

# ============================================================================
# PROCESS INTO DATAFRAME
# ============================================================================

cat("Processing results into dataframe...\n")

# Helper function for safe extraction
safe_extract <- function(result, field, default = NA) {
  if (is.null(result) || !is.list(result)) return(default)
  value <- result[[field]]
  if (is.null(value) || length(value) == 0) return(default)
  return(value)
}

sensitivity_df <- map_dfr(all_results, function(result) {
  if (is.null(result) || !is.list(result)) {
    return(data.frame())
  }

  data.frame(
    indicator = safe_extract(result, "indicator", "unknown"),
    method = safe_extract(result, "method", NA),
    method_type = safe_extract(result, "method_type", NA),
    population_size = safe_extract(result, "population_size", NA),
    pop_label = safe_extract(result, "pop_label", NA),
    seed_selection = safe_extract(result, "seed_selection", NA),
    estimate = safe_extract(result, "estimate", NA),
    se = safe_extract(result, "se", NA),
    ci_lower = safe_extract(result, "ci_lower", NA),
    ci_upper = safe_extract(result, "ci_upper", NA),
    uncertainty_method = safe_extract(result, "uncertainty_method", NA),
    parameter_type = safe_extract(result, "parameter_type", NA),
    iterations = safe_extract(result, "iterations", NA),
    M1 = safe_extract(result, "M1", NA),
    M2 = safe_extract(result, "M2", NA),
    convergence_status = safe_extract(result, "convergence_status", NA),
    combination_id = safe_extract(result, "combination_id", NA),
    stringsAsFactors = FALSE
  )
})

# Add indicator labels and formatting
indicators_info <- get_comparable_indicators()
sensitivity_df <- sensitivity_df %>%
  mutate(
    indicator_clean = case_when(
      str_detect(indicator, "_rds$") ~ indicators_info$labels[str_remove(indicator, "_rds$")],
      indicator == "composite_risk" ~ "Composite risk score",
      indicator == "whether_exploitation" ~ "Overall exploitation indicator",
      indicator == "sum_categories" ~ "Risk exposure scale (ordinal)",
      TRUE ~ indicator
    ),
    method_clean = "MA.estimates",
    uncertainty_clean = "Bayesian CI",
    estimate_pct = estimate * 100,
    ci_lower_pct = ci_lower * 100,
    ci_upper_pct = ci_upper * 100,
    se_pct = se * 100,
    estimate_with_ci = ifelse(
      !is.na(ci_lower) & !is.na(ci_upper),
      sprintf("%.1f%% (%.1f–%.1f)", estimate_pct, ci_lower_pct, ci_upper_pct),
      sprintf("%.1f%% (CI not available)", estimate_pct)
    ),
    sensitivity_combo = paste0(pop_label, "_", seed_selection, "_", parameter_type)
  ) %>%
  arrange(indicator_clean, population_size, seed_selection, parameter_type)

# ============================================================================
# CREATE SUMMARY TABLES
# ============================================================================

# Create population sensitivity table
pop_sensitivity_table <- sensitivity_df %>%
  filter(!is.na(estimate), seed_selection == "random") %>%  # Use random seed as baseline
  select(indicator_clean, pop_label, parameter_type, estimate_with_ci) %>%
  pivot_wider(names_from = pop_label, values_from = estimate_with_ci) %>%
  arrange(indicator_clean, parameter_type)

# Create seed sensitivity table
seed_sensitivity_table <- sensitivity_df %>%
  filter(!is.na(estimate), pop_label == "980K") %>%  # Use 980K as baseline
  select(indicator_clean, seed_selection, parameter_type, estimate_with_ci) %>%
  pivot_wider(names_from = seed_selection, values_from = estimate_with_ci) %>%
  arrange(indicator_clean, parameter_type)

# ============================================================================
# SAVE CONSOLIDATED RESULTS
# ============================================================================

cat("Saving consolidated results...\n")

# Save the main consolidation file that 07-comprehensive_comparison.R expects
save(all_results, sensitivity_df, sensitivity_config, pop_sensitivity_table, seed_sensitivity_table,
     file = here("output", "bayesian_sensitivity_results.RData"))

# Also save CSV tables
write.csv(sensitivity_df, here("output", "tables", "bayesian_sensitivity_full.csv"), row.names = FALSE)
write.csv(pop_sensitivity_table, here("output", "tables", "population_sensitivity.csv"), row.names = FALSE)
write.csv(seed_sensitivity_table, here("output", "tables", "seed_sensitivity.csv"), row.names = FALSE)

cat("Results saved to:\n")
cat("- bayesian_sensitivity_results.RData (", nrow(sensitivity_df), " results)\n")
cat("- tables/bayesian_sensitivity_full.csv\n")
cat("- tables/population_sensitivity.csv\n")
cat("- tables/seed_sensitivity.csv\n\n")

# ============================================================================
# SUMMARY REPORT
# ============================================================================

cat("=== CONSOLIDATION COMPLETE ===\n")
cat("Total combinations:", nrow(sensitivity_df), "\n")
cat("Successful estimations:", sum(!is.na(sensitivity_df$estimate)), "\n")

# Parameter type performance
param_performance <- sensitivity_df %>%
  filter(!is.na(estimate)) %>%
  count(parameter_type, name = "successful_runs") %>%
  arrange(desc(successful_runs))

cat("\nParameter type performance:\n")
for(i in 1:nrow(param_performance)) {
  row <- param_performance[i,]
  cat("-", row$parameter_type, "parameters:", row$successful_runs, "successful runs\n")
}

cat("\nReady for comprehensive comparison in 07-comprehensive_comparison.R!\n")
cat("The consolidated file contains sensitivity_df with all required columns.\n")
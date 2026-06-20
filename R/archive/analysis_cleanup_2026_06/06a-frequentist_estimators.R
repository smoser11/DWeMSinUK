# 06a-frequentist_estimators.R
# Frequentist RDS Estimators (RDS-I, RDS-II, RDS-SS) with Bootstrap CIs
# PRODUCTION VERSION: Clean, comprehensive frequentist analysis
#
# INCLUDES:
# - RDS-I, RDS-II, RDS-SS estimation
# - Neighb() bootstrap confidence intervals  
# - All comparable indicators (binary only - frequentist methods work best with binary)
# - Multiple population size scenarios
# - Publication-ready outputs

cat("=== FREQUENTIST RDS ESTIMATORS (Production) ===\n")
cat("Methods: RDS-I, RDS-II, RDS-SS with bootstrap confidence intervals\n")
cat("Indicators: All comparable binary indicators\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(Neighboot)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data (with weights)
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data with weights\n")
}

# ============================================================================
# FREQUENTIST CONFIGURATION
# ============================================================================

frequentist_config <- list(
  # Frequentist methods only
  methods = c("RDS_I", "RDS_II", "RDS_SS"),
  
  # Population sizes for sensitivity analysis
  population_sizes = c(50000, 100000, 980000, 1740000),
  population_labels = c("50K", "100K", "980K", "1.74M"),
  
  # Binary indicators only (frequentist methods work best with binary)
  indicators = get_comparable_indicators()$rds_vars,  # Binary indicators only
  
  # Bootstrap parameters
  n_bootstrap = 5000,           # Bootstrap samples for CIs
  confidence_level = 0.95,
  quantiles = c(0.025, 0.975),
  
  # Computational parameters
  parallel_cores = 1,           # Keep single-core for stability
  
  # Output control
  save_detailed_results = TRUE,
  create_comparison_tables = TRUE,
  verbose = TRUE
)

cat("Frequentist configuration:\n")
cat("- Methods:", paste(frequentist_config$methods, collapse = ", "), "\n")
cat("- Binary indicators:", length(frequentist_config$indicators), "\n")
cat("- Population scenarios:", length(frequentist_config$population_sizes), "\n")
cat("- Bootstrap samples:", frequentist_config$n_bootstrap, "\n\n")

# ============================================================================
# NEIGHBOOT DATA CONVERSION (Working version)
# ============================================================================

convert_rds_to_neighboot_format <- function() {
  
  # Create proper edges data frame (exclude seeds with recruiter.id == -1) 
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  # Renumber nodes sequentially to avoid "Duplicate vertex names"
  node_map <- setNames(1:length(rd.dd$id), rd.dd$id)
  
  # Use only binary comparable indicators (frequentist works best with binary)
  binary_traits <- data.frame(
    document_withholding_rds = rd.dd$document_withholding_rds,
    pay_issues_rds = rd.dd$pay_issues_rds, 
    threats_abuse_rds = rd.dd$threats_abuse_rds,
    excessive_hours_rds = rd.dd$excessive_hours_rds,
    access_to_help_rds = rd.dd$access_to_help_rds
  )
  
  # Handle missing values (replace with 0 for binary indicators)
  binary_traits <- binary_traits %>%
    mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))
  
  # Create corrected RDS data structure (WORKING FORMAT)
  rds_data <- list(
    nodes = 1:length(node_map),
    edges = data.frame(
      from = node_map[as.character(edges_df$from)],
      to = node_map[as.character(edges_df$to)]
    ),
    traits = binary_traits,
    degree = setNames(rd.dd$network.size, 1:length(rd.dd$network.size))
  )
  
  return(rds_data)
}

# ============================================================================
# FREQUENTIST ESTIMATION FUNCTIONS
# ============================================================================

# RDS-I with neighb() bootstrap
estimate_rds_i <- function(outcome_var, n_bootstrap = 5000) {
  tryCatch({
    cat("  Processing RDS-I for:", outcome_var, "\n")
    
    # Point estimate
    result <- RDS.I.estimates(rd.dd, outcome.variable = outcome_var)
    point_estimate <- result$estimate
    
    # Bootstrap confidence intervals using neighb()
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      neighb(
        RDS.data = rds_sample,
        quant = frequentist_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("    ERROR in neighb() for RDS-I:", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(frequentist_config$quantiles[1])]
      ci_upper <- var_results[as.character(frequentist_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("    SUCCESS: Bootstrap CI computed\n")
    } else {
      # Fallback to asymptotic CI
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("    FALLBACK: Using asymptotic CI\n")
    }
    
    return(list(
      method = "RDS_I",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    ))
    
  }, error = function(e) {
    cat("    ERROR in RDS-I:", e$message, "\n")
    return(list(method = "RDS_I", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
                error = e$message, method_type = "frequentist"))
  })
}

# RDS-II with neighb() bootstrap
estimate_rds_ii <- function(outcome_var, n_bootstrap = 5000) {
  tryCatch({
    cat("  Processing RDS-II for:", outcome_var, "\n")
    
    # Point estimate
    result <- RDS.II.estimates(rd.dd, outcome.variable = outcome_var)
    point_estimate <- result$estimate
    
    # Bootstrap confidence intervals using neighb()
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      neighb(
        RDS.data = rds_sample,
        quant = frequentist_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("    ERROR in neighb() for RDS-II:", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(frequentist_config$quantiles[1])]
      ci_upper <- var_results[as.character(frequentist_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("    SUCCESS: Bootstrap CI computed\n")
    } else {
      # Fallback to asymptotic CI
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("    FALLBACK: Using asymptotic CI\n")
    }
    
    return(list(
      method = "RDS_II",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    ))
    
  }, error = function(e) {
    cat("    ERROR in RDS-II:", e$message, "\n")
    return(list(method = "RDS_II", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
                error = e$message, method_type = "frequentist"))
  })
}

# RDS-SS with neighb() bootstrap
estimate_rds_ss <- function(outcome_var, population_size, n_bootstrap = 5000) {
  tryCatch({
    cat("  Processing RDS-SS for:", outcome_var, "N =", format(population_size, big.mark = ","), "\n")
    
    # Point estimate
    result <- RDS.SS.estimates(rd.dd, outcome.variable = outcome_var, N = population_size)
    point_estimate <- result$estimate
    
    # Bootstrap confidence intervals using neighb()
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      neighb(
        RDS.data = rds_sample,
        quant = frequentist_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("    ERROR in neighb() for RDS-SS:", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(frequentist_config$quantiles[1])]
      ci_upper <- var_results[as.character(frequentist_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("    SUCCESS: Bootstrap CI computed\n")
    } else {
      # Fallback to asymptotic CI
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("    FALLBACK: Using asymptotic CI\n")
    }
    
    return(list(
      method = "RDS_SS",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = population_size,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    ))
    
  }, error = function(e) {
    cat("    ERROR in RDS-SS:", e$message, "\n")
    return(list(method = "RDS_SS", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
                error = e$message, population_size = population_size, method_type = "frequentist"))
  })
}

# ============================================================================
# MAIN FREQUENTIST ANALYSIS
# ============================================================================

run_frequentist_analysis <- function() {
  
  cat("=== Running Frequentist RDS Analysis ===\n")
  
  all_results <- list()
  result_count <- 0
  
  for (indicator in frequentist_config$indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    cat("Processing indicator:", indicator, "\n")
    
    for (pop_size in frequentist_config$population_sizes) {
      pop_label <- frequentist_config$population_labels[which(frequentist_config$population_sizes == pop_size)]
      
      # RDS-I (population-independent)
      if ("RDS_I" %in% frequentist_config$methods) {
        result_count <- result_count + 1
        result <- estimate_rds_i(indicator, frequentist_config$n_bootstrap)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
      
      # RDS-II (population-independent)
      if ("RDS_II" %in% frequentist_config$methods) {
        result_count <- result_count + 1
        result <- estimate_rds_ii(indicator, frequentist_config$n_bootstrap)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
      
      # RDS-SS (population-dependent)
      if ("RDS_SS" %in% frequentist_config$methods) {
        result_count <- result_count + 1
        result <- estimate_rds_ss(indicator, pop_size, frequentist_config$n_bootstrap)
        result$indicator <- indicator
        result$pop_size <- pop_size
        result$pop_label <- pop_label
        all_results[[result_count]] <- result
      }
    }
    cat("\n")
  }
  
  return(all_results)
}

# Process results into data frame
process_frequentist_results <- function(results_list) {
  
  # Helper function to safely extract values
  safe_extract <- function(result, field, default = NA) {
    if (is.null(result) || !is.list(result)) return(default)
    value <- result[[field]]
    if (is.null(value) || length(value) == 0) return(default)
    return(value)
  }
  
  results_df <- map_dfr(results_list, function(result) {
    if (is.null(result) || !is.list(result)) {
      return(data.frame())
    }
    
    data.frame(
      indicator = safe_extract(result, "indicator", "unknown"),
      pop_size = safe_extract(result, "pop_size", NA),
      pop_label = safe_extract(result, "pop_label", NA),
      method = safe_extract(result, "method", NA),
      method_type = safe_extract(result, "method_type", NA),
      estimate = safe_extract(result, "estimate", NA),
      se = safe_extract(result, "se", NA),
      ci_lower = safe_extract(result, "ci_lower", NA),
      ci_upper = safe_extract(result, "ci_upper", NA),
      uncertainty_method = safe_extract(result, "uncertainty_method", NA),
      error_msg = safe_extract(result, "error", NA),
      stringsAsFactors = FALSE
    )
  })
  
  # Add indicator labels and formatting
  indicators_info <- get_comparable_indicators()
  results_df <- results_df %>%
    mutate(
      indicator_clean = indicators_info$labels[str_remove(indicator, "_rds$")],
      method_clean = case_when(
        method == "RDS_I" ~ "RDS-I",
        method == "RDS_II" ~ "RDS-II", 
        method == "RDS_SS" ~ "RDS-SS",
        TRUE ~ method
      ),
      uncertainty_clean = case_when(
        uncertainty_method == "neighb_bootstrap" ~ "Bootstrap",
        uncertainty_method == "asymptotic" ~ "Asymptotic",
        TRUE ~ uncertainty_method
      ),
      estimate_pct = estimate * 100,
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      se_pct = se * 100,
      estimate_with_ci = ifelse(
        !is.na(ci_lower) & !is.na(ci_upper),
        sprintf("%.1f%% (%.1f–%.1f)", estimate_pct, ci_lower_pct, ci_upper_pct),
        sprintf("%.1f%% (CI not available)", estimate_pct)
      )
    ) %>%
    arrange(indicator_clean, method_clean, pop_size)
  
  return(results_df)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Run analysis
cat("Starting frequentist analysis...\n")
frequentist_results <- run_frequentist_analysis()

# Process results
cat("Processing results...\n")
frequentist_df <- process_frequentist_results(frequentist_results)

# Save results
if (frequentist_config$save_detailed_results) {
  save(frequentist_results, frequentist_df, frequentist_config, 
       file = here("output", "frequentist_estimators_results.RData"))
  write.csv(frequentist_df, here("output", "tables", "frequentist_estimators.csv"), row.names = FALSE)
  cat("Results saved to output/frequentist_estimators_results.RData\n")
}

# Summary
cat("\n=== FREQUENTIST ANALYSIS COMPLETE ===\n")
cat("Total estimations:", nrow(frequentist_df), "\n")
cat("Methods:", paste(unique(frequentist_df$method_clean), collapse = ", "), "\n")
cat("Indicators:", length(unique(frequentist_df$indicator_clean)), "\n")
cat("Population scenarios:", length(unique(frequentist_df$pop_label)), "\n")
cat("Bootstrap samples:", frequentist_config$n_bootstrap, "\n")

# Show sample results 
cat("\nSample results (980K population):\n")
sample_results <- frequentist_df %>%
  filter(pop_label == "980K", !is.na(estimate)) %>%
  select(indicator_clean, method_clean, estimate_with_ci)

for(i in 1:nrow(sample_results)) {
  row <- sample_results[i,]
  cat(sprintf("%-25s %-8s: %s\n", row$indicator_clean, row$method_clean, row$estimate_with_ci))
}

cat("\nFrequentist analysis ready for comparison with Bayesian methods!\n")
# 07-comprehensive_results_comparison.R
# Comprehensive Results Comparison Across All Estimators and Parameters
# Loads ALL available results files and creates unified comparison
#
# PURPOSE: Compare results and CIs across:
# - Different estimators (RDS-I/II/SS, MA.estimates, posteriorsize, NSUM)
# - Different indicators (all comparable indicators) 
# - Different parameter values (iterations, M1/M2, population sizes)
# - Different time periods (track parameter evolution)

cat("=== COMPREHENSIVE RESULTS COMPARISON ===\n")
cat("Loading ALL available results for unified comparison\n\n")

# Load required libraries
library(tidyverse)
library(here)
library(lubridate)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# ============================================================================
# COMPREHENSIVE RESULTS LOADER
# ============================================================================

# Load all MA.estimates results from individual files
load_all_ma_results <- function() {
  
  cat("=== Loading All MA.estimates Results ===\n")
  
  # Find all MA results files
  ma_files <- list.files(here("output"), pattern = "^ma_", full.names = TRUE)
  
  cat("Found", length(ma_files), "MA results files:\n")
  for(file in ma_files) {
    cat("-", basename(file), "\n")
  }
  cat("\n")
  
  all_ma_results <- list()
  result_count <- 0
  
  for(file_path in ma_files) {
    file_name <- basename(file_path)
    
    tryCatch({
      # Load the file
      env <- new.env()
      load(file_path, envir = env)
      
      # Extract information based on file type
      if (grepl("ma_result_full_", file_name)) {
        # Full MA result objects
        if (exists("ma_result", envir = env)) {
          ma_obj <- get("ma_result", envir = env)
          
          # Extract indicator name from filename
          indicator <- str_extract(file_name, "(?<=ma_result_full_)[^.]+")
          
          # Convert MA result to standardized format
          result <- extract_ma_result_standardized(ma_obj, indicator, file_name, file_path)
          if (!is.null(result)) {
            result_count <- result_count + 1
            all_ma_results[[result_count]] <- result
          }
        }
        
      } else if (grepl("ma_estimates_.*_results", file_name)) {
        # Individual MA estimates results
        result_objects <- ls(env)[grepl("results", ls(env))]
        
        for(obj_name in result_objects) {
          obj <- get(obj_name, envir = env)
          if (is.list(obj)) {
            for(result in obj) {
              if (is.list(result) && "method" %in% names(result)) {
                result$source_file <- file_name
                result$file_path <- file_path
                result_count <- result_count + 1
                all_ma_results[[result_count]] <- result
              }
            }
          }
        }
        
      } else if (grepl("ma_result_composite_risk", file_name)) {
        # Your successful enhanced parameter result
        if (exists("ma_result", envir = env)) {
          ma_obj <- get("ma_result", envir = env)
          result <- extract_ma_result_standardized(ma_obj, "composite_risk", file_name, file_path)
          if (!is.null(result)) {
            result$parameter_type <- "enhanced"
            result$notes <- "Enhanced parameters - 3 iterations, M1=75, M2=50"
            result_count <- result_count + 1
            all_ma_results[[result_count]] <- result
          }
        }
      }
      
      cat("Processed:", file_name, "\n")
      
    }, error = function(e) {
      cat("Error loading", file_name, ":", e$message, "\n")
    })
  }
  
  cat("Loaded", result_count, "MA.estimates results\n\n")
  return(all_ma_results)
}

# Standardized extraction from MA result objects
extract_ma_result_standardized <- function(ma_obj, indicator, file_name, file_path) {
  
  tryCatch({
    # Get file modification time
    file_time <- file.info(file_path)$mtime
    
    # Extract parameters
    iterations <- if("number.of.iterations" %in% names(ma_obj)) ma_obj$number.of.iterations else NA
    M1 <- if("M1" %in% names(ma_obj)) ma_obj$M1 else NA
    M2 <- if("M2" %in% names(ma_obj)) ma_obj$M2 else NA
    N <- if("N" %in% names(ma_obj)) ma_obj$N else NA
    
    # Extract estimate using direct indexing method (your proven approach)
    if (!is.null(ma_obj$estimate) && inherits(ma_obj$estimate, "rds.interval.estimate")) {
      point_estimate <- ma_obj$estimate$interval[2]  # Point estimate
      ci_lower <- ma_obj$estimate$interval[4]        # Lower 95% CI  
      ci_upper <- ma_obj$estimate$interval[6]        # Upper 95% CI
      bayesian_se <- if(length(ma_obj$estimate$interval) >= 3) ma_obj$estimate$interval[3] else NA
    } else {
      point_estimate <- ci_lower <- ci_upper <- bayesian_se <- NA
    }
    
    return(list(
      method = "MA_estimates",
      indicator = indicator,
      estimate = point_estimate,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      se = bayesian_se,
      population_size = N,
      
      # Parameter details
      iterations = iterations,
      M1 = M1, 
      M2 = M2,
      
      # Metadata
      source_file = file_name,
      file_path = file_path,
      file_timestamp = file_time,
      method_type = "bayesian",
      uncertainty_method = "bayesian_credible_interval",
      
      # Additional fields for comparison
      estimate_pct = point_estimate * 100,
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      estimate_with_ci = sprintf("%.1f%% (%.1f–%.1f)", 
                                 point_estimate * 100, ci_lower * 100, ci_upper * 100)
    ))
    
  }, error = function(e) {
    cat("Error extracting from", file_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Load RDS frequentist results (from sensitivity analysis)
load_rds_frequentist_results <- function() {
  
  cat("=== Loading RDS Frequentist Results ===\n")
  
  # Load from the sensitivity analysis output
  results_files <- c(
    here("output", "final_appendix_results_NOBAYES.RData"),
    here("output", "rds_estimation_results.RData"),
    here("output", "bootstrap_results.RData")
  )
  
  all_rds_results <- list()
  
  for(file_path in results_files) {
    if (file.exists(file_path)) {
      cat("Loading:", basename(file_path), "\n")
      
      tryCatch({
        env <- new.env()
        load(file_path, envir = env)
        
        # Look for results data frames
        df_objects <- ls(env)[sapply(ls(env), function(x) is.data.frame(get(x, envir = env)))]
        
        for(df_name in df_objects) {
          df <- get(df_name, envir = env)
          if ("method" %in% names(df) || "method_clean" %in% names(df)) {
            # Convert to list format for consistency
            for(i in 1:nrow(df)) {
              result <- as.list(df[i, ])
              result$source_file <- basename(file_path)
              all_rds_results[[length(all_rds_results) + 1]] <- result
            }
          }
        }
        
      }, error = function(e) {
        cat("Error loading", basename(file_path), ":", e$message, "\n")
      })
    }
  }
  
  cat("Loaded", length(all_rds_results), "RDS frequentist results\n\n")
  return(all_rds_results)
}

# Load NSUM results (if available)
load_nsum_results <- function() {
  
  cat("=== Loading NSUM Results ===\n")
  
  nsum_files <- list.files(here("output"), pattern = "nsum.*results", full.names = TRUE)
  
  all_nsum_results <- list()
  
  for(file_path in nsum_files) {
    cat("Loading:", basename(file_path), "\n")
    
    tryCatch({
      env <- new.env()
      load(file_path, envir = env)
      
      # Extract NSUM results (structure may vary)
      result_objects <- ls(env)
      
      for(obj_name in result_objects) {
        obj <- get(obj_name, envir = env)
        if (is.list(obj) && length(obj) > 0) {
          # Add NSUM results to collection
          # (Structure depends on specific NSUM implementation)
        }
      }
      
    }, error = function(e) {
      cat("Error loading NSUM file:", e$message, "\n")
    })
  }
  
  cat("Loaded", length(all_nsum_results), "NSUM results\n\n")
  return(all_nsum_results)
}

# ============================================================================
# UNIFIED COMPARISON FUNCTIONS  
# ============================================================================

# Create comprehensive comparison table
create_comprehensive_comparison <- function() {
  
  # Load all results
  ma_results <- load_all_ma_results()
  rds_results <- load_rds_frequentist_results()
  nsum_results <- load_nsum_results()
  
  # Combine all results
  all_results <- c(ma_results, rds_results, nsum_results)
  
  # Standardize format
  comparison_df <- map_dfr(all_results, function(result) {
    
    # Ensure required fields exist
    result$method <- result$method %||% result$method_clean %||% "unknown"
    result$indicator <- result$indicator %||% result$indicator_clean %||% "unknown"
    result$estimate <- as.numeric(result$estimate %||% result$estimate_pct/100 %||% NA)
    result$ci_lower <- as.numeric(result$ci_lower %||% result$ci_lower_pct/100 %||% NA)
    result$ci_upper <- as.numeric(result$ci_upper %||% result$ci_upper_pct/100 %||% NA)
    
    data.frame(
      method = result$method,
      indicator = result$indicator,
      estimate_pct = result$estimate * 100,
      ci_lower_pct = result$ci_lower * 100,
      ci_upper_pct = result$ci_upper * 100,
      se_pct = (result$se %||% NA) * 100,
      estimate_with_ci = result$estimate_with_ci %||% 
                         sprintf("%.1f%% (%.1f–%.1f)", 
                                result$estimate * 100, 
                                result$ci_lower * 100, 
                                result$ci_upper * 100),
      method_type = result$method_type %||% "unknown",
      uncertainty_method = result$uncertainty_method %||% "unknown",
      population_size = result$population_size %||% result$pop_size %||% NA,
      iterations = result$iterations %||% result$ma_iterations %||% NA,
      M1 = result$M1 %||% NA,
      M2 = result$M2 %||% NA,
      source_file = result$source_file %||% "unknown",
      file_timestamp = result$file_timestamp %||% Sys.time(),
      stringsAsFactors = FALSE
    )
  })
  
  # Add indicator labels
  indicators_info <- get_comparable_indicators()
  comparison_df <- comparison_df %>%
    mutate(
      indicator_clean = case_when(
        str_detect(indicator, "_rds$") ~ indicators_info$labels[str_remove(indicator, "_rds$")],
        indicator == "composite_risk" ~ "Composite risk score",
        indicator == "whether_exploitation" ~ "Overall exploitation indicator", 
        indicator == "sum_categories" ~ "Risk exposure scale (ordinal)",
        TRUE ~ indicator
      ),
      method_clean = case_when(
        method == "RDS_I" ~ "RDS-I",
        method == "RDS_II" ~ "RDS-II",
        method == "RDS_SS" ~ "RDS-SS", 
        method == "MA_estimates" ~ "MA.estimates",
        method == "posteriorsize" ~ "posteriorsize",
        TRUE ~ method
      )
    ) %>%
    arrange(indicator_clean, method_clean, desc(file_timestamp))
  
  return(comparison_df)
}

# ============================================================================
# EXECUTION AND RESULTS
# ============================================================================

# Run comprehensive comparison
cat("Starting comprehensive results comparison...\n")
comprehensive_results <- create_comprehensive_comparison()

# Save results
save(comprehensive_results, file = here("output", "comprehensive_results_comparison.RData"))
write.csv(comprehensive_results, here("output", "tables", "comprehensive_comparison.csv"), row.names = FALSE)

# Summary
cat("=== COMPREHENSIVE COMPARISON COMPLETE ===\n")
cat("Total results:", nrow(comprehensive_results), "\n")
cat("Methods:", paste(unique(comprehensive_results$method_clean), collapse = ", "), "\n")
cat("Indicators:", length(unique(comprehensive_results$indicator_clean)), "\n")
cat("Method types:", paste(unique(comprehensive_results$method_type), collapse = ", "), "\n\n")

# Show key results
cat("=== KEY FINDINGS BY METHOD ===\n")
method_summary <- comprehensive_results %>%
  filter(!is.na(estimate_pct)) %>%
  group_by(method_clean, indicator_clean) %>%
  summarize(
    n_results = n(),
    mean_estimate = mean(estimate_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(indicator_clean, method_clean)

print(method_summary)

cat("\n=== FILES SAVED ===\n")
cat("- comprehensive_results_comparison.RData\n")
cat("- tables/comprehensive_comparison.csv\n\n")

cat("Use 'comprehensive_results' data frame for detailed analysis\n")
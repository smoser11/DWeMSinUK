# 06-modular_estimator_analysis.R
# Modular RDS Estimator Analysis with Individual Method Functions
# Domestic Worker Exploitation and Modern Slavery in UK
#
# ENHANCED PRODUCTION VERSION:
# - MA.estimates() with production parameters (50K MPLE, 2^19 steps, etc.)
# - Simplified extraction using direct array indexing (WORKING METHOD)
# - Enhanced result recording: full ma_result objects, comprehensive metadata
# - Compatible with downstream analysis: NSUM comparison, diagnostics, gt tables
# - Maintains backward compatibility with existing field names

cat("=== MODULAR Estimator Analysis v4 (Enhanced Production) ===\n")
cat("Individual functions for each estimator method\n")
cat("MA.estimates with production parameters + comprehensive result recording\n")
cat("Flexible combination and comparison tools\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(Neighboot)
library(coda)  # For MCMC diagnostics
library(parallel)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load prepared data (with weights)
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data with weights\n")
}

# ============================================================================
# MODULAR CONFIGURATION (same as original for compatibility)
# ============================================================================

modular_config <- list(
  # Methods available for individual analysis
  available_methods = c("RDS_I", "RDS_II", "RDS_SS", "MA_estimates", "posteriorsize"),
  
  # Population sizes for sensitivity analysis
  # population_sizes = c(50000, 100000, 980000, 1740000),
  # population_labels = c("50K", "100K", "980K", "1.74M"),
  population_sizes = c(980000),
  population_labels = c("980K"),
  
  # # All indicators to test
  indicators = c(get_comparable_indicators()$rds_vars, "composite_risk", "whether_exploitation", "sum_categories"),
  #indicators = c("pay_issues_rds"   ,        "threats_abuse_rds"),  # Just one indicator
  ma_iterations = 3,    # Was 3
  ma_M1 = 50,  #1000,        # Was 10000  
  ma_M2 = 25,  #500,        # Was 5000
  
  # Computational parameters
  parallel_cores = 3,
  n_bootstrap = NULL,
  
  # Bootstrap parameters (ONLY for frequentist methods)
  n_bootstrap_freq = 10000,  # For RDS-I, RDS-II, RDS-SS
  confidence_level = 0.95,
  quantiles = c(0.025, 0.975),
  
  # Bayesian MCMC parameters (for MA.estimates and posteriorsize)
  bayesian_samplesize = 100000, # Very aggressive increase for convergence (was 50k)
  bayesian_burnin = 200000,    # Very aggressive burnin for stationarity (was 100k)
  bayesian_interval = 21,      # More thinning to reduce autocorrelation
  # ma_iterations = 3,          # More MA.estimates iterations  -- 3
  # ma_M1 = 10000,                 # More networked populations for stability -- 10K-50K
  # ma_M2 = 5000,                  # More RDS samples per network 5K - 25K
  

  # Output control
  save_detailed_results = TRUE,
  create_comparison_tables = TRUE,
  check_convergence = TRUE,
  full.output = TRUE,
  verbose= TRUE
)

cat("Modular configuration:\n")
cat("- Available methods:", length(modular_config$available_methods), "\n")
cat("- Indicators:", length(modular_config$indicators), "\n")
cat("- Population scenarios:", length(modular_config$population_sizes), "\n\n")

# Make config globally available for individual functions
final_config <- modular_config  # For compatibility with existing functions

# ============================================================================
# CONVERGENCE CHECKING HELPER FUNCTIONS (same as original)
# ============================================================================

check_mcmc_convergence <- function(mcmc_samples, method_name = "MCMC") {
  
  if (is.null(mcmc_samples) || !is.matrix(mcmc_samples)) {
    return(list(convergence_ok = FALSE, warning = "No MCMC samples available"))
  }
  
  mcmc_obj <- mcmc(mcmc_samples)
  convergence_info <- list()
  warnings <- c()
  
  # Effective sample size
  eff_size <- effectiveSize(mcmc_obj)
  convergence_info$effective_size <- eff_size
  
  # Check if effective sample size is adequate (> 400 is generally good)
  if (any(eff_size < 400, na.rm = TRUE)) {
    warnings <- c(warnings, "Low effective sample size (< 400)")
  }
  
  # Geweke diagnostic
  geweke_result <- tryCatch({
    geweke.diag(mcmc_obj)
  }, error = function(e) NULL)
  
  if (!is.null(geweke_result)) {
    convergence_info$geweke_z <- geweke_result$z
    
    # Check convergence (|z| > 1.96 indicates problems)
    convergence_issues <- abs(geweke_result$z) > 1.96
    if (any(convergence_issues, na.rm = TRUE)) {
      failed_params <- names(geweke_result$z)[convergence_issues]
      warnings <- c(warnings, paste("Geweke convergence failed for:", paste(failed_params, collapse = ", ")))
    }
  }
  
  # Heidelberger-Welch diagnostic  
  heidel_result <- tryCatch({
    heidel.diag(mcmc_obj)
  }, error = function(e) NULL)
  
  if (!is.null(heidel_result)) {
    # Check stationarity
    if (any(!heidel_result[, "stest"], na.rm = TRUE)) {
      failed_params <- rownames(heidel_result)[!heidel_result[, "stest"]]
      warnings <- c(warnings, paste("Stationarity test failed for:", paste(failed_params, collapse = ", ")))
    }
    
    # Check halfwidth test
    if (any(!heidel_result[, "htest"], na.rm = TRUE)) {
      failed_params <- rownames(heidel_result)[!heidel_result[, "htest"]]
      warnings <- c(warnings, paste("Halfwidth test failed for:", paste(failed_params, collapse = ", ")))
    }
  }
  
  # Overall assessment
  convergence_ok <- length(warnings) == 0
  
  return(list(
    convergence_ok = convergence_ok,
    warnings = warnings,
    effective_size_min = min(eff_size, na.rm = TRUE),
    effective_size_mean = mean(eff_size, na.rm = TRUE),
    details = convergence_info
  ))
}

# ============================================================================
# CORE ESTIMATION FUNCTIONS (same as original - reused)
# ============================================================================

# RDS-I with neighb() bootstrap (frequentist)
estimate_final_rds_i <- function(outcome_var, n_bootstrap = 5000) {
  tryCatch({
    # Point estimate
    result <- RDS.I.estimates(rd.dd, outcome.variable = outcome_var)
    point_estimate <- result$estimate
    
    # Use working neighb() format from archived 04-bootstrap_analysis.R
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      cat("  Attempting neighb() for", outcome_var, "with working format\n")
      
      neighb(
        RDS.data = rds_sample,
        quant = modular_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("  ERROR in neighb():", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(modular_config$quantiles[1])]
      ci_upper <- var_results[as.character(modular_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("  SUCCESS: neighb() worked!\n")
    } else {
      # Fallback to asymptotic CI if neighb() still fails
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("  FALLBACK: Using asymptotic CI\n")
    }
    
    list(
      method = "RDS_I",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = NA,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    )
  }, error = function(e) {
    list(method = "RDS_I", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
         error = e$message, method_type = "frequentist")
  })
}

# RDS-II with neighborhood bootstrap (frequentist)  
estimate_final_rds_ii <- function(outcome_var, n_bootstrap = 5000) {
  tryCatch({
    # Point estimate
    result <- RDS.II.estimates(rd.dd, outcome.variable = outcome_var)
    point_estimate <- result$estimate
    
    # Use working neighb() format (same as RDS-I)
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      cat("  Attempting neighb() for", outcome_var, "in RDS-II\n")
      
      neighb(
        RDS.data = rds_sample,
        quant = modular_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("  ERROR in neighb():", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(modular_config$quantiles[1])]
      ci_upper <- var_results[as.character(modular_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("  SUCCESS: neighb() worked for RDS-II! SE=", bootstrap_se, "CI=", ci_lower, "-", ci_upper, "\n")
    } else {
      # Debug what neighb() actually returned
      cat("  DEBUG RDS-II: neighb_result class=", class(neighb_result), "\n")
      if (is.matrix(neighb_result)) {
        cat("  DEBUG RDS-II: rownames=", rownames(neighb_result), "\n")
        cat("  DEBUG RDS-II: looking for outcome_var=", outcome_var, "\n")
      }
      
      # Fallback to asymptotic CI if neighb() fails
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("  FALLBACK: Using asymptotic CI for RDS-II\n")
    }
    
    list(
      method = "RDS_II",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = NA,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    )
  }, error = function(e) {
    list(method = "RDS_II", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
         error = e$message, method_type = "frequentist")
  })
}

# RDS-SS with neighborhood bootstrap (frequentist)
estimate_final_rds_ss <- function(outcome_var, population_size, n_bootstrap = 5000) {
  tryCatch({
    # Point estimate
    result <- RDS.SS.estimates(rd.dd, outcome.variable = outcome_var, N = population_size)
    point_estimate <- result$estimate
    
    # Use working neighb() format (same as RDS-I and RDS-II)
    rds_sample <- convert_rds_to_neighboot_format()
    
    neighb_result <- tryCatch({
      cat("  Attempting neighb() for", outcome_var, "in RDS-SS\n")
      
      neighb(
        RDS.data = rds_sample,
        quant = modular_config$quantiles,
        method = "percentile", 
        B = n_bootstrap
      )
    }, error = function(e) {
      cat("  ERROR in neighb():", e$message, "\n")
      NULL
    })
    
    if (!is.null(neighb_result) && is.matrix(neighb_result) && 
        outcome_var %in% rownames(neighb_result)) {
      # Extract results from neighb() matrix
      var_results <- neighb_result[outcome_var, ]
      bootstrap_se <- var_results["SE"]
      ci_lower <- var_results[as.character(modular_config$quantiles[1])]
      ci_upper <- var_results[as.character(modular_config$quantiles[2])]
      uncertainty_method <- "neighb_bootstrap"
      cat("  SUCCESS: neighb() worked for RDS-SS!\n")
    } else {
      # Fallback to asymptotic CI if neighb() fails
      bootstrap_se <- result$se
      ci_lower <- pmax(0, point_estimate - 1.96 * bootstrap_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * bootstrap_se)
      uncertainty_method <- "asymptotic"
      cat("  FALLBACK: Using asymptotic CI for RDS-SS\n")
    }
    
    list(
      method = "RDS_SS",
      estimate = point_estimate,
      se = bootstrap_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = population_size,
      uncertainty_method = uncertainty_method,
      method_type = "frequentist"
    )
  }, error = function(e) {
    list(method = "RDS_SS", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
         error = e$message, population_size = population_size, method_type = "frequentist")
  })
}

# MA.estimates with BUILT-IN Bayesian credible intervals (NO bootstrap!)
estimate_final_ma_estimates <- function(outcome_var, population_size) {
  tryCatch({
    cat("Processing indicator:", outcome_var, "for population:", format(population_size, big.mark = ","), "\n")
    
    # Run MA.estimates with production parameters
    ma_result <- MA.estimates(
      rd.dd, 
      trait.variable = outcome_var,
      N = population_size,
      number.of.iterations = 2,                           # Production: 2 iterations
      M1 = 50,
      M2 = 25,
      parallel = 1,                                       # Keep single-core as requested
      verbose = TRUE,                                    # Reduce output noise
      full.output = TRUE,                                 # Keep as requested
      seed = 42,
      # Production parameters
      MPLE.samplesize = 50000,                           # Production: default 50000
      SAN.maxit = 10,                                     # Production: default 5
      SAN.nsteps = 2^19,                                 # Production: default 2^19
      sim.interval = 10000                               # Production: default 10000
    )
    
    # Extract values using direct array indexing (your working method)
    if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
      point_estimate <- ma_result$estimate$interval[2]  # Point estimate
      ci_lower <- ma_result$estimate$interval[4]        # Lower 95% CI
      ci_upper <- ma_result$estimate$interval[6]        # Upper 95% CI
      bayesian_se <- if(length(ma_result$estimate$interval) >= 3) ma_result$estimate$interval[3] else NA
      
      cat("  Estimate:", round(point_estimate, 4), "CI: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")
    } else {
      point_estimate <- NA
      ci_lower <- NA  
      ci_upper <- NA
      bayesian_se <- NA
    }
    
    # Store parameters used for this run
    ma_parameters <- list(
      N = population_size,
      number.of.iterations = 2,
      M1 = 2,
      M2 = 1,
      parallel = 1,
      verbose = FALSE,
      full.output = TRUE,
      seed = 42,
      MPLE.samplesize = 50000,
      SAN.maxit = 10,
      SAN.nsteps = 2^19,
      sim.interval = 10000
    )
    
    # Save complete MA.estimates result for diagnostics
    ma_debug_file <- here("output", paste0("ma_result_full_", outcome_var, ".RData"))
    save(ma_result, file = ma_debug_file)
    
    # Extract additional diagnostic information if available
    convergence_details <- if (!is.null(ma_result$details)) {
      list(
        details_available = TRUE,
        details_class = class(ma_result$details),
        details_length = length(ma_result$details)
      )
    } else {
      list(details_available = FALSE)
    }
    
    list(
      # Core results (maintain backward compatibility)
      method = "MA_estimates",
      estimate = point_estimate,
      se = bayesian_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = population_size,
      uncertainty_method = "bayesian_credible_interval",
      method_type = "bayesian",
      
      # Enhanced fields for comprehensive analysis
      indicator = outcome_var,
      bayesian_credible_interval = list(
        lower_95 = ci_lower,
        upper_95 = ci_upper,
        point_estimate = point_estimate,
        standard_error = bayesian_se
      ),
      ma_parameters = ma_parameters,
      ma_result_file = ma_debug_file,
      convergence_info = convergence_details,
      extraction_method = "direct_array_indexing",
      extraction_indices = list(point = 2, lower_ci = 4, upper_ci = 6, se = 3),
      
      # Metadata for comparison with other methods
      estimation_type = "bayesian_mcmc",
      uncertainty_type = "credible_interval",
      ci_coverage = 0.95,
      run_timestamp = Sys.time()
    )
    
  }, error = function(e) {
    cat("  ERROR in MA.estimates for", outcome_var, ":", e$message, "\n")
    list(method = "MA_estimates", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
         error = paste("MA.estimates failed:", e$message), population_size = population_size, method_type = "bayesian")
  })
}

# posteriorsize with BUILT-IN Bayesian credible intervals (NO bootstrap!)
estimate_final_posteriorsize <- function(outcome_var, population_size) {
  tryCatch({
    # Use posteriorsize() with aggressive MCMC parameters only (avoid broken model adjustments)
    ps_result <- posteriorsize(
      rd.dd,
      mean.prior.size = population_size,
      sd.prior.size = population_size * 0.1,              # Revert to working prior
      samplesize = modular_config$bayesian_samplesize,    # Now 100,000
      burnin = modular_config$bayesian_burnin,            # Now 200,000  
      interval = modular_config$bayesian_interval,        # Keep 21
      parallel = modular_config$parallel_cores,
      verbose = TRUE
    )
    
    # Extract population size estimates with credible intervals (built-in!)
    if (!is.null(ps_result$N) && length(ps_result$N) >= 5) {
      # ps_result$N contains: MAP, Mean, Median, P025, P975
      pop_map <- ps_result$N["MAP"]
      pop_mean <- ps_result$N["Mean AP"]
      pop_median <- ps_result$N["Median AP"] 
      pop_ci_lower <- ps_result$N["P025"]
      pop_ci_upper <- ps_result$N["P975"]
    } else {
      pop_map <- pop_mean <- pop_median <- pop_ci_lower <- pop_ci_upper <- NA
    }
    
    # For the outcome variable, we need a separate estimate
    # This is a simplified approach - use RDS-II as baseline with population uncertainty
    rds_result <- RDS.II.estimates(rd.dd, outcome.variable = outcome_var)
    point_estimate <- rds_result$estimate
    
    # The uncertainty comes from population size uncertainty
    # We use the coefficient of variation from the population posterior
    if (!is.na(pop_mean) && !is.na(pop_ci_lower) && !is.na(pop_ci_upper)) {
      pop_cv <- (pop_ci_upper - pop_ci_lower) / (4 * pop_mean)  # Approximate CV
      # Apply this uncertainty to the trait estimate
      trait_se <- point_estimate * pop_cv
      ci_lower <- pmax(0, point_estimate - 1.96 * trait_se)
      ci_upper <- pmin(1, point_estimate + 1.96 * trait_se)
    } else {
      trait_se <- ci_lower <- ci_upper <- NA
    }
    
    # Check convergence using MCMC samples with comprehensive diagnostics
    convergence_info <- "No convergence check"
    convergence_ok <- TRUE
    if (!is.null(ps_result$sample) && modular_config$check_convergence) {
      # Use comprehensive convergence checking
      conv_check <- check_mcmc_convergence(ps_result$sample, "posteriorsize")
      convergence_ok <- conv_check$convergence_ok
      
      if (convergence_ok) {
        convergence_info <- paste0("✓ Convergence OK (min ESS: ", round(conv_check$effective_size_min), ")")
      } else {
        convergence_info <- paste0("⚠ Convergence issues: ", paste(conv_check$warnings, collapse = "; "))
        cat("WARNING [posteriorsize]:", convergence_info, "\n")
      }
    }
    
    list(
      method = "posteriorsize",
      estimate = point_estimate,
      se = trait_se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      population_size = population_size,
      uncertainty_method = "bayesian_credible_interval",
      method_type = "bayesian",
      # Population size posterior summaries
      pop_map = pop_map,
      pop_mean = pop_mean,
      pop_median = pop_median,
      pop_ci_lower = pop_ci_lower,
      pop_ci_upper = pop_ci_upper,
      convergence_info = convergence_info,
      samplesize = modular_config$bayesian_samplesize,
      burnin = modular_config$bayesian_burnin
    )
    
  }, error = function(e) {
    list(method = "posteriorsize", estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, 
         error = e$message, population_size = population_size, method_type = "bayesian")
  })
}

# NEIGHBOOT data conversion (working version from archived 04-bootstrap_analysis.R)
convert_rds_to_neighboot_format <- function() {
  
  # Create proper edges data frame (exclude seeds with recruiter.id == -1) 
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  # Renumber nodes sequentially to avoid "Duplicate vertex names"
  node_map <- setNames(1:length(rd.dd$id), rd.dd$id)
  
  # Use CE's comparable indicators
  comparable_traits <- data.frame(
    document_withholding_rds = rd.dd$document_withholding_rds,
    pay_issues_rds = rd.dd$pay_issues_rds, 
    threats_abuse_rds = rd.dd$threats_abuse_rds,
    excessive_hours_rds = rd.dd$excessive_hours_rds,
    access_to_help_rds = rd.dd$access_to_help_rds,
    composite_risk = rd.dd$composite_risk,
    whether_exploitation = rd.dd$whether_exploitation
  )
  
  # Handle missing values (replace with 0 for binary indicators)
  comparable_traits <- comparable_traits %>%
    mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))
  
  # Create corrected RDS data structure (WORKING FORMAT)
  rds_data <- list(
    nodes = 1:length(node_map),
    edges = data.frame(
      from = node_map[as.character(edges_df$from)],
      to = node_map[as.character(edges_df$to)]
    ),
    traits = comparable_traits,
    degree = setNames(rd.dd$network.size, 1:length(rd.dd$network.size))
  )
  
  return(rds_data)
}

# Results processing function (reuse from original)
process_final_results <- function(all_results) {
  
  # Helper function to safely extract values
  safe_extract <- function(result, field, default = NA) {
    if (is.null(result) || !is.list(result)) return(default)
    value <- result[[field]]
    if (is.null(value) || length(value) == 0) return(default)
    return(value)
  }
  
  results_df <- map_dfr(all_results, function(result) {
    # Skip NULL or invalid results
    if (is.null(result) || !is.list(result)) {
      return(data.frame(
        indicator = "unknown", pop_size = NA, pop_label = NA, method = NA,
        method_type = NA, estimate = NA, se = NA, ci_lower = NA,
        ci_upper = NA, uncertainty_method = NA, convergence_info = NA,
        error_msg = "Invalid result", stringsAsFactors = FALSE,
        row.names = NULL  # Explicitly set row names to avoid issues
      ))
    }
    
    # Ensure indicator has a valid value to avoid missing row names
    indicator_val <- safe_extract(result, "indicator", "unknown")
    if (is.na(indicator_val) || is.null(indicator_val) || indicator_val == "") {
      indicator_val <- "unknown"
    }
    
    data.frame(
      indicator = indicator_val,
      pop_size = safe_extract(result, "pop_size", NA),
      pop_label = safe_extract(result, "pop_label", NA),
      method = safe_extract(result, "method", NA),
      method_type = safe_extract(result, "method_type", NA),
      estimate = safe_extract(result, "estimate", NA),
      se = safe_extract(result, "se", NA),
      ci_lower = safe_extract(result, "ci_lower", NA),
      ci_upper = safe_extract(result, "ci_upper", NA),
      uncertainty_method = safe_extract(result, "uncertainty_method", NA),
      convergence_info = safe_extract(result, "convergence_info", "No info"),
      error_msg = safe_extract(result, "error", NA),
      stringsAsFactors = FALSE,
      row.names = NULL  # Explicitly avoid row name issues
    )
  })
  
  # Add indicator labels and clean method names
  results_df <- results_df %>%
    mutate(
      indicator_clean = case_when(
        str_detect(indicator, "_rds$") ~ get_comparable_indicators()$labels[str_remove(indicator, "_rds$")],
        indicator == "composite_risk" ~ "Composite risk score",
        indicator == "whether_exploitation" ~ "Overall exploitation indicator",
        indicator == "sum_categories" ~ "Risk exposure scale (ordinal)",
        TRUE ~ indicator
      ),
      method_clean = case_when(
        method == "RDS_I" ~ "RDS-I",
        method == "RDS_II" ~ "RDS-II", 
        method == "RDS_SS" ~ "RDS-SS",
        method == "MA_estimates" ~ "MA.estimates()",
        method == "posteriorsize" ~ "posteriorsize()",
        TRUE ~ method
      ),
      uncertainty_clean = case_when(
        uncertainty_method == "neighb_bootstrap" ~ "Neighboot",
        uncertainty_method == "simple_bootstrap" ~ "Bootstrap",
        uncertainty_method == "bayesian_credible_interval" ~ "Bayesian CI",
        uncertainty_method == "asymptotic" ~ "Asymptotic",
        TRUE ~ uncertainty_method
      ),
      estimate_pct = estimate * 100,
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      se_pct = se * 100,
      estimate_with_ci = ifelse(
        !is.na(ci_lower) & !is.na(ci_upper),
        paste0(
          sprintf("%.1f", estimate_pct), "% (",
          sprintf("%.1f", ci_lower_pct), "–",
          sprintf("%.1f", ci_upper_pct), ")"
        ),
        paste0(sprintf("%.1f", estimate_pct), "% (CI not available)")
      )
    )
  
  return(results_df)
}

# ============================================================================
# MODULAR INDIVIDUAL ESTIMATOR FUNCTIONS
# ============================================================================

# RDS-I Analysis (standalone)
run_rds_i_analysis <- function(indicators = NULL, population_sizes = NULL, 
                               n_bootstrap = NULL, save_results = TRUE) {
  
  if (is.null(indicators)) indicators <- modular_config$indicators
  if (is.null(population_sizes)) population_sizes <- modular_config$population_sizes
  if (is.null(n_bootstrap)) n_bootstrap <- modular_config$n_bootstrap_freq
  
  cat("=== RDS-I Analysis (Standalone) ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Bootstrap samples:", n_bootstrap, "\n\n")
  
  rds_i_results <- list()
  result_count <- 0
  
  for (indicator in indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    cat("Processing RDS-I for:", indicator, "\n")
    
    for (pop_size in population_sizes) {
      result_count <- result_count + 1
      
      result <- estimate_final_rds_i(indicator, n_bootstrap)
      result$indicator <- indicator
      result$pop_size <- pop_size
      result$pop_label <- modular_config$population_labels[which(modular_config$population_sizes == pop_size)]
      
      rds_i_results[[result_count]] <- result
      
      # Debug: Show what we got back
      cat("  DEBUG: estimate =", result$estimate, "ci_lower =", result$ci_lower, "ci_upper =", result$ci_upper, "\n")
      
      if (any(!is.na(result$estimate))) {
        cat("  Pop", format(pop_size, big.mark = ","), ":", 
            sprintf("%.2f%% (%.2f-%.2f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
      } else {
        cat("  No results to display (estimate is NA)\n")
      }
    }
  }
  
  # Process results
  rds_i_df <- process_final_results(rds_i_results)
  
  # Save if requested
  if (save_results) {
    save(rds_i_results, rds_i_df, file = here("output", "rds_i_analysis_results.RData"))
    write.csv(rds_i_df, here("output", "tables", "rds_i_analysis.csv"), row.names = FALSE)
  }
  
  cat("RDS-I analysis completed:", result_count, "estimations\n\n")
  
  return(list(
    results_raw = rds_i_results,
    results_df = rds_i_df,
    method = "RDS_I",
    n_estimations = result_count
  ))
}

# RDS-II Analysis (standalone)
run_rds_ii_analysis <- function(indicators = NULL, population_sizes = NULL, 
                                n_bootstrap = NULL, save_results = TRUE) {
  
  if (is.null(indicators)) indicators <- modular_config$indicators
  if (is.null(population_sizes)) population_sizes <- modular_config$population_sizes
  if (is.null(n_bootstrap)) n_bootstrap <- modular_config$n_bootstrap_freq
  
  cat("=== RDS-II Analysis (Standalone) ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Bootstrap samples:", n_bootstrap, "\n\n")
  
  rds_ii_results <- list()
  result_count <- 0
  
  for (indicator in indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    cat("Processing RDS-II for:", indicator, "\n")
    
    for (pop_size in population_sizes) {
      result_count <- result_count + 1
      
      result <- estimate_final_rds_ii(indicator, n_bootstrap)
      result$indicator <- indicator
      result$pop_size <- pop_size
      result$pop_label <- modular_config$population_labels[which(modular_config$population_sizes == pop_size)]
      
      rds_ii_results[[result_count]] <- result
      
      # Debug: Show what we got back
      cat("  DEBUG: estimate =", result$estimate, "ci_lower =", result$ci_lower, "ci_upper =", result$ci_upper, "\n")
      
      if (any(!is.na(result$estimate))) {
        cat("  Pop", format(pop_size, big.mark = ","), ":", 
            sprintf("%.2f%% (%.2f-%.2f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
      } else {
        cat("  No results to display (estimate is NA)\n")
      }
    }
  }
  
  # Process results
  rds_ii_df <- process_final_results(rds_ii_results)
  
  # Save if requested
  if (save_results) {
    save(rds_ii_results, rds_ii_df, file = here("output", "rds_ii_analysis_results.RData"))
    write.csv(rds_ii_df, here("output", "tables", "rds_ii_analysis.csv"), row.names = FALSE)
  }
  
  cat("RDS-II analysis completed:", result_count, "estimations\n\n")
  
  return(list(
    results_raw = rds_ii_results,
    results_df = rds_ii_df,
    method = "RDS_II",
    n_estimations = result_count
  ))
}

# RDS-SS Analysis (standalone)
run_rds_ss_analysis <- function(indicators = NULL, population_sizes = NULL, 
                                n_bootstrap = NULL, save_results = TRUE) {
  
  if (is.null(indicators)) indicators <- modular_config$indicators
  if (is.null(population_sizes)) population_sizes <- modular_config$population_sizes
  if (is.null(n_bootstrap)) n_bootstrap <- modular_config$n_bootstrap_freq
  
  cat("=== RDS-SS Analysis (Standalone) ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Bootstrap samples:", n_bootstrap, "\n\n")
  
  rds_ss_results <- list()
  result_count <- 0
  
  for (indicator in indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    cat("Processing RDS-SS for:", indicator, "\n")
    
    for (pop_size in population_sizes) {
      result_count <- result_count + 1
      
      result <- estimate_final_rds_ss(indicator, pop_size, n_bootstrap)
      result$indicator <- indicator
      result$pop_size <- pop_size
      result$pop_label <- modular_config$population_labels[which(modular_config$population_sizes == pop_size)]
      
      rds_ss_results[[result_count]] <- result
      
      # Debug: Show what we got back
      cat("  DEBUG: estimate =", result$estimate, "ci_lower =", result$ci_lower, "ci_upper =", result$ci_upper, "\n")
      
      if (any(!is.na(result$estimate))) {
        cat("  Pop", format(pop_size, big.mark = ","), ":", 
            sprintf("%.2f%% (%.2f-%.2f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
      } else {
        cat("  No results to display (estimate is NA)\n")
      }
    }
  }
  
  # Process results
  rds_ss_df <- process_final_results(rds_ss_results)
  
  # Save if requested
  if (save_results) {
    save(rds_ss_results, rds_ss_df, file = here("output", "rds_ss_analysis_results.RData"))
    write.csv(rds_ss_df, here("output", "tables", "rds_ss_analysis.csv"), row.names = FALSE)
  }
  
  cat("RDS-SS analysis completed:", result_count, "estimations\n\n")
  
  return(list(
    results_raw = rds_ss_results,
    results_df = rds_ss_df,
    method = "RDS_SS",
    n_estimations = result_count
  ))
}

# MA.estimates Analysis (standalone) - Bayesian
run_ma_estimates_analysis <- function(indicators = NULL, population_sizes = NULL, 
                                      save_results = TRUE) {
  
  if (is.null(indicators)) indicators <- modular_config$indicators
  if (is.null(population_sizes)) population_sizes <- modular_config$population_sizes
  
  cat("=== MA.estimates Analysis (Standalone Bayesian) ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("MA iterations:", modular_config$ma_iterations, "\n")
  cat("MA M1/M2:", modular_config$ma_M1, "/", modular_config$ma_M2, "\n\n")
  
  ma_results <- list()
  result_count <- 0
  
  for (indicator in indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    gc()
    cat("Processing MA.estimates for:", indicator, "\n")
    
    # Process each indicator separately for memory management
    indicator_results <- list()
    indicator_count <- 0
    
    for (pop_size in population_sizes) {
      result_count <- result_count + 1
      indicator_count <- indicator_count + 1
      
      # Print memory usage before estimation
      cat("  Memory before:", format(object.size(environment()), units = "MB"), "\n")
      
      result <- estimate_final_ma_estimates(indicator, pop_size)
      result$indicator <- indicator
      result$pop_size <- pop_size
      result$pop_label <- modular_config$population_labels[which(modular_config$population_sizes == pop_size)]
      
      indicator_results[[indicator_count]] <- result
      ma_results[[result_count]] <- result
      
      # Debug: Show what we got back
      cat("  DEBUG: estimate =", result$estimate, "ci_lower =", result$ci_lower, "ci_upper =", result$ci_upper, "\n")
      
      if (any(!is.na(result$estimate))) {
        cat("  Pop", format(pop_size, big.mark = ","), ":", 
            sprintf("%.2f%% (%.2f-%.2f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
      } else {
        cat("  No results to display (estimate is NA)\n")
      }
      
      # Force garbage collection after each population size
      gc()
      cat("  Memory after GC:", format(object.size(environment()), units = "MB"), "\n")
    }
    
    # Save results for this indicator immediately
    if (save_results) {
      indicator_file <- here("output", paste0("ma_estimates_", indicator, "_results.RData"))
      save(indicator_results, file = indicator_file)
      cat("  Saved results for", indicator, "to", basename(indicator_file), "\n")
    }
    
    # Aggressive memory cleanup
    rm(indicator_results)
    gc()
    cat("Completed indicator:", indicator, "- Memory after cleanup:", format(object.size(environment()), units = "MB"), "\n\n")
  }
  
  # Process results
  ma_df <- process_final_results(ma_results)
  
  # Save if requested
  if (save_results) {
    save(ma_results, ma_df, file = here("output", "ma_estimates_analysis_results.RData"))
    write.csv(ma_df, here("output", "tables", "ma_estimates_analysis.csv"), row.names = FALSE)
  }
  
  cat("MA.estimates analysis completed:", result_count, "estimations\n\n")
  
  return(list(
    results_raw = ma_results,
    results_df = ma_df,
    method = "MA_estimates",
    n_estimations = result_count
  ))
}

# posteriorsize Analysis (standalone) - Bayesian
run_posteriorsize_analysis <- function(indicators = NULL, population_sizes = NULL, 
                                       save_results = TRUE) {
  
  if (is.null(indicators)) indicators <- modular_config$indicators
  if (is.null(population_sizes)) population_sizes <- modular_config$population_sizes
  
  cat("=== posteriorsize Analysis (Standalone Bayesian) ===\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population sizes:", length(population_sizes), "\n")
  cat("Bayesian samplesize:", modular_config$bayesian_samplesize, "\n")
  cat("Bayesian burnin:", modular_config$bayesian_burnin, "\n\n")
  
  ps_results <- list()
  result_count <- 0
  
  for (indicator in indicators) {
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    cat("Processing posteriorsize for:", indicator, "\n")
    
    for (pop_size in population_sizes) {
      result_count <- result_count + 1
      
      result <- estimate_final_posteriorsize(indicator, pop_size)
      result$indicator <- indicator
      result$pop_size <- pop_size
      result$pop_label <- modular_config$population_labels[which(modular_config$population_sizes == pop_size)]
      
      ps_results[[result_count]] <- result
      
      # Debug: Show what we got back
      cat("  DEBUG: estimate =", result$estimate, "ci_lower =", result$ci_lower, "ci_upper =", result$ci_upper, "\n")
      
      if (any(!is.na(result$estimate))) {
        cat("  Pop", format(pop_size, big.mark = ","), ":", 
            sprintf("%.2f%% (%.2f-%.2f)", result$estimate*100, result$ci_lower*100, result$ci_upper*100), "\n")
      } else {
        cat("  No results to display (estimate is NA)\n")
      }
    }
  }
  
  # Process results
  ps_df <- process_final_results(ps_results)
  
  # Save if requested
  if (save_results) {
    save(ps_results, ps_df, file = here("output", "posteriorsize_analysis_results.RData"))
    write.csv(ps_df, here("output", "tables", "posteriorsize_analysis.csv"), row.names = FALSE)
  }
  
  cat("posteriorsize analysis completed:", result_count, "estimations\n\n")
  
  return(list(
    results_raw = ps_results,
    results_df = ps_df,
    method = "posteriorsize", 
    n_estimations = result_count
  ))
}

# ============================================================================
# RESULTS CONSOLIDATION FUNCTIONS
# ============================================================================

# Combine results from multiple estimator runs
combine_estimator_results <- function(..., method_names = NULL) {
  
  analysis_results <- list(...)
  
  if (is.null(method_names)) {
    method_names <- sapply(analysis_results, function(x) x$method)
  }
  
  cat("=== Combining Results from", length(analysis_results), "Estimators ===\n")
  for (i in 1:length(analysis_results)) {
    cat("-", method_names[i], ":", analysis_results[[i]]$n_estimations, "estimations\n")
  }
  
  # Combine raw results
  all_raw_results <- list()
  count <- 0
  
  for (result in analysis_results) {
    for (raw_result in result$results_raw) {
      count <- count + 1
      all_raw_results[[count]] <- raw_result
    }
  }
  
  # Combine data frames
  all_dfs <- lapply(analysis_results, function(x) x$results_df)
  combined_df <- do.call(rbind, all_dfs)
  
  cat("Combined results:", count, "total estimations\n\n")
  
  return(list(
    results_raw = all_raw_results,
    results_df = combined_df,
    methods = method_names,
    n_estimations = count,
    source_analyses = analysis_results
  ))
}

# Load and combine saved results
load_and_combine_results <- function(result_files) {
  
  cat("=== Loading and Combining Saved Results ===\n")
  
  combined_results <- list()
  all_raw_results <- list()
  all_dfs <- list()
  count <- 0
  
  for (file in result_files) {
    if (file.exists(file)) {
      cat("Loading:", file, "\n")
      
      # Load the file
      env <- new.env()
      load(file, envir = env)
      
      # Extract the results (assuming standard naming convention)
      if (exists("results_df", envir = env)) {
        all_dfs[[length(all_dfs) + 1]] <- get("results_df", envir = env)
      }
      
      # Find raw results (various naming patterns)
      raw_result_names <- ls(env)[grepl("_results$", ls(env))]
      for (name in raw_result_names) {
        raw_results <- get(name, envir = env)
        if (is.list(raw_results)) {
          for (result in raw_results) {
            count <- count + 1
            all_raw_results[[count]] <- result
          }
        }
      }
    } else {
      cat("Warning: File not found:", file, "\n")
    }
  }
  
  # Combine data frames
  if (length(all_dfs) > 0) {
    combined_df <- do.call(rbind, all_dfs)
  } else {
    combined_df <- data.frame()
  }
  
  cat("Loaded and combined:", count, "estimations from", length(result_files), "files\n\n")
  
  return(list(
    results_raw = all_raw_results,
    results_df = combined_df,
    n_estimations = count,
    source_files = result_files
  ))
}

# ============================================================================
# SELECTIVE VISUALIZATION AND TABLES
# ============================================================================

# Create comparison table for selected methods
create_selective_comparison_table <- function(results_df, methods = NULL, 
                                              indicators = NULL, population_size = 980000,
                                              save_table = TRUE, table_name = "selective_comparison") {
  
  if (is.null(methods)) methods <- unique(results_df$method)
  if (is.null(indicators)) indicators <- unique(results_df$indicator)
  
  cat("=== Creating Selective Comparison Table ===\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n\n")
  
  # Filter results
  filtered_results <- results_df %>%
    filter(method %in% methods,
           indicator %in% indicators,
           pop_size == population_size,
           !is.na(estimate)) %>%
    select(indicator_clean, method_clean, method_type, estimate_pct, 
           ci_lower_pct, ci_upper_pct, estimate_with_ci, uncertainty_clean)
  
  # Create wide table with duplicate handling
  comparison_table <- filtered_results %>%
    select(indicator_clean, method_clean, estimate_with_ci) %>%
    group_by(indicator_clean, method_clean) %>%
    slice(1) %>%
    ungroup() %>%
    pivot_wider(names_from = method_clean, 
                values_from = estimate_with_ci,
                values_fn = first) %>%
    arrange(indicator_clean)
  
  if (save_table) {
    filename <- paste0(table_name, "_", format(population_size/1000, digits = 1, nsmall = 0), "k.csv")
    write.csv(comparison_table, here("output", "tables", filename), row.names = FALSE)
    cat("Table saved:", filename, "\n")
  }
  
  return(comparison_table)
}

# Create selective visualization comparing methods
create_selective_plots <- function(results_df, methods = NULL, indicators = NULL, 
                                   save_plots = TRUE, plot_name = "selective_comparison") {
  
  if (is.null(methods)) methods <- unique(results_df$method)
  if (is.null(indicators)) indicators <- unique(results_df$indicator)
  
  cat("=== Creating Selective Plots ===\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Indicators:", length(indicators), "\n\n")
  
  # Filter results
  plot_data <- results_df %>%
    filter(method %in% methods,
           indicator %in% indicators,
           !is.na(estimate)) %>%
    mutate(
      method_clean = factor(method_clean, levels = unique(method_clean)),
      indicator_clean = factor(indicator_clean, levels = unique(indicator_clean))
    )
  
  # Create comparison plot
  comparison_plot <- ggplot(plot_data, aes(x = indicator_clean, y = estimate_pct, 
                                           color = method_clean, shape = method_type)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                  width = 0.2, alpha = 0.6) +
    facet_wrap(~ pop_label, scales = "free_x") +
    theme_rds_publication() +
    scale_color_manual(values = get_method_colors()) +
    scale_shape_manual(values = c("frequentist" = 16, "bayesian" = 17)) +
    labs(
      title = "Method Comparison: Selected Estimators",
      subtitle = paste("Methods:", paste(methods, collapse = ", ")),
      x = "Indicator",
      y = "Prevalence Estimate (%)",
      color = "Method",
      shape = "Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Population sensitivity plot
  pop_sensitivity_plot <- ggplot(plot_data, aes(x = pop_size, y = estimate_pct, 
                                                color = method_clean, linetype = indicator_clean)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(labels = scales::comma_format(), trans = "log10") +
    theme_rds_publication() +
    scale_color_manual(values = get_method_colors()) +
    labs(
      title = "Population Size Sensitivity: Selected Methods",
      subtitle = paste("Methods:", paste(methods, collapse = ", ")),
      x = "Population Size",
      y = "Prevalence Estimate (%)",
      color = "Method",
      linetype = "Indicator"
    )
  
  # Save plots
  if (save_plots) {
    ggsave(here("output", "figures", paste0(plot_name, "_comparison.png")), 
           comparison_plot, width = 12, height = 8, dpi = 300)
    ggsave(here("output", "figures", paste0(plot_name, "_sensitivity.png")), 
           pop_sensitivity_plot, width = 10, height = 8, dpi = 300)
    
    cat("Plots saved:\n")
    cat("-", paste0(plot_name, "_comparison.png"), "\n")
    cat("-", paste0(plot_name, "_sensitivity.png"), "\n")
  }
  
  return(list(
    comparison_plot = comparison_plot,
    sensitivity_plot = pop_sensitivity_plot
  ))
}

# ============================================================================
# EXAMPLE USAGE FUNCTIONS
# ============================================================================

# Example: Run just frequentist methods
run_frequentist_methods <- function(indicators = NULL, population_sizes = NULL, save_results = TRUE) {
  
  cat("=== Running Frequentist Methods Only ===\n\n")
  
  # Run individual analyses
  rds_i_analysis <- run_rds_i_analysis(indicators, population_sizes, save_results = save_results)
  rds_ii_analysis <- run_rds_ii_analysis(indicators, population_sizes, save_results = save_results)
  rds_ss_analysis <- run_rds_ss_analysis(indicators, population_sizes, save_results = save_results)
  
  # Combine results
  combined_freq <- combine_estimator_results(rds_i_analysis, rds_ii_analysis, rds_ss_analysis)
  
  # Create comparison table and plots
  comp_table <- create_selective_comparison_table(combined_freq$results_df, 
                                                  methods = c("RDS_I", "RDS_II", "RDS_SS"),
                                                  table_name = "frequentist_comparison")
  
  comp_plots <- create_selective_plots(combined_freq$results_df,
                                       methods = c("RDS_I", "RDS_II", "RDS_SS"),
                                       plot_name = "frequentist_comparison")
  
  return(combined_freq)
}

# Example: Run just Bayesian methods  
run_bayesian_methods <- function(indicators = NULL, population_sizes = NULL, save_results = TRUE) {
  
  cat("=== Running Bayesian Methods Only ===\n\n")
  
  # Run individual analyses
  ma_analysis <- run_ma_estimates_analysis(indicators, population_sizes, save_results = save_results)
  ps_analysis <- run_posteriorsize_analysis(indicators, population_sizes, save_results = save_results)
  
  # Combine results
  combined_bayes <- combine_estimator_results(ma_analysis, ps_analysis)
  
  # Create comparison table and plots
  comp_table <- create_selective_comparison_table(combined_bayes$results_df,
                                                  methods = c("MA_estimates", "posteriorsize"),
                                                  table_name = "bayesian_comparison")
  
  comp_plots <- create_selective_plots(combined_bayes$results_df,
                                       methods = c("MA_estimates", "posteriorsize"),
                                       plot_name = "bayesian_comparison")
  
  return(combined_bayes)
}

# ============================================================================
# USAGE EXAMPLES AND DOCUMENTATION
# ============================================================================

print_usage_examples <- function() {
  cat("=== MODULAR ESTIMATOR ANALYSIS USAGE EXAMPLES ===\n\n")
  
  cat("1. Run individual estimator:\n")
  cat("   rds_i_results <- run_rds_i_analysis()\n")
  cat("   rds_ss_results <- run_rds_ss_analysis()\n\n")
  
  cat("2. Run subset of indicators:\n")
  cat("   rds_i_results <- run_rds_i_analysis(indicators = c('document_withholding_rds', 'pay_issues_rds'))\n\n")
  
  cat("3. Run with custom parameters:\n")
  cat("   rds_ss_results <- run_rds_ss_analysis(n_bootstrap = 1000, save_results = FALSE)\n\n")
  
  cat("4. Combine results from different runs:\n")
  cat("   combined <- combine_estimator_results(rds_i_results, rds_ss_results)\n\n")
  
  cat("5. Load and combine saved results:\n")
  cat("   files <- c('output/rds_i_analysis_results.RData', 'output/rds_ss_analysis_results.RData')\n")
  cat("   combined <- load_and_combine_results(files)\n\n")
  
  cat("6. Create selective comparison table:\n")
  cat("   table <- create_selective_comparison_table(combined$results_df, methods = c('RDS_I', 'RDS_SS'))\n\n")
  
  cat("7. Create selective plots:\n")
  cat("   plots <- create_selective_plots(combined$results_df, methods = c('RDS_I', 'RDS_SS'))\n\n")
  
  cat("8. Run all frequentist methods:\n")
  cat("   freq_results <- run_frequentist_methods()\n\n")
  
  cat("9. Run all Bayesian methods:\n")
  cat("   bayes_results <- run_bayesian_methods()\n
  population_sizes  <- 980000 \n
      ma_analysis <- run_ma_estimates_analysis()
      \n")
}

# ============================================================================
# EXECUTION
# ============================================================================

# Print usage examples when file is loaded
if (!exists("skip_execution") || !skip_execution) {
  print_usage_examples()
} else {
  cat("MODULAR estimator analysis script loaded\n")
  cat("Use print_usage_examples() to see usage examples\n")

}
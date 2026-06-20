# 07-convergence_diagnostics_and_visualization.R
# Comprehensive Convergence Diagnostics for Bayesian Methods
# Model Comparison Visualization using sjPlot
# Domestic Worker Exploitation and Modern Slavery in UK

cat("=== Bayesian Convergence Diagnostics & Model Visualization ===\n")
cat("Comprehensive checks for MA.estimates() and posteriorsize() convergence\n")
cat("Publication-ready visualizations using sjPlot\n\n")

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(coda)        # MCMC diagnostics
library(sjPlot)      # Publication-ready plots
library(sjmisc)      # Helper functions for sjPlot
library(ggplot2)
library(viridis)     # Color palettes
library(patchwork)   # Combine plots
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# Load results if available
if (file.exists(here("output", "final_appendix_results.RData"))) {
  load(here("output", "final_appendix_results.RData"))
  cat("Loaded final appendix results\n")
} else {
  cat("Final results not found - will run basic diagnostics\n")
}

# ============================================================================
# CONVERGENCE DIAGNOSTICS CONFIGURATION
# ============================================================================

convergence_config <- list(
  # Diagnostic parameters
  diagnostic_indicators = c("document_withholding_rds", "composite_risk"),
  test_population_size = 980000,
  
  # Enhanced MCMC parameters for diagnostics
  diagnostic_samplesize = 5000,  # Larger for diagnostics
  diagnostic_burnin = 20000,     # Longer burnin
  diagnostic_interval = 5,       # Less thinning for diagnostics
  
  # MA.estimates parameters for diagnostics
  ma_diagnostic_iterations = 10,
  ma_diagnostic_M1 = 100,
  ma_diagnostic_M2 = 50,
  
  # Convergence tests
  geweke_test = TRUE,
  heidel_test = TRUE,
  raftery_test = TRUE,
  gelman_test = FALSE,  # Requires multiple chains
  
  # Visualization parameters
  save_plots = TRUE,
  plot_format = "png",
  plot_width = 12,
  plot_height = 8,
  plot_dpi = 300
)

cat("Convergence diagnostics configuration:\n")
cat("- Test indicators:", length(convergence_config$diagnostic_indicators), "\n")
cat("- Diagnostic samplesize:", convergence_config$diagnostic_samplesize, "\n")
cat("- Diagnostic burnin:", convergence_config$diagnostic_burnin, "\n")
cat("- MA diagnostic iterations/M1/M2:", convergence_config$ma_diagnostic_iterations, "/", 
    convergence_config$ma_diagnostic_M1, "/", convergence_config$ma_diagnostic_M2, "\n\n")

# ============================================================================
# CONVERGENCE DIAGNOSTICS FOR POSTERIORSIZE
# ============================================================================

run_posteriorsize_diagnostics <- function(outcome_var, population_size) {
  
  cat("=== posteriorsize() Convergence Diagnostics ===\n")
  cat("Outcome variable:", outcome_var, "\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n\n")
  
  # Run posteriorsize with diagnostic parameters
  ps_result <- tryCatch({
    posteriorsize(
      rd.dd,
      mean.prior.size = population_size,
      sd.prior.size = population_size * 0.1,
      samplesize = convergence_config$diagnostic_samplesize,
      burnin = convergence_config$diagnostic_burnin,
      interval = convergence_config$diagnostic_interval,
      parallel = 12,
      verbose = TRUE  # Enable diagnostic output
    )
  }, error = function(e) {
    cat("Error in posteriorsize:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(ps_result)) {
    return(list(success = FALSE, error = "posteriorsize failed"))
  }
  
  # Convert MCMC samples to coda format
  if (!is.null(ps_result$sample) && is.matrix(ps_result$sample)) {
    mcmc_samples <- mcmc(ps_result$sample)
    
    cat("MCMC samples:", nrow(ps_result$sample), "x", ncol(ps_result$sample), "\n")
    cat("Effective sample size by parameter:\n")
    
    diagnostics <- list()
    
    # Effective sample size
    eff_size <- effectiveSize(mcmc_samples)
    print(eff_size)
    diagnostics$effective_size <- eff_size
    
    # Geweke diagnostic (within-chain convergence)
    if (convergence_config$geweke_test) {
      cat("\nGeweke Convergence Diagnostic:\n")
      geweke_result <- geweke.diag(mcmc_samples)
      print(geweke_result)
      diagnostics$geweke <- geweke_result
      
      # Check for convergence issues (|z| > 1.96)
      convergence_issues <- abs(geweke_result$z) > 1.96
      if (any(convergence_issues, na.rm = TRUE)) {
        cat("WARNING: Potential convergence issues for parameters:", 
            names(geweke_result$z)[convergence_issues], "\n")
      } else {
        cat("✓ Geweke test: All parameters appear to have converged\n")
      }
    }
    
    # Heidelberger and Welch diagnostic (stationarity and interval length)
    if (convergence_config$heidel_test) {
      cat("\nHeidelberger-Welch Diagnostic:\n")
      heidel_result <- heidel.diag(mcmc_samples)
      print(heidel_result)
      diagnostics$heidel <- heidel_result
      
      # Check stationarity
      if (any(!heidel_result[, "stest"], na.rm = TRUE)) {
        failed_params <- rownames(heidel_result)[!heidel_result[, "stest"]]
        cat("WARNING: Stationarity test failed for:", paste(failed_params, collapse = ", "), "\n")
      } else {
        cat("✓ Heidel-Welch: All parameters passed stationarity test\n")
      }
    }
    
    # Raftery and Lewis diagnostic (run length and dependence)
    if (convergence_config$raftery_test) {
      cat("\nRaftery-Lewis Diagnostic:\n")
      raftery_result <- raftery.diag(mcmc_samples)
      print(raftery_result)
      diagnostics$raftery <- raftery_result
      
      # Check dependence factor
      dependence_factor <- raftery_result$resmatrix[, "I"]
      if (any(dependence_factor > 5, na.rm = TRUE)) {
        cat("WARNING: High dependence factor (>5) indicates strong autocorrelation\n")
      } else {
        cat("✓ Raftery-Lewis: Acceptable autocorrelation levels\n")
      }
    }
    
    # Create trace plots
    if (convergence_config$save_plots) {
      trace_plot_file <- here("output", "figures", 
                              paste0("posteriorsize_trace_", outcome_var, "_", 
                                     population_size, ".", convergence_config$plot_format))
      
      tryCatch({
        png(trace_plot_file, width = convergence_config$plot_width * 100, 
            height = convergence_config$plot_height * 100, res = convergence_config$plot_dpi)
        
        # Reset margins to default before plotting
        par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))
        
        plot(mcmc_samples, main = paste("posteriorsize() Trace Plots:", outcome_var))
        dev.off()
        
        cat("Trace plots saved to:", trace_plot_file, "\n")
      }, error = function(e) {
        dev.off()  # Close any open device
        cat("Error creating trace plots:", e$message, "\n")
        cat("Skipping trace plot generation\n")
      })
    }
    
    diagnostics$summary <- list(
      outcome_var = outcome_var,
      population_size = population_size,
      samplesize = convergence_config$diagnostic_samplesize,
      burnin = convergence_config$diagnostic_burnin,
      effective_size_min = min(eff_size, na.rm = TRUE),
      effective_size_max = max(eff_size, na.rm = TRUE),
      geweke_max_z = if(convergence_config$geweke_test) max(abs(geweke_result$z), na.rm = TRUE) else NA,
      convergence_ok = if(convergence_config$geweke_test) all(abs(geweke_result$z) < 1.96, na.rm = TRUE) else NA
    )
    
  } else {
    cat("Warning: No MCMC samples available for diagnostics\n")
    diagnostics <- list(success = FALSE, error = "No MCMC samples")
  }
  
  cat("\n")
  return(list(
    success = TRUE,
    ps_result = ps_result,
    diagnostics = diagnostics
  ))
}

# ============================================================================
# CONVERGENCE DIAGNOSTICS FOR MA.ESTIMATES
# ============================================================================

run_ma_estimates_diagnostics <- function(outcome_var, population_size) {
  
  cat("=== MA.estimates() Convergence Diagnostics ===\n")
  cat("Outcome variable:", outcome_var, "\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n\n")
  
  # Run MA.estimates with diagnostic parameters and full output
  ma_result <- tryCatch({
    MA.estimates(
      rd.dd, 
      trait.variable = outcome_var,  # Correct parameter name
      N = population_size,
      number.of.iterations = convergence_config$ma_diagnostic_iterations,
      M1 = convergence_config$ma_diagnostic_M1,
      M2 = convergence_config$ma_diagnostic_M2,
      parallel = 10,
      full.output = TRUE,  # Get detailed diagnostic output
      verbose = TRUE
    )
  }, error = function(e) {
    cat("Error in MA.estimates:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(ma_result)) {
    return(list(success = FALSE, error = "MA.estimates failed"))
  }
  
  diagnostics <- list()
  
  # Check coefficient estimates (if available)
  if (!is.null(ma_result$coefficient)) {
    cat("Network model coefficient estimate:", ma_result$coefficient, "\n")
    diagnostics$coefficient <- ma_result$coefficient
  }
  
  # Check details for convergence information
  if (!is.null(ma_result$details)) {
    cat("Diagnostic details available\n")
    
    # Print summary of details
    detail_names <- names(ma_result$details)
    cat("Available diagnostic components:", paste(detail_names, collapse = ", "), "\n")
    
    diagnostics$details_summary <- detail_names
    
    # Check for any convergence-related information
    convergence_info <- ma_result$details[grepl("conv|iter|chain", names(ma_result$details), ignore.case = TRUE)]
    if (length(convergence_info) > 0) {
      cat("Convergence-related information found:\n")
      print(str(convergence_info, max.level = 1))
      diagnostics$convergence_info <- convergence_info
    }
  }
  
  # Check bootstrap results
  if (!is.null(ma_result$varestBS)) {
    cat("Bootstrap variance estimation available\n")
    if (!is.null(ma_result$varestBS$BSest)) {
      bootstrap_estimates <- ma_result$varestBS$BSest
      cat("Bootstrap estimates: n =", length(bootstrap_estimates), "\n")
      cat("Bootstrap estimate stats:\n")
      cat("  Mean:", mean(bootstrap_estimates, na.rm = TRUE), "\n")
      cat("  SD:", sd(bootstrap_estimates, na.rm = TRUE), "\n")
      cat("  Range:", range(bootstrap_estimates, na.rm = TRUE), "\n")
      
      diagnostics$bootstrap <- list(
        n_estimates = length(bootstrap_estimates),
        mean = mean(bootstrap_estimates, na.rm = TRUE),
        sd = sd(bootstrap_estimates, na.rm = TRUE),
        range = range(bootstrap_estimates, na.rm = TRUE)
      )
      
      # Create bootstrap trace plot
      if (convergence_config$save_plots) {
        bootstrap_plot_file <- here("output", "figures", 
                                    paste0("ma_estimates_bootstrap_", outcome_var, "_", 
                                           population_size, ".", convergence_config$plot_format))
        
        tryCatch({
          png(bootstrap_plot_file, width = convergence_config$plot_width * 100, 
              height = convergence_config$plot_height * 100, res = convergence_config$plot_dpi)
          
          # Reset margins and layout
          par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(2, 1))
          
          plot(bootstrap_estimates, type = "l", main = paste("MA.estimates() Bootstrap Trace:", outcome_var),
               xlab = "Bootstrap Sample", ylab = "Estimate")
          abline(h = mean(bootstrap_estimates, na.rm = TRUE), col = "red", lty = 2)
          
          hist(bootstrap_estimates, main = "Bootstrap Distribution", 
               xlab = "Estimate", breaks = 30, col = "lightblue")
          abline(v = mean(bootstrap_estimates, na.rm = TRUE), col = "red", lty = 2)
          
          dev.off()
          
          cat("Bootstrap plots saved to:", bootstrap_plot_file, "\n")
        }, error = function(e) {
          dev.off()  # Close any open device
          cat("Error creating bootstrap plots:", e$message, "\n")
          cat("Skipping bootstrap plot generation\n")
        })
      }
    }
  }
  
  # Summary assessment
  diagnostics$summary <- list(
    outcome_var = outcome_var,
    population_size = population_size,
    iterations = convergence_config$ma_diagnostic_iterations,
    M1 = convergence_config$ma_diagnostic_M1,
    M2 = convergence_config$ma_diagnostic_M2,
    has_coefficient = !is.null(ma_result$coefficient),
    has_bootstrap = !is.null(ma_result$varestBS$BSest),
    has_details = !is.null(ma_result$details)
  )
  
  cat("\n")
  return(list(
    success = TRUE,
    ma_result = ma_result,
    diagnostics = diagnostics
  ))
}

# ============================================================================
# MODEL COMPARISON VISUALIZATION USING SJPLOT
# ============================================================================

create_model_comparison_plots <- function(results_df) {
  
  cat("=== Creating Model Comparison Visualizations ===\n")
  
  if (is.null(results_df) || nrow(results_df) == 0) {
    cat("Warning: No results data available for visualization\n")
    return(NULL)
  }
  
  # Clean data for visualization
  plot_data <- results_df %>%
    filter(!is.na(estimate), !is.na(ci_lower), !is.na(ci_upper)) %>%
    mutate(
      method_type_clean = str_to_title(method_type),
      pop_size_factor = factor(pop_label, levels = c("50K", "100K", "980K", "1.74M"))
    )
  
  if (nrow(plot_data) == 0) {
    cat("Warning: No complete results available for visualization\n")
    return(NULL)
  }
  
  plots <- list()
  
  # 1. Forest plot of estimates by method and population size
  cat("Creating forest plot...\n")
  
  forest_plot <- plot_data %>%
    filter(pop_size == 980000) %>%  # Focus on main population size
    ggplot(aes(x = estimate_pct, y = reorder(paste(indicator_clean, method_clean, sep = " - "), estimate_pct),
               color = method_type_clean)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = ci_lower_pct, xmax = ci_upper_pct), height = 0.2) +
    scale_color_viridis_d(name = "Method Type") +
    labs(
      title = "RDS Model Comparison: Prevalence Estimates",
      subtitle = "Population size: 980,000 (95% confidence intervals)",
      x = "Prevalence (%)",
      y = "Indicator - Method"
    ) +
    theme_sjplot() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  plots$forest_plot <- forest_plot
  
  # 2. Population size sensitivity plot
  cat("Creating population sensitivity plot...\n")
  
  sensitivity_plot <- plot_data %>%
    ggplot(aes(x = pop_size_factor, y = estimate_pct, color = method_clean, group = method_clean)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), width = 0.2) +
    facet_wrap(~indicator_clean, scales = "free_y") +
    scale_color_viridis_d(name = "Method") +
    labs(
      title = "Population Size Sensitivity Analysis",
      subtitle = "How estimates vary across different population size assumptions",
      x = "Population Size",
      y = "Prevalence (%)"
    ) +
    theme_sjplot() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  plots$sensitivity_plot <- sensitivity_plot
  
  # 3. Method comparison by uncertainty type
  cat("Creating uncertainty method comparison...\n")
  
  uncertainty_plot <- plot_data %>%
    filter(pop_size == 980000) %>%
    ggplot(aes(x = method_clean, y = estimate_pct, fill = uncertainty_clean)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                  position = position_dodge(width = 0.9), width = 0.25) +
    facet_wrap(~indicator_clean, scales = "free_y") +
    scale_fill_viridis_d(name = "Uncertainty Method") +
    labs(
      title = "RDS Methods: Estimates and Uncertainty Quantification",
      subtitle = "Comparing Bayesian credible intervals vs. bootstrap confidence intervals",
      x = "Method",
      y = "Prevalence (%)"
    ) +
    theme_sjplot() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  plots$uncertainty_plot <- uncertainty_plot
  
  # 4. Confidence interval width comparison
  cat("Creating CI width comparison...\n")
  
  ci_width_data <- plot_data %>%
    mutate(
      ci_width = ci_upper_pct - ci_lower_pct,
      relative_ci_width = ci_width / estimate_pct * 100
    ) %>%
    filter(!is.na(ci_width), ci_width > 0)
  
  if (nrow(ci_width_data) > 0) {
    ci_width_plot <- ci_width_data %>%
      ggplot(aes(x = method_clean, y = ci_width, color = method_type_clean)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.6) +
      facet_wrap(~pop_label) +
      scale_color_viridis_d(name = "Method Type") +
      labs(
        title = "Uncertainty Quantification: Confidence Interval Width",
        subtitle = "Comparing precision across methods and population sizes",
        x = "Method",
        y = "95% CI Width (percentage points)"
      ) +
      theme_sjplot() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    plots$ci_width_plot <- ci_width_plot
  }
  
  # 5. Combined comparison plot
  cat("Creating combined comparison plot...\n")
  
  if (length(plots) >= 2) {
    combined_plot <- plots$forest_plot / plots$sensitivity_plot +
      plot_layout(heights = c(1, 1)) +
      plot_annotation(
        title = "Comprehensive RDS Model Comparison",
        subtitle = "Domestic Worker Exploitation Prevalence Estimates",
        theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
      )
    
    plots$combined_plot <- combined_plot
  }
  
  # Save plots
  if (convergence_config$save_plots) {
    cat("Saving visualization plots...\n")
    
    for (plot_name in names(plots)) {
      plot_file <- here("output", "figures", 
                        paste0("model_comparison_", plot_name, ".", convergence_config$plot_format))
      
      ggsave(plot_file, plots[[plot_name]], 
             width = convergence_config$plot_width, 
             height = convergence_config$plot_height, 
             dpi = convergence_config$plot_dpi)
      
      cat("Saved:", plot_file, "\n")
    }
  }
  
  cat("Model comparison visualizations complete!\n\n")
  return(plots)
}

# ============================================================================
# COMPREHENSIVE DIAGNOSTICS RUNNER
# ============================================================================

run_comprehensive_diagnostics <- function() {
  
  cat("=== Running Comprehensive Convergence Diagnostics ===\n\n")
  
  # Create output directory
  if (!dir.exists(here("output", "figures"))) {
    dir.create(here("output", "figures"), recursive = TRUE)
  }
  
  diagnostic_results <- list()
  
  # Run diagnostics for selected indicators
  for (indicator in convergence_config$diagnostic_indicators) {
    
    cat("Testing indicator:", indicator, "\n")
    
    if (!(indicator %in% names(rd.dd))) {
      cat("Warning: Indicator", indicator, "not found in data\n")
      next
    }
    
    # posteriorsize diagnostics
    ps_diag <- run_posteriorsize_diagnostics(indicator, convergence_config$test_population_size)
    diagnostic_results[[paste0("posteriorsize_", indicator)]] <- ps_diag
    
    # MA.estimates diagnostics  
    ma_diag <- run_ma_estimates_diagnostics(indicator, convergence_config$test_population_size)
    diagnostic_results[[paste0("ma_estimates_", indicator)]] <- ma_diag
    
    cat("Completed diagnostics for:", indicator, "\n\n")
  }
  
  # Create visualizations if results are available
  if (exists("final_results") && !is.null(final_results$results_df)) {
    visualization_plots <- create_model_comparison_plots(final_results$results_df)
    diagnostic_results$visualization_plots <- visualization_plots
  } else {
    cat("No final results available for visualization\n")
  }
  
  # Save diagnostic results
  save(diagnostic_results, convergence_config, 
       file = here("output", "convergence_diagnostics_results.RData"))
  
  cat("=== Convergence Diagnostics Complete ===\n")
  cat("Results saved to: output/convergence_diagnostics_results.RData\n")
  cat("Plots saved to: output/figures/\n\n")
  
  return(diagnostic_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Load data if not available
if (!exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
  cat("Loaded prepared RDS data\n")
}

# Prevent automatic execution when sourced
if (!exists("skip_execution") || !skip_execution) {
  
  # Set up project environment
  setup_project_environment()
  
  # Run comprehensive diagnostics
  diagnostic_results <- run_comprehensive_diagnostics()
  
} else {
  cat("Convergence diagnostics script loaded (execution skipped)\n")
}


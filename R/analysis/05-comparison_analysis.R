# 05-comparison_analysis.R
# Systematic comparison of RDS vs NSUM estimation methods
# Using CE's comparable indicator specifications

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(here)

# Load estimation results
load_estimation_results <- function() {
  
  results <- list()
  
  # Load RDS results
  if (file.exists(here("output", "rds_estimation_results.RData"))) {
    load(here("output", "rds_estimation_results.RData"))
    results$rds <- results  # The loaded object is named 'results'
  } else {
    cat("Warning: RDS results not found. Run 03-rds_estimation.R first.\n")
    results$rds <- NULL
  }
  
  # Load NSUM results  
  if (file.exists(here("output", "nsum_estimation_results.RData"))) {
    load(here("output", "nsum_estimation_results.RData"))
    results$nsum <- final_results  # The loaded object is named 'final_results'
  } else {
    cat("Warning: NSUM results not found. Run 04-nsum_estimation.R first.\n")
    results$nsum <- NULL
  }
  
  return(results)
}

# Extract comparable estimates for direct comparison
extract_comparable_estimates <- function(rds_results, nsum_results) {
  
  comparable_estimates <- list()
  
  # CE's comparable indicator pairs (most to least confident)
  indicator_pairs <- list(
    document_withholding = list(
      rds_var = "document_withholding_rds",
      nsum_var = "document_withholding",
      confidence = "highest",
      description = "Document withholding (Q70 vs Q71)"
    ),
    pay_issues = list(
      rds_var = "pay_issues_rds", 
      nsum_var = "pay_issues",
      confidence = "high",
      description = "Pay/debt issues (Q39+Q42 vs Q43)"
    ),
    threats_abuse = list(
      rds_var = "threats_abuse_rds",
      nsum_var = "threats_abuse", 
      confidence = "high",
      description = "Threats/abuse/force (Q45+Q47+Q48 vs Q49)"
    ),
    excessive_hours = list(
      rds_var = "excessive_hours_rds",
      nsum_var = "excessive_hours",
      confidence = "medium", 
      description = "Excessive hours (Q61+Q62 vs Q64)"
    ),
    access_to_help = list(
      rds_var = "access_to_help_rds",
      nsum_var = "access_to_help",
      confidence = "lowest",
      description = "Access to help (Q78 vs Q79)"
    )
  )
  
  # Population sizes to compare
  pop_sizes <- c("50000", "100000", "980000", "1740000")
  
  comparison_data <- list()
  
  for (indicator in names(indicator_pairs)) {
    pair_info <- indicator_pairs[[indicator]]
    
    for (pop_size in pop_sizes) {
      pop_key <- paste0("N_", pop_size)
      
      # Extract RDS estimates
      rds_estimates <- NULL
      if (!is.null(rds_results) && 
          !is.null(rds_results$rds_estimates[[pair_info$rds_var]]) &&
          !is.null(rds_results$rds_estimates[[pair_info$rds_var]][[pop_key]])) {
        
        rds_est_list <- rds_results$rds_estimates[[pair_info$rds_var]][[pop_key]]
        rds_estimates <- data.frame(
          method = paste0("RDS_", names(rds_est_list)),
          estimate = sapply(rds_est_list, function(x) ifelse(is.null(x$estimate), NA, x$estimate)),
          std_error = sapply(rds_est_list, function(x) ifelse(is.null(x$std.error), NA, x$std.error)),
          ci_lower = sapply(rds_est_list, function(x) {
            if (!is.null(x$confidence.interval) && length(x$confidence.interval) >= 2) {
              return(x$confidence.interval[1])
            } else {
              return(NA)
            }
          }),
          ci_upper = sapply(rds_est_list, function(x) {
            if (!is.null(x$confidence.interval) && length(x$confidence.interval) >= 2) {
              return(x$confidence.interval[2])  
            } else {
              return(NA)
            }
          }),
          stringsAsFactors = FALSE
        )
      }
      
      # Extract NSUM estimates
      nsum_estimates <- NULL
      if (!is.null(nsum_results) &&
          !is.null(nsum_results$estimates[[pair_info$nsum_var]]) &&
          !is.null(nsum_results$estimates[[pair_info$nsum_var]][[pop_key]])) {
        
        nsum_est_list <- nsum_results$estimates[[pair_info$nsum_var]][[pop_key]]
        
        nsum_rows <- list()
        for (weight_method in names(nsum_est_list)) {
          for (degree_method in names(nsum_est_list[[weight_method]])) {
            est_obj <- nsum_est_list[[weight_method]][[degree_method]]
            if (!is.null(est_obj$estimate) && !is.na(est_obj$estimate)) {
              nsum_rows <- append(nsum_rows, list(data.frame(
                method = paste0("NSUM_", weight_method, "_", degree_method),
                estimate = est_obj$estimate,
                std_error = NA,  # NSUM doesn't provide standard errors in this implementation
                ci_lower = NA,
                ci_upper = NA,
                visibility = ifelse(!is.null(est_obj$visibility), est_obj$visibility, NA),
                stringsAsFactors = FALSE
              )))
            }
          }
        }
        
        if (length(nsum_rows) > 0) {
          nsum_estimates <- do.call(rbind, nsum_rows)
        }
      }
      
      # Combine RDS and NSUM estimates
      if (!is.null(rds_estimates) || !is.null(nsum_estimates)) {
        combined_estimates <- data.frame()
        
        if (!is.null(rds_estimates)) {
          rds_estimates$approach <- "RDS"
          combined_estimates <- rbind(combined_estimates, rds_estimates[, names(combined_estimates) %in% names(rds_estimates) | ncol(combined_estimates) == 0])
        }
        
        if (!is.null(nsum_estimates)) {
          nsum_estimates$approach <- "NSUM"
          # Ensure column compatibility
          missing_cols <- setdiff(names(combined_estimates), names(nsum_estimates))
          for (col in missing_cols) {
            nsum_estimates[[col]] <- NA
          }
          combined_estimates <- rbind(combined_estimates, nsum_estimates[, names(combined_estimates)])
        }
        
        if (nrow(combined_estimates) > 0) {
          combined_estimates$indicator <- indicator
          combined_estimates$population_size <- pop_size
          combined_estimates$confidence_level <- pair_info$confidence
          combined_estimates$description <- pair_info$description
          
          comparison_data <- append(comparison_data, list(combined_estimates))
        }
      }
    }
  }
  
  if (length(comparison_data) > 0) {
    return(do.call(rbind, comparison_data))
  } else {
    return(data.frame())
  }
}

# Create comprehensive comparison visualizations
create_comparison_plots <- function(comparison_data) {
  
  plots <- list()
  
  if (nrow(comparison_data) == 0) {
    cat("No comparison data available for plotting\n")
    return(plots)
  }
  
  # Plot 1: Estimates by indicator and approach
  plots$estimates_by_indicator <- ggplot(comparison_data, 
                                        aes(x = indicator, y = estimate, 
                                            color = approach, shape = approach)) +
    geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.2)) +
    facet_wrap(~ population_size, scales = "free_y", 
               labeller = labeller(population_size = function(x) paste("N =", x))) +
    scale_y_log10() +
    theme_minimal() +
    labs(title = "RDS vs NSUM Population Estimates by Indicator",
         subtitle = "Comparison using CE's matched indicators",
         x = "Exploitation Indicator", 
         y = "Population Estimate (log scale)",
         color = "Method", shape = "Method") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  # Plot 2: Confidence level comparison
  if ("confidence_level" %in% names(comparison_data)) {
    plots$estimates_by_confidence <- ggplot(comparison_data,
                                          aes(x = confidence_level, y = estimate, 
                                              color = approach)) +
      geom_boxplot(alpha = 0.7) +
      scale_y_log10() +
      facet_wrap(~ population_size, 
                 labeller = labeller(population_size = function(x) paste("N =", x))) +
      theme_minimal() +
      labs(title = "Estimate Reliability by Confidence Level",
           subtitle = "Higher confidence = more reliable indicator matching",
           x = "CE's Confidence Level in Indicator Matching", 
           y = "Population Estimate (log scale)",
           color = "Method") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Plot 3: Method-specific comparison (if confidence intervals available)
  if ("ci_lower" %in% names(comparison_data) && any(!is.na(comparison_data$ci_lower))) {
    plots$confidence_intervals <- comparison_data %>%
      filter(!is.na(ci_lower), !is.na(ci_upper)) %>%
      ggplot(aes(x = indicator, y = estimate, color = approach)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      facet_wrap(~ population_size, scales = "free_y") +
      theme_minimal() +
      labs(title = "Population Estimates with Confidence Intervals",
           x = "Indicator", y = "Estimate", color = "Method") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Plot 4: Ratio comparison (RDS/NSUM ratios)
  ratio_data <- comparison_data %>%
    select(indicator, population_size, approach, estimate, confidence_level) %>%
    filter(!is.na(estimate), estimate > 0) %>%
    group_by(indicator, population_size, confidence_level) %>%
    summarise(
      rds_median = median(estimate[approach == "RDS"], na.rm = TRUE),
      nsum_median = median(estimate[approach == "NSUM"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ratio = rds_median / nsum_median,
      log_ratio = log10(ratio)
    ) %>%
    filter(is.finite(ratio))
  
  if (nrow(ratio_data) > 0) {
    plots$method_ratios <- ggplot(ratio_data, 
                                 aes(x = indicator, y = log_ratio, 
                                     fill = confidence_level)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      facet_wrap(~ population_size, 
                 labeller = labeller(population_size = function(x) paste("N =", x))) +
      theme_minimal() +
      labs(title = "RDS/NSUM Ratio by Indicator",
           subtitle = "Log ratio: 0 = equal estimates, >0 = RDS higher, <0 = NSUM higher",
           x = "Indicator", y = "Log10(RDS/NSUM Ratio)", 
           fill = "Confidence Level") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  return(plots)
}

# Create summary statistics table
create_comparison_summary <- function(comparison_data) {
  
  if (nrow(comparison_data) == 0) {
    return(data.frame())
  }
  
  summary_stats <- comparison_data %>%
    group_by(indicator, population_size, approach, confidence_level) %>%
    summarise(
      n_estimates = n(),
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      min_estimate = min(estimate, na.rm = TRUE),
      max_estimate = max(estimate, na.rm = TRUE),
      cv = sd_estimate / mean_estimate,
      .groups = "drop"
    ) %>%
    arrange(confidence_level, indicator, population_size, approach)
  
  return(summary_stats)
}

# Main comparison analysis function
run_comparison_analysis <- function() {
  
  cat("Loading estimation results...\n")
  results <- load_estimation_results()
  
  if (is.null(results$rds) && is.null(results$nsum)) {
    cat("Error: No estimation results found. Please run RDS and NSUM estimation scripts first.\n")
    return(NULL)
  }
  
  cat("Extracting comparable estimates...\n") 
  comparison_data <- extract_comparable_estimates(results$rds, results$nsum)
  
  if (nrow(comparison_data) == 0) {
    cat("Warning: No comparable estimates found for comparison.\n")
    return(list(comparison_data = comparison_data))
  }
  
  cat("Creating comparison visualizations...\n")
  plots <- create_comparison_plots(comparison_data)
  
  cat("Computing summary statistics...\n")
  summary_stats <- create_comparison_summary(comparison_data)
  
  # Compile final results
  final_comparison <- list(
    comparison_data = comparison_data,
    summary_statistics = summary_stats,
    plots = plots,
    metadata = list(
      run_date = Sys.time(),
      n_comparisons = nrow(comparison_data),
      indicators_compared = unique(comparison_data$indicator),
      population_sizes = unique(comparison_data$population_size),
      methods_compared = unique(comparison_data$approach),
      r_version = R.version.string
    )
  )
  
  # Save results
  cat("Saving comparison analysis results...\n")
  save(final_comparison, file = here("output", "comparison_analysis_results.RData"))
  
  # Save summary table
  write.csv(summary_stats, here("output", "tables", "rds_nsum_comparison_summary.csv"), row.names = FALSE)
  
  # Save detailed comparison data
  write.csv(comparison_data, here("output", "tables", "rds_nsum_comparison_detailed.csv"), row.names = FALSE)
  
  # Save plots
  for (plot_name in names(plots)) {
    ggsave(here("output", "figures", paste0("comparison_", plot_name, ".png")), 
           plots[[plot_name]], width = 12, height = 8, dpi = 300)
  }
  
  cat("Comparison analysis completed successfully!\n")
  cat("- Results saved to: output/comparison_analysis_results.RData\n")
  cat("- Summary table: output/tables/rds_nsum_comparison_summary.csv\n")
  cat("- Detailed data: output/tables/rds_nsum_comparison_detailed.csv\n")
  cat("- Plots: output/figures/comparison_*.png\n")
  cat("- Indicators compared:", length(unique(comparison_data$indicator)), "\n")
  cat("- Total comparisons:", nrow(comparison_data), "\n")
  
  return(final_comparison)
}

# Execute comparison analysis if running this script directly
if (!exists("skip_execution")) {
  comparison_results <- run_comparison_analysis()
}
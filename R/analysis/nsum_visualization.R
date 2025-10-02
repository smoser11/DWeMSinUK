# nsum_visualization.R
# Visualization and Table Generation for NSUM Results
# Creates publication-ready tables and plots for NSUM estimates
#
# Handles both basic NSUM and robust NSUM with adjustment factors

cat("=== NSUM Visualization and Display ===\n")
cat("Tables and plots for NSUM results\n\n")

# Load required libraries
library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(viridis)

# Optional: gt for publication tables
gt_available <- requireNamespace("gt", quietly = TRUE)
if (!gt_available) {
  cat("NOTE: 'gt' package not available for publication tables\n")
  cat("Install with: install.packages('gt')\n")
  cat("Falling back to data frames\n\n")
}

# Source required files
source(here("R", "analysis", "05-nsum_estimation.R"))
source(here("R", "analysis", "nsum_robust_adjustment.R"))
source(here("R", "analysis", "nsum_bootstrap.R"))

# ============================================================================
# SUMMARY TABLES
# ============================================================================

create_nsum_summary_table <- function(results_df,
                                      include_ci = TRUE,
                                      preferred_n_f = 980000,
                                      preferred_schemes = c("ss_980k", "unweighted")) {

  cat("Creating NSUM summary table...\n")

  # Filter to valid results
  valid_results <- results_df %>% filter(is.na(error))

  if (nrow(valid_results) == 0) {
    cat("No valid results for summary table\n")
    return(NULL)
  }

  # Focus on key scenarios
  main_results <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme %in% preferred_schemes,
      degree_ratio == 1.0,
      true_positive_rate == 1.0,
      precision == 1.0
    )

  if (nrow(main_results) == 0) {
    cat("No results matching filter criteria\n")
    return(valid_results)
  }

  # Format estimates
  summary_table <- main_results %>%
    mutate(
      estimate_formatted = format(round(adjusted_estimate), big.mark = ","),
      prevalence_pct = round(nsum_prevalence * 100, 2),
      rds_formatted = if_else(!is.na(rds_estimate),
                             format(round(rds_estimate), big.mark = ","),
                             "NA"),
      ratio_formatted = if_else(!is.na(nsum_rds_ratio),
                               round(nsum_rds_ratio, 2),
                               NA_real_)
    ) %>%
    select(indicator_name, scheme, estimate_formatted, prevalence_pct,
           rds_formatted, ratio_formatted, sample_size)

  # Create gt table if available
  if (gt_available && requireNamespace("gt", quietly = TRUE)) {
    tryCatch({
      summary_table <- summary_table %>%
        gt::gt() %>%
        gt::tab_header(
          title = "NSUM Population Estimates",
          subtitle = paste("Population:", format(preferred_n_f, big.mark = ","))
        ) %>%
        gt::cols_label(
          indicator_name = "Indicator",
          scheme = "Weight Scheme",
          estimate_formatted = "N_H Estimate",
          prevalence_pct = "Prevalence (%)",
          rds_formatted = "RDS Estimate",
          ratio_formatted = "NSUM/RDS Ratio",
          sample_size = "Sample Size"
        ) %>%
        gt::tab_style(
          style = list(gt::cell_text(weight = "bold")),
          locations = gt::cells_column_labels()
        )
    }, error = function(e) {
      cat("GT table creation failed, returning data frame\n")
    })
  }

  return(summary_table)
}

create_robust_sensitivity_table <- function(results_df,
                                            focus_indicator = "document_withholding",
                                            preferred_n_f = 980000,
                                            preferred_scheme = "ss_980k") {

  cat("Creating sensitivity analysis table...\n")

  valid_results <- results_df %>% filter(is.na(error))

  if (nrow(valid_results) == 0) {
    return(NULL)
  }

  # Focus on single indicator for sensitivity
  sens_data <- valid_results %>%
    filter(
      indicator_name == focus_indicator,
      N_F == preferred_n_f,
      scheme == preferred_scheme
    ) %>%
    mutate(
      adjustment_scenario = paste0(
        "δ=", degree_ratio, ", τ=", true_positive_rate, ", η=", precision
      ),
      estimate_formatted = format(round(adjusted_estimate), big.mark = ","),
      impact = round(adjustment_impact, 2)
    ) %>%
    select(adjustment_scenario, estimate_formatted, nsum_prevalence, impact) %>%
    arrange(desc(nsum_prevalence))

  if (gt_available && requireNamespace("gt", quietly = TRUE)) {
    tryCatch({
      sens_data <- sens_data %>%
        gt::gt() %>%
        gt::tab_header(
          title = paste("Sensitivity Analysis:", focus_indicator),
          subtitle = "Impact of adjustment factors"
        ) %>%
        gt::cols_label(
          adjustment_scenario = "Adjustment Factors",
          estimate_formatted = "Adjusted N_H",
          nsum_prevalence = "Prevalence",
          impact = "Adj. Impact"
        ) %>%
        gt::fmt_percent(
          columns = nsum_prevalence,
          decimals = 2
        )
    }, error = function(e) {
      cat("GT table creation failed, returning data frame\n")
    })
  }

  return(sens_data)
}

create_rds_nsum_comparison_table <- function(nsum_results_df,
                                             rds_results_file = NULL) {

  cat("Creating RDS vs NSUM comparison table...\n")

  # Load RDS results
  if (is.null(rds_results_file)) {
    rds_results_file <- here("output", "rds_estimation_results.RData")
  }

  if (!file.exists(rds_results_file)) {
    warning("RDS results not found at: ", rds_results_file)
    return(NULL)
  }

  load(rds_results_file)

  if (!exists("final_results")) {
    warning("final_results object not found in RDS file")
    return(NULL)
  }

  # Extract RDS estimates
  rds_table <- final_results$preferred_results$main_table

  # Match with NSUM results
  valid_nsum <- nsum_results_df %>%
    filter(is.na(error), degree_ratio == 1.0,
           true_positive_rate == 1.0, precision == 1.0)

  comparison <- left_join(
    rds_table %>%
      select(indicator, estimate, indicator_clean, confidence) %>%
      rename(rds_estimate = estimate),
    valid_nsum %>%
      select(rds_indicator, nsum_prevalence, adjusted_estimate) %>%
      rename(indicator = rds_indicator, nsum_estimate = nsum_prevalence),
    by = "indicator"
  ) %>%
    filter(!is.na(nsum_estimate)) %>%
    mutate(
      difference = nsum_estimate - rds_estimate,
      ratio = nsum_estimate / rds_estimate,
      rds_pct = rds_estimate * 100,
      nsum_pct = nsum_estimate * 100,
      diff_pct = difference * 100
    ) %>%
    arrange(desc(rds_estimate))

  cat("Comparison created for", nrow(comparison), "indicators\n")

  return(comparison)
}

# ============================================================================
# VISUALIZATIONS
# ============================================================================

plot_nsum_estimates <- function(results_df,
                                preferred_n_f = 980000,
                                preferred_scheme = "ss_980k",
                                include_ci = FALSE) {

  valid_results <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme == preferred_scheme,
      degree_ratio == 1.0,
      true_positive_rate == 1.0,
      precision == 1.0
    )

  if (nrow(valid_results) == 0) {
    warning("No valid results for plotting")
    return(NULL)
  }

  plot_data <- valid_results %>%
    mutate(
      indicator_label = factor(indicator_name, levels = unique(indicator_name))
    )

  p <- ggplot(plot_data, aes(x = indicator_label, y = adjusted_estimate)) +
    geom_col(fill = "#6A3D9A", alpha = 0.8) +
    geom_text(aes(label = format(round(adjusted_estimate), big.mark = ",")),
             vjust = -0.5, size = 3) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "NSUM Population Estimates by Indicator",
      subtitle = paste("Population:", format(preferred_n_f, big.mark = ",")),
      x = "Exploitation Indicator",
      y = "Estimated Hidden Population Size"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )

  return(p)
}

plot_rds_nsum_comparison <- function(results_df,
                                     preferred_n_f = 980000,
                                     preferred_scheme = "ss_980k") {

  valid_results <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme == preferred_scheme,
      degree_ratio == 1.0,
      true_positive_rate == 1.0,
      precision == 1.0,
      !is.na(rds_estimate)
    )

  if (nrow(valid_results) == 0) {
    warning("No valid results with RDS comparison")
    return(NULL)
  }

  plot_data <- valid_results %>%
    select(indicator_name, adjusted_estimate, rds_estimate) %>%
    pivot_longer(cols = c(adjusted_estimate, rds_estimate),
                names_to = "method",
                values_to = "estimate") %>%
    mutate(
      method = if_else(method == "adjusted_estimate", "NSUM", "RDS"),
      indicator_label = factor(indicator_name, levels = unique(indicator_name))
    )

  p <- ggplot(plot_data, aes(x = indicator_label, y = estimate, fill = method)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    scale_fill_manual(values = c("NSUM" = "#6A3D9A", "RDS" = "#33A02C")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "NSUM vs RDS Estimates Comparison",
      subtitle = paste("Population:", format(preferred_n_f, big.mark = ",")),
      x = "Indicator",
      y = "Estimated Count",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 14)
    )

  return(p)
}

plot_sensitivity_analysis <- function(results_df,
                                     focus_indicator = "document_withholding",
                                     preferred_n_f = 980000,
                                     preferred_scheme = "ss_980k") {

  sens_data <- results_df %>%
    filter(
      is.na(error),
      indicator_name == focus_indicator,
      N_F == preferred_n_f,
      scheme == preferred_scheme
    )

  if (nrow(sens_data) == 0) {
    warning("No data for sensitivity plot")
    return(NULL)
  }

  p <- ggplot(sens_data, aes(x = degree_ratio, y = adjusted_estimate,
                             color = as.factor(true_positive_rate),
                             group = true_positive_rate)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    scale_color_viridis_d(name = "True Positive Rate") +
    labs(
      title = paste("Sensitivity Analysis:", focus_indicator),
      subtitle = "Effect of degree ratio and reporting accuracy",
      x = "Degree Ratio (δ)",
      y = "Adjusted NSUM Estimate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  return(p)
}

plot_nsum_with_ci <- function(bootstrap_results_df) {

  if (is.null(bootstrap_results_df) || nrow(bootstrap_results_df) == 0) {
    warning("No bootstrap results for CI plot")
    return(NULL)
  }

  plot_data <- bootstrap_results_df %>%
    filter(!is.na(estimate), !is.na(ci_lower), !is.na(ci_upper)) %>%
    mutate(indicator_label = factor(indicator_name, levels = unique(indicator_name)))

  if (nrow(plot_data) == 0) {
    warning("No valid CIs for plotting")
    return(NULL)
  }

  p <- ggplot(plot_data, aes(x = indicator_label, y = estimate)) +
    geom_point(size = 3, color = "#6A3D9A") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                 width = 0.2, color = "#6A3D9A", alpha = 0.7) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "NSUM Estimates with Bootstrap Confidence Intervals",
      subtitle = paste0("95% CI from ", unique(plot_data$n_boot_valid)[1], " bootstrap samples"),
      x = "Indicator",
      y = "Estimated Hidden Population"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )

  return(p)
}

# ============================================================================
# MASTER VISUALIZATION FUNCTION
# ============================================================================

create_nsum_visualizations <- function(nsum_results = NULL,
                                      robust_results = NULL,
                                      bootstrap_results = NULL,
                                      save_plots = TRUE,
                                      save_tables = TRUE) {

  cat("=== Creating NSUM Visualizations ===\n\n")

  output_list <- list()

  # Summary table
  if (!is.null(robust_results)) {
    cat("Creating summary table...\n")
    summary_table <- create_nsum_summary_table(robust_results$valid_results)
    output_list$summary_table <- summary_table

    if (save_tables && !is.null(summary_table)) {
      if (gt_available && inherits(summary_table, "gt_tbl")) {
        gt::gtsave(summary_table,
                  here("output", "tables", "nsum_summary_table.html"))
        cat("Saved: output/tables/nsum_summary_table.html\n")
      } else {
        write_csv(summary_table,
                 here("output", "tables", "nsum_summary_table.csv"))
        cat("Saved: output/tables/nsum_summary_table.csv\n")
      }
    }
  }

  # Sensitivity table
  if (!is.null(robust_results)) {
    cat("Creating sensitivity table...\n")
    sensitivity_table <- create_robust_sensitivity_table(robust_results$valid_results)
    output_list$sensitivity_table <- sensitivity_table

    if (save_tables && !is.null(sensitivity_table)) {
      if (gt_available && inherits(sensitivity_table, "gt_tbl")) {
        gt::gtsave(sensitivity_table,
                  here("output", "tables", "nsum_sensitivity_table.html"))
      } else {
        write_csv(sensitivity_table,
                 here("output", "tables", "nsum_sensitivity_table.csv"))
      }
      cat("Saved: output/tables/nsum_sensitivity_table\n")
    }
  }

  # Comparison table
  cat("Creating RDS vs NSUM comparison table...\n")
  comparison_table <- create_rds_nsum_comparison_table(
    if (!is.null(robust_results)) robust_results$valid_results else NULL
  )
  output_list$comparison_table <- comparison_table

  if (save_tables && !is.null(comparison_table)) {
    write_csv(comparison_table,
             here("output", "tables", "rds_nsum_comparison_detailed.csv"))
    cat("Saved: output/tables/rds_nsum_comparison_detailed.csv\n")
  }

  # Plots
  if (!is.null(robust_results)) {
    cat("\nCreating plots...\n")

    # NSUM estimates plot
    p1 <- plot_nsum_estimates(robust_results$valid_results)
    output_list$nsum_estimates_plot <- p1

    # RDS vs NSUM comparison plot
    p2 <- plot_rds_nsum_comparison(robust_results$valid_results)
    output_list$comparison_plot <- p2

    # Sensitivity analysis plot
    p3 <- plot_sensitivity_analysis(robust_results$valid_results)
    output_list$sensitivity_plot <- p3

    if (save_plots) {
      if (!is.null(p1)) {
        ggsave(here("output", "figures", "nsum_estimates.png"),
              p1, width = 10, height = 6, dpi = 300)
        cat("Saved: output/figures/nsum_estimates.png\n")
      }
      if (!is.null(p2)) {
        ggsave(here("output", "figures", "nsum_rds_comparison.png"),
              p2, width = 12, height = 6, dpi = 300)
        cat("Saved: output/figures/nsum_rds_comparison.png\n")
      }
      if (!is.null(p3)) {
        ggsave(here("output", "figures", "nsum_sensitivity.png"),
              p3, width = 10, height = 6, dpi = 300)
        cat("Saved: output/figures/nsum_sensitivity.png\n")
      }
    }
  }

  # Bootstrap CI plot
  if (!is.null(bootstrap_results)) {
    cat("Creating bootstrap CI plot...\n")
    p4 <- plot_nsum_with_ci(bootstrap_results$results_df)
    output_list$bootstrap_ci_plot <- p4

    if (save_plots && !is.null(p4)) {
      ggsave(here("output", "figures", "nsum_bootstrap_ci.png"),
            p4, width = 10, height = 6, dpi = 300)
      cat("Saved: output/figures/nsum_bootstrap_ci.png\n")
    }
  }

  cat("\n=== Visualization Complete ===\n")

  return(output_list)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("\n=== NSUM Visualization Functions Loaded ===\n")
cat("To create all visualizations:\n")
cat("viz <- create_nsum_visualizations(\n")
cat("  robust_results = robust_results,\n")
cat("  bootstrap_results = bootstrap_results\n")
cat(")\n\n")

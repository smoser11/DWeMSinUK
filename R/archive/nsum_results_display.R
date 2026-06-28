# Robust NSUM Results Display and Visualization
# Using proper survey bootstrap with inclusion probabilities
# Following Feehan & Salganik (2016) and Rust & Rao rescaled bootstrap

library(tidyverse)
library(here)
library(gt)
library(ggplot2)
library(scales)
library(viridis)
library(surveybootstrap)

# Source the robust NSUM functions (prefer .R, fallback to .r)
adj_R <- here("R", "analysis", "nsum_adjustment_factors.R")
adj_r <- here("R", "analysis", "nsum_adjustment_factors.r")
if (file.exists(adj_R)) {
  source(adj_R)
} else if (file.exists(adj_r)) {
  source(adj_r)
} else {
  stop("nsum_adjustment_factors file not found (.R or .r)")
}

# ============================================================================
# ROBUST SURVEY BOOTSTRAP FOR NSUM CONFIDENCE INTERVALS
# ============================================================================

calculate_robust_nsum_ci <- function(data, rds_var, nsum_var, degree_var,
                                     weight_var, N_F,
                                     degree_ratio = 1.0,
                                     true_positive_rate = 1.0, 
                                     precision = 1.0,
                                     n_boot = 1000) {
  
  cat("Survey bootstrap CI for", rds_var, "...\n")
  
  # Use the robust bootstrap function from adjustment factors file
  ci_result <- robust_nsum_bootstrap(
    data = data,
    rds_var = rds_var,
    nsum_var = nsum_var,
    degree_var = degree_var,
    weight_var = weight_var,
    N_F = N_F,
    degree_ratio = degree_ratio,
    true_positive_rate = true_positive_rate,
    precision = precision,
    n_boot = n_boot
  )
  
  return(ci_result)
}

# ============================================================================
# ROBUST RESULTS PROCESSING WITH SURVEY BOOTSTRAP CIS  
# ============================================================================

process_robust_results_with_ci <- function(results_df, data, n_boot = 500) {

  cat("Processing robust results with survey bootstrap CIs...\n")
  
  # Debug data structure first
  valid_results <- results_df %>% filter(is.na(error))
  
  if (nrow(valid_results) == 0) {
    cat("No valid results for CI calculation\n")
    return(results_df)
  }
  
  cat("Valid results available:", nrow(valid_results), "\n")
  
  # Select key scenarios for CI calculation (focus on main results)
  available_n_f <- unique(valid_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(valid_results$scheme)
  weight_schemes <- available_schemes[1:min(3, length(available_schemes))]  # Top 3 schemes
  
  available_degree_ratios <- unique(valid_results$degree_ratio)
  preferred_degree_ratio <- if (1.0 %in% available_degree_ratios) 1.0 else available_degree_ratios[1]
  
  available_tpr <- unique(valid_results$true_positive_rate)
  preferred_tpr <- available_tpr[c(1, length(available_tpr))]  # First and last TPR values
  
  cat("CI calculation criteria:\n")
  cat("- N_F:", preferred_n_f, "\n")
  cat("- Schemes:", paste(weight_schemes, collapse = ", "), "\n")
  cat("- Degree ratio:", preferred_degree_ratio, "\n")
  cat("- True positive rates:", paste(preferred_tpr, collapse = ", "), "\n")
  
  # Select key scenarios
  key_scenarios <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme %in% weight_schemes,
      degree_ratio == preferred_degree_ratio,
      true_positive_rate %in% preferred_tpr
    )
  
  cat("Calculating CIs for", nrow(key_scenarios), "key scenarios...\n")
  
  if (nrow(key_scenarios) == 0) {
    cat("No scenarios match CI criteria\n")
    return(results_df) 
  }
  
  # Calculate confidence intervals using survey bootstrap
  ci_results <- map_dfr(1:nrow(key_scenarios), function(i) {
    scenario <- key_scenarios[i, ]
    
    cat("CI scenario", i, "of", nrow(key_scenarios), ": ", scenario$indicator_name, 
        ", scheme:", scenario$scheme, "\n")
    
    # Get corresponding weight info
    scheme_info <- robust_nsum_config$rds_weights[[scenario$scheme]]
    weight_var <- scheme_info$weight_var
    
    # Calculate CI using survey bootstrap
    ci_result <- tryCatch({
      calculate_robust_nsum_ci(
        data = data,
        rds_var = scenario$indicator,
        nsum_var = str_replace(scenario$indicator, "_rds$", "_nsum"),
        degree_var = "known_network_size",
        weight_var = weight_var,
        N_F = scenario$N_F,
        degree_ratio = scenario$degree_ratio,
        true_positive_rate = scenario$true_positive_rate,
        precision = scenario$precision,
        n_boot = n_boot
      )
    }, error = function(e) {
      cat("  CI calculation failed:", e$message, "\n")
      return(list(
        nsum_ci_lower = NA,
        nsum_ci_upper = NA,
        rds_ci_lower = NA,
        rds_ci_upper = NA,
        n_valid_boot = 0
      ))
    })
    
    # Return scenario with CI info
    scenario %>%
      mutate(
        nsum_ci_lower = ci_result$nsum_ci_lower,
        nsum_ci_upper = ci_result$nsum_ci_upper,
        rds_ci_lower = ci_result$rds_ci_lower,
        rds_ci_upper = ci_result$rds_ci_upper,
        n_valid_boot = ci_result$n_valid_boot
      )
  })
  
  # Merge CIs back with full results
  enhanced_results <- results_df %>%
    left_join(
      ci_results %>% select(indicator, scheme, N_F, degree_ratio, true_positive_rate, precision,
                           nsum_ci_lower, nsum_ci_upper, rds_ci_lower, rds_ci_upper, n_valid_boot),
      by = c("indicator", "scheme", "N_F", "degree_ratio", "true_positive_rate", "precision")
    )
  
  cat("Enhanced results created with", nrow(enhanced_results), "rows\n")
  
  return(enhanced_results)
}

# ============================================================================
# ROBUST SUMMARY TABLE CREATION
# ============================================================================

create_robust_summary_table <- function(results_df) {
  
  cat("Creating robust summary table...\n")
  
  valid_results <- results_df %>% filter(is.na(error))
  
  if (nrow(valid_results) == 0) {
    cat("No valid results for summary table\n")
    return(NULL)
  }
  
  # Focus on key comparison
  available_n_f <- unique(valid_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(valid_results$scheme)
  main_schemes <- available_schemes[grepl("vh_980k|ss_980k|rds_I", available_schemes)]
  if (length(main_schemes) == 0) main_schemes <- available_schemes[1:min(3, length(available_schemes))]
  
  # Create main estimates table
  main_estimates <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme %in% main_schemes,
      degree_ratio == 1.0,
      true_positive_rate == 0.7,
      precision == 1.0
    ) %>%
    mutate(
      # Format estimates with CIs where available
      nsum_formatted = case_when(
        !is.na(nsum_ci_lower) & !is.na(nsum_ci_upper) ~ paste0(
          format(round(adjusted_estimate), big.mark = ","),
          " (", format(round(nsum_ci_lower), big.mark = ","),
          "-", format(round(nsum_ci_upper), big.mark = ","), ")"
        ),
        TRUE ~ format(round(adjusted_estimate), big.mark = ",")
      ),
      rds_formatted = case_when(
        !is.na(rds_ci_lower) & !is.na(rds_ci_upper) ~ paste0(
          format(round(rds_estimate), big.mark = ","),
          " (", format(round(rds_ci_lower), big.mark = ","),
          "-", format(round(rds_ci_upper), big.mark = ","), ")"
        ),
        TRUE ~ format(round(rds_estimate), big.mark = ",")
      ),
      ratio_formatted = round(nsum_rds_ratio, 2),
      pi_i_range = paste0(round(pi_i_min, 4), "-", round(pi_i_max, 4))
    ) %>%
    select(indicator_name, scheme, nsum_formatted, rds_formatted, ratio_formatted, pi_i_range)
  
  # Pivot wider for display
  wide_data <- main_estimates %>%
    group_by(indicator_name, scheme) %>%
    summarise(
      nsum_formatted = first(nsum_formatted),
      rds_formatted = first(rds_formatted),
      ratio_formatted = first(ratio_formatted),
      pi_i_range = first(pi_i_range),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = scheme,
      values_from = c(nsum_formatted, rds_formatted, ratio_formatted, pi_i_range),
      names_sep = "_"
    )
  
  # Create gt table
  available_cols <- names(wide_data)
  col_labels <- list(indicator_name = "Exploitation Type")
  
  # Generate column labels dynamically
  for (col in available_cols) {
    if (grepl("nsum_formatted_", col)) {
      scheme_name <- gsub("nsum_formatted_", "", col)
      col_labels[[col]] <- paste("NSUM (", scheme_name, ")")
    } else if (grepl("rds_formatted_", col)) {
      scheme_name <- gsub("rds_formatted_", "", col)
      col_labels[[col]] <- paste("RDS (", scheme_name, ")")
    } else if (grepl("ratio_formatted_", col)) {
      col_labels[[col]] <- "NSUM/RDS Ratio"
    } else if (grepl("pi_i_range_", col)) {
      col_labels[[col]] <- "Inclusion Prob Range"
    }
  }
  
  # Create robust table with error handling
  gt_table <- tryCatch({
    wide_data %>%
      gt() %>%
      tab_header(
        title = "Robust NSUM vs RDS Estimates with Survey Bootstrap CIs",
        subtitle = paste("Population:", format(preferred_n_f, big.mark = ","),
                        "| Degree ratio: 1.0 | True positive rate: 0.7")
      ) %>%
      cols_label(.list = col_labels) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels()
      ) %>%
      tab_footnote(
        footnote = "Numbers in parentheses are 95% confidence intervals from survey bootstrap",
        locations = cells_column_labels(columns = contains("nsum_formatted"))
      ) %>%
      cols_width(
        indicator_name ~ px(180),
        everything() ~ px(120)
      )
  }, error = function(e) {
    cat("GT table creation failed:", e$message, "\n")
    return(wide_data)
  })
  
  return(gt_table)
}

# ============================================================================
# ROBUST SENSITIVITY TABLE  
# ============================================================================

create_robust_sensitivity_table <- function(results_df) {
  
  cat("Creating robust sensitivity analysis table...\n")
  
  valid_results <- results_df %>% filter(is.na(error))
  
  if (nrow(valid_results) == 0) {
    cat("No valid data for sensitivity table\n")
    return(NULL)
  }
  
  # Focus on key sensitivity scenarios
  available_n_f <- unique(valid_results$N_F) 
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(valid_results$scheme)
  preferred_scheme <- available_schemes[1]
  
  # Create sensitivity ranges
  sens_data <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme == preferred_scheme,
      indicator_name == "document_withholding"  # Focus on highest confidence indicator
    ) %>%
    mutate(
      scenario = paste(degree_ratio, true_positive_rate, sep = "_"),
      ci_formatted = case_when(
        !is.na(nsum_ci_lower) & !is.na(nsum_ci_upper) ~ paste0(
          format(round(nsum_ci_lower), big.mark = ","), " - ",
          format(round(nsum_ci_upper), big.mark = ",")
        ),
        TRUE ~ "NA"
      )
    ) %>%
    select(indicator_name, degree_ratio, true_positive_rate, precision, 
           adjusted_estimate, ci_formatted, adjustment_impact)
  
  # Pivot for sensitivity display
  sens_wide <- sens_data %>%
    tidyr::unite("degree_tpr", degree_ratio, true_positive_rate, sep = " | ") %>%
    select(degree_tpr, ci_formatted, adjusted_estimate, adjustment_impact)
  
  # Create gt table (with safe fallback)
  gt_table <- tryCatch({
    sens_wide %>%
      gt() %>%
      tab_header(
        title = "Sensitivity Analysis: Adjustment Factors",
        subtitle = paste("Population:", format(preferred_n_f, big.mark = ","))
      ) %>%
      cols_label(
        degree_tpr = "Degree ratio | True positive",
        ci_formatted = "95% CI (NSUM)",
        adjusted_estimate = "Adjusted NSUM",
        adjustment_impact = "Impact"
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels()
      )
  }, error = function(e) {
    cat("GT sensitivity table creation failed:", e$message, "\n")
    return(sens_wide)
  })
  
  return(gt_table)
}

# ============================================================================
# VISUALIZATIONS
# ============================================================================

plot_robust_comparison <- function(results_df) {
  
  valid_results <- results_df %>% filter(is.na(error))
  if (nrow(valid_results) == 0) return(NULL)
  
  preferred_n_f <- if (980000 %in% unique(valid_results$N_F)) 980000 else max(valid_results$N_F)
  preferred_scheme <- valid_results$scheme[1]
  
  plt_data <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme == preferred_scheme,
      degree_ratio == 1.0,
      true_positive_rate == 0.7,
      precision == 1.0
    ) %>%
    mutate(
      indicator_label = factor(indicator_name, levels = unique(indicator_name))
    )
  
  if (nrow(plt_data) == 0) return(NULL)
  
  ggplot(plt_data, aes(x = indicator_label)) +
    geom_point(aes(y = nsum_estimate, color = "NSUM"), size = 3, position = position_nudge(x = -0.15)) +
    geom_point(aes(y = rds_estimate, color = "RDS"), size = 3, position = position_nudge(x = 0.15)) +
    scale_color_manual(values = c("NSUM" = "#6A3D9A", "RDS" = "#33A02C")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "NSUM vs RDS Estimates",
      x = "Indicator",
      y = "Estimated count",
      color = "Method"
    ) +
    theme_minimal()
}

plot_robust_sensitivity <- function(results_df) {
  valid_results <- results_df %>% filter(is.na(error))
  if (nrow(valid_results) == 0) return(NULL)
  
  preferred_n_f <- if (980000 %in% unique(valid_results$N_F)) 980000 else max(valid_results$N_F)
  preferred_scheme <- valid_results$scheme[1]
  
  plt_data <- valid_results %>%
    filter(N_F == preferred_n_f, scheme == preferred_scheme)
  
  ggplot(plt_data, aes(x = degree_ratio, y = adjusted_estimate, color = true_positive_rate)) +
    geom_line(aes(group = true_positive_rate)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Sensitivity: Degree ratio vs Adjusted NSUM",
      x = "Degree ratio",
      y = "Adjusted estimate",
      color = "TPR"
    ) +
    theme_minimal()
}

plot_inclusion_probability_analysis <- function(results_df) {
  valid_results <- results_df %>% filter(is.na(error))
  if (nrow(valid_results) == 0) return(NULL)
  
  preferred_n_f <- if (980000 %in% unique(valid_results$N_F)) 980000 else max(valid_results$N_F)
  preferred_scheme <- valid_results$scheme[1]
  
  plt_data <- valid_results %>%
    filter(
      N_F == preferred_n_f,
      scheme == preferred_scheme,
      degree_ratio == 1.0, true_positive_rate == 0.7, precision == 1.0
    )
  
  if (!all(c("pi_i_min", "pi_i_max") %in% names(plt_data))) return(NULL)
  
  ggplot(plt_data, aes(x = indicator_name, y = nsum_rds_ratio)) +
    geom_col(fill = "#1F78B4") +
    geom_text(aes(label = paste0(round(nsum_rds_ratio, 2), " (", round(pi_i_min, 4), "-", round(pi_i_max, 4), ")")),
              vjust = -0.5, size = 3) +
    labs(
      title = "NSUM/RDS Ratio with Inclusion Probability Range",
      x = "Indicator",
      y = "Ratio (NSUM/RDS)"
    ) +
    ylim(0, max(plt_data$nsum_rds_ratio, na.rm = TRUE) * 1.25) +
    theme_minimal()
}

# ============================================================================
# MASTER FUNCTION TO RUN ROBUST DISPLAY PIPELINE
# ============================================================================

analyze_robust_nsum_results <- function(data, n_boot = 500) {
  
  cat("=== Running Robust NSUM Results Display Pipeline ===\n")
  
  # Step 1: Compute robust NSUM results (without CIs)
  cat("Step 1: Running robust NSUM adjustments...\n")
  robust_results <- tryCatch({
    run_robust_nsum_adjustments(data)
  }, error = function(e) {
    cat("Error during robust adjustments:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(robust_results)) {
    cat("ERROR: Robust NSUM adjustments failed\n")
    return(NULL)
  }
  
  results_df <- robust_results$valid_results
  
  # Step 2: Add survey bootstrap confidence intervals
  cat("Step 2: Adding survey bootstrap confidence intervals...\n")  
  enhanced_results <- process_robust_results_with_ci(results_df, data, n_boot)
  
  # Step 3: Create summary tables
  cat("Step 3: Creating summary tables...\n")
  summary_table <- create_robust_summary_table(enhanced_results)
  sensitivity_table <- create_robust_sensitivity_table(enhanced_results)
  
  # Step 4: Create visualizations
  cat("Step 4: Creating visualizations...\n")
  comparison_plot <- plot_robust_comparison(enhanced_results)
  sensitivity_plot <- plot_robust_sensitivity(enhanced_results)
  inclusion_plot <- plot_inclusion_probability_analysis(enhanced_results)
  
  # Step 5: Save results
  cat("Step 5: Saving results...\n")
  save(enhanced_results, robust_results, robust_nsum_config,
       file = here("output", "robust_nsum_complete_results.RData"))
  
  # Save tables 
  if (!is.null(summary_table)) {
    if (inherits(summary_table, "gt_tbl")) {
      gtsave(summary_table, here("output", "tables", "robust_nsum_summary_table.html"))
    } else {
      write_csv(summary_table, here("output", "tables", "robust_nsum_summary_table.csv"))
    }
  }
  
  if (!is.null(sensitivity_table)) {
    if (inherits(sensitivity_table, "gt_tbl")) {
      gtsave(sensitivity_table, here("output", "tables", "robust_nsum_sensitivity_table.html"))
    } else {
      write_csv(sensitivity_table, here("output", "tables", "robust_nsum_sensitivity_table.csv"))
    }
  }
  
  # Save plots with error handling
  if (!is.null(comparison_plot)) {
    tryCatch({
      ggsave(here("output", "figures", "robust_nsum_comparison_plot.png"), 
             comparison_plot, width = 12, height = 8, dpi = 300)
      cat("Comparison plot saved successfully\n")
    }, error = function(e) {
      cat("Error saving comparison plot:", e$message, "\n")
    })
  }
  
  if (!is.null(sensitivity_plot)) {
    tryCatch({
      ggsave(here("output", "figures", "robust_nsum_sensitivity_plot.png"), 
             sensitivity_plot, width = 14, height = 10, dpi = 300)
      cat("Sensitivity plot saved successfully\n")
    }, error = function(e) {
      cat("Error saving sensitivity plot:", e$message, "\n")
    })
  }
  
  if (!is.null(inclusion_plot)) {
    tryCatch({
      ggsave(here("output", "figures", "robust_nsum_inclusion_plot.png"), 
             inclusion_plot, width = 12, height = 8, dpi = 300)
      cat("Inclusion plot saved successfully\n")
    }, error = function(e) {
      cat("Error saving inclusion plot:", e$message, "\n")
    })
  }
  
  # Display results
  cat("Step 6: Displaying results...\n\n")
  
  if (!is.null(summary_table)) {
    print(summary_table)
    cat("\n")
  } else {
    cat("Summary table could not be created\n\n")
  }
  
  if (!is.null(sensitivity_table)) {
    print(sensitivity_table)
    cat("\n")
  } else {
    cat("Sensitivity table could not be created\n\n")
  }
  
  if (!is.null(comparison_plot)) {
    print(comparison_plot)
  } else {
    cat("Comparison plot could not be created\n")
  }
  
  if (!is.null(sensitivity_plot)) {
    print(sensitivity_plot)
  } else {
    cat("Sensitivity plot could not be created\n")
  }
  
  if (!is.null(inclusion_plot)) {
    print(inclusion_plot)
  } else {
    cat("Inclusion probability plot could not be created\n")
  }
  
  cat("=== Analysis completed! ===\n")
  cat("Results saved to:\n")
  cat("- output/robust_nsum_complete_results.RData\n")
  cat("- output/tables/robust_nsum_*.html\n")
  cat("- output/figures/robust_nsum_*.png\n\n")
  
  return(list(
    enhanced_results = enhanced_results,
    summary_table = summary_table,
    sensitivity_table = sensitivity_table,
    comparison_plot = comparison_plot,
    sensitivity_plot = sensitivity_plot,
    inclusion_plot = inclusion_plot
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("=== Robust NSUM Results Display Functions Loaded ===\n")
cat("To run complete analysis with survey bootstrap CIs:\n")
cat("robust_analysis <- analyze_robust_nsum_results(dd, n_boot = 500)\n\n")


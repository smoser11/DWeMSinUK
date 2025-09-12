# NSUM Results Display and Visualization
# Bootstrapped confidence intervals, formatted tables with gt, and graphics

library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)
library(patchwork)
library(boot)
library(scales)
library(here)

# ============================================================================
# BOOTSTRAP CONFIDENCE INTERVALS FOR NSUM ESTIMATES (FIXED)
# ============================================================================

bootstrap_nsum_ci <- function(data, rds_var, nsum_var, degree_var, weight_var = NULL,
                              N_F, degree_ratio = 1.0, true_positive_rate = 1.0, 
                              precision = 1.0, n_boot = 1000, conf_level = 0.95) {
  
  cat("Bootstrapping confidence intervals for", rds_var, "...\n")
  
  # DEBUG: Check input data
  cat("DEBUG BOOTSTRAP - Input check:\n")
  cat("  Data rows:", nrow(data), "\n")
  cat("  rds_var values:", sum(!is.na(data[[rds_var]])), "non-NA,", sum(data[[rds_var]], na.rm = TRUE), "positive\n")
  cat("  nsum_var values:", sum(!is.na(data[[nsum_var]])), "non-NA,", sum(data[[nsum_var]], na.rm = TRUE), "positive\n")
  cat("  degree_var values:", sum(!is.na(data[[degree_var]])), "non-NA, mean:", round(mean(data[[degree_var]], na.rm = TRUE), 2), "\n")
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    cat("  weight_var values:", sum(!is.na(data[[weight_var]])), "non-NA, mean:", round(mean(data[[weight_var]], na.rm = TRUE), 2), "\n")
  }
  cat("  Parameters: N_F =", N_F, ", degree_ratio =", degree_ratio, ", TPR =", true_positive_rate, ", precision =", precision, "\n")
  
  # Test the bootstrap function once first
  cat("DEBUG BOOTSTRAP - Testing bootstrap function once...\n")
  test_indices <- 1:nrow(data)  # Use all data first
  test_result <- tryCatch({
    boot_data <- data[test_indices, ]
    
    result <- calculate_nsum_with_adjustments(
      data = boot_data,
      rds_var = rds_var,
      nsum_var = nsum_var,
      degree_var = degree_var,
      weight_var = weight_var,
      N_F = N_F,
      degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate,
      precision = precision
    )
    
    if (is.null(result$error)) {
      cat("  Test successful: NSUM =", round(result$adjusted_estimate), ", RDS =", round(result$rds_estimate), "\n")
      c(result$adjusted_estimate, result$rds_estimate)
    } else {
      cat("  Test failed with error:", result$error, "\n")
      c(NA, NA)
    }
  }, error = function(e) {
    cat("  Test error:", e$message, "\n")
    c(NA, NA)
  })
  
  if (all(is.na(test_result))) {
    cat("ERROR: Bootstrap function fails on full data. Cannot proceed.\n")
    return(list(
      nsum_ci_lower = NA,
      nsum_ci_upper = NA,
      rds_ci_lower = NA,
      rds_ci_upper = NA,
      n_valid_boot = 0,
      boot_estimates = NULL
    ))
  }
  
  cat("DEBUG BOOTSTRAP - Test completed, proceeding to bootstrap...\n")
  cat("DEBUG BOOTSTRAP - test_result values:", test_result, "\n")
  
  # Clear test_result to avoid accidental return
  rm(test_result)
  
  # Bootstrap function
  boot_nsum <- function(data, indices) {
    boot_data <- data[indices, ]
    
    result <- calculate_nsum_with_adjustments(
      data = boot_data,
      rds_var = rds_var,
      nsum_var = nsum_var,
      degree_var = degree_var,
      weight_var = weight_var,
      N_F = N_F,
      degree_ratio = degree_ratio,
      true_positive_rate = true_positive_rate,
      precision = precision
    )
    
    if (is.null(result$error)) {
      return(c(result$adjusted_estimate, result$rds_estimate))
    } else {
      return(c(NA, NA))
    }
  }
  
  # Run bootstrap with reduced sample for debugging
  n_boot_debug <- min(n_boot, 100)  # Use smaller sample for debugging
  cat("DEBUG BOOTSTRAP - Running", n_boot_debug, "bootstrap samples...\n")
  cat("DEBUG BOOTSTRAP - About to call boot()...\n")
  
  boot_results <- tryCatch({
    boot(data, boot_nsum, R = n_boot_debug)
  }, error = function(e) {
    cat("ERROR in boot():", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(boot_results)) {
    cat("ERROR: Bootstrap failed completely.\n")
    return(list(
      nsum_ci_lower = NA,
      nsum_ci_upper = NA,
      rds_ci_lower = NA,
      rds_ci_upper = NA,
      n_valid_boot = 0,
      boot_estimates = NULL
    ))
  }
  
  cat("DEBUG BOOTSTRAP - Bootstrap completed, processing results...\n")
  cat("  Bootstrap results dimensions:", dim(boot_results$t), "\n")
  cat("  First 5 bootstrap results:\n")
  print(head(boot_results$t, 5))
  
  # Extract confidence intervals
  alpha <- 1 - conf_level
  
  # NSUM CI (first column)
  nsum_boot <- boot_results$t[, 1]  # First column is adjusted_estimate
  cat("DEBUG BOOTSTRAP - NSUM bootstrap values:", length(nsum_boot), "total,", sum(!is.na(nsum_boot)), "valid\n")
  cat("  NSUM range:", round(min(nsum_boot, na.rm = TRUE)), "to", round(max(nsum_boot, na.rm = TRUE)), "\n")
  
  nsum_boot <- nsum_boot[!is.na(nsum_boot)]
  
  if (length(nsum_boot) > 10) {
    nsum_ci <- quantile(nsum_boot, c(alpha/2, 1-alpha/2))
    cat("  NSUM CI:", round(nsum_ci[1]), "to", round(nsum_ci[2]), "\n")
  } else {
    cat("  NSUM CI: insufficient valid bootstrap samples (", length(nsum_boot), ")\n")
    nsum_ci <- c(NA, NA)
  }
  
  # RDS CI (second column)
  rds_boot <- boot_results$t[, 2]  # Second column is rds_estimate
  cat("DEBUG BOOTSTRAP - RDS bootstrap values:", length(rds_boot), "total,", sum(!is.na(rds_boot)), "valid\n")
  cat("  RDS range:", round(min(rds_boot, na.rm = TRUE)), "to", round(max(rds_boot, na.rm = TRUE)), "\n")
  
  rds_boot <- rds_boot[!is.na(rds_boot)]
  
  if (length(rds_boot) > 10) {
    rds_ci <- quantile(rds_boot, c(alpha/2, 1-alpha/2))
    cat("  RDS CI:", round(rds_ci[1]), "to", round(rds_ci[2]), "\n")
  } else {
    cat("  RDS CI: insufficient valid bootstrap samples (", length(rds_boot), ")\n")
    rds_ci <- c(NA, NA)
  }
  
  cat("DEBUG BOOTSTRAP - Final CI results:\n")
  cat("  NSUM CI: [", nsum_ci[1], ",", nsum_ci[2], "]\n")
  cat("  RDS CI: [", rds_ci[1], ",", rds_ci[2], "]\n")
  cat("  Valid bootstrap samples:", length(nsum_boot), "\n")
  
  return(list(
    nsum_ci_lower = nsum_ci[1],
    nsum_ci_upper = nsum_ci[2],
    rds_ci_lower = rds_ci[1],
    rds_ci_upper = rds_ci[2],
    n_valid_boot = length(nsum_boot),
    boot_estimates = boot_results$t
  ))
}

# ============================================================================
# DEBUG FUNCTION TO UNDERSTAND DATA STRUCTURE
# ============================================================================

debug_data_structure <- function(results_df) {
  cat("=== DATA STRUCTURE DEBUG ===\n")
  cat("Total rows:", nrow(results_df), "\n")
  cat("Rows without errors:", sum(is.na(results_df$error)), "\n\n")
  
  cat("Unique values for key variables:\n")
  cat("N_F values:", paste(unique(results_df$N_F), collapse = ", "), "\n")
  cat("scheme values:", paste(unique(results_df$scheme), collapse = ", "), "\n")
  cat("degree_ratio values:", paste(unique(results_df$degree_ratio), collapse = ", "), "\n")
  cat("true_positive_rate values:", paste(unique(results_df$true_positive_rate), collapse = ", "), "\n")
  cat("indicator_name values:", paste(unique(results_df$indicator_name), collapse = ", "), "\n\n")
  
  # Check specific filtering criteria
  filtered_check <- results_df %>%
    filter(is.na(error))
  
  cat("After filtering out errors:", nrow(filtered_check), "rows\n")
  
  if (nrow(filtered_check) > 0) {
    cat("Available N_F values:", paste(sort(unique(filtered_check$N_F)), collapse = ", "), "\n")
    cat("Available schemes:", paste(unique(filtered_check$scheme), collapse = ", "), "\n")
    cat("Available degree_ratios:", paste(sort(unique(filtered_check$degree_ratio)), collapse = ", "), "\n")
    cat("Available true_positive_rates:", paste(sort(unique(filtered_check$true_positive_rate)), collapse = ", "), "\n")
  }
  
  return(filtered_check)
}

# ============================================================================
# ENHANCED RESULTS PROCESSING WITH CONFIDENCE INTERVALS (FIXED)
# ============================================================================

process_results_with_ci <- function(results_df, data, n_boot = 500) {
  
  cat("Processing results and calculating confidence intervals...\n")
  
  # Debug the data first
  debug_results <- debug_data_structure(results_df)
  
  if (nrow(debug_results) == 0) {
    cat("No valid results for CI calculation\n")
    return(results_df)
  }
  
  # Use adaptive filtering based on available data
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(debug_results$scheme)
  weight_schemes <- available_schemes[!available_schemes %in% c("unweighted")]
  
  available_degree_ratios <- unique(debug_results$degree_ratio)
  preferred_degree_ratio <- if (1.0 %in% available_degree_ratios) 1.0 else available_degree_ratios[1]
  
  available_tpr <- unique(debug_results$true_positive_rate)
  preferred_tpr <- available_tpr[1:min(2, length(available_tpr))]  # Take up to 2 TPR values
  
  cat("Using CI calculation criteria:\n")
  cat("N_F:", preferred_n_f, "\n")
  cat("schemes:", paste(weight_schemes, collapse = ", "), "\n")
  cat("degree_ratio:", preferred_degree_ratio, "\n")
  cat("true_positive_rate:", paste(preferred_tpr, collapse = ", "), "\n\n")
  
  # Select key scenarios for CI calculation (to save time)
  key_scenarios <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme %in% weight_schemes,
      degree_ratio == preferred_degree_ratio,
      true_positive_rate %in% preferred_tpr
    )
  
  cat("Calculating CIs for", nrow(key_scenarios), "key scenarios...\n")
  
  if (nrow(key_scenarios) == 0) {
    cat("No scenarios match CI criteria, returning original results\n")
    return(results_df)
  }
  
  # Add confidence intervals for key scenarios
  ci_results <- map_dfr(1:nrow(key_scenarios), function(i) {
    scenario <- key_scenarios[i, ]
    
    # Determine weight_var
    weight_var <- NULL
    if (grepl("ss", scenario$scheme, ignore.case = TRUE)) {
      weight_var <- "wt.SS_980k"
    } else if (grepl("vh", scenario$scheme, ignore.case = TRUE)) {
      weight_var <- "wt.vh_980k"
    }
    
    # DEBUG CI calculation
    cat("DEBUG CI calc for scenario", i, "of", nrow(key_scenarios), ":\n")
    cat("  Indicator:", scenario$indicator, "\n")
    cat("  NSUM var:", str_replace(scenario$indicator, "_rds$", "_nsum"), "\n")
    cat("  Weight var:", weight_var, "\n")
    cat("  N_F:", scenario$N_F, "\n")
    
    ci_result <- tryCatch({
      bootstrap_nsum_ci(
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
      cat("  ERROR in bootstrap_nsum_ci:", e$message, "\n")
      return(list(
        nsum_ci_lower = NA,
        nsum_ci_upper = NA,
        rds_ci_lower = NA,
        rds_ci_upper = NA,
        n_valid_boot = 0
      ))
    })
    
    # Debug: Check what ci_result actually contains
    cat("  DEBUG CI result class:", class(ci_result), "\n")
    cat("  DEBUG CI result structure:\n")
    if (is.list(ci_result)) {
      cat("    List with names:", paste(names(ci_result), collapse = ", "), "\n")
      cat("  CI result - NSUM:", ci_result$nsum_ci_lower, "to", ci_result$nsum_ci_upper, "\n")
      cat("  CI result - RDS:", ci_result$rds_ci_lower, "to", ci_result$rds_ci_upper, "\n")
      cat("  Valid bootstrap samples:", ci_result$n_valid_boot, "\n")
    } else {
      cat("    Not a list! Content:", ci_result, "\n")
      # Convert to proper list structure if needed
      ci_result <- list(
        nsum_ci_lower = NA,
        nsum_ci_upper = NA,
        rds_ci_lower = NA,
        rds_ci_upper = NA,
        n_valid_boot = 0
      )
    }
    cat("  ---\n")
    
    scenario %>%
      mutate(
        nsum_ci_lower = ci_result$nsum_ci_lower,
        nsum_ci_upper = ci_result$nsum_ci_upper,
        rds_ci_lower = ci_result$rds_ci_lower,
        rds_ci_upper = ci_result$rds_ci_upper,
        n_valid_boot = ci_result$n_valid_boot
      )
  })
  
  # Merge back with full results
  enhanced_results <- results_df %>%
    left_join(
      ci_results %>% 
        select(indicator_name, scheme, degree_ratio, true_positive_rate, precision, N_F,
               nsum_ci_lower, nsum_ci_upper, rds_ci_lower, rds_ci_upper, n_valid_boot),
      by = c("indicator_name", "scheme", "degree_ratio", "true_positive_rate", "precision", "N_F"),
      relationship = "many-to-one"
    )
  
  return(enhanced_results)
}

# ============================================================================
# FORMATTED TABLES WITH ADAPTIVE FILTERING
# ============================================================================

create_summary_table <- function(results_df) {
  
  # Debug the data first
  debug_results <- debug_data_structure(results_df)
  
  if (nrow(debug_results) == 0) {
    cat("Error: No valid results to display\n")
    return(NULL)
  }
  
  # Use flexible filtering based on available data
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(debug_results$scheme)
  weight_schemes <- available_schemes[!available_schemes %in% c("unweighted")]
  
  available_degree_ratios <- unique(debug_results$degree_ratio)
  preferred_degree_ratio <- if (1.0 %in% available_degree_ratios) 1.0 else available_degree_ratios[1]
  
  available_tpr <- unique(debug_results$true_positive_rate)
  preferred_tpr <- if (0.6 %in% available_tpr) 0.6 else available_tpr[1]
  
  cat("Using filtering criteria:\n")
  cat("N_F:", preferred_n_f, "\n")
  cat("schemes:", paste(weight_schemes, collapse = ", "), "\n")
  cat("degree_ratio:", preferred_degree_ratio, "\n")
  cat("true_positive_rate:", preferred_tpr, "\n\n")
  
  # Main estimates table with flexible criteria
  main_estimates <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme %in% weight_schemes,
      degree_ratio == preferred_degree_ratio,
      true_positive_rate == preferred_tpr
    )
  
  cat("Filtered data has", nrow(main_estimates), "rows\n")
  
  if (nrow(main_estimates) == 0) {
    cat("Warning: No data matches the filtering criteria\n")
    return(NULL)
  }
  
  main_estimates <- main_estimates %>%
    select(indicator_name, scheme, adjusted_estimate, rds_estimate, nsum_rds_ratio,
           nsum_ci_lower, nsum_ci_upper, rds_ci_lower, rds_ci_upper) %>%
    mutate(
      # Format estimates with CIs
      nsum_formatted = case_when(
        !is.na(nsum_ci_lower) ~ paste0(
          format(round(adjusted_estimate), big.mark = ","),
          " (", format(round(nsum_ci_lower), big.mark = ","),
          "-", format(round(nsum_ci_upper), big.mark = ","), ")"
        ),
        TRUE ~ format(round(adjusted_estimate), big.mark = ",")
      ),
      rds_formatted = case_when(
        !is.na(rds_ci_lower) ~ paste0(
          format(round(rds_estimate), big.mark = ","),
          " (", format(round(rds_ci_lower), big.mark = ","),
          "-", format(round(rds_ci_upper), big.mark = ","), ")"
        ),
        TRUE ~ format(round(rds_estimate), big.mark = ",")
      ),
      ratio_formatted = round(nsum_rds_ratio, 2)
    ) %>%
    select(indicator_name, scheme, nsum_formatted, rds_formatted, ratio_formatted)
  
  # Pivot wider with proper handling of duplicates
  wide_data <- main_estimates %>%
    # Group and summarize to handle any duplicates
    group_by(indicator_name, scheme) %>%
    summarise(
      nsum_formatted = first(nsum_formatted),
      rds_formatted = first(rds_formatted), 
      ratio_formatted = first(ratio_formatted),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = scheme,
      values_from = c(nsum_formatted, rds_formatted, ratio_formatted),
      names_sep = "_"
    )
  
  # Create flexible column labels based on what actually exists
  available_cols <- names(wide_data)
  col_labels <- list(indicator_name = "Exploitation Type")
  
  # Dynamically add labels for available columns
  for (col in available_cols) {
    if (grepl("nsum_formatted_", col)) {
      scheme_name <- gsub("nsum_formatted_", "", col)
      col_labels[[col]] <- paste("NSUM (", scheme_name, ")")
    } else if (grepl("rds_formatted_", col)) {
      scheme_name <- gsub("rds_formatted_", "", col)
      col_labels[[col]] <- paste("RDS (", scheme_name, ")")
    } else if (grepl("ratio_formatted_", col)) {
      col_labels[[col]] <- "NSUM/RDS Ratio"
    }
  }
  
  # Create gt table with error handling
  cat("DEBUG: Creating gt table...\n")
  cat("Wide data dimensions:", nrow(wide_data), "x", ncol(wide_data), "\n")
  cat("Available columns:", paste(names(wide_data), collapse = ", "), "\n")
  
  # Check for ratio columns
  ratio_columns <- names(wide_data)[grepl("ratio", names(wide_data))]
  cat("Ratio columns found:", paste(ratio_columns, collapse = ", "), "\n")
  
  gt_table <- tryCatch({
    wide_data %>%
      gt() %>%
      tab_header(
        title = "NSUM vs RDS Estimates of Domestic Worker Exploitation",
        subtitle = paste("Population:", format(preferred_n_f, big.mark = ","), 
                         "| Degree ratio:", preferred_degree_ratio,
                         "| True positive rate:", preferred_tpr)
      ) %>%
      cols_label(.list = col_labels) %>%
      {
        # Only apply fmt_number if ratio columns exist and contain numeric data
        if (length(ratio_columns) > 0) {
          # Check if the ratio columns actually contain numeric data
          ratio_numeric <- sapply(ratio_columns, function(col) {
            if (col %in% names(wide_data)) {
              is.numeric(wide_data[[col]]) && !all(is.na(wide_data[[col]]))
            } else {
              FALSE
            }
          })
          
          if (any(ratio_numeric)) {
            numeric_ratio_cols <- ratio_columns[ratio_numeric]
            cat("Formatting numeric ratio columns:", paste(numeric_ratio_cols, collapse = ", "), "\n")
            fmt_number(., columns = all_of(numeric_ratio_cols), decimals = 2)
          } else {
            cat("No numeric ratio columns to format\n")
            .
          }
        } else {
          cat("No ratio columns found\n")
          .
        }
      } %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels()
      ) %>%
      tab_footnote(
        footnote = "Numbers in parentheses are 95% confidence intervals from bootstrap samples",
        locations = cells_column_labels(columns = contains("nsum_formatted"))
      ) %>%
      cols_width(
        indicator_name ~ px(180),
        everything() ~ px(120)
      )
  }, error = function(e) {
    cat("ERROR in gt table creation:", e$message, "\n")
    cat("Returning simple data frame instead of gt table\n")
    return(wide_data)  # Return the data frame if gt fails
  })
  
  return(gt_table)
}

# ============================================================================
# SENSITIVITY TABLE WITH ADAPTIVE FILTERING
# ============================================================================

create_sensitivity_table <- function(results_df) {
  
  # Use flexible filtering
  debug_results <- results_df %>% filter(is.na(error))
  
  if (nrow(debug_results) == 0) {
    cat("No valid data for sensitivity table\n")
    return(NULL)
  }
  
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(debug_results$scheme)
  preferred_scheme <- available_schemes[1]  # Just take the first available scheme
  
  # Sensitivity analysis by adjustment factors
  sensitivity_summary <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme == preferred_scheme
    ) %>%
    group_by(indicator_name, degree_ratio, true_positive_rate) %>%
    summarise(
      median_estimate = median(adjusted_estimate, na.rm = TRUE),
      min_estimate = min(adjusted_estimate, na.rm = TRUE),
      max_estimate = max(adjusted_estimate, na.rm = TRUE),
      range_ratio = max_estimate / min_estimate,
      .groups = 'drop'
    ) %>%
    mutate(
      estimate_range = paste0(
        format(round(min_estimate), big.mark = ","), 
        " - ", 
        format(round(max_estimate), big.mark = ",")
      )
    ) %>%
    select(indicator_name, degree_ratio, true_positive_rate, estimate_range, range_ratio)
  
  if (nrow(sensitivity_summary) == 0) {
    cat("No data available for sensitivity table after grouping\n")
    return(NULL)
  }
  
  # Get available degree ratios (use what's available)
  available_ratios <- sort(unique(sensitivity_summary$degree_ratio))
  selected_ratios <- if (length(available_ratios) >= 3) {
    c(available_ratios[1], available_ratios[ceiling(length(available_ratios)/2)], 
      available_ratios[length(available_ratios)])
  } else {
    available_ratios
  }
  
  # Create table showing ranges
  range_table <- sensitivity_summary %>%
    filter(degree_ratio %in% selected_ratios) %>%
    pivot_wider(
      names_from = c(degree_ratio, true_positive_rate),
      values_from = estimate_range,
      names_sep = "_"
    ) %>%
    gt() %>%
    tab_header(
      title = "Sensitivity of NSUM Estimates to Adjustment Factors",
      subtitle = "Range of estimates across different degree ratios and true positive rates"
    ) %>%
    cols_label(
      indicator_name = "Exploitation Type"
    ) %>%
    tab_footnote(
      footnote = "Format: minimum - maximum estimate across precision values",
      locations = cells_title()
    )
  
  return(range_table)
}

# ============================================================================
# ADAPTIVE VISUALIZATION FUNCTIONS
# ============================================================================

plot_estimate_comparison <- function(results_df) {
  
  # Use flexible filtering
  debug_results <- results_df %>% filter(is.na(error))
  
  if (nrow(debug_results) == 0) {
    cat("No valid data for comparison plot\n")
    return(NULL)
  }
  
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(debug_results$scheme)
  weight_schemes <- available_schemes[!available_schemes %in% c("unweighted")]
  
  plot_data <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme %in% weight_schemes
    ) %>%
    # Take first available values for other parameters
    group_by(indicator_name, scheme) %>%
    slice(1) %>%
    ungroup()
  
  if (nrow(plot_data) == 0) {
    cat("No data available for plot after filtering\n")
    return(NULL)
  }
  
  # Create colors based on available schemes
  scheme_colors <- rainbow(length(unique(plot_data$scheme)))
  names(scheme_colors) <- unique(plot_data$scheme)
  
  # Filter out rows with NA values that would cause plotting issues
  plot_data_clean <- plot_data %>%
    filter(
      !is.na(rds_estimate) & is.finite(rds_estimate),
      !is.na(adjusted_estimate) & is.finite(adjusted_estimate)
    )
  
  cat("Plot data after cleaning: ", nrow(plot_data_clean), " of ", nrow(plot_data), " rows\n")
  
  if (nrow(plot_data_clean) == 0) {
    cat("No valid data for plotting after removing NAs\n")
    return(NULL)
  }
  
  p1 <- plot_data_clean %>%
    ggplot(aes(x = rds_estimate, y = adjusted_estimate, color = scheme)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = nsum_ci_lower, ymax = nsum_ci_upper), 
                  width = 0, alpha = 0.6) +
    geom_errorbarh(aes(xmin = rds_ci_lower, xmax = rds_ci_upper), 
                   height = 0, alpha = 0.6) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    scale_color_manual(values = scheme_colors) +
    labs(
      title = "NSUM vs RDS Estimates with 95% Confidence Intervals",
      subtitle = "Dashed line shows perfect agreement (NSUM = RDS)",
      x = "RDS Estimate",
      y = "NSUM Estimate", 
      color = "Weight Scheme"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  return(p1)
}

plot_sensitivity_analysis <- function(results_df) {
  
  # Use flexible filtering and remove NA values
  debug_results <- results_df %>% 
    filter(
      is.na(error),
      !is.na(adjusted_estimate) & is.finite(adjusted_estimate),
      !is.na(degree_ratio) & is.finite(degree_ratio)
    )
  
  if (nrow(debug_results) == 0) {
    cat("No valid data for sensitivity plot\n")
    return(NULL)
  }
  
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  available_schemes <- unique(debug_results$scheme)
  preferred_scheme <- available_schemes[1]  # Take first available scheme
  
  sens_data <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      scheme == preferred_scheme
    )
  
  if (nrow(sens_data) == 0) {
    cat("No data available for sensitivity analysis\n")
    return(NULL)
  }
  
  # === DEBUGGING OUTPUT FOR SENSITIVITY PLOT ===
  cat("\n=== SENSITIVITY PLOT DEBUG ===\n")
  cat("Total rows in sens_data:", nrow(sens_data), "\n")
  cat("Preferred N_F:", preferred_n_f, "\n")
  cat("Preferred scheme:", preferred_scheme, "\n")
  
  cat("\nUnique values in sens_data:\n")
  cat("- degree_ratio:", paste(sort(unique(sens_data$degree_ratio)), collapse = ", "), "\n")
  cat("- true_positive_rate:", paste(sort(unique(sens_data$true_positive_rate)), collapse = ", "), "\n")
  cat("- indicator_name:", paste(unique(sens_data$indicator_name), collapse = ", "), "\n")
  cat("- precision:", paste(unique(sens_data$precision), collapse = ", "), "\n")
  
  # Check if we have multiple values per combination
  cat("\nData structure check:\n")
  data_structure <<- sens_data %>%
    group_by(indicator_name, degree_ratio, true_positive_rate) %>%
    summarise(
      n_rows = n(),
      min_basic = min(basic_estimate, na.rm = TRUE),
      max_basic = max(basic_estimate, na.rm = TRUE),
      min_adjusted = min(adjusted_estimate, na.rm = TRUE),
      max_adjusted = max(adjusted_estimate, na.rm = TRUE),
      unique_adjustment_impact = n_distinct(adjustment_impact, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print(data_structure)
  
  # Show actual values for one indicator to debug
  cat("\nSample data for first indicator:\n")
  sample_data <<- sens_data %>%
    filter(indicator_name == unique(sens_data$indicator_name)[1]) %>%
    select(degree_ratio, true_positive_rate, precision, basic_estimate, 
           adjusted_estimate, adjustment_impact) %>%
    arrange(degree_ratio, true_positive_rate, precision)
  
  print(head(sample_data, 15))
  
  # Check if we have enough data for faceting
  tpr_values <- unique(sens_data$true_positive_rate)
  if (length(tpr_values) == 0) {
    cat("No true_positive_rate values available\n")
    return(NULL)
  }
  
  cat("\nUsing", length(tpr_values), "true_positive_rate values:", paste(tpr_values, collapse = ", "), "\n")
  cat("================================\n")
  
  # Plot 1: Effect of degree ratio
  p2 <- sens_data %>%
    ggplot(aes(x = degree_ratio, y = adjusted_estimate, color = indicator_name)) +
    geom_line(aes(group = interaction(indicator_name, true_positive_rate)), 
              alpha = 0.6, linewidth = 1) +
    geom_point(alpha = 0.8) +
    facet_wrap(~true_positive_rate, 
               labeller = labeller(true_positive_rate = function(x) paste("TPR =", x))) +
    scale_y_continuous(labels = label_comma()) +
    scale_color_viridis_d(name = "Exploitation Type") +
    labs(
      title = "Sensitivity to Degree Ratio by True Positive Rate",
      x = "Degree Ratio (Hidden/Frame Population)",
      y = "NSUM Estimate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  # Plot 2: Population size sensitivity (if multiple N_F values available)
  available_n_f_all <- unique(debug_results$N_F)
  if (length(available_n_f_all) > 1) {
    pop_data <- results_df %>%
      filter(
        is.na(error),
        scheme == preferred_scheme,
        !is.na(nsum_prevalence) & is.finite(nsum_prevalence),
        !is.na(N_F) & is.finite(N_F)
      ) %>%
      group_by(indicator_name, N_F) %>%
      slice(1) %>%
      ungroup()
    
    p3 <- pop_data %>%
      ggplot(aes(x = N_F, y = nsum_prevalence, color = indicator_name)) +
      geom_line(aes(group = indicator_name), linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(labels = label_comma()) +
      scale_y_continuous(labels = label_percent()) +
      scale_color_viridis_d(name = "Exploitation Type") +
      labs(
        title = "NSUM Prevalence Estimates by Population Size",
        x = "Frame Population Size",
        y = "Estimated Prevalence (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    # Combine plots
    combined_plot <- p2 / p3 + plot_layout(heights = c(1, 1))
  } else {
    combined_plot <- p2
  }
  
  return(combined_plot)
}

plot_weight_comparison <- function(results_df) {
  
  # Use flexible filtering
  debug_results <- results_df %>% filter(is.na(error))
  
  if (nrow(debug_results) == 0) {
    cat("No valid data for weight comparison plot\n")
    return(NULL)
  }
  
  available_n_f <- unique(debug_results$N_F)
  preferred_n_f <- if (980000 %in% available_n_f) 980000 else max(available_n_f)
  
  weight_data <- results_df %>%
    filter(
      is.na(error),
      N_F == preferred_n_f,
      !is.na(adjusted_estimate) & is.finite(adjusted_estimate)
    ) %>%
    group_by(indicator_name, scheme) %>%
    slice(1) %>%
    ungroup()
  
  if (nrow(weight_data) == 0) {
    cat("No data available for weight comparison\n")
    return(NULL)
  }
  
  # Create colors based on available schemes
  available_schemes <- unique(weight_data$scheme)
  scheme_colors <- rainbow(length(available_schemes))
  names(scheme_colors) <- available_schemes
  
  p4 <- weight_data %>%
    ggplot(aes(x = scheme, y = adjusted_estimate, fill = scheme)) +
    geom_col(alpha = 0.8) +
    facet_wrap(~indicator_name, scales = "free_y") +
    scale_y_continuous(labels = label_comma()) +
    scale_fill_manual(values = scheme_colors) +
    labs(
      title = "Comparison of Weight Schemes Across Exploitation Types",
      x = "Weight Scheme",
      y = "NSUM Estimate",
      fill = "Weight Scheme"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p4)
}

# ============================================================================
# MAIN DISPLAY FUNCTION (FIXED)
# ============================================================================

display_nsum_results <- function(results_df, data, n_boot = 500, save_outputs = TRUE) {
  
  cat("=== NSUM RESULTS DISPLAY AND VISUALIZATION ===\n\n")
  
  # Process results with confidence intervals
  cat("Step 1: Processing results and calculating confidence intervals...\n")
  enhanced_results <- process_results_with_ci(results_df, data, n_boot)
  
  # Create formatted tables
  cat("Step 2: Creating formatted tables...\n")
  summary_table <- create_summary_table(enhanced_results)
  sensitivity_table <- create_sensitivity_table(enhanced_results)
  
  # Create visualizations
  cat("Step 3: Creating visualizations...\n")
  comparison_plot <- plot_estimate_comparison(enhanced_results)
  sensitivity_plot <- plot_sensitivity_analysis(enhanced_results)
  weight_plot <- plot_weight_comparison(enhanced_results)
  
  # Display results
  cat("Step 4: Displaying results...\n\n")
  
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
  
  if (!is.null(weight_plot)) {
    print(weight_plot)
  } else {
    cat("Weight comparison plot could not be created\n")
  }
  
  # Save outputs
  if (save_outputs) {
    cat("Step 5: Saving outputs...\n")
    
    # Save tables (only if they exist)
    if (!is.null(summary_table)) {
      gtsave(summary_table, here("output", "tables", "nsum_summary_table.html"))
    }
    if (!is.null(sensitivity_table)) {
      gtsave(sensitivity_table, here("output", "tables", "nsum_sensitivity_table.html"))
    }
    
    # Save plots (only if they exist)
    if (!is.null(comparison_plot)) {
      ggsave(here("output", "figures", "nsum_comparison_plot.png"), 
             comparison_plot, width = 10, height = 8, dpi = 300)
    }
    if (!is.null(sensitivity_plot)) {
      ggsave(here("output", "figures", "nsum_sensitivity_plot.png"), 
             sensitivity_plot, width = 12, height = 10, dpi = 300)
    }
    if (!is.null(weight_plot)) {
      ggsave(here("output", "figures", "nsum_weight_comparison.png"), 
             weight_plot, width = 12, height = 8, dpi = 300)
    }
    
    # Save enhanced results
    save(enhanced_results, file = here("output", "enhanced_nsum_results.RData"))
    write_csv(enhanced_results, here("output", "tables", "enhanced_nsum_results.csv"))
    
    cat("Outputs saved to:\n")
    cat("- Tables: output/tables/\n")
    cat("- Figures: output/figures/\n")
    cat("- Data: output/enhanced_nsum_results.RData\n")
  }
  
  return(list(
    enhanced_results = enhanced_results,
    summary_table = summary_table,
    sensitivity_table = sensitivity_table,
    comparison_plot = comparison_plot,
    sensitivity_plot = sensitivity_plot,
    weight_plot = weight_plot
  ))
}

# ============================================================================
# USAGE EXAMPLE
# ============================================================================

cat("=== USAGE INSTRUCTIONS ===\n")
cat("After running sensitivity analysis, use:\n\n")
cat("# Display all results with 500 bootstrap samples\n")
cat("display_outputs <- display_nsum_results(sensitivity_results$valid_results, dd, n_boot = 500)\n\n")
cat("# Quick display with fewer bootstrap samples\n") 
cat("display_outputs <- display_nsum_results(sensitivity_results$valid_results, dd, n_boot = 100)\n\n")
cat("# Access individual components:\n")
cat("display_outputs$summary_table        # Main estimates table\n")
cat("display_outputs$sensitivity_table    # Sensitivity analysis table\n")
cat("display_outputs$comparison_plot      # NSUM vs RDS scatter plot\n")
cat("display_outputs$sensitivity_plot     # Sensitivity analysis plots\n")
cat("display_outputs$weight_plot          # Weight scheme comparison\n")
# After running sensitivity analysis:
display_outputs <- display_nsum_results(
  sensitivity_results$valid_results, 
  dd, 
  n_boot = 500
)

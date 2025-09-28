# 06-RESULTS_analysis_comparison.R
# Post-Analysis Functions for MODULAR Estimator Results
# Read, compare, visualize, and tabulate results from 06-MODULAR_estimator_analysis.R
#
# FUNCTIONS FOR:
# - Loading saved results from individual estimator runs
# - Smart detection and combination of available results
# - Advanced comparison tables and plots
# - Publication-ready outputs
# - Method performance assessment

cat("=== RESULTS Analysis and Comparison Tools ===\n")
cat("Post-analysis tools for MODULAR estimator results\n")
cat("Load, compare, visualize, and tabulate saved results\n\n")

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(viridis)
library(scales)
library(here)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# ============================================================================
# RESULTS LOADING AND MANAGEMENT
# ============================================================================

# Smart detection of available result files
detect_available_results <- function(output_dir = here("output")) {
  
  cat("=== Detecting Available Results ===\n")
  
  # Define patterns for different estimator result files
  result_patterns <- list(
    rds_i = c("rds_i_analysis_results.RData", "rds_i_analysis.csv"),
    rds_ii = c("rds_ii_analysis_results.RData", "rds_ii_analysis.csv"),
    rds_ss = c("rds_ss_analysis_results.RData", "rds_ss_analysis.csv"),
    ma_estimates = c("ma_estimates_analysis_results.RData", "ma_estimates_analysis.csv"),
    posteriorsize = c("posteriorsize_analysis_results.RData", "posteriorsize_analysis.csv")
  )
  
  available_results <- list()
  
  for (method in names(result_patterns)) {
    
    rdata_file <- here(output_dir, result_patterns[[method]][1])
    csv_file <- here(output_dir, "tables", result_patterns[[method]][2])
    
    available_results[[method]] <- list(
      method = method,
      rdata_available = file.exists(rdata_file),
      csv_available = file.exists(csv_file),
      rdata_path = rdata_file,
      csv_path = csv_file,
      rdata_size = if(file.exists(rdata_file)) file.size(rdata_file) else 0,
      rdata_modified = if(file.exists(rdata_file)) file.mtime(rdata_file) else NA
    )
    
    if (available_results[[method]]$rdata_available) {
      cat("✓", toupper(method), "results available (", 
          round(available_results[[method]]$rdata_size/1024/1024, 1), "MB,",
          "modified", format(available_results[[method]]$rdata_modified, "%Y-%m-%d %H:%M"), ")\n")
    } else {
      cat("✗", toupper(method), "results not found\n")
    }
  }
  
  # Summary
  available_methods <- names(available_results)[sapply(available_results, function(x) x$rdata_available)]
  cat("\nAvailable methods:", length(available_methods), "of", length(result_patterns), "\n")
  cat("Methods:", paste(available_methods, collapse = ", "), "\n\n")
  
  return(available_results)
}

# Load specific method results
load_method_results <- function(method, output_dir = here("output"), verbose = TRUE) {
  
  method <- tolower(method)
  
  # Define file patterns
  file_patterns <- list(
    rds_i = "rds_i_analysis_results.RData",
    rds_ii = "rds_ii_analysis_results.RData", 
    rds_ss = "rds_ss_analysis_results.RData",
    ma_estimates = "ma_estimates_analysis_results.RData",
    posteriorsize = "posteriorsize_analysis_results.RData"
  )
  
  if (!method %in% names(file_patterns)) {
    stop("Unknown method: ", method, ". Available: ", paste(names(file_patterns), collapse = ", "))
  }
  
  result_file <- here(output_dir, file_patterns[[method]])
  
  if (!file.exists(result_file)) {
    warning("Results file not found for ", method, ": ", result_file)
    return(NULL)
  }
  
  if (verbose) cat("Loading", method, "results from:", basename(result_file), "\n")
  
  # Load in isolated environment
  env <- new.env()
  load(result_file, envir = env)
  
  # Extract expected objects (method_results, method_df)
  expected_objects <- paste0(method, c("_results", "_df"))
  
  result_data <- list()
  
  # Try to find the data with various naming patterns
  all_objects <- ls(env)
  
  # Look for results_raw or similar
  raw_results_names <- all_objects[grepl("results$|_results$", all_objects)]
  if (length(raw_results_names) > 0) {
    result_data$results_raw <- get(raw_results_names[1], envir = env)
  }
  
  # Look for results_df or similar  
  df_names <- all_objects[grepl("_df$|results_df", all_objects)]
  if (length(df_names) > 0) {
    result_data$results_df <- get(df_names[1], envir = env)
  }
  
  # Add metadata
  result_data$method <- method
  result_data$file_path <- result_file
  result_data$loaded_at <- Sys.time()
  result_data$file_size <- file.size(result_file)
  
  if (verbose && !is.null(result_data$results_df)) {
    cat("  Loaded", nrow(result_data$results_df), "estimations\n")
  }
  
  return(result_data)
}

# Load all available results
load_all_available_results <- function(methods = NULL, output_dir = here("output"), verbose = TRUE) {
  
  cat("=== Loading All Available Results ===\n")
  
  # Detect what's available
  available <- detect_available_results(output_dir)
  available_methods <- names(available)[sapply(available, function(x) x$rdata_available)]
  
  # Filter by requested methods if specified
  if (!is.null(methods)) {
    available_methods <- intersect(available_methods, methods)
    if (length(available_methods) == 0) {
      stop("None of the requested methods have available results: ", paste(methods, collapse = ", "))
    }
  }
  
  # Load each method
  all_results <- list()
  
  for (method in available_methods) {
    result_data <- load_method_results(method, output_dir, verbose = verbose)
    if (!is.null(result_data)) {
      all_results[[method]] <- result_data
    }
  }
  
  # Combine into master data frame
  if (length(all_results) > 0) {
    
    all_dfs <- list()
    total_estimations <- 0
    
    for (method in names(all_results)) {
      if (!is.null(all_results[[method]]$results_df)) {
        df <- all_results[[method]]$results_df
        df$source_method <- method
        all_dfs[[method]] <- df
        total_estimations <- total_estimations + nrow(df)
      }
    }
    
    if (length(all_dfs) > 0) {
      master_df <- do.call(rbind, all_dfs)
      rownames(master_df) <- NULL
    } else {
      master_df <- data.frame()
    }
  } else {
    master_df <- data.frame()
  }
  
  cat("Loaded", length(all_results), "method(s) with", total_estimations, "total estimations\n\n")
  
  return(list(
    individual_results = all_results,
    master_df = master_df,
    methods_loaded = names(all_results),
    total_estimations = total_estimations,
    loaded_at = Sys.time()
  ))
}

# ============================================================================
# RESULTS COMPARISON AND ANALYSIS
# ============================================================================

# Compare methods across indicators
compare_methods_by_indicator <- function(master_df, population_size = 980000, 
                                       methods = NULL, indicators = NULL) {
  
  if (is.null(methods)) methods <- unique(master_df$method)
  if (is.null(indicators)) indicators <- unique(master_df$indicator)
  
  cat("=== Method Comparison by Indicator ===\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Indicators:", length(indicators), "\n\n")
  
  # Filter data
  comparison_data <- master_df %>%
    filter(
      method %in% methods,
      indicator %in% indicators,
      pop_size == population_size,
      !is.na(estimate)
    ) %>%
    select(indicator, indicator_clean, method, method_clean, method_type,
           estimate, estimate_pct, ci_lower_pct, ci_upper_pct, 
           estimate_with_ci, uncertainty_method) %>%
    arrange(indicator_clean, method)
  
  if (nrow(comparison_data) == 0) {
    warning("No data available for the specified methods, indicators, and population size")
    return(NULL)
  }
  
  # Create summary statistics
  comparison_summary <- comparison_data %>%
    group_by(indicator_clean) %>%
    summarise(
      n_methods = n(),
      min_estimate = min(estimate_pct, na.rm = TRUE),
      max_estimate = max(estimate_pct, na.rm = TRUE), 
      range_estimate = max_estimate - min_estimate,
      mean_estimate = mean(estimate_pct, na.rm = TRUE),
      sd_estimate = sd(estimate_pct, na.rm = TRUE),
      cv_estimate = sd_estimate / mean_estimate,
      methods_available = paste(sort(unique(method_clean)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(range_estimate))
  
  cat("Comparison summary (ordered by estimate range):\n")
  print(comparison_summary %>% select(indicator_clean, n_methods, min_estimate, max_estimate, range_estimate))
  cat("\n")
  
  return(list(
    comparison_data = comparison_data,
    summary = comparison_summary,
    population_size = population_size,
    methods = methods,
    indicators = indicators
  ))
}

# Assess method performance and agreement
assess_method_performance <- function(master_df, population_size = 980000, 
                                    baseline_method = "RDS_SS") {
  
  cat("=== Method Performance Assessment ===\n")
  cat("Baseline method:", baseline_method, "\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n\n")
  
  # Filter to main population size
  perf_data <- master_df %>%
    filter(pop_size == population_size, !is.na(estimate))
  
  if (nrow(perf_data) == 0) {
    warning("No data available for performance assessment")
    return(NULL)
  }
  
  # Method-wise performance
  method_performance <- perf_data %>%
    group_by(method, method_clean, method_type) %>%
    summarise(
      n_indicators = n(),
      n_successful = sum(!is.na(estimate)),
      success_rate = n_successful / n_indicators,
      mean_estimate = mean(estimate_pct, na.rm = TRUE),
      median_estimate = median(estimate_pct, na.rm = TRUE),
      sd_estimate = sd(estimate_pct, na.rm = TRUE),
      mean_ci_width = mean(ci_upper_pct - ci_lower_pct, na.rm = TRUE),
      median_ci_width = median(ci_upper_pct - ci_lower_pct, na.rm = TRUE),
      n_with_ci = sum(!is.na(ci_lower_pct) & !is.na(ci_upper_pct)),
      ci_availability = n_with_ci / n_successful,
      .groups = "drop"
    ) %>%
    arrange(desc(success_rate), method_type, method)
  
  # Agreement with baseline method
  if (baseline_method %in% unique(perf_data$method)) {
    
    baseline_data <- perf_data %>%
      filter(method == baseline_method) %>%
      select(indicator, baseline_estimate = estimate_pct)
    
    agreement_analysis <- perf_data %>%
      filter(method != baseline_method) %>%
      left_join(baseline_data, by = "indicator") %>%
      filter(!is.na(baseline_estimate), !is.na(estimate_pct)) %>%
      group_by(method, method_clean) %>%
      summarise(
        n_comparisons = n(),
        correlation = cor(estimate_pct, baseline_estimate, use = "complete.obs"),
        mean_diff = mean(estimate_pct - baseline_estimate, na.rm = TRUE),
        mean_abs_diff = mean(abs(estimate_pct - baseline_estimate), na.rm = TRUE),
        rmse = sqrt(mean((estimate_pct - baseline_estimate)^2, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      arrange(desc(correlation))
    
  } else {
    warning("Baseline method ", baseline_method, " not found in data")
    agreement_analysis <- NULL
  }
  
  cat("Method Performance Summary:\n")
  print(method_performance %>% select(method_clean, n_indicators, success_rate, mean_estimate, mean_ci_width))
  
  if (!is.null(agreement_analysis)) {
    cat("\nAgreement with", baseline_method, ":\n")
    print(agreement_analysis %>% select(method_clean, correlation, mean_abs_diff, rmse))
  }
  cat("\n")
  
  return(list(
    method_performance = method_performance,
    agreement_analysis = agreement_analysis,
    baseline_method = baseline_method,
    population_size = population_size
  ))
}

# ============================================================================
# PUBLICATION-READY TABLE GENERATION
# ============================================================================

# Create main comparison table for publication
create_publication_table <- function(master_df, methods = NULL, indicators = NULL,
                                   population_size = 980000, table_format = "latex",
                                   save_table = TRUE, table_name = "publication_comparison") {
  
  if (is.null(methods)) methods <- unique(master_df$method)
  if (is.null(indicators)) indicators <- unique(master_df$indicator)
  
  cat("=== Creating Publication Table ===\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Indicators:", length(indicators), "\n")
  cat("Format:", table_format, "\n\n")
  
  # Filter and prepare data
  table_data <- master_df %>%
    filter(
      method %in% methods,
      indicator %in% indicators,
      pop_size == population_size,
      !is.na(estimate)
    ) %>%
    select(indicator_clean, method_clean, method_type, estimate_with_ci, uncertainty_clean) %>%
    arrange(indicator_clean, method_type, method_clean)
  
  if (nrow(table_data) == 0) {
    warning("No data available for publication table")
    return(NULL)
  }
  
  # Create wide format table
  pub_table <- table_data %>%
    group_by(indicator_clean, method_clean) %>%
    slice(1) %>%  # Handle any duplicates
    ungroup() %>%
    pivot_wider(
      names_from = method_clean,
      values_from = estimate_with_ci,
      id_cols = indicator_clean,
      values_fn = first
    ) %>%
    arrange(indicator_clean)
  
  # Add method type headers if multiple types
  method_types <- table_data %>%
    distinct(method_clean, method_type) %>%
    arrange(method_type, method_clean)
  
  # Format based on requested format
  if (table_format == "latex") {
    formatted_table <- pub_table %>%
      kbl(format = "latex", booktabs = TRUE, 
          caption = paste("Method Comparison at Population Size", format(population_size, big.mark = ","))) %>%
      kable_styling(latex_options = c("striped", "hold_position"))
    
    if (length(unique(method_types$method_type)) > 1) {
      # Add grouping headers for method types
      freq_methods <- method_types$method_clean[method_types$method_type == "frequentist"]
      bayes_methods <- method_types$method_clean[method_types$method_type == "bayesian"]
      
      if (length(freq_methods) > 0 && any(freq_methods %in% names(pub_table))) {
        freq_cols <- which(names(pub_table) %in% freq_methods)
        if (length(freq_cols) > 0) {
          formatted_table <- formatted_table %>%
            add_header_above(setNames(c(1, length(freq_cols)), c(" ", "Frequentist Methods")))
        }
      }
    }
    
  } else if (table_format == "html") {
    formatted_table <- pub_table %>%
      kbl(format = "html", table.attr = "class='table table-striped'") %>%
      kable_styling(bootstrap_options = c("striped", "hover"))
    
  } else {
    formatted_table <- pub_table
  }
  
  # Save table
  if (save_table) {
    # Save raw CSV
    csv_filename <- paste0(table_name, "_", format(population_size/1000, digits = 0), "k.csv")
    write.csv(pub_table, here("output", "tables", csv_filename), row.names = FALSE)
    
    # Save formatted table if not plain
    if (table_format %in% c("latex", "html")) {
      ext <- ifelse(table_format == "latex", "tex", "html")
      formatted_filename <- paste0(table_name, "_", format(population_size/1000, digits = 0), "k.", ext)
      
      if (table_format == "latex") {
        writeLines(as.character(formatted_table), here("output", "tables", formatted_filename))
      } else {
        save_kable(formatted_table, here("output", "tables", formatted_filename))
      }
    }
    
    cat("Table saved:\n")
    cat("- CSV:", csv_filename, "\n")
    if (table_format %in% c("latex", "html")) {
      cat("- Formatted:", paste0(table_name, "_", format(population_size/1000, digits = 0), "k.", 
                                ifelse(table_format == "latex", "tex", "html")), "\n")
    }
  }
  
  return(list(
    table_raw = pub_table,
    table_formatted = formatted_table,
    method_types = method_types,
    population_size = population_size
  ))
}

# Create sensitivity table across population sizes
create_sensitivity_table <- function(master_df, methods = NULL, indicator = "document_withholding_rds",
                                   save_table = TRUE, table_name = "sensitivity_analysis") {
  
  if (is.null(methods)) methods <- unique(master_df$method)
  
  cat("=== Creating Population Sensitivity Table ===\n")
  cat("Focus indicator:", indicator, "\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n\n")
  
  # Filter data
  sens_data <- master_df %>%
    filter(
      method %in% methods,
      indicator == !!indicator,
      !is.na(estimate)
    ) %>%
    select(pop_size, pop_label, method_clean, estimate_with_ci) %>%
    arrange(pop_size, method_clean)
  
  if (nrow(sens_data) == 0) {
    warning("No data available for sensitivity table")
    return(NULL)
  }
  
  # Create wide table
  sens_table <- sens_data %>%
    pivot_wider(
      names_from = pop_label,
      values_from = estimate_with_ci,
      id_cols = method_clean,
      values_fn = first
    ) %>%
    arrange(method_clean)
  
  # Save table
  if (save_table) {
    indicator_clean <- str_replace_all(indicator, "[^a-zA-Z0-9]", "_")
    filename <- paste0(table_name, "_", indicator_clean, ".csv")
    write.csv(sens_table, here("output", "tables", filename), row.names = FALSE)
    cat("Sensitivity table saved:", filename, "\n")
  }
  
  return(sens_table)
}

# ============================================================================
# ADVANCED PLOTTING FUNCTIONS
# ============================================================================

# Create comprehensive method comparison plot
create_method_comparison_plot <- function(master_df, methods = NULL, indicators = NULL,
                                        save_plot = TRUE, plot_name = "method_comparison") {
  
  if (is.null(methods)) methods <- unique(master_df$method)
  if (is.null(indicators)) indicators <- unique(master_df$indicator)
  
  cat("=== Creating Method Comparison Plot ===\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Indicators:", length(indicators), "\n\n")
  
  # Filter data
  plot_data <- master_df %>%
    filter(
      method %in% methods,
      indicator %in% indicators,
      !is.na(estimate)
    ) %>%
    mutate(
      method_clean = factor(method_clean),
      indicator_clean = factor(indicator_clean),
      pop_label = factor(pop_label, levels = c("50K", "100K", "980K", "1.74M"))
    )
  
  if (nrow(plot_data) == 0) {
    warning("No data available for comparison plot")
    return(NULL)
  }
  
  # Main comparison plot
  main_plot <- ggplot(plot_data, aes(x = indicator_clean, y = estimate_pct, 
                                    color = method_clean, shape = method_type)) +
    geom_point(size = 3, alpha = 0.8, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                  width = 0.2, alpha = 0.7, position = position_dodge(width = 0.6)) +
    facet_wrap(~ pop_label, nrow = 2, labeller = label_both) +
    theme_rds_publication() +
    scale_color_manual(values = get_method_colors()) +
    scale_shape_manual(values = c("frequentist" = 16, "bayesian" = 17)) +
    labs(
      title = "RDS Method Comparison Across Population Sizes",
      subtitle = paste("Methods:", paste(methods, collapse = ", ")),
      x = "Indicator", 
      y = "Prevalence Estimate (%)",
      color = "Method",
      shape = "Type",
      caption = "Error bars show 95% confidence intervals"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      strip.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  # Population sensitivity plot
  sensitivity_plot <- ggplot(plot_data, aes(x = pop_size, y = estimate_pct, 
                                           color = method_clean, linetype = indicator_clean)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    scale_x_continuous(labels = scales::comma_format(), trans = "log10") +
    theme_rds_publication() +
    scale_color_manual(values = get_method_colors()) +
    labs(
      title = "Population Size Sensitivity Analysis",
      subtitle = paste("Methods:", paste(methods, collapse = ", ")), 
      x = "Population Size (log scale)",
      y = "Prevalence Estimate (%)",
      color = "Method",
      linetype = "Indicator"
    ) +
    theme(legend.position = "bottom")
  
  # Method agreement plot (correlation matrix style)
  if (length(methods) > 1) {
    
    # Prepare data for correlation analysis
    corr_data <- plot_data %>%
      filter(pop_size == 980000) %>%  # Focus on main population size
      select(indicator, method_clean, estimate_pct) %>%
      pivot_wider(names_from = method_clean, values_from = estimate_pct) %>%
      select(-indicator)
    
    if (ncol(corr_data) > 1) {
      corr_matrix <- cor(corr_data, use = "complete.obs")
      corr_df <- expand_grid(
        Method1 = colnames(corr_matrix),
        Method2 = rownames(corr_matrix)
      ) %>%
        mutate(
          Correlation = as.vector(corr_matrix),
          Method1 = factor(Method1, levels = colnames(corr_matrix)),
          Method2 = factor(Method2, levels = rev(rownames(corr_matrix)))
        )
      
      agreement_plot <- ggplot(corr_df, aes(x = Method1, y = Method2, fill = Correlation)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3) +
        scale_fill_viridis_c(name = "Correlation", limits = c(-1, 1)) +
        theme_rds_publication() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank()
        ) +
        labs(
          title = "Method Agreement Matrix",
          subtitle = "Correlation at 980K population size"
        )
    } else {
      agreement_plot <- NULL
    }
  } else {
    agreement_plot <- NULL
  }
  
  # Save plots
  if (save_plot) {
    ggsave(here("output", "figures", paste0(plot_name, "_main.png")), 
           main_plot, width = 14, height = 10, dpi = 300)
    ggsave(here("output", "figures", paste0(plot_name, "_sensitivity.png")), 
           sensitivity_plot, width = 12, height = 8, dpi = 300)
    
    if (!is.null(agreement_plot)) {
      ggsave(here("output", "figures", paste0(plot_name, "_agreement.png")), 
             agreement_plot, width = 8, height = 6, dpi = 300)
    }
    
    cat("Plots saved:\n")
    cat("- Main comparison:", paste0(plot_name, "_main.png"), "\n")
    cat("- Sensitivity:", paste0(plot_name, "_sensitivity.png"), "\n")
    if (!is.null(agreement_plot)) {
      cat("- Agreement matrix:", paste0(plot_name, "_agreement.png"), "\n")
    }
  }
  
  return(list(
    main_plot = main_plot,
    sensitivity_plot = sensitivity_plot,
    agreement_plot = agreement_plot,
    plot_data = plot_data
  ))
}

# Create uncertainty comparison plot
create_uncertainty_comparison_plot <- function(master_df, methods = NULL, 
                                             population_size = 980000,
                                             save_plot = TRUE, plot_name = "uncertainty_comparison") {
  
  if (is.null(methods)) methods <- unique(master_df$method)
  
  cat("=== Creating Uncertainty Comparison Plot ===\n")
  cat("Methods:", paste(methods, collapse = ", "), "\n")
  cat("Population size:", format(population_size, big.mark = ","), "\n\n")
  
  # Filter data
  uncert_data <- master_df %>%
    filter(
      method %in% methods,
      pop_size == population_size,
      !is.na(estimate),
      !is.na(ci_lower_pct),
      !is.na(ci_upper_pct)
    ) %>%
    mutate(
      ci_width = ci_upper_pct - ci_lower_pct,
      method_clean = factor(method_clean),
      indicator_clean = factor(indicator_clean)
    )
  
  if (nrow(uncert_data) == 0) {
    warning("No data with confidence intervals available")
    return(NULL)
  }
  
  # CI width comparison
  ci_width_plot <- ggplot(uncert_data, aes(x = method_clean, y = ci_width, 
                                          fill = method_type)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    theme_rds_publication() +
    scale_fill_manual(values = c("frequentist" = "#E31A1C", "bayesian" = "#1F78B4")) +
    labs(
      title = "Confidence Interval Width Comparison",
      subtitle = paste("Population size:", format(population_size, big.mark = ",")),
      x = "Method",
      y = "95% CI Width (percentage points)", 
      fill = "Method Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Estimate vs uncertainty scatter
  scatter_plot <- ggplot(uncert_data, aes(x = estimate_pct, y = ci_width,
                                         color = method_clean, shape = method_type)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, alpha = 0.6) +
    theme_rds_publication() +
    scale_color_manual(values = get_method_colors()) +
    scale_shape_manual(values = c("frequentist" = 16, "bayesian" = 17)) +
    labs(
      title = "Estimate vs Uncertainty Relationship",
      subtitle = paste("Population size:", format(population_size, big.mark = ",")),
      x = "Prevalence Estimate (%)",
      y = "95% CI Width (percentage points)",
      color = "Method",
      shape = "Type"
    )
  
  # Save plots
  if (save_plot) {
    ggsave(here("output", "figures", paste0(plot_name, "_ci_width.png")), 
           ci_width_plot, width = 10, height = 8, dpi = 300)
    ggsave(here("output", "figures", paste0(plot_name, "_scatter.png")), 
           scatter_plot, width = 10, height = 8, dpi = 300)
    
    cat("Plots saved:\n")
    cat("- CI width comparison:", paste0(plot_name, "_ci_width.png"), "\n")
    cat("- Estimate vs uncertainty:", paste0(plot_name, "_scatter.png"), "\n")
  }
  
  return(list(
    ci_width_plot = ci_width_plot,
    scatter_plot = scatter_plot,
    uncertainty_data = uncert_data
  ))
}

# ============================================================================
# COMPREHENSIVE ANALYSIS WORKFLOWS
# ============================================================================

# Complete post-analysis workflow
run_complete_results_analysis <- function(methods = NULL, output_dir = here("output"),
                                        save_all = TRUE) {
  
  cat("=== COMPLETE RESULTS ANALYSIS WORKFLOW ===\n")
  cat("Loading, comparing, and visualizing all available results\n\n")
  
  # Step 1: Load all available results
  all_results <- load_all_available_results(methods = methods, output_dir = output_dir)
  
  if (nrow(all_results$master_df) == 0) {
    stop("No results available for analysis")
  }
  
  # Step 2: Method comparison analysis
  method_comparison <- compare_methods_by_indicator(all_results$master_df)
  
  # Step 3: Performance assessment  
  performance_assessment <- assess_method_performance(all_results$master_df)
  
  # Step 4: Publication table
  pub_table <- create_publication_table(all_results$master_df, save_table = save_all)
  
  # Step 5: Comprehensive plots
  comparison_plots <- create_method_comparison_plot(all_results$master_df, save_plot = save_all)
  uncertainty_plots <- create_uncertainty_comparison_plot(all_results$master_df, save_plot = save_all)
  
  # Step 6: Sensitivity analysis
  # Focus on highest confidence indicator
  top_indicator <- method_comparison$summary$indicator_clean[1]
  if (!is.null(top_indicator)) {
    # Find the actual indicator name
    top_indicator_name <- all_results$master_df$indicator[
      all_results$master_df$indicator_clean == top_indicator][1]
    
    sens_table <- create_sensitivity_table(all_results$master_df, 
                                          indicator = top_indicator_name,
                                          save_table = save_all)
  } else {
    sens_table <- NULL
  }
  
  # Compile comprehensive results
  comprehensive_results <- list(
    loaded_results = all_results,
    method_comparison = method_comparison,
    performance_assessment = performance_assessment,
    publication_table = pub_table,
    comparison_plots = comparison_plots,
    uncertainty_plots = uncertainty_plots,
    sensitivity_table = sens_table,
    analysis_timestamp = Sys.time()
  )
  
  # Save comprehensive results
  if (save_all) {
    save(comprehensive_results, file = here("output", "comprehensive_results_analysis.RData"))
    cat("\nComprehensive analysis saved: output/comprehensive_results_analysis.RData\n")
  }
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Methods analyzed:", paste(all_results$methods_loaded, collapse = ", "), "\n")
  cat("Total estimations:", all_results$total_estimations, "\n")
  cat("Results available in comprehensive_results object\n\n")
  
  return(comprehensive_results)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

print_results_analysis_examples <- function() {
  cat("=== RESULTS ANALYSIS USAGE EXAMPLES ===\n\n")
  
  cat("1. Load all available results:\n")
  cat("   all_results <- load_all_available_results()\n\n")
  
  cat("2. Load specific methods:\n")
  cat("   freq_results <- load_all_available_results(methods = c('rds_i', 'rds_ss'))\n\n")
  
  cat("3. Compare methods by indicator:\n")
  cat("   comparison <- compare_methods_by_indicator(all_results$master_df)\n\n")
  
  cat("4. Assess method performance:\n")
  cat("   performance <- assess_method_performance(all_results$master_df)\n\n")
  
  cat("5. Create publication table:\n")
  cat("   pub_table <- create_publication_table(all_results$master_df, table_format = 'latex')\n\n")
  
  cat("6. Create comparison plots:\n")
  cat("   plots <- create_method_comparison_plot(all_results$master_df)\n\n")
  
  cat("7. Run complete analysis workflow:\n")
  cat("   comprehensive <- run_complete_results_analysis()\n\n")
  
  cat("8. Detect available results:\n")
  cat("   available <- detect_available_results()\n\n")
  
  cat("9. Load single method:\n")
  cat("   rds_ss_data <- load_method_results('rds_ss')\n\n")
}

# ============================================================================
# EXECUTION
# ============================================================================

# Print usage examples when file is loaded
if (!exists("skip_execution") || !skip_execution) {
  print_results_analysis_examples()
} else {
  cat("RESULTS analysis and comparison tools loaded\n")
  cat("Use print_results_analysis_examples() to see usage examples\n")
}




all_results <- load_all_available_results()


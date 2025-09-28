# 07-comprehensive_comparison.R  
# Comprehensive Results Comparison Across All Production Estimators
# PRODUCTION VERSION: Clean comparison of 06a + 06b + 06c results
#
# COMPARES:
# - 06a: Frequentist RDS methods (RDS-I, RDS-II, RDS-SS) with bootstrap CIs
# - 06b: Core Bayesian MA.estimates with adaptive parameters  
# - 06c: Bayesian sensitivity analysis (population × seed selection)
#
# OUTPUTS:
# - Method comparison visualizations
# - Publication-ready tables with confidence intervals
# - Population sensitivity analysis
# - Parameter sensitivity analysis



# Create comprehensive color palette for all method names
method_colors <- c(
  "RDS-I" = "#E31A1C",
  "RDS-II" = "#1F78B4",
  "RDS-SS" = "#33A02C",
  "Bayesian MA" = "#FF7F00",
  "Bayesian PS" = "#6A3D9A",
  "MA.estimates" = "#FF7F00",    # Legacy name mapping
  "posteriorsize" = "#6A3D9A",   # Legacy name mapping
  "RDS_I" = "#E31A1C",          # Alternative naming
  "RDS_II" = "#1F78B4",         # Alternative naming
  "RDS_SS" = "#33A02C",         # Alternative naming
  "RDS-I_boot" = "#E31A1C",     # Bootstrap variants
  "RDS-II_boot" = "#1F78B4",    # Bootstrap variants
  "RDS-SS_boot" = "#33A02C"     # Bootstrap variants
)


cat("=== COMPREHENSIVE RESULTS COMPARISON (Production) ===\n")
cat("Comparing results from 06a (Frequentist) + 06b (Bayesian) + 06c (Sensitivity)\n")
cat("Outputs: Visualizations + Publication tables\n\n")

# Load required libraries
library(tidyverse)
library(here)
library(scales)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(gt)
library(kableExtra)
library(knitr)

# Source helper functions
source(here("R", "utils", "helper_functions.R"))

# ============================================================================
# LOAD ALL PRODUCTION RESULTS
# ============================================================================

load_production_results <- function() {
  
  cat("=== Loading Production Results ===\n")
  
  all_results <- list()
  
  # 1. Load Frequentist Results (06a)
  freq_file <- here("output", "frequentist_estimators_results.RData")
  if (file.exists(freq_file)) {
    cat("Loading frequentist results (06a)...\n")
    freq_env <- new.env()
    load(freq_file, envir = freq_env)
    
    if (exists("frequentist_df", envir = freq_env)) {
      freq_df <- get("frequentist_df", envir = freq_env)
      freq_df$analysis_source <- "06a_frequentist"
      all_results$frequentist <- freq_df
      cat("- Frequentist:", nrow(freq_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Frequentist results not found\n")
  }
  
  # 2. Load Core Bayesian Results (06b)
  bayes_file <- here("output", "bayesian_estimators_results.RData")
  if (file.exists(bayes_file)) {
    cat("Loading core Bayesian results (06b)...\n")
    bayes_env <- new.env()
    load(bayes_file, envir = bayes_env)
    
    if (exists("bayesian_df", envir = bayes_env)) {
      bayes_df <- get("bayesian_df", envir = bayes_env)
      bayes_df$analysis_source <- "06b_bayesian_core"
      # Add missing columns for compatibility
      if (!"pop_label" %in% names(bayes_df)) bayes_df$pop_label <- "980K"
      if (!"seed_selection" %in% names(bayes_df)) bayes_df$seed_selection <- "adaptive"
      all_results$bayesian_core <- bayes_df
      cat("- Core Bayesian:", nrow(bayes_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Core Bayesian results not found\n")
  }
  
  # 3. Load Sensitivity Analysis Results (06c)
  sens_file <- here("output", "bayesian_sensitivity_results.RData")
  if (file.exists(sens_file)) {
    cat("Loading Bayesian sensitivity results (06c)...\n")
    sens_env <- new.env()
    load(sens_file, envir = sens_env)
    
    if (exists("sensitivity_df", envir = sens_env)) {
      sens_df <- get("sensitivity_df", envir = sens_env)
      sens_df$analysis_source <- "06c_bayesian_sensitivity"
      all_results$bayesian_sensitivity <- sens_df
      cat("- Sensitivity analysis:", nrow(sens_df), "results loaded\n")
    }
  } else {
    cat("WARNING: Sensitivity analysis results not found\n")
  }
  
  cat("Production results loaded successfully!\n\n")
  return(all_results)
}

# ============================================================================
# UNIFIED DATA PROCESSING
# ============================================================================

create_unified_dataset <- function(results_list) {
  
  cat("=== Creating Unified Dataset ===\n")
  
  # Standardize column names across all datasets
  standardize_columns <- function(df, source_name) {
    
    # Ensure required columns exist
    required_cols <- c("indicator", "method", "estimate", "ci_lower", "ci_upper", 
                      "method_type", "indicator_clean", "method_clean", 
                      "estimate_pct", "ci_lower_pct", "ci_upper_pct", "estimate_with_ci")
    
    # Add missing columns with defaults
    for (col in required_cols) {
      if (!col %in% names(df)) {
        if (col %in% c("indicator_clean", "method_clean")) {
          df[[col]] <- df[[str_remove(col, "_clean")]] # Use base column if clean version missing
        } else if (col %in% c("estimate_pct", "ci_lower_pct", "ci_upper_pct")) {
          base_col <- str_remove(col, "_pct")
          if (base_col %in% names(df)) {
            df[[col]] <- df[[base_col]] * 100
          } else {
            df[[col]] <- NA
          }
        } else {
          df[[col]] <- NA
        }
      }
    }
    
    # Standardize method names
    df <- df %>%
      mutate(
        method_clean = case_when(
          str_detect(method, "RDS_I|RDS-I") ~ "RDS-I",
          str_detect(method, "RDS_II|RDS-II") ~ "RDS-II",
          str_detect(method, "RDS_SS|RDS-SS") ~ "RDS-SS",
          str_detect(method, "MA_estimates|MA.estimates") ~ "Bayesian MA",
          str_detect(method, "posteriorsize") ~ "Bayesian PS",
          TRUE ~ method
        ),
        method_type = case_when(
          method_clean %in% c("RDS-I", "RDS-II", "RDS-SS") ~ "frequentist",
          method_clean %in% c("Bayesian MA", "Bayesian PS") ~ "bayesian",
          TRUE ~ method_type
        )
      )
    
    # Add source identifier
    df$source <- source_name
    
    return(df)
  }
  
  # Standardize all datasets
  standardized_results <- map2(results_list, names(results_list), standardize_columns)
  
  # Combine into unified dataset
  unified_df <- bind_rows(standardized_results)
  
  # Add additional derived columns
  unified_df <- unified_df %>%
    mutate(
      # Population size grouping
      pop_group = case_when(
        is.na(pop_label) ~ "Single",
        pop_label == "50K" ~ "Conservative",
        pop_label == "100K" ~ "Conservative", 
        pop_label == "980K" ~ "Baseline",
        pop_label == "1.74M" ~ "Liberal",
        TRUE ~ "Other"
      ),
      
      # Method category
      method_category = case_when(
        method_clean %in% c("RDS-I", "RDS-II", "RDS-SS") ~ "Frequentist RDS",
        method_clean %in% c("Bayesian MA", "Bayesian PS") ~ "Bayesian ERGM",
        TRUE ~ "Other"
      ),
      
      # Parameter type (for Bayesian methods)
      param_group = case_when(
        !is.na(parameter_type) ~ parameter_type,
        source == "bayesian_core" ~ "adaptive",
        TRUE ~ "standard"
      ),
      
      # Confidence interval width
      ci_width = ci_upper_pct - ci_lower_pct,
      
      # Success indicator
      has_estimate = !is.na(estimate) & !is.na(ci_lower) & !is.na(ci_upper)
    )
  
  cat("Unified dataset created:\n")
  cat("- Total results:", nrow(unified_df), "\n")
  cat("- Methods:", paste(unique(unified_df$method_clean), collapse = ", "), "\n")
  cat("- Sources:", paste(unique(unified_df$source), collapse = ", "), "\n")
  cat("- Indicators:", length(unique(unified_df$indicator_clean)), "\n")
  cat("- Successful estimates:", sum(unified_df$has_estimate, na.rm = TRUE), "\n\n")
  
  return(unified_df)
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

# Method comparison plot
create_method_comparison_plot <- function(unified_df) {

  cat("Creating method comparison visualization...\n")

  # Filter to 980K baseline for main comparison
  plot_data <- unified_df %>%
    filter(has_estimate,
           pop_label %in% c("980K", NA),  # Include single population results
           !is.na(indicator_clean)) %>%
    # Take one result per method-indicator combination (avoid duplicates)
    group_by(indicator_clean, method_clean) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    # Add position dodge for horizontal spacing
    mutate(
      # Create interval type labels
      interval_type = case_when(
        method_type == "bayesian" ~ "95% Credible Interval",
        method_type == "frequentist" ~ "95% Confidence Interval",
        TRUE ~ "95% Interval"
      ),
      # Create method ordering for consistent dodging
      method_order = case_when(
        method_clean == "RDS-I" ~ 1,
        method_clean == "RDS-II" ~ 2,
        method_clean == "RDS-SS" ~ 3,
        method_clean == "Bayesian MA" ~ 4,
        method_clean == "Bayesian PS" ~ 5,
        TRUE ~ 6
      )
    ) %>%
    arrange(indicator_clean, method_order)

  # Check what methods we actually have and add colors if needed
  unique_methods <- unique(plot_data$method_clean)
  cat("Methods found in method comparison:", paste(unique_methods, collapse = ", "), "\n")

  # Use the global color palette and add any missing methods
  method_colors_plot <- method_colors
  missing_methods <- setdiff(unique_methods, names(method_colors_plot))
  if (length(missing_methods) > 0) {
    cat("Adding colors for missing methods:", paste(missing_methods, collapse = ", "), "\n")
    additional_colors <- viridis::viridis(length(missing_methods))
    names(additional_colors) <- missing_methods
    method_colors_plot <- c(method_colors_plot, additional_colors)
  }

  # Main comparison plot with position dodging
  p1 <- ggplot(plot_data, aes(x = reorder(indicator_clean, estimate_pct),
                              y = estimate_pct,
                              color = method_clean)) +
    geom_point(size = 3, alpha = 0.8,
               position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
                  width = 0.2, alpha = 0.7,
                  position = position_dodge(width = 0.6)) +
    scale_color_manual(values = method_colors_plot,
                       name = "Estimation Method") +
    theme_rds_publication() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(
      title = "Method Comparison: RDS vs Bayesian Estimators",
      subtitle = "Baseline population (980K) with 95% confidence/credible intervals",
      x = "Exploitation Indicator",
      y = "Prevalence Estimate (%)",
      color = "Method",
      caption = "Note: RDS methods show 95% confidence intervals (bootstrap); Bayesian methods show 95% credible intervals (MCMC)"
    )

  return(p1)
}

# Population sensitivity plot
create_population_sensitivity_plot <- function(unified_df) {

  cat("Creating population sensitivity visualization...\n")

  # Filter to data with population variation
  cat("DEBUG: Starting with", nrow(unified_df), "total rows\n")

  plot_data <- unified_df %>%
    filter(has_estimate) %>%
    {cat("After has_estimate filter:", nrow(.), "rows\n"); .} %>%
    filter(!is.na(pop_label)) %>%
    {cat("After pop_label filter:", nrow(.), "rows\n"); .} %>%
    filter(!is.na(population_size)) %>%
    {cat("After population_size filter:", nrow(.), "rows\n"); .} %>%
    group_by(indicator_clean, method_clean, population_size) %>%
    slice_head(n = 1) %>%  # Remove duplicates
    ungroup()

  cat("Final plot_data rows:", nrow(plot_data), "\n")

  if (nrow(plot_data) == 0) {
    cat("No population sensitivity data available\n")
    cat("Available pop_labels:", paste(unique(unified_df$pop_label), collapse = ", "), "\n")
    cat("Available population_sizes:", paste(unique(unified_df$population_size), collapse = ", "), "\n")
    return(NULL)
  }

  # Comprehensive color palette to handle all possible method names
  method_colors_updated <- c(
    "RDS-I" = "#E31A1C",
    "RDS-II" = "#1F78B4",
    "RDS-SS" = "#33A02C",
    "Bayesian MA" = "#FF7F00",
    "Bayesian PS" = "#6A3D9A",
    "MA.estimates" = "#FF7F00",    # Legacy name mapping
    "posteriorsize" = "#6A3D9A",   # Legacy name mapping
    "RDS_I" = "#E31A1C",          # Alternative naming
    "RDS_II" = "#1F78B4",         # Alternative naming
    "RDS_SS" = "#33A02C",         # Alternative naming
    "RDS-I_boot" = "#E31A1C",     # Bootstrap variants
    "RDS-II_boot" = "#1F78B4",    # Bootstrap variants
    "RDS-SS_boot" = "#33A02C"     # Bootstrap variants
  )

  # Check what methods and indicators we actually have
  unique_methods <- unique(plot_data$method_clean)
  unique_indicators <- unique(plot_data$indicator_clean)
  cat("Methods found in data:", paste(unique_methods, collapse = ", "), "\n")
  cat("Indicators found in data:", paste(unique_indicators, collapse = ", "), "\n")
  cat("Number of methods:", length(unique_methods), "\n")
  cat("Number of indicators:", length(unique_indicators), "\n")

  # Add any missing methods to color palette with default colors
  missing_methods <- setdiff(unique_methods, names(method_colors_updated))
  if (length(missing_methods) > 0) {
    cat("Adding colors for missing methods:", paste(missing_methods, collapse = ", "), "\n")
    # Use viridis colors for any missing methods
    additional_colors <- viridis::viridis(length(missing_methods))
    names(additional_colors) <- missing_methods
    method_colors_updated <- c(method_colors_updated, additional_colors)
  }

  # Create enough linetype values for all indicators
  n_indicators <- length(unique_indicators)
  linetype_values <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "F1", "4C88C488")
  if (n_indicators > length(linetype_values)) {
    # Add more linetypes if needed
    additional_linetypes <- rep(c("solid", "dashed"), ceiling(n_indicators/2))
    linetype_values <- c(linetype_values, additional_linetypes)
  }
  linetype_values <- linetype_values[1:n_indicators]
  names(linetype_values) <- unique_indicators

  # Try a simpler approach: just use color and shape to avoid scale conflicts
  p2 <- ggplot(plot_data, aes(x = population_size, y = estimate_pct,
                              color = method_clean)) +
    geom_line(aes(group = interaction(method_clean, indicator_clean)),
              size = 1, alpha = 0.7) +
    geom_point(aes(shape = indicator_clean),
               size = 3, alpha = 0.9) +
    scale_x_log10(labels = scales::comma_format()) +
    scale_color_manual(values = method_colors_updated,
                       name = "Estimation Method") +
    scale_shape_manual(values = 1:length(unique_indicators),
                       name = "Indicator") +
    theme_rds_publication() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10)
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    ) +
    labs(
      title = "Population Size Sensitivity Analysis",
      subtitle = "How prevalence estimates vary with assumed population size",
      x = "Population Size (log scale)",
      y = "Prevalence Estimate (%)",
      color = "Method",
      shape = "Indicator",
      caption = "Note: Lines connect estimates across population scenarios; shapes distinguish indicators"
    )

  return(p2)
}

# Parameter comparison plot (for Bayesian methods)
create_parameter_comparison_plot <- function(unified_df) {
  
  cat("Creating parameter comparison visualization...\n")
  
  # Filter to Bayesian results with parameter information
  plot_data <- unified_df %>%
    filter(has_estimate,
           method_clean %in% c("Bayesian MA", "Bayesian PS"),
           !is.na(param_group)) %>%
    group_by(indicator_clean, param_group) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  if (nrow(plot_data) == 0) {
    cat("No parameter comparison data available\n")
    return(NULL)
  }
  
  # Parameter comparison plot
  p3 <- ggplot(plot_data, aes(x = indicator_clean, y = estimate_pct, 
                              fill = param_group)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
                  position = position_dodge(width = 0.9), width = 0.2) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    theme_rds_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Bayesian Parameter Sensitivity Analysis",
      subtitle = "Enhanced vs Standard MCMC parameters across indicators",
      x = "Indicator",
      y = "Prevalence Estimate (%)",
      fill = "Parameter Type"
    )
  
  return(p3)
}

# ============================================================================
# PUBLICATION TABLES
# ============================================================================

# Main comparison table with professional formatting
create_main_comparison_table <- function(unified_df, format = "gt") {

  cat("Creating main comparison table...\n")

  # Main comparison (980K baseline)
  main_table_data <- unified_df %>%
    filter(has_estimate,
           pop_label %in% c("980K", NA)) %>%
    group_by(indicator_clean, method_clean) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, method_clean, estimate_with_ci, method_type,
           source, param_group) %>%
    pivot_wider(names_from = method_clean, values_from = estimate_with_ci) %>%
    arrange(indicator_clean) %>%
    rename("Indicator" = indicator_clean)

  if (format == "gt") {
    # Create GT table
    gt_table <- main_table_data %>%
      select(-method_type, -source, -param_group) %>%
      gt() %>%
      tab_header(
        title = "Method Comparison: RDS vs Bayesian Estimators",
        subtitle = "Prevalence estimates (%) with 95% confidence/credible intervals"
      ) %>%
      tab_spanner(
        label = "Estimation Methods",
        columns = -Indicator
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_title()
      ) %>%
      tab_footnote(
        footnote = "Population size: 980,000 (baseline scenario)",
        locations = cells_title()
      ) %>%
      cols_align(
        align = "center",
        columns = -Indicator
      ) %>%
      cols_align(
        align = "left",
        columns = Indicator
      ) %>%
      tab_options(
        table.font.size = 12,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 12
      )

    return(gt_table)

  } else if (format == "kable") {
    # Create kable table
    kable_table <- main_table_data %>%
      select(-method_type, -source, -param_group) %>%
      kable(
        caption = "Method Comparison: RDS vs Bayesian Estimators",
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        align = c("l", rep("c", ncol(.) - 1))
      ) %>%
      kable_styling(
        latex_options = c("striped", "hold_position"),
        font_size = 12
      ) %>%
      add_header_above(c(" " = 1, "Estimation Methods" = ncol(main_table_data) - 4)) %>%
      footnote(
        general = "Population size: 980,000 (baseline scenario). Values show prevalence estimates (%) with 95% confidence intervals (RDS methods) or credible intervals (Bayesian methods).",
        general_title = "Note:",
        footnote_as_chunk = TRUE
      )

    return(kable_table)
  }

  # Return raw data if format not recognized
  return(main_table_data)
}

# Population sensitivity table with professional formatting
create_population_table <- function(unified_df, format = "gt") {

  cat("Creating population sensitivity table...\n")

  pop_table_data <- unified_df %>%
    filter(has_estimate, !is.na(pop_label)) %>%
    group_by(indicator_clean, method_clean, pop_label) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, method_clean, pop_label, estimate_with_ci) %>%
    pivot_wider(names_from = pop_label, values_from = estimate_with_ci) %>%
    arrange(indicator_clean, method_clean) %>%
    rename("Indicator" = indicator_clean, "Method" = method_clean)

  if (format == "gt") {
    # Create GT table
    gt_table <- pop_table_data %>%
      gt(groupname_col = "Indicator") %>%
      tab_header(
        title = "Population Size Sensitivity Analysis",
        subtitle = "Prevalence estimates (%) across different population assumptions"
      ) %>%
      tab_spanner(
        label = "Population Size Scenarios",
        columns = -c(Indicator, Method)
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
      ) %>%
      cols_align(
        align = "center",
        columns = -c(Indicator, Method)
      ) %>%
      cols_align(
        align = "left",
        columns = Method
      ) %>%
      tab_footnote(
        footnote = "50K = Conservative, 100K = Conservative, 980K = Baseline (EU data), 1.74M = Liberal scenario",
        locations = cells_column_spanners()
      ) %>%
      tab_options(
        table.font.size = 11,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 12,
        row_group.font.weight = "bold"
      )

    return(gt_table)

  } else if (format == "kable") {
    # Create kable table
    kable_table <- pop_table_data %>%
      kable(
        caption = "Population Size Sensitivity Analysis",
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        align = c("l", "l", rep("c", ncol(.) - 2))
      ) %>%
      kable_styling(
        latex_options = c("striped", "hold_position", "scale_down"),
        font_size = 11
      ) %>%
      add_header_above(c(" " = 2, "Population Size Scenarios" = ncol(pop_table_data) - 2)) %>%
      collapse_rows(columns = 1, latex_hline = "major") %>%
      footnote(
        general = "50K = Conservative, 100K = Conservative, 980K = Baseline (EU data), 1.74M = Liberal scenario. Values show prevalence estimates (%) with 95% confidence intervals (RDS methods) or credible intervals (Bayesian methods).",
        general_title = "Note:",
        footnote_as_chunk = TRUE
      )

    return(kable_table)
  }

  # Return raw data if format not recognized
  return(pop_table_data)
}

# Parameter sensitivity table (Bayesian only) with professional formatting
create_parameter_table <- function(unified_df, format = "gt") {

  cat("Creating parameter sensitivity table...\n")

  param_table_data <- unified_df %>%
    filter(has_estimate,
           method_clean %in% c("Bayesian MA", "Bayesian PS"),
           !is.na(param_group)) %>%
    group_by(indicator_clean, param_group) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(indicator_clean, param_group, estimate_with_ci,
           iterations, M1, M2) %>%
    mutate(param_description = paste0(param_group, " (", iterations, " iter, M1=", M1, ", M2=", M2, ")")) %>%
    select(indicator_clean, param_description, estimate_with_ci) %>%
    pivot_wider(names_from = param_description, values_from = estimate_with_ci) %>%
    arrange(indicator_clean) %>%
    rename("Indicator" = indicator_clean)

  if (format == "gt") {
    # Create GT table
    gt_table <- param_table_data %>%
      gt() %>%
      tab_header(
        title = "Bayesian Parameter Sensitivity Analysis",
        subtitle = "Enhanced vs Standard MCMC parameters for Bayesian estimation"
      ) %>%
      tab_spanner(
        label = "Parameter Configurations",
        columns = -Indicator
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_title()
      ) %>%
      cols_align(
        align = "center",
        columns = -Indicator
      ) %>%
      cols_align(
        align = "left",
        columns = Indicator
      ) %>%
      tab_footnote(
        footnote = "Enhanced parameters use longer burn-in (SAN.nsteps = 2^22) and more iterations for numeric/ordinal variables",
        locations = cells_column_spanners()
      ) %>%
      tab_options(
        table.font.size = 11,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 12
      )

    return(gt_table)

  } else if (format == "kable") {
    # Create kable table
    kable_table <- param_table_data %>%
      kable(
        caption = "Bayesian Parameter Sensitivity Analysis",
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        align = c("l", rep("c", ncol(.) - 1))
      ) %>%
      kable_styling(
        latex_options = c("striped", "hold_position"),
        font_size = 11
      ) %>%
      add_header_above(c(" " = 1, "Parameter Configurations" = ncol(param_table_data) - 1)) %>%
      footnote(
        general = "Enhanced parameters use longer burn-in (SAN.nsteps = 2^22) and more iterations for numeric/ordinal variables. All values show prevalence estimates (%) with 95% Bayesian credible intervals.",
        general_title = "Note:",
        footnote_as_chunk = TRUE
      )

    return(kable_table)
  }

  # Return raw data if format not recognized
  return(param_table_data)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Load all production results
production_results <- load_production_results()

# Create unified dataset
unified_data <- create_unified_dataset(production_results)

# Create visualizations
cat("=== Creating Visualizations ===\n")

method_plot <- create_method_comparison_plot(unified_data)
population_plot <- create_population_sensitivity_plot(unified_data)
parameter_plot <- create_parameter_comparison_plot(unified_data)

# Create publication tables
cat("=== Creating Publication Tables ===\n")

# Create both GT and Kable versions for flexibility
main_comparison_table_gt <- create_main_comparison_table(unified_data, format = "gt")
main_comparison_table_kable <- create_main_comparison_table(unified_data, format = "kable")

population_sensitivity_table_gt <- create_population_table(unified_data, format = "gt")
population_sensitivity_table_kable <- create_population_table(unified_data, format = "kable")

parameter_sensitivity_table_gt <- create_parameter_table(unified_data, format = "gt")
parameter_sensitivity_table_kable <- create_parameter_table(unified_data, format = "kable")

# Raw data tables for CSV export
main_comparison_table <- create_main_comparison_table(unified_data, format = "raw")
population_sensitivity_table <- create_population_table(unified_data, format = "raw")
parameter_sensitivity_table <- create_parameter_table(unified_data, format = "raw")

# Save visualizations
cat("=== Saving Outputs ===\n")

if (!is.null(method_plot)) {
  ggsave(here("output", "figures", "method_comparison.png"), 
         method_plot, width = 12, height = 8, dpi = 300)
  cat("Saved: method_comparison.png\n")
}

if (!is.null(population_plot)) {
  ggsave(here("output", "figures", "population_sensitivity.png"), 
         population_plot, width = 12, height = 8, dpi = 300)
  cat("Saved: population_sensitivity.png\n")
}

if (!is.null(parameter_plot)) {
  ggsave(here("output", "figures", "parameter_comparison.png"), 
         parameter_plot, width = 10, height = 6, dpi = 300)
  cat("Saved: parameter_comparison.png\n")
}

# Save tables in multiple formats
cat("Saving tables in CSV, HTML, and LaTeX formats...\n")

# CSV tables (raw data)
write.csv(main_comparison_table, here("output", "tables", "main_comparison.csv"), row.names = FALSE)
write.csv(population_sensitivity_table, here("output", "tables", "population_sensitivity_full.csv"), row.names = FALSE)
write.csv(parameter_sensitivity_table, here("output", "tables", "parameter_sensitivity_comparison.csv"), row.names = FALSE)
write.csv(unified_data, here("output", "tables", "unified_results.csv"), row.names = FALSE)

# GT tables (HTML format for viewing/Quarto)
if (!is.null(main_comparison_table_gt)) {
  gtsave(main_comparison_table_gt, here("output", "tables", "main_comparison_gt.html"))
  gtsave(main_comparison_table_gt, here("output", "tables", "main_comparison_gt.tex"))
}

if (!is.null(population_sensitivity_table_gt)) {
  gtsave(population_sensitivity_table_gt, here("output", "tables", "population_sensitivity_gt.html"))
  gtsave(population_sensitivity_table_gt, here("output", "tables", "population_sensitivity_gt.tex"))
}

if (!is.null(parameter_sensitivity_table_gt)) {
  gtsave(parameter_sensitivity_table_gt, here("output", "tables", "parameter_sensitivity_gt.html"))
  gtsave(parameter_sensitivity_table_gt, here("output", "tables", "parameter_sensitivity_gt.tex"))
}

# Kable tables (LaTeX format)
if (!is.null(main_comparison_table_kable)) {
  writeLines(as.character(main_comparison_table_kable),
             here("output", "tables", "main_comparison_kable.tex"))
}

if (!is.null(population_sensitivity_table_kable)) {
  writeLines(as.character(population_sensitivity_table_kable),
             here("output", "tables", "population_sensitivity_kable.tex"))
}

if (!is.null(parameter_sensitivity_table_kable)) {
  writeLines(as.character(parameter_sensitivity_table_kable),
             here("output", "tables", "parameter_sensitivity_kable.tex"))
}

# Save complete results
save(production_results, unified_data,
     main_comparison_table, population_sensitivity_table, parameter_sensitivity_table,
     main_comparison_table_gt, population_sensitivity_table_gt, parameter_sensitivity_table_gt,
     main_comparison_table_kable, population_sensitivity_table_kable, parameter_sensitivity_table_kable,
     method_plot, population_plot, parameter_plot,
     file = here("output", "comprehensive_comparison_results.RData"))

# Summary report
cat("\n=== COMPREHENSIVE COMPARISON COMPLETE ===\n")
cat("Results processed from:\n")
for(source in names(production_results)) {
  if(!is.null(production_results[[source]])) {
    cat("- ", source, ":", nrow(production_results[[source]]), "results\n")
  }
}

cat("\nUnified dataset:\n")
cat("- Total results:", nrow(unified_data), "\n")
cat("- Successful estimates:", sum(unified_data$has_estimate, na.rm = TRUE), "\n")
cat("- Methods:", paste(unique(unified_data$method_clean), collapse = ", "), "\n")
cat("- Indicators:", length(unique(unified_data$indicator_clean)), "\n")

cat("\nOutput files created:\n")
cat("- Visualizations: output/figures/\n")
cat("  * method_comparison.png\n")
cat("  * population_sensitivity.png\n")
cat("  * parameter_comparison.png\n")
cat("- Tables: output/tables/\n")
cat("  * CSV format: main_comparison.csv, population_sensitivity_full.csv, parameter_sensitivity_comparison.csv\n")
cat("  * GT format: *_gt.html, *_gt.tex (for Quarto/LaTeX)\n")
cat("  * Kable format: *_kable.tex (for LaTeX inclusion)\n")
cat("  * Unified data: unified_results.csv\n")
cat("- Complete results: comprehensive_comparison_results.RData\n\n")

cat("=== READY FOR PUBLICATION! ===\n")
cat("All method comparisons, sensitivity analyses, and publication tables complete.\n")
<<<<<<< HEAD
cat("Use the visualization plots and CSV tables for your manuscript.\n")

=======
cat("Use the following for your Quarto manuscript:\n")
cat("- GT tables: Include .html files in Quarto or use .tex files for LaTeX\n")
cat("- Kable tables: Direct LaTeX inclusion with .tex files\n")
cat("- Visualizations: PNG files for figures\n")
cat("- Raw data: CSV files for custom formatting\n")
>>>>>>> 29180d1afe306503cf53b767e3310e341951083c

# 04-nsum_estimation.R
# Network Scale-Up Method (NSUM) estimation using RDS weights
# Estimates hidden population sizes using alter-centric network questions

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(here)

# Load prepared data
if (!exists("prepared_data")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Core NSUM estimation function
estimate_nsum_population <- function(data, weights, hidden_connections_var, 
                                   degree_vars, probe_sizes, 
                                   total_population_size = 980000,
                                   method = "basic") {
  
  # Input validation
  if (length(degree_vars) != length(probe_sizes)) {
    stop("degree_vars and probe_sizes must have the same length")
  }
  
  # Remove missing values for this estimation
  complete_cases <- complete.cases(data[[hidden_connections_var]], weights)
  data_complete <- data[complete_cases, ]
  weights_complete <- weights[complete_cases]
  
  if (nrow(data_complete) == 0) {
    return(list(estimate = NA, visibility = NA, degree_avg = NA, method = method))
  }
  
  # Calculate visibility: y_F,H (weighted average connections to hidden population)
  y_F_H <- sum(data_complete[[hidden_connections_var]] * weights_complete, na.rm = TRUE) / 
           sum(weights_complete, na.rm = TRUE)
  
  # Calculate average degree: d_F,F (weighted average degree to known populations)
  degree_estimates <- numeric(length(degree_vars))
  
  for (i in seq_along(degree_vars)) {
    if (degree_vars[i] %in% names(data_complete)) {
      degree_estimates[i] <- sum(data_complete[[degree_vars[i]]] * weights_complete, na.rm = TRUE) / 
                            sum(weights_complete, na.rm = TRUE) / probe_sizes[i]
    } else {
      degree_estimates[i] <- NA
    }
  }
  
  # Use different methods for combining degree estimates
  if (method == "mean") {
    d_F_F <- mean(degree_estimates, na.rm = TRUE)
  } else if (method == "median") {
    d_F_F <- median(degree_estimates, na.rm = TRUE)
  } else {
    # Default: arithmetic mean
    d_F_F <- mean(degree_estimates, na.rm = TRUE)
  }
  
  # NSUM estimate: N_H = (y_F,H / d_F,F) * N_total
  visibility_fraction <- y_F_H / d_F_F
  population_estimate <- visibility_fraction * total_population_size
  
  return(list(
    estimate = population_estimate,
    visibility = y_F_H,
    degree_avg = d_F_F,
    visibility_fraction = visibility_fraction,
    degree_components = degree_estimates,
    n_observations = nrow(data_complete),
    method = method
  ))
}

# Run comprehensive NSUM estimation
run_nsum_estimation <- function(
  outcome_vars = c("document_withholding_nsum", "pay_issues_nsum", "threats_abuse_nsum",
                   "excessive_hours_nsum", "access_to_help_nsum"),
  population_sizes = c(50000, 100000, 980000, 1740000),
  weight_methods = c("SS_980k", "SS_100k", "SS_050k", "vh_980k", "vh_100k", "RDS1_document_withholding"),
  degree_methods = c("basic", "mean", "median")
) {
  
  cat("Starting NSUM estimation analysis...\n")
  
  # Set up degree variables and probe sizes
  # Q13: Number of domestic workers with contact details (network size)
  # Q71, Q43, Q49, Q64, Q79: NSUM alter-centric questions  
  degree_vars <- c("q13")
  probe_sizes <- c(980000)  # Total domestic worker population estimate
  
  # Prepare weight schemes
  weight_schemes <- list()
  
  # Add available weight columns
  available_weights <- c("wt.SS_980k", "wt.SS_100k", "wt.SS_050k", "wt.SS_1740k",
                        "wt.vh_980k", "wt.vh_100k", "wt.vh_050k", "wt.vh_1740k",
                        "wt.RDS1_document_withholding", "wt.RDS1_pay_issues", 
                        "wt.RDS1_threats_abuse", "wt.RDS1_excessive_hours", "wt.RDS1_access_to_help")
  
  for (weight_name in weight_methods) {
    weight_col <- paste0("wt.", weight_name)
    if (weight_col %in% names(dd)) {
      weight_schemes[[weight_name]] <- dd[[weight_col]]
    } else {
      cat("Warning: Weight", weight_col, "not found in data\n")
    }
  }
  
  # Set up NSUM variable mappings (CE's alter-centric questions)
  nsum_vars <- list(
    document_withholding = list(
      var = "q71", 
      name = "document_withholding_nsum",
      description = "Know others without access to documents (Q71)"
    ),
    pay_issues = list(
      var = "q43",
      name = "pay_issues_nsum", 
      description = "Know others with debt/pay problems (Q43)"
    ),
    threats_abuse = list(
      var = "q49",
      name = "threats_abuse_nsum",
      description = "Know others with threat/force experiences (Q49)"
    ),
    excessive_hours = list(
      var = "q64",
      name = "excessive_hours_nsum",
      description = "Know others with labour rights issues (Q64)"
    ),
    access_to_help = list(
      var = "q79", 
      name = "access_to_help_nsum",
      description = "Know others who don't know where to get help (Q79)"
    )
  )
  
  # Run estimation for all combinations
  results <- list()
  
  for (nsum_name in names(nsum_vars)) {
    cat("Processing NSUM variable:", nsum_name, "\n")
    
    nsum_var_info <- nsum_vars[[nsum_name]]
    hidden_var <- nsum_var_info$var
    
    # Check if variable exists
    if (!hidden_var %in% names(dd)) {
      cat("  Warning: Variable", hidden_var, "not found in data, skipping\n")
      next
    }
    
    results[[nsum_name]] <- list()
    results[[nsum_name]]$metadata <- nsum_var_info
    
    for (pop_size in population_sizes) {
      results[[nsum_name]][[paste0("N_", pop_size)]] <- list()
      
      for (weight_name in names(weight_schemes)) {
        results[[nsum_name]][[paste0("N_", pop_size)]][[weight_name]] <- list()
        
        for (method in degree_methods) {
          tryCatch({
            estimate <- estimate_nsum_population(
              data = dd,
              weights = weight_schemes[[weight_name]],
              hidden_connections_var = hidden_var,
              degree_vars = degree_vars,
              probe_sizes = probe_sizes,
              total_population_size = pop_size,
              method = method
            )
            
            results[[nsum_name]][[paste0("N_", pop_size)]][[weight_name]][[method]] <- estimate
            
          }, error = function(e) {
            cat("    Error with", nsum_name, "N =", pop_size, "weight =", weight_name, 
                "method =", method, ":", e$message, "\n")
            results[[nsum_name]][[paste0("N_", pop_size)]][[weight_name]][[method]] <- list(
              estimate = NA, error = e$message
            )
          })
        }
      }
    }
  }
  
  # Create summary visualizations
  cat("Creating summary visualizations...\n")
  plots <- create_nsum_plots(results, weight_schemes)
  
  # Create summary tables
  summary_table <- create_nsum_summary_table(results)
  
  # Compile final results
  final_results <- list(
    estimates = results,
    summary_table = summary_table,
    plots = plots,
    metadata = list(
      run_date = Sys.time(),
      outcome_vars = names(nsum_vars),
      population_sizes = population_sizes,
      weight_methods = names(weight_schemes),
      degree_methods = degree_methods,
      degree_vars = degree_vars,
      probe_sizes = probe_sizes,
      n_observations = nrow(dd),
      r_version = R.version.string
    )
  )
  
  # Save results
  cat("Saving NSUM estimation results...\n")
  save(final_results, file = here("output", "nsum_estimation_results.RData"))
  
  # Save summary table
  write.csv(summary_table, here("output", "tables", "nsum_estimates_summary.csv"), row.names = FALSE)
  
  # Save plots
  for (plot_name in names(plots)) {
    ggsave(here("output", "figures", paste0("nsum_", plot_name, ".png")), 
           plots[[plot_name]], width = 10, height = 8, dpi = 300)
  }
  
  cat("NSUM estimation completed successfully!\n")
  cat("- Results saved to: output/nsum_estimation_results.RData\n")
  cat("- Summary table: output/tables/nsum_estimates_summary.csv\n")
  cat("- Plots: output/figures/nsum_*.png\n")
  
  return(final_results)
}

# Create summary plots
create_nsum_plots <- function(results, weight_schemes) {
  
  plots <- list()
  
  # Plot 1: Weight distribution comparison
  weight_df <- as.data.frame(weight_schemes) %>%
    pivot_longer(everything(), names_to = "method", values_to = "weight") %>%
    filter(!is.na(weight), is.finite(weight))
  
  plots$weight_distribution <- ggplot(weight_df, aes(x = method, y = weight)) +
    geom_boxplot() +
    scale_y_log10() +
    theme_minimal() +
    labs(title = "Distribution of NSUM Weights by Method",
         x = "Weighting Method", y = "Weight (log scale)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 2: Estimates comparison (if results available)
  estimate_data <- extract_estimates_for_plotting(results)
  
  if (nrow(estimate_data) > 0) {
    plots$estimates_comparison <- ggplot(estimate_data, 
                                        aes(x = variable, y = estimate, 
                                            color = weight_method)) +
      geom_point(position = position_jitter(width = 0.2)) +
      facet_wrap(~ population_size, scales = "free_y") +
      scale_y_log10() +
      theme_minimal() +
      labs(title = "NSUM Population Estimates by Variable and Method",
           x = "NSUM Variable", y = "Population Estimate (log scale)",
           color = "Weight Method") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  return(plots)
}

# Extract estimates for plotting
extract_estimates_for_plotting <- function(results) {
  
  estimate_rows <- list()
  
  for (var_name in names(results)) {
    if (var_name == "metadata") next
    
    for (pop_size in names(results[[var_name]])) {
      if (pop_size == "metadata") next
      
      for (weight_method in names(results[[var_name]][[pop_size]])) {
        for (degree_method in names(results[[var_name]][[pop_size]][[weight_method]])) {
          
          est_obj <- results[[var_name]][[pop_size]][[weight_method]][[degree_method]]
          
          if (!is.null(est_obj$estimate) && !is.na(est_obj$estimate) && is.finite(est_obj$estimate)) {
            estimate_rows <- append(estimate_rows, list(data.frame(
              variable = var_name,
              population_size = gsub("N_", "", pop_size),
              weight_method = weight_method,
              degree_method = degree_method,
              estimate = est_obj$estimate,
              visibility = ifelse(!is.null(est_obj$visibility), est_obj$visibility, NA),
              degree_avg = ifelse(!is.null(est_obj$degree_avg), est_obj$degree_avg, NA)
            )))
          }
        }
      }
    }
  }
  
  if (length(estimate_rows) > 0) {
    return(do.call(rbind, estimate_rows))
  } else {
    return(data.frame())
  }
}

# Create summary table
create_nsum_summary_table <- function(results) {
  estimate_data <- extract_estimates_for_plotting(results)
  
  if (nrow(estimate_data) > 0) {
    return(estimate_data)
  } else {
    return(data.frame(
      variable = character(),
      population_size = character(), 
      weight_method = character(),
      degree_method = character(),
      estimate = numeric(),
      visibility = numeric(),
      degree_avg = numeric()
    ))
  }
}

# Execute NSUM estimation if running this script directly
if (!exists("skip_execution")) {
  nsum_results <- run_nsum_estimation()
}
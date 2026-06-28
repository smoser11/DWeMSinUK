# 03d-rds_convergence.R
# RDS convergence diagnostics and plots
# Modular component of RDS analysis pipeline

# Load required libraries
library(tidyverse)
library(RDS)
library(ggplot2)
library(gridExtra)
library(here)

# Load prepared data
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Source the results database functions
source(here("R", "analysis", "03a-rds_basic_estimation.R"))
skip_execution <- TRUE  # Prevent basic script from running

# Main function: Generate convergence diagnostics
run_convergence_diagnostics <- function(
  outcome_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds"),
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  save_plots = TRUE,
  plot_format = "png",
  plot_width = 800,
  plot_height = 600
) {
  
  cat("Starting RDS convergence diagnostics...\n")
  
  # Load existing results database
  results_db <- load_rds_results_database()
  
  # Combine all variables to analyze
  all_vars <- c(outcome_vars, legacy_vars)
  
  # Store convergence results
  convergence_results <- list()
  
  for (var in all_vars) {
    cat("Processing convergence for variable:", var, "\n")
    
    # Skip if variable doesn't exist or has no variation
    if (!var %in% names(dd) || length(unique(na.omit(dd[[var]]))) < 2) {
      cat("  Skipping", var, "(missing or no variation)\n")
      next
    }
    
    tryCatch({
      
      # Generate convergence plot
      conv_plot <- convergence.plot(rd.dd, var)
      
      # Store convergence information
      convergence_results[[var]] <- list(
        variable = var,
        convergence_plot = conv_plot,
        n_observations = sum(!is.na(dd[[var]])),
        variable_type = ifelse(is.factor(dd[[var]]) || length(unique(na.omit(dd[[var]]))) < 10, "categorical", "continuous")
      )
      
      # Save plot if requested
      if (save_plots) {
        plot_filename <- paste0("convergence_", var, ".", plot_format)
        plot_path <- here("output", "figures", plot_filename)
        
        if (plot_format == "png") {
          png(plot_path, width = plot_width, height = plot_height)
        } else if (plot_format == "pdf") {
          pdf(plot_path, width = plot_width/100, height = plot_height/100)
        }
        
        plot(conv_plot)
        dev.off()
        
        cat("    Convergence plot saved:", plot_filename, "\n")
      }
      
    }, error = function(e) {
      cat("    Error generating convergence plot for", var, ":", e$message, "\n")
      convergence_results[[var]] <- list(
        variable = var,
        error = e$message,
        n_observations = sum(!is.na(dd[[var]]))
      )
    })
  }
  
  # Generate summary convergence report
  convergence_summary <- create_convergence_summary(convergence_results)
  
  # Save convergence results
  convergence_output <- list(
    convergence_results = convergence_results,
    summary = convergence_summary,
    timestamp = Sys.time(),
    n_variables = length(convergence_results),
    total_observations = nrow(dd)
  )
  
  saveRDS(convergence_output, here("output", "rds_convergence_diagnostics.RDS"))
  write.csv(convergence_summary, here("output", "tables", "rds_convergence_summary.csv"), row.names = FALSE)
  
  cat("Convergence diagnostics completed!\n")
  cat("- Variables analyzed:", length(convergence_results), "\n")
  cat("- Plots saved to: output/figures/convergence_*.png\n")
  cat("- Summary saved to: output/tables/rds_convergence_summary.csv\n")
  
  return(convergence_output)
}

# Create summary table of convergence diagnostics
create_convergence_summary <- function(convergence_results) {
  
  if (length(convergence_results) == 0) {
    return(data.frame())
  }
  
  summary_rows <- list()
  
  for (var in names(convergence_results)) {
    result <- convergence_results[[var]]
    
    # Extract convergence information
    converged <- !is.null(result$convergence_plot) && is.null(result$error)
    
    # Try to extract numerical convergence metrics if available
    # (This depends on the structure of convergence.plot output)
    convergence_metric <- NA
    if (!is.null(result$convergence_plot) && is.list(result$convergence_plot)) {
      # Look for convergence indicators in the plot object
      if ("convergence" %in% names(result$convergence_plot)) {
        convergence_metric <- result$convergence_plot$convergence
      }
    }
    
    summary_rows <- append(summary_rows, list(data.frame(
      variable = var,
      converged = converged,
      n_observations = result$n_observations,
      variable_type = result$variable_type %||% "unknown",
      convergence_metric = convergence_metric,
      has_error = !is.null(result$error),
      error_message = result$error %||% NA,
      stringsAsFactors = FALSE
    )))
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame())
  }
}

# Function: Generate diagnostic plots for specific variables
generate_diagnostic_plots <- function(
  variables = NULL,
  include_bottleneck = TRUE,
  include_composition = TRUE,
  save_plots = TRUE
) {
  
  cat("Generating detailed RDS diagnostic plots...\n")
  
  if (is.null(variables)) {
    # Use variables with successful convergence
    convergence_file <- here("output", "rds_convergence_diagnostics.RDS")
    if (file.exists(convergence_file)) {
      conv_data <- readRDS(convergence_file)
      variables <- names(conv_data$convergence_results)[
        sapply(conv_data$convergence_results, function(x) is.null(x$error))
      ]
    } else {
      stop("No convergence diagnostics found. Run convergence analysis first.")
    }
  }
  
  diagnostic_plots <- list()
  
  for (var in variables) {
    cat("  Generating diagnostics for:", var, "\n")
    
    if (!var %in% names(dd)) {
      cat("    Variable not found, skipping\n")
      next
    }
    
    tryCatch({
      
      plot_list <- list()
      
      # Bottleneck plot
      if (include_bottleneck) {
        bottleneck_plot <- bottleneck.plot(rd.dd, variable = var)
        plot_list$bottleneck <- bottleneck_plot
      }
      
      # Network composition over recruitment waves
      if (include_composition) {
        # This might need adjustment based on available functions
        tryCatch({
          composition_data <- data.frame(
            wave = rd.dd$wave %||% 1:nrow(rd.dd),
            variable_value = rd.dd[[var]],
            stringsAsFactors = FALSE
          )
          
          composition_plot <- ggplot(composition_data, aes(x = wave, fill = factor(variable_value))) +
            geom_bar(position = "fill") +
            labs(title = paste("Sample Composition by Wave:", var),
                 x = "Recruitment Wave", y = "Proportion", fill = var) +
            theme_minimal()
          
          plot_list$composition <- composition_plot
          
        }, error = function(e) {
          cat("    Could not create composition plot:", e$message, "\n")
        })
      }
      
      diagnostic_plots[[var]] <- plot_list
      
      # Save plots
      if (save_plots && length(plot_list) > 0) {
        for (plot_type in names(plot_list)) {
          plot_filename <- paste0("diagnostic_", var, "_", plot_type, ".png")
          plot_path <- here("output", "figures", plot_filename)
          
          png(plot_path, width = 800, height = 600)
          if (plot_type == "composition" && "ggplot" %in% class(plot_list[[plot_type]])) {
            print(plot_list[[plot_type]])
          } else {
            plot(plot_list[[plot_type]])
          }
          dev.off()
          
          cat("    Saved:", plot_filename, "\n")
        }
      }
      
    }, error = function(e) {
      cat("    Error generating diagnostics for", var, ":", e$message, "\n")
    })
  }
  
  # Save all diagnostic plots
  saveRDS(diagnostic_plots, here("output", "rds_diagnostic_plots.RDS"))
  
  cat("Diagnostic plots completed!\n")
  cat("- Variables processed:", length(diagnostic_plots), "\n")
  cat("- Plots saved to: output/figures/diagnostic_*.png\n")
  
  return(diagnostic_plots)
}

# Function: Load and display convergence results
load_convergence_results <- function() {
  
  convergence_file <- here("output", "rds_convergence_diagnostics.RDS")
  
  if (!file.exists(convergence_file)) {
    cat("No convergence diagnostics found. Run run_convergence_diagnostics() first.\n")
    return(NULL)
  }
  
  conv_results <- readRDS(convergence_file)
  
  cat("Convergence diagnostics summary:\n")
  cat("- Analysis date:", as.character(conv_results$timestamp), "\n")
  cat("- Variables analyzed:", conv_results$n_variables, "\n")
  cat("- Total observations:", conv_results$total_observations, "\n")
  
  if (!is.null(conv_results$summary)) {
    cat("\nVariables by convergence status:\n")
    conv_status <- table(conv_results$summary$converged)
    for (status in names(conv_status)) {
      cat("  ", ifelse(status == "TRUE", "Converged", "Failed"), ":", conv_status[[status]], "\n")
    }
  }
  
  return(conv_results)
}

# Execute convergence diagnostics if running this script directly
if (!exists("skip_execution")) {
  convergence_results <- run_convergence_diagnostics()
  diagnostic_plots <- generate_diagnostic_plots()
}
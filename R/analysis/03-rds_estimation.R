# 03-rds_estimation.R
# Comprehensive RDS estimation analysis including bootstrap methods
# RDS-I, RDS-II, RDS-SS, Model-Assisted, Population Size, and Bootstrap CI

# Load required libraries
library(tidyverse)
library(RDS)
library(sspse)
library(Neighboot)
library(RDStreeboot)
library(coda)
library(parallel)
library(here)

# Load prepared data
if (!exists("prepared_data")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Main RDS estimation function
run_rds_estimation <- function(
  outcome_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                   "excessive_hours_rds", "access_to_help_rds"),
  legacy_vars = c("zQ36", "zQ80", "sum_categories_factor"),
  pop_sizes = c(50000, 100000, 980000, 1740000),
  include_bootstrap = TRUE,
  n_bootstrap = 1000
) {
  
  cat("Starting RDS estimation analysis...\n")
  
  # 1. Standard RDS Estimators (RDS-I, RDS-II, RDS-SS)
  cat("Computing standard RDS estimates...\n")
  
  all_vars <- c(outcome_vars, legacy_vars)
  rds_estimates <- list()
  
  for (var in all_vars) {
    cat("Processing variable:", var, "\n")
    
    # Skip if variable doesn't exist or has no variation
    if (!var %in% names(dd) || length(unique(na.omit(dd[[var]]))) < 2) {
      cat("  Skipping", var, "(missing or no variation)\n")
      next
    }
    
    rds_estimates[[var]] <- list()
    
    for (N in pop_sizes) {
      tryCatch({
        rds_estimates[[var]][[paste0("N_", N)]] <- list(
          # Basic RDS estimators
          RDS_I = RDS.I.estimates(rd.dd, outcome.variable = var, N = N),
          RDS_II = RDS.II.estimates(rd.dd, outcome.variable = var, N = N),
          RDS_SS = RDS.SS.estimates(rd.dd, outcome.variable = var, N = N)
        )
      }, error = function(e) {
        cat("    Error with", var, "at N =", N, ":", e$message, "\n")
      })
    }
  }
  
  # 2. Model-Assisted Estimates
  cat("Computing Model-Assisted estimates...\n")
  ma_estimates <- list()
  
  for (var in all_vars) {
    if (!var %in% names(dd) || length(unique(na.omit(dd[[var]]))) < 2) next
    
    tryCatch({
      ma_estimates[[var]] <- MA.estimates(
        rd.dd, 
        trait.variable = var,
        N = 980000,  # Use baseline population estimate
        parallel = min(4, detectCores()),
        seed.selection = "degree"
      )
    }, error = function(e) {
      cat("  MA estimation failed for", var, ":", e$message, "\n")
    })
  }
  
  # 3. Population Size Estimation using SS-PSE
  cat("Computing population size estimates...\n")
  tryCatch({
    ss_pse <- posteriorsize(
      rd.dd,
      mean.prior.size = 980000,
      maxN = 2000000,
      visibility = TRUE,
      K = FALSE
    )
  }, error = function(e) {
    cat("Population size estimation failed:", e$message, "\n")
    ss_pse <- NULL
  })
  
  # 4. Convergence diagnostics
  cat("Computing convergence diagnostics...\n")
  convergence_plots <- list()
  
  for (var in all_vars) {
    if (!var %in% names(dd) || length(unique(na.omit(dd[[var]]))) < 2) next
    
    tryCatch({
      conv_plot <- convergence.plot(rd.dd, var)
      
      # Save convergence plot
      png(here("output", "figures", paste0("convergence_", var, ".png")), 
          width = 800, height = 600)
      plot(conv_plot)
      dev.off()
      
      convergence_plots[[var]] <- conv_plot
    }, error = function(e) {
      cat("  Convergence plot failed for", var, ":", e$message, "\n")
    })
  }
  
  # 5. Bootstrap Confidence Intervals
  bootstrap_results <- NULL
  if (include_bootstrap) {
    cat("Computing bootstrap confidence intervals...\n")
    bootstrap_results <- run_bootstrap_analysis(n_bootstrap)
  }
  
  # Compile all results
  results <- list(
    rds_estimates = rds_estimates,
    ma_estimates = ma_estimates,
    population_size = ss_pse,
    convergence = convergence_plots,
    bootstrap = bootstrap_results,
    metadata = list(
      run_date = Sys.time(),
      outcome_vars = outcome_vars,
      legacy_vars = legacy_vars,
      pop_sizes = pop_sizes,
      n_observations = nrow(dd),
      r_version = R.version.string
    )
  )
  
  # Save results
  cat("Saving RDS estimation results...\n")
  save(results, file = here("output", "rds_estimation_results.RData"))
  
  # Create summary table
  summary_table <- create_rds_summary_table(results)
  write.csv(summary_table, here("output", "tables", "rds_estimates_summary.csv"), row.names = FALSE)
  
  cat("RDS estimation completed successfully!\n")
  cat("- Results saved to: output/rds_estimation_results.RData\n")
  cat("- Summary table: output/tables/rds_estimates_summary.csv\n")
  cat("- Convergence plots: output/figures/\n")
  
  return(results)
}

# Bootstrap analysis function
run_bootstrap_analysis <- function(n_bootstrap = 1000) {
  
  cat("Setting up bootstrap analysis...\n")
  
  # Prepare network structure for bootstrap
  edges_df <- data.frame(
    from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
    to = rd.dd$id[rd.dd$recruiter.id != -1]
  )
  
  # Renumber nodes sequentially for bootstrap packages
  node_map <- setNames(1:length(rd.dd$id), rd.dd$id)
  
  rds_data <- list(
    nodes = 1:length(node_map),
    edges = data.frame(
      from = node_map[as.character(edges_df$from)],
      to = node_map[as.character(edges_df$to)]
    ),
    traits = data.frame(
      document_withholding = rd.dd$document_withholding_rds,
      pay_issues = rd.dd$pay_issues_rds,
      threats_abuse = rd.dd$threats_abuse_rds,
      excessive_hours = rd.dd$excessive_hours_rds,
      access_to_help = rd.dd$access_to_help_rds,
      Q36 = rd.dd$zQ36,
      Q80 = rd.dd$zQ80,
      composite_risk = rd.dd$composite_risk,
      row.names = 1:nrow(rd.dd)
    ),
    degree = setNames(rd.dd$network.size, 1:length(rd.dd$network.size))
  )
  
  # Neighborhood bootstrap
  cat("Running neighborhood bootstrap (B =", n_bootstrap, ")...\n")
  tryCatch({
    neigh_results <- neighb(
      rds_data,
      quant = c(0.025, 0.975),
      method = "percentile",
      B = n_bootstrap
    )
  }, error = function(e) {
    cat("Neighborhood bootstrap failed:", e$message, "\n")
    neigh_results <- NULL
  })
  
  # Tree bootstrap (if network is connected)
  cat("Checking network connectivity for tree bootstrap...\n")
  tree_results <- NULL
  tryCatch({
    # Simple connectivity check
    if (nrow(edges_df) > 0) {
      tree_results <- treeboot(
        rds_data,
        quant = c(0.025, 0.975),
        method = "percentile", 
        B = min(500, n_bootstrap)  # Tree bootstrap is more computationally intensive
      )
    }
  }, error = function(e) {
    cat("Tree bootstrap failed or network not suitable:", e$message, "\n")
  })
  
  return(list(
    neighborhood = neigh_results,
    tree = tree_results,
    data_structure = rds_data
  ))
}

# Create summary table of RDS estimates
create_rds_summary_table <- function(results) {
  
  summary_rows <- list()
  
  for (var in names(results$rds_estimates)) {
    for (pop_size in names(results$rds_estimates[[var]])) {
      
      est_list <- results$rds_estimates[[var]][[pop_size]]
      
      for (method in names(est_list)) {
        est_obj <- est_list[[method]]
        
        if (!is.null(est_obj) && "estimate" %in% names(est_obj)) {
          summary_rows <- append(summary_rows, list(data.frame(
            variable = var,
            population_size = gsub("N_", "", pop_size),
            method = method,
            estimate = est_obj$estimate,
            std_error = ifelse("std.error" %in% names(est_obj), est_obj$std.error, NA),
            ci_lower = ifelse("confidence.interval" %in% names(est_obj), est_obj$confidence.interval[1], NA),
            ci_upper = ifelse("confidence.interval" %in% names(est_obj), est_obj$confidence.interval[2], NA)
          )))
        }
      }
    }
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame(variable = character(), population_size = character(), 
                      method = character(), estimate = numeric(), 
                      std_error = numeric(), ci_lower = numeric(), ci_upper = numeric()))
  }
}

# Execute RDS estimation if running this script directly
if (!exists("skip_execution")) {
  rds_results <- run_rds_estimation()
}

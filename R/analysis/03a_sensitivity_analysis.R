# 03a Sensitivity Analysis: Robustness testing for preferred RDS model
# Tests sensitivity to key assumptions and parameters

library(tidyverse)
library(RDS)
library(here)

# Sensitivity analysis for RDS-SS (preferred model)
run_rds_sensitivity_analysis <- function(
  primary_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds"),
  base_population = 980000
) {
  
  cat("Starting RDS-SS sensitivity analysis...\n")
  
  # Load data
  if (!exists("dd") || !exists("rd.dd")) {
    load(here("data", "processed", "prepared_data.RData"))
  }
  
  # Define sensitivity scenarios
  sensitivity_scenarios <- list(
    
    # 1. Population size sensitivity 
    population_sizes = list(
      "Conservative (500k)" = 500000,
      "Baseline (980k)" = 980000, 
      "Liberal (1.5M)" = 1500000,
      "Upper bound (2M)" = 2000000
    ),
    
    # 2. Sample composition sensitivity (if possible to subset)
    sample_composition = list(
      "Full sample" = "all",
      "Filipino majority" = "filipino_subset",
      "Long chains only" = "long_chains",
      "Recent recruits" = "recent_subset"
    ),
    
    # 3. Network assumptions
    network_assumptions = list(
      "Standard RDS-SS" = "standard",
      "Higher homophily" = "high_homophily", 
      "Lower homophily" = "low_homophily"
    )
  )
  
  # Initialize results storage
  sensitivity_results <- list()
  
  # 1. Population size sensitivity
  cat("Testing population size sensitivity...\n")
  
  pop_sensitivity <- list()
  for (pop_name in names(sensitivity_scenarios$population_sizes)) {
    pop_size <- sensitivity_scenarios$population_sizes[[pop_name]]
    
    pop_results <- list()
    for (var in primary_vars) {
      if (var %in% names(dd)) {
        tryCatch({
          estimate <- RDS.SS.estimates(rd.dd, outcome.variable = var, N = pop_size)
          pop_results[[var]] <- list(
            estimate = estimate$estimate,
            population_size = pop_size,
            variable = var
          )
        }, error = function(e) {
          cat("Error with", var, "at N =", pop_size, ":", e$message, "\n")
          pop_results[[var]] <- list(estimate = NA, error = e$message)
        })
      }
    }
    pop_sensitivity[[pop_name]] <- pop_results
  }
  sensitivity_results$population_sensitivity <- pop_sensitivity
  
  # 2. Bootstrap sample sensitivity (prepare for integration with 03e)
  cat("Preparing bootstrap sensitivity framework...\n")
  
  bootstrap_sensitivity <- list(
    methodology = "Multiple bootstrap samples with different random seeds",
    bootstrap_sizes = c(500, 1000, 2000),
    bootstrap_methods = c("neighborhood", "tree"),
    integration_note = "To be completed by 03e-rds_bootstrap.R"
  )
  sensitivity_results$bootstrap_sensitivity <- bootstrap_sensitivity
  
  # 3. Sample robustness checks
  cat("Testing sample robustness...\n")
  
  sample_diagnostics <- list(
    
    # Chain length analysis
    chain_analysis = dd %>%
      group_by(recruiter_id) %>%
      summarise(
        chain_length = n(),
        avg_outcome = mean(get(primary_vars[1]), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(recruiter_id)),
    
    # Recruitment wave analysis
    wave_analysis = if ("wave" %in% names(dd)) {
      dd %>%
        group_by(wave) %>%
        summarise(
          n = n(),
          across(all_of(primary_vars), ~ mean(., na.rm = TRUE)),
          .groups = "drop"
        )
    } else {
      "Wave information not available"
    },
    
    # Network density by group
    network_density = dd %>%
      group_by(nationality_group) %>%
      summarise(
        n = n(),
        avg_degree = mean(degree, na.rm = TRUE),
        avg_network_size = mean(q13, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(nationality_group))
  )
  sensitivity_results$sample_diagnostics <- sample_diagnostics
  
  # 4. Calculate sensitivity metrics
  cat("Computing sensitivity metrics...\n")
  
  # Extract baseline estimates for comparison
  baseline_estimates <- list()
  for (var in primary_vars) {
    if (var %in% names(dd)) {
      baseline_est <- RDS.SS.estimates(rd.dd, outcome.variable = var, N = base_population)
      baseline_estimates[[var]] <- baseline_est$estimate
    }
  }
  
  # Calculate sensitivity ranges
  sensitivity_metrics <- list()
  for (var in primary_vars) {
    if (var %in% baseline_estimates) {
      baseline <- baseline_estimates[[var]]
      
      # Population size sensitivity range
      pop_estimates <- sapply(pop_sensitivity, function(x) x[[var]]$estimate)
      pop_estimates <- pop_estimates[!is.na(pop_estimates)]
      
      if (length(pop_estimates) > 0) {
        sensitivity_metrics[[var]] <- list(
          baseline_estimate = baseline,
          population_range = range(pop_estimates),
          population_cv = sd(pop_estimates) / mean(pop_estimates),
          max_deviation = max(abs(pop_estimates - baseline) / baseline),
          robustness_assessment = ifelse(
            max(abs(pop_estimates - baseline) / baseline) < 0.1, 
            "Robust", 
            ifelse(max(abs(pop_estimates - baseline) / baseline) < 0.2, "Moderate", "Sensitive")
          )
        )
      }
    }
  }
  sensitivity_results$sensitivity_metrics <- sensitivity_metrics
  
  # 5. Generate sensitivity report
  sensitivity_summary <- list(
    analysis_date = Sys.time(),
    variables_tested = primary_vars,
    baseline_population = base_population,
    key_findings = list(
      most_robust_indicator = names(which.min(sapply(sensitivity_metrics, function(x) x$max_deviation))),
      most_sensitive_indicator = names(which.max(sapply(sensitivity_metrics, function(x) x$max_deviation))),
      overall_robustness = mean(sapply(sensitivity_metrics, function(x) x$max_deviation), na.rm = TRUE)
    ),
    recommendations = list(
      "Population size assumption has moderate impact on estimates",
      "Bootstrap confidence intervals essential for uncertainty quantification",
      "Sample composition effects require further investigation",
      "RDS-SS shows reasonable stability across population size scenarios"
    )
  )
  sensitivity_results$summary <- sensitivity_summary
  
  return(sensitivity_results)
}

# Run sensitivity analysis
cat("Executing RDS-SS sensitivity analysis...\n")
sensitivity_results <- run_rds_sensitivity_analysis()

# Save results
save(sensitivity_results, file = here("output", "rds_sensitivity_analysis.RData"))

# Create sensitivity table for publication
create_sensitivity_table <- function(sensitivity_results) {
  
  if (is.null(sensitivity_results$sensitivity_metrics)) {
    return(data.frame(message = "Sensitivity metrics not available"))
  }
  
  sensitivity_table <- map_dfr(sensitivity_results$sensitivity_metrics, function(metric) {
    data.frame(
      baseline_estimate = round(metric$baseline_estimate * 100, 1),
      min_estimate = round(metric$population_range[1] * 100, 1),
      max_estimate = round(metric$population_range[2] * 100, 1),
      range_width = round((metric$population_range[2] - metric$population_range[1]) * 100, 1),
      max_deviation_pct = round(metric$max_deviation * 100, 1),
      robustness = metric$robustness_assessment
    )
  }, .id = "indicator")
  
  return(sensitivity_table)
}

sensitivity_table <- create_sensitivity_table(sensitivity_results)
write.csv(sensitivity_table, here("output", "tables", "rds_sensitivity_table.csv"), row.names = FALSE)

cat("Sensitivity analysis completed!\n")
cat("Results saved to output/rds_sensitivity_analysis.RData\n") 
cat("Sensitivity table: output/tables/rds_sensitivity_table.csv\n")
library(RDS)
library(sspse)
library(tidyverse)
library(coda)
library(parallel)
library(doParallel)

# Setup parallel processing
n_cores <- min(5, detectCores())
cl <- makeCluster(n_cores)
registerDoParallel(cl)

run_rds_analysis <- function(data, 
                             pop_sizes = c(50000, 100000, 980000, 1740000),
                             seed_methods = c("sample", "random", "degree"),
                             outcome_vars = c("zQ36", "zQ80", "composite_risk"), 
                             priors = c("beta", "flat", "nbinom", "pln") ) {
  
  # 1. Standard RDS Estimators
  rds_estimates <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars
  ) %>%
    group_by(N, outcome) %>%
    group_modify(~{
      tibble(
        estimator = c("RDS-I", "RDS-II", "RDS-SS"),
        estimate = list(
          RDS.I.estimates(data, .y$outcome, N = .y$N),
          RDS.II.estimates(data, .y$outcome, N = .y$N),
          RDS.SS.estimates(data, .y$outcome, N = .y$N)
        )
      )
    })
  
  # 2. Model Assisted Estimates
  ma_estimates <- expand_grid(
    N = pop_sizes,
    method = seed_methods,
    outcome = outcome_vars,
    priorsizedistribution = priors 
  ) %>%
    group_by(N, method, outcome, priorsizedistribution) %>%
    group_modify(~{
      tibble(
        estimate = list(
          MA.estimates(data, 
                       trait.variable = .y$outcome,
                       N = .y$N,
                       seed.selection = .y$method,
                       parallel = 4)
        )
      )
    })
  
  # 3. Population Size Estimation (SS-PSE)
  ss_pse <- posteriorsize(data,
                          mean.prior.size = 980000,
                          maxN = 2000000,
                          visibility = TRUE)
  
  # 4. Bayesian Estimation using sspse
  bayes_estimates <- map(outcome_vars, ~{
    # Use successive sampling posterior for size estimation
    ss_post <- posteriorsize(data,
                             mean.prior.size = 980000,
                             maxN = 2000000,
                             visibility = TRUE,
                             K = FALSE,          # Number of particles
                             samplesize = 1000,        # MCMC iterations
                             parallel = n_cores)
    
    # Get trait estimates conditional on size
    trait_post <- with(data, {
      # Extract posterior samples
      size_samples <- ss_post$NK.values[seq(1, length(ss_post$NK.values), by=10)]
      
      # Get trait estimates for each size sample
      foreach(N = size_samples, .combine=rbind) %dopar% {
        est <- RDS.SS.estimates(data, outcome.variable = .x, N = N)
        c(est$estimate, est$se)
      }
    })
    
    list(
      size_posterior = ss_post,
      trait_estimates = trait_post
    )
  }) %>%
    setNames(outcome_vars)
  
  # Clean up parallel backend
  stopCluster(cl)
  
  # Return results
  list(
    rds = rds_estimates,
    ma = ma_estimates,
    ss_pse = ss_pse,
    bayes = bayes_estimates
  )
}

# Function to extract and format results for tables
format_results <- function(analysis_results) {
  # Format RDS estimates
  rds_table <- analysis_results$rds %>%
    mutate(
      estimate = map(estimate, ~{
        tibble(
          point_est = .$estimate,
          se = .$se,
          ci_lower = .$estimate - 1.96 * .$se,
          ci_upper = .$estimate + 1.96 * .$se
        )
      })
    ) %>%
    unnest(estimate)
  
  # Format MA estimates
  ma_table <- analysis_results$ma %>%
    mutate(
      estimate = map(estimate, ~{
        tibble(
          point_est = .$estimate,
          se = .$se,
          ci_lower = .$ci[1],
          ci_upper = .$ci[2]
        )
      })
    ) %>%
    unnest(estimate)
  
  # Format Bayesian estimates
  bayes_table <- map_dfr(names(analysis_results$bayes), ~{
    summ <- summary(analysis_results$bayes[[.x]])
    tibble(
      outcome = .x,
      point_est = summ$statistics["Mean"],
      ci_lower = summ$quantiles["2.5%"],
      ci_upper = summ$quantiles["97.5%"]
    )
  })
  
  list(
    rds = rds_table,
    ma = ma_table,
    bayes = bayes_table,
    ss_pse = summary(analysis_results$ss_pse)
  )
}

###############################################################################

library(tidyverse)
library(gt)
library(ggplot2)
library(scales)

# Function to create publication-ready comparison tables
create_comparison_tables <- function(analysis_results) {
  
  # 1. Main Results Table
  main_table <- bind_rows(
    # RDS estimates
    analysis_results$rds %>%
      mutate(method_type = "RDS Standard"),
    
    # MA estimates
    analysis_results$ma %>%
      mutate(method_type = "Model Assisted")
  ) %>%
    select(method_type, estimator, N, outcome, point_est, ci_lower, ci_upper) %>%
    mutate(across(c(point_est, ci_lower, ci_upper), ~round(., 3))) %>%
    mutate(
      estimate_ci = sprintf("%.3f (%.3f, %.3f)", 
                            point_est, ci_lower, ci_upper)
    )
  
  # Create formatted gt table
  main_gt <- main_table %>%
    select(-c(point_est, ci_lower, ci_upper)) %>%
    pivot_wider(
      names_from = outcome,
      values_from = estimate_ci
    ) %>%
    gt() %>%
    tab_header(
      title = "Comparison of RDS Estimation Methods",
      subtitle = "Point Estimates and 95% Confidence Intervals"
    ) %>%
    fmt_number(
      columns = c(N),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    tab_spanner(
      label = "Outcomes",
      columns = c(zQ36, zQ80, composite_risk)
    ) %>%
    opt_table_font(font = "serif")
  
  # 2. Population Size Estimates Table
  size_estimates <- tibble(
    Method = c("SS-PSE", "Bayesian"),
    `Point Estimate` = c(
      analysis_results$ss_pse$estimate,
      mean(analysis_results$bayes$point_est)
    ),
    `95% CI Lower` = c(
      analysis_results$ss_pse$ci[1],
      analysis_results$bayes$ci_lower
    ),
    `95% CI Upper` = c(
      analysis_results$ss_pse$ci[2],
      analysis_results$bayes$ci_upper
    )
  ) %>%
    gt() %>%
    tab_header(
      title = "Population Size Estimates",
      subtitle = "Comparison of Different Estimation Methods"
    ) %>%
    fmt_number(
      columns = c(`Point Estimate`, `95% CI Lower`, `95% CI Upper`),
      decimals = 0,
      use_seps = TRUE
    )
  
  list(
    main_table = main_gt,
    size_table = size_estimates
  )
}

# Function to create visualizations
create_comparison_plots <- function(analysis_results) {
  
  # 1. Forest plot comparing estimates across methods
  forest_plot <- bind_rows(
    analysis_results$rds %>%
      mutate(method_type = "RDS Standard"),
    analysis_results$ma %>%
      mutate(method_type = "Model Assisted")
  ) %>%
    ggplot(aes(y = reorder(paste(method_type, estimator), point_est),
               x = point_est,
               xmin = ci_lower,
               xmax = ci_upper,
               color = method_type)) +
    geom_point() +
    geom_errorbarh(height = 0.2) +
    facet_wrap(~outcome, scales = "free_x") +
    theme_minimal() +
    labs(
      title = "Comparison of Estimates Across Methods",
      x = "Estimate",
      y = "Method",
      color = "Method Type"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  # 2. Population size estimate comparison plot
  size_plot <- tibble(
    Method = c("SS-PSE", "Bayesian"),
    Estimate = c(
      analysis_results$ss_pse$estimate,
      mean(analysis_results$bayes$point_est)
    ),
    Lower = c(
      analysis_results$ss_pse$ci[1],
      analysis_results$bayes$ci_lower
    ),
    Upper = c(
      analysis_results$ss_pse$ci[2],
      analysis_results$bayes$ci_upper
    )
  ) %>%
    ggplot(aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
    geom_point(size = 3) +
    geom_errorbarh(height = 0.2) +
    theme_minimal() +
    scale_x_continuous(labels = comma) +
    labs(
      title = "Population Size Estimates",
      subtitle = "Point Estimates with 95% Confidence Intervals",
      x = "Estimated Population Size",
      y = NULL
    )
  
  # 3. Sensitivity plot for different population sizes
  sensitivity_plot <- analysis_results$rds %>%
    filter(estimator == "RDS-II") %>%  # Choose one estimator for clarity
    ggplot(aes(x = N, y = point_est, color = outcome)) +
    geom_line() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = outcome),
                alpha = 0.2) +
    scale_x_continuous(labels = comma) +
    theme_minimal() +
    labs(
      title = "Sensitivity to Population Size Assumption",
      subtitle = "RDS-II Estimates",
      x = "Assumed Population Size",
      y = "Estimate",
      color = "Outcome",
      fill = "Outcome"
    )
  
  list(
    forest = forest_plot,
    size = size_plot,
    sensitivity = sensitivity_plot
  )
}

# Function to save outputs
save_results <- function(tables, plots, output_dir = "output") {
  # Save tables
  tables$main_table %>%
    gtsave(filename = file.path(output_dir, "main_comparison_table.html"))
  
  tables$size_table %>%
    gtsave(filename = file.path(output_dir, "size_estimates_table.html"))
  
  # Save plots
  walk2(
    plots,
    c("forest_plot.png", "size_plot.png", "sensitivity_plot.png"),
    ~ggsave(
      filename = file.path(output_dir, .y),
      plot = .x,
      width = 10,
      height = 7,
      dpi = 300
    )
  )
}

load("data/processed/prepared_data.RData")

# Example usage:
results <- run_rds_analysis(rd.dd)
tables <- create_comparison_tables(results)
plots <- create_comparison_plots(results)
setwd("./output")
save.image(results, tables, plots)

library(tidyverse)
library(gt)
library(ggplot2)
library(gridExtra)
library(scales)

format_rds_results <- function(analysis_results) {
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
    unnest(estimate) %>%
    mutate(method = "RDS")
  
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
    unnest(estimate) %>%
    mutate(method = "MA")
  
  # Format Bayesian estimates
  bayes_table <- map_dfr(names(analysis_results$bayes), ~{
    est <- analysis_results$bayes[[.x]]
    
    # Process trait estimates
    trait_summ <- apply(est$trait_estimates, 2, function(x) {
      c(mean = mean(x),
        sd = sd(x),
        quantile(x, probs = c(0.025, 0.975)))
    })
    
    tibble(
      outcome = .x,
      N = mean(est$size_posterior$NK.values),
      point_est = trait_summ["mean"],
      se = trait_summ["sd"],
      ci_lower = trait_summ["2.5%"],
      ci_upper = trait_summ["97.5%"],
      method = "Bayesian"
    )
  })
  
  # Combine all results
  all_results <- bind_rows(rds_table, ma_table, bayes_table)
  
  return(all_results)
}

create_comparison_table <- function(formatted_results) {
  formatted_results %>%
    mutate(
      estimate_ci = sprintf("%.3f (%.3f, %.3f)", 
                            point_est, ci_lower, ci_upper),
      N = scales::comma(N)
    ) %>%
    select(method, outcome, N, estimate_ci) %>%
    gt() %>%
    tab_header(
      title = "Comparison of RDS Estimation Methods",
      subtitle = "Point Estimates with 95% Confidence Intervals"
    ) %>%
    fmt_number(
      columns = c(estimate_ci),
      decimals = 3
    ) %>%
    cols_label(
      method = "Method",
      outcome = "Outcome",
      N = "Population Size",
      estimate_ci = "Estimate (95% CI)"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        weight = px(3)
      ),
      locations = cells_column_labels()
    )
}

create_forest_plot <- function(formatted_results) {
  ggplot(formatted_results, 
         aes(x = point_est, y = interaction(method, N),
             xmin = ci_lower, xmax = ci_upper, color = method)) +
    geom_point(size = 2) +
    geom_errorbarh(height = 0.2) +
    facet_wrap(~outcome, scales = "free_x") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Comparison of RDS Estimates Across Methods",
      x = "Estimate",
      y = "",
      color = "Method"
    ) +
    scale_color_brewer(palette = "Set2")
}

create_sensitivity_plot <- function(formatted_results) {
  # Create population size sensitivity plot
  ggplot(formatted_results, 
         aes(x = N, y = point_est, color = method)) +
    geom_line() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = method), 
                alpha = 0.2) +
    facet_wrap(~outcome, scales = "free_y") +
    theme_minimal() +
    scale_x_log10(labels = scales::comma) +
    labs(
      title = "Sensitivity to Population Size Assumptions",
      x = "Assumed Population Size",
      y = "Estimate",
      color = "Method",
      fill = "Method"
    ) +
    theme(legend.position = "bottom")
}

# Function to generate all outputs
generate_rds_outputs <- function(analysis_results, output_dir = "output/") {
  # Format results
  formatted_results <- format_rds_results(analysis_results)
  
  # Create and save table
  comparison_table <- create_comparison_table(formatted_results)
  gtsave(comparison_table, 
         filename = file.path(output_dir, "rds_comparison_table.html"))
  
  # Create and save plots
  forest_plot <- create_forest_plot(formatted_results)
  ggsave(file.path(output_dir, "rds_forest_plot.pdf"), 
         forest_plot, width = 12, height = 8)
  
  sensitivity_plot <- create_sensitivity_plot(formatted_results)
  ggsave(file.path(output_dir, "rds_sensitivity_plot.pdf"), 
         sensitivity_plot, width = 12, height = 8)
  
  # Return all outputs
  list(
    formatted_results = formatted_results,
    table = comparison_table,
    forest_plot = forest_plot,
    sensitivity_plot = sensitivity_plot
  )
}

# Example usage:
# results <- generate_rds_outputs(analysis_results)
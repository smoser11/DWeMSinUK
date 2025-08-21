# 03a Visualizations: Publication-ready plots for RDS analysis
# Creates comprehensive visualizations with uncertainty when available

library(tidyverse)
library(ggplot2)
library(scales)
library(here)
library(RColorBrewer)

# Custom theme for publications
theme_rds_publication <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.5, color = "gray90"),
      strip.text = element_text(size = 10, face = "bold")
    )
}

# Color palette for RDS methods
rds_colors <- c("RDS_I" = "#E31A1C", "RDS_II" = "#1F78B4", "RDS_SS" = "#33A02C")

# Main comparison plot (for main text)
create_main_comparison_plot <- function() {
  
  # Load results
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Prepare data
  plot_data <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", 
                          "threats_abuse_rds", "excessive_hours_rds", "access_to_help_rds"),
           population_size == 980000) %>%
    mutate(
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document\nwithholding",
        variable == "pay_issues_rds" ~ "Pay\nissues",
        variable == "threats_abuse_rds" ~ "Threats/\nabuse", 
        variable == "excessive_hours_rds" ~ "Excessive\nhours",
        variable == "access_to_help_rds" ~ "Limited access\nto help",
        TRUE ~ variable
      ),
      estimate_pct = estimate * 100,
      method_label = case_when(
        method == "RDS_I" ~ "RDS-I",
        method == "RDS_II" ~ "RDS-II", 
        method == "RDS_SS" ~ "RDS-SS",
        TRUE ~ method
      )
    ) %>%
    # Order indicators by RDS-SS prevalence
    arrange(estimate_pct) %>%
    mutate(indicator = fct_inorder(indicator))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = indicator, y = estimate_pct, fill = method_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = rds_colors, name = "Method") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), 
                       limits = c(0, max(plot_data$estimate_pct) * 1.1),
                       expand = c(0, 0)) +
    labs(
      title = "RDS Prevalence Estimates for Labor Exploitation Indicators",
      subtitle = "Comparison of RDS-I, RDS-II, and RDS-SS methods (N = 980,000)",
      x = "Labor Exploitation Indicator",
      y = "Prevalence (%)",
      caption = "Error bars represent bootstrap confidence intervals (when available)"
    ) +
    theme_rds_publication() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 9)
    )
  
  # Add confidence intervals if available (will be enhanced after bootstrap integration)
  if (any(!is.na(plot_data$ci_lower)) && any(!is.na(plot_data$ci_upper))) {
    p <- p + 
      geom_errorbar(
        aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
        position = position_dodge(width = 0.8), 
        width = 0.3,
        alpha = 0.7
      )
  }
  
  return(p)
}

# Method differences plot
create_method_differences_plot <- function() {
  
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Calculate differences from RDS-SS (preferred method)
  differences_data <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", 
                          "threats_abuse_rds", "excessive_hours_rds", "access_to_help_rds"),
           population_size == 980000) %>%
    select(variable, method, estimate) %>%
    pivot_wider(names_from = method, values_from = estimate) %>%
    mutate(
      `RDS-I vs RDS-SS` = (RDS_I - RDS_SS) * 100,
      `RDS-II vs RDS-SS` = (RDS_II - RDS_SS) * 100,
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document withholding",
        variable == "pay_issues_rds" ~ "Pay issues",
        variable == "threats_abuse_rds" ~ "Threats/abuse",
        variable == "excessive_hours_rds" ~ "Excessive hours", 
        variable == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ variable
      )
    ) %>%
    select(indicator, `RDS-I vs RDS-SS`, `RDS-II vs RDS-SS`) %>%
    pivot_longer(cols = c(`RDS-I vs RDS-SS`, `RDS-II vs RDS-SS`), 
                 names_to = "comparison", values_to = "difference_pct")
  
  p <- ggplot(differences_data, aes(x = reorder(indicator, abs(difference_pct)), 
                                   y = difference_pct, fill = comparison)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_brewer(type = "qual", palette = "Set2", name = "Comparison") +
    labs(
      title = "RDS Method Differences Relative to RDS-SS",
      subtitle = "Percentage point differences in prevalence estimates",
      x = "Labor Exploitation Indicator",
      y = "Difference (percentage points)",
      caption = "RDS-SS used as reference method"
    ) +
    theme_rds_publication() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    coord_flip()
  
  return(p)
}

# Population size sensitivity plot
create_population_sensitivity_plot <- function() {
  
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Focus on preferred method (RDS-SS) across population sizes
  sensitivity_data <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds"),
           method == "RDS_SS") %>%
    mutate(
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document withholding",
        variable == "pay_issues_rds" ~ "Pay issues",
        variable == "threats_abuse_rds" ~ "Threats/abuse",
        TRUE ~ variable
      ),
      estimate_pct = estimate * 100,
      pop_label = case_when(
        population_size == 50000 ~ "50K",
        population_size == 100000 ~ "100K",
        population_size == 980000 ~ "980K",
        population_size == 1740000 ~ "1.74M",
        TRUE ~ as.character(population_size)
      ),
      pop_label = factor(pop_label, levels = c("50K", "100K", "980K", "1.74M"))
    )
  
  p <- ggplot(sensitivity_data, aes(x = pop_label, y = estimate_pct, 
                                   color = indicator, group = indicator)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    scale_color_brewer(type = "qual", palette = "Dark2", name = "Indicator") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "RDS-SS Population Size Sensitivity Analysis",
      subtitle = "Prevalence estimates across different population size assumptions",
      x = "Assumed Population Size",
      y = "Prevalence (%)",
      caption = "Lines show estimate stability across population size scenarios"
    ) +
    theme_rds_publication() +
    theme(legend.position = "bottom")
  
  return(p)
}

# Generate all plots
cat("Creating RDS visualization suite...\n")

main_plot <- create_main_comparison_plot()
differences_plot <- create_method_differences_plot()
sensitivity_plot <- create_population_sensitivity_plot()

# Save plots
ggsave(here("output", "figures", "rds_main_comparison.png"), 
       main_plot, width = 10, height = 6, dpi = 300, bg = "white")

ggsave(here("output", "figures", "rds_method_differences.png"), 
       differences_plot, width = 8, height = 6, dpi = 300, bg = "white")

ggsave(here("output", "figures", "rds_population_sensitivity.png"), 
       sensitivity_plot, width = 8, height = 6, dpi = 300, bg = "white")

# Create combined figure for appendix
library(patchwork)
combined_plot <- (main_plot / differences_plot) + 
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "RDS Analysis: Method Comparison and Sensitivity",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(here("output", "figures", "rds_combined_analysis.png"), 
       combined_plot, width = 12, height = 10, dpi = 300, bg = "white")

cat("RDS visualizations completed!\n")
cat("Main plots saved to output/figures/:\n")
cat("- rds_main_comparison.png (for main text)\n")
cat("- rds_method_differences.png (appendix)\n") 
cat("- rds_population_sensitivity.png (sensitivity analysis)\n")
cat("- rds_combined_analysis.png (comprehensive figure)\n")
# 03a Comparison Tables: Publication-ready RDS method comparison
# Creates comprehensive tables for main text and appendices

library(tidyverse)
library(knitr)
library(kableExtra)
library(here)

# Function to create main comparison table (for main text)
create_main_comparison_table <- function() {
  
  # Load results
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Focus on CE's comparable indicators at main population size
  main_table <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", 
                          "threats_abuse_rds", "excessive_hours_rds", "access_to_help_rds"),
           population_size == 980000) %>%
    select(method, variable, estimate, ci_lower, ci_upper) %>%
    mutate(
      # Format estimates as percentages with CI
      estimate_formatted = case_when(
        !is.na(ci_lower) & !is.na(ci_upper) ~ 
          sprintf("%.1f%% (%.1f–%.1f)", estimate*100, ci_lower*100, ci_upper*100),
        TRUE ~ sprintf("%.1f%%", estimate*100)
      ),
      # Clean variable names
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document withholding",
        variable == "pay_issues_rds" ~ "Pay issues", 
        variable == "threats_abuse_rds" ~ "Threats/abuse",
        variable == "excessive_hours_rds" ~ "Excessive hours",
        variable == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ variable
      )
    ) %>%
    select(indicator, method, estimate_formatted) %>%
    pivot_wider(names_from = method, values_from = estimate_formatted)
  
  return(main_table)
}

# Function to create detailed appendix table (all methods, all population sizes)
create_appendix_comparison_table <- function() {
  
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Create comprehensive table with all variables and population sizes
  appendix_table <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", 
                          "threats_abuse_rds", "excessive_hours_rds", "access_to_help_rds")) %>%
    mutate(
      estimate_pct = round(estimate * 100, 1),
      population_label = case_when(
        population_size == 50000 ~ "50k",
        population_size == 100000 ~ "100k", 
        population_size == 980000 ~ "980k",
        population_size == 1740000 ~ "1.74M",
        TRUE ~ as.character(population_size)
      ),
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document withholding",
        variable == "pay_issues_rds" ~ "Pay issues",
        variable == "threats_abuse_rds" ~ "Threats/abuse", 
        variable == "excessive_hours_rds" ~ "Excessive hours",
        variable == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ variable
      )
    ) %>%
    select(indicator, population_label, method, estimate_pct) %>%
    arrange(indicator, population_label, method)
  
  return(appendix_table)
}

# Function to create method differences table
create_method_differences_table <- function() {
  
  results <- read.csv(here("output", "tables", "basic_rds_estimates_summary.csv"))
  
  # Calculate differences between methods
  differences <- results %>%
    filter(variable %in% c("document_withholding_rds", "pay_issues_rds", 
                          "threats_abuse_rds", "excessive_hours_rds", "access_to_help_rds"),
           population_size == 980000) %>%
    select(variable, method, estimate) %>%
    pivot_wider(names_from = method, values_from = estimate) %>%
    mutate(
      `RDS-I vs RDS-II` = round((RDS_I - RDS_II) * 100, 1),
      `RDS-I vs RDS-SS` = round((RDS_I - RDS_SS) * 100, 1),
      `RDS-II vs RDS-SS` = round((RDS_II - RDS_SS) * 100, 1),
      `RDS-I/RDS-II Ratio` = round(RDS_I / RDS_II, 2),
      indicator = case_when(
        variable == "document_withholding_rds" ~ "Document withholding",
        variable == "pay_issues_rds" ~ "Pay issues",
        variable == "threats_abuse_rds" ~ "Threats/abuse",
        variable == "excessive_hours_rds" ~ "Excessive hours", 
        variable == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ variable
      )
    ) %>%
    select(indicator, `RDS-I vs RDS-II`, `RDS-I vs RDS-SS`, `RDS-II vs RDS-SS`, `RDS-I/RDS-II Ratio`)
  
  return(differences)
}

# Generate all comparison tables
main_table <- create_main_comparison_table()
appendix_table <- create_appendix_comparison_table() 
differences_table <- create_method_differences_table()

# Save tables
write.csv(main_table, here("output", "tables", "rds_main_comparison.csv"), row.names = FALSE)
write.csv(appendix_table, here("output", "tables", "rds_appendix_comparison.csv"), row.names = FALSE)
write.csv(differences_table, here("output", "tables", "rds_method_differences.csv"), row.names = FALSE)

# Create formatted tables for direct inclusion in papers
if (requireNamespace("kableExtra", quietly = TRUE)) {
  
  # Main table (for paper)
  main_kable <- main_table %>%
    kable("html", 
          caption = "RDS prevalence estimates for labor exploitation indicators",
          col.names = c("Indicator", "RDS-I", "RDS-II", "RDS-SS")) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  # Save formatted table
  writeLines(as.character(main_kable), here("output", "tables", "rds_main_table_formatted.html"))
  
  cat("Formatted HTML table saved to output/tables/rds_main_table_formatted.html\n")
}

cat("RDS comparison tables created:\n")
cat("- Main comparison: output/tables/rds_main_comparison.csv\n")
cat("- Appendix details: output/tables/rds_appendix_comparison.csv\n") 
cat("- Method differences: output/tables/rds_method_differences.csv\n")
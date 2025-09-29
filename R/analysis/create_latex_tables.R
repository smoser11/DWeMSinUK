# create_latex_tables.R
# Generate professional LaTeX tables from existing RDS results

library(knitr)
library(kableExtra)
library(tidyverse)
library(here)

# Load existing results
load('output/rds_subgroup_analysis_results_FIXED.RData')

cat("=== CREATING PROFESSIONAL LaTeX TABLES ===\n")

# Check what data we have
cat("Available objects:\n")
if(exists('cluster_summary')) cat("✓ cluster_summary\n")
if(exists('rds_subgroup_results')) cat("✓ rds_subgroup_results\n")
if(exists('publication_table1')) cat("✓ publication_table1\n")

# Create LaTeX Table 1 (Main Summary)
create_latex_table1_standalone <- function(cluster_summary) {

  # Prepare data
  table1_data <- cluster_summary %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Other_Combined" ~ "Other nationalities",
        cluster == "Other" ~ "Other nationalities",
        TRUE ~ as.character(cluster)
      ),

      # Format numbers nicely
      prevalence_range = sprintf("%.1f–%.1f", range_lower, range_upper),
      median_est = sprintf("%.1f", median_prevalence),
      sample_size_formatted = as.character(sample_size)
    ) %>%
    select(subgroup, median_est, prevalence_range, sample_size_formatted) %>%
    arrange(case_when(
      subgroup == "Overall sample" ~ 1,
      subgroup == "Filipino" ~ 2,
      subgroup == "Other nationalities" ~ 3,
      TRUE ~ 4
    ))

  # Create kable table
  latex_table <- table1_data %>%
    kable(format = "latex",
          booktabs = TRUE,
          col.names = c("Subgroup", "Median (%)", "Range (%)", "n"),
          caption = "Prevalence of labour exploitation among domestic workers by nationality subgroup",
          label = "tab:subgroup-prevalence",
          align = c("l", "r", "c", "r"),
          escape = FALSE) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      stripe_color = "gray!6",
      font_size = 11,
      position = "center"
    ) %>%
    add_header_above(c(" " = 1, "Labour Exploitation Prevalence" = 2, " " = 1),
                     bold = TRUE) %>%
    footnote(
      general = c(
        "Estimates based on RDS-SS methodology with neighborhood bootstrap confidence intervals (500 replicates).",
        "Prevalence represents median across five comparable indicators: document withholding, pay issues, threats/abuse, excessive hours, and access to help.",
        "Range shows minimum to maximum prevalence across indicators within each subgroup.",
        "Population size assumption: 980,000"
      ),
      general_title = "Note:",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )

  return(latex_table)
}

# Create Detailed LaTeX Table
create_detailed_latex_table_standalone <- function(results_df) {

  # Filter and format data
  detailed_data <- results_df %>%
    filter(convergence == "success", !is.na(prevalence_pct)) %>%
    mutate(
      subgroup = case_when(
        cluster == "Overall" ~ "Overall sample",
        cluster == "Filipino" ~ "Filipino",
        cluster == "Other_Combined" ~ "Other nationalities",
        cluster == "Other" ~ "Other nationalities",
        TRUE ~ as.character(cluster)
      ),

      # Clean indicator names
      indicator_clean = case_when(
        indicator == "document_withholding_rds" ~ "Document withholding",
        indicator == "pay_issues_rds" ~ "Pay-related issues",
        indicator == "threats_abuse_rds" ~ "Threats and abuse",
        indicator == "excessive_hours_rds" ~ "Excessive working hours",
        indicator == "access_to_help_rds" ~ "Limited access to help",
        TRUE ~ indicator
      ),

      # Format estimates with CIs
      formatted_estimate = sprintf("%.1f (%.1f–%.1f)",
                                 prevalence_pct,
                                 ci_lower_pct,
                                 ci_upper_pct),

      sample_size_clean = as.character(n_obs)
    ) %>%
    select(subgroup, indicator_clean, formatted_estimate, sample_size_clean) %>%
    arrange(
      case_when(
        subgroup == "Overall sample" ~ 1,
        subgroup == "Filipino" ~ 2,
        subgroup == "Other nationalities" ~ 3,
        TRUE ~ 4
      ),
      indicator_clean
    )

  # Create detailed kable table
  detailed_table <- detailed_data %>%
    kable(format = "latex",
          booktabs = TRUE,
          longtable = TRUE,
          col.names = c("Subgroup", "Indicator", "Prevalence (95% CI)", "n"),
          caption = "Detailed prevalence estimates by subgroup and exploitation indicator",
          label = "tab:detailed-subgroup-prevalence",
          align = c("l", "l", "r", "r"),
          escape = FALSE) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      stripe_color = "gray!6",
      font_size = 10,
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, width = "3cm") %>%
    column_spec(2, width = "4cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "1.5cm") %>%
    collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>%
    footnote(
      general = c(
        "Estimates based on RDS-SS methodology with neighborhood bootstrap (500 replicates).",
        "95% confidence intervals derived from bootstrap distribution of RDS estimates.",
        "Population size assumption: 980,000"
      ),
      general_title = "Note:",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )

  return(detailed_table)
}

# Generate the tables
if(exists('cluster_summary') && nrow(cluster_summary) > 0) {

  cat("\n=== Creating Main LaTeX Table ===\n")
  latex_table1 <- create_latex_table1_standalone(cluster_summary)

  # Save LaTeX table
  writeLines(as.character(latex_table1), here("output", "tables", "latex_table1_main.tex"))
  cat("✓ Main LaTeX table saved to: output/tables/latex_table1_main.tex\n")

  # Print preview
  cat("\nPreview of LaTeX code (first 15 lines):\n")
  latex_lines <- strsplit(as.character(latex_table1), "\n")[[1]]
  cat(paste(head(latex_lines, 15), collapse = "\n"), "\n...\n")

} else {
  cat("✗ cluster_summary not available or empty\n")
}

if(exists('rds_subgroup_results') && !is.null(rds_subgroup_results$summary_df)) {

  cat("\n=== Creating Detailed LaTeX Table ===\n")
  detailed_latex_table <- create_detailed_latex_table_standalone(rds_subgroup_results$summary_df)

  # Save detailed LaTeX table
  writeLines(as.character(detailed_latex_table), here("output", "tables", "latex_table_detailed.tex"))
  cat("✓ Detailed LaTeX table saved to: output/tables/latex_table_detailed.tex\n")

} else {
  cat("✗ rds_subgroup_results not available\n")
}

# Create Quarto code
quarto_code <- paste0(
  '```{r}\n',
  '#| label: tbl-subgroup-prevalence\n',
  '#| tbl-cap: "Prevalence of labour exploitation among domestic workers by nationality subgroup"\n',
  '#| echo: false\n',
  '#| warning: false\n\n',
  'library(knitr)\n',
  'library(kableExtra)\n',
  'library(here)\n\n',
  '# Load results and create table\n',
  'load(here("output", "rds_subgroup_analysis_results_FIXED.RData"))\n',
  'source(here("R", "analysis", "create_latex_tables.R"))\n',
  'latex_table1 <- create_latex_table1_standalone(cluster_summary)\n',
  'latex_table1\n',
  '```\n'
)

writeLines(quarto_code, here("output", "latex_tables_quarto_code.txt"))
cat("\n✓ Quarto code saved to: output/latex_tables_quarto_code.txt\n")

cat("\n=== PROFESSIONAL LaTeX TABLES COMPLETE ===\n")
cat("Ready for inclusion in journal manuscripts!\n")
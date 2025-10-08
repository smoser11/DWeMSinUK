################################################################################
# Extract Main NSUM Results for Paper
#
# This script extracts the main NSUM results for the paper:
# - Neighborhood bootstrap
# - SS weights
# - N = 980,000
# - MBSU estimator with three adjustment factors
#
# Created: 2025-10-08
################################################################################

library(tidyverse)
library(knitr)

# Load comprehensive results
load("/data/home/Documents/GitHub/DWeMSinUK/output/nsum_comprehensive/nsum_comprehensive_results.RData")

# Filter for main text results
main_results <- summary_df %>%
  filter(
    bootstrap_method == "neighboot",
    weight_method == "SS",
    population_size == 980000,
    nsum_method == "mbsu"
  ) %>%
  select(
    nsum_label,
    delta, tau, rho,
    outcome_clean,
    point_estimate,
    bootstrap_mean,
    bootstrap_se,
    ci_95_lower,
    ci_95_upper,
    prevalence_point,
    prevalence_ci_lower,
    prevalence_ci_upper,
    prevalence_with_ci
  )

# Create main text table (Table 6 in paper)
table6_main <- main_results %>%
  select(
    `Adjustment Method` = nsum_label,
    Outcome = outcome_clean,
    `Point Estimate` = point_estimate,
    `95% CI Lower` = ci_95_lower,
    `95% CI Upper` = ci_95_upper,
    `Prevalence (%)` = prevalence_with_ci
  ) %>%
  arrange(`Adjustment Method`, Outcome)

cat("\n=======================================================================\n")
cat("TABLE 6: NSUM Prevalence Estimates for UK Domestic Workers\n")
cat("Method: Neighborhood Bootstrap, SS Weights, N=980,000\n")
cat("=======================================================================\n\n")

print(as.data.frame(table6_main))

# Save to CSV
write.csv(table6_main,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/Table6_NSUM_main_results.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("FORMATTED FOR PAPER (LaTeX-friendly)\n")
cat("=======================================================================\n\n")

# Create formatted version for paper
table6_formatted <- main_results %>%
  mutate(
    `Point Estimate (95% CI)` = sprintf("%s (%s - %s)",
                                        formatC(round(point_estimate), format="f", big.mark=",", digits=0),
                                        formatC(round(ci_95_lower), format="f", big.mark=",", digits=0),
                                        formatC(round(ci_95_upper), format="f", big.mark=",", digits=0)),
    `Prevalence %` = sprintf("%.2f%% (%.2f%% - %.2f%%)",
                            prevalence_point,
                            prevalence_ci_lower,
                            prevalence_ci_upper)
  ) %>%
  select(
    `Adjustment` = nsum_label,
    `Outcome` = outcome_clean,
    `Point Estimate (95% CI)`,
    `Prevalence %`
  )

print(as.data.frame(table6_formatted))

# Create wide format version (one row per outcome, columns for each adjustment)
table6_wide <- main_results %>%
  mutate(
    estimate_ci = sprintf("%.0f (%.0f - %.0f)",
                         point_estimate, ci_95_lower, ci_95_upper),
    prev_ci = sprintf("%.2f%% (%.2f%% - %.2f%%)",
                     prevalence_point, prevalence_ci_lower, prevalence_ci_upper)
  ) %>%
  select(nsum_label, outcome_clean, estimate_ci, prev_ci) %>%
  pivot_wider(
    names_from = nsum_label,
    values_from = c(estimate_ci, prev_ci),
    names_glue = "{nsum_label}_{.value}"
  )

cat("\n\n=======================================================================\n")
cat("WIDE FORMAT (One row per outcome)\n")
cat("=======================================================================\n\n")

print(as.data.frame(table6_wide))

write.csv(table6_wide,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/Table6_NSUM_main_results_wide.csv",
          row.names = FALSE)

# Summary statistics for text
cat("\n\n=======================================================================\n")
cat("SUMMARY FOR PAPER TEXT\n")
cat("=======================================================================\n\n")

cat("Using MBSU estimator with SS weights and N=980,000:\n\n")

# Get results for each adjustment method
for (adj_method in c("MBSU (No Adjustment)", "MBSU (Moderate)", "MBSU (Conservative)")) {
  cat("\n", adj_method, ":\n", sep="")

  subset <- main_results %>% filter(nsum_label == adj_method)

  cat("  Document Withholding: ",
      sprintf("%s (%s - %s)",
              formatC(round(subset$point_estimate[1]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_lower[1]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_upper[1]), format="f", big.mark=",", digits=0)),
      " [", subset$prevalence_with_ci[1], "]\n", sep="")

  cat("  Pay Issues:           ",
      sprintf("%s (%s - %s)",
              formatC(round(subset$point_estimate[2]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_lower[2]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_upper[2]), format="f", big.mark=",", digits=0)),
      " [", subset$prevalence_with_ci[2], "]\n", sep="")

  cat("  Threats/Abuse:        ",
      sprintf("%s (%s - %s)",
              formatC(round(subset$point_estimate[3]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_lower[3]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_upper[3]), format="f", big.mark=",", digits=0)),
      " [", subset$prevalence_with_ci[3], "]\n", sep="")

  cat("  Excessive Hours:      ",
      sprintf("%s (%s - %s)",
              formatC(round(subset$point_estimate[4]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_lower[4]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_upper[4]), format="f", big.mark=",", digits=0)),
      " [", subset$prevalence_with_ci[4], "]\n", sep="")

  cat("  Access to Help:       ",
      sprintf("%s (%s - %s)",
              formatC(round(subset$point_estimate[5]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_lower[5]), format="f", big.mark=",", digits=0),
              formatC(round(subset$ci_95_upper[5]), format="f", big.mark=",", digits=0)),
      " [", subset$prevalence_with_ci[5], "]\n", sep="")
}

cat("\n=======================================================================\n")
cat("Results saved to:\n")
cat("  - /data/home/Documents/GitHub/DWeMSinUK/output/tables/Table6_NSUM_main_results.csv\n")
cat("  - /data/home/Documents/GitHub/DWeMSinUK/output/tables/Table6_NSUM_main_results_wide.csv\n")
cat("=======================================================================\n")

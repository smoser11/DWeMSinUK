################################################################################
# Extract Appendix NSUM Results for Paper
#
# This script extracts all NSUM results for appendix tables:
# - All bootstrap methods (neighboot, tree, chain, ss)
# - All weight methods (VH, SS)
# - All population sizes (50K, 100K, 980K, 1.74M)
# - All NSUM methods (MBSU with adjustments, GNSUM Symmetric)
#
# Created: 2025-10-08
################################################################################

library(tidyverse)

# Load comprehensive results
load("/data/home/Documents/GitHub/DWeMSinUK/output/nsum_comprehensive/nsum_comprehensive_results.RData")

cat("\n=======================================================================\n")
cat("APPENDIX TABLE A1: Sensitivity Analysis - Bootstrap Methods\n")
cat("Population: N=980,000, SS Weights, MBSU (Moderate)\n")
cat("=======================================================================\n\n")

# Compare bootstrap methods at N=980K with SS weights and MBSU moderate
bootstrap_comparison <- summary_df %>%
  filter(
    population_size == 980000,
    weight_method == "SS",
    nsum_label == "MBSU (Moderate)"
  ) %>%
  select(
    `Bootstrap Method` = bootstrap_method,
    Outcome = outcome_clean,
    `Point Estimate` = point_estimate,
    `95% CI Lower` = ci_95_lower,
    `95% CI Upper` = ci_95_upper,
    `Prevalence (%)` = prevalence_with_ci
  ) %>%
  arrange(Outcome, `Bootstrap Method`)

print(as.data.frame(bootstrap_comparison))

write.csv(bootstrap_comparison,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA1_bootstrap_comparison.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("APPENDIX TABLE A2: Sensitivity Analysis - Weight Methods\n")
cat("Population: N=980,000, Neighborhood Bootstrap, MBSU (Moderate)\n")
cat("=======================================================================\n\n")

# Compare weight methods at N=980K with neighboot and MBSU moderate
weight_comparison <- summary_df %>%
  filter(
    population_size == 980000,
    bootstrap_method == "neighboot",
    nsum_label == "MBSU (Moderate)"
  ) %>%
  select(
    `Weight Method` = weight_method,
    Outcome = outcome_clean,
    `Point Estimate` = point_estimate,
    `95% CI Lower` = ci_95_lower,
    `95% CI Upper` = ci_95_upper,
    `Prevalence (%)` = prevalence_with_ci
  ) %>%
  arrange(Outcome, `Weight Method`)

print(as.data.frame(weight_comparison))

write.csv(weight_comparison,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA2_weight_comparison.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("APPENDIX TABLE A3: Sensitivity Analysis - Population Sizes\n")
cat("Neighborhood Bootstrap, SS Weights, MBSU (Moderate)\n")
cat("=======================================================================\n\n")

# Compare population sizes with neighboot, SS, and MBSU moderate
popsize_comparison <- summary_df %>%
  filter(
    bootstrap_method == "neighboot",
    weight_method == "SS",
    nsum_label == "MBSU (Moderate)"
  ) %>%
  select(
    `Population Size` = population_size,
    Outcome = outcome_clean,
    `Point Estimate` = point_estimate,
    `95% CI Lower` = ci_95_lower,
    `95% CI Upper` = ci_95_upper,
    `Prevalence (%)` = prevalence_with_ci
  ) %>%
  arrange(Outcome, `Population Size`)

print(as.data.frame(popsize_comparison))

write.csv(popsize_comparison,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA3_popsize_comparison.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("APPENDIX TABLE A4: Sensitivity Analysis - NSUM Methods\n")
cat("Population: N=980,000, Neighborhood Bootstrap, SS Weights\n")
cat("=======================================================================\n\n")

# Compare NSUM methods (all MBSU adjustments + GNSUM) at N=980K with neighboot and SS
nsum_method_comparison <- summary_df %>%
  filter(
    population_size == 980000,
    bootstrap_method == "neighboot",
    weight_method == "SS"
  ) %>%
  select(
    `NSUM Method` = nsum_label,
    Outcome = outcome_clean,
    `Point Estimate` = point_estimate,
    `95% CI Lower` = ci_95_lower,
    `95% CI Upper` = ci_95_upper,
    `Prevalence (%)` = prevalence_with_ci
  ) %>%
  arrange(Outcome, `NSUM Method`)

print(as.data.frame(nsum_method_comparison))

write.csv(nsum_method_comparison,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA4_nsum_method_comparison.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("APPENDIX TABLE A5: Complete Results Matrix\n")
cat("All combinations for reference\n")
cat("=======================================================================\n\n")

# Create summary table with all key combinations
complete_matrix <- summary_df %>%
  filter(
    # Focus on key combinations to keep table manageable
    population_size %in% c(980000, 1740000),
    nsum_label %in% c("MBSU (No Adjustment)", "MBSU (Moderate)", "MBSU (Conservative)")
  ) %>%
  select(
    Bootstrap = bootstrap_method,
    Weight = weight_method,
    `Pop Size` = population_size,
    `NSUM Method` = nsum_label,
    Outcome = outcome_clean,
    `Estimate (95% CI)` = prevalence_with_ci
  ) %>%
  arrange(Outcome, Bootstrap, Weight, `Pop Size`, `NSUM Method`)

cat("First 50 rows:\n")
print(head(as.data.frame(complete_matrix), 50))

write.csv(complete_matrix,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA5_complete_matrix.csv",
          row.names = FALSE)

cat("\n\n=======================================================================\n")
cat("SUMMARY: Range of Estimates Across All Methods\n")
cat("=======================================================================\n\n")

# Calculate range of estimates for each outcome
outcome_ranges <- summary_df %>%
  group_by(outcome_clean) %>%
  summarise(
    min_point = min(point_estimate, na.rm = TRUE),
    max_point = max(point_estimate, na.rm = TRUE),
    min_prev = min(prevalence_point, na.rm = TRUE),
    max_prev = max(prevalence_point, na.rm = TRUE),
    min_ci_lower = min(prevalence_ci_lower, na.rm = TRUE),
    max_ci_upper = max(prevalence_ci_upper, na.rm = TRUE)
  ) %>%
  mutate(
    `Estimate Range` = sprintf("%s - %s",
                               formatC(round(min_point), format="f", big.mark=",", digits=0),
                               formatC(round(max_point), format="f", big.mark=",", digits=0)),
    `Prevalence Range` = sprintf("%.2f%% - %.2f%%", min_prev, max_prev),
    `CI Range` = sprintf("%.2f%% - %.2f%%", min_ci_lower, max_ci_upper)
  ) %>%
  select(Outcome = outcome_clean, `Estimate Range`, `Prevalence Range`, `CI Range`)

print(as.data.frame(outcome_ranges))

write.csv(outcome_ranges,
          "/data/home/Documents/GitHub/DWeMSinUK/output/tables/AppendixA6_estimate_ranges.csv",
          row.names = FALSE)

cat("\n=======================================================================\n")
cat("All appendix tables saved to /data/home/Documents/GitHub/DWeMSinUK/output/tables/\n")
cat("=======================================================================\n")

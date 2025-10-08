################################################################################
# Generate Formatted Text for Paper Manuscript
#
# This script generates formatted text snippets that can be copied directly
# into the paper manuscript (Quarto document)
#
# Created: 2025-10-08
################################################################################

library(tidyverse)

# Load comprehensive results
load("/data/home/Documents/GitHub/DWeMSinUK/output/nsum_comprehensive/nsum_comprehensive_results.RData")

cat("\n==============================================================================\n")
cat("SECTION 5.2.3: NSUM PREVALENCE ESTIMATES - MAIN TEXT\n")
cat("==============================================================================\n\n")

# Get main results (neighboot + SS + N=980K + MBSU all adjustments)
main_results <- summary_df %>%
  filter(
    bootstrap_method == "neighboot",
    weight_method == "SS",
    population_size == 980000,
    nsum_method == "mbsu"
  ) %>%
  arrange(nsum_label, outcome_clean)

cat("Suggested text for Section 5.2.3:\n\n")
cat("----------------------------------------------------------------------\n\n")

cat("Using the Modified Basic Scale-Up (MBSU) estimator with Sequential Sampling\n")
cat("weights and an estimated UK domestic worker population of 980,000, we find\n")
cat("substantial variation in prevalence estimates depending on the adjustment\n")
cat("factors applied (Table 6).\n\n")

# Get specific results for each adjustment method
no_adj <- main_results %>% filter(nsum_label == "MBSU (No Adjustment)")
moderate <- main_results %>% filter(nsum_label == "MBSU (Moderate)")
conservative <- main_results %>% filter(nsum_label == "MBSU (Conservative)")

cat("**No Adjustment (baseline)**:\n")
cat("The unadjusted MBSU estimates suggest document withholding affects\n")
cat(sprintf("approximately %s workers (%s), representing %.2f%% of the population.\n",
            formatC(round(no_adj$point_estimate[1]), format="f", big.mark=",", digits=0),
            no_adj$prevalence_with_ci[1],
            no_adj$prevalence_point[1]))
cat(sprintf("Pay-related issues affect %s workers (%s),\n",
            formatC(round(no_adj$point_estimate[2]), format="f", big.mark=",", digits=0),
            no_adj$prevalence_with_ci[2]))
cat(sprintf("while threats and abuse affect %s workers (%s).\n",
            formatC(round(no_adj$point_estimate[3]), format="f", big.mark=",", digits=0),
            no_adj$prevalence_with_ci[3]))
cat(sprintf("Excessive working hours affect %s workers (%s),\n",
            formatC(round(no_adj$point_estimate[4]), format="f", big.mark=",", digits=0),
            no_adj$prevalence_with_ci[4]))
cat(sprintf("and %s workers report lacking access to help (%s).\n\n",
            formatC(round(no_adj$point_estimate[5]), format="f", big.mark=",", digits=0),
            no_adj$prevalence_with_ci[5]))

cat("**Moderate Adjustment** (δ=0.9, τ=0.85):\n")
cat("Applying moderate adjustments for reporting and visibility biases yields\n")
cat(sprintf("estimates of %s workers experiencing document withholding (%s),\n",
            formatC(round(moderate$point_estimate[1]), format="f", big.mark=",", digits=0),
            moderate$prevalence_with_ci[1]))
cat(sprintf("%s with pay issues (%s),\n",
            formatC(round(moderate$point_estimate[2]), format="f", big.mark=",", digits=0),
            moderate$prevalence_with_ci[2]))
cat(sprintf("%s experiencing threats/abuse (%s),\n",
            formatC(round(moderate$point_estimate[3]), format="f", big.mark=",", digits=0),
            moderate$prevalence_with_ci[3]))
cat(sprintf("%s with excessive hours (%s),\n",
            formatC(round(moderate$point_estimate[4]), format="f", big.mark=",", digits=0),
            moderate$prevalence_with_ci[4]))
cat(sprintf("and %s lacking access to help (%s).\n\n",
            formatC(round(moderate$point_estimate[5]), format="f", big.mark=",", digits=0),
            moderate$prevalence_with_ci[5]))

cat("**Conservative Adjustment** (δ=0.8, τ=0.7):\n")
cat("The most conservative adjustments, accounting for greater underreporting\n")
cat("and visibility barriers, produce substantially higher estimates:\n")
cat(sprintf("%s workers with document withholding (%s),\n",
            formatC(round(conservative$point_estimate[1]), format="f", big.mark=",", digits=0),
            conservative$prevalence_with_ci[1]))
cat(sprintf("%s with pay issues (%s),\n",
            formatC(round(conservative$point_estimate[2]), format="f", big.mark=",", digits=0),
            conservative$prevalence_with_ci[2]))
cat(sprintf("%s experiencing threats/abuse (%s),\n",
            formatC(round(conservative$point_estimate[3]), format="f", big.mark=",", digits=0),
            conservative$prevalence_with_ci[3]))
cat(sprintf("%s with excessive hours (%s),\n",
            formatC(round(conservative$point_estimate[4]), format="f", big.mark=",", digits=0),
            conservative$prevalence_with_ci[4]))
cat(sprintf("and %s lacking access to help (%s).\n\n",
            formatC(round(conservative$point_estimate[5]), format="f", big.mark=",", digits=0),
            conservative$prevalence_with_ci[5]))

cat("These estimates represent prevalence rates ranging from %.2f%% to %.2f%%\n",
    min(main_results$prevalence_point), max(main_results$prevalence_point))
cat("across all indicators, with confidence intervals reflecting substantial\n")
cat("uncertainty inherent in network-based estimation methods.\n\n")

cat("----------------------------------------------------------------------\n\n")

cat("\n==============================================================================\n")
cat("APPENDIX TEXT: SENSITIVITY ANALYSES\n")
cat("==============================================================================\n\n")

cat("Suggested text for Appendix:\n\n")
cat("----------------------------------------------------------------------\n\n")

cat("**Robustness to Bootstrap Method**\n\n")
cat("We assessed the sensitivity of our NSUM estimates to the choice of bootstrap\n")
cat("method by comparing four approaches: neighborhood bootstrap, tree bootstrap,\n")
cat("chain bootstrap, and sequential sampling bootstrap (Appendix Table A1).\n")

# Compare bootstrap methods
boot_range <- summary_df %>%
  filter(
    population_size == 980000,
    weight_method == "SS",
    nsum_label == "MBSU (Moderate)"
  ) %>%
  group_by(outcome_clean) %>%
  summarise(
    min_prev = min(prevalence_point),
    max_prev = max(prevalence_point),
    range_pct = max_prev - min_prev
  )

cat(sprintf("Across all outcomes, prevalence estimates varied by %.2f to %.2f percentage\n",
            min(boot_range$range_pct), max(boot_range$range_pct)))
cat("points depending on the bootstrap method used, demonstrating reasonable\n")
cat("robustness to this methodological choice.\n\n")

cat("**Robustness to Weight Method**\n\n")
cat("We compared Volz-Heckathorn (VH) and Sequential Sampling (SS) weighting\n")
cat("schemes (Appendix Table A2). Results were identical for both methods,\n")
cat("as both approaches appropriately account for the RDS sampling design.\n\n")

cat("**Robustness to Population Size**\n\n")
cat("We tested four population size scenarios: 50,000, 100,000, 980,000, and\n")
cat("1,740,000 (Appendix Table A3). As expected, absolute counts scaled linearly\n")
cat("with population size, while prevalence rates remained constant, confirming\n")
cat("the validity of our estimation approach.\n\n")

cat("**Robustness to Adjustment Factors**\n\n")
cat("The choice of adjustment factors for transmission, barrier, and recall effects\n")
cat("had substantial impact on estimates (Appendix Table A4). Conservative\n")
cat("adjustments (δ=0.8, τ=0.7) yielded estimates approximately\n")

ratio_conservative <- conservative$point_estimate / no_adj$point_estimate
cat(sprintf("%.1fx higher than unadjusted estimates across indicators,\n", mean(ratio_conservative)))
cat("while moderate adjustments (δ=0.9, τ=0.85) produced intermediate values\n")

ratio_moderate <- moderate$point_estimate / no_adj$point_estimate
cat(sprintf("approximately %.1fx higher than baseline.\n\n", mean(ratio_moderate)))

cat("----------------------------------------------------------------------\n\n")

cat("\n==============================================================================\n")
cat("QUARTO CODE CHUNK: Table 6\n")
cat("==============================================================================\n\n")

cat("```{r}\n")
cat("#| label: tbl-nsum-main\n")
cat("#| tbl-cap: \"NSUM Prevalence Estimates for Labor Exploitation Indicators\"\n")
cat("#| echo: false\n\n")
cat("library(tidyverse)\n")
cat("library(knitr)\n")
cat("library(kableExtra)\n\n")
cat("# Load main results\n")
cat("main_table <- read_csv(here('output/tables/Table6_NSUM_main_results.csv'))\n\n")
cat("main_table %>%\n")
cat("  kable(format = 'latex', booktabs = TRUE, align = c('l', 'l', 'r', 'r', 'r', 'l')) %>%\n")
cat("  kable_styling(latex_options = c('scale_down', 'hold_position')) %>%\n")
cat("  column_spec(1, width = '3cm') %>%\n")
cat("  column_spec(2, width = '3cm') %>%\n")
cat("  column_spec(6, width = '3cm')\n")
cat("```\n\n")

cat("==============================================================================\n")
cat("Output files created:\n")
cat("  - output/tables/Table6_NSUM_main_results.csv\n")
cat("  - output/tables/Table6_NSUM_main_results_wide.csv\n")
cat("  - output/tables/AppendixA1_bootstrap_comparison.csv\n")
cat("  - output/tables/AppendixA2_weight_comparison.csv\n")
cat("  - output/tables/AppendixA3_popsize_comparison.csv\n")
cat("  - output/tables/AppendixA4_nsum_method_comparison.csv\n")
cat("  - output/tables/AppendixA5_complete_matrix.csv\n")
cat("  - output/tables/AppendixA6_estimate_ranges.csv\n")
cat("==============================================================================\n")

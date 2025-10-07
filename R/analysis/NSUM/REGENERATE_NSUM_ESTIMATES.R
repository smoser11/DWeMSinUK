# ==============================================================================
# REGENERATE ALL NSUM ESTIMATES WITH CORRECTED RDS WEIGHTS
# ==============================================================================
#
# This script regenerates all NSUM estimates using standard RDS package weights
# instead of the previous custom manual implementations.
#
# Workflow:
# 1. Regenerate prepared data with correct RDS weights (02-data_preparation.R)
# 2. Load or generate bootstrap samples (Boot_Step1.R)
# 3. Apply correct RDS weights to bootstrap samples (Boot_Step2_FIXED.R)
# 4. Compute NSUM estimates on weighted bootstrap samples (Boot_Step3.R)
# 5. Calculate bootstrap confidence intervals
# 6. Save results
#
# Date: 2025-01-07
# ==============================================================================

library(tidyverse)
library(here)
library(RDS)
library(parallel)

# Configuration
CONFIG <- list(
  # Bootstrap parameters
  n_bootstrap = 1000,              # Number of bootstrap replicates
  bootstrap_method = "tree",        # "tree", "neighboot", or "chain"

  # RDS weight method for NSUM
  rds_weight_method = "VH",         # "VH" (RDS-II), "SS" (RDS-SS), or "RDSI" (RDS-I)

  # Population parameters
  population_size = 980000,         # UK domestic worker population

  # NSUM estimator
  nsum_method = "mbsu",             # "mbsu" or "gnsum_symmetric"

  # Comparable indicators (CE's specifications)
  nsum_outcomes = c(
    "document_withholding_nsum",
    "pay_issues_nsum",
    "threats_abuse_nsum",
    "excessive_hours_nsum",
    "access_to_help_nsum"
  ),

  # Parallel processing
  use_parallel = TRUE,
  n_cores = 4,

  # Force recomputation flags
  force_regenerate_data = TRUE,     # Regenerate prepared_data.RData with new weights
  force_regenerate_bootstrap = FALSE, # Regenerate bootstrap samples from Step 1
  force_reweight = TRUE,            # Recompute weights on bootstrap samples

  # Output paths
  output_dir = here("output", "nsum_corrected"),
  save_intermediate = TRUE
)

# Create output directory
if (!dir.exists(CONFIG$output_dir)) {
  dir.create(CONFIG$output_dir, recursive = TRUE)
  cat("Created output directory:", CONFIG$output_dir, "\n")
}

# ==============================================================================
# STEP 0: SAVE CONFIGURATION
# ==============================================================================

cat("\n=== NSUM Recalculation Configuration ===\n")
cat("Bootstrap samples:", CONFIG$n_bootstrap, "\n")
cat("Bootstrap method:", CONFIG$bootstrap_method, "\n")
cat("RDS weight method:", CONFIG$rds_weight_method, "\n")
cat("Population size:", format(CONFIG$population_size, big.mark = ","), "\n")
cat("NSUM method:", CONFIG$nsum_method, "\n")
cat("Number of outcomes:", length(CONFIG$nsum_outcomes), "\n")
cat("Parallel processing:", CONFIG$use_parallel, "(", CONFIG$n_cores, "cores )\n")
cat("Output directory:", CONFIG$output_dir, "\n\n")

# Save configuration
saveRDS(CONFIG, file.path(CONFIG$output_dir, "nsum_config.RDS"))

# ==============================================================================
# STEP 1: REGENERATE PREPARED DATA WITH CORRECT RDS WEIGHTS
# ==============================================================================

cat("\n=== STEP 1: Regenerating Prepared Data with Correct Weights ===\n")

prepared_data_path <- here("data", "processed", "prepared_data.RData")

if (CONFIG$force_regenerate_data || !file.exists(prepared_data_path)) {
  cat("Running 02-data_preparation.R with updated weight calculations...\n")

  # First run data cleaning if needed
  cleaned_data_path <- here("data", "processed", "cleaned_data.RData")
  if (!file.exists(cleaned_data_path)) {
    cat("Running 01-data_cleaning.R first...\n")
    source(here("R", "data_processing", "01-data_cleaning.R"))
    clean_data()
  }

  # Run data preparation with standard RDS weights
  source(here("R", "data_processing", "02-data_preparation.R"))
  prepare_data()

  cat("✓ Prepared data regenerated with standard RDS package weights\n")
} else {
  cat("Using existing prepared data (set force_regenerate_data = TRUE to regenerate)\n")
}

# Load prepared data
load(prepared_data_path)
cat("Loaded prepared data: n =", nrow(dd), "\n")

# Verify correct weights exist
required_weight_cols <- c("wt.RDS1_document_withholding", "wt.vh_980k", "wt.SS_980k")
missing_weights <- setdiff(required_weight_cols, names(dd))
if (length(missing_weights) > 0) {
  stop("Missing required weight columns: ", paste(missing_weights, collapse = ", "))
}
cat("✓ Verified weight columns present\n\n")

# ==============================================================================
# STEP 2: GENERATE OR LOAD BOOTSTRAP SAMPLES
# ==============================================================================

cat("\n=== STEP 2: Bootstrap Samples (Step 1) ===\n")

bootstrap_samples_path <- file.path(CONFIG$output_dir, "bootstrap_samples_step1.RData")

if (CONFIG$force_regenerate_bootstrap || !file.exists(bootstrap_samples_path)) {
  cat("Generating", CONFIG$n_bootstrap, "bootstrap samples using", CONFIG$bootstrap_method, "method...\n")

  # Source Boot_Step1.R
  source(here("R", "analysis", "NSUM", "Boot_Step1.r"))

  # Generate bootstrap samples
  boot_samples <- bootstrap_rds_sample(
    rds_sample = rd.dd,
    method = CONFIG$bootstrap_method,
    B = CONFIG$n_bootstrap,
    traits = CONFIG$nsum_outcomes,
    degree_col = "known_network_size",
    return_rds_df = FALSE,  # Return as data frames for easier manipulation
    verbose = TRUE
  )

  # Save bootstrap samples
  if (CONFIG$save_intermediate) {
    save(boot_samples, file = bootstrap_samples_path)
    cat("✓ Bootstrap samples saved to:", bootstrap_samples_path, "\n")
  }

} else {
  cat("Loading existing bootstrap samples from Step 1...\n")
  load(bootstrap_samples_path)
  cat("✓ Loaded", length(boot_samples), "bootstrap samples\n")
}

cat("Bootstrap sample size range:",
    min(sapply(boot_samples, nrow)), "to",
    max(sapply(boot_samples, nrow)), "\n\n")

# ==============================================================================
# STEP 3: APPLY CORRECT RDS WEIGHTS TO BOOTSTRAP SAMPLES
# ==============================================================================

cat("\n=== STEP 3: Adding Correct RDS Weights (Step 2 - FIXED) ===\n")

weighted_samples_path <- file.path(CONFIG$output_dir, "bootstrap_samples_weighted_step2.RData")

if (CONFIG$force_reweight || !file.exists(weighted_samples_path)) {
  cat("Applying standard RDS package weights to bootstrap samples...\n")
  cat("Weight method:", CONFIG$rds_weight_method, "\n")
  cat("Population size:", format(CONFIG$population_size, big.mark = ","), "\n\n")

  # Source Boot_Step2_FIXED.R
  source(here("R", "analysis", "NSUM", "Boot_Step2_FIXED.R"))

  # Use the first outcome as reference for weight calculation
  reference_outcome <- CONFIG$nsum_outcomes[1]
  cat("Using reference outcome for weights:", reference_outcome, "\n")

  # Apply weights to all bootstrap samples
  weighted_samples <- compute_weights_batch_standard(
    bootstrap_samples = boot_samples,
    weight_method = CONFIG$rds_weight_method,
    population_size = CONFIG$population_size,
    outcome_variable = reference_outcome,
    parallel = CONFIG$use_parallel,
    n_cores = CONFIG$n_cores,
    verbose = TRUE
  )

  # Save weighted samples
  if (CONFIG$save_intermediate) {
    save(weighted_samples, file = weighted_samples_path)
    cat("✓ Weighted bootstrap samples saved to:", weighted_samples_path, "\n")
  }

} else {
  cat("Loading existing weighted bootstrap samples...\n")
  load(weighted_samples_path)
  cat("✓ Loaded", length(weighted_samples), "weighted bootstrap samples\n")
}

# Verify weights are present
first_sample <- weighted_samples[[1]]
weight_cols_present <- c("weight_vh", "weight_rds_i", "weight_rds_ss", "inclusion_prob")
missing_weights <- setdiff(weight_cols_present, names(first_sample))
if (length(missing_weights) > 0) {
  stop("Missing weight columns in bootstrap samples: ", paste(missing_weights, collapse = ", "))
}
cat("✓ Verified all weight columns present in bootstrap samples\n\n")

# ==============================================================================
# STEP 4: COMPUTE NSUM ESTIMATES ON WEIGHTED BOOTSTRAP SAMPLES
# ==============================================================================

cat("\n=== STEP 4: Computing NSUM Estimates (Step 3) ===\n")

# Source Boot_Step3.R
source(here("R", "analysis", "NSUM", "Boot_Step3.R"))

# Initialize results storage
nsum_results <- list()
bootstrap_distributions <- list()

# Determine weight column names for original data vs bootstrap samples
if (CONFIG$rds_weight_method == "VH") {
  original_weight_col <- paste0("wt.vh_", gsub("000$", "k", as.character(CONFIG$population_size)))
  bootstrap_weight_col <- "weight_vh"
} else if (CONFIG$rds_weight_method == "SS") {
  original_weight_col <- paste0("wt.SS_", gsub("000$", "k", as.character(CONFIG$population_size)))
  bootstrap_weight_col <- "weight_rds_ss"
} else if (CONFIG$rds_weight_method == "RDSI") {
  # RDS-I is outcome-specific - will be set per outcome
  bootstrap_weight_col <- "weight_rds_i"
}

cat("Weight columns: original =", original_weight_col, ", bootstrap =", bootstrap_weight_col, "\n")

# Process each outcome variable
for (outcome_var in CONFIG$nsum_outcomes) {

  cat("\n--- Processing:", outcome_var, "---\n")

  # For RDS-I, set outcome-specific weight column
  if (CONFIG$rds_weight_method == "RDSI") {
    outcome_base <- gsub("_nsum$", "_rds", outcome_var)
    original_weight_col <- paste0("wt.RDS1_", outcome_base)
  }

  # Estimate on original data (point estimate)
  cat("Computing point estimate on original data...\n")
  cat("Using weight column:", original_weight_col, "\n")

  point_estimate_result <- estimate_nsum(
    data = dd,
    nsum_method = CONFIG$nsum_method,
    outcome_variable = outcome_var,
    degree_variable = "known_network_size",
    frame_size = CONFIG$population_size,
    weight_column = original_weight_col,
    verbose = FALSE
  )

  # Extract the numeric estimate (use N_H_estimate field)
  point_estimate <- point_estimate_result$N_H_estimate
  cat("Point estimate:", point_estimate, "\n")

  # Estimate on all bootstrap samples
  cat("Computing estimates on", length(weighted_samples), "bootstrap samples...\n")

  boot_estimates <- sapply(weighted_samples, function(boot_sample) {
    tryCatch({
      result <- estimate_nsum(
        data = boot_sample,
        nsum_method = CONFIG$nsum_method,
        outcome_variable = outcome_var,
        degree_variable = "known_network_size",
        frame_size = CONFIG$population_size,
        weight_column = bootstrap_weight_col,
        verbose = FALSE
      )
      return(result$N_H_estimate)  # Use N_H_estimate, not $estimate
    }, error = function(e) {
      return(NA)
    })
  })

  # Remove failed estimates
  boot_estimates_valid <- boot_estimates[!is.na(boot_estimates)]
  n_valid <- length(boot_estimates_valid)
  n_failed <- CONFIG$n_bootstrap - n_valid

  if (n_failed > 0) {
    cat("Warning:", n_failed, "bootstrap samples failed (",
        round(100 * n_failed / CONFIG$n_bootstrap, 1), "%)\n")
  }

  # Calculate bootstrap confidence intervals
  if (n_valid >= 100) {
    ci_95 <- quantile(boot_estimates_valid, probs = c(0.025, 0.975), na.rm = TRUE)
    ci_90 <- quantile(boot_estimates_valid, probs = c(0.05, 0.95), na.rm = TRUE)
    boot_se <- sd(boot_estimates_valid, na.rm = TRUE)
    boot_mean <- mean(boot_estimates_valid, na.rm = TRUE)

    cat("Bootstrap mean:", round(boot_mean, 2), "\n")
    cat("Bootstrap SE:", round(boot_se, 2), "\n")
    cat("95% CI: [", round(ci_95[1], 2), ",", round(ci_95[2], 2), "]\n")
    cat("90% CI: [", round(ci_90[1], 2), ",", round(ci_90[2], 2), "]\n")

  } else {
    warning("Insufficient valid bootstrap estimates (", n_valid, ") for CI calculation")
    ci_95 <- c(NA, NA)
    ci_90 <- c(NA, NA)
    boot_se <- NA
    boot_mean <- NA
  }

  # Store results
  nsum_results[[outcome_var]] <- list(
    outcome_variable = outcome_var,
    point_estimate = point_estimate,  # Already extracted as numeric
    bootstrap_mean = boot_mean,
    bootstrap_se = boot_se,
    ci_95_lower = ci_95[1],
    ci_95_upper = ci_95[2],
    ci_90_lower = ci_90[1],
    ci_90_upper = ci_90[2],
    n_bootstrap = CONFIG$n_bootstrap,
    n_valid = n_valid,
    n_failed = n_failed,
    population_size = CONFIG$population_size,
    nsum_method = CONFIG$nsum_method,
    rds_weight_method = CONFIG$rds_weight_method
  )

  bootstrap_distributions[[outcome_var]] <- boot_estimates_valid
}

cat("\n✓ All NSUM estimates computed\n\n")

# ==============================================================================
# STEP 5: CREATE SUMMARY TABLES AND SAVE RESULTS
# ==============================================================================

cat("\n=== STEP 5: Creating Summary Tables ===\n")

# Create summary data frame
summary_df <- do.call(rbind, lapply(names(nsum_results), function(var) {
  res <- nsum_results[[var]]
  data.frame(
    outcome_variable = var,
    point_estimate = res$point_estimate,
    bootstrap_mean = res$bootstrap_mean,
    bootstrap_se = res$bootstrap_se,
    ci_95_lower = res$ci_95_lower,
    ci_95_upper = res$ci_95_upper,
    ci_90_lower = res$ci_90_lower,
    ci_90_upper = res$ci_90_upper,
    n_valid = res$n_valid,
    n_failed = res$n_failed,
    stringsAsFactors = FALSE
  )
}))

# Add formatted columns
summary_df <- summary_df %>%
  mutate(
    # Prevalence as percentage
    prevalence_pct = (point_estimate / CONFIG$population_size) * 100,

    # Formatted estimate with CI
    estimate_with_ci = sprintf("%.0f (%.0f - %.0f)",
                               point_estimate, ci_95_lower, ci_95_upper),

    # Formatted prevalence with CI
    prevalence_with_ci = sprintf("%.2f%% (%.2f%% - %.2f%%)",
                                 prevalence_pct,
                                 (ci_95_lower / CONFIG$population_size) * 100,
                                 (ci_95_upper / CONFIG$population_size) * 100)
  )

# Print summary table
cat("\n=== NSUM Estimates Summary ===\n")
print(summary_df %>%
        select(outcome_variable, point_estimate, bootstrap_se,
               ci_95_lower, ci_95_upper, n_valid))

# Save all results
cat("\nSaving results...\n")

# Save full results object
saveRDS(nsum_results, file.path(CONFIG$output_dir, "nsum_results_full.RDS"))
saveRDS(bootstrap_distributions, file.path(CONFIG$output_dir, "bootstrap_distributions.RDS"))

# Save summary table (CSV for easy viewing)
write.csv(summary_df,
          file.path(CONFIG$output_dir, "nsum_estimates_summary.csv"),
          row.names = FALSE)

# Save summary table (RData for R use)
save(summary_df, nsum_results, bootstrap_distributions, CONFIG,
     file = file.path(CONFIG$output_dir, "enhanced_nsum_results.RData"))

cat("✓ Results saved to:", CONFIG$output_dir, "\n")

# ==============================================================================
# STEP 6: CREATE VISUALIZATION
# ==============================================================================

cat("\n=== STEP 6: Creating Visualizations ===\n")

# Forest plot of estimates with CIs
library(ggplot2)

plot_df <- summary_df %>%
  mutate(
    outcome_clean = case_when(
      outcome_variable == "document_withholding_nsum" ~ "Document Withholding",
      outcome_variable == "pay_issues_nsum" ~ "Pay Issues",
      outcome_variable == "threats_abuse_nsum" ~ "Threats/Abuse",
      outcome_variable == "excessive_hours_nsum" ~ "Excessive Hours",
      outcome_variable == "access_to_help_nsum" ~ "Access to Help",
      TRUE ~ outcome_variable
    )
  ) %>%
  arrange(desc(point_estimate))

forest_plot <- ggplot(plot_df, aes(x = point_estimate, y = reorder(outcome_clean, point_estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_95_lower, xmax = ci_95_upper), height = 0.2) +
  labs(
    title = "NSUM Estimates of Exploitation (Corrected RDS Weights)",
    subtitle = paste0("Population: ", format(CONFIG$population_size, big.mark = ","),
                     " | Method: ", CONFIG$nsum_method, " | Weights: ", CONFIG$rds_weight_method),
    x = "Estimated Population Size",
    y = ""
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

ggsave(file.path(CONFIG$output_dir, "nsum_forest_plot.png"),
       forest_plot, width = 10, height = 6, dpi = 300)

cat("✓ Forest plot saved\n")

# ==============================================================================
# COMPLETION MESSAGE
# ==============================================================================

cat("\n" , rep("=", 80), "\n", sep = "")
cat("NSUM RECALCULATION COMPLETE\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Summary:\n")
cat("- Bootstrap samples:", CONFIG$n_bootstrap, "\n")
cat("- Valid estimates:", min(summary_df$n_valid), "-", max(summary_df$n_valid), "\n")
cat("- Outcomes analyzed:", nrow(summary_df), "\n")
cat("- Method:", CONFIG$nsum_method, "with", CONFIG$rds_weight_method, "weights\n")
cat("- Population size:", format(CONFIG$population_size, big.mark = ","), "\n\n")

cat("Output files:\n")
cat("1. Full results:", file.path(CONFIG$output_dir, "nsum_results_full.RDS"), "\n")
cat("2. Summary table:", file.path(CONFIG$output_dir, "nsum_estimates_summary.csv"), "\n")
cat("3. Combined results:", file.path(CONFIG$output_dir, "enhanced_nsum_results.RData"), "\n")
cat("4. Forest plot:", file.path(CONFIG$output_dir, "nsum_forest_plot.png"), "\n\n")

cat("Next steps:\n")
cat("1. Review summary table:", file.path(CONFIG$output_dir, "nsum_estimates_summary.csv"), "\n")
cat("2. Compare with previous estimates to assess impact of weight correction\n")
cat("3. Update paper with new estimates and confidence intervals\n")
cat("4. Consider sensitivity analysis with different weight methods (VH vs SS)\n\n")

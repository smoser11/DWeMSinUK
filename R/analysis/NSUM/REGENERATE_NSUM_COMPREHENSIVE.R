# ==============================================================================
# COMPREHENSIVE NSUM ESTIMATION WITH MULTIPLE CONFIGURATIONS
# ==============================================================================
#
# This script performs NSUM estimation across multiple:
# - RDS weight methods (VH, SS, RDSI)
# - NSUM estimators (MBSU, GNSUM Symmetric)
# - Population sizes (50K, 100K, 980K, 1.74M)
# - Adjustment factors for MBSU (Feehan & Salganik 2016)
#
# Results are saved with informative file names for comparison
#
# Date: 2025-01-07
# ==============================================================================

library(tidyverse)
library(here)
library(RDS)
library(parallel)

# ==============================================================================
# CONFIGURATION: DEFINE ALL PARAMETER COMBINATIONS
# ==============================================================================

# Base configuration
BASE_CONFIG <- list(
  # Bootstrap parameters (shared across all runs)
  n_bootstrap = 1000,
  bootstrap_method = "tree",

  # Outcomes to analyze
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
  force_regenerate_data = FALSE,      # Set TRUE if data needs updating
  force_regenerate_bootstrap = FALSE,  # Set TRUE to regenerate bootstrap samples
  force_reweight = FALSE,              # Set TRUE to recompute weights

  # Output
  output_base_dir = here("output", "nsum_comprehensive"),
  save_intermediate = TRUE
)

# Population sizes to test
POPULATION_SIZES <- c(50000, 100000, 980000, 1740000)

# RDS weight methods to test
WEIGHT_METHODS <- c("VH", "SS")  # "RDSI" is outcome-specific, handle separately

# NSUM estimators to test
NSUM_METHODS <- list(
  mbsu_no_adj = list(
    method = "mbsu",
    label = "MBSU (No Adjustment)",
    adjustment_factors = list(delta = 1.0, tau = 1.0, rho = 1.0)
  ),
  mbsu_conservative = list(
    method = "mbsu",
    label = "MBSU (Conservative)",
    adjustment_factors = list(delta = 0.8, tau = 0.7, rho = 1.0)
  ),
  mbsu_moderate = list(
    method = "mbsu",
    label = "MBSU (Moderate)",
    adjustment_factors = list(delta = 0.9, tau = 0.85, rho = 1.0)
  ),
  gnsum_symmetric = list(
    method = "gnsum_symmetric",
    label = "GNSUM Symmetric",
    adjustment_factors = NULL,  # Not applicable
    needs_hidden_indicator = TRUE
  )
)

# Create output directory
if (!dir.exists(BASE_CONFIG$output_base_dir)) {
  dir.create(BASE_CONFIG$output_base_dir, recursive = TRUE)
}

cat("\n", rep("=", 80), "\n", sep = "")
cat("COMPREHENSIVE NSUM ESTIMATION\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Configuration:\n")
cat("- Bootstrap samples:", BASE_CONFIG$n_bootstrap, "\n")
cat("- Population sizes:", paste(format(POPULATION_SIZES, big.mark = ","), collapse = ", "), "\n")
cat("- Weight methods:", paste(WEIGHT_METHODS, collapse = ", "), "\n")
cat("- NSUM methods:", length(NSUM_METHODS), "\n")
cat("- Total configurations:", length(POPULATION_SIZES) * length(WEIGHT_METHODS) * length(NSUM_METHODS), "\n")
cat("- Output directory:", BASE_CONFIG$output_base_dir, "\n\n")

# ==============================================================================
# STEP 0: PREPARE DATA AND BOOTSTRAP SAMPLES
# ==============================================================================

cat("\n=== Preparing Data and Bootstrap Samples ===\n")

# Load prepared data
prepared_data_path <- here("data", "processed", "prepared_data.RData")
if (!file.exists(prepared_data_path)) {
  stop("Prepared data not found. Run 02-data_preparation.R first.")
}
load(prepared_data_path)
cat("✓ Loaded prepared data: n =", nrow(dd), "\n")

# Load or generate bootstrap samples (shared across all configurations)
bootstrap_samples_path <- file.path(BASE_CONFIG$output_base_dir, "bootstrap_samples_shared.RData")

if (BASE_CONFIG$force_regenerate_bootstrap || !file.exists(bootstrap_samples_path)) {
  cat("Generating", BASE_CONFIG$n_bootstrap, "bootstrap samples...\n")

  source(here("R", "analysis", "NSUM", "Boot_Step1.r"))

  boot_samples <- bootstrap_rds_sample(
    rds_sample = rd.dd,
    method = BASE_CONFIG$bootstrap_method,
    B = BASE_CONFIG$n_bootstrap,
    traits = BASE_CONFIG$nsum_outcomes,
    degree_col = "known_network_size",
    return_rds_df = FALSE,
    verbose = TRUE
  )

  save(boot_samples, file = bootstrap_samples_path)
  cat("✓ Bootstrap samples saved\n")
} else {
  cat("Loading existing bootstrap samples...\n")
  load(bootstrap_samples_path)
  cat("✓ Loaded", length(boot_samples), "bootstrap samples\n")
}

# Source required scripts
source(here("R", "analysis", "NSUM", "Boot_Step2_FIXED.R"))
source(here("R", "analysis", "NSUM", "Boot_Step3.R"))

# ==============================================================================
# STEP 1: RUN ALL CONFIGURATIONS
# ==============================================================================

cat("\n=== Running All Configurations ===\n\n")

# Initialize results storage
all_results <- list()
config_counter <- 0
total_configs <- length(POPULATION_SIZES) * length(WEIGHT_METHODS) * length(NSUM_METHODS)

# Loop over all combinations
for (pop_size in POPULATION_SIZES) {
  for (weight_method in WEIGHT_METHODS) {

    # Apply weights to bootstrap samples for this combination
    cat("\n--- Applying", weight_method, "weights for N =", format(pop_size, big.mark = ","), "---\n")

    weighted_samples_path <- file.path(
      BASE_CONFIG$output_base_dir,
      sprintf("bootstrap_weighted_%s_N%s.RData", weight_method, pop_size)
    )

    if (BASE_CONFIG$force_reweight || !file.exists(weighted_samples_path)) {
      weighted_samples <- compute_weights_batch_standard(
        bootstrap_samples = boot_samples,
        weight_method = weight_method,
        population_size = pop_size,
        outcome_variable = BASE_CONFIG$nsum_outcomes[1],  # Reference outcome
        parallel = BASE_CONFIG$use_parallel,
        n_cores = BASE_CONFIG$n_cores,
        verbose = FALSE
      )
      save(weighted_samples, file = weighted_samples_path)
      cat("✓ Weighted samples saved\n")
    } else {
      load(weighted_samples_path)
      cat("✓ Loaded weighted samples\n")
    }

    # Determine weight column names (must match 02-data_preparation.R naming)
    # Actual column names: wt.vh_050k, wt.vh_100k, wt.vh_980k, wt.vh_1740k
    pop_suffix <- if (pop_size == 50000) {
      "050k"
    } else if (pop_size == 100000) {
      "100k"
    } else if (pop_size == 980000) {
      "980k"
    } else if (pop_size == 1740000) {
      "1740k"
    } else {
      stop("Unsupported population size: ", pop_size)
    }

    if (weight_method == "VH") {
      original_weight_col <- paste0("wt.vh_", pop_suffix)
      bootstrap_weight_col <- "weight_vh"
    } else if (weight_method == "SS") {
      original_weight_col <- paste0("wt.SS_", pop_suffix)
      bootstrap_weight_col <- "weight_rds_ss"
    }

    # Test each NSUM method
    for (nsum_config_name in names(NSUM_METHODS)) {
      nsum_config <- NSUM_METHODS[[nsum_config_name]]

      config_counter <- config_counter + 1
      cat(sprintf("\n[%d/%d] %s | %s weights | N=%s\n",
                  config_counter, total_configs,
                  nsum_config$label, weight_method,
                  format(pop_size, big.mark = ",")))

      config_id <- sprintf("%s_%s_N%s", nsum_config_name, weight_method, pop_size)

      # Process each outcome
      outcome_results <- list()

      for (outcome_var in BASE_CONFIG$nsum_outcomes) {

        cat("  Processing:", outcome_var, "...")

        # Build estimation parameters
        est_params <- list(
          data = dd,
          nsum_method = nsum_config$method,
          outcome_variable = outcome_var,
          degree_variable = "known_network_size",
          frame_size = pop_size,
          weight_column = original_weight_col,
          verbose = FALSE
        )

        # Add method-specific parameters
        if (nsum_config$method == "mbsu") {
          est_params$adjustment_factors <- nsum_config$adjustment_factors
        } else if (nsum_config$method == "gnsum_symmetric") {
          # Need hidden member indicator (RDS version of outcome)
          hidden_indicator <- gsub("_nsum$", "_rds", outcome_var)
          est_params$hidden_member_indicator <- hidden_indicator
          est_params$in_reports_variable <- NULL
        }

        # Point estimate on original data
        point_result <- tryCatch({
          do.call(estimate_nsum, est_params)
        }, error = function(e) {
          cat(" ERROR:", e$message, "\n")
          return(NULL)
        })

        if (is.null(point_result)) {
          next
        }

        # Extract point estimate with safety checks
        point_estimate <- point_result$N_H_estimate
        if (is.null(point_estimate) || is.list(point_estimate) ||
            !is.numeric(point_estimate) || length(point_estimate) != 1) {
          cat(" ERROR: Invalid point estimate structure\n")
          next
        }
        point_estimate <- as.numeric(point_estimate)

        # Bootstrap estimates
        est_params$data <- NULL  # Will be replaced in loop
        est_params$weight_column <- bootstrap_weight_col

        boot_estimates <- sapply(weighted_samples, function(boot_sample) {
          tryCatch({
            est_params$data <- boot_sample
            result <- do.call(estimate_nsum, est_params)
            # Extract numeric estimate (handle different return structures)
            estimate <- result$N_H_estimate
            if (is.null(estimate)) return(NA)
            if (is.list(estimate)) return(NA)
            if (!is.numeric(estimate)) return(NA)
            if (length(estimate) != 1) return(NA)
            return(as.numeric(estimate))
          }, error = function(e) {
            return(NA)
          })
        })

        # Calculate CIs
        boot_valid <- boot_estimates[!is.na(boot_estimates)]
        n_valid <- length(boot_valid)

        if (n_valid >= 100) {
          ci_95 <- quantile(boot_valid, probs = c(0.025, 0.975), na.rm = TRUE)
          boot_se <- sd(boot_valid, na.rm = TRUE)
          boot_mean <- mean(boot_valid, na.rm = TRUE)
          cat(" ✓ (", n_valid, " valid)\n", sep = "")
        } else {
          ci_95 <- c(NA, NA)
          boot_se <- NA
          boot_mean <- NA
          cat(" WARNING: Only", n_valid, "valid estimates\n")
        }

        # Store results
        outcome_results[[outcome_var]] <- list(
          outcome = outcome_var,
          point_estimate = point_estimate,
          bootstrap_mean = boot_mean,
          bootstrap_se = boot_se,
          ci_95_lower = ci_95[1],
          ci_95_upper = ci_95[2],
          n_valid = n_valid,
          prevalence_point = (point_estimate / pop_size) * 100,
          prevalence_ci_lower = (ci_95[1] / pop_size) * 100,
          prevalence_ci_upper = (ci_95[2] / pop_size) * 100
        )
      }

      # Store configuration results
      all_results[[config_id]] <- list(
        config_id = config_id,
        nsum_method = nsum_config$method,
        nsum_label = nsum_config$label,
        weight_method = weight_method,
        population_size = pop_size,
        adjustment_factors = nsum_config$adjustment_factors,
        outcomes = outcome_results,
        timestamp = Sys.time()
      )
    }
  }
}

cat("\n✓ All configurations completed\n\n")

# ==============================================================================
# STEP 2: CREATE SUMMARY TABLES
# ==============================================================================

cat("\n=== Creating Summary Tables ===\n")

# Flatten results into data frame
summary_rows <- list()

for (config_id in names(all_results)) {
  config <- all_results[[config_id]]

  for (outcome_var in names(config$outcomes)) {
    result <- config$outcomes[[outcome_var]]

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      config_id = config_id,
      nsum_method = config$nsum_method,
      nsum_label = config$nsum_label,
      weight_method = config$weight_method,
      population_size = config$population_size,
      delta = ifelse(is.null(config$adjustment_factors), NA, config$adjustment_factors$delta),
      tau = ifelse(is.null(config$adjustment_factors), NA, config$adjustment_factors$tau),
      rho = ifelse(is.null(config$adjustment_factors), NA, config$adjustment_factors$rho),
      outcome = result$outcome,
      point_estimate = result$point_estimate,
      bootstrap_mean = result$bootstrap_mean,
      bootstrap_se = result$bootstrap_se,
      ci_95_lower = result$ci_95_lower,
      ci_95_upper = result$ci_95_upper,
      prevalence_point = result$prevalence_point,
      prevalence_ci_lower = result$prevalence_ci_lower,
      prevalence_ci_upper = result$prevalence_ci_upper,
      n_valid = result$n_valid,
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- bind_rows(summary_rows)

# Add formatted columns
summary_df <- summary_df %>%
  mutate(
    outcome_clean = case_when(
      outcome == "document_withholding_nsum" ~ "Document Withholding",
      outcome == "pay_issues_nsum" ~ "Pay Issues",
      outcome == "threats_abuse_nsum" ~ "Threats/Abuse",
      outcome == "excessive_hours_nsum" ~ "Excessive Hours",
      outcome == "access_to_help_nsum" ~ "Access to Help",
      TRUE ~ outcome
    ),
    config_label = sprintf("%s | %s | N=%s",
                          nsum_label, weight_method,
                          format(population_size, big.mark = ",")),
    prevalence_with_ci = sprintf("%.2f%% (%.2f%% - %.2f%%)",
                                prevalence_point,
                                prevalence_ci_lower,
                                prevalence_ci_upper)
  )

# Save summary table
write.csv(summary_df,
          file.path(BASE_CONFIG$output_base_dir, "nsum_comprehensive_summary.csv"),
          row.names = FALSE)

cat("✓ Summary table saved\n")

# Save full results object
saveRDS(all_results, file.path(BASE_CONFIG$output_base_dir, "nsum_comprehensive_results.RDS"))
save(all_results, summary_df, BASE_CONFIG, NSUM_METHODS,
     file = file.path(BASE_CONFIG$output_base_dir, "nsum_comprehensive_results.RData"))

cat("✓ Full results saved\n\n")

# ==============================================================================
# STEP 3: CREATE PREVALENCE VISUALIZATIONS
# ==============================================================================

cat("\n=== Creating Visualizations ===\n")

library(ggplot2)

# 1. Forest plot by outcome (prevalence %) - comparing all configurations
for (outcome in unique(summary_df$outcome)) {

  plot_data <- summary_df %>%
    filter(outcome == !!outcome) %>%
    arrange(desc(prevalence_point))

  outcome_name <- unique(plot_data$outcome_clean)

  p <- ggplot(plot_data, aes(x = prevalence_point, y = reorder(config_label, prevalence_point))) +
    geom_point(aes(color = nsum_label), size = 3) +
    geom_errorbarh(aes(xmin = prevalence_ci_lower, xmax = prevalence_ci_upper,
                       color = nsum_label), height = 0.3) +
    facet_wrap(~ outcome_clean, scales = "free_x") +
    labs(
      title = paste0("Prevalence Estimates: ", outcome_name),
      subtitle = "Comparing RDS weights, NSUM methods, and population sizes",
      x = "Prevalence (%)",
      y = "",
      color = "NSUM Method"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      axis.text.y = element_text(size = 8)
    )

  filename <- paste0("prevalence_forest_", gsub("_nsum", "", outcome), ".png")
  ggsave(file.path(BASE_CONFIG$output_base_dir, filename),
         p, width = 12, height = 10, dpi = 300)

  cat("✓ Saved:", filename, "\n")
}

# 2. Faceted forest plot - all outcomes, grouped by method
p_all <- ggplot(summary_df %>% filter(population_size == 980000),
                aes(x = prevalence_point, y = reorder(outcome_clean, prevalence_point))) +
  geom_point(aes(color = weight_method), size = 2.5) +
  geom_errorbarh(aes(xmin = prevalence_ci_lower, xmax = prevalence_ci_upper,
                     color = weight_method), height = 0.2) +
  facet_wrap(~ nsum_label, ncol = 2) +
  labs(
    title = "Prevalence Estimates Across All Outcomes (N = 980,000)",
    subtitle = "Comparing NSUM methods and RDS weight schemes",
    x = "Prevalence (%)",
    y = "",
    color = "RDS Weights"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(BASE_CONFIG$output_base_dir, "prevalence_all_outcomes.png"),
       p_all, width = 14, height = 10, dpi = 300)

cat("✓ Saved: prevalence_all_outcomes.png\n")

# 3. Population size sensitivity (for main configuration)
main_config <- summary_df %>%
  filter(nsum_label == "MBSU (No Adjustment)", weight_method == "VH")

p_sensitivity <- ggplot(main_config,
                       aes(x = population_size, y = prevalence_point,
                           color = outcome_clean)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = prevalence_ci_lower, ymax = prevalence_ci_upper,
                  fill = outcome_clean), alpha = 0.2, color = NA) +
  scale_x_continuous(labels = scales::comma,
                    breaks = POPULATION_SIZES) +
  labs(
    title = "Population Size Sensitivity Analysis",
    subtitle = "MBSU (No Adjustment) with VH weights",
    x = "Population Size",
    y = "Prevalence (%)",
    color = "Outcome",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave(file.path(BASE_CONFIG$output_base_dir, "population_sensitivity.png"),
       p_sensitivity, width = 12, height = 8, dpi = 300)

cat("✓ Saved: population_sensitivity.png\n")

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("COMPREHENSIVE NSUM ESTIMATION COMPLETE\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Summary:\n")
cat("- Total configurations:", nrow(summary_df) / length(BASE_CONFIG$nsum_outcomes), "\n")
cat("- Total estimates:", nrow(summary_df), "\n")
cat("- Output directory:", BASE_CONFIG$output_base_dir, "\n\n")

cat("Key files:\n")
cat("1. Summary table: nsum_comprehensive_summary.csv\n")
cat("2. Full results: nsum_comprehensive_results.RData\n")
cat("3. Prevalence plots: prevalence_*.png\n")
cat("4. All outcomes: prevalence_all_outcomes.png\n")
cat("5. Sensitivity: population_sensitivity.png\n\n")

# Print sample of results
cat("Sample results (N=980K, VH weights):\n")
print(summary_df %>%
        filter(population_size == 980000, weight_method == "VH") %>%
        select(nsum_label, outcome_clean, prevalence_with_ci) %>%
        head(10))

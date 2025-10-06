# ==============================================================================
# COMPLETE NSUM ESTIMATION WORKFLOW
# ==============================================================================
#
# Analysis: NSUM population size estimation with bootstrap confidence intervals
# Resampling: Neighborhood bootstrap
# Weighting: Gile's Sequential Sampling (SS)
# Output: Point estimates + 95% bootstrap CIs
#
# Author: NSUM Analysis Team
# Date: 2025-01-XX
# ==============================================================================

library(tidyverse)
library(here)
library(RDS)
library(knitr)
library(kableExtra)

cat("=== NSUM COMPLETE WORKFLOW ===\n")
cat("Resampling method: Neighborhood Bootstrap\n")
cat("Weighting method: Gile's Sequential Sampling (SS)\n")
cat("Confidence level: 95%\n\n")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

config <- list(
  # Data
  data_file = here("data", "processed", "prepared_data.RData"),

  # NSUM indicators
  nsum_indicators = c(
    "document_withholding_nsum",
    "pay_issues_nsum",
    "threats_abuse_nsum",
    "excessive_hours_nsum",
    "access_to_help_nsum"
  ),

  # Corresponding RDS indicators (for labels)
  rds_indicators = c(
    "document_withholding_rds",
    "pay_issues_rds",
    "threats_abuse_rds",
    "excessive_hours_rds",
    "access_to_help_rds"
  ),

  # Population parameters
  frame_population = 980000,  # UK domestic workers

  # Bootstrap parameters
  bootstrap_method = "neighboot",
  bootstrap_samples = 1000,
  confidence_level = 0.95,

  # RDS weighting
  weight_method = "SS",  # Gile's Sequential Sampling

  # NSUM parameters
  degree_variable = "known_network_size",
  adjustment_factors = list(
    delta = 1.0,  # No transmission bias adjustment
    tau = 1.0,    # No barrier effect adjustment
    rho = 1.0     # No popularity bias adjustment
  ),

  # Computational
  use_parallel = TRUE,
  n_cores = 4,
  random_seed = 12345,

  # Output
  output_dir = here("output", "nsum_complete"),
  save_bootstrap_samples = FALSE,
  verbose = TRUE
)

# Create output directory
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
  cat("Created output directory:", config$output_dir, "\n")
}

# ==============================================================================
# LOAD REQUIRED SCRIPTS
# ==============================================================================

cat("\n=== Loading required scripts ===\n")

# Load core estimators FIRST and save reference
source(here("R", "analysis", "nsum_core_estimators.R"))
core_estimate_mbsu <- estimate_mbsu  # Save core function before it gets shadowed

# Load bootstrap scripts
source(here("R", "analysis", "NSUM", "Boot_Step1.r"))
source(here("R", "analysis", "NSUM", "Boot_Step2.R"))

# DON'T source Boot_Step3.R here - it shadows estimate_mbsu()
# We'll load it only on parallel workers where we need the wrappers

cat("✓ Core estimators and bootstrap scripts loaded\n")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("\n=== Loading data ===\n")
load(config$data_file)

if (!exists("dd") || !exists("rd.dd")) {
  stop("Required data objects (dd, rd.dd) not found in ", config$data_file)
}

cat("✓ Data loaded:\n")
cat("  - dd: ", nrow(dd), " observations\n")
cat("  - rd.dd: ", nrow(rd.dd), " observations (RDS object)\n")

# Validate NSUM indicators exist
missing_vars <- setdiff(config$nsum_indicators, names(dd))
if (length(missing_vars) > 0) {
  stop("Missing NSUM indicators: ", paste(missing_vars, collapse = ", "))
}
cat("✓ All", length(config$nsum_indicators), "NSUM indicators found\n")

# ==============================================================================
# STEP 0: POINT ESTIMATES (Original Data with SS Weights)
# ==============================================================================

cat("\n=== STEP 0: Point Estimates ===\n")
cat("Computing NSUM point estimates with SS weights...\n")

point_estimates <- list()

for (indicator in config$nsum_indicators) {

  if (config$verbose) cat("  Processing:", indicator, "... ")

  # Get SS weights for this indicator
  ss_weight_col <- paste0("wt.SS_", config$frame_population / 1000, "k")

  if (!ss_weight_col %in% names(dd)) {
    # Fallback to generic SS weights if indicator-specific not available
    ss_weight_col <- "wt.SS_980k"
  }

  if (!ss_weight_col %in% names(dd)) {
    warning("SS weights not found for ", indicator, ", skipping")
    next
  }

  # Extract weights
  ss_weights <- dd[[ss_weight_col]]

  # Estimate using core estimator (saved before Boot_Step3 shadowed it)
  result <- tryCatch({
    core_estimate_mbsu(
      data = dd,
      hidden_var = indicator,
      degree_var = config$degree_variable,
      N_F = config$frame_population,
      weights = ss_weights,
      use_inclusion_probs = TRUE,
      adjustment_factors = config$adjustment_factors,
      verbose = FALSE
    )
  }, error = function(e) {
    warning("Failed to estimate ", indicator, ": ", e$message)
    NULL
  })

  if (!is.null(result)) {
    point_estimates[[indicator]] <- result
    if (config$verbose) {
      cat("N_hat =", format(round(result$N_hat), big.mark = ","), "\n")
    }
  }
}

cat("✓ Completed", length(point_estimates), "point estimates\n")

# ==============================================================================
# STEP 1: NEIGHBORHOOD BOOTSTRAP RESAMPLING
# ==============================================================================

cat("\n=== STEP 1: Neighborhood Bootstrap Resampling ===\n")
cat("Method:", config$bootstrap_method, "\n")
cat("Number of resamples:", config$bootstrap_samples, "\n")

set.seed(config$random_seed)

boot_step1_start <- Sys.time()

boot_samples <- bootstrap_rds_sample(
  rds_sample = rd.dd,
  method = config$bootstrap_method,
  B = config$bootstrap_samples,
  degree_col = config$degree_variable,
  return_rds_df = TRUE,
  verbose = config$verbose
)

boot_step1_time <- as.numeric(difftime(Sys.time(), boot_step1_start, units = "secs"))

# Extract bootstrap samples from result
if (is.list(boot_samples) && "bootstrap_samples" %in% names(boot_samples)) {
  boot_samples_list <- boot_samples$bootstrap_samples
} else if (is.list(boot_samples)) {
  # If boot_samples is already a list of samples
  boot_samples_list <- boot_samples
} else {
  stop("Unexpected bootstrap_rds_sample() return structure")
}

cat("✓ Generated", length(boot_samples_list), "bootstrap samples\n")
cat("  Time elapsed:", round(boot_step1_time, 1), "seconds\n")

# ==============================================================================
# STEP 2: REWEIGHT BOOTSTRAP SAMPLES (SS Weights)
# ==============================================================================

cat("\n=== STEP 2: Reweighting Bootstrap Samples ===\n")
cat("Weight method: Gile's Sequential Sampling (SS)\n")

boot_step2_start <- Sys.time()

# Function to reweight a single bootstrap sample
reweight_sample <- function(sample_idx) {
  sample <- boot_samples_list[[sample_idx]]

  # Compute SS weights
  weighted_sample <- compute_rds_weights(
    resample = sample,
    weight_method = config$weight_method,
    population_size = config$frame_population,
    outcome_variable = config$nsum_indicators[1],  # Use first indicator for weight calc
    network_size_col = config$degree_variable,
    verbose = FALSE,
    validate_rds = TRUE
  )

  return(weighted_sample)
}

# Reweight all samples (with optional parallel processing)
if (config$use_parallel && config$bootstrap_samples > 100) {
  library(parallel)
  cl <- makeCluster(config$n_cores)

  # Load all required scripts and data on each worker
  clusterEvalQ(cl, {
    library(RDS)
    library(dplyr)
    library(here)
  })

  # Source Boot_Step2.R on each worker to get all helper functions
  clusterEvalQ(cl, {
    source(here::here("R", "analysis", "NSUM", "Boot_Step2.R"))
  })

  # Export required data
  clusterExport(cl, c("boot_samples_list", "config", "reweight_sample"),
                envir = environment())

  cat("Using parallel processing with", config$n_cores, "cores...\n")
  weighted_samples <- parLapply(cl, 1:config$bootstrap_samples, reweight_sample)
  stopCluster(cl)

} else {
  cat("Processing sequentially...\n")
  weighted_samples <- lapply(1:config$bootstrap_samples, function(i) {
    if (i %% 100 == 0) cat("  Reweighted", i, "/", config$bootstrap_samples, "\n")
    reweight_sample(i)
  })
}

boot_step2_time <- as.numeric(difftime(Sys.time(), boot_step2_start, units = "secs"))

cat("✓ Reweighted", length(weighted_samples), "bootstrap samples\n")
cat("  Time elapsed:", round(boot_step2_time, 1), "seconds\n")

# ==============================================================================
# STEP 3: NSUM ESTIMATION ON BOOTSTRAP SAMPLES
# ==============================================================================

cat("\n=== STEP 3: NSUM Estimation on Bootstrap Samples ===\n")

boot_step3_start <- Sys.time()

bootstrap_estimates <- list()

for (indicator in config$nsum_indicators) {

  cat("Processing:", indicator, "\n")

  # Function to estimate NSUM for one bootstrap sample
  estimate_one_sample <- function(sample_idx) {
    sample <- weighted_samples[[sample_idx]]

    result <- tryCatch({
      # Call core estimator directly with pi_column
      core_estimate_mbsu(
        data = sample,
        hidden_var = indicator,
        degree_var = config$degree_variable,
        N_F = config$frame_population,
        pi_column = "inclusion_prob",  # Read from column (Step 2 output)
        adjustment_factors = config$adjustment_factors,
        verbose = FALSE
      )
    }, error = function(e) {
      list(N_hat = NA, error = e$message)
    })

    return(result$N_hat)
  }

  # Estimate for all bootstrap samples
  if (config$use_parallel && config$bootstrap_samples > 100) {
    library(parallel)
    cl <- makeCluster(config$n_cores)

    # Load required libraries on each worker
    clusterEvalQ(cl, {
      library(tidyverse)
      library(here)
    })

    # Source ONLY core estimators (not Boot_Step3.R which shadows functions)
    clusterEvalQ(cl, {
      source(here::here("R", "analysis", "nsum_core_estimators.R"))
      # Save reference to core function
      core_estimate_mbsu <- estimate_mbsu
    })

    # Export required data
    clusterExport(cl, c("weighted_samples", "indicator", "config", "estimate_one_sample"),
                  envir = environment())

    estimates <- parLapply(cl, 1:config$bootstrap_samples, estimate_one_sample)
    stopCluster(cl)

  } else {
    estimates <- lapply(1:config$bootstrap_samples, function(i) {
      if (i %% 100 == 0) cat("  Sample", i, "/", config$bootstrap_samples, "\n")
      estimate_one_sample(i)
    })
  }

  # Convert to numeric vector
  estimates_vec <- unlist(estimates)

  # Remove NAs
  estimates_vec <- estimates_vec[!is.na(estimates_vec)]

  bootstrap_estimates[[indicator]] <- estimates_vec

  cat("  ✓ Completed:", length(estimates_vec), "valid estimates\n")
}

boot_step3_time <- as.numeric(difftime(Sys.time(), boot_step3_start, units = "secs"))

cat("✓ Completed NSUM estimation on all bootstrap samples\n")
cat("  Time elapsed:", round(boot_step3_time, 1), "seconds\n")

# ==============================================================================
# STEP 4: CALCULATE BOOTSTRAP CONFIDENCE INTERVALS
# ==============================================================================

cat("\n=== STEP 4: Bootstrap Confidence Intervals ===\n")

alpha <- 1 - config$confidence_level
ci_probs <- c(alpha/2, 1 - alpha/2)

results_table <- data.frame()

for (indicator in config$nsum_indicators) {

  # Point estimate
  point_est <- point_estimates[[indicator]]$N_hat
  point_prev <- point_estimates[[indicator]]$prevalence_rate * 100

  # Bootstrap distribution
  boot_dist <- bootstrap_estimates[[indicator]]

  # Calculate CI using percentile method
  ci <- quantile(boot_dist, probs = ci_probs, na.rm = TRUE)

  # Calculate bootstrap statistics
  boot_mean <- mean(boot_dist, na.rm = TRUE)
  boot_sd <- sd(boot_dist, na.rm = TRUE)

  # Get clean label
  clean_label <- gsub("_nsum$", "", indicator)
  clean_label <- gsub("_", " ", clean_label)
  clean_label <- tools::toTitleCase(clean_label)

  # Add to results
  results_table <- rbind(results_table, data.frame(
    Indicator = clean_label,
    Point_Estimate = point_est,
    Point_Prevalence_Pct = point_prev,
    Bootstrap_Mean = boot_mean,
    Bootstrap_SD = boot_sd,
    CI_Lower = ci[1],
    CI_Upper = ci[2],
    CI_Width = ci[2] - ci[1],
    N_Bootstrap = length(boot_dist),
    stringsAsFactors = FALSE
  ))
}

# Sort by point estimate (descending)
results_table <- results_table %>% arrange(desc(Point_Estimate))

cat("✓ Calculated bootstrap CIs for all indicators\n")

# ==============================================================================
# STEP 5: FORMAT AND DISPLAY RESULTS
# ==============================================================================

cat("\n=== STEP 5: Results Summary ===\n\n")

# Create formatted table
formatted_table <- results_table %>%
  mutate(
    # Format numbers with thousand separators
    `N (Point Est.)` = format(round(Point_Estimate), big.mark = ","),
    `Prevalence (%)` = sprintf("%.2f", Point_Prevalence_Pct),
    `95% CI Lower` = format(round(CI_Lower), big.mark = ","),
    `95% CI Upper` = format(round(CI_Upper), big.mark = ","),
    `95% CI` = paste0("[", format(round(CI_Lower), big.mark = ","),
                      ", ", format(round(CI_Upper), big.mark = ","), "]"),
    `Bootstrap Mean` = format(round(Bootstrap_Mean), big.mark = ","),
    `Bootstrap SD` = format(round(Bootstrap_SD), big.mark = ",")
  ) %>%
  select(
    Indicator,
    `N (Point Est.)`,
    `Prevalence (%)`,
    `95% CI`,
    `Bootstrap Mean`,
    `Bootstrap SD`
  )

# Print to console
cat("NSUM POPULATION SIZE ESTIMATES\n")
cat("===============================\n")
cat("Resampling: Neighborhood Bootstrap (B =", config$bootstrap_samples, ")\n")
cat("Weighting: Gile's Sequential Sampling (SS)\n")
cat("Frame Population:", format(config$frame_population, big.mark = ","), "\n")
cat("Confidence Level:", config$confidence_level * 100, "%\n\n")

print(kable(formatted_table, format = "simple", align = c("l", "r", "r", "l", "r", "r")))

# ==============================================================================
# STEP 6: SAVE RESULTS
# ==============================================================================

cat("\n\n=== Saving Results ===\n")

# Save detailed results
results_file <- file.path(config$output_dir, "nsum_complete_results.RData")
save(
  config,
  point_estimates,
  bootstrap_estimates,
  results_table,
  formatted_table,
  file = results_file
)
cat("✓ Saved RData:", results_file, "\n")

# Save formatted table (CSV)
csv_file <- file.path(config$output_dir, "nsum_results_table.csv")
write.csv(results_table, csv_file, row.names = FALSE)
cat("✓ Saved CSV:", csv_file, "\n")

# Save formatted table (pretty text)
txt_file <- file.path(config$output_dir, "nsum_results_table.txt")
sink(txt_file)
cat("NSUM POPULATION SIZE ESTIMATES\n")
cat("===============================\n")
cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Resampling: Neighborhood Bootstrap (B =", config$bootstrap_samples, ")\n")
cat("Weighting: Gile's Sequential Sampling (SS)\n")
cat("Frame Population:", format(config$frame_population, big.mark = ","), "\n")
cat("Confidence Level:", config$confidence_level * 100, "%\n\n")
print(kable(formatted_table, format = "simple", align = c("l", "r", "r", "l", "r", "r")))
cat("\n\nTotal runtime:", round(boot_step1_time + boot_step2_time + boot_step3_time, 1), "seconds\n")
sink()
cat("✓ Saved formatted table:", txt_file, "\n")

# Optional: Save bootstrap samples for further analysis
if (config$save_bootstrap_samples) {
  samples_file <- file.path(config$output_dir, "bootstrap_samples.RData")
  save(boot_samples, weighted_samples, file = samples_file)
  cat("✓ Saved bootstrap samples:", samples_file, "\n")
}

# ==============================================================================
# STEP 7: DIAGNOSTIC PLOTS (Optional)
# ==============================================================================

cat("\n=== Creating Diagnostic Plots ===\n")

# Bootstrap distribution plots
pdf(file.path(config$output_dir, "bootstrap_distributions.pdf"), width = 12, height = 8)

par(mfrow = c(2, 3))
for (indicator in config$nsum_indicators) {
  boot_dist <- bootstrap_estimates[[indicator]]
  point_est <- point_estimates[[indicator]]$N_hat
  ci <- quantile(boot_dist, probs = ci_probs, na.rm = TRUE)

  clean_label <- gsub("_nsum$", "", indicator)
  clean_label <- gsub("_", " ", clean_label)
  clean_label <- tools::toTitleCase(clean_label)

  hist(boot_dist,
       breaks = 50,
       main = clean_label,
       xlab = "Population Size Estimate",
       col = "lightblue",
       border = "white")

  abline(v = point_est, col = "red", lwd = 2, lty = 1)
  abline(v = ci[1], col = "darkgreen", lwd = 2, lty = 2)
  abline(v = ci[2], col = "darkgreen", lwd = 2, lty = 2)

  legend("topright",
         legend = c("Point Estimate", "95% CI"),
         col = c("red", "darkgreen"),
         lty = c(1, 2),
         lwd = 2,
         cex = 0.8)
}

dev.off()
cat("✓ Saved diagnostic plots: bootstrap_distributions.pdf\n")

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("\n")
cat("=================================================================\n")
cat("                    WORKFLOW COMPLETED                           \n")
cat("=================================================================\n")
cat("Total processing time:", round(boot_step1_time + boot_step2_time + boot_step3_time, 1), "seconds\n")
cat("Output directory:", config$output_dir, "\n")
cat("\nFiles created:\n")
cat("  - nsum_complete_results.RData (detailed results)\n")
cat("  - nsum_results_table.csv (formatted table)\n")
cat("  - nsum_results_table.txt (pretty text table)\n")
cat("  - bootstrap_distributions.pdf (diagnostic plots)\n")
cat("=================================================================\n\n")

# Return results invisibly
invisible(list(
  config = config,
  point_estimates = point_estimates,
  bootstrap_estimates = bootstrap_estimates,
  results_table = results_table,
  formatted_table = formatted_table
))

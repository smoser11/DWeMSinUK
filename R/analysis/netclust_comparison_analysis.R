# =============================================================================
# netclust Comparison Analysis: Fixing the Prevalence Estimation Issue
# =============================================================================

library(tidyverse)
library(here)

# Load both RDS and netclust results
load(here("output", "netclust_subgroup_analysis_results_FIXED.RData"))
rds_results <- read.csv(here("output", "tables", "rds_main_results.csv"))

# =============================================================================
# SOLUTION 1: Extract Correct Prevalence from netclust
# =============================================================================

# netclust estimates population SIZE, not prevalence directly
# To get prevalence comparable to RDS-SS, we need to calculate:
# Prevalence = (number with trait in sample) / (estimated reachable population)

create_comparable_estimates <- function() {

  netclust_prevalence <- data.frame()

  for(i in 1:length(final_results$individual_results)) {
    result <- final_results$individual_results[[i]]
    indicator <- result$indicator

    # Get netclust population estimate
    mcmc_samples <- result$netclust_output$sample
    N_estimates <- mcmc_samples[, "N"]

    # Number with trait in sample
    n_trait <- result$n_trait
    n_total <- result$n_obs

    # Calculate prevalence within the REACHABLE population
    # This is what we should compare to RDS-SS
    reachable_pop_median <- median(N_estimates)
    sample_prevalence <- (n_trait / n_total) * 100  # Sample proportion

    # The issue: netclust assumes tiny reachable population
    # So prevalence calculation becomes meaningless

    netclust_prevalence <- rbind(netclust_prevalence, data.frame(
      indicator = indicator,
      sample_prevalence = sample_prevalence,
      netclust_reachable_pop = reachable_pop_median,
      n_trait = n_trait,
      n_total = n_total,
      # This is the problematic calculation:
      netclust_prevalence_estimate = (n_trait / reachable_pop_median) * 100
    ))
  }

  return(netclust_prevalence)
}

# =============================================================================
# SOLUTION 2: Use netclust for Population Size, RDS-SS for Prevalence
# =============================================================================

create_hybrid_estimates <- function() {

  # Get RDS-SS prevalence estimates
  rds_clean <- rds_results %>%
    select(indicator, estimate_pct) %>%
    rename(rds_prevalence = estimate_pct)

  # Get netclust population size estimates
  netclust_pop_sizes <- data.frame()

  for(i in 1:length(final_results$individual_results)) {
    result <- final_results$individual_results[[i]]
    indicator <- result$indicator

    mcmc_samples <- result$netclust_output$sample
    N_estimates <- mcmc_samples[, "N"]

    netclust_pop_sizes <- rbind(netclust_pop_sizes, data.frame(
      indicator = indicator,
      netclust_population_estimate = median(N_estimates),
      netclust_pop_ci_lower = quantile(N_estimates, 0.025),
      netclust_pop_ci_upper = quantile(N_estimates, 0.975)
    ))
  }

  # Combine: RDS prevalence + netclust population size
  hybrid_results <- merge(rds_clean, netclust_pop_sizes, by = "indicator") %>%
    mutate(
      # Number of people affected
      affected_count = (rds_prevalence / 100) * netclust_population_estimate,
      affected_count_lower = (rds_prevalence / 100) * netclust_pop_ci_lower,
      affected_count_upper = (rds_prevalence / 100) * netclust_pop_ci_upper
    )

  return(hybrid_results)
}

# =============================================================================
# SOLUTION 3: Force netclust to Use 980,000 Population
# =============================================================================

# This requires re-running netclust with much stronger priors
# The current prior allows population to be as small as 5,000
# We need to force it to 980,000

create_fixed_netclust_config <- function() {

  cat("=== CONFIGURATION FOR FIXED NETCLUST ===\n")
  cat("Problem: Current netclust estimates ~15,000 reachable population\n")
  cat("Solution: Force population to be closer to 980,000\n\n")

  # Calculate required Beta prior parameters
  n_sample <- 85
  N_target <- 980000
  target_proportion <- n_sample / N_target

  cat("Target sample proportion:", target_proportion, "\n")
  cat("This is extremely small, suggesting one of:\n")
  cat("1. The 980,000 estimate is too large\n")
  cat("2. Your RDS network only reaches a small fraction\n")
  cat("3. netclust may not be the right method for this scale\n\n")

  # To force Beta prior around target proportion:
  # If we want mean = target_proportion with reasonable variance
  # Beta(a, b) has mean = a/(a+b)

  # For very small proportions, need careful parameterization
  # Option: Use very concentrated prior

  a_param <- 1
  b_param <- round(a_param * (1 - target_proportion) / target_proportion)

  cat("Suggested Beta prior parameters:\n")
  cat("a =", a_param, ", b =", b_param, "\n")
  cat("This gives prior mean =", a_param/(a_param + b_param), "\n")
  cat("But this is still not practical for such extreme proportions\n\n")

  cat("RECOMMENDATION: \n")
  cat("netclust is telling you that only ~15,000 domestic workers\n")
  cat("are reachable through your RDS network. This may be correct!\n")
  cat("Consider using netclust estimates as-is, or use hybrid approach.\n")

}

# =============================================================================
# RUN ANALYSIS
# =============================================================================

cat("=== NETCLUST COMPARISON ANALYSIS ===\n\n")

# Solution 1: Show the prevalence calculation issue
cat("SOLUTION 1: Comparable Prevalence Estimates\n")
comparable_estimates <- create_comparable_estimates()
print(comparable_estimates)

cat("\n" %R% "==" %R% "\n")

# Solution 2: Hybrid approach
cat("SOLUTION 2: Hybrid Estimates (RDS prevalence + netclust population)\n")
hybrid_estimates <- create_hybrid_estimates()
print(hybrid_estimates)

cat("\n" %R% "==" %R% "\n")

# Solution 3: Configuration advice
create_fixed_netclust_config()

# Save results
save(comparable_estimates, hybrid_estimates,
     file = here("output", "netclust_comparison_analysis.RData"))

cat("\nResults saved to: output/netclust_comparison_analysis.RData\n")
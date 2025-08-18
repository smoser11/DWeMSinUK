# 03-sequential-sampling.R
library(RDS)
library(sspse)
library(tidyverse)

#' Combined Sequential Sampling Analysis
#' 
#' This function performs both:
#' 1. Gile's SS estimation for trait estimation
#' 2. Bayesian SS-PSE for population size estimation
#' 
run_ss_analysis <- function(data,
                            outcome_vars = c("zQ36", "zQ80", "composite_risk"),
                            pop_sizes = c(50000, 100000, 980000, 1740000),
                            priors = c("beta", "flat", "nbinom", "pln"),
                            verbose = TRUE) {
  
  # Part 1: Population Size Estimation using Bayesian SS-PSE
  pop_size_estimates <- list()
  
  for(prior in priors) {
    if(verbose) cat(sprintf("\nEstimating population size using %s prior...\n", prior))
    
    ss_pse <- tryCatch({
      posteriorsize(
        data,
        mean.prior.size = 980000,  # Prior mean
        maxN = 2000000,           # Maximum possible size
        visibility = TRUE,         # Use visibility adjustment
        priorsizedistribution = prior,
        parallel = 5              # Use 5 cores
      )
    }, error = function(e) {
      if(verbose) cat(sprintf("Error in posteriorsize: %s\n", e$message))
      NULL
    })
    
    if(!is.null(ss_pse)) {
      pop_size_estimates[[prior]] <- tibble(
        prior = prior,
        mean = mean(ss_pse$sample[,"N"]),
        median = median(ss_pse$sample[,"N"]),
        sd = sd(ss_pse$sample[,"N"]),
        q025 = quantile(ss_pse$sample[,"N"], 0.025),
        q975 = quantile(ss_pse$sample[,"N"], 0.975)
      )
    }
  }
  
  # Part 2: Trait Estimation using Gile's SS
  trait_estimates <- list()
  counter <- 1
  
  # Generate combinations
  combinations <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars
  )
  
  for(i in 1:nrow(combinations)) {
    N_val <- combinations$N[i]
    outcome_var <- combinations$outcome[i]
    
    if(verbose) {
      cat(sprintf("\nGile's SS Estimation:\n"))
      cat(sprintf("N = %d, outcome = %s\n", N_val, outcome_var))
    }
    
    # Run SS estimation
    ss_result <- tryCatch({
      RDS.SS.estimates(
        data,
        outcome.variable = outcome_var,
        N = N_val,
        number.ss.samples.per.iteration = 500,
        number.ss.iterations = 5
      )
    }, error = function(e) {
      if(verbose) cat(sprintf("Error: %s\n", e$message))
      NULL
    })
    
    if(!is.null(ss_result)) {
      trait_estimates[[counter]] <- tibble(
        N = N_val,
        outcome = outcome_var,
        estimate = ss_result$estimate,
        se = ss_result$se,
        ci_lower = ss_result$estimate - 1.96 * ss_result$se,
        ci_upper = ss_result$estimate + 1.96 * ss_result$se
      )
    }
    
    counter <- counter + 1
  }
  
  # Combine and return results
  results <- list(
    population_size = bind_rows(pop_size_estimates),
    trait_estimates = bind_rows(trait_estimates)
  )
  
  # Print summary if verbose
  if(verbose) {
    cat("\nPopulation Size Estimates:\n")
    print(results$population_size)
    
    cat("\nTrait Estimates:\n")
    print(
      results$trait_estimates %>%
        group_by(outcome) %>%
        summarise(
          n_estimates = n(),
          mean_est = mean(estimate),
          mean_se = mean(se)
        )
    )
  }
  
  return(results)
}

#' Plot Population Size Posterior Distributions
#' 
plot_population_size <- function(results) {
  results$population_size %>%
    ggplot(aes(x = prior)) +
    geom_pointrange(aes(y = median, 
                        ymin = q025, 
                        ymax = q975)) +
    theme_minimal() +
    labs(x = "Prior Distribution",
         y = "Population Size Estimate") +
    coord_flip()
}

#' Plot Trait Estimates Across Population Sizes
#' 
plot_trait_estimates <- function(results) {
  results$trait_estimates %>%
    ggplot(aes(x = factor(N), y = estimate)) +
    geom_pointrange(aes(ymin = ci_lower, 
                        ymax = ci_upper)) +
    facet_wrap(~outcome, scales = "free_y") +
    theme_minimal() +
    labs(x = "Population Size",
         y = "Estimate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example usage:
results <- run_ss_analysis(rd.dd, verbose = TRUE)
plot_population_size(results)
plot_trait_estimates(results)

save.image("./output/ss.RData")

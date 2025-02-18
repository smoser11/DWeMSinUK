# 02-model-assisted.R
library(RDS)
library(tidyverse)
library(parallel)

# run_ma_estimates <- function(data,
#                              outcome_vars = c("zQ36", "zQ80", "composite_risk"),
#                              pop_sizes = c(50000, 100000, 980000, 1740000),
#                              seed_methods = c("degree", "random", "sample"),
#                              n_cores = 5,
#                              verbose = TRUE) {
  

  run_ma_estimates <- function(data,
                               outcome_vars = c("zQ36", "composite_risk"),
                               pop_sizes = c(980000),
                               seed_methods = c("degree", "random", "sample"),
                               n_cores = 5,
                               verbose = TRUE) {
    
  
  
    if(verbose) cat(sprintf("Using %d cores for parallel processing\n", n_cores))
  
  # Initialize results storage
  results_list <- list()
  counter <- 1
  
  # Record warnings
  warning_log <- list()
  
  # Generate all combinations
  combinations <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars,
    seed_method = seed_methods
  )
  
  for(i in 1:nrow(combinations)) {
    N_val <- combinations$N[i]
    outcome_var <- combinations$outcome[i]
    seed_method <- combinations$seed_method[i]
    
    if(verbose) {
      cat(sprintf("\nModel Assisted Estimation:\n"))
      cat(sprintf("N = %d, outcome = %s, seed method = %s\n", 
                  N_val, outcome_var, seed_method))
    }
    
    # Count missing values before estimation
    missing_outcome <- sum(is.na(data[[outcome_var]]))
    missing_network <- sum(is.na(data$network.size.variable) | 
                             data$network.size.variable == 0)
    
    if(verbose && (missing_outcome > 0 || missing_network > 0)) {
      cat(sprintf("Missing values before estimation:\n"))
      cat(sprintf("Outcome: %d missing\n", missing_outcome))
      cat(sprintf("Network size: %d missing/zero\n", missing_network))
    }
    
    tryCatch({
      # Capture warnings
      withCallingHandlers({
        # Run MA estimation
        result <- MA.estimates(
          rds.data = data,
          trait.variable = outcome_var,
          seed.selection = seed_method,
          N = N_val,
          M1 = 25,          # Number of networked populations
          M2 = 20,          # Number of RDS samples per network
          number.of.iterations = 3,
          parallel = n_cores,
          parallel.type = "PSOCK",
          verbose = FALSE
        )
        
        # Extract all components
        results_list[[counter]] <- tibble(
          N = N_val,
          outcome = outcome_var,
          seed_method = seed_method,
          estimate = result$estimate,
          standard_error = if(!is.null(result$se)) result$se else NA_real_,
          ci_lower = if(!is.null(result$ci)) result$ci[1] else NA_real_,
          ci_upper = if(!is.null(result$ci)) result$ci[2] else NA_real_,
          missing_outcome = missing_outcome,
          missing_network = missing_network,
          success = TRUE,
          error = NA_character_
        )
        
      }, warning = function(w) {
        warning_log[[length(warning_log) + 1]] <<- 
          sprintf("N=%d, %s, %s: %s", 
                  N_val, outcome_var, seed_method, w$message)
        invokeRestart("muffleWarning")
      })
      
    }, error = function(e) {
      results_list[[counter]] <<- tibble(
        N = N_val,
        outcome = outcome_var,
        seed_method = seed_method,
        estimate = NA_real_,
        standard_error = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        missing_outcome = missing_outcome,
        missing_network = missing_network,
        success = FALSE,
        error = as.character(e)
      )
      
      if(verbose) cat(sprintf("Error: %s\n", e$message))
    })
    
    counter <- counter + 1
  }
  
  # Combine results
  results_df <- bind_rows(results_list)
  
  if(verbose) {
    cat("\nSummary:\n")
    cat(sprintf("Successful estimations: %d of %d\n", 
                sum(results_df$success), nrow(results_df)))
    
    # Print summary without using se
    print(
      results_df %>%
        group_by(seed_method, outcome) %>%
        summarise(
          n_success = sum(success),
          mean_est = mean(estimate, na.rm = TRUE),
          mean_missing = mean(missing_outcome),
          .groups = "drop"
        )
    )
    
    cat("\nWarnings encountered:\n")
    print(unique(warning_log))
  }
  
  attr(results_df, "warnings") <- warning_log
  return(results_df)
}

# Function to plot results
plot_ma_results <- function(results) {
  results %>%
    filter(success) %>%
    ggplot(aes(x = factor(N), y = estimate, color = seed_method)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.2),
                  width = 0.2) +
    facet_wrap(~outcome, scales = "free_y") +
    theme_minimal() +
    labs(x = "Population Size",
         y = "Estimate",
         color = "Seed Selection Method") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

ma_results <- run_ma_estimates(rd.dd, n_cores = 5, verbose = TRUE)
# Usage example:
View(ma_results)
plot_ma_results(ma_results)
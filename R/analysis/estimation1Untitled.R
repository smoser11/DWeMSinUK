library(RDS)
library(sspse)
library(tidyverse)

run_rds_estimates <- function(data, 
                              pop_sizes = c(50000, 100000, 980000, 1740000),
                              outcome_vars = c("zQ36", "zQ80", "composite_risk"),
                              verbose = TRUE) {
  
  # Pre-process data to handle network sizes
  data_clean <- data %>%
    mutate(
      network.size.variable = case_when(
        is.na(network.size.variable) ~ median(network.size.variable, na.rm = TRUE),
        network.size.variable == 0 ~ median(network.size.variable, na.rm = TRUE),
        TRUE ~ network.size.variable
      )
    )
  
  # Initialize results storage
  results_list <- list()
  counter <- 1
  
  # Generate all combinations
  combinations <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars,
    estimator = c("RDS-I", "RDS-II", "Gile's SS")
  )
  
  # Loop through combinations
  for(i in 1:nrow(combinations)) {
    N_val <- combinations$N[i]
    outcome_var <- combinations$outcome[i]
    weight_type <- combinations$estimator[i]
    
    if(verbose) {
      cat(sprintf("\nEstimating: N=%d, outcome=%s, estimator=%s\n", 
                  N_val, outcome_var, weight_type))
    }
    
    tryCatch({
      # Attempt estimation
      result <- RDS.bootstrap.intervals(
        rds.data = data_clean,
        outcome.variable = outcome_var,
        weight.type = weight_type,
        N = N_val,
        number.of.bootstrap.samples = 1000,
        confidence.level = 0.95,
        fast = TRUE,
        control = control.rds.estimates(
          seed = 12345,
          remove.missing = TRUE,
          impute.network.size = TRUE
        )
      )
      
      # Store successful result
      results_list[[counter]] <- list(
        N = N_val,
        outcome = outcome_var,
        estimator = weight_type,
        estimate = result$estimate,
        se = attr(result, "standard.error"),
        ci_lower = attr(result, "confidence.interval")[1],
        ci_upper = attr(result, "confidence.interval")[2],
        success = TRUE,
        error = NA_character_
      )
      
    }, error = function(e) {
      # Store failed attempt
      results_list[[counter]] <<- list(
        N = N_val,
        outcome = outcome_var,
        estimator = weight_type,
        estimate = NA_real_,
        se = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        success = FALSE,
        error = as.character(e)
      )
      
      if(verbose) cat(sprintf("Error: %s\n", e$message))
    })
    
    counter <- counter + 1
  }
  
  # Convert results to tibble
  results_df <- bind_rows(results_list)
  
  if(verbose) {
    cat("\nSummary:\n")
    cat(sprintf("Successful estimations: %d of %d\n", 
                sum(results_df$success), nrow(results_df)))
    cat(sprintf("Failed estimations: %d of %d\n", 
                sum(!results_df$success), nrow(results_df)))
  }
  
  return(results_df)
}

# Function to create summary tables
create_summary_tables <- function(results) {
  # Summary by estimator
  estimator_summary <- results %>%
    group_by(estimator) %>%
    summarise(
      n_total = n(),
      n_success = sum(success),
      success_rate = mean(success),
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE)
    )
  
  # Summary by outcome
  outcome_summary <- results %>%
    group_by(outcome) %>%
    summarise(
      n_total = n(),
      n_success = sum(success),
      success_rate = mean(success),
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE)
    )
  
  # Summary by population size
  size_summary <- results %>%
    group_by(N) %>%
    summarise(
      n_total = n(),
      n_success = sum(success),
      success_rate = mean(success),
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE)
    )
  
  list(
    by_estimator = estimator_summary,
    by_outcome = outcome_summary,
    by_size = size_summary,
    full_results = results
  )
}

# Example usage:
results <- run_rds_estimates(rd.dd, verbose = TRUE)
# summary_tables <- create_summary_tables(results)
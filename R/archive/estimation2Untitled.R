library(RDS)
library(sspse)
library(tidyverse)

prepare_rds_data <- function(data, outcome_var) {
  data_clean <- data
  
  # Standardize recruiter.id
  if("recruiter_id" %in% names(data_clean)) {
    data_clean$recruiter.id <- data_clean$recruiter_id
    data_clean$recruiter_id <- NULL
  }
  
  # Find initial seeds
  seeds <- is.na(data_clean$recruiter.id) | 
    data_clean$recruiter.id == -1 |
    !(data_clean$recruiter.id %in% data_clean$id)
  data_clean$recruiter.id[seeds] <- 0
  
  # Handle network sizes
  data_clean$network.size.variable <- ifelse(
    is.na(data_clean$network.size.variable) | 
      data_clean$network.size.variable == 0,
    median(data_clean$network.size.variable[
      data_clean$network.size.variable > 0], 
      na.rm = TRUE),
    data_clean$network.size.variable
  )
  
  # Find complete recruitment chains
  missing_outcome <- is.na(data_clean[[outcome_var]])
  
  if(any(missing_outcome)) {
    # Get IDs of complete cases
    complete_ids <- data_clean$id[!missing_outcome]
    
    # Include recruiters of complete cases to preserve chains
    keep_ids <- unique(c(
      complete_ids,
      data_clean$recruiter.id[data_clean$id %in% complete_ids]
    ))
    
    # Keep only complete chains
    data_clean <- data_clean[data_clean$id %in% keep_ids, ]
    
    # Ensure seeds are marked correctly in reduced dataset
    seeds_new <- is.na(data_clean$recruiter.id) | 
      data_clean$recruiter.id == -1 |
      !(data_clean$recruiter.id %in% data_clean$id)
    data_clean$recruiter.id[seeds_new] <- 0
  }
  
  return(data_clean)
}

run_rds_estimates <- function(data, 
                              pop_sizes = c(50000, 100000, 980000, 1740000),
                              outcome_vars = c("zQ36", "zQ80", "composite_risk"),
                              verbose = TRUE) {
  
  # Initialize results storage
  results_list <- list()
  counter <- 1
  
  # Generate all combinations
  combinations <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars,
    estimator = c("RDS-I", "RDS-II", "Gile's SS")
  )
  
  for(i in 1:nrow(combinations)) {
    N_val <- combinations$N[i]
    outcome_var <- combinations$outcome[i]
    weight_type <- combinations$estimator[i]
    
    if(verbose) {
      cat(sprintf("\nEstimating: N=%d, outcome=%s, estimator=%s\n", 
                  N_val, outcome_var, weight_type))
    }
    
    # Prepare data for this outcome
    data_outcome <- prepare_rds_data(data, outcome_var)
    
    if(verbose) {
      cat(sprintf("Sample size: %d, Seeds: %d\n",
                  nrow(data_outcome), 
                  sum(data_outcome$recruiter.id == 0)))
    }
    
    tryCatch({
      # Run bootstrap estimation
      result <- RDS.bootstrap.intervals(
        rds.data = data_outcome,
        outcome.variable = outcome_var,
        weight.type = weight_type,
        N = N_val,
        number.of.bootstrap.samples = 1000,
        confidence.level = 0.95,
        fast = TRUE
      )
      
      # Store results
      results_list[[counter]] <- tibble(
        N = N_val,
        outcome = outcome_var,
        estimator = weight_type,
        estimate = result$estimate,
        se = attr(result, "standard.error"),
        ci_lower = attr(result, "confidence.interval")[1],
        ci_upper = attr(result, "confidence.interval")[2],
        sample_size = nrow(data_outcome),
        seeds = sum(data_outcome$recruiter.id == 0),
        success = TRUE,
        error = NA_character_
      )
      
    }, error = function(e) {
      results_list[[counter]] <<- tibble(
        N = N_val,
        outcome = outcome_var,
        estimator = weight_type,
        estimate = NA_real_,
        se = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        sample_size = nrow(data_outcome),
        seeds = sum(data_outcome$recruiter.id == 0),
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
    for(est in unique(results_df$estimator)) {
      cat(sprintf("%s success rate: %.1f%%\n", 
                  est, 
                  100*mean(results_df$success[results_df$estimator == est])))
    }
    
    if(sum(results_df$success) > 0) {
      cat("\nEstimates summary for successful estimations:\n")
      print(
        results_df %>%
          filter(success) %>%
          group_by(estimator, outcome) %>%
          summarise(
            mean_est = mean(estimate, na.rm = TRUE),
            mean_se = mean(se, na.rm = TRUE),
            mean_sample = mean(sample_size),
            mean_seeds = mean(seeds),
            n = n(),
            .groups = "drop"
          )
      )
    }
  }
  
  return(results_df)
}


results <- run_rds_estimates(rd.dd, verbose = TRUE)

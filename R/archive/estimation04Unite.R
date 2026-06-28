# 01-traditional-rds.R
library(RDS)
library(tidyverse)

run_traditional_rds <- function(data,
                                outcome_vars = c("zQ36", "zQ80", "composite_risk"),
                                pop_sizes = c(50000, 100000, 980000, 1740000),
                                verbose = TRUE) {
  
  # Prepare data - ensure seeds have recruiter.id = 0
  data_clean <- data
  seeds <- is.na(data_clean$recruiter.id) | 
    data_clean$recruiter.id == -1 |
    !(data_clean$recruiter.id %in% data_clean$id)
  data_clean$recruiter.id[seeds] <- 0
  
  if(verbose) {
    cat("Data preparation:\n")
    cat(sprintf("Total sample size: %d\n", nrow(data_clean)))
    cat(sprintf("Number of seeds: %d\n\n", sum(data_clean$recruiter.id == 0)))
  }
  
  # Initialize results storage
  results <- list()
  
  # Loop through outcomes and population sizes
  for(outcome in outcome_vars) {
    if(verbose) cat(sprintf("\nAnalyzing outcome: %s\n", outcome))
    
    # Remove NAs for this outcome
    data_outcome <- data_clean[!is.na(data_clean[[outcome]]), ]
    
    for(N in pop_sizes) {
      if(verbose) cat(sprintf("Population size N = %d\n", N))
      
      # RDS-I with bootstrap intervals
      rds1 <- tryCatch({
        RDS.bootstrap.intervals(
          data_outcome,
          outcome.variable = outcome,
          weight.type = "RDS-I",
          N = N,
          number.of.bootstrap.samples = 1000,
          confidence.level = 0.95
        )
      }, error = function(e) {
        if(verbose) cat(sprintf("RDS-I Error: %s\n", e$message))
        NULL
      })
      
      # RDS-II with bootstrap intervals
      rds2 <- tryCatch({
        RDS.bootstrap.intervals(
          data_outcome,
          outcome.variable = outcome,
          weight.type = "RDS-II",
          N = N,
          number.of.bootstrap.samples = 1000,
          confidence.level = 0.95
        )
      }, error = function(e) {
        if(verbose) cat(sprintf("RDS-II Error: %s\n", e$message))
        NULL
      })
      
      # Store results
      results[[paste(outcome, N)]] <- list(
        outcome = outcome,
        N = N,
        sample_size = nrow(data_outcome),
        seeds = sum(data_outcome$recruiter.id == 0),
        RDS_I = rds1,
        RDS_II = rds2
      )
    }
  }
  
  # Format results into a data frame
  results_df <- map_dfr(results, ~{
    tibble(
      outcome = .x$outcome,
      N = .x$N,
      sample_size = .x$sample_size,
      seeds = .x$seeds,
      RDS_I_estimate = if(!is.null(.x$RDS_I)) .x$RDS_I$estimate else NA_real_,
      RDS_I_se = if(!is.null(.x$RDS_I)) attr(.x$RDS_I, "standard.error") else NA_real_,
      RDS_I_ci_lower = if(!is.null(.x$RDS_I)) attr(.x$RDS_I, "confidence.interval")[1] else NA_real_,
      RDS_I_ci_upper = if(!is.null(.x$RDS_I)) attr(.x$RDS_I, "confidence.interval")[2] else NA_real_,
      RDS_II_estimate = if(!is.null(.x$RDS_II)) .x$RDS_II$estimate else NA_real_,
      RDS_II_se = if(!is.null(.x$RDS_II)) attr(.x$RDS_II, "standard.error") else NA_real_,
      RDS_II_ci_lower = if(!is.null(.x$RDS_II)) attr(.x$RDS_II, "confidence.interval")[1] else NA_real_,
      RDS_II_ci_upper = if(!is.null(.x$RDS_II)) attr(.x$RDS_II, "confidence.interval")[2] else NA_real_
    )
  })
  
  if(verbose) {
    cat("\nResults Summary:\n")
    print(
      results_df %>%
        group_by(outcome) %>%
        summarise(
          n_estimates = n(),
          mean_RDS_I = mean(RDS_I_estimate, na.rm = TRUE),
          mean_RDS_II = mean(RDS_II_estimate, na.rm = TRUE)
        )
    )
  }
  
  return(results_df)
}

# Example usage:
results <- run_traditional_rds(rd.dd, verbose = TRUE)
View(results)
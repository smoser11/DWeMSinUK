library(RDS)
library(sspse)
library(tidyverse)
library(coda)
library(parallel)
library(doParallel)

data <-rd.dd
pop_sizes = c(50000, 100000, 980000, 1740000)
seed_methods = c("sample", "random", "degree")
outcome_vars = c("zQ36", "zQ80", "composite_risk") 
priors = c("beta", "flat", "nbinom", "pln")

run_rds_analysis <- function(data, 
                             pop_sizes = c(50000, 100000, 980000, 1740000),
                             seed_methods = c("sample", "random", "degree"),
                             outcome_vars = c("zQ36", "zQ80", "composite_risk"), 
                             priors = c("beta", "flat", "nbinom", "pln")) {
  
  # Setup parallel processing
  n_cores <- min(5, detectCores())
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # 1. Standard RDS Estimators
  rds_estimates <- expand_grid(
    N = pop_sizes,
    outcome = outcome_vars
  ) %>%
    group_by(N, outcome) %>%
    group_modify(~{
      outcome_var <- .y$outcome
      N_val <- .y$N
      
      tibble(
        estimator = c("RDS-I", "RDS-II", "RDS-SS"),
        estimate = list(
          RDS.bootstrap.intervals(data, outcome_var, 
                                  weight.type = "RDS-I",
                                  N = N_val,
                                  number.of.bootstrap.samples = 1000,
                                  confidence.level = 0.95,
                                  fast = TRUE) #,
          # RDS.bootstrap.intervals(data, outcome_var,
          #                         weight.type = "RDS-II", 
          #                         N = N_val,
          #                         number.of.bootstrap.samples = 1000,
          #                         confidence.level = 0.95,
          #                         fast = TRUE)
          # # RDS.bootstrap.intervals(data, outcome_var,
          #                         weight.type = "Gile's SS",
          #                         N = N_val, 
          #                         number.of.bootstrap.samples = 1000,
          #                         confidence.level = 0.95,
          #                         fast = TRUE)
        )
      )
    })
  
  
  
  
  
  
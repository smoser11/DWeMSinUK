# Add to 03-rds_estimation.R
library(RDS)
library(sspse)
library(tidyverse)

run_bayesian_rds <- function() {
	load("data/processed/prepared_data.RData")
	
	# 1. Bayesian SS-PSE for population size
	bayes_size <- posteriorsize(rd.dd, 
								mean.prior.size = 980000,
								maxN = 2000000,
								visibility = TRUE,
								K = FALSE,
								priorsizedistribution = "beta")
	
	# 2. Bayesian estimation of composite risk distribution
	bayes_trait <- posteriormean(rd.dd,
								 trait.variable = "composite_risk",
								 mean.prior.size = 980000,
								 maxN = 2000000,
								 B = 1000)  # MCMC iterations
	
	# 3. Examine posterior distributions
	posterior_summaries <- list(
		size = summary(bayes_size, HPD.level = 0.95),
		trait = summary(bayes_trait, HPD.level = 0.95)
	)
	
	# Save results
	save(bayes_size, bayes_trait, posterior_summaries,
		 file = "output/bayesian_results.RData")
	
	return(list(
		size = bayes_size,
		trait = bayes_trait,
		summaries = posterior_summaries
	))
}

bayesian_results <- run_bayesian_rds()

save(bayesian_results, 
     file = "output/rds_estimates-Bayes.RData")
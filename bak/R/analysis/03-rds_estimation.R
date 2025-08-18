# 03-rds_estimation.R
library(RDS)
library(sspse)
library(tidyverse)

run_rds_estimates <- function() {
	load("data/processed/prepared_data.RData")
	
	# 1. Population Size Estimates with Different N
	pop_sizes <- c(50000, 100000, 980000, 1740000)
	
	# Store results for different estimators and population sizes
	pop_estimates <- list()
	
	for(N in pop_sizes) {
		pop_estimates[[as.character(N)]] <- list(
			# RDS-I Estimates
			RDS_I_q36 = RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=N),
			RDS_I_q80 = RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=N),
			
			# RDS-II Estimates
			RDS_II_q36 = RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=N),
			RDS_II_q80 = RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=N),
			
			# Successive Sampling Estimates
			RDS_SS_q36 = RDS.SS.estimates(rd.dd, outcome.variable = "zQ36", N=N),
			RDS_SS_q80 = RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=N)
		)
	}
	
	# 2. Population Size Estimation using SS-PSE
	ss_pse <- posteriorsize(rd.dd, 
							mean.prior.size = 980000,
							maxN = 2000000,
							visibility = TRUE, 
							K = FALSE)
	
	# 3. Model Assisted Estimates
	ma_estimates <- list(
		MA_q36 = MA.estimates(rd.dd, trait.variable = "zQ36", 
							  N = 980000, parallel = 4, 
							  seed.selection = "degree"),
		MA_q80 = MA.estimates(rd.dd, trait.variable = "zQ80", 
							  N = 980000, parallel = 4, 
							  seed.selection = "degree")
	)
	
	# 4. Compute convergence diagnostics
	convergence_diag <- list(
		q36_plot = convergence.plot(rd.dd, "zQ36"),
		q80_plot = convergence.plot(rd.dd, "zQ80")
	)
	
	# Save all results
	results <- list(
		pop_estimates = pop_estimates,
		ss_pse = ss_pse,
		ma_estimates = ma_estimates,
		convergence = convergence_diag
	)
	
	save(results, file = "output/rds_estimates.RData")
	return(results)
}

# Run analysis
rds_results <- run_rds_estimates()



#### BAYES:


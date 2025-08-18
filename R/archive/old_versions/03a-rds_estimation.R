# 03-rds_estimation.R
library(RDS)
library(sspse)
library(tidyverse)
library(coda)

run_rds_analyses <- function() {
	load("data/processed/prepared_data.RData")
	
	# Population sizes to test
	pop_sizes <- c(10000, 100000, 953000, 1000000, 1500000, 1740000)
	
	# Run analyses by seed selection method
	seed_methods <- c("sample", "random", "degree")
	ma_results <- list()
	
	for(method in seed_methods) {
		for(N in pop_sizes) {
			ma_results[[paste0(method, "_", N)]] <- list(
				q36 = MA.estimates(rd.dd, trait.variable = "zQ36", 
								   N = N, parallel = 4, 
								   seed.selection = method),
				q80 = MA.estimates(rd.dd, trait.variable = "zQ80", 
								   N = N, parallel = 4, 
								   seed.selection = method),
				composite_risk = MA.estimates(rd.dd, trait.variable = "composite_risk",
											  N = N, parallel = 4,
											  seed.selection = method)
			)
		}
	}
	
	# Standard RDS estimators
	rds_results <- list()
	for(N in pop_sizes) {
		rds_results[[as.character(N)]] <- list(
			rds_i = list(
				q36 = RDS.I.estimates(rd.dd, "zQ36", N = N),
				q80 = RDS.I.estimates(rd.dd, "zQ80", N = N)
			),
			rds_ii = list(
				q36 = RDS.II.estimates(rd.dd, "zQ36", N = N),
				q80 = RDS.II.estimates(rd.dd, "zQ80", N = N)
			),
			rds_ss = list(
				q36 = RDS.SS.estimates(rd.dd, "zQ36", N = N),
				q80 = RDS.SS.estimates(rd.dd, "zQ80", N = N)
			)
		)
	}
	
	# SS-PSE for population size estimation
	ss_pse <- posteriorsize(rd.dd, 
							mean.prior.size = 980000,
							maxN = 2000000,
							visibility = TRUE)
	
	save(ma_results, rds_results, ss_pse, 
		 file = "output/rds_estimates-a.RData")
	
	return(list(
		ma = ma_results,
		rds = rds_results,
		ss_pse = ss_pse
	))
}

results <- run_rds_analyses()
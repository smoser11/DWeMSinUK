# 02-data_preparation.R
source("R/utils/helper_functions.R")
library(RDS)

prepare_rds_data <- function() {
	# Load cleaned data
	load("data/processed/cleaned_data.RData")
	
	# Prepare RDS format data
	rd.dd <- as.rds.data.frame(data_nonzero, 
							   id="id", 
							   recruiter.id="recruiter.id", 
							   max.coupons = 5, 
							   check.valid = FALSE)
	
	# Calculate RDS weights
	dd <- data_nonzero %>%
		mutate(
			wt.RDS1_zQ36 = rds.I.weights(rd.dd, "zQ36"),
			wt.RDS1_zQ80 = rds.I.weights(rd.dd, "zQ80"),
			wt.vh_980k = vh.weights(numRef, N= 980000),
			wt.vh_100k = vh.weights(numRef, N= 100000),
			wt.vh_050k = vh.weights(numRef, N= 50000),
			wt.SS_980k = gile.ss.weights(numRef, N= 980000),
			wt.SS_100k = gile.ss.weights(numRef, N= 100000),
			wt.SS_050k = gile.ss.weights(numRef, N= 50000)
		)
	
	# Save prepared data
	save(dd, rd.dd, file = "data/processed/prepared_data.RData")
	
	return(list(dd = dd, rd.dd = rd.dd))
}

# Execute preparation
prepared_data <- prepare_rds_data()
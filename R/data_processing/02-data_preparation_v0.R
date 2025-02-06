# 02-data_preparation.R
source("R/utils/helper_functions.R")
library(RDS)
library(igraph)

prepare_network_data <- function() {
	# Load cleaned data
	load("data/processed/cleaned_data.RData")
	
	# Create RDS format data
	rd.dd <- as.rds.data.frame(data_nonzero, 
							   id="id", 
							   recruiter.id="recruiter.id", 
							   max.coupons = 5, 
							   check.valid = FALSE)
	
	# Calculate RDS weights
	data_nonzero <- data_nonzero %>%
		mutate(
			wt.RDS1_zQ36 = rds.I.weights(rd.dd, "zQ36"),
			wt.RDS1_zQ80 = rds.I.weights(rd.dd, "zQ80"),
			wt.vh_980k = vh.weights(numRef, N = 980000),
			wt.vh_100k = vh.weights(numRef, N = 100000),
			wt.vh_050k = vh.weights(numRef, N = 50000),
			wt.SS_980k = gile.ss.weights(numRef, N = 980000),
			wt.SS_100k = gile.ss.weights(numRef, N = 100000),
			wt.SS_050k = gile.ss.weights(numRef, N = 50000)
		)
	
	# Create network graph
	edges <- data.frame(
		from = data_nonzero$recruiter.id,
		to = data_nonzero$id
	)
	
	g <- graph_from_data_frame(edges, directed = TRUE)
	
	# Save prepared data
	save(data_nonzero, rd.dd, g, 
		 file = "data/processed/network_data.RData")
	
	return(list(
		data = data_nonzero,
		rds_data = rd.dd,
		graph = g
	))
}

# Execute preparation
network_data <- prepare_network_data()
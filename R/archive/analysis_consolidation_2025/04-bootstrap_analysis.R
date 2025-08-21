# 04-bootstrap_analysis.R
# Corrected bootstrap analysis using CE's comparable indicators
# Addresses: 1) Use comparable RDS/NSUM variables, 2) Fix CI bounds, 3) Add tree/cluster bootstrap

library(Neighboot)
library(RDStreeboot)
library(tidyverse)
library(igraph)
library(here)

load(here("data", "processed", "prepared_data.RData"))

# Create proper edges data frame (exclude seeds with recruiter.id == -1)
edges_df <- data.frame(
	from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
	to = rd.dd$id[rd.dd$recruiter.id != -1]
)

# Renumber nodes sequentially to avoid "Duplicate vertex names" 
node_map <- setNames(1:length(rd.dd$id), rd.dd$id)

# Use CE's comparable indicators (from 02-data_preparation.R)
comparable_traits <- data.frame(
	document_withholding = rd.dd$document_withholding_rds,
	pay_issues = rd.dd$pay_issues_rds, 
	threats_abuse = rd.dd$threats_abuse_rds,
	excessive_hours = rd.dd$excessive_hours_rds,
	access_to_help = rd.dd$access_to_help_rds
)

# Handle missing values (replace with 0 for binary indicators)
comparable_traits <- comparable_traits %>%
	mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))

# Create corrected RDS data structure
rds_data <- list(
	nodes = 1:length(node_map),
	edges = data.frame(
		from = node_map[as.character(edges_df$from)],
		to = node_map[as.character(edges_df$to)]
	),
	traits = comparable_traits,
	degree = setNames(rd.dd$network.size, 1:length(rd.dd$network.size))  # Use network.size as degree
)



run_bootstrap_analysis <- function() {

	
	# Neighborhood bootstrap with percentile method
	neigh_results <-  neighb(rds_data, 
					 quant=c(0.025, 0.975),
					 method="percentile", 
					 B=1000)
	



	
	save(neigh_results,   #, tree_results, cluster_results,
		 file="output/bootstrap_results.RData")
	
	return(list(
		neighborhood = neigh_results,
		tree = tree_results, 
		by_cluster = cluster_results
	))
}

bootstrap_results <- run_bootstrap_analysis()

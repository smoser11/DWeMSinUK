# 04-bootstrap_analysis.R
library(Neighboot)
library(RDStreeboot)
library(tidyverse)

load("./data/processed/prepared_data.RData")

edges_df <- data.frame(
	from = rd.dd$recruiter.id[rd.dd$recruiter.id != -1],
	to = rd.dd$id[rd.dd$recruiter.id != -1]
)

# Renumber nodes sequentially 
node_map <- setNames(1:length(rd.dd$id), rd.dd$id)

rds_data <- list(
	nodes = 1:length(node_map),
	edges = data.frame(
		from = node_map[as.character(edges_df$from)],
		to = node_map[as.character(edges_df$to)]
	),
	traits = data.frame(
		composite_risk = rd.dd$composite_risk,
		Q36 = rd.dd$zQ36, 
		Q80 = rd.dd$zQ80,
		row.names = 1:nrow(rd.dd)
	),
	degree = setNames(rd.dd$q13, 1:length(rd.dd$q13))
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
		neighborhood = neigh_results
	#	tree = tree_results, 
	#	by_cluster = cluster_results
	))
}

bootstrap_results <- run_bootstrap_analysis()
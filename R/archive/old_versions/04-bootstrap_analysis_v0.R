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
	

	# # Tree bootstrap 
	# tree_results <- list(
	# 	q36 = treeboot.RDS(rds_data, 
	# 					   c(0.025, 0.10, 0.90, 0.975), 
	# 					   B=2000),
	# 	
	# 	q80 = treeboot.RDS(rd.dd,
	# 					   c(0.025, 0.10, 0.90, 0.975), 
	# 					   B=2000)
	# )
	
	# Compare bootstrap results by nationality clusters
	
	# Function to create RDS data structure for each cluster
	create_cluster_rds <- function(cluster_ids, full_rds_data) {
		cluster_nodes <- cluster_ids
		
		# Filter edges for this cluster
		cluster_edges <- full_rds_data$edges %>%
			filter(from %in% cluster_ids & to %in% cluster_ids)
		
		list(
			nodes = cluster_nodes,
			edges = list(
				node1 = cluster_edges$from,
				node2 = cluster_edges$to
			),
			traits = full_rds_data$traits[cluster_nodes,],
			degree = full_rds_data$degree[cluster_nodes]
		)
	}
	
	# Apply analysis by cluster
	cluster_results <- split(rds_data$nodes, rd.dd$nationality_cluster) %>%
		map(~create_cluster_rds(.x, rds_data) %>%
				neighb(quant=c(0.025, 0.975), method="percentile", B=1000))
	
	rds_df <- data.frame(
		id = rds_data$nodes,
		nationality_cluster = rd.dd$nationality_cluster,  # Add this from original data
		traits = data.frame(
			composite_risk = rd.dd$composite_risk,
			Q36 = rd.dd$zQ36, 
			Q80 = rd.dd$zQ80,
			row.names = 1:nrow(rd.dd)
		),
		degree = rds_data$degree
	)
	
	cluster_results <- rds_df %>%
		group_by(nationality_cluster) %>%
		group_map(~neighb(.x, 
						  quant=c(0.025, 0.975),
						  method="percentile", 
						  B=1000))
	cluster_results <- rds_data %>%
		group_by(nationality_cluster) %>%
		group_map(~neighb(.x, 
						  quant=c(0.025, 0.975),
						  method="percentile", 
						  B=1000))
	
	save(neigh_results, tree_results, cluster_results,
		 file="output/bootstrap_results.RData")
	
	return(list(
		neighborhood = neigh_results,
	#	tree = tree_results, 
		by_cluster = cluster_results
	))
}

bootstrap_results <- run_bootstrap_analysis()
# 04-bootstrap_analysis.R
library(Neighboot)
library(RDStreeboot)

load("./data/processed/prepared_data.RData")

# Renumber nodes sequentially 
node_map <- setNames(1:length(rds_data$nodes), rds_data$nodes)

rds_data <- list(
	nodes = 1:length(node_map),
	edges = data.frame(
		from = node_map[as.character(edges_df$from)],
		to = node_map[as.character(edges_df$to)]
	),
	traits = data.frame(
		sum_categories = csv_data$sum_categories,
		Q36 = csv_data$zQ36, 
		Q80 = csv_data$zQ80,
		row.names = 1:nrow(csv_data)
	),
	degree = setNames(csv_data$q13, 1:length(csv_data$q13))
)


neighb(rds_data, 
	   quant=c(0.025, 0.975),
	   method="percentile", 
	   B=1000)



run_bootstrap_analysis <- function() {

	
	# Neighborhood bootstrap with percentile method
	neigh_results <- list(
		q36 = neighb(rd.dd, 
					 quant=c(0.025, 0.975),
					 method="percentile", 
					 B=1000),
		
		q80 = neighb(rd.dd, 
					 quant=c(0.025, 0.975),
					 method="percentile", 
					 B=1000)
	)
	
	# Tree bootstrap 
	tree_results <- list(
		q36 = treeboot.RDS(rd.dd, 
						   c(0.025, 0.10, 0.90, 0.975), 
						   B=2000),
		
		q80 = treeboot.RDS(rd.dd,
						   c(0.025, 0.10, 0.90, 0.975), 
						   B=2000)
	)
	
	# Compare bootstrap results by nationality clusters
	cluster_results <- dd %>%
		group_by(nationality_cluster) %>%
		group_map(~neighb(.x, 
						  quant=c(0.025, 0.975),
						  method="percentile", 
						  B=1000))
	
	save(neigh_results, tree_results, cluster_results,
		 file="output/bootstrap_results.RData")
	
	return(list(
		neighborhood = neigh_results,
		tree = tree_results, 
		by_cluster = cluster_results
	))
}

bootstrap_results <- run_bootstrap_analysis()
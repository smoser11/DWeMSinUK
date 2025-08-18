# 04-bootstrap_analysis.R
library(Neighboot)
library(RDStreeboot)

# First read the data
csv_data <- rd.dd  #read.csv("UpdateSelimRiskIndexsum_cat.csv")

# Create RDS data object 
rds_data <- list(
	nodes = sort(unique(c(csv_data$id, csv_data$recruiter.id))),
	edges = list(
		node1 = csv_data$recruiter.id,
		node2 = csv_data$id
	),
	traits = data.frame(
		sum_categories = csv_data$sum_categories,
		Q36 = csv_data$q36,
		Q80 = csv_data$q80,
		row.names = csv_data$id
	),
	degree = setNames(csv_data$q13, csv_data$id)
)

# Fix the RDS data structure to ensure alignment
rds_data <- list(
	nodes = unique(c(csv_data$id[!is.na(csv_data$id)])),
	edges = data.frame(
		node1 = csv_data$recruiter.id[!is.na(csv_data$recruiter.id)],
		node2 = csv_data$id[!is.na(csv_data$recruiter.id)]
	),
	traits = data.frame(
		sum_categories = csv_data$sum_categories,
		Q36 = csv_data$q36,
		Q80 = csv_data$q80,
		row.names = csv_data$id
	),
	degree = setNames(csv_data$q13, csv_data$id)
)

neighb(rds_data, 
			 quant=c(0.025, 0.975),
			 method="percentile", 
			 B=1000)

g <- graph_from_data_frame(rds_data$edges, directed = TRUE)

plot(g, 
	 vertex.size = 5,
	 vertex.label = NA,
	 edge.arrow.size = 0.5,
	 layout = layout_with_fr(g))	





edges_df <- data.frame(
	from = csv_data$recruiter.id[csv_data$recruiter.id != -1],
	to = csv_data$id[csv_data$recruiter.id != -1]
)

edges_df
g <- graph_from_data_frame(edges_df, directed = TRUE)

plot(g, 
	 vertex.size = 5,
	 vertex.label = NA,
	 edge.arrow.size = 0.5,
	 layout = layout_with_fr(g))	





summary(rd.dd$q36)


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
	load("data/processed/prepared_data.RData")

		
	# Prepare data structure for neighb()
	rds_data_q36 <- list(
		nodes = rd.dd$id,
		edges = list(
			node1 = rd.dd$recruiter.id,
			node2 = rd.dd$id
		),
		traits = data.frame(zQ36 = rd.dd$zQ36),  # Single trait
		degree = rd.dd$numRef
	)
	
	rds_data_q80 <- list(
		nodes = rd.dd$id,
		edges = list(
			node1 = rd.dd$recruiter.id,
			node2 = rd.dd$id
		),
		traits = data.frame(zQ80 = rd.dd$zQ80),  # Single trait
		degree = rd.dd$numRef
	)
	
	# Neighborhood bootstrap with percentile method
	neigh_results <- list(
		q36 = neighb(rds_data_q36, 
					 quant=c(0.025, 0.975),
					 method="percentile", 
					 B=1000),
		
		q80 = neighb(rds_data_q80, 
					 quant=c(0.025, 0.975),
					 method="percentile", 
					 B=1000)
	)
	
	# Save results
	save(neigh_results, file="output/bootstrap_results.RData")
	
	return(neigh_results)
}

bootstrap_results <- run_bootstrap_analysis()
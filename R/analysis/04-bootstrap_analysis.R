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
	
	cat("Running bootstrap analysis on CE's comparable indicators...\n")
	cat("Variables:", paste(names(rds_data$traits), collapse = ", "), "\n\n")
	
	# Neighborhood bootstrap with percentile method
	cat("Running neighborhood bootstrap (B=1000)...\n")
	neigh_results <- neighb(rds_data, 
					quant=c(0.025, 0.975),
					method="percentile", 
					B=1000)
	
	cat("Neighborhood bootstrap completed.\n")
	print("Results:")
	print(neigh_results)
	
	# Tree bootstrap (if network structure permits)
	cat("\nAttempting tree bootstrap...\n")
	tree_results <- NULL
	
	tryCatch({
		# Check if network is connected for tree bootstrap
		g <- graph_from_data_frame(rds_data$edges, directed = TRUE)
		
		if (is_connected(g, mode = "weak")) {
			# Create adjacency matrix for treeboot.RDS()
			n <- length(rds_data$nodes)
			adj_matrix <- matrix(0, n, n)
			
			for(i in 1:nrow(rds_data$edges)) {
				adj_matrix[rds_data$edges$from[i], rds_data$edges$to[i]] <- 1
			}
			
			# Tree bootstrap data structure
			tree_data <- list(
				traits = rds_data$traits,
				adj.mat = adj_matrix,
				degree = rds_data$degree,
				nodes = rds_data$nodes
			)
			
			tree_results <- treeboot.RDS(tree_data, 
									c(0.025, 0.975), 
									B=500)  # Smaller B for tree bootstrap
			
			cat("Tree bootstrap completed successfully.\n")
			
		} else {
			cat("Network not connected - skipping tree bootstrap.\n")
			tree_results <- "Network not connected"
		}
		
	}, error = function(e) {
		cat("Tree bootstrap failed:", e$message, "\n")
		tree_results <- paste("Error:", e$message)
	})
	
	# Cluster analysis (by nationality if available)
	cat("\nAttempting cluster analysis...\n")
	cluster_results <- NULL
	
	tryCatch({
		if ("nationality" %in% names(rd.dd)) {
			cluster_var <- rd.dd$nationality
		} else if ("nationality_cluster" %in% names(rd.dd)) {
			cluster_var <- rd.dd$nationality_cluster  
		} else {
			cluster_var <- rep("All", nrow(rd.dd))
		}
		
		# Function to create cluster-specific RDS data
		create_cluster_rds <- function(cluster_indices, full_rds_data) {
			
			# Map original indices to sequential node indices
			cluster_nodes <- cluster_indices
			
			# Filter edges within this cluster
			cluster_edges <- full_rds_data$edges %>%
				filter(from %in% cluster_nodes & to %in% cluster_nodes)
			
			if (nrow(cluster_edges) == 0) {
				return(NULL)  # Skip clusters with no internal edges
			}
			
			list(
				nodes = cluster_nodes,
				edges = cluster_edges,
				traits = full_rds_data$traits[cluster_nodes, , drop = FALSE],
				degree = full_rds_data$degree[cluster_nodes]
			)
		}
		
		# Split by clusters and run bootstrap
		cluster_splits <- split(1:length(cluster_var), cluster_var)
		
		cluster_results <- list()
		for (cluster_name in names(cluster_splits)) {
			cluster_indices <- cluster_splits[[cluster_name]]
			
			if (length(cluster_indices) < 10) {
				cat("Skipping cluster", cluster_name, "- too few observations (", length(cluster_indices), ")\n")
				next
			}
			
			cluster_rds <- create_cluster_rds(cluster_indices, rds_data)
			
			if (!is.null(cluster_rds) && nrow(cluster_rds$edges) > 0) {
				cat("Running bootstrap for cluster:", cluster_name, "(n =", length(cluster_indices), ")\n")
				
				cluster_results[[cluster_name]] <- neighb(cluster_rds,
													quant=c(0.025, 0.975),
													method="percentile", 
													B=500)  # Smaller B for clusters
			} else {
				cat("Skipping cluster", cluster_name, "- no internal edges\n")
			}
		}
		
		if (length(cluster_results) == 0) {
			cluster_results <- "No clusters with sufficient internal connectivity"
		}
		
	}, error = function(e) {
		cat("Cluster analysis failed:", e$message, "\n") 
		cluster_results <- paste("Error:", e$message)
	})
	
	# Save results
	cat("\nSaving bootstrap results...\n")
	save(neigh_results, tree_results, cluster_results,
		 file = here("output", "bootstrap_results.RData"))
	
	# Create summary table
	bootstrap_summary <- data.frame(
		Variable = rownames(neigh_results),
		SE = neigh_results[, "SE"],
		CI_Lower = neigh_results[, "0.025"],
		CI_Upper = neigh_results[, "0.975"],
		CI_Width = neigh_results[, "0.975"] - neigh_results[, "0.025"]
	)
	
	write.csv(bootstrap_summary, here("output", "tables", "bootstrap_summary.csv"), row.names = FALSE)
	
	cat("Bootstrap analysis completed!\n")
	cat("Results saved to:\n")
	cat("- output/bootstrap_results.RData\n")
	cat("- output/tables/bootstrap_summary.csv\n")
	
	return(list(
		neighborhood = neigh_results,
		tree = tree_results, 
		by_cluster = cluster_results,
		summary = bootstrap_summary
	))
}

# Execute if run directly
if (!exists("skip_execution")) {
	bootstrap_results <- run_bootstrap_analysis()
}

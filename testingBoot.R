# Extract unique node IDs
nodes <- unique(c(as.numeric(rd.dd$id), as.numeric(rd.dd$recruiter.id)))

# Create the edges list
edges <- list(
	node1 = as.numeric(rd.dd$recruiter.id),
	node2 = as.numeric(rd.dd$id)
)

# Remove rows with missing or invalid recruiter or recruit IDs
valid_edges <- !is.na(edges$node1) & !is.na(edges$node2) &
	edges$node1 %in% nodes & edges$node2 %in% nodes
edges$node1 <- edges$node1[valid_edges]
edges$node2 <- edges$node2[valid_edges]

# Create the traits data frame with an "id" column and convert relevant columns to numeric
trait_vars <- c("suspicious_variable", "q1", "q2", "q3", "q4", "q5", "q6", "q7",
				"q8", "q8_a", "q9", "q10", "q11", "q12", "q13", "q14", "q15",
				"q16", "q17", "q18_0", "q18_1", "q18_2", "q18_3", "q18_4",
				"q18_5", "q18_6", "q18_a", "q19", "q20", "q21", "q22", "q23",
				"q24", "q25", "q26", "q26_a", "q26_b", "q26_c", "q26_d", "q26_e",
				"q26_f", "q26_g", "q27", "q28", "q29", "q30", "q31", "q32",
				"q33", "q34", "q34_a", "q35", "q36", "q37", "q38", "q39",
				"q40", "q41", "q42", "q43", "q44", "q44_a", "q44_b", "q45",
				"q46", "q47", "q48", "q48_a", "q49", "q50", "q51", "q51_a",
				"q52", "q53", "q54", "q55_0", "q55_1", "q55_2", "q55_3",
				"q55_4", "q55_5", "q55_6", "q55_7", "q55_8", "q55_9", "q56",
				"q57_0", "q57_1", "q57_2", "q57_3", "q57_4", "q58", "q58_a",
				"q59", "q59_a", "q60", "q61", "q62", "q63", "q64", "q65",
				"q66", "q67_0", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5",
				"q67_6", "q67_7", "q67_8", "q67_9", "q67_10", "q67_11",
				"q67_12", "q67_a", "q68", "q69", "q70", "q71", "q72", "q73",
				"q74", "q75", "q76", "q77", "q78", "q79", "q80", "q81",
				"q82_92_0", "q82_92_1", "q82_92_2", "q82_92_3", "q82_92_4",
				"q82_5", "q82_6", "q82_7", "q82_8", "q82_9", "q82_10",
				"q82_11", "q82_12", "q82_13", "q82_14", "q82_15", "q82_16",
				"q82_17", "q82_18", "q82_19", "q82_a", "q83_88", "q84_89",
				"q85_90_93_96", "q86", "q87_95", "q98", "q99", "q100", "q101",
				"q102", "q103", "q103_a", "q104", "q105", "q106", "q107",
				"q108", "q109", "q110", "q111", "q112", "q113", "q114", "q115",
				"q116", "q117", "q118")

traits <- rd.dd[, c("id", trait_vars)]
traits[, trait_vars] <- lapply(traits[, trait_vars], as.numeric)

# Convert traits to numeric
traits <- as.data.frame(lapply(traits, as.numeric))
traits <- traits %>% select(where(is.numeric))
traits <- as.matrix(traits)


# Create the degree vector
degree <- as.numeric(rd.ddd$out_degree0)  # Assuming 'network.size' represents the degree

# Create the RDS.data list
RDS.data <- list(
	nodes = nodes,
	edges = edges,
	traits = traits,
	degree = degree
)

# Perform neighborhood bootstrap using neighb()
bootstrap_results <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)


.Nb <- function(RDS.data, B) {
	RDS.gr <- igraph::graph_from_data_frame(RDS.data$edges, directed = FALSE, vertices = data.frame(id = RDS.data$nodes))
	
	e.deg <- igraph::degree(RDS.gr, mode = "total")
	cr <- mean(e.deg)
	
	resamp <- list()
	sz <- round(length(RDS.data$nodes) / cr)
	
	for (b in 1:B) {
		xx.s <- sample(RDS.data$nodes, size = sz, replace = TRUE)
		x.neig <- as.numeric(unlist(igraph::ego(RDS.gr, order = 1, nodes = xx.s, mode = "all", mindist = 1)))
		resamp[[b]] <- x.neig
	}
	
	return(resamp)
}

.Nb(RDS.data, B = 10)


library(RDStreeboot)
treeboot.RDS(RDS.samp)
treeboot.RDS(RDS.data)


# Extract unique node IDs
nodes <- unique(c(as.numeric(rd.dd$id), as.numeric(rd.dd$recruiter.id)))

# Create the edges list
edges <- list(
	node1 = as.numeric(rd.dd$recruiter.id),
	node2 = as.numeric(rd.dd$id)
)

# Remove rows with missing or invalid recruiter or recruit IDs
valid_edges <- !is.na(edges$node1) & !is.na(edges$node2) &
	edges$node1 %in% nodes & edges$node2 %in% nodes
edges$node1 <- edges$node1[valid_edges]
edges$node2 <- edges$node2[valid_edges]

# Create the traits matrix with an "id" column
trait_vars <- c("suspicious_variable", "q1", "q2", "q3", "q4", "q5", "q6", "q7",
				"q8", "q8_a", "q9", "q10", "q11", "q12", "q13", "q14", "q15",
				"q16", "q17", "q18_0", "q18_1", "q18_2", "q18_3", "q18_4",
				"q18_5", "q18_6", "q18_a", "q19", "q20", "q21", "q22", "q23",
				"q24", "q25", "q26", "q26_a", "q26_b", "q26_c", "q26_d", "q26_e",
				"q26_f", "q26_g", "q27", "q28", "q29", "q30", "q31", "q32",
				"q33", "q34", "q34_a", "q35", "q36", "q37", "q38", "q39",
				"q40", "q41", "q42", "q43", "q44", "q44_a", "q44_b", "q45",
				"q46", "q47", "q48", "q48_a", "q49", "q50", "q51", "q51_a",
				"q52", "q53", "q54", "q55_0", "q55_1", "q55_2", "q55_3",
				"q55_4", "q55_5", "q55_6", "q55_7", "q55_8", "q55_9", "q56",
				"q57_0", "q57_1", "q57_2", "q57_3", "q57_4", "q58", "q58_a",
				"q59", "q59_a", "q60", "q61", "q62", "q63", "q64", "q65",
				"q66", "q67_0", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5",
				"q67_6", "q67_7", "q67_8", "q67_9", "q67_10", "q67_11",
				"q67_12", "q67_a", "q68", "q69", "q70", "q71", "q72", "q73",
				"q74", "q75", "q76", "q77", "q78", "q79", "q80", "q81",
				"q82_92_0", "q82_92_1", "q82_92_2", "q82_92_3", "q82_92_4",
				"q82_5", "q82_6", "q82_7", "q82_8", "q82_9", "q82_10",
				"q82_11", "q82_12", "q82_13", "q82_14", "q82_15", "q82_16",
				"q82_17", "q82_18", "q82_19", "q82_a", "q83_88", "q84_89",
				"q85_90_93_96", "q86", "q87_95", "q98", "q99", "q100", "q101",
				"q102", "q103", "q103_a", "q104", "q105", "q106", "q107",
				"q108", "q109", "q110", "q111", "q112", "q113", "q114", "q115",
				"q116", "q117", "q118")

traits <- as.data.frame(rd.dd[, c("id", trait_vars)])

# Create the degree vector
degree <- as.numeric(rd.dd$numRef)  # Assuming 'network.size' represents the degree

# Create the RDS.data list
RDS.data <- list(
	nodes = nodes,
	edges = edges,
	traits = traits,
	degree = degree
)

# Perform neighborhood bootstrap using neighb()
bootstrap_results <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)







# Extract unique node IDs
nodes <- unique(c(as.numeric(rd.dd$id), as.numeric(rd.dd$recruiter.id)))

# Create the edges list
edges <- list(
	node1 = as.numeric(rd.dd$recruiter.id),
	node2 = as.numeric(rd.dd$id)
)

# Remove rows with missing or invalid recruiter or recruit IDs
valid_edges <- !is.na(edges$node1) & !is.na(edges$node2) &
	edges$node1 %in% nodes & edges$node2 %in% nodes
edges$node1 <- edges$node1[valid_edges]
edges$node2 <- edges$node2[valid_edges]

# Create the traits data frame
traits <- as.data.frame(rd.dd[, c("suspicious_variable", "q1", "q2", "q3", "q4", "q5", "q6", "q7",	"q8", "q8_a", "q9", "q10", "q11", "q12", "q13", "q14", "q15",
					"q16", "q17", "q18_0", "q18_1", "q18_2", "q18_3", "q18_4",
					"q18_5", "q18_6", "q18_a", "q19", "q20", "q21", "q22", "q23",
					"q24", "q25", "q26", "q26_a", "q26_b", "q26_c", "q26_d", "q26_e",
					"q26_f", "q26_g", "q27", "q28", "q29", "q30", "q31", "q32",
					"q33", "q34", "q34_a", "q35", "q36", "q37", "q38", "q39",
					"q40", "q41", "q42", "q43", "q44", "q44_a", "q44_b", "q45",
					"q46", "q47", "q48", "q48_a", "q49", "q50", "q51", "q51_a",
					"q52", "q53", "q54", "q55_0", "q55_1", "q55_2", "q55_3",
					"q55_4", "q55_5", "q55_6", "q55_7", "q55_8", "q55_9", "q56",
					"q57_0", "q57_1", "q57_2", "q57_3", "q57_4", "q58", "q58_a",
					"q59", "q59_a", "q60", "q61", "q62", "q63", "q64", "q65",
					"q66", "q67_0", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5",
					"q67_6", "q67_7", "q67_8", "q67_9", "q67_10", "q67_11",
					"q67_12", "q67_a", "q68", "q69", "q70", "q71", "q72", "q73",
					"q74", "q75", "q76", "q77", "q78", "q79", "q80", "q81",
					"q82_92_0", "q82_92_1", "q82_92_2", "q82_92_3", "q82_92_4",
					"q82_5", "q82_6", "q82_7", "q82_8", "q82_9", "q82_10",
					"q82_11", "q82_12", "q82_13", "q82_14", "q82_15", "q82_16",
					"q82_17", "q82_18", "q82_19", "q82_a", "q83_88", "q84_89",
					"q85_90_93_96", "q86", "q87_95", "q98", "q99", "q100", "q101",
					"q102", "q103", "q103_a", "q104", "q105", "q106", "q107",
					"q108", "q109", "q110", "q111", "q112", "q113", "q114", "q115",
					"q116", "q117", "q118")])

# Convert traits to numeric
traits <- as.data.frame(lapply(traits, as.numeric))
traits <- traits %>% select(where(is.numeric))
traits <- as.matrix(traits)


# Create the degree vector
degree <- as.numeric(rd.dd$numRef)  # Assuming 'network.size' represents the degree

# Create the RDS.data list
RDS.data <- list(
	nodes = nodes,
	edges = edges,
	traits = traits,
	degree = degree
)

# Perform neighborhood bootstrap using neighb()
bootstrap_results <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)










# Extract unique node IDs
nodes <- unique(c(as.character(rd.dd$id), as.character(rd.dd$recruiter.id)))

# Create the edges list
edges <- list(
	node1 = as.character(rd.dd$recruiter.id),
	node2 = as.character(rd.dd$id)
)

# Remove rows with missing or invalid recruiter or recruit IDs
valid_edges <- !is.na(edges$node1) & !is.na(edges$node2) &
	edges$node1 %in% nodes & edges$node2 %in% nodes
edges$node1 <- edges$node1[valid_edges]
edges$node2 <- edges$node2[valid_edges]

# Create the traits data frame
traits <- rd.dd  # Replace with actual trait variable names

# Create the degree vector
degree <- rd.dd$network.size  # Assuming 'network.size' represents the degree

# Create the RDS.data list
RDS.data <- list(
	nodes = nodes,
	edges = edges,
	traits = traits,
	degree = degree
)

# Perform neighborhood bootstrap using neighb()
bootstrap_results <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)




# Assuming 'rd.dd' is your RDS data frame

# Extract unique node IDs
nodes <- as.numeric(unique(c(rd.dd$id, rd.dd$recruiter.id)) )

# Create the edges list
edges <- list(
	node1 = as.numeric( rd.dd$recruiter.id),
	node2 = as.numeric(rd.dd$id)
)

# Create the traits data frame
traits <- rd.dd  # Replace with actual trait variable names

# Create the degree vector
degree <- rd.dd$numRef  # Assuming 'network.size' represents the degree

# Create the RDS.data list
RDS.data <- list(
	nodes = nodes,
	edges = edges,
	traits = traits,
	degree = degree
)

.Nb(RDS.data, 10)
# Perform neighborhood bootstrap using neighb()
bootstrap_results <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)















library(dplyr)
library(Neighboot)

# Assuming rd.dd is already read into the environment
# Ensure the correct path to your file is used in your actual script
rd.dd <- read.csv("/path/to/your/dd.csv")

# Create nodes list ensuring all are unique and properly accounted for
nodes <- unique(c(rd.dd$recruiter.id, rd.dd$id))

# Ensure edges only include valid nodes and account for every node
edges <- rd.dd %>%
	filter(recruiter.id %in% nodes & id %in% nodes) %>%
	select(recruiter.id, id) %>%
	rename(node1 = recruiter.id, node2 = id)

# Correctly preparing traits assuming 'numRef' and 'NonEmptyCount' are the traits of interest
traits <- rd.dd %>%
	select(id, numRef, NonEmptyCount) %>%
	filter(id %in% nodes) %>% # Ensure traits are only for valid nodes
	rename(node = id)

# Compute degree correctly ensuring to account for all references of a node
degree <- as.vector(table(unlist(edges)))

# Preparing the correct list for neighb(), ensuring all components are correctly aligned
RDS.data <- list(nodes = as.numeric(nodes), edges = as.data.frame(edges), traits = traits, degree = degree)

# Now, the RDS.data is correctly formatted for use in neighb()
 result <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)





# Load necessary library
library(dplyr)

# Assuming rd.dd is already loaded into your R session

# Correct the approach to create edges and ensure it's a data frame with two columns: node1 and node2
edges <- rd.dd %>%
	filter(recruiter.id != -1, !is.na(recruiter.id)) %>%
	select(node1 = recruiter.id, node2 = id) %>%
	as.data.frame()

# Ensure nodes list includes all unique IDs from both recruiter.id and id, excluding invalid recruiter IDs
nodes <- unique(c(rd.dd$recruiter.id[rd.dd$recruiter.id != -1 & !is.na(rd.dd$recruiter.id)], rd.dd$id))

# Re-visit traits preparation ensuring it matches the required format and is comprehensive
traits <- rd.dd %>%
	select(id, numRef, NonEmptyCount) %>% # Adjust selected traits as needed
	distinct() %>%
	as.data.frame()

# Degree calculation needs to account for both appearances as recruiter and as recruit
# First, create a unified list of IDs from both roles
all_ids <- c(rd.dd$recruiter.id[rd.dd$recruiter.id != -1 & !is.na(rd.dd$recruiter.id)], rd.dd$id)
# Then, calculate degree (frequency of each ID)
degree_table <- as.data.frame(table(all_ids))
# Make sure to name the degree vector correctly and align it with the nodes
degree <- degree_table$Freq[match(nodes, degree_table$all_ids)]

# Combine everything into a list for neighb()
RDS.data <- list(nodes = nodes, edges = edges, traits = traits, degree = degree)

# Now, you can run neighb() with RDS.data
 result <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)








# Assuming rd.dd is already read into your R session
library(dplyr)

# Create nodes vector uniquely identifying all participants
nodes <- unique(c(rd.dd$recruiter.id, rd.dd$id))

# Ensure nodes are numeric and account for 'seed' or similar non-numeric IDs
nodes <- setdiff(nodes, "seed") # Remove 'seed' if present
nodes <- as.numeric(nodes)

# Create edges data frame without causing the 'by' error
edges <- data.frame(node1 = as.numeric(rd.dd$recruiter.id[rd.dd$recruiter.id != "seed" & !is.na(rd.dd$recruiter.id)]),
					node2 = rd.dd$id[rd.dd$recruiter.id != "seed" & !is.na(rd.dd$recruiter.id)])

# Check for any node missing in edges
all_nodes <- sort(unique(c(edges$node1, edges$node2)))
missing_nodes <- setdiff(nodes, all_nodes)

# If there are missing nodes, consider how you want to handle these. For example,
# if a node has no edges, it might still be important to include it in your analysis
# based on your specific requirements.

# Prepare traits (adjust according to your dataset's relevant columns)
traits <- rd.dd %>% 
	select(id, numRef, NonEmptyCount) %>%
	filter(id %in% nodes) %>%
	rename(node = id)

# Compute degree considering both sides of each edge
degree <- data.frame(node = nodes, degree = sapply(nodes, function(n) sum(edges$node1 == n | edges$node2 == n)))

# Prepare the list for neighb()
RDS.data <- list(nodes = nodes, edges = edges, traits = traits, degree = degree$degree)

# Now the RDS.data structure should be correctly formatted for neighb() without causing errors
 result <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)



# Load necessary libraries
library(dplyr)

# Read the data
rd.dd <- read.csv("/path/to/your/dd.csv")

rd.dd$recruiter.id <- as.numeric(rd.dd$recruiter.id)
rd.dd$id <- as.numeric(rd.dd$id)

# Create nodes vector
nodes <- as.numeric(unique(c(rd.dd$recruiter.id, rd.dd$id)) )

# Create edges data frame
edges <- rd.dd %>% 
	select(recruiter.id, id) %>% 
	filter(!is.na(recruiter.id) ) %>% # Assuming -1 and NA are not valid IDs
	rename(node1 = (recruiter.id), node2 = (id) ) 

# Prepare traits data frame (select relevant traits columns, example uses `numRef` and `NonEmptyCount`)
traits <- rd.dd %>% 
	select(id, numRef, NonEmptyCount) %>%
	rename(node = id)

# Compute degree (assuming degree is the count of how often a node appears as recruiter or recruit)
degree_data <- rbind(data.frame(id = rd.dd$recruiter.id), data.frame(id = rd.dd$id))
degree <- degree_data %>%
	filter(id != -1 & !is.na(id)) %>% # Assuming -1 and NA are not valid IDs
	group_by(id) %>%
	summarise(degree = n()) %>%
	ungroup() %>%
	arrange(id)

# Prepare the list for neighb()
RDS.data <- list(nodes = nodes, edges = edges, traits = traits, degree = degree$degree)

# Run neighb() function (example, adjust parameters as needed)
 result <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)






.Nb <- function(RDS.data, B) {
	# Assuming RDS.data$edges is structured correctly with node1 and node2 columns
	# and RDS.data$traits has an 'id' column that matches the node IDs
	
	# Create a vertices dataframe that ensures unique names by using unique node IDs from both node1 and node2
	all_node_ids <- unique(c(RDS.data$edges$node1, RDS.data$edges$node2))
	vertices_df <- data.frame(id = all_node_ids)
	
	# Ensure traits information is aligned with these IDs
	# Assuming the first column of traits is 'id' and uniquely identifies traits for each node
	# Merge to ensure all nodes have trait information, fill missing with NA or appropriate values
	vertices_df <- merge(vertices_df, RDS.data$traits, by = "id", all.x = TRUE)
	
	# Create the graph using the corrected vertices and edges information
	RDS.gr <- igraph::graph_from_data_frame(RDS.data$edges, directed = FALSE, vertices = vertices_df)
	
	e.deg <- igraph::degree(RDS.gr, mode = "total")
	cr <- mean(e.deg)
	
	resamp <- list()
	sz <- round(length(all_node_ids) / cr)
	
	for (b in 1:B) {
		xx.s <- sample(all_node_ids, size = sz, replace = TRUE)
		x.neig <- as.numeric(unlist(igraph::ego(
			RDS.gr,
			order = 1,
			nodes = xx.s,
			mode = "all",
			mindist = 1
		)))
		resamp[[b]] <- x.neig
	}
	return(resamp)
}







.Nb <- function(RDS.data, B) {
	# Ensure vertex names are unique by using a unique identifier column directly
	# Assuming 'id' is a column in RDS.data$traits that uniquely identifies each row
	unique_ids <- RDS.data$traits$id
	
	# Create the graph using the unique identifiers for vertices
	RDS.gr <- igraph::graph_from_data_frame(RDS.data$edges, directed = FALSE, 
											vertices = RDS.data$traits)
	
	e.deg <- igraph::degree(RDS.gr, mode = "total")
	cr <- mean(e.deg)
	
	resamp <- list()
	sz <- round(length(unique_ids) / cr)
	
	for (b in 1:B) {
		xx.s <- sample(unique_ids, size = sz, replace = TRUE)
		x.neig <- as.numeric(unlist(igraph::ego(
			RDS.gr,
			order = 1,
			nodes = xx.s,
			mode = "all",
			mindist = 1
		)))
		resamp[[b]] <- x.neig
	}
	return(resamp)
}


######################################################################
######################################################################
######################################################################
########## TESTINg

library(Neighboot)
library(RDStreeboot)
library(tidyverse)

set.seed(11111111)
data(faux.network)

dim(faux.network$adj.mat )
apply( faux.network$adj.mat,
	   2, sum)

samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)
str(samp)
samp$degree
neighb(samp, B=10)
names(samp$traits)

dim(faux.network2$adj.mat )
nZ <- 800

faux.network_zeroRows <- faux.network
faux.network_zeroRows$adj.mat[1:nZ,] <- matrix(0, nrow = 1000, ncol = nZ) 
apply( faux.network_zeroRows$adj.mat,
	   1, sum)
samp2r <- sample.RDS(faux.network_zeroRows$traits, faux.network_zeroRows$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)
samp2r$degree

neighb(samp2r, B=10)


faux.network_zeroCols <- faux.network
faux.network_zeroCols$adj.mat[,1:nZ] <- matrix(0, nrow = 1000, ncol = nZ) 
apply( faux.network_zeroCols$adj.mat,
	   2, sum)
samp2c <- sample.RDS(faux.network_zeroCols$traits, faux.network_zeroCols$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)
str(samp2c)
samp2c$degree
neighb(samp2c, B=10)

## So provided CRAN packages do not allow for zero degree vertices.  


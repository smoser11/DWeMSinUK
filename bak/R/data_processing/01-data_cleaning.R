
## 01-data_cleaning.R
source("R/utils/helper_functions.R")

# Import and clean raw data 
read_clean_data <- function() {
	data <- read.csv("data/raw/UpdateSelimRiskIndex-sum_cat.csv") %>%
		clean_names() %>%
		mutate(
			# Convert numeric columns
			across(c("q13", "composite_risk", starts_with("category_"), 
					 "sum_categories", "whether_exploitation", "suspicious_variable"), 
					 as.numeric),
			# Handle recruiter ID
			recruiter.id = replace_na(recruiter_id, -1),
			network.size.variable = as.numeric(q13)
		) %>%
		filter(!is.na(id))
	
	return(data)
}

# Process referrals and calculate network size
process_network_data <- function(data) {
	df_processed <- data %>%
		rowwise() %>%
		mutate(
			NonEmptyCount = sum(!is.na(c_across(q105:q115))),
			NonEmptyValues = list(na.omit(c_across(q105:q115)))
		) %>%
		ungroup()
	
	# Calculate referral frequencies
	count_df <- df_processed %>%
		group_by(node_1_recruiter) %>%
		summarise(referedFreq = n(), .groups = 'drop')
	
	df_processed <- df_processed %>%
		left_join(count_df, by = c("id" = "node_1_recruiter")) %>%
		mutate(
			referedFreq = replace_na(referedFreq, 0),
			suspicious_variable = ifelse(NonEmptyCount > q13 | referedFreq > q13, 1, 0),
			numRef = pmax(NonEmptyCount, q13, referedFreq),
			network.size = numRef
		)
	
	return(df_processed)
}

# Main cleaning function
clean_data <- function() {
	data <- read_clean_data()
	data <- process_network_data(data)
	data <- clean_exploitation_indicators(data)
	data <- create_nationality_clusters(data)
	
	# Create two versions
	data_nonzero <- data %>% filter(numRef > 0)
	
	# Save processed data
	save(data, data_nonzero, file = "./data/processed/cleaned_data.RData")
	
	return(list(full = data, nonzero = data_nonzero))
}

# Execute cleaning
cleaned_data <- clean_data()

# helper_functions.R
library(tidyverse)
library(janitor)

# Network size validation and calculation
calculateOutDegreeIncludingZeros <- function(rds.df) {
	unique_ids <- unique(rds.df$id)
	out_degree_df <- data.frame(id = unique_ids, out_degree = 0)
	valid_recruiter_ids <- rds.df$recruiter.id[rds.df$recruiter.id != -1]
	out_degree <- table(valid_recruiter_ids)
	
	for (i in seq_along(out_degree_df$id)) {
		id <- out_degree_df$id[i]
		if (id %in% names(out_degree)) {
			out_degree_df$out_degree[i] <- out_degree[[as.character(id)]]
		}
	}
	return(out_degree_df)
}

# Nationality clustering
create_nationality_clusters <- function(data) {
	data %>%
		mutate(
			nationality_cluster = case_when(
				str_detect(tolower(q8_a), "filipin|pilip|asia") ~ "Filipino",
				str_detect(tolower(q8_a), "latin|spain|spai|brazil|colombia|mexico|peru|ecuador") ~ "Latinx",
				q8 == 0 | str_detect(tolower(q8_a), "british|uk|united kingdom") ~ "British",
				TRUE ~ "Other"
			)
		)
}

# Handle Don't Know responses
clean_exploitation_indicators <- function(data) {
	data %>%
		mutate(
			zQ36 = case_when(
				q36 %in% c(0,1,2,3) ~ 1,
				q36 == 4 ~ 0,
				q36 == 5 ~ NA_real_,
				TRUE ~ NA_real_
			),
			zQ80 = case_when(
				q80 %in% c(0,1) ~ 1,
				q80 %in% c(2,3) ~ 0,
				q80 == 4 ~ NA_real_,
				TRUE ~ NA_real_
			)
		)
}
# 05-nsum_estimation.R
library(tidyverse)

estimate_hidden_pop_size <- function(data, weights, hidden_var, 
									 hidden_connections_var, frame_size,
									 degree_vars, probe_sizes) {
	
	# Check inputs
	if (length(degree_vars) != length(probe_sizes)) {
		stop("degree_vars and probe_sizes must have the same length")
	}
	
	# Calculate y_F,H (average number of hidden population members known)
	y_F_H <- sum(data[[hidden_connections_var]] / weights, na.rm = TRUE)
	
	# Get frame population (excluding hidden population)
	frame_data <- data[data[[hidden_var]] == 0, ]
	frame_weights <- weights[data[[hidden_var]] == 0]
	
	# Calculate d_F,F (average personal network size)
	d_F_F <- sapply(1:length(degree_vars), function(i) {
		sum(frame_data[[degree_vars[i]]] / frame_weights, na.rm = TRUE) / probe_sizes[i]
	})
	d_F_F <- sum(d_F_F, na.rm = TRUE)
	
	# Calculate N_H estimate
	N_H_estimate <- (y_F_H / d_F_F) * frame_size
	
	return(list(
		N_H_estimate = N_H_estimate,
		y_F_H = y_F_H,
		d_F_F = d_F_F,
		prop = (y_F_H / d_F_F)
	))
}

run_nsum_estimation <- function() {
	load("data/processed/prepared_data.RData")
	
	# Define estimation parameters
	params <- list(
		# For Q36 (below minimum wage)
		q36 = list(
			weights = dd$wt.SS_980k,
			hidden_var = "zQ36",
			hidden_connections = "hidden_connections_var_z",
			frame_size = 51000,
			degree_vars = c("q13", "q84_89"),
			probe_sizes = c(980000, 73019)
		),
		
		# For Q80 (NRM referral)
		q80 = list(
			weights = dd$wt.RDS1_zQ80,
			hidden_var = "zQ80", 
			hidden_connections = "q84_89",
			frame_size = 51000,
			degree_vars = c("q13", "q84_89"),
			probe_sizes = c(980000, 73019)
		)
	)
	
	# Run estimations
	results <- lapply(params, function(p) {
		estimate_hidden_pop_size(
			data = dd,
			weights = p$weights,
			hidden_var = p$hidden_var,
			hidden_connections_var = p$hidden_connections,
			frame_size = p$frame_size,
			degree_vars = p$degree_vars,
			probe_sizes = p$probe_sizes
		)
	})
	
	save(results, file = "output/nsum_results.RData")
	return(results)
}

nsum_results <- run_nsum_estimation()



###############################################################################

# 05-nsum_estimation.R
library(tidyverse)
library(RDS)
library(ggplot2)

# Core NSUM estimation function with RDS weights
estimate_hidden_pop_size <- function(data, weights, hidden_var, 
									 hidden_connections_var, total_pop_size,
									 degree_vars, probe_sizes) {
	
	# Basic error checking
	if (length(degree_vars) != length(probe_sizes)) {
		stop("degree_vars and probe_sizes must have same length")
	}
	
	# Calculate visibility fraction (y_F,H)
	y_F_H <- sum(data[[hidden_connections_var]] * weights, na.rm = TRUE) / 
		sum(weights, na.rm = TRUE)
	
	# Calculate average degree (d_F,F) using known populations
	# Use weighted average of degree variables (don't divide by probe_sizes - that's for scaling)
	d_F_F <- sapply(1:length(degree_vars), function(i) {
		sum(data[[degree_vars[i]]] * weights, na.rm = TRUE) / 
			sum(weights, na.rm = TRUE)
	})
	d_F_F <- mean(d_F_F, na.rm = TRUE)
	
	# NSUM estimate - calculate proportion first, then scale if needed
	proportion_estimate <- y_F_H / d_F_F
	N_H_estimate <- proportion_estimate * total_pop_size
	
	return(list(
		N_H_estimate = N_H_estimate,
		proportion_estimate = proportion_estimate,
		y_F_H = y_F_H,
		d_F_F = d_F_F,
		vis_fraction = proportion_estimate
	))
}

run_nsum_estimation <- function() {
	load("data/processed/prepared_data.RData")
	
	# Define weighting schemes to test
	weight_schemes <- list(
		SS_980k = dd$wt.SS_980k,
		SS_100k = dd$wt.SS_100k,
		SS_050k = dd$wt.SS_050k,
		RDS1_q36 = dd$wt.RDS1_zQ36,
		RDS1_q80 = dd$wt.RDS1_zQ80,
		vh_980k = dd$wt.vh_980k,
		vh_100k = dd$wt.vh_100k,
		vh_050k = dd$wt.vh_050k
	)
	
	# Define estimation parameters
	estimation_params <- list(
		# Q36 (below minimum wage) parameters
		q36 = list(
			hidden_var = "zQ36",
			hidden_connections = "hidden_connections_var_z",
			degree_vars = c("q13", "q84_89"),
			probe_sizes = c(980000, 73019)  # Total DW pop and NRM referrals
		),
		# Q80 (NRM experience) parameters
		q80 = list(
			hidden_var = "zQ80",
			hidden_connections = "q84_89",
			degree_vars = c("q13", "q84_89"),
			probe_sizes = c(980000, 73019)
		)
	)
	
	# Run all combinations
	results <- list()
	for(trait in names(estimation_params)) {
		results[[trait]] <- list()
		for(weight_name in names(weight_schemes)) {
			results[[trait]][[weight_name]] <- estimate_hidden_pop_size(
				data = dd,
				weights = weight_schemes[[weight_name]],
				hidden_var = estimation_params[[trait]]$hidden_var,
				hidden_connections_var = estimation_params[[trait]]$hidden_connections,
				total_pop_size = 980000,  # Total DW population estimate
				degree_vars = estimation_params[[trait]]$degree_vars,
				probe_sizes = estimation_params[[trait]]$probe_sizes
			)
		}
	}
	
	# Create comparison plots
	plot_weights <- function(weight_schemes) {
		weight_df <- as.data.frame(weight_schemes) %>%
			gather(key = "method", value = "weight")
		
		ggplot(weight_df, aes(x = method, y = weight)) +
			geom_boxplot() +
			theme_minimal() +
			labs(title = "Comparison of RDS Weighting Methods",
				 x = "Method", y = "Weight") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1))
	}
	
	weight_plot <- plot_weights(weight_schemes)
	
	# Save results
	save(results, weight_plot, file = "output/nsum_results.RData")
	
	return(list(
		estimates = results,
		weight_plot = weight_plot
	))
}

nsum_results <- run_nsum_estimation()

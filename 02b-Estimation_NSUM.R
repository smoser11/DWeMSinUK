 

rm(list=ls())
library(rstudioapi)
library(tidyverse)
dirname(rstudioapi::getSourceEditorContext()$path) %>% setwd()

load("./survey/dd.RData")

names(dd)

###


estimate_hidden_pop_size <- function(rds_data, rds_weights, hidden_var, hidden_connections_var,  frame_size, degree_vars, probe_sizes) {
	
	# Check that degree_vars and probe_sizes have the same length
	if (length(degree_vars) != length(probe_sizes)) {
		stop("degree_vars and probe_sizes must have the same length")
	}
	
	# Calculate y_F,H
	y_F_H <- sum(rds_data[[hidden_connections_var]] / rds_weights)
	
	# Subset to the frame population (excluding the hidden population)
	frame_data <- rds_data[rds_data[[hidden_var]] == 0, ]
	frame_weights <- rds_weights[rds_data[[hidden_var]] == 0]
	
	# Calculate d_F,F
	d_F_F <- sapply(1:length(degree_vars), function(i) {
		sum(frame_data[[degree_vars[i]]] / frame_weights) / probe_sizes[i]
	})
	d_F_F <- sum(d_F_F)
	
	# Calculate the estimate of N_H
	N_H_estimate <- (y_F_H / d_F_F) * frame_size
	
	# Return the result
	return(list(N_H_estimate = N_H_estimate, 
				y_F_H = y_F_H, 
				d_F_F = d_F_F))
}




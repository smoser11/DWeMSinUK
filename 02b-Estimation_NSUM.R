' Compute standard errors and confidence intervals
#'
#' This function estimate standard errors and compute confidence intervals from
#' an RDS sample using the neighborhood bootstrap method.
#'
#'@usage neighb(RDS.data, quant=c(0.025, 0.975),
#'       method=c("percentile","Wald"), B=1000)
#' @param RDS.data A list containing the following objects:
#' \describe{
#' \item{\code{nodes}}{ a numeric vector containing IDs}
#' \item{\code{edges}}{ a list containing two vectors: \code{node1} for the recruiter's ID and \code{node2} for the recruit's ID.  }
#' \item{\code{traits}}{a data frame containing respondents' traits. }
#' \item{\code{degree}}{a vector containing each node's degree, or number of social connections. }
#' }
#' @param quant a vector of positive integers between 0 and 1, representing quantiles to be estimated.
#' @param method a character string representing the method for computing confidence intervals,
#'               either \code{percentile} or \code{Wald}. Default is \code{percentile}.
#' @param B the number of bootstrap repetitions. Default is 1000.
#' @details The function \code{neighb} compute standard errors and confidence intervals using
#'          the neighborhood bootstrap method for RDS. Confidence intervals can be computed using
#'          the percentile method or the studentized method.
#' @return A matrix of estimated standard errors and quantiles. Each row represents a trait.
#' @author Mamadou Yauck <yauck.mamadou@uqam.ca> and Erica E. M. Moodie.
#' @export
#' @importFrom stats qt quantile sd
#' @importFrom igraph graph_from_data_frame degree ego
#' @importFrom RDStreeboot sample.RDS
#' @examples
#' #Load the synthetic population network dataset.
#' data("pop.network")
#'
#' #Draw an RDS sample from the simulated network using the sampleRDS function
#' #from the package RDStreeboot.
#' require(RDStreeboot)
#' RDS.samp <- sample.RDS(pop.network$traits, pop.network$adj.mat, 200, 10,
#'  3, c(1/6,1/3,1/3,1/6), FALSE)
#'
#' #Compute 95\% confidence intervals using the percentile method
#' neighb(RDS.data=RDS.samp, quant=c(0.025, 0.975),method="percentile", B=100)
neighb<- function(RDS.data, quant=c(0.025, 0.975),method=c("percentile","Wald"), B=1000) {
	p.est<-apply((RDS.data$traits/RDS.data$degree)/sum(1/RDS.data$degree),2,sum,na.rm=TRUE)
	resamp <- .Nb(RDS.data, B)
	results <- matrix(NA, dim(RDS.data$traits)[2], (length(quant)+1))
	method <- match.arg(method)
	for(t in 1:dim(RDS.data$traits)[2]) {
		p.TBS <- sapply(resamp, .propvh, RDS.data$traits[,t], RDS.data$degree)
		results[t,1] <- sd(p.TBS,na.rm = TRUE)
		for(q in 2:(length(quant)+1)){
			if(method%in%c("percentile")){
				results[t,q] <- quantile(p.TBS,quant[q-1],na.rm=TRUE)
			}else if(method%in%c("studentized")){
				results[t,q]<-p.est[t]+qt(quant[q-1],dim(RDS.data$traits)[2]-1)*results[t,1]
			}else{
				stop("The method is invalid.")
			}
		}
	}
	
	rownames(results) <- colnames(RDS.data$traits)
	colnames(results) <- c("SE",quant)
	return(results)
}

### FROM:  https://rdrr.io/cran/Neighboot/src/R/neighb.R

.propvh<- function(RDS.data, trait, dg) sum(trait[RDS.data]/dg[RDS.data], na.rm=T)/sum((!is.na(trait[RDS.data]))/dg[RDS.data])
.Nb<- function(RDS.data, B) {
	
	RDS.gr<-igraph::graph_from_data_frame(RDS.data$edges, directed = F, vertices = cbind(id=1:length(RDS.data$traits[,2]),RDS.data$traits))
	e.deg<-igraph::degree(RDS.gr,mode="total")
	cr<-mean(e.deg)
	
	resamp <- list()
	sz<-round(length(RDS.data$traits[,2])/cr)
	
	for(b in 1:B) {
		xx.s<-sample(1:length(RDS.data$traits[,2]),size=sz,replace=TRUE)
		x.neig<-as.numeric(unlist(igraph::ego(
			RDS.gr,
			order = 1,
			nodes = xx.s,
			mode = "all",
			mindist = 1
		)))
		#x.neig<-as.numeric(unlist(adjacent_vertices(RDS.gr, v=xx.s, mode = "all")))
		resamp[[b]]<-x.neig
	}
	return(resamp)
}


###############################################
rds.df <- rd.dd



calculateOutDegreeIncludingZeros <- function(rds.df) {
	# Initialize a dataframe with unique ids and out_degree set to 0
	unique_ids <- unique(rds.df$id)
	out_degree_df <- data.frame(id = unique_ids, out_degree = 0)
	
	# Filter out '-1' from recruiter.id to exclude seeds
	valid_recruiter_ids <- rds.df$recruiter.id[rds.df$recruiter.id != -1]
	
	# Calculate the out-degree (frequency) of each recruiter ID
	out_degree <- table(valid_recruiter_ids)
	
	# Update the out_degree_df with the calculated frequencies
	for (i in seq_along(out_degree_df$id)) {
		id <- out_degree_df$id[i]
		if (id %in% names(out_degree)) {
			out_degree_df$out_degree[i] <- out_degree[[as.character(id)]]
		}
	}
	
	return(out_degree_df)
}

# Example usage:
# Assuming your rds.data.frame is named rds.df
 out_degree0_df <- calculateOutDegreeIncludingZeros(rds.df)
 out_degree0_df
 colnames(out_degree0_df) <- c("id2", "out_degree0")
 
 rd.ddd <- cbind(rd.dd, out_degree_df)
 sort(names(rd.ddd))
 
 rd.ddd <- rd.ddd %>% select(recruiter.id, recruiter_id, network.size.variable, id, id2,out_degree, numRef, NonEmptyCount, q13, referedFreq, everything())
names(rd.ddd)
 	
 
 
####################


 

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
	y_F_H <- sum(rds_data[[hidden_connections_var]] / rds_weights, na.rm = TRUE)
	
	# Subset to the frame population (excluding the hidden population)
	frame_data <- rds_data[rds_data[[hidden_var]] == 0, ]
	frame_weights <- rds_weights[rds_data[[hidden_var]] == 0]
	
	# Calculate d_F,F
	d_F_F <- sapply(1:length(degree_vars), function(i) {
		sum(frame_data[[degree_vars[i]]] / frame_weights, na.rm = TRUE) / probe_sizes[i]
	})
	d_F_F <- sum(d_F_F, na.rm = TRUE)
	
	# Calculate the estimate of N_H
	N_H_estimate <- (y_F_H / d_F_F) * frame_size
	
	# Return the result
	return(list(N_H_estimate = N_H_estimate, 
				y_F_H = y_F_H, 
				d_F_F = d_F_F,
				prop = (y_F_H / d_F_F)) )
}

estimate_hidden_pop_size <- function(rds_data, rds_weights, hidden_var, hidden_connections_var, 
									 degree_vars, probe_sizes) {
	
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
	N_H_estimate <- y_F_H / d_F_F
	
	# Return the result
	return(list(N_H_estimate = N_H_estimate, 
				y_F_H = y_F_H, 
				d_F_F = d_F_F))
}


sort(names(rd.dd) )
rd.dd$hidden_connections_var <- rd.dd$q83_88 + rd.dd$q84_89

rd.dd$zq83_88 <- rd.dd$q83_88 
rd.dd$zq83_88[is.na(rd.dd$zq83_88)] <- 0
rd.dd$zq84_89 <-  rd.dd$q84_89
rd.dd$zq84_89[is.na(rd.dd$zq84_89)] <- 0

rd.dd$hidden_connections_var_z <- rd.dd$zq83_88 + rd.dd$zq84_89
rd.dd$hidden_connections_var_z
rd.dd$zQ80

estimate_hidden_pop_size(rds_data = rd.dd, 
						 rds_weights =  rd.dd$wt.SS_980k, #rd.dd$wt.SS_980k,
						 hidden_var = "zQ36",
						 hidden_connections_var = "hidden_connections_var_z",
						 frame_size = 51000,
						 degree_vars = c("q13", "q84_89"),
						 probe_sizes = c(980000, 73019))


estimate_hidden_pop_size(rds_data = rd.dd, 
						 rds_weights =  rd.dd$wt.RDS1_zQ80, #rd.dd$wt.RDS1_zQ80, #rd.dd$wt.SS_980k,
						 hidden_var = "zQ80",
						 hidden_connections_var = "q84_89",
						 frame_size = 51000,
						 degree_vars = c("q13", "q84_89"),
						 probe_sizes = c(980000, 73019))

sort(names(rd.dd))
estimate_hidden_pop_size(rds_data = rd.dd, 
						 rds_weights =  rd.dd$wt.SS_100k, #rd.dd$wt.RDS1_zQ80, #rd.dd$wt.SS_980k,
						 hidden_var = "zQ36",
						 hidden_connections_var = "q84_89",
						 frame_size = 51000,
						 degree_vars = c("q84_89"),
						 probe_sizes = c(73019))


library(Neighboot)
#
library(RDStreeboot)
.Nb(rd.dd, 10)
neighb(rd.dd)

# Load the data
dd <- read.csv("/path/to/your/dd.csv") # Update the path to where your file is located
names(dd)

# Nodes (unique IDs of individuals)
nodes <- unique(dd$id)
nodes

# Edges (relationships between recruiters and recruits)
edges <- list(node1 = dd$recruiter.id, node2 = as.numeric(as.character(dd$id)))

# Traits (assuming you select specific columns as traits; adjust the column names as necessary)
traits <- dd[, c("id", "trait1", "trait2")] # Replace "trait1", "trait2" with actual trait columns

# Degree (if degree information is not directly available, calculate it based on recruiter.id)
# This example assumes each row in your data frame represents a connection or recruitment event.
degree <- dd$numRef
degree

# Prepare the list for neighb()
RDS.data <- list(nodes = nodes, edges = edges, traits = dd, degree = degree)

.Nb(RDS.data, 10)

# Use neighb() function
result <- neighb(RDS.data, quant = c(0.025, 0.975), method = "percentile", B = 1000)





Using this function:
	"""
	Using this function:
"""
.Nb<- function(RDS.data, B) {
	
	RDS.gr<-igraph::graph_from_data_frame(RDS.data$edges, directed = F, vertices = cbind(id=1:length(RDS.data$traits[,2]),RDS.data$traits))
	e.deg<-igraph::degree(RDS.gr,mode="total")
	cr<-mean(e.deg)
	
	resamp <- list()
	sz<-round(length(RDS.data$traits[,2])/cr)
	
	for(b in 1:B) {
		xx.s<-sample(1:length(RDS.data$traits[,2]),size=sz,replace=TRUE)
		x.neig<-as.numeric(unlist(igraph::ego(
			RDS.gr,
			order = 1,
			nodes = xx.s,
			mode = "all",
			mindist = 1
		)))
		#x.neig<-as.numeric(unlist(adjacent_vertices(RDS.gr, v=xx.s, mode = "all")))
		resamp[[b]]<-x.neig
	}
	return(resamp)
}
"""
 on `RDS.data` results in this error:
"""
Error in igraph::graph_from_data_frame(RDS.data$edges, directed = F, vertices = cbind(id = 1:length(RDS.data$traits[,  : 
																														Duplicate vertex names
																													"""
																													
																													
																													
.Nb<- function(RDS.data, B) {
	
	RDS.gr<-igraph::graph_from_data_frame(RDS.data$edges, directed = F, vertices = cbind(id=1:length(RDS.data$traits[,2]),RDS.data$traits))
	e.deg<-igraph::degree(RDS.gr,mode="total")
	cr<-mean(e.deg)
	
	resamp <- list()
	sz<-round(length(RDS.data$traits[,2])/cr)
	
	for(b in 1:B) {
		xx.s<-sample(1:length(RDS.data$traits[,2]),size=sz,replace=TRUE)
		x.neig<-as.numeric(unlist(igraph::ego(
			RDS.gr,
			order = 1,
			nodes = xx.s,
			mode = "all",
			mindist = 1
		)))
		#x.neig<-as.numeric(unlist(adjacent_vertices(RDS.gr, v=xx.s, mode = "all")))
		resamp[[b]]<-x.neig
	}
	return(resamp)
}
"""
on `RDS.data` results in this error:
	"""
Error in igraph::graph_from_data_frame(RDS.data$edges, directed = F, vertices = cbind(id = 1:length(RDS.data$traits[,  : 
  Duplicate vertex names
"""

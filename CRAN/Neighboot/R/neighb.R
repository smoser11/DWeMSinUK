#' Compute standard errors and confidence intervals
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


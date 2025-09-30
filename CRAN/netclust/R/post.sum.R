#' @title Estimate the Size of a Clustered Hidden Population Using RDS Data:
#' The Posterior Sum Model
#'
#' @description Produces a sample from the posterior distribution of the
#' overall population size for a clustered population. Given a list of sspse
#' fit objects from each cluster \eqn{1, ..., m}, \code{post.sum} produces a
#' sample from the posterior distribution of the overall population size
#' \eqn{N = N_1 + ... + N_m}.
#'
#' @param sspse.fits list of length \eqn{m}; sspse fit objects from each
#' cluster.
#' @param samplesize count; desired sample size from the posterior distribution of
#' overall population size.
#'
#' @return A vector containing a sample from the distribution of the sum of
#' cluster sizes: \eqn{N = N_1 + ... + N_m} over \eqn{m} clusters.
#'
#' @references
#'
#' Gamble, Laura J. (2021) \emph{Estimating the Size of Clustered Hidden
#' Populations}, Ph.D. Thesis, Department of Statistics, Oregon State
#' University.
#'
#' Gile, Krista J. (2011) \emph{Improved Inference for Respondent-Driven
#' Sampling Data With Application to HIV Prevalence Estimation}, Journal of the
#' American Statistical Association, 106, 493, 135-146.
#'
#' Gile, Krista J. and Handcock, Mark S. (2018) \pkg{sspse}: Estimating Hidden
#' Population Size using Respondent Driven Sampling Data. Los Angeles, CA.
#' Version 0.8, \url{http://hpmrg.org}.
#'
#' Handcock, Mark S., Gile, Krista J. and Mar, Corinne M. (2014)
#' \emph{Estimating Hidden Population Size using Respondent-Driven Sampling
#' Data}, Electronic Journal of Statistics, 8, 1, 1491-1521
#'
#' @examples
#'
#' data(csamp)
#' # Make sure data is ordered by sample order
#' csamp <- csamp[order(csamp$samp.order), ]
#'
#' # Separate by cluster
#' csamp1 <- subset(csamp, cluster == 1)
#' csamp2 <- subset(csamp, cluster == 2)
#' csamp3 <- subset(csamp, cluster == 3)
#'
#' # Obtain individual sspse fits using the sspse package
#' # interval, burnin, and samplesize should be larger in a real application.
#' library(sspse)
#' fit1 <- posteriorsize(csamp1$network.size.variable,
#'                       median.prior.size = 1500, K = 100,
#'                       burnin = 100, samplesize = 500, interval = 1)
#' fit2 <- posteriorsize(csamp2$network.size.variable,
#'                       median.prior.size = 750, K = 100,
#'                       burnin = 100, samplesize = 500, interval = 1)
#' fit3 <- posteriorsize(csamp3$network.size.variable,
#'                       median.prior.size = 750, K = 100,
#'                       burnin = 100, samplesize = 500, interval = 1)
#'
#' # Use the Posterior Sum model to combine the individual fits
#' fit <- post.sum(list(fit1, fit2, fit3),
#'                 samplesize = 5000)
#' summary.c(fit)
#' post.plot(fit)
#'
#' @export post.sum

post.sum <- function(sspse.fits, samplesize = 1000){

  m <- length(sspse.fits)
  p.sum <- rep(0, samplesize)
  posts <- sspse.fits

  for(i in 1:samplesize){
    r.draw <- rep(0, m)
    for(j in 1:m){
      r.draw[j] <- sample(sspse.fits[[j]]$sample[,1], 1)
    }

    p.sum[i] <- sum(r.draw)
  }

  return(p.sum)

}

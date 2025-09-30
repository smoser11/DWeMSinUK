#' @title Summary Statistics for Clustered SS-PSE Model Fits
#'
#' @description Prints summary information about the prior and posterior
#' distributions of a given clustered sspse fit.
#'
#' @param fit clustered sspse fit object or a list of individual sspse fits
#' from each cluster or a list of individual sspse fits from each cluster and
#' a sample from the posterior sum distribution.
#' @param support count; the number of equally spaced points at which the posterior
#' densities are estimated.
#' @param HPD.level scalar; probability level of the highest probability
#' density interval determined from the estimated posterior.
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
#' # First, the Extended Bayesian model
#' # interval, burnin, and samplesize should be larger in a real application.
#' fit.eb <- posteriorsize.c(csamp$network.size.variable,
#'                           csamp$cluster,
#'                           median.prior.size = 3000,
#'                           prop.prior.params = rep(1, 3),
#'                           burnin = 100, samplesize = 500, interval = 1)
#'
#' summary.c(fit.eb)
#'
#' # Next, the Posterior Sum model
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
#' fit.ps <- post.sum(list(fit1, fit2, fit3),
#'                    samplesize = 5000)
#'
#' summary.c(list(fit1, fit2, fit3, fit.ps),
#'           HPD.level = 0.9, support = 10000)
#'
#' @export summary.c

summary.c <- function(fit, support = 512, HPD.level=0.95){
  if(class(fit) == "sspse"){
    n.sim <- fit$samplesize
    m <- fit$m
    out <- summary(fit, HPD.level = HPD.level, support = support)
    for(i in 1:m){
      N.temp <- density(fit$sample[,"N"]*fit$sample[,2*m+1+i], n = support)
      N.temp$Fx <- cumsum(N.temp$y/sum(N.temp$y))
      out <- rbind(out, c(round(sum(N.temp$x*N.temp$y)/sum(N.temp$y)),
                          round(N.temp$x[which.min(abs(N.temp$Fx - 0.5))]),
                          round(N.temp$x[which.max(N.temp$y)]),
                          round(N.temp$x[which.min(abs(N.temp$Fx - 0.25))]),
                          round(N.temp$x[which.min(abs(N.temp$Fx - 0.75))]),
                          round(N.temp$x[which.min(abs(N.temp$Fx - 0.9))]),
                          round(N.temp$x[which.min(abs(N.temp$Fx -
                                                         (1-HPD.level)/2))]),
                          round(N.temp$x[which.min(abs(N.temp$Fx -
                                                         (1 - (1-HPD.level)/2)))])))
      rownames(out) <- c(rownames(out)[-(2 + i)], paste("Cluster", i))
    }
    return(out)
  } else if(class(fit) == "list"){
    m <- 0
    for(i in 1:length(fit)){
      m <- m + (class(fit[[i]]) == "sspse")
    }
    if(length(fit) == m){
      out <- summary(fit[[1]], HPD.level = HPD.level, support = support)
      rownames(out) <- c("Cluster 1 Prior", "Cluster 1 Posterior")

      for(i in 2:m){
        out <- rbind(out, summary(fit[[i]], HPD.level = HPD.level,
                                  support = support))
        rownames(out) <- c(rownames(out)[-((2*i - 1):(2*i))],
                           paste("Cluster", i, "Prior"),
                           paste("Cluster", i, "Posterior"))
      }
      return(out)
    } else {
      fit.c <- list()
      count <- 1
      for(i in 1:length(fit)){
        if(class(fit[[i]]) == "sspse"){
          fit.c[[count]] <- fit[[i]]
          count <- count + 1
        } else {
          sum.obj <- fit[[i]]
        }
      }
      n.sim <- length(sum.obj)
      out <- summary(fit.c[[1]], HPD.level = HPD.level, support = support)
      rownames(out) <- c("Cluster 1 Prior", "Cluster 1 Posterior")

      for(i in 2:m){
        out <- rbind(out, summary(fit.c[[i]], HPD.level = HPD.level,
                                  support = support))
        rownames(out) <- c(rownames(out)[-((2*i - 1):(2*i))],
                           paste("Cluster", i, "Prior"),
                           paste("Cluster", i, "Posterior"))
      }

      N.temp <- density(sum.obj, n = support)
      N.temp$Fx <- cumsum(N.temp$y/sum(N.temp$y))
      out <- rbind(c(round(sum(N.temp$x*N.temp$y)/sum(N.temp$y)),
                     round(N.temp$x[which.min(abs(N.temp$Fx - 0.5))]),
                     round(N.temp$x[which.max(N.temp$y)]),
                     round(N.temp$x[which.min(abs(N.temp$Fx - 0.25))]),
                     round(N.temp$x[which.min(abs(N.temp$Fx - 0.75))]),
                     round(N.temp$x[which.min(abs(N.temp$Fx - 0.9))]),
                     round(N.temp$x[which.min(abs(N.temp$Fx -
                                                    (1-HPD.level)/2))]),
                     round(N.temp$x[which.min(abs(N.temp$Fx -
                                                    (1 - (1-HPD.level)/2)))])),
                   out)
      rownames(out) <- c("Posterior", rownames(out)[-1])
      return(out)
    }
  }
}

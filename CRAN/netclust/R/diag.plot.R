#' @title Trace Plots and Density Plots for Clustered SS-PSE Model Fits
#'
#' @description Displays MCMC trace plots and density plots for the population
#' size, the degree distribution parameters mu and sigma, the proportion of the
#' population in each cluster, and the cluster sizes. These plots show the
#' sampled values for each parameter at each iteration of the
#' Metropolis-Hastings algorithm and the overall densities produced. Plots are
#' displayed using the \code{ggplot2} and \code{gridExtra} packages.
#'
#' @param fit clustered sspse fit object or a list of individual sspse fits
#' from each cluster.
#' @param type character string; type of diagnostic plot. Either \code{"trace"}
#' for trace plots or \code{"density"} for density plots.
#' @param titl character string; title of the plot.
#'
#' @references
#'
#' Auguie, B. (2017) \pkg{gridExtra}: Miscellaneous Functions for "Grid"
#' Graphics. Version 2.3. \url{https://CRAN.R-project.org/package=gridExtra}
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
#' Wickham, H. (2016) \pkg{ggplot2}: Elegant Graphics for Data Analysis.
#' Springer-Verlag, New York. \url{https://ggplot2.tidyverse.org}.
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
#' # Investigate the Extended Bayesian fit using MCMC diagnostics
#' diag.plot(fit.eb, type = "trace",
#'           titl = "Trace Plots for the Extended Bayesian Model")
#' diag.plot(fit.eb, type = "density",
#'           titl = "Density Plots for the Extended Bayesian Model")
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
#' # Investigate the individual sspse fits using MCMC diagnostics
#'
#' diag.plot(list(fit1, fit2, fit3), type = "trace",
#'           titl = "Trace Plots for the Posterior Sum Model")
#' diag.plot(list(fit1, fit2, fit3), type = "density",
#'           titl = "Density Plots for the Posterior Sum Model")
#'
#' @export diag.plot

diag.plot <- function(fit, type = c("trace", "density"),
                       titl = NULL){
  type = type[1]
  if(!is.null(fit$m)){
    n.sim <- fit$samplesize
    m <- fit$m

    Ni <- NULL
    for(i in 1:m){
      Ni <- c(Ni, fit$sample[,"N"]*fit$sample[,2*m+1+i])
    }
    plotfit <- data.frame("step" = rep(1:n.sim, m),
                          "cluster" = rep(1:m, rep(n.sim, m)),
                          "N" = rep(as.vector(fit$sample[,1]), m),
                          "mu" = as.vector(fit$sample[,2:(m+1)]),
                          "sigma" = as.vector(fit$sample[,(m+2):(2*m + 1)]),
                          "p" = as.vector(fit$sample[,(2*m + 2):(3*m + 1)]),
                          "cluster.N" = Ni)

    c.labs <- paste("Cluster", 1:m)
    names(c.labs) <- 1:m

    if(type == "trace"){
      p1 <- ggplot(plotfit, aes(x=step, y=N)) + geom_line() +
        ggtitle("Trace of N") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p2 <- ggplot(plotfit, aes(x=step, y=mu)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of mu") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p3 <- ggplot(plotfit, aes(x=step, y=sigma)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of sigma") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p4 <- ggplot(plotfit, aes(x=step, y=p)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of p") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p5 <- ggplot(plotfit, aes(x=step, y=cluster.N)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of Cluster Ns") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    } else if(type == "density"){
      p1 <- ggplot(plotfit, aes(x=N)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        ggtitle("Density of N") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p2 <- ggplot(plotfit, aes(x=mu)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of mu") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p3 <- ggplot(plotfit, aes(x=sigma)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of sigma") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p4 <- ggplot(plotfit, aes(x=p)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of p") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p5 <- ggplot(plotfit, aes(x=cluster.N)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of Cluster Ns") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    }

    grid.arrange(p1, p2, p3, p4, p5,
                 layout_matrix = matrix(c(1, 1, 2, 3, 4, 5), byrow = TRUE,
                                        nrow = 3), top = titl)
  } else {

    m <- length(fit)

    Ni <- NULL
    mui <- NULL
    sigmai <- NULL
    n.sim.v <- NULL
    n.sim <- rep(0, m)
    for(i in 1:m){
      n.sim[i] <- fit[[i]]$samplesize
      n.sim.v <- c(n.sim.v, 1:(n.sim[i]))
      Ni <- c(Ni, fit[[i]]$sample[, "N"])
      mui <- c(mui, fit[[i]]$sample[, "mu"])
      sigmai <- c(sigmai, fit[[i]]$sample[, "sigma"])
    }

    plotfit <- data.frame("step" = n.sim.v,
                          "cluster" = rep(1:m, n.sim),
                          "cluster.N" = Ni,
                          "mu" = mui,
                          "sigma" = sigmai)

    c.labs <- paste("Cluster", 1:m)
    names(c.labs) <- 1:m

    if(type == "trace"){
      p1 <- ggplot(plotfit, aes(x=step, y=cluster.N)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of Cluster Ns") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p2 <- ggplot(plotfit, aes(x=step, y=mu)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of mu") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p3 <- ggplot(plotfit, aes(x=step, y=sigma)) + geom_line() +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Trace of sigma") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    } else if(type == "density"){
      p1 <- ggplot(plotfit, aes(x=cluster.N)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of Cluster Ns") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p2 <- ggplot(plotfit, aes(x=mu)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of mu") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      p3 <- ggplot(plotfit, aes(x=sigma)) +
        geom_density(fill = "dodgerblue4", alpha = 0.5) +
        facet_wrap(~cluster, labeller = labeller(cluster = c.labs)) +
        ggtitle("Density of sigma") + ylab("") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    }

    grid.arrange(p1, p2, p3,
                 layout_matrix = matrix(c(1, 1, 2, 3), byrow = TRUE, nrow = 2),
                 top = titl)
  }
}

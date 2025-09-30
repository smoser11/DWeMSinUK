#' @title Plot the Posterior Distributions Given by the Extended Bayesian
#' and Posterior Sum Models
#'
#' @description Plots the posterior distributions for overall population size,
#' \eqn{P(N | Data)}; for the marginal population proportions, \eqn{P(p_i | Data)}
#' in each cluster \eqn{i}; and for the marginal cluster sizes,
#' \eqn{P(N_i | Data)} in each cluster \eqn{i}.
#'
#' @param sspse.obj when using the Extended Bayesian model, a clustered sspse
#' fit object for plot types \code{N}, \code{p}, or \code{N.c}. When using the
#' Posterior Sum model, a vector of draws from the posterior sum distribution
#' for plot type \code{N} and a list of individual sspse fit objects for plot
#' type \code{N.c}.
#' @param type character string; type of posterior plot. Either \code{"N"} for
#' overall population size, \code{"p"} for population proportions, or
#' \code{"N.c"} for cluster sizes.
#' @param band.post scalar; multiplication factor for the smoothing bandwidth
#' used in kernel density estimation.
#' @param n count; sample size of the RDS sample. This can be extracted from
#' sspse fit objects.
#' @param zoom upper x-axis limit given as a quantile of the posterior
#' distribution.
#' @param nrow count; number of rows passed to \code{\link[ggplot2]{facet_wrap}}.
#' @param titl character string; title of the plot.
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
#' post.plot(fit.eb, type = "N",
#'           titl = "Posterior for Overall Size: Extended Bayesian")
#' post.plot(fit.eb, type = "p")
#' post.plot(fit.eb, type = "N.c", zoom = 1, nrow = 3)
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
#' post.plot(fit.ps, type = "N", n = 1500, zoom = 1)
#' post.plot(list(fit1, fit2, fit3), type = "N.c", zoom = 1, nrow = 3)
#'
#' @export post.plot

post.plot <- function(sspse.obj, type = c("N", "p", "N.c"),
                      band.post = 1, n = NULL, zoom = 0.975, nrow = NULL,
                      titl = "Posterior for Population Size"){
  type <- type[1]
  if(type == "N"){
    if(class(sspse.obj) == "sspse"){
      n = sspse.obj$n
      mcmc.n <- sspse.obj$samplesize

      prior <- exp(sspse.obj$lpriorm - max(sspse.obj$lpriorm))
      denom <- sum(prior[((0:99) + (1:100))/200*length(prior)]*length(prior)/100)
      prior.n <- prior/denom

      sumry <- summary(sspse.obj, HPD.level = 0.9)
      df <- data.frame("x" =  n:(length(prior) + n - 1),
                       "Prior" = prior.n)
      df2 <- data.frame("Median" = sumry$Median[2],
                        "Mean" = sumry$Mean[2],
                        "5P" = sumry$'5%'[2],
                        "95P" = sumry$'95%'[2],
                        "Prior.Median" = sumry$Median[1])
      dens <- data.frame("x" = density(sspse.obj$sample[,"N"],
                                       adjust = band.post, from = n)$x,
                         "y" = density(sspse.obj$sample[,"N"],
                                       adjust = band.post, from = n)$y)

      dens$Fx <- cumsum(dens$y/sum(dens$y))
      x.lim <- dens[which.min(abs(dens$Fx - zoom)), "x"]

      y.lim <- max(dens$y, df$Prior)

      labs.c <- c("Posterior" = "black", "Prior" = "black",
                  "Posterior Median" = "darkred", "Posterior Mean" = "darkgreen",
                  "Prior Median" = "black")
      labs.l <- c("Posterior" = 1, "Prior" = 2,
                  "Posterior Median" = 1, "Posterior Mean" = 1, "Prior Median" = 3)
      labs.txt <- data.frame("x" = c(n, df2$X5P, df2$Median, df2$Mean,
                                     df2$X95P, df2$Prior.Median),
                             "y" = rep(-.05*y.lim, 6),
                             "label" = round(c(n, df2$X5P, df2$Median, df2$Mean,
                                               df2$X95P,
                                               df2$Prior.Median)))

      p.plot <- ggplot(df, aes(x = x, y = Prior)) +
        geom_line(aes(color = 'Prior', lty = 'Prior')) +
        geom_line(data = dens, aes(x = x, y = y, color = 'Posterior',
                                   lty = 'Posterior')) +
        geom_segment(data = dens, aes(x = n, y = 0,
                                      xend = n, yend = y[1],
                                      color = 'Posterior',
                                      lty = 'Posterior')) +
        geom_area(data = subset(dens, (x >= df2$X5P & x <= df2$X95P)),
                  aes(x = x, y = y, fill = '90% Interval')) +
        geom_segment(aes(x = n, y = 0, xend = x.lim, yend = 0)) +
        geom_segment(data = df2, aes(x = Median, y = 0,
                                     xend = Median, yend = 1.1*y.lim,
                                     color = 'Posterior Median',
                                     lty = 'Posterior Median'), size = 1) +
        geom_segment(data = df2, aes(x = Mean, y = 0,
                                     xend = Mean, yend = 1.1*y.lim,
                                     color = 'Posterior Mean',
                                     lty = 'Posterior Mean'), size = 1) +
        geom_segment(data = df2, aes(x = Prior.Median, y = 0,
                                     xend = Prior.Median, yend = 1.1*y.lim,
                                     color = 'Prior Median',
                                     lty = 'Prior Median'), size = 1) +
        xlim(c(0, x.lim)) +
        scale_colour_manual(name = "",
                            breaks = names(labs.c),
                            values = labs.c) +
        scale_linetype_manual(name = "",
                              breaks = names(labs.l),
                              values = labs.l) +
        scale_fill_manual(name = "", labels = "90% Interval",
                          values = alpha("dodgerblue4", 0.5)) +
        geom_text(data = labs.txt, aes(x = x, y = y, label = label), size = 2,
                  angle = 45, color = c("black", "dodgerblue4", "darkred", "darkgreen",
                                        "dodgerblue4", "black")) +
        xlab("Population Size") + ylab("Density") +
        ggtitle(titl) + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      if(class(sspse.obj) == "list"){
        stop("Plot type N is available for Extended Bayesian fits if given an sspse object
             and Posterior Sum fits if given a vector of random draws from the posterior
             sum distribution",
             call. = FALSE)
      }
      if(is.null(n)){
        warning("Sample size n not provided. The density will not represent the true minimum of n",
                call. = FALSE)

        sum.obj <- sspse.obj
        mcmc.n <- length(sum.obj)

        df2 <- data.frame("Median" = median(sum.obj),
                          "Mean" = mean(sum.obj),
                          "5P" = sort(sum.obj)[0.05*mcmc.n],
                          "95P" = sort(sum.obj)[0.95*mcmc.n])
        dens <- data.frame("x" = density(sum.obj,
                                         adjust = band.post, from = 0)$x,
                           "y" = density(sum.obj,
                                         adjust = band.post, from = 0)$y)

        dens$Fx <- cumsum(dens$y/sum(dens$y))
        x.lim <- dens[which.min(abs(dens$Fx - zoom)), "x"]

        y.lim <- max(dens$y)

        labs.c <- c("Posterior" = "black",
                    "Posterior Median" = "darkred",
                    "Posterior Mean" = "darkgreen")
        labs.l <- c("Posterior" = 1,
                    "Posterior Median" = 1,
                    "Posterior Mean" = 1)
        labs.txt <- data.frame("x" = c(df2$X5P, df2$Median, df2$Mean,
                                       df2$X95P),
                               "y" = rep(-.05*y.lim, 4),
                               "label" = round(c(df2$X5P, df2$Median, df2$Mean,
                                                 df2$X95P)))

        p.plot <- ggplot(dens, aes(x = x, y = y)) +
          geom_line(aes(x = x, y = y, color = 'Posterior',
                        lty = 'Posterior')) +
          geom_segment(aes(x = 0, y = 0,
                           xend = 0, yend = y[1],
                           color = 'Posterior',
                           lty = 'Posterior')) +
          geom_area(data = subset(dens, (x >= df2$X5P & x <= df2$X95P)),
                    aes(x = x, y = y, fill = '90% Interval')) +
          geom_segment(aes(x = 0, y = 0, xend = x.lim, yend = 0)) +
          geom_segment(data = df2, aes(x = Median, y = 0,
                                       xend = Median, yend = 1.1*y.lim,
                                       color = 'Posterior Median',
                                       lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df2, aes(x = Mean, y = 0,
                                       xend = Mean, yend = 1.1*y.lim,
                                       color = 'Posterior Mean',
                                       lty = 'Posterior Mean'), size = 1) +
          xlim(c(0, x.lim)) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label), size = 2,
                    angle = 45, color = c("dodgerblue4", "darkred", "darkgreen",
                                          "dodgerblue4")) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      } else {

        sum.obj <- sspse.obj
        mcmc.n <- length(sum.obj)

        df2 <- data.frame("Median" = median(sum.obj),
                          "Mean" = mean(sum.obj),
                          "5P" = sort(sum.obj)[0.05*mcmc.n],
                          "95P" = sort(sum.obj)[0.95*mcmc.n])
        dens <- data.frame("x" = density(sum.obj,
                                         adjust = band.post, from = n)$x,
                           "y" = density(sum.obj,
                                         adjust = band.post, from = n)$y)

        x.lim <- sort(sum.obj)[zoom*mcmc.n]

        y.lim <- max(dens$y)

        labs.c <- c("Posterior" = "black",
                    "Posterior Median" = "darkred",
                    "Posterior Mean" = "darkgreen")
        labs.l <- c("Posterior" = 1,
                    "Posterior Median" = 1,
                    "Posterior Mean" = 1)
        labs.txt <- data.frame("x" = c(n, df2$X5P, df2$Median, df2$Mean,
                                       df2$X95P),
                               "y" = rep(-.05*y.lim, 5),
                               "label" = round(c(n, df2$X5P, df2$Median, df2$Mean,
                                                 df2$X95P)))

        p.plot <- ggplot(dens, aes(x = x, y = y)) +
          geom_line(aes(x = x, y = y, color = 'Posterior',
                        lty = 'Posterior')) +
          geom_segment(aes(x = n, y = 0,
                           xend = n, yend = y[1],
                           color = 'Posterior',
                           lty = 'Posterior')) +
          geom_area(data = subset(dens, (x >= df2$X5P & x <= df2$X95P)),
                    aes(x = x, y = y, fill = '90% Interval')) +
          geom_segment(aes(x = n, y = 0, xend = x.lim, yend = 0)) +
          geom_segment(data = df2, aes(x = Median, y = 0,
                                       xend = Median, yend = 1.1*y.lim,
                                       color = 'Posterior Median',
                                       lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df2, aes(x = Mean, y = 0,
                                       xend = Mean, yend = 1.1*y.lim,
                                       color = 'Posterior Mean',
                                       lty = 'Posterior Mean'), size = 1) +
          xlim(c(0, x.lim)) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label), size = 2,
                    angle = 45, color = c("black", "dodgerblue4", "darkred",
                                          "darkgreen", "dodgerblue4")) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
  } else if(type == "p"){
    if(class(sspse.obj) == "sspse"){
      m <- sspse.obj$m

      x <- seq(0, 1, by = 0.01)
      prior.df <- data.frame("x" = NULL,
                             "f_x" = NULL,
                             "Variable" = NULL)
      sumry.df <- data.frame("Median" = NULL,
                             "Mean" = NULL,
                             "5P" = NULL,
                             "95P" = NULL,
                             "Prior.Median" = NULL,
                             "Variable" = NULL)
      post.df <- data.frame("x" = NULL,
                            "f_x" = NULL,
                            "Variable" = NULL)
      post.df.area <- data.frame("x" = NULL,
                                 "f_x" = NULL,
                                 "Variable" = NULL)
      alph <- sspse.obj$concent*sspse.obj$propsprior
      for(j in 1:m){
        prior.df <- rbind(prior.df,
                          data.frame("x" = x,
                                     "f_x" = dbeta(x,
                                                   shape1 = alph[j],
                                                   shape2 = sum(alph[-j])),
                                     "Variable" = paste("Cluster", j)))
        sumry.df <- rbind(sumry.df,
                          data.frame("Median" = median(sspse.obj$sample[,paste("p", j)]),
                                     "Mean" = mean(sspse.obj$sample[,paste("p", j)]),
                                     "5P" = quantile(sspse.obj$sample[,paste("p", j)],
                                                     probs = 0.05),
                                     "95P" = quantile(sspse.obj$sample[,paste("p", j)],
                                                      probs = 0.95),
                                     "Prior.Mean" = sspse.obj$propsprior[j],
                                     "Variable" = paste("Cluster", j)))
        post.temp <- data.frame("x" = density(sspse.obj$sample[,paste("p", j)],
                                              adjust = band.post,
                                              from = 0,
                                              to = 1)$x,
                                "f_x" = density(sspse.obj$sample[,paste("p", j)],
                                                adjust = band.post,
                                                from = 0,
                                                to = 1)$y,
                                "Variable" = paste("Cluster", j))
        post.df <- rbind(post.df, post.temp)

        post.df.area <- rbind(post.df.area, subset(post.temp,
                                                   x > sumry.df[j, "X5P"] &
                                                     x < sumry.df[j, "X95P"]))
      }

      y.lim <- max(post.df$f_x, prior.df$f_x)

      labs.c <- c("Posterior" = "black", "Prior" = "black",
                  "Posterior Median" = "darkred", "Posterior Mean" = "darkgreen",
                  "Prior Mean" = "black")
      labs.l <- c("Posterior" = 1, "Prior" = 2,
                  "Posterior Median" = 1, "Posterior Mean" = 1, "Prior Mean" = 3)

      if(is.null(nrow)){
        p.plot <- ggplot(prior.df, aes(x = x, y = f_x)) +
          facet_wrap(~Variable) +
          geom_line(aes(color = 'Prior', lty = 'Prior')) +
          geom_line(data = post.df, aes(x = x, y = f_x, color = 'Posterior',
                                        lty = 'Posterior')) +
          geom_area(data = post.df.area,
                    aes(x = x, y = f_x, fill = '90% Interval')) +
          geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
          geom_segment(data = sumry.df, aes(x = Median, y = 0,
                                            xend = Median, yend = 1.1*y.lim,
                                            color = 'Posterior Median',
                                            lty = 'Posterior Median'), size = 1) +
          geom_segment(data = sumry.df, aes(x = Mean, y = 0,
                                            xend = Mean, yend = 1.1*y.lim,
                                            color = 'Posterior Mean',
                                            lty = 'Posterior Mean'), size = 1) +
          geom_segment(data = sumry.df, aes(x = Prior.Mean, y = 0,
                                            xend = Prior.Mean, yend = 1.1*y.lim,
                                            color = 'Prior Mean',
                                            lty = 'Prior Mean'), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlab("Population Proportion") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p.plot <- ggplot(prior.df, aes(x = x, y = f_x)) +
          facet_wrap(~Variable, nrow = nrow) +
          geom_line(aes(color = 'Prior', lty = 'Prior')) +
          geom_line(data = post.df, aes(x = x, y = f_x, color = 'Posterior',
                                        lty = 'Posterior')) +
          geom_area(data = post.df.area,
                    aes(x = x, y = f_x, fill = '90% Interval')) +
          geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
          geom_segment(data = sumry.df, aes(x = Median, y = 0,
                                            xend = Median, yend = 1.1*y.lim,
                                            color = 'Posterior Median',
                                            lty = 'Posterior Median'), size = 1) +
          geom_segment(data = sumry.df, aes(x = Mean, y = 0,
                                            xend = Mean, yend = 1.1*y.lim,
                                            color = 'Posterior Mean',
                                            lty = 'Posterior Mean'), size = 1) +
          geom_segment(data = sumry.df, aes(x = Prior.Mean, y = 0,
                                            xend = Prior.Mean, yend = 1.1*y.lim,
                                            color = 'Prior Mean',
                                            lty = 'Prior Mean'), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlab("Population Proportion") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else {
      stop("Plot type p is only available for Extended Bayesian fits",
            call. = FALSE)
    }

  } else if(type == "N.c"){
    if(class(sspse.obj) == "sspse"){
      n <- sspse.obj$nc
      m <- sspse.obj$m
      mcmc.n <- sspse.obj$samplesize
      Ni <- NULL
      for(i in 1:m){
        Ni <- c(Ni, sspse.obj$sample[,"N"]*sspse.obj$sample[,2*m+1+i])
      }
      plotfit <- data.frame("cluster" = rep(1:m, rep(mcmc.n, m)),
                            "Ni" = Ni)

      sumry <- summary.c(sspse.obj, HPD.level = 0.9)

      df <- data.frame("Median" = sumry$Median[3:(2+m)],
                       "5P" = sumry$'5%'[3:(2+m)],
                       "95P" = sumry$'95%'[3:(2+m)],
                       "Mean" = sumry$Mean[3:(2+m)],
                       "clust" = 1:m)

      dens <- data.frame("x" = NULL,
                         "y" = NULL,
                         "clust" = NULL)

      maxN <- max(plotfit$Ni)
      for(i in 1:m){
        dens <- rbind(dens, data.frame("x" = density(plotfit$Ni[plotfit$cluster == i],
                                                     adjust = band.post, from = n[i],
                                                     to = maxN)$x,
                                       "y" = density(plotfit$Ni[plotfit$cluster == i],
                                                     adjust = band.post, from = n[i],
                                                     to = maxN)$y,
                                       "clust" = i))
      }

      x.lim <- rep(0, m)
      y.lim <- rep(0, m)
      dens.area <- dens
      for(i in 1:m){
        temp <- subset(dens, clust == i)
        temp$Fx <- cumsum(temp$y/sum(temp$y))
        x.lim[i] <- temp[which.min(abs(temp$Fx - zoom)), "x"]
        y.lim[i] <- max(temp$y)
        #dens <- dens[!(dens$clust == i & dens$x > x.lim[i]), ]
        dens.area <- dens.area[!(dens.area$clust == i & (dens.area$x < df$X5P[i] |
                                                           dens.area$x > df$X95P[i])), ]
      }
      x.lim.plot <- max(x.lim)
      y.lim.plot <- max(y.lim)

      labs.c <- c("Posterior" = "black",
                  "Posterior Median" = "darkred", "Posterior Mean" = "darkgreen")
      labs.l <- c("Posterior" = 1,
                  "Posterior Median" = 1, "Posterior Mean" = 1)

      labs.txt <- data.frame("x" = c(n, df$X5P, df$Median,
                                     df$X95P, df$Mean),
                             "y" = rep(-.05*y.lim.plot, 5),
                             "label" = round(c(n, df$X5P, df$Median,
                                               df$X95P,
                                               df$Mean)),
                             "clust" = rep(1:m, 5))
      labs.txt <- labs.txt[order(labs.txt$clust),]
      c.labs <- paste("Cluster", 1:m)
      names(c.labs) <- 1:m
      n.segs <- data.frame("n" = NULL,
                           "clust" = NULL,
                           "yend" = NULL)
      for(i in 1:m){
        n.segs <- rbind(n.segs, data.frame("n" = n[i], "clust" = i,
                                           "yend" = subset(dens, clust == i)$y[1]))
      }
      n.segs$xlim <- x.lim.plot
      df$ylim <- y.lim.plot*1.1

      if(is.null(nrow)){
        p.plot <- ggplot(dens, aes(x = x, y = y, group = clust)) +
          facet_wrap(~clust, labeller = labeller(clust = c.labs)) +
          geom_area(data = dens.area,
                    aes(x = x, y = y, fill = '90% Interval', group = clust)) +
          geom_segment(data = n.segs, aes(x = n, y = 0, xend = xlim, yend = 0,
                                          group = clust)) +
          geom_segment(data = n.segs, aes(x = n, y = 0,
                                          xend = n, yend = yend, group = "clust")) +
          geom_line() +
          geom_segment(data = df, aes(x = Median, y = 0,
                                      xend = Median, yend = ylim,
                                      color = 'Posterior Median',
                                      lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df, aes(x = Mean, y = 0,
                                      xend = Mean, yend = ylim,
                                      color = 'Posterior Mean',
                                      lty = "Posterior Mean"), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlim(c(0, x.lim.plot)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label, group = clust), size = 2,
                    angle = 45, color = rep(c("black", "dodgerblue4", "darkred",
                                              "dodgerblue4", "darkgreen"), m)) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p.plot <- ggplot(dens, aes(x = x, y = y, group = clust)) +
          facet_wrap(~clust, nrow = nrow,
                     labeller = labeller(clust = c.labs)) +
          geom_area(data = dens.area,
                    aes(x = x, y = y, fill = '90% Interval', group = clust)) +
          geom_segment(data = n.segs, aes(x = n, y = 0, xend = xlim, yend = 0,
                                          group = clust)) +
          geom_segment(data = n.segs, aes(x = n, y = 0,
                                          xend = n, yend = yend, group = "clust")) +
          geom_line() +
          geom_segment(data = df, aes(x = Median, y = 0,
                                      xend = Median, yend = ylim,
                                      color = 'Posterior Median',
                                      lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df, aes(x = Mean, y = 0,
                                      xend = Mean, yend = ylim,
                                      color = 'Posterior Mean',
                                      lty = "Posterior Mean"), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlim(c(0, x.lim.plot)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label, group = clust), size = 2,
                    angle = 45, color = rep(c("black", "dodgerblue4", "darkred",
                                              "dodgerblue4", "darkgreen"), m)) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if(class(sspse.obj) == "list") {
      sspse.obj.c <- list()
      count <- 1
      for(i in 1:length(sspse.obj)){
        if(class(sspse.obj[[i]]) == "sspse"){
          sspse.obj.c[[count]] <- sspse.obj[[i]]
          count <- count + 1
        }
      }
      m <- count - 1
      sspse.obj <- sspse.obj.c

      n <- rep(0, m)
      for(i in 1:m){
        n[i] <- sspse.obj[[i]]$n
      }

      mcmc.n <- rep(0, m)
      Ni <- NULL
      for(i in 1:m){
        Ni <- c(Ni, sspse.obj[[i]]$sample[,"N"])
        mcmc.n[i] <- sspse.obj[[i]]$samplesize
      }
      plotfit <- data.frame("cluster" = rep(1:m, mcmc.n),
                            "Ni" = Ni)

      df <- data.frame("x" = NULL, "Prior" = NULL, "cluster" = NULL)
      for(i in 1:m){
        prior <- exp(sspse.obj[[i]]$lpriorm - max(sspse.obj[[i]]$lpriorm))
        denom <- sum(prior[((0:99) + (1:100))/200*length(prior)]*length(prior)/100)
        prior.n <- prior/denom

        df <- rbind(df, data.frame("x" = n[i]:(length(prior) + n[i] - 1),
                                   "Prior" = prior.n,
                                   "cluster" = i))
      }

      sumry <- summary.c(sspse.obj, HPD.level = 0.9)
      df2 <- data.frame("Median" = sumry$Median[seq(2, 2*m, by = 2)],
                        "5P" = sumry$'5%'[seq(2, 2*m, by = 2)],
                        "95P" = sumry$'95%'[seq(2, 2*m, by = 2)],
                        "Mean" = sumry$Mean[seq(2, 2*m, by = 2)],
                        "Prior Median" = sumry$Median[seq(1, 2*m, by = 2)],
                        "cluster" = 1:m)

      maxN <- max(plotfit$Ni, sumry[,8])

      dens <- data.frame("x" = density(plotfit$Ni[plotfit$cluster == 1],
                                       adjust = band.post, from = n[1],
                                       to = maxN)$x,
                         "y" = density(plotfit$Ni[plotfit$cluster == 1],
                                       adjust = band.post, from = n[1],
                                       to = maxN)$y,
                         "cluster" = 1)
      for(i in 2:m){
        temp <- data.frame("x" = density(plotfit$Ni[plotfit$cluster == i],
                                         adjust = band.post, from = n[i],
                                         to = maxN)$x,
                           "y" = density(plotfit$Ni[plotfit$cluster == i],
                                         adjust = band.post, from = n[i],
                                         to = maxN)$y,
                           "cluster" = i)
        dens <- rbind(dens, temp)
      }

      x.lim <- rep(0, m)
      y.lim <- rep(0, m)
      dens.area <- dens
      for(i in 1:m){
        temp <- subset(dens, cluster == i)
        temp$Fx <- cumsum(temp$y/sum(temp$y))
        x.lim[i] <- temp[which.min(abs(temp$Fx - zoom)), "x"]
        y.lim[i] <- max(temp$y, subset(df, cluster == i)$Prior)
        #dens <- dens[!(dens$cluster == i & dens$x > x.lim[i]), ]
        #df <- df[!(df$cluster == i & df$x > x.lim[i]), ]
        dens.area <- dens.area[!(dens.area$cluster == i & (dens.area$x < df2$X5P[i] |
                                                             dens.area$x > df2$X95P[i])), ]
      }

      x.lim.plot <- max(x.lim)
      y.lim.plot <- max(y.lim)


      labs.c <- c("Posterior" = "black", "Prior" = "black",
                  "Posterior Median" = "darkred", "Posterior Mean" = "darkgreen",
                  "Prior Median" = "black")
      labs.l <- c("Posterior" = 1, "Prior" = 2,
                  "Posterior Median" = 1, "Posterior Mean" = 1, "Prior Median" = 3)

      labs.txt <- data.frame("x" = c(n, df2$X5P, df2$Median, df2$Mean,
                                     df2$X95P, df2$Prior.Median),
                             "y" = rep(-.05*y.lim.plot, 6),
                             "label" = round(c(n, df2$X5P, df2$Median, df2$Mean,
                                               df2$X95P,
                                               df2$Prior.Median)),
                             "cluster" = rep(1:m, 6))
      labs.txt <- labs.txt[order(labs.txt$cluster),]
      c.labs <- paste("Cluster", 1:m)
      names(c.labs) <- 1:m
      n.segs <- data.frame("n" = n[1], "cluster" = 1,
                           "yend" = subset(dens, cluster == 1)$y[1])
      for(i in 2:m){
        n.segs <- rbind(n.segs, data.frame("n" = n[i], "cluster" = i,
                                           "yend" = subset(dens, cluster == i)$y[1]))
      }
      n.segs$xlim <- x.lim.plot
      df2$ylim <- y.lim.plot*1.1

      if(is.null(nrow)){
        p.plot <- ggplot(df, aes(x = x, y = Prior, group = "cluster")) +
          facet_wrap(~cluster, labeller = labeller(cluster = c.labs))+
          geom_line(aes(color = 'Prior', lty = 'Prior')) +
          geom_line(data = dens, aes(x = x, y = y, color = 'Posterior',
                                     lty = 'Posterior')) +
          geom_area(data = dens.area,
                    aes(x = x, y = y, fill = '90% Interval', group = cluster)) +
          geom_segment(data = n.segs, aes(x = n, y = 0, xend = xlim, yend = 0,
                                          group = cluster)) +
          geom_segment(data = n.segs, aes(x = n, y = 0,
                                          xend = n, yend = yend, group = "cluster")) +
          geom_segment(data = df2, aes(x = Median, y = 0,
                                       xend = Median, yend = ylim,
                                       color = 'Posterior Median',
                                       lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df2, aes(x = Mean, y = 0,
                                       xend = Mean, yend = ylim,
                                       color = 'Posterior Mean',
                                       lty = 'Posterior Mean'), size = 1) +
          geom_segment(data = df2, aes(x = Prior.Median, y = 0,
                                       xend = Prior.Median, yend = ylim,
                                       color = 'Prior Median',
                                       lty = 'Prior Median'), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlim(c(0, x.lim.plot)) +
          scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label, group = cluster), size = 2,
                    angle = 45, color = rep(c("black", "dodgerblue4", "darkred", "darkgreen",
                                              "dodgerblue4", "black"), m)) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p.plot <- ggplot(df, aes(x = x, y = Prior, group = "cluster")) +
          facet_wrap(~cluster, nrow = nrow,
                     labeller = labeller(cluster = c.labs))+
          geom_line(aes(color = 'Prior', lty = 'Prior')) +
          geom_line(data = dens, aes(x = x, y = y, color = 'Posterior',
                                     lty = 'Posterior')) +
          geom_area(data = dens.area,
                    aes(x = x, y = y, fill = '90% Interval', group = cluster)) +
          geom_segment(data = n.segs, aes(x = n, y = 0, xend = xlim, yend = 0,
                                          group = cluster)) +
          geom_segment(data = n.segs, aes(x = n, y = 0,
                                          xend = n, yend = yend, group = "cluster")) +
          geom_segment(data = df2, aes(x = Median, y = 0,
                                       xend = Median, yend = ylim,
                                       color = 'Posterior Median',
                                       lty = 'Posterior Median'), size = 1) +
          geom_segment(data = df2, aes(x = Mean, y = 0,
                                       xend = Mean, yend = ylim,
                                       color = 'Posterior Mean',
                                       lty = 'Posterior Mean'), size = 1) +
          geom_segment(data = df2, aes(x = Prior.Median, y = 0,
                                       xend = Prior.Median, yend = ylim,
                                       color = 'Prior Median',
                                       lty = 'Prior Median'), size = 1) +
          scale_colour_manual(name = "",
                              breaks = names(labs.c),
                              values = labs.c) +
          scale_linetype_manual(name = "",
                                breaks = names(labs.l),
                                values = labs.l) +
          scale_fill_manual(name = "", labels = "90% Interval",
                            values = alpha("dodgerblue4", 0.5)) +
          xlim(c(0, x.lim.plot)) +
          scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
          geom_text(data = labs.txt, aes(x = x, y = y, label = label, group = cluster), size = 2,
                    angle = 45, color = rep(c("black", "dodgerblue4", "darkred", "darkgreen",
                                              "dodgerblue4", "black"), m)) +
          xlab("Population Size") + ylab("Density") +
          ggtitle(titl) + theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else {
      stop("Plot type N.c is only available for Extended Bayesian sspse fits or lists
           of sspse fits in each cluster", call. = FALSE)
    }
  } else {
    stop("Plot type must be N, p, or N.c", call. = FALSE)
  }

  suppressWarnings(print(p.plot))
}

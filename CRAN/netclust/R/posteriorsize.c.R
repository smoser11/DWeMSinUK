#' @title Estimate the Size of a Clustered Hidden Population Using RDS Data:
#' The Extended Bayesian Model
#'
#' @description Computes the joint posterior distribution of the population size
#' and other model parameters for clustered populations using data collected by
#' Respondent Driven Sampling. This method is an extension of the Successive
#' Sampling Population Size Estimation approach:
#' \code{\link[sspse]{posteriorsize}}.
#'
#' Based on HPMRG project software (http://hpmrg.org).
#' For license and citation information see http://hpmrg.org/attribution
#'
#' @param s either a vector of integers or an \code{rds.data.frame} providing network
#' size information with a column indicating cluster membership.
#' @param c vector of cluster assignments for each member in the sample. If an
#' \code{rds.data.frame} is passed, this can instead be the name of the column
#' in the \code{rds.data.frame} indicating cluster membership.
#' @param median.prior.size scalar; A hyperparameter being the median of the
#' prior distribution on the population size.
#' @param interval count; the number of proposals between sampled statistics.
#' @param burnin count; the number of proposals before any MCMC sampling is
#' done. It typically is set to a fairly large number.
#' @param maxN integer; maximum possible population size. By default this is
#' determined from an upper quantile of the prior distribution.
#' @param K count; the maximum degree for an individual. This is usually
#' calculated as \code{round(stats::quantile(s,0.80))}.
#' @param samplesize count; the number of Monte-Carlo samples to draw to
#' compute the posterior. This is the number returned by the
#' Metropolis-Hastings algorithm.The default is 1000.
#' @param quartiles.prior.size vector of length 2; A pair of hyperparameters
#' being the lower and upper quartiles of the prior distribution on the
#' population size. For example, \cr \code{quartiles.prior.size=c(1000,4000)}
#' corresponds to a prior where the lower quartile (25\%) is 1000 and the upper
#' (75\%) is 4000.
#' @param mean.prior.size scalar; A hyperparameter being the mean of the prior
#' distribution on the population size.
#' @param mode.prior.size scalar; A hyperparameter being the mode of the prior
#' distribution on the population size.
#' @param priorsizedistribution character; the type of parametric distribution
#' to use for the prior on population size. The options are \code{beta} (for a
#' Beta prior on the sample proportion (i.e. \eqn{n/N})), \code{flat}
#' (uniform), \code{nbinom} (Negative-Binomial), and \code{pln}
#' (Poisson-log-normal). The default is \code{beta}.
#' @param effective.prior.df scalar; A hyperparameter being the effective
#' number of samples worth of information represented in the prior distribution
#' on the population size. By default this is 1, but it can be greater (or
#' less!) to allow for different levels of uncertainty.
#' @param sd.prior.size scalar; A hyperparameter being the standard deviation
#' of the prior distribution on the population size.
#' @param mode.prior.sample.proportion scalar; A hyperparameter being the mode
#' of the prior distribution on the sample proportion \eqn{n/N}.
#' @param alpha scalar; A hyperparameter being the first parameter of the beta
#' prior model for the sample proportion. By default this is NULL, meaning that
#' 1 is chosen. it can be any value at least 1 to allow for different levels of
#' uncertainty.
#' @param mean.prior.degree vector; A hyper parameter being the mean of
#' the unit size distribution in each cluster.
#' @param sd.prior.degree vector; A hyper parameter being the standard deviation
#' of the unit size distribution in each cluster.
#' @param max.sd.prior.degree vector; The maximum allowed value of
#' \code{sd.prior.degree}. If the passed or computed value is higher, it is
#' reduced to this value. This is done for numerical stability reasons.
#' @param df.mean.prior scalar; A hyper parameter being the degrees-of-freedom
#' of the prior for the mean. This gives the equivalent sample size that would
#' contain the same amount of information inherent in the prior.
#' @param df.sd.prior scalar; A hyper parameter being the degrees-of-freedom of
#' the prior for the standard deviation. This gives the equivalent sample size
#' that would contain the same amount of information inherent in the prior for
#' the standard deviation.
#' @param nk vector; the vector of counts for the number of people in the
#' sample with degree k. This is usually computed from \eqn{s} automatically as
#' \code{tabulate(s,nbins=K)} and not usually specified by the user.
#' @param n integer; the number of people in the sample. This is usually computed from
#' \eqn{s} automatically and not usually specified by the user.
#' @param muproposal scalar; The standard deviation of the proposal
#' distribution for the mean degree.
#' @param sigmaproposal scalar; The standard deviation of the proposal
#' distribution for the standard deviation of the degree.
#' @param burnintheta count; the number of proposals in the Metropolis-Hastings
#' sub-step for the degree distribution parameters (\eqn{\theta}) before any
#' MCMC sampling is done. It typically is set to a modestly large number.
#' @param seed integer; random number integer seed.  Defaults to \code{NULL} to
#' use whatever the state of the random number generator is at the time of the
#' call.
#' @param maxbeta scalar; The maximum allowed value of the \code{beta} parameter.
#' If the implied or computed value is higher, it is reduced to this value.
#' This is done for numerical stability reasons.
#' @param supplied list; If supplied, is a list with components \code{maxN} and
#' \code{sample}. In this case \code{supplied} is a matrix with a column named
#' \code{N} being a sample from a prior distribution for the population size.
#' The value \code{maxN} specifies the maximum value of the population size, a
#' priori.
#' @param max.coupons The number of recruitment coupons distributed to each
#' enrolled subject (i.e. the maximum number of recruitees for any subject).
#' By default it is taken by the attribute or data, else the maximum recorded number of coupons.
#' @param recruit.time vector; An optional value for the data/time that the person was interviewed.
#' It needs to resolve as a numeric vector with number of elements the number
#' of rows of the data with non-missing values of the network variable. If it
#' is a character name of a variable in the data then that variable is used.
#' If it is NULL then the sequence number of the recruit in the data is used.
#' If it is NA then the recruitment is not used in the model.
#' Otherwise, the recruitment time is used in the model to better predict the visibility of the person.
#' @param include.tree logical; If \code{TRUE},
#' augment the reported network size by the number of recruits and one for the recruiter (if any).
#' This reflects a more accurate value for the visibility, but is not the self-reported degree.
#' In particular, it typically produces a positive visibility (compared to a possibility zero self-reported degree).
#' @param unit.scale numeric; If not \code{NULL} it sets the numeric value of the scale parameter
#' of the distribution of the unit sizes.
#' For the negative binomial, it is the multiplier on the variance of the negative binomial
#' compared to a Poisson (via the Poisson-Gamma mixture representation). Sometimes the scale is
#' unnaturally large (e.g. 40) so this give the option of fixing it (rather than using
#' the MLE of it). The model is fit with the parameter fixed at this passed value.
#' @param reflect.time logical; If \code{FALSE} then the \code{recruit.time} is the time before the
#' end of the study (instead of the time since the survey started or chronological time).
#' @param verbose logical; if this is \code{TRUE}, the program will print out
#' additional information, including goodness of fit statistics.
#'
#' @return A list consisting of the following elements:
#'
#'\item{pop}{vector; The final posterior draw for the
#' degrees of the population. The first \eqn{n} are the sample in sequence and
#' the reminder are non-sequenced. The vector is of length \code{maxN*m}}
#'\item{K}{count; the maximum degree for an
#' individual. This is usually calculated as twice the maximum observed
#' degree.}
#'\item{m}{count; the number of clusters.}
#'\item{n}{count; the overall sample size.}
#'\item{nc}{vector; the sample size in each cluster.}
#'\item{samplesize}{count; the
#' number of Monte-Carlo samples to draw to compute the posterior. This is the
#' number returned by the Metropolis-Hastings algorithm.The default is 1000.}
#'\item{burnin}{count; the number of proposals before any MCMC sampling is
#' done. It typically is set to a fairly large number.}
#'\item{interval}{count; the number of proposals between sampled statistics.}
#'\item{mu}{scalar; The
#' hyper parameter \code{mean.prior.degree} being the mean degree for the prior
#' distribution for a randomly chosen person. The prior has this mean.}
#'\item{sigma}{scalar; The hyper parameter \code{sd.prior.degree} being the
#' standard deviation of the degree for a randomly chosen person. The prior has
#' this standard deviation.}
#'\item{df.mean.prior}{scalar; A hyper parameter
#' being the degrees-of-freedom of the prior for the mean. This gives the
#' equivalent sample size that would contain the same amount of information
#' inherent in the prior.}
#'\item{df.sd.prior}{scalar; A hyper parameter being
#' the degrees-of-freedom of the prior for the standard deviation. This gives
#' the equivalent sample size that would contain the same amount of information
#' inherent in the prior for the standard deviation.}
#'\item{muproposal}{scalar; The standard deviation of the proposal
#' distribution for the mean degree.}
#'\item{sigmaproposal}{scalar; The standard
#' deviation of the proposal distribution for the standard deviation of the
#' degree.}
#'\item{N}{vector of length 5; summary statistics for the posterior
#' population size.
#' \describe{
#'\item{MAP}{maximum aposteriori value of N}
#'\item{Mean AP}{mean aposteriori value of N}
#'\item{Median AP}{median aposteriori value of N}
#'\item{P025}{the 2.5th
#' percentile of the (posterior) distribution for the N. That is, the lower
#' point on a 95\% probability interval.}
#'\item{P975}{the 97.5th
#' percentile of the (posterior) distribution for the N. That is, the upper
#' point on a 95\% probability interval.} } }
#'\item{maxN}{integer; maximum
#' possible population size. By default this is determined from an upper
#' quantile of the prior distribution.}
#'\item{sample}{matrix of dimension \code{samplesize} by \code{1 + 5*m}
#' of draws from the joint posterior distribution of model parameters. This is
#' also an object of class \code{mcmc} so it can be plotted and summarized
#' via the \code{mcmc.diagnostics} function in the \code{ergm} package
#' (and also the \code{coda} package). The model parameters are:
#'\describe{
#'\item{N}{scalar; population size.}
#'\item{mu i}{scalar; the mean of the unit size distribution in cluster \code{i}
#' for \code{i = 1, ..., m}.}
#'\item{sigma i}{scalar; the standard deviation of the unit size distribution
#' in cluster \code{i} for \code{i = 1, ..., m}.}
#'\item{p i}{scalar; the proportion of the population in cluster \code{i}
#' for \code{i = 1, ..., m}.}
#'\item{lambda i}{scalar; the \eqn{\lambda_i} parameter in cluster \code{i} from
#' the standard parameterization of the Conway-Maxwell-Poisson model for the
#' degree distribution.}
#'\item{nu i}{scalar; the \eqn{\nu_i} parameter in cluster \code{i} from the
#' standard parameterization of the Conway-Maxwell-Poisson model for the degree
#' distribution.} } }
#'\item{lpriorm}{vector; the vector of (log) prior
#' probabilities on each value of \eqn{M=N-n} - that is, the number of
#' unobserved members of the population. The values are
#' \code{n:(length(lpriorm)-1+n)}. This M is not the number of clusters.}
#'\item{burnintheta}{count; the number of
#' proposals in the Metropolis-Hastings sub-step for the degree distribution
#' parameters (\eqn{\theta_i}) before any MCMC sampling is done. It typically is
#' set to a modestly large number.}
#'\item{verbose}{logical; if this is
#' \code{TRUE}, the program printed out additional information, including
#' goodness of fit statistics.}
#'\item{MAP}{vector of length \code{1 + 5*m}
#' of MAP estimates corresponding to the output \code{sample}. These are:
#'\describe{
#'\item{N}{scalar; population size.}
#'\item{mu i}{scalar; the mean of the unit size distribution in cluster \code{i}
#' for \code{i = 1, ..., m}.}
#'\item{sigma i}{scalar; the standard deviation of the unit size distribution
#' in cluster \code{i} for \code{i = 1, ..., m}.}
#'\item{p i}{scalar; the proportion of the population in cluster \code{i}
#' for \code{i = 1, ..., m}.}
#'\item{lambda i}{scalar; the \eqn{\lambda_i} parameter in cluster \code{i} from
#' the standard parameterization of the Conway-Maxwell-Poisson model for the
#' degree distribution.}
#'\item{nu i}{scalar; the \eqn{\nu_i} parameter in cluster \code{i} from the
#' standard parameterization of the Conway-Maxwell-Poisson model for the degree
#' distribution.} } }
#'\item{median.prior.size}{scalar; A hyperparameter
#' being the median of the prior distribution on the population size.}
#'\item{mode.prior.size}{scalar; A hyperparameter being the mode of the prior
#' distribution on the population size.}
#'\item{mean.prior.size}{scalar; A
#' hyperparameter being the mean of the prior distribution on the population
#' size.}
#'\item{quartiles.prior.size}{vector of length 2; A pair of
#' hyperparameters being the lower and upper quartiles of the prior
#' distribution on the population size.}
#'\item{propsprior}{vector; the mean of the Dirichlet prior on the population
#' proportion in each cluster.}
#'\item{concent}{scalar; the concentration parameter for the Dirichlet prior
#' on the population proportion in each cluster.}
#'\item{degreedistribution}{character; the
#' parametric distribution to use for the individual network sizes (i.e.,
#' degrees).}
#' \item{priorsizedistribution}{character; the type of parametric distribution
#' to use for the prior on population size. The options are \code{beta} (for a
#' Beta prior on the sample proportion (i.e. \eqn{n/N}), \code{nbinom}
#' (Negative-Binomial), \code{pln} (Poisson-log-normal), \code{flat} (uniform),
#' and \code{continuous} (the continuous version of the Beta prior on the
#' sample proportion. The default is \code{beta}. }
#'
#' @section Details on priors: The best way to specify the prior is via the
#' hyperparameter \code{mode.prior.size} which specifies the mode of the prior
#' distribution on the population size or the hyperparameter
#' \code{median.prior.size} which specifies the median of the prior
#' distribution on the population size, or \code{mean.prior.sample proportion}
#' which specifies the mean of the prior distribution on the
#' proportion of the population size in the sample. Finally, you can specify
#' \code{quartiles.prior.size} as a vector of length 2 being the pair of lower
#' and upper quartiles of the prior distribution on the population size.
#'
#' The prior distribution on population proportions should be specified using
#' \code{props.prior.param}. The mean of the resulting Dirichlet distribution
#' will be \code{props.prior.param/sum(props.prior.param)}. The variance of
#' the resulting Dirichlet distribution is governed by the concentration
#' parameter \code{sum(props.prior.param)}.
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
#' Gile, Krista J. and Handcock, Mark S. (2014) \pkg{sspse}: Estimating Hidden
#' Population Size using Respondent Driven Sampling Data
#' R package, Los Angeles, CA.  Version 0.5, \url{http://hpmrg.org}.
#'
#' Handcock, Mark S., Gile, Krista J. and Mar, Corinne M. (2014)
#' \emph{Estimating Hidden Population Size using Respondent-Driven Sampling
#' Data}, Electronic Journal of Statistics, 8, 1, 1491-1521
#'
#' @seealso sspse, network, statnet
#'
#' @examples
#'
#' data(csamp)
#' # Make sure data is ordered by sample order
#' csamp <- csamp[order(csamp$samp.order), ]
#'
#' # interval, burnin, and samplesize should be larger in a real application.
#' fit <- posteriorsize.c(csamp$network.size.variable,
#'                        csamp$cluster,
#'                        median.prior.size = 3000,
#'                        prop.prior.params = rep(1, 3),
#'                        burnin = 100, samplesize = 500, interval = 1)
#' summary.c(fit)
#' post.plot(fit)
#'
#' @export posteriorsize.c

posteriorsize.c<-function(s, c, prop.prior.params = NULL,
                  median.prior.size=NULL,
                  interval=10,
                  burnin=5000,
                  maxN=NULL,
                  K=NULL,
                  samplesize=1000,
                  quartiles.prior.size=NULL,
                  mean.prior.size=NULL,
                  mode.prior.size=NULL,
                  priorsizedistribution=c("beta","flat","nbinom","pln","supplied"),
                  effective.prior.df=1,
                  sd.prior.size=NULL,
                  mode.prior.sample.proportion=NULL,
                  alpha=NULL,
                  mean.prior.degree=NULL, sd.prior.degree=NULL, max.sd.prior.degree=4,
                  df.mean.prior=1,df.sd.prior=3,
                  nk=NULL,
                  n=NULL,
                  muproposal=0.1,
                  sigmaproposal=0.15,
                  conc.proposal=5,
                  burnintheta=500,
                  seed=NULL,
                  maxbeta=120,
                  supplied=list(maxN=maxN),
                  max.coupons=NULL,
                  recruit.time=NULL,include.tree=TRUE, unit.scale=FALSE,
                  optimism = TRUE,
                  reflect.time=TRUE,
                  verbose=TRUE){
# Based on 'statnet' project software (http://statnet.org).
# For license and citation information see http://statnet.org/attribution

# Based on HPMRG project software (http://hpmrg.org).
# For license and citation information see http://hpmrg.org/attribution

  diff.size.by.clust <- TRUE
  degreedistribution <- "cmp"
  posfn <- poscmp
  # If the passed "s" is an rds.rata.frame, extract out the components
  if(!methods::is(s,"rds.data.frame")){
   visibility <- FALSE
   if(is.null(K)) K=max(s,na.rm=TRUE)
   if(is.null(n)) n=length(s)
   m <- length(unique(c))
   if(length(s) != length(c)){
     stop("Vectors of unit size and cluster membership should be the same length")
   }
  }else{
  rds.data <- s
  if(is.character(c)){
    c <- rds.data[, c]
  } else {
    if(nrow(rds.data) != length(c)){
      stop("Vector of cluster membership should be the same length as the RDS data")
    }
  }
  m <- length(unique(c))
  n <- nrow(rds.data)
  if(is.null(attr(rds.data,"network.size.variable")))
    stop("rds.data must have a network.size attribute.")
  nr <- RDS::get.number.of.recruits(rds.data)
  nw <- RDS::get.wave(rds.data)
  ns <- RDS::get.seed.id(rds.data)
  is.seed <- (RDS::get.rid(rds.data)=="seed")

  if(is.null(max.coupons)){
    max.coupons <- attr(rds.data,"max.coupons")
    if(is.null(max.coupons)){
      max.coupons <- max(nr,na.rm=TRUE)
    }
  }
  if(length(recruit.time)==1){
    if(is.character(recruit.time)){
      if(recruit.time=="wave"){
        recruit.times <- nw
      }else{
       recruit.times <- rds.data[[recruit.time]]
       if(methods::is(recruit.times,"POSIXt") | methods::is(recruit.times,"Date")){
        recruit.times <- as.numeric(recruit.times) / (24*60*60)
       }else{
        recruit.times <- as.numeric(recruit.times)
       }
      }
      recruit.time <- TRUE
    }else{
      if(is.na(recruit.time)){
        recruit.times <- rep(0,n)
        recruit.time <- FALSE
      }else{
        stop("The recruitment time should be a variable in the RDS data, or 'wave' to indicate the wave number or NA/NULL to indicate that the recruitment time is not available and/or used.")
      }
    }
  }else{
    if(length(recruit.time)==0 & is.null(recruit.time)){
      recruit.time <- 1:n
    }else{
      if(length(recruit.time)!=n | (!is.numeric(recruit.time) & !methods::is(recruit.time,"POSIXt") & !methods::is(recruit.time,"Date"))){
        stop("The recruitment time should be a variable in the RDS data, or 'wave' to indicate the wave number or NA/NULL to indicate that the recruitment time is not available and/or used.")
      }
    }
    if(length(recruit.time)==n & (methods::is(recruit.time,"POSIXt") | methods::is(recruit.time,"Date"))){
      recruit.times <- as.numeric(recruit.time) / (24*60*60)
    }else{
      recruit.times <- recruit.time
    }
    recruit.time <- TRUE
  }
  if(any(is.na(recruit.times))){
    med.index <- cbind(c(2,1:(n-1)),c(3,3:n,n))
    moving.median=function(i){stats::median(recruit.times[med.index[i,]],na.rm=TRUE)}
    while(any(is.na(recruit.times))){
      for(i in which(is.na(recruit.times))){recruit.times[i] <- moving.median(i)}
    }
  }
# gap <- diff(sort(recruit.times))
# gap <- min(gap[gap > 0])
# recruit.times <- recruit.times + 0.01*(1:n)*gap/(n+1)
  recruit.times <- recruit.times - min(recruit.times)
  if(reflect.time){
    recruit.times <- max(recruit.times)-recruit.times
  }
  network.size <- as.numeric(rds.data[[attr(rds.data,"network.size.variable")]])
  remvalues <- is.na(network.size)
  if(any(remvalues)){
    warning(paste(sum(remvalues),"of",nrow(rds.data),
                  "network sizes were missing. These will be imputed from the marginal distribution"), call. = FALSE)
  }

  if(is.null(K)){
    if(length(network.size[!remvalues])>0){
      K <- round(stats::quantile(network.size[!remvalues],0.95))
    }
  }

  #Augment the reported network size by the number of recruits and the recruiter (if any).
  if(include.tree){
    nsize <- pmax(network.size,nr+!is.seed)
  }else{
    nsize <- network.size
  }

  gmean <- HT.estimate(RDS::vh.weights(nsize[!is.na(nsize)]),nsize[!is.na(nsize)])
  if(is.na(gmean)) gmean <- 38

  s <- nsize
  }

  remvalues <- is.na(s)
  if(sum(!remvalues) < length(s)){
   warning(paste(length(s)-sum(!remvalues),"of",length(s),
          "sizes values were missing and were removed."), call. = FALSE)
   s <- s[!remvalues]
   n <- length(s)
  }
  s.prior <- s

  priorsizedistribution=match.arg(priorsizedistribution)
  #if(priorsizedistribution=="nbinom" && missing(mean.prior.size)){
  #  stop("You need to specify 'mean.prior.size', and possibly 'sd.prior.size' if you use the 'nbinom' prior.")
  #}
  if(is.null(K)){
    K=round(stats::quantile(s.prior,0.90))+1
    degs <- s.prior
    degs[degs>K] <- K
    degs[degs==0]<-1
    ds<-degs
    isnas <- is.na(degs)
    degs <- sum(!isnas)*(degs)/sum(degs,na.rm=TRUE)
    weights <- (1/degs)
    weights[is.na(weights)] <- 0
    mean.pd <- sum(ds*weights)/sum(weights)
    sd.pd <- sum(ds*ds*weights)/sum(weights)
    sd.pd <- sqrt(sd.pd - mean.pd^2)
    if(sd.pd > max.sd.prior.degree*mean.pd){
     sd.pd <- min(max.sd.prior.degree*mean.pd, sd.pd)
    }
    xv <- ds
#   xp <- weights*ds
    xp <- weights
    xp <- length(xp)*xp/sum(xp)
    txp <- tapply(xp,xv,sum)
    txv <- tapply(xv,xv,stats::median)
    fit <- cmpmle(txv,txp,cutoff=1,cutabove=K-1,guess=c(mean.pd, sd.pd))
    y=dcmp.natural(v=fit,x=(0:max(s.prior)))
    K=(0:max(s.prior))[which.max(cumsum(y)>0.99)]
#   K=round(stats::quantile(s,0.99))
  }
  cat(sprintf("The cap on influence of the personal network size is K = %d.\n",K))

  # Find Prior Mean and SD
  if(is.null(mean.prior.degree)){
    if(diff.size.by.clust){
      mean.prior.degree <- rep(0, m)
      sd.pd <- rep(0, m)
      ds <- list()
      weights <- list()
      for(i in 1:m){
        degs <- s.prior[c == i]
        degs[degs>K] <- K
        degs[degs==0] <- 1
        ds[[i]] <- degs
        isnas <- is.na(degs)
        degs <- sum(!isnas)*(degs)/sum(degs,na.rm=TRUE)
        weights[[i]] <- (1/degs)
        weights[[i]][is.na(weights[[i]])] <- 0
        mean.prior.degree[i] <- sum(ds[[i]]*weights[[i]])/sum(weights[[i]])
        if(is.null(sd.prior.degree)){
          sd.pd[i] <- sum(ds[[i]]*ds[[i]]*weights[[i]])/sum(weights[[i]])
          sd.pd[i] <- sqrt(sd.pd[i] - mean.prior.degree[i]^2)
        }
      }
      if(is.null(sd.prior.degree)){
        sd.prior.degree <- sd.pd
      }

      # Degrees
      xv <- ds

      # Weights
      xp <- weights
      xp[is.na(xp)] <- 0

      for(i in 1:m){
        xp[[i]] <- length(xp[[i]])*xp[[i]]/sum(xp[[i]])

        # Sum of the weighs for all subjects with each reported degree
        txp <- tapply(xp[[i]],xv[[i]],sum)

        # Unique reported degrees
        txv <- tapply(xv[[i]],xv[[i]],stats::median)

        # Prior distribution for unit size
        fit <- cmpmle(txv,txp,cutoff=1,cutabove=K-1,
                      guess=c(mean.prior.degree[i],sd.prior.degree[i]))
        fit <- cmp.mu(fit,max.mu=5*mean.prior.degree[i])
        if(verbose){
          cat(sprintf("The preliminary empirical value of the mean of the prior distribution for degree in cluster %i is %f.\n",i, mean.prior.degree[i]))
          cat(sprintf("The preliminary empirical value of the s.d. of the prior distribution for degree in cluster %i is %f.\n",i, sd.prior.degree[i]))
        }

        # Mean and SD for prior on unit size
        mean.prior.degree[i] = fit[1]
        sd.prior.degree[i] = fit[2]
      }

    }else{
      #degs <- s.prior
      #degs[degs>K] <- K
      #degs[degs==0]<-1
      #ds<-degs
      #isnas <- is.na(degs)
      #degs <- sum(!isnas)*(degs)/sum(degs,na.rm=TRUE)
      #weights <- (1/degs)
      #weights[is.na(weights)] <- 0
      #mean.prior.degree <- sum(ds*weights)/sum(weights)
      #if(is.null(sd.prior.degree)){
      #  sd.prior.degree <- sum(ds*ds*weights)/sum(weights)
      #  sd.prior.degree <- sqrt(sd.prior.degree - mean.prior.degree^2)
      #}
      #xv <- ds
      #xp <- weights
      #xp[is.na(xp)] <- 0
      #xp <- length(xp)*xp/sum(xp)
      #txp <- tapply(xp,xv,sum)
      #txv <- tapply(xv,xv,stats::median)
      #fit <- cmpmle(txv,txp,cutoff=1,cutabove=K-1,
      #              guess=c(mean.prior.degree,sd.prior.degree))
      #fit <- cmp.mu(fit,max.mu=5*mean.prior.degree)
      #if(verbose){
      #  cat(sprintf("The preliminary empirical value of the mean of the prior distribution for degree is %f.\n",mean.prior.degree))
      #  cat(sprintf("The preliminary empirical value of the s.d. of the prior distribution for degree is %f.\n",sd.prior.degree))
      #}
      #mean.prior.degree = fit[1]
      #sd.prior.degree = fit[2]
    }
  }else{
    if(is.null(sd.prior.degree)){
      sd.prior.degree <- sqrt(mean.prior.degree)
    }
  }
  # End Find Prior Mean and SD for unit size


  if(verbose){
    if(diff.size.by.clust){
      for(i in 1:m){
        cat(sprintf("The mean of the prior distribution for degree in cluster %i is %f.\n",i, mean.prior.degree[i]))
        cat(sprintf("The s.d. of the prior distribution for degree in cluster %i is %f.\n",i, sd.prior.degree[i]))
      }
    } else {
    cat(sprintf("The mean of the prior distribution for degree is %f.\n",mean.prior.degree))
    cat(sprintf("The s.d. of the prior distribution for degree is %f.\n",sd.prior.degree))
    }
  }
  if(diff.size.by.clust){
    if(sum(sd.prior.degree > max.sd.prior.degree*mean.prior.degree) > 0){
      sd.prior.degree <-
        apply(cbind(max.sd.prior.degree*mean.prior.degree, sd.prior.degree),
              1, min)
      cat(sprintf("The suggested s.d. of the prior distribution for degree is too large and has been reduced to the more reasonable %f.\n",sd.prior.degree))
    }
  } else {
    if(sd.prior.degree > max.sd.prior.degree*mean.prior.degree){
      sd.prior.degree <- min(max.sd.prior.degree*mean.prior.degree, sd.prior.degree)
      cat(sprintf("The suggested s.d. of the prior distribution for degree is too large and has been reduced to the more reasonable %f.\n",sd.prior.degree))
    }
  }

  if(is.null(prop.prior.params)){
    prop.prior.params <- rep(1, m)
  }

  Cret <- posfn(s=s,c=c,m=m,K=K,nk=nk,maxN=maxN,
                prop.prior.params=prop.prior.params,
                mean.prior.degree=mean.prior.degree,df.mean.prior=df.mean.prior,
                sd.prior.degree=sd.prior.degree,df.sd.prior=df.sd.prior,
                muproposal=muproposal, sigmaproposal=sigmaproposal,
                conc.proposal=conc.proposal,
                samplesize=samplesize,burnin=burnin,interval=interval,
                burnintheta=burnintheta,
                priorsizedistribution=priorsizedistribution,
                mean.prior.size=mean.prior.size, sd.prior.size=sd.prior.size,
                mode.prior.sample.proportion=mode.prior.sample.proportion,
                median.prior.size=median.prior.size,
                mode.prior.size=mode.prior.size,
                quartiles.prior.size=quartiles.prior.size,
                effective.prior.df=effective.prior.df,
                alpha=alpha,
                seed=seed,
                supplied=supplied,
                num.recruits=nr[!remvalues],
                recruit.times=recruit.times[!remvalues],
                max.coupons=max.coupons,
                maxbeta=maxbeta)
  #return()
  Cret$N <- c(Cret$MAP["N"],
              mean(Cret$sample[,"N"]),
              stats::median(Cret$sample[,"N"]),
              stats::quantile(Cret$sample[,"N"],c(0.025,0.975)))
  names(Cret$N) <- c("MAP","Mean AP","Median AP","P025","P975")
  #
  Cret$sample <- Cret$sample[,-match(c(paste("degree1", 1:m), paste("totalsize", 1:m)),
                                     colnames(Cret$sample))]
  #
  #if(Cret$predictive.degree[length(Cret$predictive.degree)] > 0.01){
  # warning("There is a non-trivial proportion of the posterior mass on very high degrees. This may indicate convergence problems in the MCMC.", call. = FALSE)
  #}
  Cret$degreedistribution <- degreedistribution
  Cret$priorsizedistribution <- priorsizedistribution

# Cret$mean.prior.size <- mean.prior.size
  ### return result
  class(Cret) <- "sspse"
  Cret
}

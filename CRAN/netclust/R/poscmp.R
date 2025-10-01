poscmp<-function(s,c,m,maxN=NULL,
                 prop.prior.params,
                  K=2*max(s), nk=NULL, n=length(s),
                  mean.prior.degree=7, sd.prior.degree=3,
                  df.mean.prior=1, df.sd.prior=5,
                  muproposal=0.1,
                  sigmaproposal=0.15,
                  conc.proposal=5,
                  samplesize=10,burnin=0,interval=1,burnintheta=500,
                  priorsizedistribution=c("beta","nbinom","pln","flat","supplied"),
                  mean.prior.size=NULL, sd.prior.size=NULL,
                  mode.prior.sample.proportion=NULL,
                  median.prior.sample.proportion=NULL,
                  median.prior.size=NULL,
                  mode.prior.size=NULL,
                  quartiles.prior.size=NULL,
                  effective.prior.df=1,
                  alpha=NULL,
                  seed=NULL,
                  maxbeta=120,
                  supplied=list(maxN=maxN),
              num.recruits=NULL,
              recruit.times=NULL,
              max.coupons=NULL,
                  verbose=TRUE){
    #this function takes a vector of population sizes and a vector s of
    #sequential sizes of sampled units and returns a log likelihood value
    #s values must all be positive integers
    if(!is.null(seed))  set.seed(as.integer(seed))
    #
    # Cap the maximum degree to K
    #
    s[s>K] <- K
    # nk is a vector of counts for each degree in each cluster
    nk <- rep(0, K*m)
    for(i in 1:m){
      nk[((i - 1)*K + 1):(i*K)] <- as.integer(tabulate(s[c==i],nbins=K))
    }

    # Transform observed mean parametrization to log-normal
    # parametrization
    #
    mu <- rep(0, m)
    sigma <- rep(0, m)
    for (i in 1:m){
      out <- cmp.natural(mu=mean.prior.degree[i], sigma=sd.prior.degree[i])
      mu[i] <- log(out$lambda)
      sigma[i] <- out$nu
    }
    dimsample <- 5*m+1

    priorsizedistribution=match.arg(priorsizedistribution)

    prior <- sspse::dsizeprior(n=n,
                  type=priorsizedistribution,
                  sd.prior.size=sd.prior.size,
                  mode.prior.sample.proportion=mode.prior.sample.proportion,
                  median.prior.sample.proportion=median.prior.sample.proportion,
                  median.prior.size=median.prior.size,
                  mode.prior.size=mode.prior.size,
                  mean.prior.size=mean.prior.size,
                  quartiles.prior.size=quartiles.prior.size,
                  effective.prior.df=effective.prior.df,
                  alpha=alpha,
                  maxN=maxN,
                  maxbeta=maxbeta,
                  log=TRUE,
                  supplied=supplied,
                  verbose=verbose)

    n.c <- as.integer(table(c))
    pop <- rep(0, m*prior$maxN)
    for(i in 1:m){
      pop[((i - 1)*prior$maxN + 1):((i - 1)*prior$maxN + n.c[i])] <- as.integer(s[c==i])
    }
    ppos <- rep(0, m*K)

    # separate mean vector from concentration parameter for prior params
    concent <- sum(prop.prior.params)
    prop.prior.params <- prop.prior.params/concent

   Cret <- .C("gcmp3",
              pop=as.integer(pop),
              nk=as.integer(nk),
              m=as.integer(m),
              propsprior=as.double(prop.prior.params),
              concent=as.double(concent),
              K=as.integer(K),
              n=as.integer(n),
              nc=as.integer(n.c),
              ppos=as.double(ppos),
              samplesize=as.integer(samplesize),
              burnin=as.integer(burnin),
              interval=as.integer(interval),
              mu=as.double(mean.prior.degree), df.mean.prior=as.double(df.mean.prior),
              sigma=as.double(sd.prior.degree), df.sd.prior=as.double(df.sd.prior),
              muproposal=as.double(muproposal),
              sigmaproposal=as.double(sigmaproposal),
              concentproposal=as.double(conc.proposal),
              N=as.integer(prior$N),
              maxN=as.integer(prior$maxN),
              sample=double(samplesize*dimsample),
              lpriorm=as.double(prior$lprior),
              burnintheta=as.integer(burnintheta),
              verbose=as.integer(verbose), PACKAGE="netclust")

    Cret$sample<-matrix(Cret$sample,nrow=samplesize,ncol=dimsample,byrow=TRUE)
    degnames <- NULL
    colnamessample <- c("N", paste("mu", 1:m), paste("sigma", 1:m), paste("p", 1:m),
                        paste("degree1", 1:m), paste("totalsize", 1:m))
    max.mu <- 2*mean.prior.degree
    colnames(Cret$sample) <- colnamessample
    #
    # Transform observed mean parametrization to log-normal
    # parametrization
    #
    # Expectation and s.d. of normal from log-normal
    #
    Cret$sample[,2:(m+1)] <- exp(Cret$sample[,2:(m+1)])
    Cret$sample <- cbind(Cret$sample,Cret$sample[,2:(2*m+1)])
    colnames(Cret$sample)[ncol(Cret$sample)-((2*m-1):0)] <- c(paste("lambda", 1:m),
                                                              paste("nu", 1:m))
    # Transform to mean value parametrization
    for(i in 1:m){
      a <- t(apply(Cret$sample[,c(1+i, m+1+i)],1,cmp.to.mu.sd,
                   force = TRUE, K = K, max.mu=max.mu[i]))
      nas <- apply(a,1,function(x){any(is.na(x))})
      if(!all(nas)){
        inas <- sample(seq_along(nas)[!nas],size=sum(nas),replace=TRUE)
        a[nas,] <- a[inas,]
        #    Cret$sample[,c("mu","sigma")] <- t(apply(Cret$sample[,c("mu","sigma")],1,cmp.mu,max.mu=5*mean.prior.degree)))
        Cret$sample[,c(1+i, m+1+i)] <- a
      }
    }

    Cret$predictive.degree.count<-Cret$nk / samplesize
    Cret$predictive.degree<-Cret$ppos
    endrun <- burnin+interval*(samplesize-1)
    attr(Cret$sample, "mcpar") <- c(burnin+1, endrun, interval)
    attr(Cret$sample, "class") <- "mcmc"
    ### compute modes of posterior samples, Maximum A Posterior (MAP) values
    ### for N, mu_i, sigma_i, p_i, lambda_i, nu_i
    Cret$MAP <- apply(Cret$sample[, c("N",
                                      paste("mu", 1:m), paste("sigma", 1:m),
                                      paste("p", 1:m),
                                      paste("lambda", 1:m), paste("nu", 1:m))],
                      2, mode.density)
    Cret$MAP["N"] <- mode.density(Cret$sample[,"N"],lbound=n,ubound=prior$maxN)
    Cret$maxN <- prior$maxN
    Cret$quartiles.prior.size <- prior$quartiles.prior.size
    Cret$mode.prior.size <- prior$mode.prior.size
    Cret$mean.prior.size <- prior$mean.prior.size
    Cret$effective.prior.df <- prior$effective.prior.df
    Cret$median.prior.size <- prior$median.prior.size
    Cret$mode.prior.sample.proportion <- prior$mode.prior.sample.proportion
    Cret$N <- prior$N
    Cret$K <- K
    Cret <- Cret[c("pop", "K", "m", "n", "nc", "samplesize", "burnin", "interval",
                   "mu", "sigma", "df.mean.prior", "df.sd.prior",
                   "muproposal", "sigmaproposal", "N",
                   "maxN", "sample", "lpriorm", "burnintheta",
                   "verbose", "MAP",
                   "median.prior.size", "mode.prior.size", "mean.prior.size",
                   "quartiles.prior.size",
                   "propsprior", "concent",
                   "degreedistribution", "priorsizedistribution")]
    Cret
}

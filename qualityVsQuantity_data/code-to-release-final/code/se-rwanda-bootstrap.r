#! /usr/bin/env Rscript

###################################################################
## se-rwanda-bootstrap.r
##
## take many bootstrap resamples of the Rwanda survey dataset.
## for each one,
##   * estimate respondents' degrees
##   * perform internal validation checks
##   * produce estimates for each tie defn
##   * using another round of bootstraps, estimate sampling variance
##     for the known and unknown estimands
##

library(plyr)
library(dplyr)
library(networkreporting)

# this ensures that results are replicable
set.seed(100)

source(file.path("se-rwanda-directories.r"))
set.dirs()

load(paste(data.dir, "/se-rwanda-data.RData", sep=""))

## for final version, turning off use.parallel (so it's easier to replicate)
use.parallel <- FALSE
#use.parallel <- TRUE

#numproc <- 19
#M <- 5
M <- 2000

if (use.parallel) {
    library(doMC)
    registerDoMC(numproc)
}

use.writelog <- TRUE
#use.writelog <- FALSE

## set up logging
if(use.writelog) { 
  writelog <- cat
  
  logfile <- file(file.path(out.dir, "network-bootstrap.log"), open="wt")
  sink(logfile, split=TRUE)
  
} else { 
  writelog <- function(...) { invisible(NULL) }
}

# number of bootstrap resamples *within* each bootstrap
# resample (used when we estimate variance within each
# bootstrap resample)
M.inner <- M

all.verbose <- FALSE

## this is the dataset we'll analyze in this file...
survey.dat <- topcoded.dat

## we want results as proportions, so we'll convert the known popns
## into proportions
kp.prop <- (attr(survey.dat, "known.popns") / attr(survey.dat, "total.popn.size"))

## compute the estimated network sizes
survey.dat$d.hat <- kp.individual.estimator_(resp.data=survey.dat,
                                known.populations=names(kp.prop),
                                total.kp.size=sum(kp.prop),
                                alter.popn.size=1)$dbar.Fcell.F

## ic results for the full dataset
## (this variable is called 'iv.res' because we changed terminology
##  from internal validation to internal consistency)
## (we take bootstrap samples and do this w/in each sample later;
##  these are the point estimates)
iv.res <- dlply(survey.dat,
                .(tie.defn),
                function(x) {
                    nsum.internal.validation(survey.data=x,
                                             kp.method=TRUE,
                                             known.popns=kp.prop,
                                             missing="complete.obs",
                                             weights="indweight",
                                             total.popn.size=NA)
                })


estimated.degrees <- survey.dat[,c("tie.defn", "d.hat", "indweight")]
estimated.degrees <- plyr::rename(estimated.degrees, c("indweight"="weight"))

## split recoded dataset up by defn of tie
survey.dat$cluster <- factor(survey.dat$cluster)
recoded.bytie <- dlply(survey.dat, .(tie.defn), identity)

################################
## this function estimates the size of the four hidden popns
## from a bootstrapped dataset
boot.qoi.estimate <- function(survey.data, 
                              weights, 
                              verbose=FALSE, 
                              kp=kp.prop,
                              qois=c("sex.workers","msm","idu","clients")) {

    ## estimates for the hidden popn sizes
    ests <- ldply(qois,
                  function(thisqoi) {

                      res <- nsum.estimator(survey.data=survey.data,
                                            weights=weights,
                                            d.hat.vals="d.hat",
                                            y.vals=thisqoi,
                                            total.popn.size=NA)

                      return(data.frame(qoi=thisqoi,
                                        res))
                  })
    return(ests)
}

################################
## this function will get called on each bootstrap resample
## of the dataset. it does several things, each of which is
## studied in more depth in other files. 
##
## specifically, for each bootstrap resample, this function
##  - makes a dataset of of the respondents' network sizes and
##    sampling weights
##  - computes the internal validation checks and related estimates
##    of prediction error
##  - computes the estimates of the hidden population sizes
##  - also compute estimates of the known population sizes (from the
##    full sample)
##  - computes the average number of reported connections to
##    each popn
boot.estimate <- function(survey.data, weights, verbose=FALSE, kp=kp.prop, parallel=FALSE) {

    force(weights)
  
    writelog("START-OUTER : starting outer rep...\n")
  
    degs <- data.frame(d.hat=survey.data$d.hat,
                       weight=weights,
                       tie.defn=survey.data$tie.defn)
    
    ## internal validation checks
    iv.res <- nsum.internal.validation(survey.data,
                                       kp.method=TRUE,
                                       known.popns=kp,
                                       missing="complete.obs",
                                       weights=weights,
                                       total.popn.size=NA)

    ## estimates for the hidden popn sizes
    ests <- boot.qoi.estimate(survey.data, weights)
    
    this.survey.data <- survey.data

    ## bootstrap uncertainty estimates for the hidden popn sizes
    ## get bootstrap estimates of qoi variance within this tie defn
    inner.bootres <- bootstrap.estimates(survey.data=this.survey.data,
                                         survey.design= ~ cluster + strata(region),
                                         bootstrap.fn="rescaled.bootstrap.sample",
                                         ## qoi.boot.estimate is the function we 
                                         ## just defined above
                                         estimator.fn=boot.qoi.estimate,
                                         weights="indweight",
                                         parallel=parallel,
                                         num.reps=M.inner,
                                         verbose=all.verbose)

    ## make one big, long-form dataset out of the M.inner
    ## bootstrap resampled datasets that get returned
    inner.bootres <- do.call("rbind", inner.bootres)

    ## summarize the inner bootstrap resamples; use
    ## the percentile method for confidence intervals, but
    ## also get point estimates of the variance and std error
    ci.ests <- ddply(inner.bootres,
                     .(qoi),
                     summarise,
                     ci.low = quantile(estimate, .025),
                     ci.high = quantile(estimate, .975),
                     var = var(estimate),
                     se = sqrt(var(estimate)))

    ests <- join(ests,
                 ci.ests,
                 by="qoi")

    ## avg number reported known
    num.conn <- laply(names(kp),
                      function(this.kp) {
                        res <- weighted.mean(survey.data[,this.kp],
                                             w=networkreporting:::get.weights(survey.data,
                                                                              weights))
                        return(res)
                      })
    num.conn <- data.frame(name=names(kp), avg.num.conn=num.conn)

    return(list(iv.res=iv.res, 
                estimates=ests, 
                degrees=degs,
                num.conn=num.conn))
}

writelog("TIME-MARK", format(Sys.time(), "%a %b %d %X %Y"), "\n")

writelog("Starting ", M, " x ", M, " bootstrap resamples...\n\n")

## get bootstrap estimates of total error within each tie defn
bootres <- llply(recoded.bytie,
                 bootstrap.estimates,
                 survey.design= ~ cluster + strata(region),
                 bootstrap.fn="rescaled.bootstrap.sample",
                 estimator.fn=boot.estimate,
                 weights="indweight",
                 num.reps=M,
                 # the inner bootstrap reps are actually done in parallel
                 parallel=use.parallel)

writelog("Finished bootstrap resamples; saving...\n")

writelog("TIME-MARK", format(Sys.time(), "%a %b %d %X %Y"), "\n")

save.image(file=file.path(out.dir, "rw-bootstrap-resamples-image.RData"),
           compress="xz")

############
## summarize the bootstrap avg num reported connections results
avg.num.conn.all <- ldply(bootres,
                          function(x) {
                              ldply(1:length(x),
                                    function(boot.idx) {
                                        return(data.frame(x[[boot.idx]]$num.conn, 
                                                           boot.rep=boot.idx))
                                    })
                          })

avg.num.conn.all <- avg.num.conn.all %>% rename(tie.defn=.id)

avg.num.conn.summ <- ddply(avg.num.conn.all,
                           .(tie.defn, name),
                           summarise,
                           avg.num.conn.ci.low=quantile(avg.num.conn, .025),
                           avg.num.conn.ci.high=quantile(avg.num.conn, .975),
                           avg.num.conn.se=sd(avg.num.conn),
                           avg.num.conn=mean(avg.num.conn))


############
## summarize the bootstrap internal validation results
boot.iv.summary <- function(reslist) {

    reslist <- llply(reslist, "[[", "iv.res")

    return(list(errors=data.frame(mse=laply(reslist, "[[", "mse"),
                                  rmse=laply(reslist, "[[", "rmse"),
                                  are=laply(reslist, "[[", "are"),
                                  mae=laply(reslist, "[[", "mae"),
                                  boot.num=1:length(reslist)),
                results=ldply(1:length(reslist),
                              function(thisidx) {
                                  thisdat <- reslist[[thisidx]]$results
                                  thisdat$boot.num <- thisidx
                                  return(thisdat)
                              })))
}

acc.check.bootres <- llply(bootres, boot.iv.summary)

## assemble the results into one single dataset, for plotting
acc.check.kp <- ldply(acc.check.bootres,
                      "[[", "errors")
acc.check.kp <- plyr::rename(acc.check.kp, c(".id"="tie.defn"))

#############
## summarize the bootstrap hidden popn size estimates
boot.est.summary <- function(reslist) {

    reslist <- llply(reslist, "[[", "estimates")

    ldply(1:length(reslist),
          function(idx) {
              return(data.frame(boot.num=idx,
                                reslist[[idx]]))
          })
}

ests.bootres <- llply(bootres, boot.est.summary)

ests.all <- ldply(ests.bootres,
                  identity)
ests.all <- plyr::rename(ests.all, c(".id"="tie.defn"))

#############
## summarize the bootstrap degree estimates
boot.deg.summary <- function(reslist) {

    reslist <- llply(reslist, "[[", "degrees")

    ldply(1:length(reslist),
          function(idx) {
              return(data.frame(boot.num=idx,
                                reslist[[idx]]))
          })
}

deg.bootres <- llply(bootres, boot.deg.summary)
deg.bootres <- ldply(deg.bootres, identity)

# NB: using xz compression makes the file waaaay smaller...
save(bootres, 
     avg.num.conn.all,
     avg.num.conn.summ,
     estimated.degrees,
     acc.check.kp, acc.check.bootres,
     ests.all,
     deg.bootres,
     kp.prop,
     iv.res,
     file=file.path(out.dir, "rw-bootstrap-resamples.RData"),
     compress="xz")

writelog("Done.\n")

writelog(format(Sys.time(), "%a %b %d %X %Y"), "\n")

if(use.writelog) {
  sink(NULL, type="message")
  sink(NULL)
}


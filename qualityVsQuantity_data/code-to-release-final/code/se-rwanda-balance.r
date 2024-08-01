#! /usr/bin/env Rscript

###################################################################
## se-rwanda-balance.r
##
## look at the balance in the two arms of the survey experiment
##

library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(stargazer)
# used for the omnibus balance check of Hansen and Bowers (2008)
# NB: we explicitly include base package methods because the RItools
# package doesn't seem to properly import it (meaning it fails if
# run via Rscript)
library(methods)
library(RItools)

source("se-rwanda-directories.r")
set.dirs()

load(file.path(data.dir, "se-rwanda-data.RData"))

balance.dat <- recoded.dat %>% 
               mutate(is.meal = ifelse(tie.defn == "Meal", 1, 0)) %>%
               mutate(hhid = str_replace(id, ".\\d+$", "")) %>%
               select(is.meal,
                      age,
                      sex,
                      region,
                      kigali,
                      educ,
                      wealth,
                      tie.defn,
                      cluster, region,
                      indweight, 
                      hhid, id)

# double-check that everyone in a given hh responded to the
# same tie definition
hh.tiecheck <- balance.dat %>% group_by(hhid) %>%
               summarise(n=n(),
                         var.td = ifelse(n==1,
                                         0,
                                         var(as.numeric(tie.defn))))
# looks ok
stopifnot(hh.tiecheck$var.td==0)

# hh-level dataset (we'll use this to permute acq/meal assignments)
hh.dat <- balance.dat %>% 
          rename(raw.is.meal=is.meal) %>% 
          group_by(hhid) %>% 
          mutate(hh.size=n()) %>%
          slice(1) %>%
          ungroup()

get.diff <- function(x) { 
    return(x[1] - x[2])
}

########################
## check household-level covariates
## (we cluster-randomized at the hh level)
########################

check.hh.balance <- function(this.hh.dat) {

    res <- this.hh.dat %>% 
           group_by(this.is.meal) %>%
           summarize(hh.size=mean(hh.size),
                     lowest.wealth.quint=mean(wealth=="Lowest"),
                     second.wealth.quint=mean(wealth=="Second"),
                     third.wealth.quint=mean(wealth=="Third"),
                     fourth.wealth.quint=mean(wealth=="Fourth"),
                     highest.wealth.quint=mean(wealth=="Highest"))

    return(res)
}

compute.hh.balance.diffs <- function(this.hh.balance) {

    res <- this.hh.balance %>%
           arrange(this.is.meal) %>%
           summarise(hh.size.diff = get.diff(hh.size),
                     lowest.wealth.quint.diff = get.diff(lowest.wealth.quint),
                     second.wealth.quint.diff = get.diff(second.wealth.quint),
                     third.wealth.quint.diff = get.diff(third.wealth.quint),
                     fourth.wealth.quint.diff = get.diff(fourth.wealth.quint),
                     highest.wealth.quint.diff = get.diff(highest.wealth.quint))

    return(res)

}

bal.check.hh.raw <- hh.dat %>% mutate(this.is.meal=raw.is.meal) %>%
                    check.hh.balance() %>%
                    mutate(name=ifelse(this.is.meal, "meal households", "acq households"))

# we'll use this in the permutation tests below
bal.check.hh.raw.diff <- bal.check.hh.raw %>% compute.hh.balance.diffs() %>% as.data.frame()

#########################
## permutation checks (conditioning on sample constant and not using sampling weights)
#########################

perm.M <- 5000
#perm.M <- 100

set.seed(12345)

perm.hh.summ <- ldply(1:perm.M,
                   function(idx) {

                       # reshuffle assignment to meal/acquaintance within cluster
                       # for each hh
                       this.hh.dat <- hh.dat %>% group_by(cluster) %>%
                                      mutate(this.is.meal = sample(raw.is.meal, 
                                                                   length(raw.is.meal), 
                                                                   replace=FALSE))

                       # compute the summary for households
                       hh.bal.check.summ <- this.hh.dat %>%
                                            check.hh.balance() %>%
                                            compute.hh.balance.diffs() %>%
                                            mutate(perm.idx=idx) %>%
                                            as.data.frame()

                       # return results
                       return(hh.bal.check.summ)

                   },
                   .progress='text')

# randomization p values for each covar:
# fraction of times that diff is more extreme than obsvd
hh.covars <- colnames(perm.hh.summ)[-match("perm.idx",colnames(perm.hh.summ))]

## compute empirical p values
rand.twosided.p <- function(test.val, perm.vals) {

    perm.mean <- mean(perm.vals)

    z <- as.numeric(abs(perm.vals - perm.mean) >= abs(test.val - perm.mean))

    return(mean(z))

}

hh.perm.p.vals <- ldply(hh.covars,
                        function(this.covar) {

                            this.twosided.p <- 
                                rand.twosided.p(bal.check.hh.raw.diff[[this.covar]],
                                                perm.hh.summ[,this.covar])

                          return(data.frame(covar=this.covar,
                                            twosided.p = this.twosided.p))
                        })

#####################
## save hh permutation test table

## reshape so that this is in long format
bal.check.hh.long <- bal.check.hh.raw %>%
                     select(-this.is.meal) %>%
                     gather(qty, avg, -name) %>%
                     spread(name, avg)

hh.perm.p.vals <- hh.perm.p.vals %>%
                  mutate(qty = str_replace(covar, ".diff", ""))

bal.check.hh.long <- bal.check.hh.long %>% 
       join(hh.perm.p.vals, by="qty") %>%
       select(-covar)

bal.check.hh.tab <- bal.check.hh.long %>% data.frame()
colnames(bal.check.hh.tab) <- c("", "Acquaintance", "Meal", "two-sided p-value")

## nice names for covars
bal.check.hh.tab[,1] <- c("Num. respondents in household",
                          "Lowest wealth quintile",
                          "Second wealth quintile",
                          "Third wealth quintile",
                          "Fourth wealth quintile",
                          "Highest wealth quintile")

hh.tab.cap <- "Comparison between households assigned to the acquaintance
tie definition and households assigned to the meal tie definition. All values
are fractions, with the exception of the average number of respondents.
P-values are from non-independent random permutation tests. 
Sampling weights were not used in computing these means. "

stargazer(bal.check.hh.tab,
          summary=FALSE,
          digits=3,
          style="demography",
          rownames=FALSE,
          title=hh.tab.cap,
          label="tab:balance-hh",
          out=file.path(out.dir, "balance-check-hh-tab.tex"))

################################################
## hansen bowers (2008) omnibus test

## hansen bowers pg 228:
## aggregate up to cluster level (here, this is the hh)
## and sum individual-level covars to hh totals

## fraction of hh in cluster in each tie defn
hh.covars <- recoded.dat %>%
             mutate(is.meal = ifelse(tie.defn == "Meal", 1, 0)) %>%
             mutate(hhid = str_replace(id, ".\\d+$", "")) %>%
             group_by(hhid) %>%
             mutate(hh.tot.age = sum(age),
                    hh.tot.f = sum(sex=="female"),
                    hh.tot.seconded = sum(educ=="Secondary+"),
                    hh.tot.primaryed = sum(educ=="Primary"),
                    hh.tot.noed=sum(educ=="No education"),
                    hh.num.resp=n()) %>%
             # grab only the first row (we only want one obs per hh)
             slice(1) %>%
             ungroup() %>%
             mutate(hh.lowest.inc = as.numeric(wealth=="Lowest"),
                    hh.second.inc = as.numeric(wealth=="Second"),
                    hh.third.inc = as.numeric(wealth=="Third"),
                    hh.fourth.inc = as.numeric(wealth=="Fourth"),
                    hh.highest.inc = as.numeric(wealth=="Highest")) %>%
             select(is.meal, hhid, cluster, region,
                    starts_with("hh."))
             
# double check that this worked...
stopifnot(sum(hh.covars$hh.tot.age) == sum(recoded.dat$age))
stopifnot(sum(hh.covars$hh.tot.f) == sum(recoded.dat$sex=="female"))

# what xBalance function calls "strata", i would call "blocks"
# so, in this case, that means we should use cluster id
# (and not the actual survey strata, which was the regions)
hb.balcheck <- xBalance(is.meal ~ hh.tot.age + hh.tot.f + 
                                  hh.tot.seconded + hh.tot.primaryed + hh.tot.noed + 
                                  hh.lowest.inc + hh.second.inc +
                                  hh.third.inc + hh.fourth.inc + hh.highest.inc +
                                  hh.num.resp,
                        strata=as.factor(hh.covars$cluster),
                        data=hh.covars,
                        report="all")

sink(file.path(out.dir, "hb-balance-check.log"))
print(hb.balcheck)
sink(NULL)




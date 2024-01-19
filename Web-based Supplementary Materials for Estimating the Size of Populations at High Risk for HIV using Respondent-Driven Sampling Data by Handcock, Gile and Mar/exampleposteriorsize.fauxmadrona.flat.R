library(size)
load("fauxmadrona.RData")
#
pdf("exampleposteriorsize.fauxmadrona.flat.pdf")
#
# The true population size is 1000.
#
# Pick the, err, the 77th network
ad <- fauxmadrona$degree    #degrees of sampled nodes in order of sampling
# The mean sample degree
mean(ad)
# The mean population degree
pop.deg <- sapply(fauxmadrona.network$iel,length)+sapply(fauxmadrona.network$oel,length)
mean(pop.deg)
table(pop.deg)
#
set.seed(1)
#
fit <- posteriorsize(s=ad, 
  K=20, # the maximum degree. Values greater than K are truncated to K.
  priorsizedistribution="flat", # Set the prior to be uniform
  maxN=10000,
  samplesize=25000,
  #parallel=FALSE,  #40,
  burnin=1000,interval=50)
# What was the median, mode and mean of the  prior for the population size?
fit$median.prior.size
fit$mode.prior.size
fit$mean.prior.size
fit$quartiles.prior.size
#
# Here are the abridged results
#
fitp <- fit
fitp$pop <- NULL
fitp$lpriorm <- NULL
fitp$sample <- NULL
fitp
#
# Some nice plots help make sense of it.
# The rest of this is post-processing
#
plot.size(fit, mcmc=TRUE)
plot.size(fit, N=1000)
summary.size(fit)

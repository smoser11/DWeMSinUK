# reproduce_bottleneck.R
# Matthew Salganik
# This file produces the bottleneck plots for all groups and cities
# It also produces the heatmaps that summarize the test results

# load in functions need to reproduce results
source("reproduce_functions.R")

# load and prepare data
set.seed(1)
source("load_and_prepare_dr_data.R")

print("*******************************************")
print("Begin bottleneck")
print("*******************************************")
gc()

print("*** Beginning bottleneck tests and plots ***")
estimates.and.envelopes <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))))
dim(estimates.and.envelopes) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(estimates.and.envelopes) <- list(d1=group.vec, d2=city.vec)

estimates.and.envelopes.perm <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))))
dim(estimates.and.envelopes.perm) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(estimates.and.envelopes.perm) <- list(d1=group.vec, d2=city.vec)

test.statistics <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))))
dim(test.statistics) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(test.statistics) <- list(d1=group.vec, d2=city.vec)

test.statistics.perm <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))))
dim(test.statistics.perm) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(test.statistics.perm) <- list(d1=group.vec, d2=city.vec)

for (city in city.vec) {
  for (group in group.vec) {
    print(paste("City:", city, ", Group:", group))
    current.df <- datasets[[group, city]]
    traits.to.consider <- get.traits.to.consider(group=group)
        
    estimates.and.envelopes[[group, city]] <- list()
    estimates.and.envelopes.perm[[group, city]] <- list()
    
    test.statistics.labels <- c("total.squared.deviation.weight.n", 
                                "total.squared.deviation.weight.sqrt.n",
                                "total.squared.deviation.jackknife.weight.n",
                                "total.squared.deviation.jackknife.weight.sqrt.n")
                                
    test.statistics[[group, city]] <- array(NA, dim=c(length(traits.to.consider), length(test.statistics.labels)),
                                            dimnames=list(d1=traits.to.consider, d2=test.statistics.labels))

    test.statistics.perm[[group, city]] <- array(NA, dim=c(length(traits.to.consider), num.perms, length(test.statistics.labels)),
                                            dimnames=list(d1=traits.to.consider, d2=NULL, d3=test.statistics.labels))
    
    for (trait.str in traits.to.consider) {
      estimates.and.envelopes[[group, city]][[trait.str]] <- 
        calculate.estimates.and.envelopes(degree.vector=current.df[, "P204r"], 
                                          trait.vector=current.df[, trait.str], 
                                          trait.value=1, 
                                          seed.vector=current.df[, "seed.of.origin"],
                                          is.seed.vector=current.df[, "IsSeed"], 
                                          VERBOSE=FALSE)
      
      test.statistics[[group, city]][trait.str, ] <- calculate.test.statistics(results.this.trait=estimates.and.envelopes[[group, city]][[trait.str]], test.statistics.labels=test.statistics.labels,
                                                                               VERBOSE=FALSE)
            
      perm.df <- list()
      estimates.and.envelopes.perm[[group, city]][[trait.str]] <- list()
      for (perm in 1:num.perms) {
        if (perm%%(num.perms/10)==0) print(paste("Beginning permutation:", perm))
        perm.df[[perm]] <- current.df

        # test statistic is determined by three things: seed.of.origin, weight, trait
        # if we permute trait, then we keep chain lengths and weights fixed
        perm.df[[perm]][!as.logical(perm.df[[perm]][, "IsSeed"]), trait.str] <- 
          sample(perm.df[[perm]][!as.logical(perm.df[[perm]][, "IsSeed"]), trait.str])
        # randomly permute seed.of.origin for all non-seeds
        #perm.df[[perm]][!as.logical(perm.df[[perm]][, "IsSeed"]), "seed.of.origin"] <- 
        #  sample(perm.df[[perm]][!as.logical(perm.df[[perm]][, "IsSeed"]), "seed.of.origin"])
        
        estimates.and.envelopes.perm[[group, city]][[trait.str]][[perm]] <- 
          calculate.estimates.and.envelopes(degree.vector= perm.df[[perm]][, "P204r"], 
                                            trait.vector= perm.df[[perm]][, trait.str], 
                                            trait.value=1, 
                                            seed.vector=perm.df[[perm]][, "seed.of.origin"],
                                            is.seed.vector=perm.df[[perm]][, "IsSeed"], 
                                            VERBOSE=FALSE)
        
        test.statistics.perm[[group, city]][trait.str, perm, test.statistics.labels] <- 
          calculate.test.statistics(results.this.trait=estimates.and.envelopes.perm[[group, city]][[trait.str]][[perm]],
                                    test.statistics.labels=test.statistics.labels, VERBOSE=FALSE)
        
        perm.df[[perm]] <- NA # clears up memory
        # TODO also possible clear estimates.and.envelopes.perm to save RAM
      }      
      estimates.and.envelopes.perm[[group, city]][[trait.str]] <- NA # clear memory
      gc()
    }
  }
}

p.values.test <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))))
dim(p.values.test) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(p.values.test) <- list(d1=group.vec, d2=city.vec)

fail.bottleneck.test <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))));
dim(fail.bottleneck.test) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
dimnames(fail.bottleneck.test) <- list(d1=group.vec, d2=city.vec)

# now do testing
critical.p.value <- 0.1
for (city in city.vec) {
  for (group in group.vec) {
    print(paste("City:", city, ", Group:", group))
    
    traits.to.consider <- get.traits.to.consider(group=group)
    
    p.values.test[[group, city]] <- array(NA, dim=c(length(traits.to.consider), length(test.statistics.labels)),
                                          dimnames=list(d1=traits.to.consider, d2=test.statistics.labels))
    
    fail.bottleneck.test[[group, city]] <- array(NA, dim=c(length(traits.to.consider), length(test.statistics.labels)),
                                                 dimnames=list(d1=traits.to.consider, d2=test.statistics.labels))
    for (trait.str in traits.to.consider) {
      for (test.statistic in test.statistics.labels) {
        #filename <- paste("figures/", group, "_", city, "_", trait.str, "_", test.statistic, "_bottlenecktest_dist.pdf", sep="")
        #plot.distribution.test(test.value=test.statistics[[group, city]][trait.str, test.statistic],
        #                       distribution=test.statistics.perm[[group, city]][trait.str, 1:num.perms, test.statistic],
        #                       filename=filename)
        
        p.values.test[[group, city]][trait.str, test.statistic] <- 
          mean(test.statistics[[group, city]][trait.str, test.statistic] <= test.statistics.perm[[group, city]][trait.str, 1:num.perms, test.statistic])
        
        fail.bottleneck.test[[group, city]][trait.str, test.statistic] <- (p.values.test[[group, city]][trait.str, test.statistic] < critical.p.value)
        
      }      
    }
  }
}


# now do bottleneck plots
p.values.to.plot <- c("total.squared.deviation.weight.n", 
                      "total.squared.deviation.jackknife.weight.n")
for (city in city.vec) {
  for (group in group.vec) {
    
    traits.to.consider <- get.traits.to.consider(group=group)
    current.df <- datasets[[group, city]]
    
    for (trait.str in traits.to.consider) {
      filename <- paste("figures/", group, "_", city, "_", trait.str, "_bottleneck.pdf", sep="")
      make.bottleneck.plot(results.this.trait=estimates.and.envelopes[[group, city]][[trait.str]],
                           trait.vector=current.df[, trait.str],
                           is.seed.vector=current.df[,"IsSeed"],
                           y.label.str=trait.str, 
                           p.values=p.values.test[[group, city]][trait.str, p.values.to.plot],
                           filename=filename,
                           weighted=TRUE,
                           VERBOSE=FALSE)
    } 
  }
}



# # post-process results for heatmap
test.statistic <- "total.squared.deviation.weight.n"
results.mat <- list()
for (group in group.vec) {
  results.mat[[group]] <- cbind(as.matrix(fail.bottleneck.test[[group, city.vec[1]]][ , test.statistic]),
                                as.matrix(fail.bottleneck.test[[group, city.vec[2]]][ , test.statistic]), 
                                as.matrix(fail.bottleneck.test[[group, city.vec[3]]][ , test.statistic]),
                                as.matrix(fail.bottleneck.test[[group, city.vec[4]]][ , test.statistic]))
  colnames(results.mat[[group]]) <- city.vec
  rownames(results.mat[[group]]) <- get.traits.to.consider(group=group, labels=TRUE)
  
  # convert from logical to numeric
  results.mat[[group]][(results.mat[[group]]==TRUE)] <- 1
  results.mat[[group]][(results.mat[[group]]==FALSE)] <- 0
  
  # "flip" so that HIV+ is on bottom (so it will be on top in plot)
  results.mat[[group]] <- results.mat[[group]][(nrow(results.mat[[group]]):1), ]
}
 
print("Mean failure rate for bottleneck test:")
print(sapply(X=results.mat, FUN=mean))
print(results.mat)
make.heatmap(results.mat=results.mat, 
             filename=paste("figures/12sites_heatmap_bottleneck_test.pdf", sep=""),
             VERBOSE=FALSE)

#rm(results.mat) # used for plotting
#rm(estimates.and.envelopes) # estiamted and envelopes
print("*** Finished bottleneck tests and plots ***")


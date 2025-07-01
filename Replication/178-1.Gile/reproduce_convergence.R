# reproduce_covergence_bottleneck_allpoints.R
# Matthew Salganik
# This file produces the convergence, bottleneck, and allpoints plots for all groups and cities
# It also produces the heatmaps that summarize the test results

# load in functions need to reproduce results
source("reproduce_functions.R")

# load and prepare data
set.seed(1)
source("load_and_prepare_dr_data.R")

print("*******************************************")
print("Begin convergence")
print("*******************************************")

max.wave <- 0
for (group in c("MSM", "DU", "FSW")) {
  for (city in c("SD", "SA", "BA" , "HI")) {
    max.wave <- max(max.wave, max(datasets[[group, city]][, "Wave"]))
  }
}
print(paste("Longest chain:", max.wave))

fail.convergence.test <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(fail.convergence.test) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
dimnames(fail.convergence.test) <- list(d1=group.vec, d2=city.vec)

# set critical values for flagging for convergence test
critical.t.vec <- c(50); # test is over the last critical.t records
critical.p.vec <- c(0.02, 0.05); 

for (city in city.vec) {
  for (group in group.vec) {
    print(paste("City:", city, ", Group:", group));
    current.df <- datasets[[group, city]];
    traits.to.consider <- get.traits.to.consider(group=group)
    
    fail.convergence.test[[group, city]] <- array(NA, 
                                                  dim=c(length(traits.to.consider), length(critical.t.vec), length(critical.p.vec)),
                                                  dimnames=list(d1=traits.to.consider, d2=as.character(critical.t.vec), d3=as.character(critical.p.vec)));
    
    for (trait.str in traits.to.consider) {
      # assumes records are sorted by time
      estimates.vec <- rds2.estimate(current.df[!current.df$IsSeed, 'P204r'], 
                                     current.df[!current.df$IsSeed, trait.str],
                                     1, running=TRUE)
      final.estimate <- estimates.vec[length(estimates.vec)]
      make.sample.dynamics.plot(group=group, city=city, trait.str=trait.str)
      
      for (critical.t in critical.t.vec) {
        for (critical.p in critical.p.vec) {
          fail.convergence.test[[group, city]][trait.str, as.character(critical.t), as.character(critical.p)] <- any( abs(estimates.vec[(length(estimates.vec) - critical.t + 1):length(estimates.vec)] - final.estimate) > critical.p );
        }
      }
      
    } # trait loop
  } # group loop
} # city loop     

# print selected results
vector.of.indexes <- vector.of.indexes <- 1:nrow(datasets[["DU", "BA"]])
vector.of.non.seed.indexes <- vector.of.indexes[!(datasets[["DU", "BA"]][, "IsSeed"])]
print("DU, Barahona, use drugs every day, first 50 respondents:")
cases.to.use <- vector.of.non.seed.indexes[1:50]
print(rds2.estimate(degree.vector=datasets[["DU", "BA"]][cases.to.use, "P204r"], 
                    trait.vector=datasets[["DU", "BA"]][cases.to.use, "Use.Drugs.Every.Day"],
                    trait.value=1, running=FALSE))
print("DU, Barahona, use drugs every day, final 50 respondents:")
cases.to.use <- vector.of.non.seed.indexes[(length(vector.of.non.seed.indexes) - 50 + 1):length(vector.of.non.seed.indexes)]
print(rds2.estimate(degree.vector=datasets[["DU", "BA"]][cases.to.use, "P204r"], 
                    trait.vector=datasets[["DU", "BA"]][cases.to.use, "Use.Drugs.Every.Day"],
                    trait.value=1, running=FALSE))
rm(cases.to.use, vector.of.indexes, vector.of.non.seed.indexes)

# process data to prepare to pass to heatmap
for (epsilon in critical.p.vec) {
  results.mat <- list();
  for (group in group.vec) {
    results.mat[[group]] <- cbind(as.matrix(fail.convergence.test[[group, city.vec[1]]][, , as.character(epsilon)]),
                                  as.matrix(fail.convergence.test[[group, city.vec[2]]][, , as.character(epsilon)]), 
                                  as.matrix(fail.convergence.test[[group, city.vec[3]]][, , as.character(epsilon)]),
                                  as.matrix(fail.convergence.test[[group, city.vec[4]]][, , as.character(epsilon)]));
    colnames(results.mat[[group]]) <- city.vec
    rownames(results.mat[[group]]) <- get.traits.to.consider(group=group, labels=TRUE)
    
    # convert from logical to numeric
    results.mat[[group]][(results.mat[[group]]==TRUE)] <- 1
    results.mat[[group]][(results.mat[[group]]==FALSE)] <- 0
    
    # "flip" so that HIV+ is on bottom (so it will be on top in plot)
    results.mat[[group]] <- results.mat[[group]][(nrow(results.mat[[group]]):1), ]
  }
  
  make.heatmap(results.mat=results.mat, 
               filename=paste("figures/12sites_heatmap_convergence_test_", as.character(epsilon), ".pdf", sep=""))
  
  # print summary of tests
  print(paste("results.mat for epsilon:", epsilon))
  print(results.mat)
  
  print(paste("Convergence test failure rate MSM (tau = 50, epsilon =", epsilon,"):"))
  print(mean(results.mat[["MSM"]]))
  print(paste("Number of failed tests:", sum(results.mat[["MSM"]])))
  print(paste("Convergence test failure rate FSW (tau = 50, epsilon =", epsilon, "):"))
  print(mean(results.mat[["FSW"]]))
  print(paste("Number of failed tests:", sum(results.mat[["FSW"]])))
  print(paste("Convergence test failure rate DU (tau = 50, epsilon=", epsilon, "):"))
  print(mean(results.mat[["DU"]]))
  print(paste("Number of failed tests:", sum(results.mat[["DU"]])))
}    

rm(results.mat)
rm(critical.t.vec, critical.p.vec) # clean-up

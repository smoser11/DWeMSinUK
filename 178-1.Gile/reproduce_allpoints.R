# reproduce_allpoints.R
# Matthew Salganik
# This file produces the allpoints plots for all groups and cities

# load in functions need to reproduce results
source("reproduce_functions.R")

# load and prepare data
set.seed(1)
source("load_and_prepare_dr_data.R")

print("*******************************************")
print("Begin allpoints")
print("*******************************************")

# Define traits to be used in tests
common.trait.str.vec <- c("HIVPositive", "SYPHPositive", "Had.HIV.Test");
common.trait.str.labels.vec <- c("HIV+", "Syphilis+", "Had HIV Test");
du.trait.vec <- c("Working", "Main.Drug.Crack", "Main.Drug.Cocaine", "Main.Drug.Marijuana", "Female", "Risky.Sex", "Use.Drugs.Every.Day", "Been.Imprisoned", "Paid.A.Woman.For.Sex");
du.trait.labels.vec <- c("Working", "Main Drug Crack", "Main Drug Cocaine", "Main Drug Marijuana", "Female", "Risky Sex", "Use Drugs Every Day", "Been Imprisoned", "Paid For Sex");
fsw.trait.vec <- c("Use.Condom.Last.Client", "Use.Drugs", "Last.Client.Street", "Last.Client.Brothel", "Participated.In.Program");
fsw.trait.labels.vec <- c("Used Condom", "Use Drugs", "Last Client Street", "Last Client Brothel", "Been In Program");
msm.trait.vec <- c("Working", "Use.Drugs", "Heterosexual", "Bisexual", "Trans", "Used.Condom", "Sex.With.Woman");
msm.trait.labels.vec <- c("Working", "Use Drugs", "Heterosexual", "Bisexual", "Trans", "Used Condom", "Sex With Woman");

for (city in city.vec) {
  for (group in group.vec) {
    print(paste("City:", city, ", Group:", group));
    current.df <- datasets[[group, city]];
    if (group=="DU") traits.to.consider <- c(common.trait.str.vec, du.trait.vec);
    if (group=="FSW") traits.to.consider <- c(common.trait.str.vec, fsw.trait.vec);
    if (group=="MSM") traits.to.consider <- c(common.trait.str.vec, msm.trait.vec);
    
    for (trait.str in traits.to.consider) {
      
      filename <- paste("figures/", group, "_", city, "_", trait.str, "_allpoints.pdf", sep="")
      make.allpoints.plot(trait.vector=current.df[, trait.str],
                          seed.of.origin.vector=current.df[, "seed.of.origin"],
                          degree.vector=current.df[,"P204r"],
                          filename=filename)
      rm(filename, trait.str)  
    }
  }
}  


print("Final estimate for MSM-HI Heterosexual:")
cases.to.use <- !(datasets[["MSM", "HI"]][, "IsSeed"]);
print(rds2.estimate(degree.vector=datasets[["MSM", "HI"]][cases.to.use, "P204r"], 
                    trait.vector=datasets[["MSM", "HI"]][cases.to.use, "Heterosexual"],
                    trait.value=1, running=FALSE))
rm(cases.to.use)
print("************************************************")
print("Finished allpoints")
print("*************************************************")


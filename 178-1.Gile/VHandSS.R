# VHandSS.R
# Krista Gile
# Compare SS and VH:  
# This file has several sections.  To get to particular section, search "Section X"
# 1:  Setup: Create upper and lower limits of population sizes from data in literature, and create vector of characteristics to estimate.
# 2:  Compute SS estiamtes for minimum popluation sizes from ranges from literature, and VH estimates.
# 3:  Use size package to estimate population sizes.
# 4:  Find the SS and VH estimates using the posterior HPD and mean estimates.
# 5:  Plot and table

#############################################
#############################################
# Section 1:  Setup: Create upper and lower limits of population sizes from data in literature, and create vector of characteristics to estimate.
#############################################
#############################################

#####  Population Sizes ######
# These are from literature - total, men, and women in each city
pops<-matrix(0,4,3)
pops[1,]<-c(1082455, 517297, 565158)
pops[2,]<-c(726338, 357160, 369178)
pops[3,]<-c(87799, 42854, 44945)
pops[4,]<-c(196867, 99508, 97359)
dimnames(pops)[[1]]<-c("SD", "SA", "BA", "HI")
dimnames(pops)[[2]]<-c("All", "Men", "Women")

popsage<-pops*.6  # 60% of population within applicable ages

msmlow<-popsage[,2]*.01
msmhigh<-popsage[,2]*.08

duhigh<-.10*popsage[,1]
dulow<-.01*popsage[,1]
fswhigh<-.10*popsage[,3]
fswlow<-.01*popsage[,3]

all_low<-rbind(fswlow, dulow, msmlow)
all_high<-rbind(fswhigh, duhigh, msmhigh)

save.image(file="DRsizes.RData")

#######  Variables and Variable Names  #######
city.vec <- c("SD", "SA", "BA", "HI");
group.vec <- c("FSW", "DU", "MSM");

city.vec.long <- c("Santo Domingo", "Santiago", "Barahona", "Higuey");
group.vec.long <- c("Female sex workers", "Drug users", "MSM");

# load datasets
datasets <- as.list(array(NA, dim=c(length(city.vec), length(group.vec)))); # stores datasets
dim(datasets) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    filename <- paste("g", group.counter, "p", city.counter, "v03.RData", sep="");
    load(filename); rm(filename);
    datasets[[group.counter, city.counter]] <- DF;
  }
}

# create title array
title.array <- array(NA, dim=c(length(group.vec), length(city.vec)));
short.title.array <- array(NA, dim=c(length(group.vec), length(city.vec)));
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    title.array[group.counter, city.counter] <- paste(group.vec.long[group.counter], ", ", city.vec.long[city.counter], sep="");
    short.title.array[group.counter, city.counter] <- paste(group.vec[group.counter], ", ", city.vec[city.counter], sep="");
  }
}


for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    ### recode 0 network sizes to missing data
    network.questions.to.recode <- c('P201r', 'P202r', 'P203r', 'P204r', 'RE_P9Ar', 'RE_P9Br', 'RE_P9Cr','RE_P9Dr');
    print(title.array[group.counter,city.counter]);
    print("------------------------")
    for (network.question in network.questions.to.recode) {
      network.size.zero <- (!is.na(datasets[[group.counter, city.counter]][, network.question])) & (datasets[[group.counter, city.counter]][ , network.question]==0);
      datasets[[group.counter, city.counter]][network.size.zero, network.question] <- NA;
      print(paste("Number of times", network.question , "reassigned from 0 to NA:", sum(network.size.zero)));
    }
    print("------------------------")
    ### finishing recoding 0 network sizes to missing data
    ### create a new variables which has RE_P9Dr or if that is NA it has P204r (minimum possible error)
    datasets[[group.counter, city.counter]][ ,'RE_P9Dr_bestcase'] <-  datasets[[group.counter, city.counter]][ ,'RE_P9Dr'];
      # where retest is NA take test
    entries.to.change <- (is.na(datasets[[group.counter, city.counter]][, 'RE_P9Dr']));
    datasets[[group.counter, city.counter]][entries.to.change, 'RE_P9Dr_bestcase'] <-  datasets[[group.counter, city.counter]][entries.to.change ,'P204r'];
    ###

   ## recode trats: DU
    if (group.vec[group.counter]=="DU") {
      datasets[[group.counter, city.counter]][,"Had.HIV.Test"] <- as.numeric(datasets[[group.counter, city.counter]][,"P822"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA    
      datasets[[group.counter, city.counter]][,"Working"] <- as.numeric(datasets[[group.counter, city.counter]][,"P113"]==2); # recode 2 -> 1, 1 -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Main.Drug.Crack"] <- as.numeric(datasets[[group.counter, city.counter]][,"P304"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Main.Drug.Marijuana"] <- as.numeric(datasets[[group.counter, city.counter]][,"P304"]==1); # recode 1 -> 1, everything else -> 0; missing data goes to NA     
      datasets[[group.counter, city.counter]][,"Main.Drug.Cocaine"] <- as.numeric(datasets[[group.counter, city.counter]][,"P304"]==3); # recode 3 -> 1, everything else -> 0; missing data goes to NA     
      datasets[[group.counter, city.counter]][,"Female"] <- as.numeric(datasets[[group.counter, city.counter]][,"H004"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA     
      datasets[[group.counter, city.counter]][,"Group.Sex"] <- as.numeric(datasets[[group.counter, city.counter]][,"P539"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Risky.Sex"] <- as.numeric(datasets[[group.counter, city.counter]][,"P505"]==1); # recode 1 -> 1, everything else 0; people who used a condom not once
      HAD.SEX <- (datasets[[group.counter, city.counter]][,"P502"]==2);  # people who have had sex in the last month (P502==2)
      HAD.SEX[is.na(HAD.SEX)] <- FALSE;  # people who are NA, we will assume they did not have sex
      datasets[[group.counter, city.counter]][!HAD.SEX, "Risky.Sex"] <- 0; # people who did not report having sex did not have risky sex
      for (row in 1:length(datasets[[group.counter, city.counter]][,"Risky.Sex"])) {
        if (is.na(datasets[[group.counter, city.counter]][row, "Risky.Sex"])) {
          # only way Risky.Sex should be NA is if P502 is 2 and P505 is NA
          if ((datasets[[group.counter, city.counter]][row, "P502"]==2) & (is.na(datasets[[group.counter, city.counter]][row, "P505"]))) {
            # OK
          } else {
            print(datasets[[group.counter, city.counter]][row, "P502"]);
            print(datasets[[group.counter, city.counter]][row, "P505"]);
            print(datasets[[group.counter, city.counter]][row ,"Risky.Sex"]);
          }
        }
      }
      rm(HAD.SEX);
      datasets[[group.counter, city.counter]][,"Use.Drugs.Every.Day"] <- as.numeric(datasets[[group.counter, city.counter]][,"P305"]==4); # recode 4 -> 1, everything else 0; missing goes to NA
      datasets[[group.counter, city.counter]][,"Paid.A.Woman.For.Sex"] <- as.numeric(datasets[[group.counter, city.counter]][,"P516"]==2); # recode 2 -> 1, everything else 0; missing goes to NA
      datasets[[group.counter, city.counter]][,"Was.Paid.For.Sex.With.Man"] <- as.numeric(datasets[[group.counter, city.counter]][,"P533"]==2); # recode 2 -> 1, everything else 0; missing goes to NA
      datasets[[group.counter, city.counter]][,"Been.Imprisoned"] <- as.numeric(datasets[[group.counter, city.counter]][,"P703"]==2); # recode 2 -> 1, everything else 0; missing goes to NA
    }

    ## recode trats: FSW
    if (group.vec[group.counter]=="FSW") {
      datasets[[group.counter, city.counter]][,"Had.HIV.Test"] <- as.numeric(datasets[[group.counter, city.counter]][,"P1323"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA

      datasets[[group.counter, city.counter]][,"Has.Pimp"] <- as.numeric(datasets[[group.counter, city.counter]][,"P618"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Use.Condom.Last.Client"] <- as.numeric(datasets[[group.counter, city.counter]][,"P704"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Last.Client.Street"] <- as.numeric(datasets[[group.counter, city.counter]][,"P604"] %in% c(7,8)); # recode 7 and 8 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Last.Client.Brothel"] <- as.numeric(datasets[[group.counter, city.counter]][,"P604"]==1); # recode 1 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Participated.In.Program"] <- as.numeric(datasets[[group.counter, city.counter]][,"P1503"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA

      datasets[[group.counter, city.counter]][,"Use.Drugs"] <- as.numeric(datasets[[group.counter, city.counter]][,"P305"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      USE.DRUGS.EVER <- (datasets[[group.counter, city.counter]][,"P303"]==2);  
      USE.DRUGS.EVER[is.na(USE.DRUGS.EVER)] <- FALSE;  # people who are NA, we will assume they have not used drugs
      datasets[[group.counter, city.counter]][!USE.DRUGS.EVER, "Use.Drugs"] <- 0; # people who did not report using drugs did not use drugs in the past 30 days
      rm(USE.DRUGS.EVER); # clean-up
    }
    ## recode trats: MSM
    if (group.vec[group.counter]=="MSM") {
      datasets[[group.counter, city.counter]][,"Had.HIV.Test"] <- as.numeric(datasets[[group.counter, city.counter]][,"P1222"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Working"] <- as.numeric(datasets[[group.counter, city.counter]][,"P113"]==2); # recode 2 -> 1, 1 -> 0; missing data goes to NA

      datasets[[group.counter, city.counter]][,"Used.Condom"] <- as.numeric(datasets[[group.counter, city.counter]][,"P607"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Bisexual"] <- as.numeric(datasets[[group.counter, city.counter]][,"P105"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Heterosexual"] <- as.numeric(datasets[[group.counter, city.counter]][,"P105"]==3); # recode 3 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Trans"] <- as.numeric(datasets[[group.counter, city.counter]][,"P105"] %in% c(4,5,6)); # recode 4,5,6 -> 1, everything else -> 0; missing data goes to NA
      datasets[[group.counter, city.counter]][,"Sex.With.Woman"] <- as.numeric(datasets[[group.counter, city.counter]][,"P701"]!=0); # recode everything but 0 to 1; people who don't know get recoded to having had sex with a woman    
      datasets[[group.counter, city.counter]][,"Use.Drugs"] <- as.numeric(datasets[[group.counter, city.counter]][,"P305"]==2); # recode 2 -> 1, everything else -> 0; missing data goes to NA
      USE.DRUGS.EVER <- (datasets[[group.counter, city.counter]][,"P303"]==2);  
      USE.DRUGS.EVER[is.na(USE.DRUGS.EVER)] <- FALSE;  # people who are NA, we will assume they have not used drugs
      datasets[[group.counter, city.counter]][!USE.DRUGS.EVER, "Use.Drugs"] <- 0; # people who did not report using drugs did not use drugs in the past 30 days
      rm(USE.DRUGS.EVER); # clean-up
    }
}
}	

   rm(network.question, network.questions.to.recode, network.size.zero); # clean up

common.trait.str.vec <- c("HIVPositive", "SYPHPositive", "Had.HIV.Test");
common.trait.str.labels.vec <- c("HIV+", "Syphilis+", "Had HIV Test");
du.trait.vec <- c("Working", "Main.Drug.Crack", "Main.Drug.Cocaine", "Main.Drug.Marijuana", "Female", "Risky.Sex", "Use.Drugs.Every.Day", "Been.Imprisoned", "Paid.A.Woman.For.Sex");
du.trait.labels.vec <- c("Working", "Main Drug Crack", "Main Drug Cocaine", "Main Drug Marijuana", "Female", "Risky Sex", "Use Drugs Every Day", "Been Imprisoned", "Paid For Sex");
fsw.trait.vec <- c("Use.Condom.Last.Client", "Use.Drugs", "Last.Client.Street", "Last.Client.Brothel", "Participated.In.Program");
fsw.trait.labels.vec <- c("Used Condom", "Use Drugs", "Last Client Street", "Last Client Brothel", "Been In Program");
msm.trait.vec <- c("Working", "Use.Drugs", "Heterosexual", "Bisexual", "Trans", "Used.Condom", "Sex.With.Woman");
msm.trait.labels.vec <- c("Working", "Use Drugs", "Heterosexual", "Bisexual", "Trans", "Used Condom", "Sex With Woman");
 
save.image(file="namedvarsfile.RData")

#############################################
#############################################
# Section 2:  Compute SS estiamtes for minimum popluation sizes from ranges from literature, and VH estimates.
#############################################
#############################################

load(file="namedvarsfile.RData")
library(RDS)

ss.estimates <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(ss.estimates) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
vh.estimates <- as.list(array(NA, dim=c(length(group.vec), length(city.vec))));
dim(vh.estimates) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list

for (city.counter in (1:length(city.vec))) {
  for (group.counter in (1:length(group.vec))) {
    print(paste("City:", city.vec[city.counter]))
    print(paste("Group:", group.vec[group.counter]))
    current.df <- datasets[[group.counter, city.counter]];
    if (group.vec[group.counter]=="DU") traits.to.consider <- c(common.trait.str.vec, du.trait.vec);
    if (group.vec[group.counter]=="FSW") traits.to.consider <- c(common.trait.str.vec, fsw.trait.vec);
    if (group.vec[group.counter]=="MSM") traits.to.consider <- c(common.trait.str.vec, msm.trait.vec);

	N<-max(all_low[group.counter,city.counter], dim(current.df)[1])
    mmm<-dr.to.rds.data.frame(current.df,N=N,deg="P204r")
	
	ss.estimates[[group.counter,city.counter]]<-rep(NA, length(traits.to.consider))
	vh.estimates[[group.counter,city.counter]]<-rep(NA, length(traits.to.consider))


    for (trait.counter in 1:length(traits.to.consider)) {
      title.str <- paste(city.vec[city.counter], group.vec[group.counter], traits.to.consider[trait.counter]);
	print(title.str)

      ss.estimates[[group.counter, city.counter]][trait.counter]<-SS.estimates(mmm, outcome.variable = traits.to.consider[trait.counter], N=N)@estimate
      vh.estimates[[group.counter, city.counter]][trait.counter]<-RDS.II.estimates(mmm, outcome.variable = traits.to.consider[trait.counter])@estimate

}

}

save(ss.estimates, vh.estimates, file="vhandss.RData")    
    
}

#############################################
#############################################
# Section 3:  Use size package to estimate population sizes.
#############################################
#############################################
library(size)
# note that this package is still under development, so there is a chance syntax will change slightly
library(coda)

load(file="DRsizes.RData")

groups<-c("FSW","DU","MSM")
cities<-c("SD","SA","BA","HI")


for(group.counter in c(1:3)){
for(city.counter in c(1:4)){

	stem<-paste(groups[group.counter],cities[city.counter],sep="")
	stem2<-paste("g", group.counter, "p", city.counter,sep="")
	print(stem)	
	pdf(paste("DRposteriorsize",stem2,".pdf",sep=""))	
		
	load(file=paste("g", group.counter, "p", city.counter,"v03.RData", sep=""))
	ad<-DF$P204r  #all degrees - in order.
	ad<-ad[!is.na(ad)]

	mean(ad)

	set.seed(1)
	minN<-max(length(ad),all_low[group.counter,city.counter])
	maxN<-all_high[group.counter,city.counter]

	post <- posteriorsize(s=ad, mean.prior.degree=4, sd.prior.degree=4,
  		K=20, # the maximum degree. Values greater than K are truncated to K.
		samplesize=1000, burnin=1000,interval=500, quartiles.prior.size=c(minN,maxN),
  		parallel=50)

	table(post$sample[,"N"])

	postp <- post
	postp$pop <- NULL
	postp$lpriorm <- NULL
	postp

	out <- post$sample

	hpd <- HPDinterval(post$sample[,"N"])[1:2]

	out[is.na(out)] <- apply(out,2,median,na.rm=T)
	save.image(paste("DRposteriorsize",stem2,".RData",sep=""))
	dev.off()
}}

##  Now combine all of the estimates:  

minhpd<-matrix(0,3,4)
maxhpd<-matrix(0,3,4)
postmean<-matrix(0,3,4)

for(group.counter in 1:3){
	for(city.counter in 1:4){
    load(file=paste("DRposteriorsizeg",group.counter,"p",city.counter, ".RData", sep=""))
    minhpd[group.counter,city.counter]<-hpd[1]
    maxhpd[group.counter,city.counter]<-hpd[2]
    postmean[group.counter,city.counter]<-round(mean(out[,"N"],na.rm=T),0)
    }
}

save(minhpd,maxhpd, postmean, all_high, all_low, file="DRpostsizes.RData")

#############################################
#############################################
# Section 4:  Find the SS and VH estimates using the posterior HPD and mean estimates.
#############################################
#############################################
library(RDS)

load(file="namedvarsfile.RData")
load(file="vhandss.RData") # for comparison
load(file="DRpostsizes.RData")

#initialize
ss.estimates.minhpd <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(ss.estimates.minhpd) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
ss.estimates.maxhpd <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(ss.estimates.maxhpd) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
ss.estimates.postmean <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(ss.estimates.postmean) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list


for (city.counter in (1:length(city.vec))) {
  for (group.counter in (1:length(group.vec))) {
    print(paste("City:", city.vec[city.counter]))
    print(paste("Group:", group.vec[group.counter]))
    current.df <- datasets[[group.counter, city.counter]];
    if (group.vec[group.counter]=="DU") traits.to.consider <- c(common.trait.str.vec, du.trait.vec);
    if (group.vec[group.counter]=="FSW") traits.to.consider <- c(common.trait.str.vec, fsw.trait.vec);
    if (group.vec[group.counter]=="MSM") traits.to.consider <- c(common.trait.str.vec, msm.trait.vec);

	N<-max(minhpd[group.counter,city.counter], dim(current.df)[1])
    mmm<-dr.to.rds.data.frame(current.df,N=N,deg="P204r")
	
	ss.estimates.minhpd[[group.counter,city.counter]]<-rep(NA, length(traits.to.consider))
	
    for (trait.counter in 1:length(traits.to.consider)) {
      title.str <- paste(city.vec[city.counter], group.vec[group.counter], 
      	traits.to.consider[trait.counter]);
		print(title.str)

      ss.estimates.minhpd[[group.counter, city.counter]][trait.counter]<-SS.estimates(mmm, 
      	outcome.variable = traits.to.consider[trait.counter], N=N)@estimate
   	}

	N<-max(maxhpd[group.counter,city.counter], dim(current.df)[1])
    mmm<-dr.to.rds.data.frame(current.df,N=N,deg="P204r")
	
	ss.estimates.maxhpd[[group.counter,city.counter]]<-rep(NA, length(traits.to.consider))
	
    for (trait.counter in 1:length(traits.to.consider)) {
      title.str <- paste(city.vec[city.counter], group.vec[group.counter], 
      	traits.to.consider[trait.counter]);
	  print(title.str)

      ss.estimates.maxhpd[[group.counter, city.counter]][trait.counter]<-SS.estimates(mmm, 
      	outcome.variable = traits.to.consider[trait.counter], N=N)@estimate
   	}

	N<-max(postmean[group.counter,city.counter], dim(current.df)[1])
    mmm<-dr.to.rds.data.frame(current.df,N=N,deg="P204r")
	
	ss.estimates.postmean[[group.counter,city.counter]]<-rep(NA, length(traits.to.consider))
	
    for (trait.counter in 1:length(traits.to.consider)) {
      title.str <- paste(city.vec[city.counter], group.vec[group.counter], 		
      	traits.to.consider[trait.counter]);
	  print(title.str)

      ss.estimates.postmean[[group.counter, city.counter]][trait.counter]<-SS.estimates(mmm, 
      	outcome.variable = traits.to.consider[trait.counter], N=N)@estimate   
	}
}

save(ss.estimates, vh.estimates, ss.estimates.minhpd, ss.estimates.maxhpd, ss.estimates.postmean, file="vhandsspost.RData")    
}

#############################################
#############################################
# Section 5:  Plot and Table.
#############################################
#############################################

load(file="vhandsspost.RData")
load(file="namedvarsfile.RData")
legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

dif.minhpd <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(dif.minhpd) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
dif.postmean <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(dif.postmean) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
dif.priormin <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))); 
dim(dif.priormin) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list

for (city.counter in (1:length(city.vec))) {
  for (group.counter in (1:length(group.vec))) {
  	 vh.dat<-vh.estimates[[group.counter,city.counter]]
  	 dif.postmean[[group.counter, city.counter]]<-ss.estimates.postmean[[group.counter, city.counter]]-vh.dat
    dif.minhpd[[group.counter, city.counter]]<-ss.estimates.minhpd[[group.counter, city.counter]]-vh.dat
    dif.priormin[[group.counter, city.counter]]<-ss.estimates[[group.counter, city.counter]]-vh.dat  	
 }
}


all.minhpd<-NULL
all.postmean<-NULL
all.priormin<-NULL
for(i in 1:3){
  for(j in 1:4){
    all.minhpd<-c(all.minhpd,dif.minhpd[[i,j]])
    all.postmean<-c(all.postmean,dif.postmean[[i,j]])
    if(i==3){
      all.priormin<-c(all.priormin,dif.priormin[[i,j]])
    }
  }
}

## Figure 4
pdf(file="SSVHhistminHPD.pdf", height=4, width=7)
  par(mar=c(5,5,0,0))
  hist(all.minhpd, xlab="Difference:  SS (Lower HPD bound) -VH", ylab="Frequency", main="")
dev.off()

## Figure S4(a)
pdf(file="SSVHhistpostmean.pdf", height=4, width=7)
  par(mar=c(5,5,0,0))
  hist(all.postmean, xlab="Difference:  SS (Posterior Mean) -VH", ylab="Frequency", main="")
dev.off()

## Figure S4(b)
pdf(file="SSVHhistpriormin.pdf", height=4, width=7)
  par(mar=c(5,5,0,0))
  hist(all.priormin, xlab="Difference:  SS (Prior Minimum) -VH", ylab="Frequency", main="")
dev.off()


## Figures S5

s1<-8 # symbol for prior min (1%)
s2<-1 # symbol for posterior mean
s3<-6 # symbol for posterior Min Hpd

legat.group<-c("topleft","topright", "bottomleft")

for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    
    pdf(file=paste("SSVH",city.vec[city.counter], "-", group.vec[group.counter], 
                   ".pdf", sep=""))  
        
    alldif<-cbind(dif.priormin[[group.counter,city.counter]], 
                  dif.postmean[[group.counter, city.counter]], 
                  dif.minhpd[[group.counter,city.counter]])
    
    
    thisvh<-cbind(vh.estimates[[group.counter, city.counter]], 
                  vh.estimates[[group.counter, city.counter]], 
                  vh.estimates[[group.counter, city.counter]])
    
    symb<-alldif
    symb[,1]<-s1
    symb[,2]<-s2
    symb[,3]<-s3
    
    if(group.counter!=3){
      alldif<-alldif[,c(2:3)]
      thisvh<-thisvh[,c(2:3)]
      symb<-symb[,2:3]
    }
    
    par(mar=c(6,6,3.1,2.1))    
    plot(thisvh,alldif, pch=symb, ylab="SS-VH", xlab="VH Estimate",  
         main=paste(city.vec[city.counter], "-", group.vec[group.counter]), 
         ylim=c(-.02, .03), xlim=c(0,1), cex.lab=2, cex.axis=2, cex=2, cex.main=2)
    if(group.counter==3){
      legend(x=legat.group[group.counter], horiz=F, 
             legend=c("Post. Mean", "Min HPD", "1%"), pch=c(s2,s3,s1), cex=2)
    }else{
      legend(x=legat.group[group.counter], horiz=F, 
             legend=c("Post. Mean", "Min HPD"), pch=c(s2,s3), cex=2)
    }
    abline(h=0)
    dev.off()    
  }}

## Table S1
fsw.traits<-c(common.trait.str.vec, fsw.trait.vec)
du.traits<-c(common.trait.str.vec, du.trait.vec)
msm.traits<-c(common.trait.str.vec, msm.trait.vec)
all.traits<-list(fsw.traits, du.traits, msm.traits)
all.traits.labels<-list(c(common.trait.str.labels.vec,fsw.trait.labels.vec), c(common.trait.str.labels.vec,du.trait.labels.vec), c(common.trait.str.labels.vec,msm.trait.labels.vec))

outtable<-matrix(0,0,8)
for (group.counter in (1:length(group.vec))) {
   for (city.counter in (1:length(city.vec))) {
     dif.postm<-dif.postmean[[group.counter, city.counter]]
     dif.minh<-dif.minhpd[[group.counter, city.counter]]
     dif.pri<-dif.priormin[[group.counter, city.counter]]
	  if(group.counter==3){bads<-which(abs(dif.postm) > .01 | abs(dif.minh) >.01 | abs(dif.pri) > .01)
	  	}else{bads<-which(abs(dif.postm) > .01 | abs(dif.minh) >.01)}
	  for(j in bads){
	  	ests<-c(vh.estimates[[group.counter, city.counter]][j], ss.estimates.maxhpd[[group.counter, city.counter]][j], ss.estimates.postmean[[group.counter, city.counter]][j], ss.estimates.minhpd[[group.counter, city.counter]][j], ss.estimates[[group.counter, city.counter]][j])
	  	newline<-c(group.vec[group.counter], city.vec[city.counter], all.traits.labels[[group.counter]][j], round(ests,3))
	  	if(group.counter!=3){newline[8]<-"-"}
	  	outtable<-rbind(outtable,newline)
	  }
}}

dimnames(outtable)<-NULL
outtable

library(xtable)
xtable(outtable)



 

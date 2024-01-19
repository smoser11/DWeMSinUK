# reproduce_network_size.R
# Matthew Salganik
# make network size calcualtions and graphs


print("*******************************************")
print("Begin network size stuff")
print("*******************************************")

# load in functions need to reproduce results
source("reproduce_functions.R")

# load and prepare data
set.seed(1)
source("load_and_prepare_dr_data.R")

# calculate correlations
test.degree.vec <- c("P201r","P202r","P203r", "P204r")
retest.degree.vec <- c("RE_P9Ar", "RE_P9Br", "RE_P9Cr", "RE_P9Dr")
pearson.cor.coef <- array(NA, 
                          dim=c(length(group.vec), length(city.vec), length(test.degree.vec)),
                          dimnames=list(group.vec, city.vec, test.degree.vec))
spearman.cor.coef <- array(NA, 
                           dim=c(length(group.vec), length(city.vec), length(test.degree.vec)),
                           dimnames=list(group.vec, city.vec, test.degree.vec))

for (group in group.vec) {
  for (city in city.vec) {
    for (degree.counter in (1:length(test.degree.vec))) {
      spearman.cor.coef[group, city, degree.counter] <- 
        cor(datasets[[group, city]][ , test.degree.vec[degree.counter] ],
            datasets[[group, city]][ , retest.degree.vec[degree.counter] ],
            use="complete.obs", method="spearman")      
    }
  }
}


print("spearman correlation coefficients")

print(paste("min degree correlation:", min(spearman.cor.coef[ , ,"P204r"])))
print(paste("max degree correlation:", max(spearman.cor.coef[ , ,"P204r"])))
print("Median degree correlation for the three groups")
print(apply(spearman.cor.coef[ , ,"P204r"], 1, median))


######################################
# 12 sites for main degree question

pdf(file="figures/12sites_spearman_P204r.pdf")
x.counter <- 0;
group.mag <- 1.8;
plot(c(0,1), c(0,1), xlim=c(1,14), ylim=c(0,1), ylab="Spearman correlation", xlab="", 
     type="n", xaxt="n");
for (group in group.vec) {
  x.counter <- x.counter + 1;
  for (city in city.vec) {
    points(x.counter, spearman.cor.coef[group, city, 'P204r'], pch=19);
    x.counter <- x.counter + 1;
  }
  #print(x.counter);
  # plot median
  #print(c(x.counter-length(city.vec), x.counter));
  #print(rep(median(spearman.cor.coef[group, , 'P204r']), 2));
  lines(c(x.counter-length(city.vec), x.counter), 
        rep(median(spearman.cor.coef[group, , 'P204r']), 2),
        lty=3);
}
axis(1, at=c(1,2,3,4,6,7,8,9,11,12,13,14), lab=rep(city.vec, length(group.vec)));
mtext(group.vec[1], side=1, at=2.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[2], side=1, at=7.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[3], side=1, at=12.5, outer=FALSE, line=3, cex=group.mag)
box();
rm(x.counter, group.mag); # clean up
dev.off()
########################################


######################################
# 12 sites for all degree questions

pdf(file="figures/12sites_spearman_correlation.pdf")
x.counter <- 0
group.mag <- 1.8
fuzz <- 0.1;
plot(c(0,1), c(0,1), xlim=c(1,14), ylim=c(0,1), 
     xlab="", ylab="Spearman correlation", type="n", xaxt="n", main="",
     cex.axis=1, cex.lab=1)
for (group in group.vec) {
  x.counter <- x.counter + 1
  for (city in city.vec) {
    spacing.vec <- c(-1.5, -0.5, 0.5, 1.5);
    for (i in spacing.vec) {
      # vertical lines
      lines(c(x.counter + (i * fuzz), x.counter + (i * fuzz)),
            c(0, spearman.cor.coef[group, city, i==spacing.vec ]), lty=3)
    }
    points(c(x.counter + spacing.vec[1] * fuzz, x.counter+spacing.vec[2] * fuzz, x.counter + spacing.vec[3] * fuzz),
           spearman.cor.coef[group, city, 1:3], pch=21, bg="white");
    points(c(x.counter + spacing.vec[4] * fuzz), spearman.cor.coef[group, city, 4], pch=21, bg="black");
    x.counter <- x.counter + 1;
  }
}
legend(10, 1, legend=c("Non-time bound", "Time bound"), pch=(c(1, 16))); 
axis(1, at=c(1,2,3,4,6,7,8,9,11,12,13,14), lab=rep(city.vec, length(group.vec)));
mtext(group.vec[1], side=1, at=2.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[2], side=1, at=7.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[3], side=1, at=12.5, outer=FALSE, line=3, cex=group.mag)
box()
rm(i, x.counter, group.mag, fuzz, spacing.vec) # clean up
dev.off()


######################################################################
### Delta network size
delta.network.size <- as.list(array(NA, dim=c(length(group.vec), length(city.vec)))) # stores datasets
dim(delta.network.size) <- c(length(group.vec), length(city.vec)) # this hack is to get multidimensional list
dimnames(delta.network.size) <- list(d1=group.vec, d2=city.vec)
  
for (group in group.vec) {
  for (city in city.vec) {
    delta.network.size[[group, city]] <- datasets[[group, city]]$P204r - datasets[[group, city]]$RE_P9Dr
  }
}

print("Summary of difference in network size at test and retest (P204 - RE_P9Dr):")
master.delta.network.size.vec <- NULL;
for (group in group.vec) {
  for (city in city.vec) {
    master.delta.network.size.vec <- c(master.delta.network.size.vec, as.vector(delta.network.size[[group, city]]))
  }
}
print(summary(master.delta.network.size.vec))

pdf("figures/12sites_delta_network_size.pdf")
x.counter <- 0
group.mag <- 1.8
plot(c(0,1), c(0,1), xlim=c(1,14), ylim=c(-30, 30), 
     xlab="", ylab="Change in degree", type="n", 
     xaxt="n", yaxt="n")
for (group in group.vec) {
  x.counter <- x.counter + 1
  for (city in city.vec) {
    boxplot(delta.network.size[[group, city]], add=TRUE, at=x.counter, 
            axes=FALSE, outline=FALSE)
    # outline=FALSE doesn't show outliers
    x.counter <- x.counter + 1
  }
}
lines(c(-999, 999), c(0,0), lty=3)
axis(1, at=c(1,2,3,4,6,7,8,9,11,12,13,14), lab=rep(c("SD", "SA", "BA", "HI"), 3))
axis(2, at=c(-30,-20,-10,0,10,20,30))
mtext(group.vec[1], side=1, at=2.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[2], side=1, at=7.5, outer=FALSE, line=3, cex=group.mag)
mtext(group.vec[3], side=1, at=12.5, outer=FALSE, line=3, cex=group.mag)
box()
rm(x.counter, group.mag) # clean up
dev.off()

########################################
      

########################################
# Effect on estimates

trait.vec <- c('HIVPositive', 'HEPBPositive', 'HEPCPositive', 'SYPHPositive')
trait.labels <- c('HIV', 'HVB', 'HVC', 'Syphilis')
trait.labels.short <- c('HIV', 'HVB', 'HVC', 'Syph')
estimate.p204r <-  array(NA, 
                         dim=c(length(group.vec), length(city.vec), length(trait.vec)),
                         dimnames=list(d1=group.vec, d2=city.vec, d3=trait.vec))
estimate.rep9dr <-  array(NA, 
                          dim=c(length(group.vec), length(city.vec), length(trait.vec)),
                          dimnames=list(d1=group.vec, d2=city.vec, d3=trait.vec))

estimate.p204r.deg.both.times <- array(NA, 
                                       dim=c(length(group.vec), length(city.vec), length(trait.vec)),
                                       dimnames=list(d1=group.vec, d2=city.vec, d3=trait.vec))
estimate.rep9dr.deg.both.times <- array(NA, 
                                        dim=c(length(group.vec), length(city.vec), length(trait.vec)),
                                        dimnames=list(d1=group.vec, d2=city.vec, d3=trait.vec))

for (group in group.vec) {
  for (city in city.vec) {
    df <- datasets[[group, city]]
    print(paste("Group:", group, " , City:", city))
    for (trait in trait.vec) {
      # raw degree dist
      estimate.p204r[group, city, trait] <- rds2.estimate(degree.vector=df[!(df$IsSeed), "P204r"], 
                                                          trait.vector=df[!(df$IsSeed), trait],
                                                          trait.value=1, running=FALSE)
      estimate.rep9dr[group, city, trait] <- rds2.estimate(degree.vector=df[!(df$IsSeed), "RE_P9Dr"],
                                                           trait.vector=df[!(df$IsSeed), trait],
                                                           trait.value=1, running=FALSE)
      # no missing data
      respondents.to.use <- (!(df$IsSeed) & (df$degree.both.times))
      estimate.p204r.deg.both.times[group, city, trait] <- rds2.estimate(degree.vector=df[respondents.to.use, "P204r"],
                                                                                trait.vector=df[respondents.to.use, trait],
                                                                                trait.value=1, running=FALSE)
      estimate.rep9dr.deg.both.times[group, city, trait] <- rds2.estimate(degree.vector=df[respondents.to.use, "RE_P9Dr"],
                                                                                 trait.vector=df[respondents.to.use, trait],
                                                                                 trait.value=1, running=FALSE)
    }
    rm(df)
    
  }
}

print("*** Difference in estimates using networksize at test and retest ***")

print("Absolue difference in estimates")
abs.diff <- abs(estimate.p204r.deg.both.times - estimate.rep9dr.deg.both.times)
print(summary(abs.diff))

print("Median absolute difference in estimates using networksize at test and retest")
print(apply(abs.diff, 3, median))

print("Relative absoluate difference > 0.5 in estimates using networksize at test and retest")
rel.abs.diff <- abs.diff/estimate.p204r.deg.both.times
print(mean(rel.abs.diff > 0.5, na.rm=TRUE))

# clean up
rm(abs.diff, rel.abs.diff)

# respondents with degree both times
plot.side.by.side.estimates(array.estimate.1=estimate.p204r.deg.both.times, 
                            array.estimate.2=estimate.rep9dr.deg.both.times,
                            text.str="",
                            filename="figures/12sites_4disease_2estimates_2column_degreebothtimes.pdf");

rm(estimate.p204r, estimate.rep9dr, 
   estimate.p204r.deg.both.times, estimate.rep9dr.deg.both.times)

print("*******************************************")
print("Finished network size stuff")
print("*******************************************")


# this code produces the figure showing the same size in each site
# it assumes that "load_and_prepare_dr_data.R" has already been run

# load in functions need to reproduce results
source("reproduce_functions.R")

# load and prepare data
set.seed(1)
source("load_and_prepare_dr_data.R")

print("*******************************************")
print("Begin sample size stuff")
print("*******************************************")


sample.size.initial <- array(NA, dim=c(length(group.vec), length(city.vec)),
                          dimnames=list(group.vec, city.vec));
sample.size.followup <- array(NA, dim=c(length(group.vec), length(city.vec)),
                            dimnames=list(group.vec, city.vec));

for (group in group.vec) {
  for (city in city.vec) {
    df <- datasets[[group, city]];
    #sample.size.P204[group, city] <- sum( df[, "Eligible"] & !is.na(df[, "P204r"]) );
    #sample.size.RE_P9D[group, city] <-  sum( df[, "Eligible"] & !is.na(df[, "RE_P9Dr"]) )
    
    sample.size.initial[group, city] <- sum(df[, "Eligible"]);
    # next we calculate the number of people who are not missing all information from return interview
    # this is what we call the sample size for the follow-up interview
    sample.size.followup[group, city] <-  sum(!df[, "Return.All.Missing"]);
    
    # CHECK
    if (sample.size.initial[group, city] < sample.size.followup[group, city]) {
      stop("ERROR: sample.size.initial < sample.size.followup")
    }
    rm(df) # clean-up
  }
}

# total sample size
print(paste("Total sample size initial:", sum(sample.size.initial)));
# sample size follow-up
print(paste("Total sample size follow-up:", sum(sample.size.followup)));
print(paste("Overall proportion who participated in follow-up:", 
            sum(sample.size.followup)/sum(sample.size.initial)))
print("Summary of sample size initial:")
print(summary(as.vector(sample.size.initial)))

# create array of data to plot
# first row is number of cases with inital and follow-up
# second row is number of cases with inital only
# each column is a population and we want final order to be
# FSW, SD
# FSW, SA
# FSW, BA
# FSW, HI
# DU, SD
# DU, SA
# DU, BA
# DU, HI
# MSM, SD
# MSM, SA
# MSM, BA
# MSM, HI
# so we need the labels in this order
col.labels <- c("MSM, HI", "MSM, BA", "MSM, SA", "MSM, SD", 
                "DU, HI", "DU, BA", "DU, SA", "DU, SD",
                "FSW, HI", "FSW, BA", "FSW, SA", "FSW, SD");

data.to.plot <- array(NA, dim=c(2,12), 
                      dimnames=list(d1=c("Initial + Follow-up", "Initial only"),
                                    d2=col.labels))


for (group in group.vec) {
  for (city in city.vec) {
    col.label <- paste(group, ", ", city, sep="");
    data.to.plot["Initial + Follow-up", col.label] <- sample.size.followup[group, city];
    data.to.plot["Initial only", col.label] <- sample.size.initial[group, city] - sample.size.followup[group, city];
    rm(col.label) # clean-up
  }
}
# finished creating data.to.plot

big.space <- 0.7
small.space <- 0.2
color1 <- "grey25"
color2 <- "grey90"
inner.y.labels <- rep(c("HI", "BA", "SA", "SD"), 3)

# Simple sample size plot
pdf(file="figures/12sites_samplesizes.pdf", width=7.5, height=5)
par(omd=c(0.075,1,0,1), mar=c(5.1, 4.1, 0.5, 0.5));
mp <- barplot(height=data.to.plot, 
              xlab="Participants", horiz=TRUE, xlim=c(0,550), 
              main="", col=c(color1, color2), axes=FALSE, axisnames=FALSE,
              space=c(rep(small.space, 4), big.space, rep(small.space, 3), big.space, rep(small.space, 3)));
axis(1, at=c(0,100,200,300,400,500), las=0);
axis(2, at=cumsum(c(rep(1+small.space, 4), 1+big.space, rep(1+small.space, 3), 1+big.space, rep(1+small.space, 3)))-0.5, 
     labels=inner.y.labels, las=2);
box();
legend(362, 2.6, c("Initial and follow-up", "Initial only"), fill=c(color1,color2));
mtext("FSW", side=2, at=mean(mp[10:11]), outer=FALSE, cex=1.7, line=2.5, las=2)
mtext("DU", side=2, at=mean(mp[6:7]), outer=FALSE, cex=1.7, line=2.5, las=2)
mtext("MSM", side=2, at=mean(mp[2:3]), outer=FALSE, cex=1.7, line=2.5, las=2)
dev.off();

rm(sample.size.initial, sample.size.followup,
  col.labels, data.to.plot,
   big.space, small.space, color1, color2, inner.y.labels, mp)

print("*******************************************")
print("End sample size stuff")
print("*******************************************")

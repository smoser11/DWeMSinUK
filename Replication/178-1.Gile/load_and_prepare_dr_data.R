
library(date)

print("****************************************")
print("Begining loading and cleaning of data")
print("****************************************")

city.vec <- c("SD", "SA", "BA", "HI")
group.vec <- c("FSW", "DU", "MSM")

city.vec.long <- c("Santo Domingo", "Santiago", "Barahona", "Higuey");
group.vec.long <- c("Female sex workers", "Drug users", "MSM");

# load datasets
datasets <- as.list(array(NA, dim=c(length(city.vec), length(group.vec)))); # stores datasets
dim(datasets) <- c(length(group.vec), length(city.vec)); # this hack is to get multidimensional list
dimnames(datasets) <- list(d1=group.vec, d2=city.vec)

for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    filename <- paste("~/Dropbox/rds_data/dr/krista_lisa_matt/Data/g", group.counter, "p", city.counter, "v03.RData", sep="");
    load(filename) 
    rm(filename)
    datasets[[group.counter, city.counter]] <- DF;
  }
}
rm(DF) # clean up

# create title array
title.array <- array(NA, dim=c(length(group.vec), length(city.vec)));
short.title.array <- array(NA, dim=c(length(group.vec), length(city.vec)));
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    title.array[group.counter, city.counter] <- paste(group.vec.long[group.counter], ", ", city.vec.long[city.counter], sep="");
    short.title.array[group.counter, city.counter] <- paste(group.vec[group.counter], ", ", city.vec[city.counter], sep="");
  }
}

# create new variables
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {
    datasets[[group.counter, city.counter]]$delta.network.size <- datasets[[group.counter, city.counter]]$P204r - datasets[[group.counter, city.counter]]$RE_P9Dr;
    datasets[[group.counter, city.counter]]$days.between.interviews <- as.numeric(datasets[[group.counter, city.counter]]$ReturnDate1 - datasets[[group.counter, city.counter]]$StartDate);
    datasets[[group.counter, city.counter]]$no.return.date <- (is.na(datasets[[group.counter, city.counter]]$ReturnDate1) & is.na(datasets[[group.counter, city.counter]]$ReturnDate2) & is.na(datasets[[group.counter, city.counter]]$ReturnDate3)); 
    datasets[[group.counter, city.counter]]$degree.both.times <- !is.na(datasets[[group.counter, city.counter]]$P204r) & !is.na(datasets[[group.counter, city.counter]]$RE_P9Dr); 

    ### Find the row of the parent of each sample member, NA for seeds
    # initialize to -999
    datasets[[group.counter, city.counter]]$parent.row <- -999;
    for (i in 1:length(datasets[[group.counter, city.counter]]$parent.row)) {
      datasets[[group.counter, city.counter]][i, "parent.row"] <- find.parent(datasets[[group.counter, city.counter]], datasets[[group.counter, city.counter]][i, "InCoupon"]);
    }
    # checks
    if (sum(datasets[[group.counter, city.counter]]$parent.row==-999, na.rm=TRUE) > 0) {
      stop("ERROR: There are still -999 in parent.row");
    }
    for (i in 1:length(datasets[[group.counter, city.counter]]$parent.row)) {
      if(is.na(datasets[[group.counter, city.counter]][i, "parent.row"]) - as.logical(datasets[[group.counter, city.counter]][i, "IsSeed"])) {

        # checks to make sure that parent.row is NA if and only if IsSeed is true
        print(paste("ERROR: The IsSeed and parent.row values for record", i ,"are in conflict for group =", group.counter ," and city =", city.counter));
        if (datasets[[group.counter, city.counter]][i, "InCoupon"]==10) {
           # there are two known errors where InCoupon is 10.  I'm not sure how this happens.  It is reasonable to code parents as NA
           print(paste("For record", i ,"InCoupon == 10 so NA is reasonable for parent.row even if it is not a seed"));

        }
      }
    }
    ### Finished creating parent.row
    
    ### Find seed of origin for each case
    two.digit.seed.numbers <- c(10, 14:20, 24:30, 34:40, 44:50, 54:60, 64:70, 74:80, 84:90, 94:99);
    for (i in (1:length(datasets[[group.counter, city.counter]][, "InCoupon"]))) {
      InCoupon.str <- as.character(datasets[[group.counter, city.counter]][i, "InCoupon"]);
      # check for two digit seed number
      if ( substr(InCoupon.str, start=1, stop=2) %in%  two.digit.seed.numbers) {
        print("Note that we have a two digit seed number for  (this is not a problem, but it should be rare) . . . ")
        print(paste("Record:", i));
        print(paste("Group:", group.vec[group.counter]));
        print(paste("City:", city.vec[city.counter]));
        seed.of.origin.result <-  as.numeric(substr(InCoupon.str, start=1, stop=2));
        print(paste("InCoupon:", InCoupon.str));
        print(paste("seed.of.origin:", seed.of.origin.result));
      } else {
        # one digit seed number
        seed.of.origin.result <- as.numeric(substr(InCoupon.str, 1, 1));
      }
      datasets[[group.counter, city.counter]][i, "seed.of.origin"] <- as.numeric(seed.of.origin.result);
      rm(InCoupon.str, seed.of.origin.result); # clean up
    }    
    ### Finsihed finding seed of origin for each case
    

    ################################
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
    rm(network.question, network.questions.to.recode, network.size.zero, entries.to.change); # clean-up
    ################################

    
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

    #######################
    # return interview completed
    return.interview.questions <- c("RE_P1",
                                    "RE_P3A", "RE_P3B", "RE_P3C",
                                    "RE_P4A", "RE_P4B", "RE_P4C",
                                    "RE_P4_A", "RE_P4_B", "RE_P4_C",
                                    "RE_P5A", "RE_P5B", "RE_P5C",
                                    "RE_P6A", "RE_P6B", "RE_P6C",
                                    "RE_P7",
                                    "RE_P8",
                                    "RE_P8A", "RE_P8B", "RE_P8C", "RE_P8D", "RE_P8E",
                                    "RE_P9A", "RE_P9B", "RE_P9C", "RE_P9D", "RE_P9E");
    # The following questions are not included in return.interview.questions because they are factors
    # ("RE_P2A", "RE_P2B") 
    # ("ORE_P6A", "ORE_P6B", "ORE_P6C") 
    # ("ORE_P8A", "ORE_P8B", "ORE_P8C", "ORE_P8D", "ORE_P8E") 
    # note that questions like "ORE_P6A" store open ended responses to RE_P6A
    # note that the survey form had two question number 4 so that explains
    # the difference between ("RE_P4A", "RE_P4B", "RE_P4C") and ("RE_P4_A", "RE_P4_B", "RE_P4_C")
    # we cheched the metadata in the original SPSS files 
    # and RE_P4A is the question about reciprocity
    # and RE_P4_A is the question about employment
    
    # for each row, are all the return.interview.questions NA?
    datasets[[group.counter, city.counter]][,"Return.All.Missing"] <- 
      apply(X=(is.na(datasets[[group.counter, city.counter]][, return.interview.questions])), 
            MARGIN=1, FUN=all)
    rm(return.interview.questions); # clean-up
    ############################

  }
}
rm(i, city.counter, group.counter)

print("****************************************")
print("End loading and cleaning of data")
print("****************************************")

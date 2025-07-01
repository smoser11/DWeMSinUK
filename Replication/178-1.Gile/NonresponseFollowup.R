# NonresponseFollowup.R
# Krista Gile
# This file begins with functions to be used later, then proceeds to general code.

########################################################################################
######  Functions                                          											  
######
########################################################################################

#populate the table for a given varaible
populate<-function(thisrow, rowname, varname, test.type, outdata, groups=c(1,2,3)){

falses<-NULL  # create a vector of all the degrees for falses
trues<-NULL   # create a vector of all the degrees for trues
Fgroups<-NULL # corresponding vector of the groups for the falses
Fcities<-NULL # etc.
Tgroups<-NULL
Tcities<-NULL

for (group.counter in groups) {        
  for (city.counter in (1:length(city.vec))) {    
    newfalse<-datasets[[group.counter, city.counter]][,varname][datasets[[group.counter, city.counter]][,"Return.All.Missing"]==F]
    newfalse<-newfalse[!is.na(newfalse)]
    newtrue<-datasets[[group.counter, city.counter]][,varname][datasets[[group.counter, city.counter]][,"Return.All.Missing"]==T]
    newtrue<-newtrue[!is.na(newtrue)]
    falses<-c(falses,newfalse)
    trues<-c(trues,newtrue)
    Tgroups<-c(Tgroups,rep(group.vec[group.counter], times=length(newtrue)))
    Fgroups<-c(Fgroups,rep(group.vec[group.counter], times=length(newfalse)))
    Tcities<-c(Tcities,rep(city.vec[city.counter], times=length(newtrue)))
    Fcities<-c(Fcities,rep(city.vec[city.counter], times=length(newfalse)))
  } 
}

if(1 %in% groups){
  outdata[thisrow,1]<-mean(falses[Fgroups=="FSW"])
  outdata[thisrow,2]<-mean(trues[Tgroups=="FSW"])
}
if(2 %in% groups){
  outdata[thisrow,3]<-mean(falses[Fgroups=="DU"])
  outdata[thisrow,4]<-mean(trues[Tgroups=="DU"])
}
if(3 %in% groups){
  outdata[thisrow,5]<-mean(falses[Fgroups=="MSM"])
  outdata[thisrow,6]<-mean(trues[Tgroups=="MSM"])
}
outdata[thisrow,7]<-mean(falses)
outdata[thisrow,8]<-mean(trues)

outstring<-NULL
#test for significant differences:  overall, by group, and by site
if(test.type=="t"){
  if(t.test(falses,trues)$p.value<=.05){
    outstring<-paste(outstring," Total ", sep="") #, round(mean(falses)-mean(trues),2), ")",sep="")
  }
#  for(group.counter in (1:length(group.vec))){
  for(group.counter in groups){
    falsetemp<-falses[Fgroups==group.vec[group.counter]]
    truetemp<-trues[Tgroups==group.vec[group.counter]]
    if(t.test(falsetemp,truetemp)$p.value<=.05)
    {
      outstring<-paste(outstring," ", group.vec[group.counter], sep="")
                      # " (", round(mean(falsetemp)-mean(truetemp),2), ")",sep="")
    }
  }
#  for(group.counter in (1:length(group.vec))){
  for(group.counter in groups){    
    for(city.counter in (1:length(city.vec))){
      falsetemp<-falses[Fgroups==group.vec[group.counter] & Fcities==city.vec[city.counter]]
      truetemp<-trues[Tgroups==group.vec[group.counter] & Tcities==city.vec[city.counter]]
      if(t.test(falsetemp, truetemp)$p.value<=.05)  
      {
        outstring<-paste(outstring," ", group.vec[group.counter], city.vec[city.counter], " (", 
                         round(mean(falsetemp)-mean(truetemp),2), ")",sep="")
      }	
    }
  }
}else{
  if(chisq.test.list(falses,trues)$p.value<=.05){
    outstring<-paste(outstring," Total ", sep="") #(", round(mean(falses)-mean(trues),2), ")",sep="")
  }
#  for(group.counter in (1:length(group.vec))){
  for(group.counter in groups){  
    falsetemp<-falses[Fgroups==group.vec[group.counter]]
    truetemp<-trues[Tgroups==group.vec[group.counter]]
    if(chisq.test.list(falsetemp,truetemp)$p.value<=.05)
    {
      outstring<-paste(outstring," ", group.vec[group.counter], sep="")
                       #" (", round(mean(falsetemp)-mean(truetemp),2), ")",sep="")
    }
  }
#  for(group.counter in (1:length(group.vec))){
  for(group.counter in groups){
    for(city.counter in (1:length(city.vec))){
      falsetemp<-falses[Fgroups==group.vec[group.counter] & Fcities==city.vec[city.counter]]
      truetemp<-trues[Tgroups==group.vec[group.counter] & Tcities==city.vec[city.counter]]
      if(chisq.test.list(falsetemp,truetemp)$p.value<=.05)  
      {
        outstring<-paste(outstring," ", group.vec[group.counter], city.vec[city.counter], " (", 
                         round(mean(falsetemp)-mean(truetemp),2), ")",sep="")
      }	
    }
  }    
}

if(is.null(outstring)){outstring<-"none"}
outdata[thisrow,9]<-outstring
dimnames(outdata)[[1]][thisrow]<-rowname
rm(city.counter, group.counter, thisrow, test.type, rowname, varname, falses, trues, Fgroups, Fcities, Tgroups, Tcities, outstring)
outdata
}


#do a chi square test based on two vectors, one containing data for each category
chisq.test.list<-function(list1, list2){
  chisq.test(c(list1,list2),rep(c(0,1), times= c(length(list1),length(list2))))
}

library(xtable)



########################################################################################
######  End Functions                                          									
######
########################################################################################



########################################################################################
######  First determine when return is all missing 								   #####
########################################################################################

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
    filename <- paste("~/Dropbox/Research/krista_lisa_matt/Data/g", group.counter, "p", city.counter, "v03.RData", sep="");
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
                                    "RE_P9A", "RE_P9B", "RE_P9C", "RE_P9D", "RE_P9E")
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
    
    
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    datasets[[group.counter, city.counter]][,"Return.All.Missing"] <- 
      apply(X=(is.na(datasets[[group.counter, city.counter]][, return.interview.questions])), 
            MARGIN=1, FUN=all)
    ############################
  }
}
rm(city.counter, group.counter, return.interview.questions)


########################################################################################
######  Set up output matrix for storing data to ouput in an Xtable for the paper ######
########################################################################################
outdata<-as.data.frame(matrix(0,1,9))
dimnames(outdata)[[2]]<-c("FSW Resp", "FSW Non-Resp", "DU Resp", "DU Non-Resp", "MSM Resp", "MSM Non-Resp",
	"Total Resp", "Total Non-Resp", "Sig. Sites")


########################################################################################
######  Get sample sizes														                              ######
########################################################################################
thisrow<-1  # row in data frame outdata
newrow<-rep(0,8)
for (group.counter in (1:length(group.vec))) {
  temp<-c(0,0)	
  for (city.counter in (1:length(city.vec))) {    
    temp<-temp+table(datasets[[group.counter, city.counter]][,"Return.All.Missing"])   
    } 
  newrow[c(2*group.counter-1,2*group.counter)]<-temp
  newrow[c(7,8)]<-temp+as.numeric(newrow[c(7,8)])
}
outdata[thisrow,1:8]<-newrow
outdata[thisrow,9]<-"-"
dimnames(outdata)[[1]][thisrow]<-"N"
rm(city.counter, group.counter, newrow, temp)

########################################################################################
######  Find degree differences  												                          ######
########################################################################################
outdata<-populate(2,"Mean Degree", "P204r", "t", outdata)


########################################################################################
######  NChildren                                     							  ######
########################################################################################
outdata<-populate(4, "# Recruits", "NChildren", "chi", outdata)


########################################################################################
######  No Children 															  ######
########################################################################################
# First create new variable
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    datasets[[group.counter, city.counter]][,"No.Children"] <- 
    	datasets[[group.counter, city.counter]][,"NChildren"] ==0
  }
}
rm(city.counter, group.counter)

outdata<-populate(5, "No Recruits", "No.Children", "chi", outdata)

########################################################################################
######  Offspring.ifsome                                    										  ######
########################################################################################
# First create new variable
for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    temp<-datasets[[group.counter, city.counter]][,"NChildren"]
    datasets[[group.counter, city.counter]][,"Offspring.ifsome"] <- temp
    datasets[[group.counter, city.counter]][,"Offspring.ifsome"][temp==0] <- NA 
  }
}
rm(city.counter, group.counter, temp)

outdata<-populate(6, "# Recruits/0", "Offspring.ifsome", "t", outdata)


########################################################################################
######  Days since start        												  ######
########################################################################################
#Create a new variable for date offset

for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    datasets[[group.counter, city.counter]][,"Date.Offset"] <- 
    	datasets[[group.counter, city.counter]][,"StartDate"] - 
    	min(datasets[[group.counter, city.counter]][,"StartDate"])
  }
}
rm(city.counter, group.counter)

outdata<-populate(7, "Date.Offset", "Date.Offset", "t", outdata)


########################################################################################
######  Days since start, exclude no recruits             											  ######
########################################################################################
#Create a new variable for date offset

for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    temp<-datasets[[group.counter, city.counter]][,"NChildren"]
    datasets[[group.counter, city.counter]][,"Date.Offset.ifkids"] <- 
      datasets[[group.counter, city.counter]][,"StartDate"] - 
      min(datasets[[group.counter, city.counter]][,"StartDate"])
    datasets[[group.counter, city.counter]][,"Date.Offset.ifkids"][temp==0]<-NA
  }
}
rm(city.counter, group.counter,temp)

outdata<-populate(8, "Date.Offset/0", "Date.Offset.ifkids", "t", outdata)

########################################################################################
######  HIVPositive                                         										  ######
########################################################################################
outdata<-populate(9, "HIV Positive", "HIVPositive", "chi", outdata)


########################################################################################
######  Test result motivation  										          ######
########################################################################################
# First, create a new variable in the datasets
# Also create a varaible for money motivation

#FSW, DU, MSM
motivation.questions<-c("P214","P215","P212")
incentive.response<-1
HIV.response<-2
test.response<-c(2:5)	      

for (group.counter in (1:length(group.vec))) {
  for (city.counter in (1:length(city.vec))) {    
    # for each row, are all the return.interview.questions NA?
    datasets[[group.counter, city.counter]][,"For.Incentive"] <- 
      datasets[[group.counter, city.counter]][,motivation.questions[group.counter]] %in% incentive.response
    datasets[[group.counter, city.counter]][,"For.HIV"] <- 
      datasets[[group.counter, city.counter]][,motivation.questions[group.counter]] %in% HIV.response
    datasets[[group.counter, city.counter]][,"For.Tests"] <- 
      datasets[[group.counter, city.counter]][,motivation.questions[group.counter]] %in% test.response      
  }
}
rm(city.counter, group.counter, motivation.questions, incentive.response, HIV.response, test.response)

outdata<-populate(10, "For Incentive", "For.Incentive", "chi", outdata)
outdata<-populate(11, "For HIV Test", "For.HIV", "chi", outdata)
outdata<-populate(12, "For Any Test", "For.Tests", "chi", outdata)


########################################################################################
######  Print xtable                                    												  ######
########################################################################################

xtable(outdata)

## Table S4
xtable(outdata[,1:8])

## Table S5
xtable(cbind(dimnames(outdata)[[1]],outdata[,9]))



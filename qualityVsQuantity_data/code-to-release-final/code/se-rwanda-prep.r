####################################################################
##
## network-rwanda-prep.r
##
## prep the rwanda dataset for use in the tie definition study
##

library(foreign)
library(networkreporting)
library(car)
library(dhstools)
library(plyr)
library(dplyr)

source(file.path("se-rwanda-directories.r"))
set.dirs()

## rwanda popn from the 2011 projection Bernard obtained
rwanda.popn <- 10718378

## read in the raw datafile that came from Macro
## note that you must download this file and open up the extract
## the zip archive before running the rest of this code
## (see README.md for more information)
raw.dat <- read.dta(file.path(data.dir, "RWIQ6AFL.DTA"))

## make an id out of the cluster, hh structure number, 
## house number, and line in hh roster
raw.dat$id <- paste(raw.dat$qcluster, raw.dat$qstnum, 
                    raw.dat$qnumber, raw.dat$qline, sep=".")

## be sure id is unique
stopifnot(length(unique(raw.dat$id))==nrow(raw.dat))

## mean of individual weights for all cases whose individual weight is not 0
## is 1
stopifnot(abs(mean(subset(raw.dat, qweight != 0)$qweight) - 1000000)<1)

## keep a version of the raw dataset with some variables renamed;
## this will make writing the code which reproduces Bernard's tables
## a little bit easier on the eye
raw.renamed.dat <- raw.dat %>% 
                   mutate(
                          ## the raw data calls one tie definition standard and 
                          ## one meal; we're going to rename the standard 
                          ## definition to be called acquaintance
                          tie.defn=qhqtype,
                          cluster=qcluster,
                          strata=qhstrata,
                          region=qhregion,
                          hhweight=qhweight/1000000,
                          origindweight=qweight/100000,
                          indweight=origindweight,
                          wealth=qhwlthi,
                          sex=qsex,
                          age=q103,
                          ## these breaks should agree with the ones
                          ## used in the age categories for deaths
                          ## (see below)
                          age.cat=cut(q103,
                            breaks=c(0, seq(from=15,to=65,by=10),95),
                            include.lowest=TRUE, right=FALSE),
                          age.cat5=cut(q103,
                            breaks=c(0, seq(from=15,to=65,by=5),95),
                            include.lowest=TRUE, right=FALSE),
                          educ=q105,
                          religion=q113,
                          resp.is.muslim=(q113=="4"),
                          kigali=(qhregion=="kigali city"))

## this file has mappings between variable numbers and
## more readable names. for variables representing known
## populations, it also has the total population size
varmap <- read.csv(file.path(data.dir, "rwanda-varnames.csv"))

## rename aggregate relational data (ARD)
## variables in a more user-friendly way
hm.q <- paste("q", 201:226, sep="")
names(hm.q) <- paste(subset(varmap,
                            qnum %in% 201:226)$varname)

## also rename MARP questions in a more user-friendly way
marprows <- subset(varmap,
                   floor(qnum/100)==4)
marp.q <- paste("q", marprows$qnum, sep="")
names(marp.q) <- paste(marprows$varname)

## make a vector whose entries are known popn sizes
## and whose names are the corresponding "how many X"
## variable name in the dataset
known.popns <- subset(varmap, ! is.na(known.size))
known.popn.names <- paste("q", known.popns$qnum, sep="")

## vector of known populations with new variable names
new.known.popns <- known.popns$known.size
names(new.known.popns) <- known.popns$varname

## vector of known populations with raw variable names
known.popns <- known.popns$known.size
names(known.popns) <- known.popn.names

## make a pared-down, and more user-friendly version of the topcoded
## dataset called recoded.dat
recoded.dat <- raw.renamed.dat

## first put more descriptive names in for the 'how many X' questions
## and the MARP variables
recoded.dat <- recoded.dat %>% rename_(.dots=hm.q)
recoded.dat <- recoded.dat %>% rename_(.dots=marp.q)

## we'll build up a list of vars to keep in the recoded dataset here
vars.tokeep <- c("id",
                 names(hm.q), names(marp.q),
                 "tie.defn", "cluster", "strata", "region", "hhweight", "indweight",
                 "wealth", "sex", "age", "age.cat", "age.cat5", "educ", "marital",
                 "religion", "resp.is.muslim", "kigali")

recoded.dat <- recoded.dat %>%
       ## relabel the wealth quintile variables to avoid 'poorest' language, 
       mutate(wealth = revalue(as.factor(wealth),
                               c('poorest'='Lowest',
                                 'poorer'='Second',
                                 'middle'='Third',
                                 'richer'='Fourth',
                                 'richest'='Highest'))) %>%
       ## relabel the tie.defn variable to use the terms from our paper
       mutate(tie.defn = revalue(as.factor(tie.defn),
                                 c('standard'='Acquaintance',
                                   'meal'='Meal'))) %>%
       ## relabel the Kigali variable 
       mutate(kigali = revalue(as.factor(kigali),
                               c('FALSE'='Other Provinces',
                                 'TRUE'='Kigali')))

## recode the education variable so that it has categories
## none, primary, secondary+
recoded.dat <- recoded.dat %>%
       mutate(educ=NA,
              educ=ifelse(q104=="no", 
                          "No education",
                          ifelse(q105=="primary", 
                                 "Primary",
                                 ifelse(q105!="primary" & (! is.na(q105)), 
                                        "Secondary+", 
                                        educ))))

## generate a marital status variable
recoded.dat <- recoded.dat %>%
       mutate(marital=NA,
              marital=ifelse(q120=="currently married", 
                             "Married", 
                             ifelse(q120=="living with a man", 
                                    "Living together", 
                                    ifelse(q121=="no", 
                                           "Never Married",
                                           ifelse(q122=="widowed", 
                                                   "Widowed", 
                                                   ifelse(q122 %in% c("divorced", "separated"),
                                                          "Divorced/separated",
                                                          marital))))))

## and Q229.12 is a factor, but all of the responses are NA, so just make it numeric...
recoded.dat$q229_12 <- as.numeric(recoded.dat$q229_12)

###############################
## add the estimates of degrees for the recoded data
## NOTE: we explicitly coerce to a vector because the current
## version of kp.degree.estimator is returning a matrix
## (this will change in the future)
recoded.dat$d.hat <- kp.individual.estimator_(resp.data=recoded.dat,
                                              known.populations=names(new.known.popns),
                                              total.kp.size=sum(new.known.popns),
                                              alter.popn.size=rwanda.popn)$dbar.Fcell.F

vars.tokeep <- c(vars.tokeep, 'd.hat')

########################################################
## make a topcoded dataset

recoded.dat <- recoded.dat %>% select_(.dots=vars.tokeep)

sink(file.path(data.dir, "se-rwanda-prep.log"))
## make topcoded dataset by starting from recoded.dat and
##   * dropping rows with NA on one of the 'how many x' qs
##   * topcoding responses to the 'how many x' qs at 30
topcoded.dat <- recoded.dat
cat("Rwanda: original dataset has ", nrow(topcoded.dat), "rows.\n")

all.q <- c(hm.q, marp.q)

any.na <- apply(topcoded.dat[,names(all.q)],
                1,
                function(x) { 
                    any(is.na(x) | x==99) 
                })

ids.tolose <- data_frame(any.na, id=topcoded.dat$id) %>% filter(any.na)

cat("Rwanda: removing ", nrow(ids.tolose), " rows because of respondent missingness.\n")
topcoded.dat <- anti_join(topcoded.dat, ids.tolose)

topcode <- 30
chk <- topcoded.dat[,names(all.q)]
topcoded.dat <- topcode.data(topcoded.dat,
                             vars=names(all.q),
                             max=topcode)
diffs <- sum(topcoded.dat[,names(all.q)] != chk)
tot <- dim(chk)[1]*dim(chk)[2]
cat("Rwanda: topcoding changed ", diffs, " out of ", tot, " responses.\n")

cat("Rwanda: final dataset has ", nrow(topcoded.dat), "rows.\n")
sink(NULL)

## meta-data for the recoded dataset
attr(recoded.dat, "known.popns") <- new.known.popns
attr(recoded.dat, "hm.q") <- names(hm.q)
attr(recoded.dat, "marp.q") <- names(marp.q)
attr(recoded.dat, "total.popn.size") <- rwanda.popn

## meta-data for the topcoded dataset
attr(topcoded.dat, "known.popns") <- new.known.popns
attr(topcoded.dat, "hm.q") <- names(hm.q)
attr(topcoded.dat, "marp.q") <- names(marp.q)
attr(topcoded.dat, "total.popn.size") <- rwanda.popn
attr(topcoded.dat, "topcode") <- topcode

########################################################
## save the resulting datasets
save(raw.dat, topcoded.dat, recoded.dat,
     varmap, rwanda.popn, 
     file=file.path(data.dir, "se-rwanda-data.RData"))



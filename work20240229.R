rm(list=ls())
library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
library(gtools)

# setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/RightsLab/GNSUM-CEandS")
# setwd("~/Dropbox/research/RightsLab/DWeMSinUK/survey")

### DATA WORK:


dir()
library(janitor)
data <- read_xlsx("./survey/Further Coding Update.xlsx")

names(data)
data <- read.csv("./survey/Update Selim Risk Index.csv")
data <- read.csv("./survey/UpdateSelimRiskIndex-sum_cat.csv")

data <- data %>% select(-contains("column"))
data <- clean_names(data)
data$q13 <- as.numeric(data$q13)
names(data)
data$recruiter.id <- as.numeric(data$node_1_recruiter)
data<- data %>% select(recruiter.id, q13, starts_with("node_"),  everything())

data$rowNum <- rownames(data)
names(data)
data$id <- data$node_2_id_respondent_recruit
data$ridc <- data$node_1_recruiter
data$recruiter.id[is.na(data$recruiter.id)] <- -1
data<- data %>% select(rowNum, ridc, recruiter.id,  id, q13, everything())
data$ridc[is.na(data$ridc)] <- "seed"

sort(names(data))

library(RDS)
data$network.size.variable <- as.numeric(data$q13)
sort(names(data))
write.csv(data, file = "./survey/data.csv", row.names = FALSE)


## deal with zeros: fix if incorrect; remove if 'true zero' -- TAKE TWO
### Fix if incorrect:
df <- data

df_processed <- df %>%
	rowwise() %>%
	mutate(NonEmptyCount = sum(!is.na(c_across(q105:q115))),
		   NonEmptyValues = list(na.omit(c_across(q105:q115)))) %>%
	ungroup()


data <- df_processed %>%
mutate(suspicious_variable = ifelse(NonEmptyCount > q13, 1, 0),
	   numRef = pmax(NonEmptyCount, q13)) %>% select(numRef, NonEmptyCount, q13, everything())


### Remove if 'true zero'

dd <- data %>% filter(numRef >0)



## Make long, sure why not.

df_long <- df_processed %>%
	mutate(id = row_number()) %>%
	unnest(NonEmptyValues) %>%
	select(id, NonEmptyCount, NonEmptyValues, everything())

write.csv(df_long, "./survey/long_format_data.csv", row.names = FALSE)




## This removes the D/K
library(car)
unique(dd$q36)
dd$zQ36 <- recode(dd$q36, "5=NA")
unique(dd$zQ36)
dd$zQ36 <- recode(dd$q36, "c(0,1,2,3) = 1; c(4,5)=0")
unique(dd$q80)
dd$zQ80 <- recode(dd$q80, "4=NA")
dd$zQ80 <- recode(dd$q80, "c(0,1) = 1; c(2,3,4,5)=0")

summary(dd$zQ36)
unique(dd$q80)
library(psych)
describe(as.numeric(dd$q80) )

summary(dd$zQ80)

# ## Count D/K as not experiencing Exploitation -- this is CONSERVATIVE b/c the proportion of those experienceing Exp will be lower than if removing D/Ks
# 
# library(car)
# unique(dd$q36)
# dd$zQ36 <- recode(dd$q36, "c(0,1,2,3) = 1; c(4,5)=0")
# unique(dd$q80)
# dd$zQ80 <- recode(dd$q80, "c(0,1) = 1; c(2,3,4,5)=0")

summary(dd$zQ36)
unique(dd$q80)
library(psych)
describe(as.numeric(dd$q80) )

summary(dd$zQ80)

# dd$network.size.variable <- dd$q13
dd$network.size.variable <- as.numeric(dd$numRef)
dd$network.size <- dd$numRef

# data$network.size.variable <- data$numRef

#####--
## Use updated sum_categories2 (!!!)

names(dd)
library(car)
dd$sum_categories_factor <- as.factor(dd$sum_categories)
dd$sum_categories_cut <- cut_interval(dd$sum_categories, n = 10)
table(dd$sum_categories_cut)
describe(dd$sum_categories)

sort(names(dd))
# dd <- dd %>% select(id, recruiter.id, wave, network.size.variable, degree, everything())






## Playing with RDS and NSUM methods

# https://hpmrg.org/

# install.packages(c("RDS","sspse", "Neighboot","RDStreeboot"))

# https://github.com/LJGamble/netclust/tree/main/R
# library(devtools)
# devtools::install_github("https://github.com/LJGamble/netclust")
library(netclust)
library(RDS)
library(sspse)    # https://github.com/LJGamble/netclust is the non-connected version of 
library(Neighboot)


data(fauxmadrona)

ff <- as.rds.data.frame(fauxmadrona, max.coupons = 5)

RDS.SS.estimates(ff, outcome.variable = "disease")

## Gile et. al. replication materials!
# https://www-tandfonline-com.nottingham.idm.oclc.org/doi/suppl/10.1198/jasa.2011.ap09475
# https://onlinelibrary-wiley-com.nottingham.idm.oclc.org/doi/full/10.1111/biom.12255
# https://cran.r-project.org/web/packages/RDS/
# 
# http://hpmrg.org/sspse/


# http://hpmrg.org/rds/

#	https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XKOVUN

rd.dd <- as.rds.data.frame(dd, id="id", recruiter.id="recruiter.id", max.coupons = 5, check.valid = FALSE)

rd.dd <- as.rds.data.frame(dd, id="id", recruiter.id="recruiter.id", max.coupons = 5, check.valid = FALSE)


dd$network.size.variable <- dd$numRef
dd$network.size <- dd$q13

unique(dd$network.size.variable)

dd$recruiter.id <- as.character(dd$ridc)
dd$recruiter.id <- as.character(dd$recruiter.id)

unique(dd$recruiter.id)
levels(dd$recruiter.id)

getwd()
df <- apply(dd,2,as.character)
write.csv(as.data.frame(df), file = "./survey/dd.csv", row.names = FALSE)


# Identify unique recruiter.ids that don't match any id in the dataset
unique_recruiter_ids <- unique(dd$recruiter.id)
seed_recruiter_ids <- unique_recruiter_ids[!unique_recruiter_ids %in% dd$id]

# Print out the identified seed recruiter.ids for review
print(seed_recruiter_ids)

dd %>% filter(recruiter.id == seed_recruiter_ids) %>% View()

# Decide on a standard seed identifier, e.g., 0
standard_seed_id <- 0

# Update the dataset: Set recruiter.id to the standard seed identifier for seeds
dd$recruiter.id[dd$recruiter.id %in% seed_recruiter_ids] <- standard_seed_id

# Optionally, check if there are still any recruiter.id values that don't match any id (there shouldn't be)
remaining_unique_recruiter_ids <- unique(dd$recruiter.id[!dd$recruiter.id %in% dd$id])
print(remaining_unique_recruiter_ids)

# Continue with converting to an rds.data.frame
# Assuming other necessary columns are correctly formatted
rd.dd <- as.rds.data.frame(dd, id="id", recruiter.id="recruiter.id", max.coupons = 5, check.valid = FALSE)

# Note: This code assumes the column names for ID and recruiter ID in your dataset are 'id' and 'recruiter.id', respectively.
# Adjust the column names in the code if they are different.



rd.dd <- as.rds.data.frame(dd, max.coupons = 5, check.valid = FALSE)

dd$recruiter.id


reingold.tilford.plot(rd.dd, 
					  vertex.label="id", 
					  vertex.size="degree",
					  show.legend=TRUE)

convergence.plot(rd.dd,c("zQ36","zQ80"))

#install.packages(c("JGR","Deducer","DeducerExtras"))




MA.estimates(rd.dd, trait.variable = "zQ36", N=1000, parallel = 4)
MA.estimates(rd.dd, trait.variable = "zQ80", N=1000, parallel = 4)

MA.estimates(rd.dd, trait.variable = "zQ36", N=10000)
MA.estimates(rd.dd, trait.variable = "zQ36", N=100000)
MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample")
MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "random")



## (overnight)
mP_q36_samp <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4)
mP_q80_samp <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "sample")

mP_q80_rand <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "random")
mP_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "random")

save.image()

m100k_q80_samp <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, seed.selection = "sample", parallel = 4)
m100k_q36_samp <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)

m100k_q80_rand <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, seed.selection = "random", parallel = 4)
m100k_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "random", parallel = 4)

save.image()


m1.7m_q36_samp <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)


m1m_q80_rand <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, seed.selection = "random", parallel = 4)
m1m_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, seed.selection = "random", parallel = 4)


m1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4)
m1.7m_q36_samp <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1.76e6, seed.selection = "sample", parallel = 4)

m1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, parallel = 4)
m1.7m_q80_samp <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1.76e6, seed.selection = "sample", parallel = 4)
save.image()



m1m_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4)
m1.7m_q36_samp <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1.76e6, seed.selection = "random", parallel = 4)

m1m_q80_rand <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, seed.selection = "random", parallel = 4)
m1m_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, seed.selection = "random", parallel = 4)
save.image()
##
m1.7m_q36_rand <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1.76e6, seed.selection = "random", parallel = 4)

m1.7m_q80_rand <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1.76e6, seed.selection = "random", parallel = 4)

save.image()


## Use updated sum_categories2 (!!!)

RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories", N=10000)

RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories_factor", N=10000)

gss <- RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories_cut", N=100000)
gss
summary(gss)
str(gss)

ddf <- as.data.frame(gss[1:2])
ddf
## https://stackoverflow.com/questions/19599957/plotting-confidence-intervals-in-ggplot

# ggplot2(ddf, aex(x))

RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=10000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=100000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1000000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1.74e6)

RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=1000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=10000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=100000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=1000000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=1.74e6)


RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=1000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=100000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=1000000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=1.74e6)

RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=10000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=100000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1000000)
RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1.74e6)

RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=1000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=10000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=100000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=1000000)
RDS.II.estimates(rd.dd, outcome.variable = "zQ80", N=1.74e6)


## 1.74 mil. as guess for size of DW in the UK comes from....
####  




## ---
ddd <- read_xlsx("Further Coding Update.xlsx")
ddd <- clean_names(ddd)
ddd$q13 <- as.numeric(ddd$q13)
unique(ddd$q13)
# ddd <- ddd %>% filter(q13 >0)

ddd<- ddd %>% select(q13, everything())
names(ddd)
ddd$id <- ddd$node_2_id_respondent_recruit
ddd$recruiter.id <- ddd$node_1_recruiter
ddd$recruiter.id[is.na(ddd$recruiter.id)] <- -1

ddd<- ddd %>% select(q13, id, recruiter.id, wave, everything())
names(ddd)

unique(ddd$recruiter.id)


library(car)
unique(ddd$q36)
ddd$zQ36 <- recode(ddd$q36, "5=NA")
unique(ddd$zQ36)
ddd$zQ36 <- recode(ddd$q36, "c(0,1,2,3) = 1; c(4,5)=0")
unique(dd$q80)
ddd$zQ80 <- recode(ddd$q80, "4=NA")
ddd$zQ80 <- recode(ddd$q80, "c(0,1) = 1; c(2,3,4,5)=0")

summary(ddd$zQ36)
unique(ddd$q80)
library(psych)
describe(as.numeric(ddd$q80) )

ddd$network.size.variable <- ddd$q13
ddd$degree <- ddd$q13
unique(ddd$degree)
ddd$recruiter.id <- as.character(ddd$recruiter.id)
unique(ddd$recruiter.id)
rd.ddd <- as.rds.data.frame(ddd, max.coupons = 5)
rd.ddd
get.seed.rid(rd.ddd)

library(sspse) ## what is being estimated here??
###  https://hpmrg.org/sspse/
fit2 <- posteriorsize(rd.dd, 
					  #median.prior.size=1740000,
					  mean.prior.size=1740000,
					  maxN = 2000000,
					  visibility=TRUE, K=FALSE, verbose = TRUE, max.coupons = 5,
					  priorsizedistribution = "beta", # c("beta", "flat", "nbinom", "pln", "supplied")					  
					  visibilitydistribution = "cmp" # c("cmp", "nbinom", "pln"),
)

plot(fit2, type = "N")
pospreddeg(fit2)
summary(fit2, HPD.level = 0.95)

fit3_noVis2 <- posteriorsize(rd.dd, 
							#	 mode.prior.sample.proportion = 5.5e-5,
					  median.prior.size=1740000,
					#  mode.prior.size = 1740000,
					 # mean.prior.size=1740000, sd.prior.size = 20000,
					alpha = 1,
					 maxN = 5000000,
					  visibility=FALSE, K=FALSE, verbose = TRUE, max.coupons = 5,
					  priorsizedistribution = "beta", # c("beta", "flat", "nbinom", "pln", "supplied")			
					maxbeta = 110,
					  visibilitydistribution = "cmp" # c("cmp", "nbinom", "pln"),
)


fit3_noVisNoMax[["maxN"]]
summary(fit3_noVis2, HPD.level = 0.95)

fit3_noVis[["maxN"]]
fit3_noVis$mean.prior.size
fit3_noVis$mode.prior.size
names(fit3_noVis)
fit3_noVis$priorsizedistribution
fit3_noVis$mu
fit3_noVis$sigma

plot(fit3_noVis, type = "N")
pospreddeg(fit3_noVis)
summary(fit3_noVis, HPD.level = 0.95)


fit3_Vis <- posteriorsize(rd.dd, 
							#median.prior.size=1740000,
							#mean.prior.size=1740000,
							mode.prior.size=1740000,
							#mode.prior.sample.proportion = 0.000056,

							# maxN = 50000000,
							visibility=TRUE, K=FALSE, verbose = TRUE, max.coupons = 5,
							priorsizedistribution = "beta", # c("beta", "flat", "nbinom", "pln", "supplied")					  
							visibilitydistribution = "cmp" # c("cmp", "nbinom", "pln"),
)

str(fit3_Vis)
names(fit3_Vis)

fit3_Vis[["maxN"]]
fit3_Vis$mean.prior.size
fit3_Vis$mode.prior.size

fit3_Vis$priorsizedistribution


plot(fit3_Vis, type = "N")
pospreddeg(fit3_Vis)
summary(fit3_Vis, HPD.level = 0.95)



library(RDS)
# CLear what is being estimated, but not clear why N is required(?!)
## http://hpmrg.org/rds/

ffit <- RDS.SS.estimates(rd.ddd, outcome.variable = "zQ36", N = 10000)
summary(ffit)
ffit
convergence.plot(rd.ddd,outcome.variable = "zQ36")
bottleneck.plot(rd.ddd,outcome.variable = "zQ36")

MA.estimates(rd.dd, trait.variable = "zQ36")

MA.estimates(rd.ddd, trait.variable = "zQ36", verbose= TRUE, 
			 number.of.coupons = 5, N=1.74e6,
			 seed.selection = "sample"     # "degree"   "random"
			 )


RDS.bootstrap.intervals(rd.ddd, outcome.variable = "zQ36", N = 1.74e6)

RDS.bootstrap.intervals(rd.dd, outcome.variable = "zQ36", N = 10000)
RDS.bootstrap.intervals(rd.ddd, outcome.variable = "zQ80", N = 10000)
RDS.bootstrap.intervals(rd.dd, outcome.variable = "zQ80", N = 10000)



##########################################

########

library(Neighboot)
data("pop.network")

?`Neighboot-package`
neighb

require(RDStreeboot)
RDS.samp <- sample.RDS(pop.network$traits, pop.network$adj.mat, 200, 10,
					   3, c(1/6,1/3,1/3,1/6), FALSE)

treeboot.RDS(RDS.samp, c(0.025, 0.10, 0.90, 0.975), 2000)
rr <- neighb(RDS.data=RDS.samp, quant=c(0.025, 0.975),method="percentile", B=100)


########

Nsamp <- length(rownames(data))

# Find recruiters
rec <- sort(unique(data$recruiter.id) )
rec
rec <- rec[-1]

Nrec <- length(rec)

# first re-sample with replacement

boot1 <- sample(rec,Nrec, replace = TRUE)

# add in everyone those in boot1 recruited:

ww <- which(  data$recruiter.id %in% boot1 )
ww

data[ww,] %>% View()

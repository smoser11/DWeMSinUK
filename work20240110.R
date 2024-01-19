library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
library(gtools)

setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/RightsLab/GNSUM-CEandS")
setwd("~/Dropbox/research/RightsLab/DWeMSinUK/survey")


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

## Gile et. al. replication materials!
# https://www-tandfonline-com.nottingham.idm.oclc.org/doi/suppl/10.1198/jasa.2011.ap09475
# https://onlinelibrary-wiley-com.nottingham.idm.oclc.org/doi/full/10.1111/biom.12255
# https://cran.r-project.org/web/packages/RDS/
# 
# http://hpmrg.org/sspse/

library(sspse)
data(fauxmadrona)
reingold.tilford.plot(fauxmadrona, 
                      vertex.label=NA, 
                      vertex.size="degree",
                      show.legend=FALSE,
                      vertex.color="seed")



# http://hpmrg.org/rds/


reingold.tilford.plot(fauxmadrona, 
                      vertex.label=NA, 
                      vertex.size="degree",
                      show.legend=FALSE,
                      vertex.color="seed")

RDS.I.estimates(rds.data=fauxmadrona,outcome.variable="disease",N=1000)
RDS.II.estimates(rds.data=fauxmadrona,outcome.variable="disease",N=1000)
RDS.SS.estimates(rds.data=fauxmadrona,outcome.variable="disease",N=1000)
RDS.HCG.estimates(rds.data=fauxmadrona,outcome.variable="disease",N=1000)

data(fauxtime)
RDS.HCG.estimates(rds.data=fauxtime,outcome.variable='var1')
show.rds.data.frame(fauxtime)

#	https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XKOVUN


dir()
library(janitor)
dd <- read_xlsx("Further Coding Update.xlsx")
dd <- clean_names(dd)
dd$q13 <- as.numeric(dd$q13)

dd<- dd %>% select(q13, everything())
names(dd)
dd$id <- dd$node_2_id_respondent_recruit
dd$recruiter.id <- dd$node_1_recruiter
dd$recruiter.id[is.na(dd$recruiter.id)] <- -1

summary(dd$q13)
dd$network.size.variable <- dd$q13+1
dd$degree <- dd$q13+1
unique(dd$degree)
dd1 <- dd
dd <- dd %>% filter( degree <105)

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

prop.table(as.numeric(dd$q62) )
proportions(table(as.numeric(dd$q62) ))

str(rd.dd)
library(car)
dd$sum_categories_factor <- as.factor(dd$sum_categories)
dd$sum_categories_cut <- cut_interval(dd$sum_categories, n = 10)
table(dd$sum_categories_cut)

dd <- dd %>% select(id, recruiter.id, wave, network.size.variable, degree, everything())

dd$recruiter.id <- as.character(dd$recruiter.id)
rd.dd <- as.rds.data.frame(dd, max.coupons = 5)



reingold.tilford.plot(rd.dd, 
					  vertex.label="id", 
					  vertex.size="degree",
					  show.legend=TRUE)

data(faux)
convergence.plot(faux,c("X","Y"))

convergence.plot(rd.dd,c("zQ36","zQ80"))

install.packages(c("JGR","Deducer","DeducerExtras"))

RDS.SS.estimates(rd.dd, outcome.variable = "zQ36", N=1000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ36", N=10000)

RDS.II.estimates(rd.dd, outcome.variable = "zQ36", N=1000)

RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=1000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ80", N=10000)

RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories", N=10000)

RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories_factor", N=10000)

gss <- RDS.SS.estimates(rd.dd, outcome.variable = "sum_categories_cut", N=10000)
gss
summary(gss)
str(gss)

ddf <- as.data.frame(gss[1:2])

## https://stackoverflow.com/questions/19599957/plotting-confidence-intervals-in-ggplot

# ggplot2(ddf, aex(x))



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

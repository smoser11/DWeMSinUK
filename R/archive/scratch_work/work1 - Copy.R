library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
library(gtools)

setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/RightsLab/DWeMSinUK")
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
dd <- read_xlsx("./survey/Further Coding Update.xlsx")
dd <- clean_names(dd)
dd$q13 <- as.numeric(dd$q13)

dd<- dd %>% select(q13, everything())
names(dd)
dd$id <- dd$node_2_id_respondent_recruit
dd$recruiter.id <- dd$node_1_recruiter
dd$recruiter.id[is.na(dd$recruiter.id)] <- -1

dd$network.size.variable <- dd$q13+1
dd$degree <- dd$q13+1
unique(dd$degree)
dd1 <- dd
dd <- dd %>% filter( degree <105)

library(car)
unique(dd$q36)
dd$zQ36 <- recode(dd$q36, "c(0,1,2,3) = 1; c(4,5)=0")
unique(dd$q80)
dd$zQ80 <- recode(dd$q80, "c(0,1) = 1; c(2,3,4,5)=0")

library(car)
dd$sum_categories_factor <- as.factor(dd$sum_categories)
dd$sum_categories_cut <- cut_interval(dd$sum_categories, n = 10)
table(dd$sum_categories_cut)

dd <- dd %>% select(id, recruiter.id, wave, network.size.variable, degree, everything())

dd$recruiter.id <- as.character(dd$recruiter.id)
rd.dd <- as.rds.data.frame(dd, max.coupons = 10)



reingold.tilford.plot(rd.dd, 
					  vertex.label=NA, 
					  vertex.size="degree",
					  show.legend=FALSE)



RDS.SS.estimates(rd.dd, outcome.variable = "zQ36", N=1000)
RDS.SS.estimates(rd.dd, outcome.variable = "zQ36", N=10000)

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
ddd <- read_xlsx("./survey/Further Coding Update.xlsx")
ddd <- clean_names(ddd)
ddd$q13 <- as.numeric(ddd$q13)
unique(ddd$q13)
ddd <- ddd %>% filter(q13 >0)

ddd<- ddd %>% select(q13, everything())
names(ddd)

ddd$id <- ddd$node_2_id_respondent_recruit
ddd$recruiter.id <- ddd$node_1_recruiter
unique(ddd$recruiter.id)
ddd$recruiter.id[is.na(ddd$recruiter.id)] <- -1
unique(ddd$recruiter.id)


ddd$network.size.variable <- ddd$q13
ddd$degree <- ddd$q13
unique(ddd$degree)

rd.ddd <- as.rds.data.frame(ddd, max.coupons = 10)




ddd <- dd %>% filter(degree >0)
unique(ddd$recruiter.id)
class(ddd$degree)	
class(ddd$recruiter.id)
rd.ddd <- as.rds.data.frame(ddd, max.coupons = 10)


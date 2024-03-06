rm(list=ls())
library(rstudioapi)
library(tidyverse)
dirname(rstudioapi::getSourceEditorContext()$path) %>% setwd()

load("./survey/dd.RData")

names(dd)

###


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


# http://hpmrg.org/rds/

#	https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XKOVUN

#install.packages(c("JGR","Deducer","DeducerExtras"))



library(psych)
describe(dd$sum_categories)
summary(dd$sum_categories_cut)
summary(dd$sum_categories_factor)


ff <- filter(dd, str_detect(q8_a, "Fili" ) )

# Identify unique recruiter.ids that don't match any id in the dataset
unique_recruiter_ids <- unique(ff$recruiter.id)
seed_recruiter_ids <- unique_recruiter_ids[!unique_recruiter_ids %in% ff$id]

unique_recruiter_ids
# Print out the identified seed recruiter.ids for review
print(seed_recruiter_ids)

ff %>% filter(recruiter.id == seed_recruiter_ids) %>% View()

# Decide on a standard seed identifier, e.g., 0
standard_seed_id <- 0

# Update the dataset: Set recruiter.id to the standard seed identifier for seeds
ff$recruiter.id[ff$recruiter.id %in% seed_recruiter_ids] <- standard_seed_id

# Optionally, check if there are still any recruiter.id values that don't match any id (there shouldn't be)
remaining_unique_recruiter_ids <- unique(dd$recruiter.id[!dd$recruiter.id %in% dd$id])
print(remaining_unique_recruiter_ids)


fff <-  as.rds.data.frame(ff, max.coupons = 5, id="id", recruiter.id="recruiter.id",)



# Note: This code assumes the column names for ID and recruiter ID in your dataset are 'id' and 'recruiter.id', respectively.
# Adjust the column names in the code if they are different.


library(dplyr)

data2 <- ff %>%
	mutate(recruiter.id = if_else(recruiter.id == 'seed', '-1', recruiter.id))


# (overnight)
## MODEL ASSISTED

library(tidyverse)
Fili_MAsamp_10k_q36 <- MA.estimates(  fff, trait.variable = "zQ36", N=10000, parallel = 4, seed.selection = "sample")
Fili_MAdegr_953k_q36 <- MA.estimates(  fff, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "degree")

Fili_MAdegr_1m_q36 <- MA.estimates(  fff, trait.variable = "zQ36", N=1000000, parallel = 4, seed.selection = "degree")

Fili_MAdegr_1m_q80 <- MA.estimates(  fff, trait.variable = "zQ80", N=1000000, parallel = 4, seed.selection = "degree")

Fili_MAdegr_1m_sum_categories <- MA.estimates(fff, trait.variable = "sum_categories", N=1000000, parallel = 4, seed.selection = "degree")
Fili_MAdegr_1m_sum_categories_cut <- MA.estimates(fff, trait.variable = "sum_categories_cut", N=1000000, parallel = 4, seed.selection = "degree")


save.image(file="filiOnly.RData")


##########################################################################################################################################################
#############################################################################
#############################################################################
#############################################################################


MAsamp_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_10k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, parallel = 4, seed.selection = "sample")
MAsamp_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, parallel = 4, seed.selection = "sample")

#############################################################################
#############################################################################


MAsamp_10k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_10k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=10000, parallel = 4, seed.selection = "sample")
MAsamp_10k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=10000, parallel = 4, seed.selection = "sample")

MAdegr_10k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=10000, parallel = 4, seed.selection = "degree")
MAdegr_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "degree")
MAdegr_10k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=10000, parallel = 4, seed.selection = "degree")
MAdegr_10k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=10000, parallel = 4, seed.selection = "degree")


save.image()

MAsamp_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, parallel = 4, seed.selection = "sample")
MAsamp_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, parallel = 4, seed.selection = "sample")
MAsamp_100k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=100000, parallel = 4, seed.selection = "sample")
MAsamp_100k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=100000, parallel = 4, seed.selection = "sample")

MArand_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, parallel = 4, seed.selection = "random")
MArand_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, parallel = 4, seed.selection = "random")
MArand_100k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=100000, parallel = 4, seed.selection = "random")
MArand_100k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=100000, parallel = 4, seed.selection = "random")

MAdegr_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, parallel = 4, seed.selection = "degree")
MAdegr_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, parallel = 4, seed.selection = "degree")
MAdegr_100k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=100000, parallel = 4, seed.selection = "degree")
MAdegr_100k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=100000, parallel = 4, seed.selection = "degree")

save.image()


MAsamp_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "sample")
MAsamp_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "sample")
MAsamp_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "sample")
MAsamp_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "sample")

MArand_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "random")


MArand_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "random")

MAdegr_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, parallel = 4, seed.selection = "degree")
MAdegr_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, parallel = 4, seed.selection = "degree")
MAsamp_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "degree")
MAsamp_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "degree")

MAsamp_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "degree")
MAsamp_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "degree")

MAsamp_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "sample")
MAsamp_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "sample")



MAdegr_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "degree")
MAdegr_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "degree")


save.image()


MArand_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=953000, parallel = 4, seed.selection = "random")
MArand_953k_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=953000, parallel = 4, seed.selection = "random")

MAsamp_1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4, seed.selection = "sample")
MAsamp_1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, parallel = 4, seed.selection = "sample")
MAsamp_1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4, seed.selection = "sample")
MAsamp_1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, parallel = 4, seed.selection = "sample")
MAsamp_1m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1000000, parallel = 4, seed.selection = "sample")
MAsamp_1m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1000000, parallel = 4, seed.selection = "sample")

MAdegr_1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, parallel = 4, seed.selection = "degree")
MAdegr_1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4, seed.selection = "degree")
MAdegr_1m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1000000, parallel = 4, seed.selection = "degree")
MAdegr_1m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1000000, parallel = 4, seed.selection = "degree")

MArand_1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, parallel = 4, seed.selection = "random")
MArand_1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, parallel = 4, seed.selection = "random")
MArand_1m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1000000, parallel = 4, seed.selection = "random")
MArand_1m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1000000, parallel = 4, seed.selection = "random")

save.image(file = "MAestimates.RData")



MAsamp_1.5m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1500000, parallel = 4, seed.selection = "sample")

MArand_1.5m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1500000, parallel = 4, seed.selection = "random")
MArand_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "random")
MArand_1.5m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1500000, parallel = 4, seed.selection = "random")
MArand_1.5m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1500000, parallel = 4, seed.selection = "random")

MAdegr_1.5m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1500000, parallel = 4, seed.selection = "degree")
MAdegr_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "degree")
MAdegr_1.5m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1500000, parallel = 4, seed.selection = "degree")
MAdegr_1.5m_sum_categories_cut <- MA.estimates(rd.dd, trait.variable = "sum_categories_cut", N=1500000, parallel = 4, seed.selection = "degree")


save.image(file = "MAestimates.RData")








MArand_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "random")
MArand_10k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=10000, parallel = 4, seed.selection = "random")
MArand_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "random")
MArand_10k_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=10000, parallel = 4, seed.selection = "random")
MArand_10k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=10000, parallel = 4, seed.selection = "random")







MAsamp_1.5m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_sum_categories <- MA.estimates(rd.dd, trait.variable = "sum_categories", N=1500000, parallel = 4, seed.selection = "sample")
MAsamp_1.5m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1500000, parallel = 4, seed.selection = "sample")


save.image(file = "MAestimates.RData")












######



MAsamp_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, seed.selection = "sample", parallel = 4)
MAsamp_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)

MArand_100k_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=100000, seed.selection = "random", parallel = 4)
MArand_100k_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "random", parallel = 4)

save.image()


MAsamp_1.7m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)
MAsamp_1.7m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)

MAsamp_1.7m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=100000, seed.selection = "sample", parallel = 4)

MArand_1m_q80 <- MA.estimates(rd.dd, trait.variable = "zQ80", N=1000000, seed.selection = "random", parallel = 4)
MArand_1m_q36 <- MA.estimates(rd.dd, trait.variable = "zQ36", N=1000000, seed.selection = "random", parallel = 4)


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


############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
## RDS estimates

# ggplot2(ddf, aex(x))

rds_1_1k_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1000)
rds_1_10k_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=10000)
rds_1_100k_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=100000)
rds_1_1m_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1000000)
rds_1_1.74_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=1.74e6)
rds_1_953k_q36  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ36", N=953000)

rds_1_1k_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1000)
rds_1_10k_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=10000)
rds_1_100k_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=100000)
rds_1_1m_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1000000)
rds_1_1.74_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=1.74e6)
rds_1_953k_q80  <- RDS.I.estimates(rd.dd, outcome.variable = "zQ80", N=953000)

save.image(file = "RDSI.RData")




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













#######################################################################################
### Make table


##################################################
##################################################
## MAKE TABLE

rm(list=ls())
dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for Rstudio

getwd()
library(tidyverse)

dir()
# data
load("./data/df_census.RData")
total_pop


################################################
## Start a data frame for estimate comparison (to be 'spread' or 'reshaped' later)

mm <- data.frame(Estimator = character(), PivotGroups = character(), HiddenPopulation = character(), EstimatedSize = numeric(), lower95 = numeric(), upper95 = numeric())



################################################
## Start with Sabrina's calculations
load("./data/basicNSUM_data.RData") # created in 'basic_nsum.R'

perp1[4:6] <- round( as.numeric(perp1[4:6]) )
vic1[4:6] <- round( as.numeric(vic1[4:6]) )

mm[(dim(mm)[1]+1),] <- perp1
mm[(dim(mm)[1]+1),] <- vic1
glimpse(mm)

################################################
###### ADD Killworth PIMLE and MLE estimates (from 02aFrequentistNSUM.R)
library(networkscaleup)
library(tidyverse)
library(gtools)

getwd()

load("KillworthModelsResults.RData")

modelsResults <- smartbind(mm,modelsResults)




################################################
################################################
################################################
################################################
###### ADD Bayesian modes (RDatas from Google drive)
################################################

BmodelsResults <- data.frame(Estimator = character(), PivotGroups = character(), HiddenPopulation = character(), EstimatedSize = numeric(), lower95 = numeric(), upper95 = numeric())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for Rstudio

dir("./data")
library(stringr)

trMods <- dir("./data")[grepl(dir("./data"), pattern = "tr_.*\\.RData")]
barMods <- dir("./data")[grepl(dir("./data"), pattern = "bar_.*\\.RData")]
corMods <- dir("./data")[grepl(dir("./data"), pattern = "cor_.*\\.RData")]

setwd("./data")
dim(BmodelsResults)
trMods
barMods
corMods
load(corMods[3]) 
## repeat for all trMods -- MANUALLY adjust index and `.*Mods`

barMods

bb <- cor_ageOnly


PG <- "9 occ. grps. + one <17 age grp.(300na)"
PG <- "9 occ. grps. + 4 age grps."
PG <- "4 age grps."
PG <- "7 occ. categories"

PG <- "4 age grps. (child pop)"
PG <- "9 occ. grps."
PG <- "9 occ. grps. + one <17 age grp.(70na)"
PG <- "9 occ. grps. + one <17 age grp.(100na)"


## same for Barrier models  (MANUALLY)

## same for Correlated models  (MANUALLY)


### Perform ex post diagnostic checks for convergence of the `*.values` slots in `test` (=output)

library(coda)
ttt <- data.frame(t(bb$NK.values))
bbt.mc <- mcmc(ttt, thin = 5, start = 10000, end =110000)

## for `cor` models
ttt <- data.frame(bb$sizes)
bbt.mc <- mcmc(ttt, thin = 1, start = 1, end =900)
summary(bbt.mc)
geweke.diag(bbt.mc)

# TO DO: include z-statistics from Geweke test for each hidden pop.
### Perform ex post diagnostic checks for convergence of the `*.values` slots in `test` (=output)


ss <-summary(bbt.mc)
str(ss)
ss$quantiles[,c(1,5)]
ssize <- ss$statistics[, "Mean"]
str(ssize)
ssize

# for cor models:
ssize <- ssize[(length(ssize)-6):length(ssize)]

## add results to table and record model and PG
Hpops <- c("perp", "vic", "q9_1", "q10_1", "q12a_2","q12a_3","q12a_4")

Hpops
modres <-rbind(Hpops, round(ssize) )
modres

modres <-rbind(modres, round(t(ss$quantiles[,c(1,5)]) ) )
## corr only:
modres <-cbind( t(modres), round((ss$quantiles[ (dim(ss$quantiles)[1]-6):dim(ss$quantiles)[1] ,c(1,5)]) ) )

modres
BmodelsResults.bak <- BmodelsResults
BmodresLabels <- data.frame( "Correlated", PG, t(modres)) 

## cor models ONLY
BmodresLabels <- data.frame( "Correlated", PG, (modres)) 
names(BmodresLabels) <- names(BmodelsResults)
BmodelsResults <- rbind(BmodelsResults, BmodresLabels)

names(BmodelsResults)
dim(BmodelsResults)




#### after all models

allModels <- rbind(modelsResults, BmodelsResults)



## standardize variable names
allModels[] <- lapply(allModels, function(x) {gsub("Q", "q", x)})

allModels <- allModels %>% arrange(HiddenPopulation, PivotGroups, Estimator)
#alternatively:  allModels <- allModels %>% arrange(HiddenPopulation, Estimator, PivotGroups)
# 
View(allModels)


write.csv(allModels, file = "../allModels.csv", row.names = FALSE)





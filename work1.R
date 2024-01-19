library(tidyverse)
library(haven)
library(openxlsx)
library(gtools)


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
# @hand15-supporting  and @hand15-estimating
"~/Dropbox/research/DWeMSinUK2/Web-based Supplementary Materials for Estimating the Size of Populations at High Risk for HIV using Respondent-Driven Sampling Data by Handcock, Gile and Mar"

# https://cran.r-project.org/web/packages/RDS/
# ##CITATIONS:

# @fell12-deducer; @hand23-rds; @RDSA

citation(package="RDS")

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



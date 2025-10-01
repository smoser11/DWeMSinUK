#############################################################################
##                                                                         ##
##                               netclust                                  ##
##                                                                         ##
##  Estimate the Size of Clustered Hidden Populations Using Respondent-    ##
##  Driven Sampling Data: an Extension of SS-PSE                           ##
##                                                                         ##
##  Created by                                                             ##
##    Laura J. Gamble, Oregon State University, Corvallis, OR              ##
##    Katherin R. McLaughlin, Oregon State University, Corvallis, OR       ##
##                                                                         ##
#############################################################################
##
## Based on 'statnet' project software (http://statnet.org).
## For license and citation information see http://statnet.org/attribution
##
## Based on HPMRG project software (http://hpmrg.org).
## For license and citation information see http://hpmrg.org/attribution

.onLoad <- function(libname, pkgname){
  temp<-packageDescription("netclust")
  msg<-paste(temp$Package,": ",temp$Title,"\n",
             "Version ",temp$Version,
             " created on ",
             temp$Date,".\n", sep="")
  msg<-paste(msg,"Based on 'statnet' project software (http://statnet.org).\n",
             "For license and citation information see http://statnet.org/attribution\n",
             "Based on HPMRG project software (http://hpmrg.org).\n",
             "For license and citation information see http://hpmrg.org/attribution\n",sep="")
  packageStartupMessage(msg)
}

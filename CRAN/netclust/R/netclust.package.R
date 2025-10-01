#' Estimate the Size of Clustered Hidden Populations Using Respondent-
#' Driven Sampling Data: an Extension of SS-PSE
#'
#' Extends SS-PSE methods created by Handcock, Gile, and Mar (2014) and
#' implemented in the sspse package by Handcock and Gile (2014). This package
#' allows for population size estimation of a clustered or disconnected
#' networked population using  respondent-driven sampling data collected from
#' each cluster.
#'
#' Based on HPMRG project software (http://hpmrg.org).
#' For license and citation information see http://hpmrg.org/attribution
#'
#' When publishing results obtained using this package the original authors are
#' to be cited as:
#'
#' Laura, Gamble J. and McLaughlin, Katherine R. (2021) \pkg{netclust}:
#' Estimate the Size of Clustered Hidden Populations Using Respondent Driven
#' Sampling Data: an Extension of SS-PSE.
#' R package, Corvallis, OR.  Version 0.1.0, \url{http://}.
#'
#' All programs derived from this package must cite it. For complete citation
#' information, use\cr \code{citation(package="netclust")}.
#'
#' @name netclust.package
#' @docType package
#' @author Laura J. Gamble \email{gamblel@@oregonstate.edu},\cr
#' Katherine R. McLaughlin \email{katherine.mclaughlin@@oregonstate.edu}
#'
#' Maintainer: Laura J. Gamble \email{gamblel@@oregonstate.edu}
#'
#' @references
#'
#' Baraff, Aaron J. (2016). \pkg{RDStreeboot}: RDS Tree Bootstrap Method.
#' Version 1.0. \url{https://CRAN.R-project.org/package=RDStreeboot}.
#'
#' Gamble, Laura J. (2021) \emph{Estimating the Size of Clustered Hidden
#' Populations}, Ph.D. Thesis, Department of Statistics, Oregon State
#' University.
#'
#' Gile, Krista J. (2011) \emph{Improved Inference for Respondent-Driven
#' Sampling Data With Application to HIV Prevalence Estimation}, Journal of the
#' American Statistical Association, 106, 493, 135-146.
#'
#' Gile, Krista J. and Handcock, Mark S. (2018) \pkg{sspse}: Estimating Hidden
#' Population Size using Respondent Driven Sampling Data. Los Angeles, CA.
#' Version 0.8, \url{http://hpmrg.org}.
#'
#' Handcock, Mark S., Fellows, Ian E., and Gile, Krista J. (2012) \pkg{RDS}:
#' Respondent-Driven Sampling. Los Angeles, CA. Version 0.9-2,
#' \url{http://hpmrg.org}.
#'
#' Handcock, Mark S., Gile, Krista J. and Mar, Corinne M. (2014)
#' \emph{Estimating Hidden Population Size using Respondent-Driven Sampling
#' Data}, Electronic Journal of Statistics, 8, 1, 1491-1521
#'
#' Wickham, H. (2016) \pkg{ggplot2}: Elegant Graphics for Data Analysis.
#' Springer-Verlag, New York. \url{https://ggplot2.tidyverse.org}.
#'
#' @useDynLib netclust
#' @importFrom stats density
#' @import RDS
#' @import scam
#' @import coda
#' @import ggplot2
#' @import gridExtra
#' @import network
#' @import Matrix
#' @import statnet
#' @import RDStreeboot
#' @import resample
#' @import sspse
NULL

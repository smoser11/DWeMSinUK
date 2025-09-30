#' @title A Simulated RDS Data Set Drawn From a Clustered Population
#'
#' @description This is a faux data set drawn from a simulated clustered
#' population.
#'
#' @docType data
#'
#' @usage data(csamp)
#'
#' @format An object of class \code{"rds.data.frame"}
#'
#' @details The population was made up of \eqn{N = 3,000} members with
#' \eqn{m = 3} clusters of population proportions \strong{p} \eqn{= (0.5, 0.25,
#' 0.25)}.
#'
#' Two seeds in each cluster were selected with probability proportional to
#' size. Each respondent was given 3 coupons with which to recruit, and
#' recruitment probabilities were set such that \eqn{P(Recruit 0) =
#' P(Recruit 3) = 0.1} and \eqn{P(Recruit 2) = P(Recruit 3) = 0.4}. Recruits
#' were selected randomly from peers adjacent to each respondent. Sampling was
#' conducted until the target sample fraction of 0.5 was reached in each
#' cluster.
#'
#' @references
#' Handcock, Mark S., Fellows, Ian E., and Gile, Krista J. (2012) \pkg{RDS}:
#' Respondent-Driven Sampling. Los Angeles, CA. Version 0.9-2,
#' \url{http://hpmrg.org}.
#'
#' @examples
#' data(csamp)
#'
#' csamp$wave <- get.wave(csamp)
#' csamp$seed.id <- get.seed.id(csamp)
"csamp"

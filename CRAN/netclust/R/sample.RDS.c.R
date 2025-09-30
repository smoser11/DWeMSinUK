#' @title Draw an RDS Sample from a Clustered Population
#'
#' @description Simulates an RDS sample from each cluster in a clustered
#' population with \code{m} clusters. This method of approximating an RDS
#' process is a clustered extension of the
#' \code{\link[RDStreeboot]{sample.RDS}} function in the \code{RDStreeboot}
#' package.
#'
#' @param traits data frame; including unique ID numbers and cluster membership
#' for all nodes in the population.
#' @param adj.mat list of length \code{m} or matrix; each entry being an
#' adjacency matrix from each cluster. Alternately a single adjacency matrix
#' for the whole population. If a single adjacency matrix is supplied,
#' \code{N.c} must be supplied as well.
#' @param N.c vector of length \code{m}; the population size in each cluster.
#' @param id character string; the column name in \code{traits} for the unique ID
#' number of each member. These should be the integers 1, ..., \code{sum(N.c)}
#' that directly correspond to the order of the adjacency matrix.
#' @param cluster character string; the column name in \code{traits} for the cluster
#' membership of each member.
#' @param n vector of length \code{m} or count; the target sample sizes to be
#' drawn from each cluster. If a single value is entered, that sample size will
#' be used for all clusters.
#' @param num.seeds vector of length \code{m} or count; the number of seeds in
#' each cluster. If a single value is entered, that number of seeds will be
#' used for all clusters.
#' @param num.samp vector of length \code{m} or count; the maximum number of
#' recruits for a respondent in each cluster. If a single value is entered,
#' that number of possible recruits will be used for all clusters.
#' @param num.prob list of length \code{m} or vector of length
#' \code{num.samp} + 1; each entry being a vector of positive numbers with
#' length \code{num.samp} + 1. Each vector represents the probability of a
#' respondent in the corresponding cluster recruiting 0, 1, ..., \code{num.samp}
#' new respondents. If a single vector is entered, that vector of probabilities
#' will be used for all clusters.
#' @param replace Boolean; whether sampling is performed with replacement.
#'
#' @details This function simulates an RDS sample from a given clustered social
#' network. Seeds are drawn with probability proportional to unit size within
#' their clusters. Each respondent is recruited instantly and in order according
#' to the recruitment probabilities supplied. Recruitment is done uniformly
#' randomly.
#'
#' The network can be supplied as a list of adjacency matrices in each cluster
#' or as an overall adjacency matrix of the entire population. Networks can
#' therefore be supplied in which connections between clusters exist. In this
#' case, sampling will allow for travel between clusters. The assumed cluster
#' membership, or the membership of each respondent's seed, is reported along
#' with the true cluster membership for each individual in the sample.
#'
#' @return A data frame with columns
#'
#' \item{id}{each respondent's unique ID number.}
#' \item{order}{the order in which each respondent was sampled within their
#' cluster.}
#' \item{recruiter.id}{unique ID number for the recruiter of each respondent.}
#' \item{network.size.variable}{degree of each respondent.}
#' \item{cluster}{true cluster membership.}
#' \item{seed.cluster}{cluster membership of the corresponding seed for each
#' respondent.}
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
#' Handcock, Mark S., Fellows, Ian E., and Gile, Krista J. (2012) \pkg{RDS}:
#' Respondent-Driven Sampling. Los Angeles, CA. Version 0.9-2,
#' \url{http://hpmrg.org}.
#'
#' @examples
#'
#' # First, generate a clustered population
#' N <- 500
#' m <- 2
#' probs <- c(0.75, 0.25)
#'
#' d1 <- rnbinom(N*probs[1], 15, mu = 7)
#' d2 <- rnbinom(N*probs[2], 25, mu = 13)
#'
#' cpop <- generate.pop(N, m, probs, list(d1, d2))
#'
#' RDS.csamp <- sample.RDS.c(cpop[[1]], cpop[[2]],
#'                           id = "node", n = c(75, 50),
#'                           num.seeds = 2, num.samp = 3,
#'                           num.prob = c(0.1, 0.4, 0.4, 0.1))
#'
#' @export sample.RDS.c

sample.RDS.c <- function (traits, adj.mat, N.c,
                          id = "id", cluster = "cluster",
                          n, num.seeds, num.samp,
                          num.prob, replace = FALSE){

  traits <- traits[order(traits[, id]), ]
  m <- length(table(traits[, cluster]))

  if(class(adj.mat) == "list"){
    N.c <- rep(0, m)
    for(i in 1:m){
      N.c[i] <- dim(adj.mat[[i]])[1]
    }

    csum <- rep(0, m + 1)
    csum[2:(m + 1)] <- cumsum(N.c)
    full.adj <- matrix(0, nrow = sum(N.c), ncol = sum(N.c))
    for(j in 1:m){
      full.adj[(csum[j] + 1):csum[j + 1], (csum[j] + 1):csum[j + 1]] <- adj.mat[[j]]
    }

    adj.mat <- full.adj
  }

  N <- dim(adj.mat)[1]

  if(length(n) == 1){
    n <- rep(n, m)
  }
  if(length(num.seeds) == 1){
    num.seeds <- rep(num.seeds, m)
  }
  if(length(num.samp) == 1){
    num.samp <- rep(num.samp, m)
  }
  if(!is.list(num.prob)){
    temp <- list()
    for(i in 1:m){
      temp[[i]] <- num.prob
    }
    num.prob <- temp
  }

  nodes <- rep(NA, sum(n))
  edges <- data.frame(node1 = rep(NA, sum(n) - sum(num.seeds)),
                      node2 = rep(NA, sum(n) - sum(num.seeds)))
  deg <- apply(adj.mat, 1, sum)

  seeds <- NULL
  cutoffs <- list()

  for(i in 1:m){
    cutoffs[[i]] <- c((cumsum(c(0, N.c))[-(length(N.c) + 1)] + 1)[i],
                      cumsum(N.c)[i])
    pi <- apply(adj.mat, 1, sum)
    pi[-c(cutoffs[[i]][1]:cutoffs[[i]][2])] <- 0
    seeds <- c(seeds,
               sample(N, num.seeds[i], prob = pi, replace = replace))
  }
  seeds <- sample(seeds)

  nodes[1:sum(num.seeds)] <- seeds
  if (!replace)
    adj.mat[, seeds] <- 0
  curr <- 1
  end <- sum(num.seeds) + 1
  n.count <- as.vector(table(traits[seeds, cluster]))
  traits$clust.guess <- 0
  brk.ind <- 0

  while (end <= sum(n) & brk.ind == 0) {
    curr.clust <- traits[nodes[curr], cluster]
    if(curr <= sum(num.seeds)){
      clust.guess <- curr.clust
      traits[nodes[curr], "clust.guess"] <- clust.guess
    } else {
      clust.guess <- traits[nodes[curr], "clust.guess"]
    }

    adj <- which(adj.mat[nodes[curr], ] == 1)
    if (replace){
      num <- min(sample.int(num.samp[clust.guess] + 1,
                            1,
                            prob = num.prob[[clust.guess]]) - 1,
                 n[clust.guess] - n.count[clust.guess])
    } else {
      num <- min(sample.int(num.samp[clust.guess] + 1,
                            1,
                            prob = num.prob[[clust.guess]]) - 1,
                 length(adj),
                 n[clust.guess] - n.count[clust.guess])
    }
    if (num > 0) {
      samp.adj <- adj[sample(length(adj), num, replace = replace)]
      nodes[end:(end + num - 1)] <- samp.adj
      edges[(end - sum(num.seeds)):(end - sum(num.seeds) + num -
                                      1), ] <- cbind(curr, end:(end + num - 1))
      traits[samp.adj, "clust.guess"] <- clust.guess

      if (!replace) {
        adj.mat[nodes[curr], ] <- 0
        adj.mat[, samp.adj] <- 0
      }
    }

    # Update total sampled counts
    end <- end + num
    n.count[clust.guess] <- n.count[clust.guess] + num

    # Select new current node
    curr <- curr + 1
    if(curr > sum(num.seeds) & !(curr %in% edges$node2)){
      brk.ind <- 1
    }

    # Check sample sizes in each cluster
    # If clust.guess sample size has been reached,
    # move on to next current node, until all
    # sampled nodes have been tried
    if(curr <= sum(num.seeds)){
      clust.guess <- curr.clust
      traits[nodes[curr], "clust.guess"] <- clust.guess
    } else {
      clust.guess <- traits[nodes[curr], "clust.guess"]
    }

    while(n.count[clust.guess] >= n[clust.guess]){
      if(curr + 1 <= sum(num.seeds) | (curr + 1) %in% edges$node2){
        curr <- curr + 1
        curr.clust <- traits[nodes[curr], cluster]
        if(curr <= sum(num.seeds)){
          clust.guess <- curr.clust
          traits[nodes[curr], "clust.guess"] <- clust.guess
        } else {
          clust.guess <- traits[nodes[curr], "clust.guess"]
        }
      } else {
        brk.ind <- 1
        break }
    }
  }
  if (end <= sum(n)) {
    nodes <- nodes[-(end:sum(n))]
    edges <- edges[-(end:sum(n)), ]
  }
  return(data.frame("id" = nodes,
                    "order" = 1:length(nodes),
                    "recruiter.id" = c(rep(-1, sum(num.seeds)),
                                       nodes[edges[!is.na(edges[,"node1"]),
                                                   "node1"]]),
                    "network.size.variable" = traits[nodes, "degree"],
                    "cluster" = traits[nodes, cluster],
                    "seed.cluster" = traits[nodes, "clust.guess"]))
}

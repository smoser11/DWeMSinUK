#' @title Generate a Clustered Population Network
#'
#' @description Simulates a clustered population network given the population
#' structure and target degree distribution.
#'
#' @param N count; the overall population size
#' @param m count; the number of clusters in the population
#' @param probs vector of length \code{m}; the proportion of the population in
#' each cluster. Should sum to 1.
#' @param degs list of length \code{m}; the fully specified degree distribution
#' in each cluster. Should be a list of vectors, each of length \eqn{p_i N} for
#' cluster \eqn{i}.
#'
#' @return A list consisting of the following elements:
#'
#'\item{traits}{data frame; gives the id, degree, and cluster membership of
#' each population member.}
#'\item{adj.mat}{list of length \code{m}; adjacency matrices for each cluster.}
#'
#' @details Networks are generated according to the configuration model. Once
#' members are assigned to clusters, and a degree distribution is specified,
#' edge ends within cluster are uniformly randomly matched to produce the
#' final network.
#'
#' @references
#'
#' Gamble, Laura J. (2021) \emph{Estimating the Size of Clustered Hidden
#' Populations}, Ph.D. Thesis, Department of Statistics, Oregon State
#' University.
#'
#' Gile, Krista J. (2011) \emph{Improved Inference for Respondent-Driven
#' Sampling Data With Application to HIV Prevalence Estimation}, Journal of the
#' American Statistical Association, 106, 493, 135-146.
#'
#' @examples
#'
#' N <- 500
#' m <- 2
#' probs <- c(0.75, 0.25)
#'
#' d1 <- rnbinom(N*probs[1], 15, mu = 7)
#' d2 <- rnbinom(N*probs[2], 25, mu = 13)
#'
#' cpop <- generate.pop(N, m, probs, list(d1, d2))
#'
#' @export generate.pop

generate.pop <- function(N, m, probs, degs){
  pop.return <- list()
  clusts <- list()
  clusts.n <- list()

  clust.pop <- rep(1:m, probs*N)

  for(i in 1:m){
    clusts[[i]] <- matrix(0, nrow = sum(clust.pop == i),
                          ncol = sum(clust.pop == i))
    clust.deg <- data.frame("node" = 1:sum(clust.pop == i),
                            "degree" = degs[[i]])
    connections <- rep(clust.deg$node, clust.deg$degree)
    if(!(length(connections) %% 2 == 0)){
      connections <- connections[-length(connections)]
    }
    connections <- sample(connections, length(connections))
    for(j in 1:length(connections/2)){
      node1 <- min(connections[2*j - 1], connections[2*j])
      node2 <- max(connections[2*j - 1], connections[2*j])
      clusts[[i]][node1, node2] <- 1
    }
    clusts[[i]][lower.tri(clusts[[i]], diag = FALSE)] <-
      t(clusts[[i]])[lower.tri(clusts[[i]], diag = FALSE)]
    clusts.n[[i]] <- as.network.matrix(clusts[[i]], matrix.type = "adjacency",
                                       directed = FALSE)
  }

  adj.all <- as.matrix(bdiag(clusts))
  adj.all.n <- as.network.matrix(adj.all, matrix.type = "adjacency",
                                 directed = FALSE)

  N.pop <- rep(0, m)
  for(j in 1:m){
    N.pop[j] <- sum(clust.pop == j)
  }

  traits <- data.frame("node" = 1:N, "degree" = colSums(adj.all, dims = 1),
                       "cluster" = sort(clust.pop, decreasing = FALSE))

  pop.return[[1]] <- traits
  pop.return[[2]] <- clusts
  names(pop.return) <- c("traits", "adj.mat")

  return(pop.return)
}

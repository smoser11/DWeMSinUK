#' Tranform an \code{sample.RDS} object to an \code{rds.data.frame} object.
#'
#' This function transforms an output from the \code{sample.RDS} function of the
#' \pkg{RDStreeboot} package to an \code{rds.data.frame} object of the
#' \pkg{RDS} package.
#'
#'@usage to.rds(RDS.data)
#' @param RDS.data A list containing the following objects:
#' \describe{
#' \item{\code{nodes}}{ a numeric vector containing IDs}
#' \item{\code{edges}}{ a list containing two vectors: \code{node1} for the recruiter's ID and \code{node2} for the recruit's ID.  }
#' \item{\code{traits}}{a data frame containing respondents' traits. }
#' \item{\code{degree}}{a vector containing each node's degree, or number of social connections. }
#' }
#' @return An \code{rds.data.frame} object.
#' @author Mamadou Yauck <yauck.mamadou@uqam.ca> and Erica E. M. Moodie.
#' @export
#' @importFrom igraph graph_from_data_frame degree ego all_simple_paths V
#' @importFrom dplyr inner_join
#' @importFrom RDS as.rds.data.frame
#' @examples
#' #Load the synthetic population network dataset.
#' data("pop.network")
#'
#' #Draw an RDS sample from the simulated network using the sampleRDS function
#' #from the package RDStreeboot.
#' require(RDStreeboot)
#' RDS.samp <- sample.RDS(pop.network$traits, pop.network$adj.mat, 200, 10,
#'  3, c(1/6,1/3,1/3,1/6), FALSE)
#'
#' #Tranform RDS.samp to an rds.data.frame object
#' require(RDS)
#' to.rds(RDS.data=RDS.samp)
to.rds<-function(RDS.data){

  nSize<-length(RDS.data$traits[,2])

  RDS.gr<-graph_from_data_frame(RDS.data$edges, directed = TRUE, vertices = cbind(id=1:length(RDS.data$traits[,2]),RDS.data$traits))
  leaves= which(degree(RDS.gr, v = V(RDS.gr), mode = "out")==0, useNames = F)
  root= which(degree(RDS.gr, v = V(RDS.gr), mode = "in")==0, useNames = F)

  nSeeds<-length(root)

  seed.l<-list()
  for (a in 1:nSeeds) {
    xx<-all_simple_paths(RDS.gr, from = root[a], to = leaves)
    seed.l[[a]]<-if(is.null(unique(names(unlist(xx))))){
      as.character(a)
    }else{
      unique(names(unlist(xx)))
    }
  }

  seed.l[which(sapply(seed.l,length)==0)]<-names(root[which(sapply(seed.l,length)==0)])
  s.length<-sapply(seed.l,length)
  seed.id<-names(root[rep(1:nSeeds,s.length)])

  seed.df<-data.frame(id=as.numeric(unlist(seed.l)),seed.id=seed.id)

  RDS.df<-RDS.data$traits
  RDS.df$id<-1:nSize
  RDS.df<-inner_join(RDS.df, seed.df,by = "id")
  RDS.df$network.size.variable<-RDS.data$degree
  RDS.df$recruiter.id<-c(rep(-1,nSeeds),RDS.data$edges[,1])

  RDS.df<-RDS.df[,c("id","seed.id","recruiter.id","network.size.variable",
                        names(RDS.data$traits))]
  RDS.df<-as.rds.data.frame(RDS.df)

  return(RDS.df=RDS.df)


}

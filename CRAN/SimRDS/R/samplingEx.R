#' Extracts samples from external populations
#'
#' @description Extract samples from populations that the user inputs from an
#' external source without using the inbuilt function `population` to simulate
#' a population. Data should be in .csv format. Inputs are recruited in the
#' format of the output given in the inbuilt function `population.`
#'
#' @param population .csv file containing the population details. The first
#' column should contain the individual's ID.
#' @param response_column Column including responses.
#' @param degree_column Column including degrees.
#' @param categorical_column Column including categories.
#' @param id_column Column including IDs.
#' @param degree .csv file containing the network sizes. If it's not provided,
#' the degree size would be the number of individuals in the networks provided.
#' If both the networks and the degrees are not provided, then it will randomly
#' produce degrees for each respondent.
#' @param net .csv file contains the individual's network. It should contain
#' the same format as the output of the 'net size table' given from the output
#' of the inbuilt function 'population'. If not provided, would randomly assign
#' individuals considering the degree.
#' @param seeds Number of seeds. It should be a positive integer.
#' @param coupon Number of coupons. It should be a positive integer.
#' @param waves Number of waves. It should be a positive integer.
#' @param size Sample size. It should be a positive integer.
#' @param prob Whether the seeds are selected probabilistically. If you want to
#' be selected randomly then leave it as 'NULL' If you want to relate it to a
#' variable, add the variable name, and it should be related. For example, if
#' the seeds selected should depend on the network size, then the 'network'
#' should be used. If it is inversely proportional, then it should enter as
#' '1/network'.
#' @param prop_sam If the individuals from the network of an individual should
#' be chosen randomly use "NULL". If the selection should be done
#' probabilistically, state the variable and how it should be related.
#' @param showids If TRUE, it displays the IDs being recruited.
#' @param timeInterval Days between recruitment waves. Zeroth wave is assumed
#' to recruit at the present day.
#'
#' @importFrom stats rexp
#' @importFrom RDS as.rds.data.frame
#' @importFrom RDS get.seed.id
#'
#' @return RDS sample
#' @export
#'
#' @examples
#' \donttest{
#' RDS.population <- population(N = 1000,p.ties = 0.6,minVal = 10,zeros = 0,
#' pr = .5,pa = .1, atype_char = "net",atype_res = "net*char")
#'
#' # This should be replaced with a real world dataset.
#' RDS.samplingEx <- samplingEx(population=RDS.population$frame,
#' response_column="response", degree_column="network",
#' categorical_column="character", id_column="s", seeds = 40, coupon = 2,
#' waves =8)
#' }
#'
#' # This function may take considerable time to produce output, depending on
#' # the computer's performance. For a quick reference to the expected result,
#' # a saved version of the output is available.
#' data(RDS.samplingEx)
#'
samplingEx <- function(population,
                      response_column,
                      degree_column,
                      categorical_column,
                      id_column,
                      degree = NULL,
                      net = NULL,
                      seeds=10,
                      coupon=2,
                      waves = NULL,
                      size = 300,
                      prob=NULL,
                      prop_sam = NULL,
                      showids =TRUE,
                      timeInterval = NULL){

  if(is.null(waves)&&is.null(size)){
    stop("Must provide number of waves or sample size")
  }

  if(is.null(id_column)){
    population$id_column <- 1:nrow(population)
    id_column <- "id_column"
  }else{
    population$id_column <- population[, names(population)%in%id_column]
  }

  population <- population[order(population$id_column),]

  if(is.null(degree)){
    if(is.null(degree_column)){
      if(!is.null(net)){
        uniq <- unique(net[,1])
        numU <- numeric()
        for(ii in 1:length(uniq)){
          numU <- c(numU,sum(net[,1]%in%uniq[ii]))
        }
        uniq <- c(uniq, population$id_column[which(!(population$id_column%in%uniq))])
        degree <- data.frame(id = uniq, total = c(numU,rep(0,(length(uniq)-length(numU)))))
        degree <- degree[order(degree$id),]
      }else{
        q <- round(stats::rexp(nrow(population),0.05))
        q[q>=nrow(population)] <- nrow(population)-1
        q[q==0] <- sample(1:max(q), length(q[q==0]), replace = TRUE, prob = max(q):1)
        if(is.null(id_column)){
          degree <- data.frame(id = 1:nrow(population), total = q)
        }else{
          degree <- data.frame(id = population[,id_column], total = q)
        }
      }
    }else{
      q1 <- which(names(population)%in%degree_column)
      q1 <- c(q1,which(names(population)%in%id_column))
      degree <- data.frame("id"=population[,which(names(population)%in%id_column)], "total"=sapply(strsplit(population[,q1]$network, ","), length))
    }
  }else{
    degree <- degree
  }
  names(degree) <- c("id", "total")
  if(is.null(degree_column)){
    population[population[,names(population)%in%id_column]%in%degree$id,"degree_column"] = degree$total
    degree_column <- "degree_column"
  }else{
    population[,"degree_column"] <- population[,names(population)%in%degree_column]
  }

  N <- nrow(population)
  adj <- matrix(0, nrow = N, ncol = N); adj = as.data.frame(adj); row.names(adj) = 1:N; names(adj) = 1:N
  contact <- data.frame(respondent = NULL, connection = NULL)

  if(is.null(net)){
    frame <- data.frame("s" = degree$id,"netSize" = degree$total, "network" = 0)#Initiating the dataframe with generated network sizes
    ties <- data.frame(respondent1 = NULL, respondent2 = NULL)#Initiating a dataframe to get the ties
    adj <- matrix(0, nrow = N, ncol = N);
    adj <- as.data.frame(adj);
    row.names(adj) <- 1:N;
    names(adj) <- 1:N
    contact <- data.frame(respondent = NULL, connection = NULL)

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    frame1 <- frame[(frame$netSize != 0),]
    n <-sort(unique(frame1$netSize), decreasing = TRUE)
    for(i in 1:length(n)){
      while(TRUE){
        y <- frame1[frame1$netSize == n[i],] #Sub frame with network sizes n[i]
        if(nrow(y) == 0)
          break

        x <- frame1[frame1$s != y$s[1],] #subframe excluding y$s[j]th row
        if(nrow(x) == 0)
          break

        r <- x$netSize


        if(nrow(x)==1){
          u <- x$s #if the x has only one row, respondent corresponding to theat row goes ibto the nrtwork of y[j]
        }else{
          f<- as.numeric(unlist(strsplit(as.character(y[1,]$network), ","))) #getting the network as a vector to identify the number of vacants
          if(length(x$s)<(y$netSize[1]-length(f)+1)){
            u <- NULL
          }else{
            if(length(x$s)==1){
              a1 <- x$s
            }else{
              u <- sample(x$s, size = y$netSize[1]-length(f)+1, prob = 10*r) #generating respondents for the vacants
            }

          }
        }

        if(!is.null(u)){
          ties <- rbind(ties, data.frame(respondent1 = y$s[1], respondent2 = u))
          adj[as.integer(row.names(adj)) == y$s[1], u] <- u
          adj[u,as.integer(row.names(adj)) == y$s[1]] <- y$s[1]

          contact <- rbind(contact, data.frame(respondent = y$s[1], connection = u))
          contact <- rbind(contact, data.frame(respondent = u, connection = y$s[1]))
        }

        frame[frame$s == y$s[1],]$network <- paste(c(frame[frame$s == y$s[1],]$network,u), collapse = ",")
        frame1 <- frame1[frame1$s != y$s[1],]
        frame[frame$s%in%u,]$network <- paste(frame[frame$s%in%u,"network"], rep(y$s[1],length(u)), sep=",")
        v <- sapply(strsplit(as.character(frame[frame$s%in%u,]$network), ","),length)
        t <- frame[frame$s%in%u,]
        y <- t[t$netSize + 1 != v,]
        frame1 <- rbind(frame1[!frame1$s%in%u,],y)
      }
      if(nrow(frame1) == 0)
        break
    }
    data <- contact
  }else{
    data <- net
  }
  row.names(data) <- 1:nrow(data)

  pop <- population
  N <- nrow(pop)
  l<-sample(degree$id,seeds,replace=FALSE,prob=degree$total)
  if(is.null(categorical_column)){
    pop$categorical_column <- 1
    categorical_column <- "categorical_column"
  }
  t <- data.frame(id = l, recruiter.id = "seed", network.size = pop[pop[,id_column]%in%l, degree_column], wave = 0, response = pop[pop[,id_column]%in%l, response_column], character = pop[pop[,id_column]%in%l, categorical_column])
  if(!is.null(timeInterval)){ et <- rep(Sys.time(), length(l))}

  l0 <- nrow(t)
  w <- 1
  size1 <- ifelse(is.null(size), l0, size)
  w1 <- ifelse(is.null(waves), w, waves + 1)
  #t <- data.frame(seed = rep(0,N))
  #t[row.names(t)%in%l,] = sort(l)

  lli <- l
  while (w1 != w || l0 != size1) {
    qlq <- numeric()
    for(i in 1:length(lli)){
      bond1 <- data[data$respondent%in%lli[i],"connection"]
      bond <- bond1[!bond1%in%(t$id)]

      if(length(bond)==0){
        next
      }else{
        c <- ifelse(length(bond)<=coupon, length(bond),coupon)
        c1 <- sample(x = 0:c, size = 1, prob = 1:(c+1))

        if(!is.null(prop_sam)){
          prop_sam <- pop[pop$s%in%bond,"character"]
        }

        if(c1 == 0){
          next
        }else{
          while(TRUE){
            if(length(bond)==1){
              a1 <- bond
            }else{
            a1 <- sample(x = bond, replace = FALSE, size = c1, prob = prop_sam)
            }

            if(sum(t$id%in%a1)==0){
              break
            }
          }

          t <- rbind(t, data.frame(id = a1, recruiter.id = as.character(lli[i]), network.size = pop[pop[,id_column]%in%a1, degree_column], wave = w, response = pop[pop[,id_column]%in%a1, response_column], character = pop[pop[,id_column]%in%a1, categorical_column]))
          qlq <- c(qlq, a1)
        }
      }
      if(showids){ message(paste0(lli[i]))}
    }
    lli <- qlq
    if(!is.null(timeInterval)){ et <- c(et, rep(Sys.time()+60*60*24*timeInterval*w, length(qlq)))}
    w <- w +1
    l0 <- nrow(t)

    if(is.null(size)){
      size1 <- l0
    }else{
      if(l0>size){t <- t[1:size,]; l0 <- nrow(t) }
      size1 <- size
    }
    w1 <- ifelse(is.null(waves), w, waves + 1)
    #if(!is.null(timeInterval)){ et <- rep(Sys.time(), length(l))}
    if(length(qlq)==0){
      warning(paste("Haven't attained the required waves or sample size, waves=",w," size=",l0))
      break()
    }

  }

  if(length(qlq)!=0){
    warning(paste("waves=", w, "size=",nrow(t)))
  }

  t$netSize <- sapply(strsplit(t$network, ","), length)

  if(!is.null(timeInterval)){t$time <- et}
  if(!is.null(timeInterval)){
    samp11<-RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="netSize",population.size=N, time = "time")
  }else{
    samp11<-RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="netSize",population.size=N)
  }
  samp11$seed<-RDS::get.seed.id(samp11)

  return(samp11)
}

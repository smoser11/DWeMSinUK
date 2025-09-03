#' Extract samples from generated populations.
#'
#' @description
#' Extract samples from the generated populations using the `SimRDS::population`
#' function. Can directly use this output to the RDS estimates in the RDS package.
#'
#' @param population Population generated using the 'SimRDS::population' function.
#' @param seeds Number of seeds.
#' @param coupon Number of coupons.
#' @param waves Number of waves.
#' @param size Sample size.
#' @param prob Whether the seeds are selected probabilistically. If you want to
#' be selected randomly, leave it as 'NULL' If you want to relate it to a variable,
#' add the variable name. For example, if the seeds selected depend on the
#' network size, then 'network' should be used. It should enter as '1/network'
#' if it is inversely proportional.
#' @param character_sam If the individuals from the network of an individual
#' should be chosen randomly use "NULL". If the selection should be done
#' probabilistically, state the variable and how it should be related. Possible
#' inputs are NULL, netSize, character.
#' @param showids If TRUE, it displays the IDs being recruited.
#' @param timeInterval Days between recruitment waves. Zeroth wave is assumed
#' to recruit at the present day.
#'
#' @return RDS sample
#' @export
#'
#' @importFrom RDS as.rds.data.frame
#' @importFrom RDS get.seed.id
#'
#' @examples
#' \donttest{
#' RDS.population <- population(N = 1000,p.ties = 0.6,minVal = 10,zeros = 0,
#' pr = .5,pa = .1, atype_char = "net",atype_res = "net*char")
#'
#' RDS.samplingP <- samplingP(population=RDS.population, seeds = 40, coupon = 2,
#' waves = 8, prob = '1/network', character_sam = 'netSize')
#' }
#'
#' # This function may take considerable time to produce output, depending on
#' # the computer's performance. For a quick reference to the expected result,
#' # a saved version of the output is available.
#' data(RDS.samplingP)
#'
samplingP <- function(population, seeds, coupon, waves = NULL, size = NULL,
                     prob=NULL, character_sam = NULL, showids = TRUE,
                     timeInterval = NULL){

  if(is.null(waves)&&is.null(size)){
    stop("Must provide number of waves or sample size")
    }

  data <- population$Contact
  row.names(data) <- 1:nrow(data)

  degree <- population$Degree
  names(degree) <- c("id", "total")

  pop <- population$frame
  pop <- pop[,names(pop)%in%c("s", "netSize", "network", "character", "response")]

  N <- nrow(pop)
  network <- pop$netSize
  chara <- pop$character

  y111<-population$`Adjusent Matrix`
  l<-sample(degree$id,seeds,replace=FALSE,prob=eval(parse(text=prob)))
  y <- y111

  t <- data.frame(id = l, recruiter.id = "seed", network.size = pop[pop$s%in%l, "netSize"], wave = 0, response = pop[pop$s%in%l, "response"], character = pop[pop$s%in%l, "character"])

  if(!is.null(timeInterval)){ et = rep(Sys.time(), length(l))}

  l0 <- nrow(t)
  w <- 1
  size1 <- ifelse(is.null(size), l0, size)
  w1 <- ifelse(is.null(waves), w, waves + 1)


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

        if(!is.null(character_sam)){
          prop_sample = pop[pop$s%in%bond,names(pop)%in%character_sam]
        }else{
          prop_sample <- NULL
        }

        if(c1 == 0){
          next
        }else{
          while(TRUE){
            if(length(bond)==1){
              a1 <- bond
            }else{
              a1 <- sample(x = bond, replace = FALSE, size = c1, prob = prop_sample)
            }

            if(sum(t$id%in%a1)==0){
              break
            }
          }

          t <- rbind(t, data.frame(id = a1, recruiter.id = as.character(lli[i]), network.size = pop[pop$s%in%a1, "netSize"], wave = w, response = pop[pop$s%in%a1, "response"], character = pop[pop$s%in%a1, "character"]))
          qlq <- c(qlq, a1)
        }
      }
      if(showids){ message(paste0(lli[i]))}
    }
    lli = qlq
    if(!is.null(timeInterval)){ et = c(et, rep(Sys.time()+60*60*24*timeInterval*w, length(qlq)))}
    w <- w +1
    l0 <- nrow(t)

    if(is.null(size)){
      size1 <- l0
    }else{
      if(l0>size){ t <- t[1:size,]; l0 <- nrow(t) }
      size1 <- size
    }
    w1 <- ifelse(is.null(waves), w, waves + 1)
    #if(!is.null(timeInterval)){ et <- rep(Sys.time(), length(l))}
    if(length(qlq)==0){
      warning(paste("Haven't attained the required waves or sample size, waves=",w," siz=",l0))
      break()
    }

  }

  if(length(qlq)!=0){
    warning(paste("waves=", w, "size=",nrow(t)))
  }
  if(!is.null(timeInterval)){ t$time <- et}

  if(!is.null(timeInterval)){
    samp11<-RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N, time = "time")
  }else{
    samp11<-RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N)
  }
  samp11$seed<-RDS::get.seed.id(samp11)

  return(samp11)
}

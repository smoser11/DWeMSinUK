# reproduce_functions.R
# Matthew Salganik
# These funcations are noeed to repdocuce the results from the DR diagnostics paper

my.ls <- function (pos = 1, sorted = F) {
  # Taken from internet
  # October 26, 2009
  .result <- sapply(ls(pos = pos, all.names = TRUE), function(..x) object.size(eval(as.symbol(..x))))
  if (sorted) {
    .result <- rev(sort(.result))
  }
  .ls <- as.data.frame(rbind(as.matrix(.result), `**Total` = sum(.result)))
  names(.ls) <- "Size"
  .ls$Size <- formatC(.ls$Size, big.mark = ",", digits = 0, format = "f")
  .ls$Mode <- c(unlist(lapply(rownames(.ls)[-nrow(.ls)], function(x) mode(eval(as.symbol(x))))),"-------")
  .ls
}


make.heatmap <- function(results.mat, filename, VERBOSE=FALSE) {
  
  # this function makes heatmaps to summarize the results of lots of tests
  x1.vec <- c(0, 0.345, 0.666)
  names(x1.vec) <- group.vec
  x2.vec <- c(0.333, 0.675, 1)
  names(x2.vec) <- group.vec
  y1.vec <- c(0.28, 0, 0.16)
  names(y1.vec) <- group.vec
  y2.vec <- c(1, 1, 1)
  names(y2.vec) <- group.vec
  
  new.vec <- c(FALSE, TRUE, TRUE)
  names(new.vec) <- group.vec
  
  pdf(file=filename, width=7, heigh=3)
  
  for (label in group.vec) {
    if (VERBOSE) print(paste("Plotting results for label:", label))
    if (VERBOSE) print(results.mat[[label]])
    
    if (all(results.mat[[label]]==1)) {
      print(paste("label:", label))
      print(results.mat[[label]])
      stop("ERROR: results.mat[[label]] is all true so it will not plot correctly")
      # note this could be fixed probably by playing with the image command
      # since this should never happen (fail all tests), I'm not going to 
      # handle that edge case now
    }
      
    par(fig=c(x1.vec[label], x2.vec[label], y1.vec[label], y2.vec[label]), 
        new=new.vec[label])
    par(mar=c(2, 5, 1, 0.2))
    image(x=1:4, y=1:nrow(results.mat[[label]]), z=t(results.mat[[label]]), 
          col=c("gray92", "red"), 
          xaxt="n", yaxt="n", xlab="", ylab="", main="");
    axis(side=1, at=1:4, city.vec, cex.axis=0.6, tck=-0.06, padj=-2.8) 
    axis(side=2, at=1:nrow(results.mat[[label]]), 
         rownames(results.mat[[label]]), las=1, cex.axis=0.6, tck=-0.06, mgp=c(3, 0.6, 0));
    mtext(text=toupper(label), side=3);
    # now add grid lines
    for (row in 1:nrow(results.mat[[label]])) {
      lines(c(-99, 99), c(row + 0.5, row + 0.5), lty=1, col="white")
    }
    for (col in 0:ncol(results.mat[[label]])) {
      lines(c(col + 0.5, col + 0.5), c(-99, 99), lty=1, col="white")
    }
    box()
  }
  dev.off() 
}


make.sample.dynamics.plot <- function(city, group, trait.str,
                                      color=TRUE, VERBOSE=FALSE) {
  # this function makes convergence plots
  
  # set colors
  if (color) {
    infected.color <- rgb(255, 0, 0, 255, maxColorValue=255)
    uninfected.color <- rgb(0, 0, 200, 255, maxColorValue=255)
  } else{
    infected.color <- "black"
    uninfected.color <- "black"    
  }
  background.color <- "grey90"
  

  sample.size.ticks <- c(1,seq(from=50, to=1000, by=50))
  
  box.height <- 0.08;
  tck.value.box <- -0.25;
  tck.value.center <- -0.012;

  filename <- paste("figures/", group, "_", city, "_", trait.str, "_convergence.pdf", sep="");
  pdf(filename)
  
  current.df.no.seeds <- current.df[!current.df$IsSeed, ]; # it is easier to work with only the non-seed data
  
  ############
  # top box
  ############
  if (1==1) {
    # "infected" people
    par(fig=c(0.05, 1, 1-box.height, 1));
    par(omd=c(0, 1, 0, 1));
    par(mar=c(0.8, 1, 1, 1));
    PLOT.CASE <- (current.df.no.seeds[, trait.str]==1);
    PLOT.CASE[is.na(PLOT.CASE)] <- FALSE; # remove NA
    plot(1:length(current.df.no.seeds[, trait.str]), rep(1, length(current.df.no.seeds[, trait.str])), 
         xaxt="n", yaxt="n", xlab="", ylab="", type="n", main="", ylim=c(0, 1))
    for (x.val in which(PLOT.CASE==TRUE)) {
      lines(x=rep(x.val, 2), y=c(0, 1), lwd=1, col=infected.color)  
    } 
    axis(side=1, at=sample.size.ticks, 
         labels=rep("", length(sample.size.ticks)),
         tck=tck.value.box) # empty ticks
    axis(side=2, at=0.5, labels="Yes", las=2);
    box();
  }
  
  #############
  # bottom box
  #############
  if (1==1) {
    # "uninfected" people
    par(fig=c(0.05, 1, 0, box.height), new=TRUE);
    par(omd=c(0, 1, 0, 1)); 
    par(mar=c(0.8, 1, 1, 1));
    PLOT.CASE <- (current.df.no.seeds[, trait.str]==0);
    PLOT.CASE[is.na(PLOT.CASE)] <- FALSE; # remove NA
    plot(x=1:length(current.df.no.seeds[, trait.str]), 
         y=rep(0, length(current.df.no.seeds[, trait.str])), 
         xaxt="n", yaxt="n", xlab="", ylab="", 
         ylim=c(0,1), type="n");
    for (x.val in which(PLOT.CASE==TRUE)) {
      lines(x=rep(x.val, 2), y=c(0, 1), lwd=1, col=uninfected.color)  
    } 
    axis(side=3, at=sample.size.ticks,
         labels=rep("", length(sample.size.ticks)),
         tck=tck.value.box) # empty ticks
    axis(side=2, at=0.5, labels="No", las=2);
    box()
  }
  

  ##############
  # middle box
  #############
  if (1==1) {
    par(fig=c(0.05,1,box.height, 1-box.height), new=TRUE);
    plot(current.df.no.seeds[, trait.str], type="n", 
         ylab=trait.str, xaxt="n", yaxt="n", ylim=c(0,1));
    polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color);
    final.estimate <- rds2.estimate(current.df.no.seeds[, 'P204r'], current.df.no.seeds[, trait.str], 1, running=F);
    lines(c(1, length(current.df.no.seeds[, trait.str])), rep(final.estimate, 2),
          lty="solid", col="white", lwd=3);
    lines(rds2.estimate(current.df.no.seeds[, 'P204r'], current.df.no.seeds[, trait.str], 1, running=T), 
          col="black", lty=1); # RDS 2 estimate
    axis(side=1, at=sample.size.ticks, tck=tck.value.center, padj=-1);
    axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), las=2);
    axis(side=3, at=sample.size.ticks, tck=tck.value.center, padj=1);
    box();
  }
  
  dev.off();
}



wilson.interval <- function(x.vec, n.vec) {
  # calculates the Wilson interval
  # does not invluce any boundry condition corrections
  # see  Brown et al. (2001) http://www.jstor.org/stable/10.2307/2676784
  
  # x.vec: number of success
  # n.vec: number of trials
  # if x.vec has NA values then envelope will have missing values

  if (any(x.vec < 0, na.rm=TRUE)) stop("ERROR: some value in x.vec less than 0")
  if (any(n.vec < 0, na.rm=TRUE)) stop("ERROR: some value in n.vec less than 0")
  if (any(x.vec > n.vec, na.rm=TRUE)) stop("ERROR: some value in x.vec is bigger than n.vec")
  
  kappa <- qnorm(0.975) # ~1.96 
  p.hat.vec <- x.vec/n.vec
  q.hat.vec <- 1 - p.hat.vec
  
  # see formula (4) in Brown et al. (2001)
  p.tilde.vec <- (x.vec + (kappa^2 / 2))/(n.vec + kappa^2)
  if (any(p.tilde.vec < 0, na.rm=TRUE)) stop("ERROR some value in p.tilde.vec < 0")
  if (any(p.tilde.vec > 1, na.rm=TRUE)) stop("ERROR some value in p.tilde.vec > 1")
  if ( !(identical(is.na(x.vec), is.na(p.tilde.vec))) ) {
    # for all entries where x.vec is NA, p.tilde.vec should be NA
    print("x.vec")
    print(length(x.vec))
    print(x.vec)
    print(str(is.na(x.vec)))
    print("p.tilde.vec")
    print(length(p.tilde.vec))
    print(p.tilde.vec)
    print(str(is.na(p.tilde.vec)))
    stop("ERROR: problems with NA in x.vec and p.tilde.vec")
  }
  
  width.vec <- (kappa * sqrt(n.vec))/(n.vec + kappa^2) * 
    sqrt(p.hat.vec * q.hat.vec + (kappa^2/(4 * n.vec)))
  if (any(width.vec < 0, na.rm=TRUE)) stop("ERROR in calculated width")
  if (!(identical(as.vector(is.na(x.vec)), as.vector(is.na(width.vec))))) {
    # for all entries where x.vec is NA, width.vec should be NA
    print(x.vec)
    print(width.vec)
    stop("ERROR: problems with NA in x.vec and width.vec")
  }
  
  ci.array <- array(NA, dim=c(2, length(x.vec)),
                    dimnames=list(d1=c("lower", "upper"), d2=NULL))
  ci.array["lower", ] <- p.tilde.vec - width.vec
  ci.array["upper", ] <- p.tilde.vec + width.vec

  # now do final checks before returning result
  # this check needs all.equal instead of identical because of names
  # as.vector is needed because when n = 1 we get some weird type issues
  if ( !(all.equal(as.vector(is.na(x.vec)), as.vector(is.na(ci.array["lower", ])))) ) {
    # for all entries where x.vec is NA, ci.array["lower", ] should be NA
    print("x.vec")
    print(x.vec)
    print("ci.array[lower, ]")
    print(ci.array["lower", ])
    stop("ERROR: problems with NA in x.vec and ci.array[lower, ]")
  }
  if ( !(all.equal(as.vector(is.na(x.vec)), as.vector(is.na(ci.array["upper", ])))) ) {
    # for all entries where x.vec is NA, ci.array["upper", ] should be NA
    print(x.vec)
    print(ci.array["upper", ])
    stop("ERROR: problems with NA in x.vec and ci.array[upper, ]")
  }
  
  # there are some numerical precision problems
  fuzz <- 0.000001    
  if (any(ci.array < (0 - fuzz), na.rm=TRUE)) {
    print(ci.array)
    stop("ERROR: some ci values less than 0")
  }
  if (any(ci.array > (1 + fuzz), na.rm=TRUE)) {
    print(ci.array)
    stop("ERROR: some ci values greater than 1")
  }
  # finished checks
  
  return(ci.array)
}

calculate.estimates.and.envelopes <- function(degree.vector, trait.vector,
                                              trait.value, seed.vector,
                                              is.seed.vector, VERBOSE=FALSE) {
  
  #degree.vector is the repsondent's social network size
  #trait.vector is what will be estimated
  #seed.vector stores what seed each node comes from (this is not TRUE/FALSE)
  #is.seed.vector stores where each node is a seed or not (0/1)
    
  # NOTE one edge case is that  
  # where a seed does not recruit, estimate becomes NA
  # where a seed does recruit but trait or degree is missing, estimate becomes NA
  # these two cases cannot be distinguished from running.estimates
  # I experimented with using NULL when a seed does not recruit, but that indroduce lots of 
  # extra code complexity elsewhere
  # we distingish between these two cases with offspring.this.seed
  
  if (VERBOSE) print("[calculate.estimates.and.envelopes] Beginning calculations")

  # check input
  if (is.logical(is.seed.vector)) {
    # is.seed.vector is logical (TRUE/FALSE), but we want it to be 0/1
    is.seed.vector <- as.numeric(is.seed.vector);
  }
  if (!( (length(degree.vector)==length(trait.vector)) 
         & (length(trait.vector)==length(seed.vector)) 
         & (length(seed.vector)==length(is.seed.vector)) )) {
    print(paste("length(degree.vector):", length(degree.vector)));
    print(paste("length(trait.vector):", length(trait.vector)));
    print(paste("length(seed.vector):", length(seed.vector)));
    print(paste("length(is.seed.vector):", length(is.seed.vector)));
    stop("ERROR: inputs are not equal length");
  }
  if (sum(is.na(seed.vector)) > 0) {
    print(seed.vector);
    stop("ERROR: seed.vector contains NA");
  }
  if (sum(is.na(is.seed.vector)) > 0) {
    print(is.seed.vector);
    stop("ERROR: is.seed.vector contains NA");
  }
  if (sum(is.na(trait.vector)) > 0) {
    # trait vector can contain NA; this is not a problem
  }
  if (any(is.na(trait.vector[as.logical(is.seed.vector)]))) {
    print(trait.vector[as.logical(is.seed.vector)]);
    print("WARNING: Some seeds are missing trait information");
    warning("WARNING: Some seeds are missing trait information");
  }
  if (any(is.na(seed.vector))) {
    print(seed.vector)
    stop("ERROR: missing values in seed.vector")
  }
  # finished checking input
  if (VERBOSE) print("[calculate.estimates.and.envelopes] Finished checking inputs")
    
  num.seeds <- max(seed.vector) - min(seed.vector) + 1;
  longest.chain <- max(table(seed.vector))
  seeds.that.recruit <- which(table(seed.vector) > 1)
  num.seeds.that.recruit <- length(seeds.that.recruit)
  
  offspring.this.seed.no.missing.vec <- rep(0, length(num.seeds))
  offspring.this.seed.vec <- rep(0, length(num.seeds))
  for (seed in 1:num.seeds) {
    offspring.this.seed.vec[seed] <-
      sum((seed.vector==seed) & !is.seed.vector)
    
    offspring.this.seed.no.missing.vec[seed] <- 
      sum((seed.vector==seed) & !is.seed.vector & !is.na(trait.vector) & !is.na(degree.vector))
  }
  
  final.estimate <- rds2.estimate(degree.vector[!is.seed.vector], 
                                  trait.vector[!is.seed.vector],
                                  trait.value, running=FALSE)
  
  final.estimate.unweighted <- mean(trait.vector[!is.seed.vector], na.rm=TRUE)
  
  running.estimates <- list()
  running.estimates.unweighted <- list()
  n <- list()
  x <- list()
  envelopes <- list()
  
  jackknifed.estimates <- rep(NA, num.seeds)
  jackknifed.estimates.unweighted <- rep(NA, num.seeds)
  jackknifed.envelopes <- list()
  
  for (seed in 1:num.seeds) {
    # running estimate for this seed
    if (sum(seed.vector==seed) == 1) {
      # seed did not recruit
      if (VERBOSE) print(paste("[calculate.estimates.and.envelopes] Not calculating estimates and envelopes for seed (it did not recruit):", seed))
      running.estimates.unweighted[[seed]] <- NA
      running.estimates[[seed]] <- NA
      temp <- array(NA, dim=c(2, 1), dimnames=list(d1=c("lower", "upper"), d2=NULL))
      envelopes[[seed]] <- temp
      rm(temp)
      
      jackknifed.estimates[seed] <- final.estimate
      jackknifed.estimates.unweighted[seed] <- final.estimate.unweighted
      
      cases.to.include <- ((!is.seed.vector) & !(seed.vector == seed))
      running.jackknifed.estimates <- rds2.estimate(degree.vector[cases.to.include],
                                                    trait.vector[cases.to.include],
                                                    trait.value, running=TRUE)
      
      n[[seed]] <- 1:length(running.jackknifed.estimates)
      x[[seed]] <- running.jackknifed.estimates * n[[seed]]
      jackknifed.envelopes[[seed]] <- wilson.interval(x=x[[seed]], n=n[[seed]])
      
    } else {
      # seed did recruit
      if (VERBOSE) print(paste("[calculate.estimates.and.envelopes] Calculating estimates and envelopes for seed:", seed))
      
      # running estimates
      cases.to.include <- ((!is.seed.vector) & (seed.vector == seed))
      if (length(cases.to.include) != length(degree.vector)) stop("ERROR: cases.to.include is wrong length")
      running.estimates[[seed]] <- rds2.estimate(degree.vector[cases.to.include],
                                                 trait.vector[cases.to.include],
                                                 trait.value, running = TRUE)
      
      if (any(running.estimates[[seed]][ !(is.na(running.estimates[[seed]])) ] < 0)) {
        stop("ERROR: problem with running.estimates[[seed]], some are less than 0")
      }
      if (any(running.estimates[[seed]][ !(is.na(running.estimates[[seed]])) ] > 1)) {
        stop("ERROR: problem with running.estimates[[seed]], some are greater than 1")
      }

      # running estimates unweighted
      cases.to.include <- ((!is.seed.vector) & (seed.vector == seed) & (!is.na(trait.vector)))    
      trait.vector.to.include <- trait.vector[cases.to.include]
      running.estimates.unweighted[[seed]] <- rep(NA, length(trait.vector.to.include))
      for (i in 1:length(trait.vector.to.include)) {
        running.estimates.unweighted[[seed]][i] <- sum(trait.vector.to.include[1:i]==trait.value)/i
      }
      if (any(running.estimates.unweighted[[seed]][ !(is.na(running.estimates.unweighted[[seed]])) ] < 0)) {
        stop("ERROR: running.estimates.unweighted[[seed]], some are less than 0")
      }
      if (any(running.estimates.unweighted[[seed]][ !(is.na(running.estimates.unweighted[[seed]])) ] > 1)) {
        print(running.estimates.unweighted[[seed]])
        stop("ERROR: running.estimates.unweighted[[seed]], some are greater than 1")
      }    
      
      # jackknife estimates
      cases.to.include <- ((!is.seed.vector) & !(seed.vector == seed))
      jackknifed.estimates[seed] <- rds2.estimate(degree.vector[cases.to.include],
                                                  trait.vector[cases.to.include],
                                                  trait.value, running = FALSE)

      cases.to.include <- ((!is.seed.vector) & !(seed.vector == seed))
      jackknifed.estimates.unweighted[seed] <- mean(trait.vector[cases.to.include], na.rm=TRUE)
      
      # calculate Wilson intervals      
      n[[seed]] <- 1:length(running.estimates[[seed]])
      x[[seed]] <- running.estimates[[seed]] * n[[seed]]
      if (VERBOSE) print(paste("[calculate.estimates.and.envelopes] Calculating envelopes for seed:", seed))      
      envelopes[[seed]] <- wilson.interval(x=x[[seed]], n=n[[seed]])

      cases.to.include <- ((!is.seed.vector) & !(seed.vector == seed))
      running.jackknifed.estimates <- rds2.estimate(degree.vector[cases.to.include],
                                                    trait.vector[cases.to.include],
                                                    trait.value, running=TRUE)
      
      n[[seed]] <- 1:length(running.jackknifed.estimates)
      x[[seed]] <- running.jackknifed.estimates * n[[seed]]
      jackknifed.envelopes[[seed]] <- wilson.interval(x=x[[seed]], n=n[[seed]])
            
    }    
  }

  # get final estimates
  final.estimates <- rep(NA, num.seeds)
  for (seed in 1:num.seeds) {
    if (length(running.estimates[[seed]]) > 0) {
      final.estimates[seed] <- running.estimates[[seed]][length(running.estimates[[seed]])]
    }
  }
  
  if (VERBOSE) print("[calculate.estimates.and.envelopes] Finished calculations")
  
  # checks
  if (any(is.na(offspring.this.seed.no.missing.vec))) {
    stop("ERROR: offspring.this.seed.no.missing.vec has NA")
  }
  if (any(is.na(offspring.this.seed.vec))) {
    stop("ERROR: offspring.this.seed.vec has NA")
  }
  if (any(offspring.this.seed.vec < offspring.this.seed.no.missing.vec)) {
    print("offspring.this.seed.vec")
    print(offspring.this.seed.vec)
    print("offspring.this.seed.no.missing.vec")
    print(offspring.this.seed.no.missing.vec)
    stop("ERROR: any(offspring.this.seed.vec < offspring.this.seed.no.missing.vec)")
  }
  
  return(list(final.estimate=final.estimate, 
              final.estimate.unweighted=final.estimate.unweighted,
              running.estimates=running.estimates,
              running.estimates.unweighted=running.estimates.unweighted,
              envelopes=envelopes,
              seeds.that.recruit=seeds.that.recruit,
              offspring.this.seed.vec=offspring.this.seed.vec,
              offspring.this.seed.no.missing.vec=offspring.this.seed.no.missing.vec,
              final.estimates=final.estimates,
              jackknifed.estimates=jackknifed.estimates,
              jackknifed.estimates.unweighted=jackknifed.estimates.unweighted,
              jackknifed.envelopes=jackknifed.envelopes))
}



make.bottleneck.plot <- function(results.this.trait,
                                 trait.vector,
                                 is.seed.vector, y.label.str,
                                 p.values,
                                 filename,
                                 weighted=TRUE,
                                 VERBOSE=FALSE) {
  
  # NOTES: These plots could look funny if there is a seed that is missing data on trait.vector
  # running.estimates and envelopes are lists
  # one element for each seed
  # trait.vector and is.seed.vector are for the seed panel of the plot
  # weighted: should we plot weighted (as opposed to unweighted) estimates
  
  # pull out results from list that is passed in
  running.estimates <- results.this.trait$running.estimates
  running.estimates.unweighted <- results.this.trait$running.estimates.unweighted
  envelopes <- results.this.trait$envelopes
  final.estimate <- results.this.trait$final.estimate
  final.estimate.unweighted <- results.this.trait$final.estimate.unweighted  
  offspring.this.seed.vec <- results.this.trait$offspring.this.seed.vec
  final.estimates <- results.this.trait$final.estimates
  jackknifed.envelopes <- results.this.trait$jackknifed.envelopes
  
  # check input
  # check to make sure the number of chains are the same
  if (sum(is.seed.vector) != length(envelopes)) {
    print(paste("sum(is.seed.vector):", sum(is.seed.vector)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: envelopes does not match the number of seeds")
  }
  if (!(is.list(running.estimates))) stop("ERROR: running.estimates is not a list")
  if (!(is.list(envelopes))) stop("ERROR: envelopes is not a list")
  if (length(trait.vector) != length(is.seed.vector)) stop("ERROR: trait.vector and is.seed.vector do not match")
  if (length(offspring.this.seed.vec) != length(envelopes)) {
    print(paste("length(offspring.this.seed.vec):", length(offspring.this.seed.vec)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: length(offspring.this.seed.vec) != length(envelopes)")
  }
  # finished checking input
  
  if (VERBOSE) print(paste("Beginning creating:", filename))
  pdf(filename)
  
  axis.mag <- 1
  label.mag <- 1
  main.mag <- 0.9
  
  num.seeds <- sum(is.seed.vector)
  num.most.offspring <- max(offspring.this.seed.vec)
  
  bottom.mar <- 3.7
  top.mar <- 1.5
  seed.box.length <- 0.08
  final.estimates.box.length <- 0
  
  #############
  # main box
  ##############
  if (VERBOSE) print("[make.bottleneck.plot] Beginning drawing estimates and envelopes")
  estimates.line.color <- "black"
  ci.shading.color <- "grey84"
  background.color <- "grey90"
  final.estimate.color <- rgb(0, 0, 0, alpha=1)
  
  # title.str <- paste(filename, "\n", paste(p.values, collapse=", "))
  title.str <- ""
  par(fig=c(seed.box.length, 1 - final.estimates.box.length, 0, 1));
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar, 1.5, top.mar, 2));  # bottom, left, top, right
  plot(c(0, num.most.offspring), c(0, 1), type="n", 
       xlab="", ylab=y.label.str, 
       main=title.str, axes=TRUE, 
       cex.axis=axis.mag, cex.lab=label.mag, cex.main=main.mag, yaxt="n")
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), las=2, hadj=0.7)
  mtext(text="Cumulative sample size", side=1, line=1.9)
  polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color)
  box()
  
  # plot complete RDS estimate
  if (weighted) {
    estimate.to.plot <- final.estimate
  } else {
    estimate.to.plot <- final.estimate.unweighted
  }
  lines(c(0 - 999, num.most.offspring + 999), rep(estimate.to.plot, 2),
        lty="solid", col="white", lwd=3)
  rm(estimate.to.plot)

  for (seed in 1:num.seeds) {
    
    if (weighted) {
      data.to.plot <- running.estimates
      
#       seed.interval <- envelopes[[seed]][ , ncol(envelopes[[seed]])]
#       jackknifed.interval <- jackknifed.envelopes[[seed]][ , ncol(jackknifed.envelopes[[seed]])]
#       intervals.overlap <- overlaps(seed.interval, jackknifed.interval)
#       if (is.na(intervals.overlap)) {
#         color <- "black"
#       } else {
#         if (intervals.overlap) {
#           color <- "blue"
#         } else {
#           color <- "red"
#         }
#       }
      color <- "black"
    } else {    
      stop("ERROR: unweighted and and intervals don't make sense")
    }
    
    if (VERBOSE) {
      print("[make.bottleneck.plot] data.to.plot:")
      print(data.to.plot)
    }
    
    if (VERBOSE) print(paste("[make.bottleneck.plot] plotting estimates for seed:", seed))
    if (length(data.to.plot[[seed]])==1) {
      # only one estimate so we use point
      points(x=1, y=data.to.plot[[seed]], pch=".")
    } else {
      # more than one point so we can use line
      lines(data.to.plot[[seed]], col=color, lwd=1)
    }
    points(x=length(data.to.plot[[seed]]), 
           y=data.to.plot[[seed]][length(data.to.plot[[seed]])],
           pch=16, col=color)
  }
  if (VERBOSE) print("[make.bottleneck.plot] Finished drawing estimates and envelopes")
  # now add tick marks on axis 4 with number of times that each value appears
  if (weighted) axis(side=4, at=final.estimates, labels=rep("", length(final.estimates)))
  results.table <- table(final.estimates)
  for (i in 1:length(results.table)) { 
    if (results.table[i] > 1) {
      mtext(text=results.table[i], side=4, at=as.numeric(names(results.table[i])), las=2, line=0.8)
    }
  } 
  if (VERBOSE) print("[make.bottleneck.plot] Finished final estimates and intervals box")
  
  ##############
  # seed panel
  ##############
  par(fig=c(0, seed.box.length, 0, 1), new=TRUE);
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar,0.5,top.mar,1)); # bottom, left, top, right
  
  plot(c(0,0), c(0,1), type="n", axes=FALSE, main="Seeds", 
       xlab="", ylab="", 
       cex.main=0.5);
  axis(side=4, at=c(0,1), labels=c("",""));
  box();
  
  x.location <- 0; # set by trial and error
  scaling.factor.size.of.circle <- 0.25; # set by trial and error
  scaling.factor.location.of.text <- 100; # set by trial and error
  c <- 0.022;
  # zeros
  seeds.zero <- sum(trait.vector[as.logical(is.seed.vector)]==0, na.rm=TRUE);
  radius.zero <- (seeds.zero/pi)^0.5; 
  if (seeds.zero > 0) symbols(x=x.location, y=0, circles=scaling.factor.size.of.circle * radius.zero, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=0 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.zero, labels=seeds.zero); # plot number of seeds (text)
  # ones
  seeds.one <- sum(trait.vector[as.logical(is.seed.vector)]==1, na.rm=TRUE);
  radius.one <- (seeds.one/pi)^0.5; 
  if (seeds.one > 0) symbols(x=x.location, y=1, circles=scaling.factor.size.of.circle * radius.one, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=1 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.one, labels=seeds.one); # plot number of seeds (text)
  # missing
  seeds.missing.data <- sum(is.na(trait.vector[as.logical(is.seed.vector)]));
  radius.missing.data <- (seeds.missing.data/pi)^0.5; 
  if (seeds.missing.data > 0) {
    symbols(x=x.location, y=0.5, circles=scaling.factor.size.of.circle * radius.missing.data, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
    text(x=x.location, y=0.5 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels=seeds.missing.data); # plot number of seeds (text)
    text(x=x.location, y=0.5 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels="?");
  }
  # CHECK
  if (seeds.zero + seeds.one + seeds.missing.data != num.seeds) { 
    print(paste("seeds.zero:", seeds.zero))
    print(paste("seeds.one:", seeds.one))
    print(paste("seeds.missing.data:", seeds.missing.data))
    print(paste("num.seeds:", num.seeds))
    stop("ERROR: problem with area plots of seeds")
  }
  rm(x.location, scaling.factor.size.of.circle, scaling.factor.location.of.text, c); # clean up
  
  dev.off()
  if (VERBOSE) print(paste("[make.bottleneck.plot] Finished creating:", filename))
  
}

make.bottleneck.jackknife.plot <- function(results.this.trait,
                                           filename,
                                           VERBOSE=FALSE) {
  
  # NOTES: These plots could look funny if there is a seed that is missing data on trait.vector
  # running.estimates and envelopes are lists
  # one element for each seed
  # trait.vector and is.seed.vector are for the seed panel of the plot
  # weighted: should we plot weighted (as opposed to unweighted) estimates
  
  # pull out results from list that is passed in
  final.estimate <- results.this.trait$final.estimate
  jackknifed.estimates <- results.this.trait$jackknifed.estimates
  final.estimates <- results.this.trait$final.estimates
  envelopes <- results.this.trait$envelopes
  jackknifed.envelopes <- results.this.trait$jackknifed.envelopes 
  offspring.this.seed.vec <- results.this.trait$offspring.this.seed.vec
  num.seeds <- length(offspring.this.seed.vec)

  # check input
  # check to make sure the number of chains are the same
  if (!(is.list(envelopes))) stop("ERROR: envelopes is not a list")
  if (length(offspring.this.seed.vec) != length(envelopes)) {
    print(paste("length(offspring.this.seed.vec):", length(offspring.this.seed.vec)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: length(offspring.this.seed.vec) != length(envelopes)")
  }
  # finished checking input
  
  if (VERBOSE) print(paste("Beginning creating:", filename))
  pdf(filename)
  
  fuzz <- 0.1
  axis.mag <- 1
  label.mag <- 1
  main.mag <- 1
  bottom.mar <- 3.7
  top.mar <- 1.5
  estimates.line.color <- "black"
  background.color <- "grey90"
  final.estimate.color <- rgb(0, 0, 0, alpha=1)
  
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar, 2.5, top.mar, 1));  # bottom, left, top, right
  plot(c(1, num.seeds), c(0, 1), type="n", 
       xlab="", ylab="", 
       main=filename, axes=TRUE, 
       cex.axis=axis.mag, cex.lab=label.mag, cex.main=main.mag, yaxt="n")
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), las=2, hadj=0.7)
  mtext(text="Seed", side=1, line=1.9)
  polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color)
  box()
  
  # plot complete RDS estimate
  lines(c(-999, 999), rep(final.estimate, 2), lty="solid", col="white", lwd=3)
  
  for (seed in 1:num.seeds) {
    if (VERBOSE) print(paste("[make.bottleneck.jackknife.plot] plotting estimates for seed:", seed))
    
    # check for overlap
    seed.interval <- envelopes[[seed]][ , ncol(envelopes[[seed]])]
    jackknifed.interval <- jackknifed.envelopes[[seed]][ , ncol(jackknifed.envelopes[[seed]])]
    intervals.overlap <- overlaps(seed.interval, jackknifed.interval)
    if (is.na(intervals.overlap)) {
      color <- "black"
    } else {
      if (intervals.overlap) {
        color <- "blue"
      } else {
        color <- "red"
      }
    }
    
    # plot estimate and interval from seed
    points(seed - fuzz, final.estimates[seed], pch=16, col=color)
    if (ncol(envelopes[[seed]]) > 0) {
      lines(rep((seed - fuzz), 2), seed.interval, lwd=1, col=color)
    }
    # plot estimate and interval from jackknife
    points(x=seed + fuzz, y=jackknifed.estimates[seed], pch=1, col=color)    
    lines(rep((seed + fuzz), 2), jackknifed.interval, lwd=1, col=color)
  }
  if (VERBOSE) print("[make.bottleneck.jackknife.plot] Finished drawing estimates")
  
  dev.off()
  if (VERBOSE) print(paste("[make.bottleneck.jackknife.plot] Finished creating:", filename))
  
}




calculate.interval.overlap <- function(results.this.trait, cutoff=1, VERBOSE=FALSE) {
  
  # results is just for one trait
  # it should be a list that has at least
  # running.estimates 
  # envelopes
  # final.estimate
  
  # cutoff: only consider chains >= cutoff
  
  # check inputs
  if (!(is.list(results.this.trait))) {
    stop("ERROR: results in not a list")
  }
  if (cutoff < 1) {
    stop("ERROR: lower cutoff less than 1; 
         this is a problem because we cant handle chains of length 0")
  }
  # finsish checks
  
  offspring.this.seed.vec <- results.this.trait$offspring.this.seed.vec
  if (VERBOSE) print("[calcualte.interval.overlap] offpsring.this.seed.vec:")
  if (VERBOSE) print(offspring.this.seed.vec)
  num.seeds <- length(offspring.this.seed.vec)
  if (VERBOSE) print(paste("[calcualte.interval.overlap] num.seeds:", num.seeds))
  
  # create matrix including NAs for envelopes (this helps because list is different lengths
  upper.ci.vec <- rep(NA, num.seeds)
  lower.ci.vec <- rep(NA, num.seeds)
  for (seed in 1:num.seeds) {
    if (VERBOSE) print(paste("[calcualte.interval.overlap] seed:", seed))
    if (offspring.this.seed.vec[seed] >= cutoff) {
      upper.ci.vec[seed] <- results.this.trait$envelopes[[seed]]["upper", offspring.this.seed.vec[seed]]
      lower.ci.vec[seed] <- results.this.trait$envelopes[[seed]]["lower", offspring.this.seed.vec[seed]]
    }
  }
  if (VERBOSE) {
    print("[calcualte.interval.overlap] lower.ci.vec:")
    print(lower.ci.vec)
  }
  if (VERBOSE) {
    print("[calcualte.interval.overlap] upper.ci.vec:")    
    print(upper.ci.vec)
  }
  
  overlap.mat <- matrix(NA, nrow=num.seeds, ncol=num.seeds)
  interval.1 <- rep(NA, 2)
  interval.2 <- rep(NA, 2)
  for (seed.1 in 1:num.seeds) {
    for (seed.2 in 1:num.seeds) {
      interval.1 <- c(lower.ci.vec[seed.1], upper.ci.vec[seed.1])
      names(interval.1) <- c("lower", "upper")
      interval.2 <- c(lower.ci.vec[seed.2], upper.ci.vec[seed.2])
      names(interval.2) <- c("lower", "upper")      
      overlap.mat[seed.1, seed.2] <- overlaps(interval.1=interval.1,
                                              interval.2=interval.2)
    }
  }
  
  if (VERBOSE) print("[calcualte.interval.overlap] overlap.mat:")
  if (VERBOSE) print(overlap.mat)
  
  overlaps.all.vec <- rep(NA, num.seeds)
  for (seed in 1:num.seeds) {
    if (all(is.na(overlap.mat[seed, ]))) {
      # overlap.mat[seed, ] are all NA
      # in this case all( , na.rm=TRUE) does not work as desired so we introduce a special case
      overlaps.all.vec[seed] <- NA
    } else {
      # there are some non NA for this seed
      overlaps.all.vec[seed] <- all(overlap.mat[seed, ]==TRUE, na.rm=TRUE)
    }
  }  
  if (!is.logical(overlaps.all.vec)) stop("ERROR: overlaps.all.vec is not TRUE/FALSE/NA")
  if (VERBOSE) {
    print(paste("[calcualte.interval.overlap] overlaps.all.vec:"))
    print(overlaps.all.vec)
  }

  if (all(overlaps.all.vec==TRUE, na.rm=TRUE)) {
    FAIL <- FALSE
  } else {
    FAIL <- TRUE
  }

  if (VERBOSE) print(paste("[calcualte.interval.overlap] FAIL:", FAIL))
  
  return(overlaps.all.vec)
}





make.allpoints.plot <- function(seed.of.origin.vector, 
                                trait.vector,
                                degree.vector,
                                filename, 
                                VERBOSE=FALSE,
                                color=TRUE) {
  
  # CHECK INPUTS
  if (any(trait.vector < 0, na.rm=TRUE)) stop("ERROR: some trait.vector < 0")
  if (any(trait.vector > 1, na.rm=TRUE)) stop("ERROR: some trait.vector > 1")
  if (length(seed.of.origin.vector) != length(trait.vector)) {
    stop("ERROR: seed.of.origin and trait.vector are not same length")
  }      
  if (length(seed.of.origin.vector) != length(degree.vector)) {
    stop("ERROR: seed.of.origin and degree.vector are not same length")
  }
  
  pdf(filename)
  par(mar=c(3, 2, 0, 0), oma=c(1, 1, 1, 1))
  
  # might need rgb so that we can set alpha
  if (color) {
    infected.color <- rgb(255, 0, 0, 255, maxColorValue=255)
    uninfected.color <- rgb(0, 0, 255, 255, maxColorValue=255)
  } else {
    infected.color <- "black"
    uninfected.color <- "black"    
  }
  fuzz <- 0.35
  sample.size <- length(seed.of.origin.vector)
  
  x.min <- 1
  x.max <- sample.size
  y.min <- 1 - fuzz
  y.max <- max(seed.of.origin.vector) + fuzz
  
  plot(c(0, 1), c(0, 1),
       xlim=c(x.min, x.max), ylim=c(y.min, y.max),
       xlab="", ylab="Seed", type="n", yaxt="n", xaxt="n",
       main="", axes=FALSE)
  #polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color)
  axis(side=1, at=c(1,seq(from=50, to=9999, by=50)))
  axis(side=2, at=1:max(seed.of.origin.vector), las=2, hadj=0.3)
  mtext("Seed", side=2, line=1.5)
  mtext("Sample element", side=1, line=2.1)
  box()
  
  for (current.seed.of.origin in sort(unique(seed.of.origin.vector))) {
    
    if (VERBOSE) print(paste("current.seed.of.origin:", current.seed.of.origin))

    lines(c(1, sample.size), rep(current.seed.of.origin, 2), lty=3, lwd=0.5)
    #lines(c(-999, 999), rep(current.seed.of.origin + 0.5, 2), lty=1, lwd=0.5)
    
    # plot "infected"
    PLOT.CASE <- ((seed.of.origin.vector==current.seed.of.origin) & (trait.vector==1))
    PLOT.CASE[is.na(PLOT.CASE)] <- FALSE; # remove NA
    if (length(which(PLOT.CASE==TRUE)) > 0) {
      for (x.val in which(PLOT.CASE==TRUE)) {
        lines(x=rep(x.val, 2), 
              y=c(current.seed.of.origin, current.seed.of.origin + fuzz), 
              lwd=1, col=infected.color)  
      }
    }
    
    # plot "uninfected"
    PLOT.CASE <- ((seed.of.origin.vector==current.seed.of.origin) & (trait.vector==0))
    PLOT.CASE[is.na(PLOT.CASE)] <- FALSE; # remove NA
    if (length(which(PLOT.CASE==TRUE)) > 0) {
      for (x.val in which(PLOT.CASE==TRUE)) {
        lines(x=rep(x.val, 2), 
              y=c(current.seed.of.origin, current.seed.of.origin - fuzz), 
              lwd=1, col=uninfected.color)  
      }
    }
    
  } # seed.of.origin loop
  
  dev.off()
}


rds2.estimate <- function(degree.vector, trait.vector, trait.value, running = FALSE){
  
  if (length(trait.vector) == 1) {
    # sample only has one element
    if (is.na(trait.vector)) {
      muHat <- NA;
    } else {
      if (trait.vector == trait.value) {
        muHat <- 1;
      } else {
        muHat <- 0;
      }
    }
    return(muHat);
  } else {
    # calculate weight
    # note that some people have degree 0 so their weight becomes infinite 
    weight <- 1/degree.vector;
    numerator.vector <- as.numeric(trait.vector == trait.value) * weight;
    # we want to exclude cases where degree = 0 degree = NA or trait = NA
    exclude <- ( (degree.vector == 0) | is.na(degree.vector) | (is.na(trait.vector)) )
    #print(cbind(weight, numerator.vector, exclude))
    
    if (running) {
      numerator.vector[exclude] <- 0; # set to 0 because it doesn't affect cumulative sum
      weight[exclude] <- 0; # set to 0 because it doesn't affect cumulative sum
      
      runningMuHat <- cumsum(numerator.vector)/cumsum(weight);
      return(runningMuHat);
    } else {
      muHat <- sum(numerator.vector[!exclude])/sum(weight[!exclude]);
      return(muHat);
    }
  }
}

plot.side.by.side.estimates <- function(array.estimate.1, array.estimate.2, 
                                        text.str, filename) {

  
  if (any(is.na(array.estimate.1))) stop("ERROR: array.estimate.1 has NA")
  if (any(is.na(array.estimate.2))) stop("ERROR: array.estimate.2 has NA")
  
  pdf(file=filename)
  
  par(mfrow=c(3,4), mar=c(3,1.8,1,1), oma=c(1,1,1,1))
  fuzz <- 0.1;
  marker.type.1=16;
  marker.type.2=1;
  for (group in group.vec) {
    for (city in city.vec) {
      plot(c(0.8,length(trait.vec)+0.2), c(0,1), 
           type='n', ylim=c(0,0.15), 
           main=paste(toupper(group), ",", toupper(city)), 
           xlab="", ylab="", xaxt="n", yaxt="n"); # canvas sub=paste("Degree quests: ", subtitle.text, sep="")
      axis(1, 1:4, trait.labels.short, cex.axis=0.85);
      axis(2, at=c(0, 0.05, 0.10, 0.15))
      axis(4, at=c(0, 0.05, 0.10, 0.15), labels=c("", "", "", ""));
      counter <- 0
      for (trait in trait.vec) {
        counter <- counter + 1
        estimate.1 <- array.estimate.1[group, city, trait];
        estimate.2 <- array.estimate.2[group, city, trait];
        points(counter - fuzz, estimate.1, pch=marker.type.1);
        points(counter + fuzz, estimate.2, pch=marker.type.2);
        rm(estimate.1, estimate.2); # clean up
      }
    }
  }
  mtext(text.str, 3, outer=TRUE);
  dev.off()
}

find.parent <- function(mySample, coupon.number) {
  # this function takes a coupon number and returns the row of the parent in mySample
  # if there is no parent (ie seed) it returns NA
  
  # The column which stores the coupon numbers have to be OutCoupon1, OutCoupon2, OutCoupon3
  # If there are more than 3 OutCoupons this will not work
  
  # CHECK TO MAKE SURE COUPON IS NUMBER VALID
  if (sum(mySample$InCoupon == coupon.number)==0) {
    stop(paste("ERROR: Invalid coupon number.  There is no record with InCoupon =", coupon.number));
  }
  
  if (length(which(mySample$OutCoupon1 == coupon.number))==1) {
    parent.row <- which(mySample$OutCoupon1 == coupon.number);
  } else if (length(which(mySample$OutCoupon2 == coupon.number))==1) {
    parent.row <- which(mySample$OutCoupon2 == coupon.number);
  } else if (length(which(mySample$OutCoupon3 == coupon.number))==1) {
    parent.row <- which(mySample$OutCoupon3 == coupon.number);
  } else {
    parent.row <- NA;
  }
  return(parent.row);
  
}

overlaps <- function(interval.1, interval.2) {
  if (length(interval.1) != 2) stop("ERROR: interval.1 is not the right size")
  if (length(interval.2) != 2) stop("ERROR: interval.2 is not the right size")

  if ((all(!is.na(interval.1))) & (all(!is.na(interval.2)))) {
    # no NAs in interval
    RESULT <- TRUE
    if (interval.1["lower"] > interval.2["upper"]) {
      RESULT <- FALSE
    }
    if (interval.1["upper"] < interval.2["lower"]) {
      RESULT <- FALSE
    }
  } else {
    RESULT <- NA  
  } 
  
  return(RESULT)
}

calculate.test.statistics <- function(results.this.trait, test.statistics.labels, VERBOSE=FALSE) {
  results.vec <- rep(NA, length(test.statistics.labels))
  names(results.vec) <- test.statistics.labels 
  
  deviation.vec <- (results.this.trait$final.estimates - results.this.trait$final.estimate)
  if (VERBOSE) {
    print("deviation.vec:")
    print(deviation.vec)
  }
  jackknifed.deviation.vec <- (results.this.trait$final.estimates - results.this.trait$jackknifed.estimates)
  if (VERBOSE) {
    print("jackknifed.deviation.vec:")
    print(jackknifed.deviation.vec)
  }
  n.vec <- results.this.trait$offspring.this.seed.no.missing.vec
  if (VERBOSE) {
    print("n.vec:")
    print(n.vec)
  }
  
  # deviation.vec and jackknifed.deviation.vec can have some NA but not all
  # n.vec should have no NA
  if (all(is.na(deviation.vec))) stop("ERROR: deviation.vec is all NA")
  if (all(is.na(jackknifed.deviation.vec))) stop("ERROR: jackknifed.deviation.vec is all NA")
  if (any(is.na(n.vec))) stop("ERROR: n.vec has NA")
  
  results.vec["total.squared.deviation.weight.n"] <- sum(n.vec * deviation.vec^2, na.rm=TRUE)
  results.vec["total.squared.deviation.weight.sqrt.n"] <- sum(sqrt(n.vec) * deviation.vec^2, na.rm=TRUE)
  results.vec["total.squared.deviation.jackknife.weight.n"] <- sum(n.vec * jackknifed.deviation.vec^2, na.rm=TRUE)
  results.vec["total.squared.deviation.jackknife.weight.sqrt.n"] <- sum(sqrt(n.vec) * jackknifed.deviation.vec^2, na.rm=TRUE)
  if (VERBOSE) print(results.vec)
  
  # check
  # note that final.estimates can have NA because sometimes a seed does not recruit
  if (any(is.na(results.vec))) stop("ERROR: results.vec has NA")
  if (results.vec["total.squared.deviation.weight.n"] < results.vec["total.squared.deviation.weight.sqrt.n"]) {
    print(results.vec)
    stop("ERROR: results.vec[total.squared.deviation.weight.n] < results.vec[total.squared.deviation.weight.sqrt.n]")
  }
  if (results.vec["total.squared.deviation.jackknife.weight.n"] < results.vec["total.squared.deviation.jackknife.weight.sqrt.n"]) {
    print(results.vec)
    stop("ERROR: results.vec[total.squared.deviation.jackknife.weight.n] < results.vec[total.squared.deviation.jackknife.weight.sqrt.n]")
  }
  
  return(results.vec)
}

plot.distribution.test <- function(test.value, distribution, filename) {

  # test.values is the observed value
  # distribution is the set of values under the permutation test
  
  if (is.na(test.value)) stop("ERROR: test.value is NA")
  if (any(is.na(distribution))) stop("ERROR: distribution has NA")

  x.min <- min(test.value, distribution)
  x.max <- max(test.value, distribution)
  
  p.value <- mean(test.value <= distribution)
  
  plot(density(distribution),
       xlim=c(x.min, x.max),
       main=paste(filename, "\nEmpirical p-value:", p.value))
  lines(rep(test.value, 2), c(-999999, 999999), col="green")
  
}


get.traits.to.consider <- function(group, labels=FALSE) {
  # if labels=TRUE we want labels used in graphs (e.g., "HIV+")
  # if labels=FALSE we want labels used to index data (e.g., "HIVPositive")
  
  if (!(group %in% c("DU", "FSW", "MSM"))) {
    print(paste("group:", group))
    stop("ERROR: invalid group")
  }
  
  # Define traits to be used in tests
  common.trait.str.vec <- c("HIVPositive", "SYPHPositive", "Had.HIV.Test")
  common.trait.str.labels.vec <- c("HIV+", "Syphilis+", "Had HIV Test")
  du.trait.vec <- c("Working", "Main.Drug.Crack", "Main.Drug.Cocaine", "Main.Drug.Marijuana", "Female", "Risky.Sex", "Use.Drugs.Every.Day", "Been.Imprisoned", "Paid.A.Woman.For.Sex")
  du.trait.labels.vec <- c("Working", "Main Drug Crack", "Main Drug Cocaine", "Main Drug Marijuana", "Female", "Risky Sex", "Use Drugs Every Day", "Been Imprisoned", "Paid For Sex")
  fsw.trait.vec <- c("Use.Condom.Last.Client", "Use.Drugs", "Last.Client.Street", "Last.Client.Brothel", "Participated.In.Program")
  fsw.trait.labels.vec <- c("Used Condom", "Use Drugs", "Last Client Street", "Last Client Brothel", "Been In Program")
  msm.trait.vec <- c("Working", "Use.Drugs", "Heterosexual", "Bisexual", "Trans", "Used.Condom", "Sex.With.Woman")
  msm.trait.labels.vec <- c("Working", "Use Drugs", "Heterosexual", "Bisexual", "Trans", "Used Condom", "Sex With Woman")

  if (labels==FALSE) {
    if (group=="DU") traits.to.consider <- c(common.trait.str.vec, du.trait.vec)
    if (group=="FSW") traits.to.consider <- c(common.trait.str.vec, fsw.trait.vec)
    if (group=="MSM") traits.to.consider <- c(common.trait.str.vec, msm.trait.vec)
  } else {
    if (group=="DU") traits.to.consider <- c(common.trait.str.labels.vec, du.trait.labels.vec)
    if (group=="FSW") traits.to.consider <- c(common.trait.str.labels.vec, fsw.trait.labels.vec)
    if (group=="MSM") traits.to.consider <- c(common.trait.str.labels.vec, msm.trait.labels.vec)    
  }
  
  return(traits.to.consider)
}




























############ OUTTAKES


make.bottleneck.plot.old <- function(results.this.trait,
                                     trait.vector,
                                     is.seed.vector, y.label.str,
                                     p.value,
                                     filename,
                                     weighted=TRUE,
                                     VERBOSE=FALSE) {
  
  # NOTES: These plots could look funny if there is a seed that is missing data on trait.vector
  # running.estimates and envelopes are lists
  # one element for each seed
  # trait.vector and is.seed.vector are for the seed panel of the plot
  # weighted: should we plot weighted (as opposed to unweighted) estimates
  
  # pull out results from list that is passed in
  running.estimates <- results.this.trait$running.estimates
  running.estimates.unweighted <- results.this.trait$running.estimates.unweighted
  envelopes <- results.this.trait$envelopes
  final.estimate <- results.this.trait$final.estimate
  final.estimate.unweighted <- results.this.trait$final.estimate.unweighted  
  offspring.this.seed.vec <- results.this.trait$offspring.this.seed.vec
  final.estimates <- results.this.trait$final.estimates
  
  # check input
  # check to make sure the number of chains are the same
  if (sum(is.seed.vector) != length(envelopes)) {
    print(paste("sum(is.seed.vector):", sum(is.seed.vector)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: envelopes does not match the number of seeds")
  }
  if (!(is.list(running.estimates))) stop("ERROR: running.estimates is not a list")
  if (!(is.list(envelopes))) stop("ERROR: envelopes is not a list")
  if (!(is.numeric(p.value))) stop("ERROR: p.value is not a numeric")
  if (length(trait.vector) != length(is.seed.vector)) stop("ERROR: trait.vector and is.seed.vector do not match")
  if (length(offspring.this.seed.vec) != length(envelopes)) {
    print(paste("length(offspring.this.seed.vec):", length(offspring.this.seed.vec)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: length(offspring.this.seed.vec) != length(envelopes)")
  }
  # finished checking input
  
  if (VERBOSE) print(paste("Beginning creating:", filename))
  pdf(filename)
  
  axis.mag <- 1
  label.mag <- 1
  main.mag <- 1
  
  num.seeds <- sum(is.seed.vector)
  num.most.offspring <- max(offspring.this.seed.vec)
  
  bottom.mar <- 3.7
  top.mar <- 1.5
  seed.box.length <- 0.08
  final.estimates.box.length <- 0
  
  #############
  # main box
  ##############
  if (VERBOSE) print("[make.bottleneck.plot] Beginning drawing estimates and envelopes")
  estimates.line.color <- "black"
  ci.shading.color <- "grey84"
  background.color <- "grey90"
  final.estimate.color <- rgb(0, 0, 0, alpha=1)
  
  par(fig=c(seed.box.length, 1 - final.estimates.box.length, 0, 1));
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar, 1.5, top.mar, 1));  # bottom, left, top, right
  plot(c(0, num.most.offspring), c(0, 1), type="n", 
       xlab="", ylab=y.label.str, 
       main=paste(filename, p.value), axes=TRUE, 
       cex.axis=axis.mag, cex.lab=label.mag, cex.main=main.mag, yaxt="n")
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), las=2, hadj=0.7)
  mtext(text="Cumulative sample size", side=1, line=1.9)
  polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color)
  box()
  
  # plot complete RDS estimate
  if (weighted) {
    estimate.to.plot <- final.estimate
  } else {
    estimate.to.plot <- final.estimate.unweighted
  }
  lines(c(0 - 999, num.most.offspring + 999), rep(estimate.to.plot, 2),
        lty="solid", col="white", lwd=3)
  rm(estimate.to.plot)
  
  # plot envelopes first so that estimate go over top of them
  #   for (seed in 1:num.seeds) {
  #     if (VERBOSE) print(paste("[make.bottleneck.plot] plotting envelopes for seed:", seed))
  #     if (length(running.estimates[[seed]])==1) {
  #       # only one point so we use lines
  #       lines(x=c(1, 1), y=envelopes[[seed]][, 1], col=ci.shading.color)
  #     } else {
  #       # more than one point so we do envelopes
  #       # plot confidence "envelopes"
  #       estimates.temp <- running.estimates[[seed]];
  #       upper.ci.temp <- envelopes[[seed]]["upper", ];
  #       lower.ci.temp <- envelopes[[seed]]["lower", ];
  #       upper.ci.temp[(upper.ci.temp > 1)] <- 1; #top code
  #       lower.ci.temp[(lower.ci.temp < 0)] <- 0; #bottom code
  #       
  #       polygon(x=c(1:length(estimates.temp), length(estimates.temp):1),
  #               y=c(upper.ci.temp, rev(estimates.temp)), 
  #               col=ci.shading.color, border=NA) # upper
  #       polygon(x=c(1:length(estimates.temp), length(estimates.temp):1),
  #               y=c(lower.ci.temp, rev(estimates.temp)),
  #               col=ci.shading.color, border=NA) # lower
  #       rm(estimates.temp, upper.ci.temp, lower.ci.temp)
  #     }
  #   }
  if (VERBOSE) print("[make.bottleneck.plot] finished plotting envelopes")
  
  # now plot estimates over envelopes
  if (weighted) {
    data.to.plot <- running.estimates
  } else {
    data.to.plot <- running.estimates.unweighted
  }
  if (VERBOSE) {
    print("[make.bottleneck.plot] data.to.plot:")
    print(data.to.plot)
  }
  
  for (seed in 1:num.seeds) {
    if (VERBOSE) print(paste("[make.bottleneck.plot] plotting estimates for seed:", seed))
    if (length(data.to.plot[[seed]])==1) {
      # only one estimate so we use point
      points(x=1, y=data.to.plot[[seed]], pch=".")
    } else {
      # more than one point so we can use line
      lines(data.to.plot[[seed]], col=estimates.line.color, lwd=1)
    }
    points(x=length(data.to.plot[[seed]]), 
           y=data.to.plot[[seed]][length(data.to.plot[[seed]])],
           pch=16, col=final.estimate.color)
  }
  if (VERBOSE) print("[make.bottleneck.plot] Finished drawing estimates and envelopes")
  if (weighted) axis(side=4, at=final.estimates, labels=NULL)
  
  ########################
  # final estimates box
  #########################
  
  if (VERBOSE) print("[make.bottleneck.plot] Finished final estimates and intervals box")
  
  ##############
  # seed panel
  ##############
  par(fig=c(0, seed.box.length, 0, 1), new=TRUE);
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar,0.5,top.mar,1)); # bottom, left, top, right
  
  plot(c(0,0), c(0,1), type="n", axes=FALSE, main="Seeds", 
       xlab="", ylab="", 
       cex.main=0.5);
  axis(side=4, at=c(0,1), labels=c("",""));
  box();
  
  x.location <- 0; # set by trial and error
  scaling.factor.size.of.circle <- 0.25; # set by trial and error
  scaling.factor.location.of.text <- 100; # set by trial and error
  c <- 0.022;
  # zeros
  seeds.zero <- sum(trait.vector[as.logical(is.seed.vector)]==0, na.rm=TRUE);
  radius.zero <- (seeds.zero/pi)^0.5; 
  if (seeds.zero > 0) symbols(x=x.location, y=0, circles=scaling.factor.size.of.circle * radius.zero, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=0 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.zero, labels=seeds.zero); # plot number of seeds (text)
  # ones
  seeds.one <- sum(trait.vector[as.logical(is.seed.vector)]==1, na.rm=TRUE);
  radius.one <- (seeds.one/pi)^0.5; 
  if (seeds.one > 0) symbols(x=x.location, y=1, circles=scaling.factor.size.of.circle * radius.one, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=1 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.one, labels=seeds.one); # plot number of seeds (text)
  # missing
  seeds.missing.data <- sum(is.na(trait.vector[as.logical(is.seed.vector)]));
  radius.missing.data <- (seeds.missing.data/pi)^0.5; 
  if (seeds.missing.data > 0) {
    symbols(x=x.location, y=0.5, circles=scaling.factor.size.of.circle * radius.missing.data, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
    text(x=x.location, y=0.5 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels=seeds.missing.data); # plot number of seeds (text)
    text(x=x.location, y=0.5 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels="?");
  }
  # CHECK
  if (seeds.zero + seeds.one + seeds.missing.data != num.seeds) { 
    print(paste("seeds.zero:", seeds.zero))
    print(paste("seeds.one:", seeds.one))
    print(paste("seeds.missing.data:", seeds.missing.data))
    print(paste("num.seeds:", num.seeds))
    stop("ERROR: problem with area plots of seeds")
  }
  rm(x.location, scaling.factor.size.of.circle, scaling.factor.location.of.text, c); # clean up
  
  dev.off()
  if (VERBOSE) print(paste("[make.bottleneck.plot] Finished creating:", filename))
  
}


make.bottleneck.plot.older <- function(results.this.trait,
                                 trait.vector,
                                 is.seed.vector, y.label.str,
                                 overlaps.all.vec,
                                 filename,
                                 VERBOSE=FALSE) {
  
  # NOTES: These plots could look funny if there is a seed that is missing data on trait.vector
  # running.estimates and envelopes are lists
  # one element for each seed
  # trait.vector and is.seed.vector are for the seed panel of the plot
  
  # pull out results from list that is passed in
  running.estimates <- results.this.trait$running.estimates
  envelopes <- results.this.trait$envelopes
  final.estimate <- results.this.trait$final.estimate
  offspring.this.seed.vec <- results.this.trait$offspring.this.seed.vec
  
  # check input
  # check to make sure the number of chains are the same
  if (sum(is.seed.vector) != length(envelopes)) {
    print(paste("sum(is.seed.vector):", sum(is.seed.vector)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: envelopes does not match the number of seeds")
  }
  if (!(is.list(running.estimates))) stop("ERROR: running.estimates is not a list")
  if (!(is.list(envelopes))) stop("ERROR: envelopes is not a list")
  if (length(trait.vector) != length(is.seed.vector)) stop("ERROR: trait.vector and is.seed.vector do not match")
  if (length(offspring.this.seed.vec) != length(envelopes)) {
    print(paste("length(offspring.this.seed.vec):", length(offspring.this.seed.vec)))
    print(paste("length(envelopes):", length(envelopes)))
    stop("ERROR: length(offspring.this.seed.vec) != length(envelopes)")
  }
  # finished checking input
  
  if (VERBOSE) print(paste("Beginning creating:", filename))
  pdf(filename)
  
  axis.mag <- 1;
  label.mag <- 1;
  main.mag <- 1;
  
  num.seeds <- sum(is.seed.vector)
  num.most.offspring <- max(offspring.this.seed.vec)
  
  bottom.mar <- 3.7
  top.mar <- 1.5
  seed.box.length <- 0.08
  final.estimates.box.length <- 0.02 * num.seeds # set by trial and error
  
  #############
  # main box
  ##############
  if (VERBOSE) print("[make.bottleneck.plot] Beginning drawing estimates and envelopes")
  estimates.line.color <- "black"
  ci.shading.color <- "grey84"
  background.color <- "grey90"
  
  par(fig=c(seed.box.length, 1 - final.estimates.box.length, 0, 1));
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar, 1.5, top.mar, 0));  # bottom, left, top, right
  plot(c(0, num.most.offspring), c(0, 1), type="n", 
       xlab="", ylab=y.label.str, 
       main=filename, axes=TRUE, 
       cex.axis=axis.mag, cex.lab=label.mag, cex.main=main.mag, yaxt="n")
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), las=2, hadj=0.7)
  mtext(text="Cumulative sample size", side=1, line=1.9)
  polygon(c(-999, -999, 999, 999), c(-999, 999, 999, -999), col=background.color)
  box()
  
  # plot complete RDS estimate
  lines(c(0 - 999, num.most.offspring + 999), rep(final.estimate, 2),
        lty="solid", col="white", lwd=2)
  
  # plot envelopes first so that estimate go over top of them
  #   for (seed in 1:num.seeds) {
  #     if (VERBOSE) print(paste("[make.bottleneck.plot] plotting envelopes for seed:", seed))
  #     if (length(running.estimates[[seed]])==1) {
  #       # only one point so we use lines
  #       lines(x=c(1, 1), y=envelopes[[seed]][, 1], col=ci.shading.color)
  #     } else {
  #       # more than one point so we do envelopes
  #       # plot confidence "envelopes"
  #       estimates.temp <- running.estimates[[seed]];
  #       upper.ci.temp <- envelopes[[seed]]["upper", ];
  #       lower.ci.temp <- envelopes[[seed]]["lower", ];
  #       upper.ci.temp[(upper.ci.temp > 1)] <- 1; #top code
  #       lower.ci.temp[(lower.ci.temp < 0)] <- 0; #bottom code
  #       
  #       polygon(x=c(1:length(estimates.temp), length(estimates.temp):1),
  #               y=c(upper.ci.temp, rev(estimates.temp)), 
  #               col=ci.shading.color, border=NA) # upper
  #       polygon(x=c(1:length(estimates.temp), length(estimates.temp):1),
  #               y=c(lower.ci.temp, rev(estimates.temp)),
  #               col=ci.shading.color, border=NA) # lower
  #       rm(estimates.temp, upper.ci.temp, lower.ci.temp)
  #     }
  #   }
  if (VERBOSE) print("[make.bottleneck.plot] finished plotting envelopes")
  
  # now plot estimates over envelopes
  for (seed in 1:num.seeds) {
    if (VERBOSE) print(paste("[make.bottleneck.plot] plotting estimates for seed:", seed))
    if (length(running.estimates[[seed]])==1) {
      # only one estimate so we use point
      points(x=1, y=running.estimates[[seed]], pch=".")
    } else {
      # more than one point so we can use line
      lines(running.estimates[[seed]], col=estimates.line.color, lwd=1)
    }
  }
  if (VERBOSE) print("[make.bottleneck.plot] Finished drawing estimates and envelopes")
  
  ########################
  # final estimates box
  #########################
  if (VERBOSE) print("[make.bottleneck.plot] Beginning final estimates and intervals box")
  par(fig=c(1 - final.estimates.box.length + 0.01, 0.99, 0, 1), new=TRUE);
  par(omd=c(0, 1, 0, 1)); 
  par(mar=c(bottom.mar, 0, top.mar, 0)); # bottom, left, top, right
  
  plot(c(0,0), c(0,1), type="n", axes=FALSE, 
       main="", xlab="", ylab="",
       xlim=c(1, num.seeds), ylim=c(0, 1))
  
  final.estimates <- rep(NA, num.seeds)
  for (seed in 1:num.seeds) {
    if (length(running.estimates[[seed]]) > 0) {
      final.estimates[seed] <- running.estimates[[seed]][length(running.estimates[[seed]])]
    }
  }  
  
  counter <- 0
  for (seed in order(final.estimates)) {
    counter <- counter + 1
    # if overlaps.all.vec is NA or 1 then black
    # else red
    if (!is.na(final.estimates[seed])) {
      if (is.na(overlaps.all.vec[seed])) {
        col.str <- "black"
      } else {
        if (overlaps.all.vec[seed]==1) {
          col.str <- "black"
        } else {
          col.str <- "red"
        }
      }
      points(counter, final.estimates[seed], cex=0.75, pch=1, col=col.str)
      lines(x=rep(counter, 2),
            y=envelopes[[seed]][c("lower", "upper"), length(envelopes[[seed]]["upper", ])],
            lwd=1, lty=1, col=col.str)
      rm(col.str)
    }
  }
  axis(side=1, at=1:num.seeds, labels=order(final.estimates), 
       lwd=0, lwd.ticks=1, cex.axis=0.3)
  mtext(text="Seed", side=1, line=1.9, cex=0.3)
  axis(side=3, at=1:num.seeds, labels=offspring.this.seed.vec[order(final.estimates)], 
       lwd=0, lwd.ticks=1, cex.axis=0.3)
  mtext(text="Sample size", side=3, line=1.9, cex=0.3)
  
  if (VERBOSE) print("[make.bottleneck.plot] Finished final estimates and intervals box")
  
  ##############
  # seed panel
  ##############
  par(fig=c(0, seed.box.length, 0, 1), new=TRUE);
  par(omd=c(0,1,0,1)); 
  par(mar=c(bottom.mar,0.5,top.mar,1)); # bottom, left, top, right
  
  plot(c(0,0), c(0,1), type="n", axes=FALSE, main="Seeds", 
       xlab="", ylab="", 
       cex.main=0.5);
  axis(side=4, at=c(0,1), labels=c("",""));
  box();
  
  x.location <- 0; # set by trial and error
  scaling.factor.size.of.circle <- 0.25; # set by trial and error
  scaling.factor.location.of.text <- 100; # set by trial and error
  c <- 0.022;
  # zeros
  seeds.zero <- sum(trait.vector[as.logical(is.seed.vector)]==0, na.rm=TRUE);
  radius.zero <- (seeds.zero/pi)^0.5; 
  if (seeds.zero > 0) symbols(x=x.location, y=0, circles=scaling.factor.size.of.circle * radius.zero, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=0 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.zero, labels=seeds.zero); # plot number of seeds (text)
  # ones
  seeds.one <- sum(trait.vector[as.logical(is.seed.vector)]==1, na.rm=TRUE);
  radius.one <- (seeds.one/pi)^0.5; 
  if (seeds.one > 0) symbols(x=x.location, y=1, circles=scaling.factor.size.of.circle * radius.one, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
  text(x=x.location, y=1 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.one, labels=seeds.one); # plot number of seeds (text)
  # missing
  seeds.missing.data <- sum(is.na(trait.vector[as.logical(is.seed.vector)]));
  radius.missing.data <- (seeds.missing.data/pi)^0.5; 
  if (seeds.missing.data > 0) {
    symbols(x=x.location, y=0.5, circles=scaling.factor.size.of.circle * radius.missing.data, fg="black", bg="black", inches=FALSE, add=TRUE); # plot seeds circle
    text(x=x.location, y=0.5 - c - (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels=seeds.missing.data); # plot number of seeds (text)
    text(x=x.location, y=0.5 + c + (scaling.factor.size.of.circle/scaling.factor.location.of.text)*seeds.missing.data, labels="?");
  }
  # CHECK
  if (seeds.zero + seeds.one + seeds.missing.data != num.seeds) { 
    print(paste("seeds.zero:", seeds.zero))
    print(paste("seeds.one:", seeds.one))
    print(paste("seeds.missing.data:", seeds.missing.data))
    print(paste("num.seeds:", num.seeds))
    stop("ERROR: problem with area plots of seeds")
  }
  rm(x.location, scaling.factor.size.of.circle, scaling.factor.location.of.text, c); # clean up
  
  dev.off()
  if (VERBOSE) print(paste("[make.bottleneck.plot] Finished creating:", filename))
  
}


run.bottleneck.test.old <- function(results, lower.cutoff) {
  
  # results should be a list that has at least
  # running.estimates 
  # envelopes
  # final.estimate
  # lower.cutoff: only consider chains >= lower.cutoff
  
  if (!(is.list(results))) {
    stop("ERROR: results in not a list")
  }
  if (lower.cutoff < 0) stop("ERROR: lower cutoff less than 0")
  
  chain.length.vec <- sapply(running.estimates, length)
  seed.longest.chain <- which(chain.length.vec==max(chain.length.vec))
  longest.chain <- max(chain.length.vec)
  
  if (length(seed.longest.chain)==1) {
    # there is one longest chain
    not.longest.chain.vec <- 1:num.seeds;
    not.longest.chain.vec <- not.longest.chain.vec[-seed.longest.chain];
    seeds.to.loop.over <- not.longest.chain.vec
  } else {
    # there are more than 1 chain tied for longest
    # therefore we loop over all chains
    seeds.to.loop.over <- 1:num.seeds;
  }
  
  # create matrix including NAs for envelopes (this helps because list is different lengths
  upper.ci.array <- array(NA, dim=c(num.seeds, longest.chain));
  lower.ci.array <- array(NA, dim=c(num.seeds, longest.chain));
  for (seed in 1:num.seeds) {
    upper.ci.array[seed, 1:length(envelopes[[seed]]["upper", ])] <- envelopes[[seed]];
    lower.ci.array[seed, 1:length(envelopes[[seed]]["lower", ])] <- envelopes[[seed]];
  }
  print("upper.ci.array");
  print(upper.ci.array);
  print("lower.ci.array");
  print(lower.ci.array);
  
  LOWER.TOO.HIGH.vec <- rep(NA, num.seeds);
  UPPER.TOO.LOW.vec <- rep(NA, num.seeds);
  lower.cutoff <- 3;
  for (seed in seeds.to.loop.over) {
    print(paste("seed:", seed))
    if (length(running.estimates[[seed]]) >= lower.cutoff) {
      #LOWER.TOO.HIGH.vec[seed] <- lower.ci.array[seed, length(running.estimates[[seed]])] > max(upper.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE);
      #UPPER.TOO.LOW.vec[seed] <- upper.ci.array[seed, length(running.estimates[[seed]])] < min(lower.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE);
      
      print(paste("lower.ci.array[seed, length(running.estimates[[seed]])]:", lower.ci.array[seed, length(running.estimates[[seed]])]));
      print(paste("max(upper.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE):", max(upper.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE)));
      LOWER.TOO.HIGH.vec[seed] <- lower.ci.array[seed, length(running.estimates[[seed]])] > max(upper.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE);
      print(paste("upper.ci.array[seed, length(running.estimates[[seed]])]:", upper.ci.array[seed, length(running.estimates[[seed]])]))
      print(paste("min(lower.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE):", min(lower.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE)));
      UPPER.TOO.LOW.vec[seed] <- upper.ci.array[seed, length(running.estimates[[seed]])] < min(lower.ci.array[-seed, length(running.estimates[[seed]])], na.rm=TRUE);
      print(paste("LOWER.TOO.HIGH:", LOWER.TOO.HIGH.vec[seed]));
      print(paste("UPPER.TOO.LOW:", UPPER.TOO.LOW.vec[seed]));  
    } else {
      # chain is too short so we say no problem
      LOWER.TOO.HIGH.vec[seed] <- FALSE;
      UPPER.TOO.LOW.vec[seed] <- FALSE;
    }
  }
  FAIL.PARALLEL.CHAIN.TEST <- any(c(LOWER.TOO.HIGH.vec, UPPER.TOO.LOW.vec), na.rm=TRUE);
  print(paste("fail parallel chain test:", FAIL.PARALLEL.CHAIN.TEST ));
  
  
}

#! /usr/bin/env Rscript

###################################################################
## se-rwanda-bias-illustration.r
##
## use the blending results to show the impact that biases
## have on the best estimates
##

library(ggplot2)
library(scales)
library(plyr)
library(gridExtra)
library(reshape2)
library(stringr)
library(car)
library(functional)
library(dplyr)

source(file.path("se-rwanda-directories.r"))
set.dirs()

load(file.path(out.dir, "rw-bootstrap-resamples.RData"))

## set the plot theme
source(file.path(theme.dir, "se-rwanda-theme.r"))
theme_set(theme_se())

# dataframe with known popn values
kp.prop.dat <- data.frame(known.size=kp.prop,
                          name=names(kp.prop))

get.blending.estimate <- function(var.A, 
                                  var.B, 
                                  adj.factor.A, 
                                  adj.factor.B,
                                  est.A, est.B, 
                                  cov.AB=0) {

    if (cov.AB != 0) {
        stop("covariance between arms not implemented...\n")
    }

    # if EV of estimator is mu.A and truth is Y, then 
    # the adjustment factor is the alpha that satisfies
    #   Y = mu.A * alpha

    # to construct unbiased estimates, multiply by
    # the adjustment factor
    unbiased.est.A <- est.A * adj.factor.A
    unbiased.est.B <- est.B * adj.factor.B

    # the variance of the unbiased estimator will be
    # var(unbiased) = alpha^2 * var(Y.hat)
    var.unbiased.A <- (adj.factor.A^2)*var.A
    var.unbiased.B <- (adj.factor.B^2)*var.B

    denom <- var.unbiased.A + var.unbiased.B
    w.A <- var.unbiased.B / denom
    w.B <- var.unbiased.A / denom

    blended.est <- w.A*unbiased.est.A + w.B*unbiased.est.B

    return(c("est.A"=est.A,
             "est.B"=est.B,
             "unbiased.est.A"=unbiased.est.A,
             "unbiased.est.B"=unbiased.est.B,
             "w.A"=w.A, 
             "w.B"=w.B,
             "adj.factor.A"=adj.factor.A,
             "adj.factor.B"=adj.factor.B,
             "var.A"=var.A,
             "var.B"=var.B,
             "unbiased.var.A"=var.A,
             "unbiased.var.B"=var.B,
             "blended.est"=blended.est))
}

########################

## get estimates for sampling variance for each qoi
ests.wide <- melt(ests.all,
                  id.vars=c("tie.defn", "boot.num", "qoi"),
                  measure.vars=c("estimate"))
ests.wide <- dcast(ests.wide, qoi + boot.num ~ tie.defn)

qoi.var.est <- ddply(ests.wide,
                     .(qoi),
                     summarise,
                     est.acq = mean(Acquaintance),
                     est.meal = mean(Meal),
                     var.hat.acq = var(Acquaintance),
                     var.hat.meal = var(Meal))

qoi.vals <- paste(qoi.var.est$qoi)
names(qoi.vals) <- qoi.vals

# this list contains functions, one for each of the qoi
est.fns <- dlply(qoi.var.est,
                 .(qoi),
                 function(x) {
                     this.fn <- Curry(get.blending.estimate,
                                      var.A=x$var.hat.meal,
                                      var.B=x$var.hat.acq,
                                      est.A=x$est.meal,
                                      est.B=x$est.acq)
                     return(this.fn)
                 })

adj.factor.range <- seq(from=0.5, to=2, by=.01)

adj.factor.vals <- expand.grid(meal=adj.factor.range,
                               acq=adj.factor.range)

## put results in terms of absolute numbers instead of proportions
rw.popn <- 10718378
N <- rw.popn

# compute the blended estimate for each qoi and for all of the
# combinations of relative bias
#
# NOTE: this takes a bit of time to run

sim.ests <- llply(qoi.vals,
                  function(this.qoi) {
                    this.fn <- est.fns[[this.qoi]]
                    ests <- adply(adj.factor.vals,
                                  1,
                                  function(adj.vals) {
                                      this.est <- this.fn(adj.factor.A=adj.vals$meal,
                                                          adj.factor.B=adj.vals$acq)
                                      est.vals <- as.data.frame(do.call("cbind",
                                                                         as.list(this.est)))
                                      return(est.vals)
                                  })
                    # rename to meal and basic instead of A and B
                    ests <- plyr::rename(ests,
                                   c("w.A"="w.meal",
                                     "w.B"="w.acq",
                                     "est.A"="est.meal",
                                     "est.B"="est.acq",
                                     "unbiased.est.A"="unbiased.est.meal",
                                     "unbiased.est.B"="unbiased.est.acq",
                                     "blended.est"="est.blended",
                                     "adj.factor.A"="adj.factor.meal",
                                     "adj.factor.B"="adj.factor.acq",
                                     "unbiased.var.A"="unbiased.var.meal",
                                     "unbiased.var.B"="unbiased.var.acq",
                                     "var.A"="var.meal",
                                     "var.B"="var.acq"))
                  },
                  .progress="text")

# specific example we use in main text
alpha.meal.ex <- 1
alpha.acq.ex <- 1.5
fsw.ex <- sim.ests[["sex.workers"]] %>% 
          filter(adj.factor.meal==alpha.meal.ex, 
                 adj.factor.acq==alpha.acq.ex) %>%
          select(est.blended) %>% 
          mutate(est.blended=est.blended*rw.popn) %>%
          as.numeric()

sink(file.path(out.dir, "maintext-blending-example.log"))
cat("If alpha.meal = ", alpha.meal.ex, " and alpha.acq = ", alpha.acq.ex, "\n")
cat("then the blended estimate for female sex workers is ", fsw.ex, "\n")
sink(NULL)


### This is Figure 5
panel.titles <- c('sex.workers'='A',
                  'clients'='B',
                  'msm'='C',
                  'idu'='D')

low.col <- "white"
high.col <- "black"

biasill.plots <- llply(qoi.vals,
                       function(toplot.qoi) {
                         this.dat <- sim.ests[[toplot.qoi]]
                         this.mid <- subset(this.dat, 
                                            adj.factor.acq==1 & 
                                            adj.factor.meal==1)$est.blended

                         res.p <- ggplot(this.dat) +
                                  geom_raster(aes(x=adj.factor.meal, 
                                                  y=adj.factor.acq, 
                                                  fill=est.blended*N,
                                                  color=est.blended*N)) +
                                  geom_point(aes(x=1, y=1), pch=1) +
                                  scale_color_gradient(low=low.col, high=high.col,
                                                       name="", labels=comma) +
                                  scale_fill_gradient(low=low.col, high=high.col,
                                                      name="", labels=comma) +
                                  #ggtitle(panel.titles[toplot.qoi]) +
                                  xlab(expression(atop("",
                                                       paste("Adjustment Factor ", 
                                                        alpha, ", meal")))) +
                                  ylab(expression(atop(paste("Adjustment Factor ", 
                                                        alpha, ", acquaintance")),
                                                  ""))+
                                  coord_equal() +
                                  theme_classic() +
                                  theme(axis.ticks=element_line(size=1),
                                        axis.line=element_line(size=1, lineend='square'),
                                        text=element_text(size=10),
                                        axis.text=element_text(size=10),
                                        legend.text=element_text(size=10))
                                  

                                  ggsave(plot=res.p,
                                         filename=file.path(out.dir,
                                                            paste0("figure_5",
                                                                   panel.titles[toplot.qoi],
                                                                   ".pdf")),
                                         dpi=300,
                                         width=4, height=4)

                                  ggsave(plot=res.p,
                                         filename=file.path(out.dir,
                                                            paste0("figure_5",
                                                                   panel.titles[toplot.qoi],
                                                                   ".eps")),
                                         width=4, height=4)
                                    
                         return(res.p)
                       })

plotorder <- c('sex.workers', 'clients', 'msm', 'idu')

fig5 <- do.call(arrangeGrob,
                c(biasill.plots[plotorder],
                  list(ncol=2)))

ggsave(plot=fig5,
     filename=file.path(out.dir,
                        paste0("figure_5.pdf")),
     dpi=300,
     width=9, height=9)

ggsave(plot=fig5,
     filename=file.path(out.dir,
                        paste0("figure_5.eps")),
     width=9, height=9)


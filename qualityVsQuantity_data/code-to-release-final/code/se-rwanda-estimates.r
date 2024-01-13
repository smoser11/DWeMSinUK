#! /usr/bin/env Rscript

###################################################################
## se-rwanda-estimates.r
##
## estimates by tie defn and blended estimates from Rwanda study
##

library(ggplot2)
library(scales)
library(plyr)
library(gridExtra)
library(reshape2)
library(stringr)
library(car)
library(dplyr)
library(tidyr)
library(stargazer)

source(file.path("se-rwanda-directories.r"))
set.dirs()

load(file.path(out.dir, "rw-bootstrap-resamples.RData"))

## set the plot theme
source(file.path(theme.dir, "se-rwanda-theme.r"))
theme_set(theme_se())

# dataframe with known popn values
kp.prop.dat <- data.frame(known.size=kp.prop,
                          name=names(kp.prop))

##############
## linear blending estimates

## to compute the weights for the hidden populations, we need to get
## estimates of the variance for each bootstrap rep

## Bernard's 2011 projection of Rw's popn
rwanda.popn <- 10718378

ests.forvar <- ests.all
ests.forvar <- mutate(ests.forvar,
                      estimate.tot = estimate * rwanda.popn)
agg.var.ests <- ddply(ests.forvar,
                      .(tie.defn, qoi),
                      summarise,
                      ## this is the variance of the bootstrap estimates
                      var.hat = var(estimate.tot),
                      ## this is the mean of the bootstrap estimates
                      tot.hat = mean(estimate.tot))
agg.var.ests <- plyr::rename(agg.var.ests, c("qoi"="name"))

#########
## make wide version to save for appendix table
agg.var.wide <- agg.var.ests %>% 
                gather(qty, value, var.hat:tot.hat) %>%
                unite("tie_qty", tie.defn, qty) %>%
                spread(tie_qty, value)

## make sure rows are in the right order
qoi.order <- data.frame(name=c("sex.workers", "msm", "idu", "clients"),
                        label=c("Female sex workers", "Men who have sex with men",
                                "People who inject drugs", "Male clients of sex workers"),
                        order=c(2,3,4,1))

agg.var.wide <- join(qoi.order, agg.var.wide, by="name")

agg.var.wide <- agg.var.wide %>%
                arrange(order) %>%
                select(label,
                       Meal_tot.hat, Meal_var.hat,
                       Acquaintance_tot.hat, Acquaintance_var.hat) 

agg.tab <- stargazer(agg.var.wide,
                     summary=FALSE,
                     digits=0,
                     style="demography",
                     rownames=FALSE,
                     float=FALSE)

cat(agg.tab[8:11],
    file=file.path(out.dir, "agg-est-var-table.tex"),
    sep="\n")

##### BEGIN worked example from appendix

adj.factors <- data.frame(phi = c(1.3,1.3),
                          delta = c(.9, .5),
                          tau = c(.8, .4),
                          eta = c(.99, .95))
rownames(adj.factors) <- c("Meal", "Acquaintance")

adj.alpha <- adj.factors$eta / (adj.factors$delta*adj.factors$tau*adj.factors$phi)
names(adj.alpha) <- c("Meal", "Acquaintance")

var.fsw.meal <- agg.var.ests %>% filter(tie.defn=='Meal',name=='sex.workers') %>%
               select(var.hat) %>% as.numeric()
var.fsw.acq <- agg.var.ests %>% filter(tie.defn=='Acquaintance',name=='sex.workers') %>%
               select(var.hat) %>% as.numeric()
lbw.fsw <- var.fsw.acq / (var.fsw.acq + var.fsw.meal)
lbw.fsw <- c(lbw.fsw, 1-lbw.fsw)
names(lbw.fsw) <- c("Meal", "Acquaintance")

est.fsw.meal <- agg.var.ests %>% filter(tie.defn=='Meal',name=='sex.workers') %>%
               select(tot.hat) %>% as.numeric()
est.fsw.acq <- agg.var.ests %>% filter(tie.defn=='Acquaintance',name=='sex.workers') %>%
               select(tot.hat) %>% as.numeric()
est.fsw <- c(est.fsw.meal, est.fsw.acq)
names(est.fsw) <- c("Meal", "Acquaintance")

adj.fsw <- adj.alpha*est.fsw

blended.fsw <- sum(lbw.fsw*adj.fsw)

## save these to a file so we can easily get them into the appendix
## where they show up in Table E.1
sink(file.path(out.dir, "som_table_e1.log"))
cat("\nvariance and size estimates: \n")
print(agg.var.ests, digits=3)
cat("\nassumed adjustment factors:\n")
print(adj.factors, digits=3)
cat("\nalphas:\n")
print(adj.alpha)
cat("\nadjusted estimates (alpha * est):\n")
print(adj.fsw)
cat("\nlinear blending weight for fsw:\n")
print(lbw.fsw)
cat("\nblended estimated total:\n")
print(blended.fsw)
sink(NULL)

##### END worked example from appendix

## reshape so that each row has, for a particular
## bootstrap rep / qoi, the estimate from the acquaintance and
## the meal definition. this makes computing the blended estimate
## a within-row operation

## NB: ests.all has the bootstrap results from se-rwanda-bootstrap
## (the variance estimates in ests.all come from the inner bootstrap reps)
ests.all <- melt(ests.all,
                 id.vars=c("tie.defn", "qoi", "boot.num"))
ests.all <- dcast(ests.all,
                  qoi + boot.num ~ variable + tie.defn)

## compute the blended estimates

ests.all <- mutate(ests.all,
                   w.ulb.meal = var_Acquaintance / (var_Acquaintance + var_Meal),
                   w.ulb.acq = var_Meal / (var_Acquaintance + var_Meal),
                   blended.ulb = w.ulb.meal*estimate_Meal + w.ulb.acq*estimate_Acquaintance)

blended.ests.qoi <- ests.all
blended.ests.qoi <- plyr::rename(blended.ests.qoi,
                           c("qoi"="name"))

###########
## blended estimates for the hidden populations
tmp <- plyr::rename(blended.ests.qoi,
              c("estimate_Acquaintance"="acquaintance",
                "estimate_Meal"="meal"))
qoi.ests.summ <- melt(tmp,
                      id.vars=c("name", "boot.num"),
                      measure.vars=c("acquaintance", "meal", "blended.ulb"))
qoi.ests.summ <- ddply(qoi.ests.summ,
                       .(name, variable),
                       summarise,
                       estimate=mean(value),
                       ci.low=quantile(value, .025),
                       ci.high=quantile(value, .975))

## order factor levels by blended mean estimate
qoi.ests.summ$name <- reorder(qoi.ests.summ$name, -qoi.ests.summ$estimate, FUN=mean)

qoi.ests.summ$variable <- factor(qoi.ests.summ$variable,
                                 levels=c('acquaintance', 'blended.ulb', 'meal'))

fig.dat <- qoi.ests.summ
fig.dat$variable <- recode(fig.dat$variable,
                            "'blended.ulb'='blended'")

## make estimates in terms of totals, not proportions
rw.popn <- 10718378
N <- rw.popn

fig.dat <- transform(fig.dat,
                      estimate = N*estimate,
                      ci.low = N*ci.low,
                      ci.high = N*ci.high)

## manually add estimates from other sources (see the survey report, table pg 57)
other.ests <- data.frame(name=c('sex.workers', 'sex.workers', 'sex.workers', 'clients'),
                         variable=c('cap.recap', 'mapping', 'enumeration', 'rdhs2005.direct'),
                         label=c(NA, ' mapping', 
                                 'enumeration &\ncapture-recapture', 'direct (RDHS 05)'),
                         estimate=c(3205, 10000, 3348, 20142))

unaids.def <- data.frame(name=c('sex.workers', 'clients', 'idu', 'msm'),
                         variable=c('unaids','unaids', 'unaids', 'unaids'),
                         label=c('UNAIDS\nbenchmark', 'UNAIDS\nbenchmark', 
                                 'UNAIDS\nbenchmark', 'UNAIDS\nbenchmark'),
                         estimate=c(60452, 281357, 67490, NA),
                         ci.low=c(10298, 267960, 26260, NA),
                         ci.high=c(110613, 294755, 108717, NA))

## see
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
## for the source of this function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

fig.dat$variable <- sapply(paste(fig.dat$variable), simpleCap)

fig.dat <- fig.dat %>% group_by(name) %>%
           mutate(order=estimate[variable=='Blended'])

#######
## relabel the hidden populations to have nicely visible names

fig.dat$name <- recode(fig.dat$name,
                       "'sex.workers'='Female Sex\\nWorkers';
                        'msm'='Men Who Have\\nSex With Men';
                        'idu'='People Who\\nInject Drugs';
                        'clients'='Male Clients\\nof Sex Workers'",
                        as.factor.result=TRUE)

other.ests$name <- recode(other.ests$name,
                       "'sex.workers'='Female Sex\\nWorkers';
                        'msm'='Men Who Have\\nSex With Men';
                        'idu'='People Who\\nInject Drugs';
                        'clients'='Male Clients\\nof Sex Workers'",
                        as.factor.result=TRUE)


other.ests$variable <- factor(paste(other.ests$variable),
                              levels=c("mapping", "enumeration", "cap.recap", "rdhs2005.direct"))
other.ests <- other.ests %>% arrange(variable)

fig.dat$name <- reorder(fig.dat$name, -fig.dat$order, ordered=TRUE)

unaids.def$name <- recode(unaids.def$name,
                          "'sex.workers'='Female Sex\\nWorkers';
                           'msm'='Men Who Have\\nSex With Men';
                           'idu'='People Who\\nInject Drugs';
                           'clients'='Male Clients\\nof Sex Workers'",
                           as.factor.result=TRUE)

unaids.def$name <- factor(paste(unaids.def$name),
                          levels=c('Male Clients\nof Sex Workers', 
                                   'Female Sex\nWorkers', 
                                   'Men Who Have\nSex With Men', 
                                   'People Who\nInject Drugs'), 
                          ordered=TRUE)

## this is the version with all of the editorial changes
## AJE wanted

scale_shape_tiedefn <- scale_shape_manual(values=c("Acquaintance"=0,
                                                   "Meal"=2,
                                                   "Blended"=5,
                                                   "mapping"=19,
                                                   "enumeration"=19,
                                                   "cap.recap"=1,
                                                   "rdhs2005.direct"=4))

scale_color_tiedefn <- scale_color_manual(values=c("Acquaintance"=NA,
                                                 "Meal"=NA,
                                                 "Blended"=NA,
                                                 "mapping"="black",
                                                 "enumeration"="darkgrey",
                                                 "cap.recap"="black",
                                                 "rdhs2005.direct"="black"))

## This is the code for Figure 4
fig  <- ggplot(fig.dat) +
        geom_pointrange(aes(x=name,
                            y=estimate,
                            ymax=ci.high,
                            ymin=ci.low,
                            shape=variable),
                        position=position_dodge(width=.3)) +
        geom_point(data=other.ests,
                   aes(x=name, 
                       y=estimate,
                       shape=variable,
                       color=variable)) + 
        scale_shape_tiedefn +
        scale_color_tiedefn +
        xlab("\nHidden Population") + 
        ylab("Estimated No.\n") +
        scale_y_continuous(labels=comma) +
        theme_classic() +
        theme(legend.position="none",
              axis.ticks=element_line(size=1),
              axis.ticks.length=unit(0.25, 'cm'),
              axis.line=element_line(size=1, lineend='square'),
              axis.ticks.x=element_blank(),
              text=element_text(size=10),
              axis.text=element_text(size=10))

ggsave(fig,
       filename=file.path(out.dir, "figure_4.pdf"),
       height=5, width=8)

ggsave(fig,
       filename=file.path(out.dir, "figure_4.eps"),
       dpi=300,
       height=5, width=8)


## This is web figure 2

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scale_color_tiedefn <- scale_color_manual(values=c("Acquaintance"=cbbPalette[4],
                                                   "Meal"=cbbPalette[6],
                                                   "Blended"=cbbPalette[8]),
                                          name="tie\ndefinition")

fig.withunaids  <- ggplot(fig.dat) +
        geom_pointrange(aes(x=name,
                            y=estimate,
                            ymax=ci.high,
                            ymin=ci.low,
                            color=variable),
                        position=position_dodge(width=.3)) +
        geom_point(aes(x=name, y=estimate), 
                   pch=1,
                   data=other.ests) +
        geom_text(aes(x=name, y=estimate, label=label),
                   data=other.ests,
                   lineheight=.6,
                   hjust=-0.1, size=2.5) +
        geom_pointrange(aes(x=name,
                            y=estimate,
                            ymax=ci.high,
                            ymin=ci.low),
                        linetype=2,
                        pch=4,
                        position=position_dodge(width=.3),
                        data=unaids.def) +
        geom_text(aes(x=name, y=estimate, label=label),
                   data=unaids.def,
                   hjust=-0.1, size=2.5) +
        scale_color_tiedefn +
        #scale_color_discrete("estimator") +
        xlab("") + ylab("Estimated Size") +
        scale_y_continuous(labels=comma)
        #scale_y_continuous(labels=percent)

ggsave(fig.withunaids,
       filename=file.path(out.dir, "web_figure_2.pdf"),
       height=7, width=10)



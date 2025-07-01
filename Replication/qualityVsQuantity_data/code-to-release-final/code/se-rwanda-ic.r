#! /usr/bin/env Rscript

###################################################################
## se-rwanda-ic.r
##
## the internal validation checks for the data from
## the Rwanda survey experiment
##

library(ggplot2)
library(scales)
library(plyr)
library(gridExtra)
library(reshape2)
library(scales)
library(car)
library(magrittr)
library(stringr)

## for convertUnit, which maps picas to inches
library(grid)

library(networkreporting)

source("se-rwanda-directories.r")
set.dirs()

## set the plot theme
source(file.path(theme.dir, "se-rwanda-theme.r"))
theme_set(theme_se())

load(paste(out.dir, "/rw-bootstrap-resamples.RData", sep=""))

## make a plot with summaries of the error, by tie defn
acc.check.long <- melt(acc.check.kp,
                       id.vars=c("tie.defn", "boot.num"))

acc.check.long <- subset(acc.check.long,
                         variable %in% c("mse", "are", "mae"))

acc.check.long <- plyr::rename(acc.check.long,
                         c("variable"="error.metric"))

## shows the differences, basic - meal, across bootstrap rsamples,
## for each error metric
errcomp.wide <- dcast(acc.check.long,
                      boot.num + error.metric ~ tie.defn)
errcomp.wide$diff <- errcomp.wide$Acquaintance - errcomp.wide$Meal

## get the p-values implied for err(meal) < err(basic)
p.vals.diff <- ddply(errcomp.wide,
                     .(error.metric),
                     summarise,
                     p.meal.lt.basic = mean(diff > 0))

facetlab.size <- 6.5
yaxis.size <- 9

tiecomp <- dlply(errcomp.wide,
                 .(error.metric),
                 function(dat) {

                     xrange <- range(max(dat$diff), min(dat$diff),
                                     min(-dat$diff), max(-dat$diff))

                     ggplot(dat) +
                         geom_histogram(aes(x=diff)) +
                         geom_vline(aes(xintercept=0), linetype="dotted") +
                         xlab("estimated difference in error\n(acquaintance - meal)") +
                         ylab("number of bootstrap resamples") +
                         xlim(xrange) +
                         theme(strip.text.x = element_text(size = facetlab.size),
                               axis.title.y= element_text(size=yaxis.size,angle=90)) +
                         ggtitle(dat$error.metric[1])
                 })

## it's a bit complex to make all of the y axis have the same range
blds <- llply(tiecomp, ggplot_build)

bld.yranges <- llply(blds, function(x) { x$panel$ranges[[1]]$y.range  })
yrange <- c(0, max(unlist(bld.yranges)))

## update the plots, forcing each one to have the same y range
## (so we can put them side-by-side), and adding textual annotations
## that clarify which direction is better for the meal definition, and
## which is better for the basic definition
tiecomp <- llply(tiecomp,
                 function(plot) {

                     qual.lab <- data.frame(x=c(0,0),
                                            y=c(yrange[2],yrange[2]),
                                            qlab=c("<- acq is better",
                                                   "meal is better ->"))

                     qual.lab.m <- qual.lab[2,]
                     qual.lab.b <- qual.lab[1,]

                     labeled.plot <- plot +
                         ylim(yrange) +
                         geom_text(aes(x=x,y=y,label=qlab), size=2, color="red",
                                   data=qual.lab.m,
                                   hjust=-.1) +
                         geom_text(aes(x=x,y=y,label=qlab), size=2, color="red",
                                   data=qual.lab.b,
                                   hjust=1.1)

                     return(labeled.plot)
                 })

pdf(file.path(out.dir, "web_figure_4.pdf"),
    height=5, width=8)
grid.arrange(tiecomp[[1]], tiecomp[[2]], tiecomp[[3]],
             ncol=3)
dev.off()

##############################################
## internal validation plots with CVs
acc.fullres <- ldply(acc.check.bootres,
                     "[[", "results")
acc.fullres <- plyr::rename(acc.fullres, c(".id"="tie.defn"))
acc.summres <- ddply(acc.fullres,
                     .(tie.defn, name),
                     function(kp.td) {
                         return(data.frame(estimate.mean=mean(kp.td$nsum.holdout.est),
                                           estimate.ci.high=quantile(kp.td$nsum.holdout.est,
                                                                     .975),
                                           estimate.ci.low=quantile(kp.td$nsum.holdout.est,
                                                                    .025),
                                           truth=kp.td$known.size[1]))
                     })


## put results in terms of absolute numbers instead of proportions
rw.popn <- 10718378
N <- rw.popn
acc.summres <- transform(acc.summres,
                         estimate.mean = estimate.mean * N,
                         estimate.ci.high = estimate.ci.high * N,
                         estimate.ci.low = estimate.ci.low * N,
                         truth = truth * N)


### 
## this is Figure 3

scale_shape_tiedefn <- scale_shape_manual(name="Internal Consistency\nEstimates, Rwanda, 2011",
                                          values=c("Acquaintance"=0,
                                                   "Meal"=2,
                                                   'true size'=1),
                                          labels=c("Acquaintance Estimate",
                                                   "Meal Estimate",
                                                   "True Size"))




##########
## version of plot w/ text labels for appendix
capitalize <- function(str) {
    str_sub(str,1,1) <- toupper(str_sub(str,1,1))
    return(str)
}

acc.summres.toplot <- acc.summres
acc.summres.toplot$name <- capitalize(acc.summres.toplot$name)
acc.summres.toplot$name <- str_replace_all(acc.summres.toplot$name, "\\.", " ")
acc.summres.toplot$name <- recode(acc.summres.toplot$name,
                                  "'Woman smoke'='Women Who Smoke';
                                   'Woman gave birth'='Women Who Gave Birth';
                                   'Man divorced'='Divorced Men';
                                   'Nurse or doctor'='Nurse or Doctor';
                                   'Male community health'='Male Community Health Worker'")

acc.summres.toplot$name <- reorder(acc.summres.toplot$name, acc.summres.toplot$truth)

#acc.summres.toplot <- acc.summres

fig3 <- ggplot(acc.summres.toplot) +
              geom_pointrange(aes(y=estimate.mean,
                                  ymin=estimate.ci.low, 
                                  ymax=estimate.ci.high,
                                  x=name,
                                  shape=tie.defn,
                                  linetype='ci'),
                             position=position_dodge(width=0.9),
                             show.legend=FALSE) +
              geom_point(aes(y=truth, x=name, 
                             shape='true size')) +
              # this is just to trick a separate scale to show up
              # explaining that the lines are CIs
              #geom_linerange(aes(ymax=truth,
              #                   ymin=truth,
              #                   x=name,
              #                   linetype='ci'),
              #               position=position_dodge(width=0.9)) +
              #theme(legend.position=c(.8,.2),
              #      axis.text.x=element_text(size=8, angle=0, vjust=0),
              #      axis.text.y=element_text(size=8)) +
              #scale_color_tiedefn +
              scale_shape_tiedefn +
              scale_linetype_manual(name="",
                                    values=c('ci'=1),
                                    labels=c("95% Confidence Interval")) +
              ylab("Group Size, no.") + xlab("") +
              scale_y_continuous(labels=comma) +
              coord_flip() +
              #theme_classic() +
              theme(axis.ticks=element_line(size=1),
                    axis.line=element_line(size=1, lineend='square'),
                    #axis.ticks.y = element_blank(),
                    text=element_text(size=8),
                    axis.text = element_text(size=8),
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    legend.position="none",
                    legend.title = element_text(size=rel(1), face='plain'),
                    legend.text = element_text(size=rel(1)))
              #guides(linetype=guide_legend(order=2),
              #       shape=guide_legend(order=1))

## wikipedia says that there picas are 1/6 of an inch
fig.height <- convertUnit(unit(45, 'picas'), 'inches', valueOnly=TRUE)
fig.width <- convertUnit(unit(42, 'picas'), 'inches', valueOnly=TRUE)

ggsave(file.path(out.dir, "figure_3.pdf"),
       plot=fig3,
       height=fig.height,
       width=fig.width,
       units='in')

ggsave(file.path(out.dir, "figure_3.eps"),
       plot=fig3,
       dpi=300,
       height=fig.height, 
       width=fig.width,
       units='in')


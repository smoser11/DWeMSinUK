#! /usr/bin/env Rscript

###################################################################
## se-rwanda-degree-estimates.r
##
## produce the plot of estimated degrees by tie defn
## for the Rwanda survey experiment
##

## if we don't explicitly load the methods package,
## Rscript doesn't work
library(methods)
library(plyr)
library(dplyr)
library(car)
library(gridExtra)
library(tidyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(reshape2)

source("se-rwanda-directories.r")
set.dirs()

load(paste(out.dir, "/rw-bootstrap-resamples.RData", sep=""))

## set the plot theme
#source(file.path(theme.dir, "se-rwanda-theme.r"))
#theme_set(theme_se())

########################
## average degree, by tie defn
avg.degrees <- ddply(deg.bootres,
                    .(boot.num, tie.defn),
                    summarise,
                    mean.degree=weighted.mean(d.hat, weight))

summ.avg.degrees <- ddply(avg.degrees,
                         .(tie.defn),
                         summarise,
                         degree.mean=mean(mean.degree),
                         degree.ci.low=quantile(mean.degree, .0275),
                         degree.ci.high=quantile(mean.degree, .975))


sink(file.path(out.dir, "avg_degree_estimates.log"))
print(summ.avg.degrees)
sink(NULL)

## THIS IS FIGURE 2 IN THE PAPER

#### degree distributions, side by side
deg.toplot <- deg.bootres

## we'll normalize the weights so that the results are estimated
## fractions of the population
deg.toplot <- deg.toplot %>% group_by(boot.num, tie.defn) %>%
              mutate(weight=weight/sum(weight))

est.deg.toplot <- estimated.degrees %>%
                  group_by(tie.defn) %>%
                  mutate(weight=weight/sum(weight))

## NB: we'll have to do each panel separately to satisfy
##     the journal's editorial requirements
main.xmax <- 1000
main.ymax <- 0.3
uber.alpha <- .1

uber.binwidth <- 25

this.tie <- "Acquaintance"
fig2.panelA <- ggplot(deg.toplot %>% filter(tie.defn == this.tie)) + 
               geom_freqpoly(aes(x=d.hat,
                                 weight=weight,
                                 group=interaction(boot.num)),
                             alpha = uber.alpha,
                             binwidth=uber.binwidth) +
               geom_freqpoly(aes(x=d.hat,
                                 weight=weight),
                             binwidth=uber.binwidth,
                             data=est.deg.toplot  %>% filter(tie.defn==this.tie)) +
            #ylim(0, main.ymax) +
            scale_x_continuous(labels=comma) +
                               #limits=c(0, main.xmax+1)) +
            coord_cartesian(xlim=c(0, main.xmax + 35),
                            ylim=c(0, main.ymax)) +
            xlab(paste0("Estimated No. of Connections")) +
            ylab("Fraction of Respondents") +
            theme(panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_blank(),
                  axis.ticks=element_line(size=1),
                  axis.line=element_line(size=1),
                  text=element_text(size=10),
                  axis.text=element_text(size=10, color='black'))

ggsave(plot=fig2.panelA,
       filename=file.path(out.dir, "figure_2A.pdf"),
       height=5, width=5)

ggsave(plot=fig2.panelA,
       filename=file.path(out.dir, "figure_2A.eps"),
       dpi=300,
       height=5, width=5)

this.tie <- "Meal"
fig2.panelB <- ggplot(deg.toplot %>% filter(tie.defn == this.tie)) + 
               geom_freqpoly(aes(x=d.hat,
                                 weight=weight,
                                 group=interaction(boot.num)),
                             alpha = uber.alpha,
                             binwidth=uber.binwidth) +
               geom_freqpoly(aes(x=d.hat,
                                 weight=weight),
                             binwidth=uber.binwidth,
                             data=est.deg.toplot  %>% filter(tie.defn==this.tie)) +
            #ylim(0, main.ymax) +
            scale_x_continuous(labels=comma) +
                               #limits=c(0, main.xmax+1)) +
            coord_cartesian(xlim=c(0, main.xmax + 35),
                            ylim=c(0, main.ymax)) +
            xlab(paste0("Estimated No. of Connections")) +
            ylab("Fraction of Respondents") +
            theme(panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_blank(),
                  axis.ticks=element_line(size=1),
                  axis.line=element_line(size=1),
                  text=element_text(size=10),
                  axis.text=element_text(size=10, color='black'))

ggsave(plot=fig2.panelB,
       filename=file.path(out.dir, "figure_2B.pdf"),
       height=5, width=5)

ggsave(plot=fig2.panelB,
       filename=file.path(out.dir, "figure_2B.eps"),
       dpi=300,
       height=5, width=5)

fig2 <- arrangeGrob(fig2.panelA, fig2.panelB, nrow=1)

ggsave(plot=fig2,
       filename=file.path(out.dir, "figure_2.pdf"),
       height=5, width=10.5)

ggsave(plot=fig2,
       filename=file.path(out.dir, "figure_2.eps"),
       dpi=300,
       height=5, width=10.5)

## this should have a smaller size so that the editor
## can print it
ggsave(plot=fig2,
       filename=file.path(out.dir, "figure_2.png"),
       height=5, width=10.5)


###########
## compare avg number reported connections by tie defn
## (Web Figure 1)
avgconn.toplot <- melt(avg.num.conn.summ,
                       id.vars=c("name", "tie.defn"))
avgconn.toplot <- dcast(avgconn.toplot,
                        name ~ tie.defn + variable)

plotrange <- range(avg.num.conn.summ$avg.num.conn.ci.low, 
                   avg.num.conn.summ$avg.num.conn.ci.high)

avgconn.plot <- ggplot(avgconn.toplot) +
                geom_abline(intercept=0, slope=1) +
                geom_point(aes(x=Acquaintance_avg.num.conn,
                               y=Meal_avg.num.conn)) +
                xlim(plotrange) + ylim(plotrange) +
                xlab("average # reported connections\n(acquaintance definition)") +
                ylab("average # reported connections\n(meal definition)") +
                theme(panel.background=element_blank()) +
                coord_equal()

ggsave(filename=file.path(out.dir, "web_figure_1.pdf"),
       height=6, width=6,
       plot=avgconn.plot)






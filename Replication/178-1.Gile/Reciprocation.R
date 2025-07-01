# Reciprocation.R
# Krista Gile


source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

re4a<-getmany("RE_P4A")
re4b<-getmany("RE_P4B")
re4c<-getmany("RE_P4C")

pctmtx<-matrix(0,3,4)
count<-1
for(i in 1:3){
	for(j in 1:4){
		pctmtx[i,j]<-100-100*round(mean(c(re4a$dat[[count]],re4b$dat[[count]],re4c$dat[[count]]),na.rm=T)-1,2)
		count<-count+1		
		}}
dimnames(pctmtx)[[1]]<-c("FSW","DU","MSM")
dimnames(pctmtx)[[2]]<-c("SD","SA","BA","HI")
library(xtable)

## Table 3
xtable(pctmtx,digits=0)

rm(list=ls())

####  Supporting Information
## Section S3

source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

tome<-getmany(c("P205","P206","P205"))
tothem<-getmany(c("P210","P211","P210"))

alltothem<-NULL
alltome<-NULL
allcats<-NULL
for(i in 1:12){
	tmp1<-tothem$dat[[i]]
	tmp2<-tome$dat[[i]]
	alltothem<-c(alltothem,tmp1)
	alltome<-c(alltome,tmp2)
	allcats<-c(allcats,rep(legtext[[i]],length(tmp1)))
	}

sum(alltothem==alltome, na.rm=T)/length(!is.na(alltothem==alltome))
table(alltothem>alltome)/sum(table(alltothem>alltome))
table(alltome>alltothem)/sum(table(alltome>alltothem))
summary(alltothem-alltome)

for(i in 1:12){
	tmp1<-tothem$dat[[i]]
	tmp2<-tome$dat[[i]]
	tmp3<-abs(tmp1-tmp2)
	print(summary(lm(tmp3~apply(cbind(tmp1,tmp2),1,max))))
		}

summary(abs(alltothem-alltome)/apply(cbind(alltothem,alltome),1,max))

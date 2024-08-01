# TimeDynamics.R
# Krista Gile
# This file begins with functions to be used later, then proceeds to general code.


####  Functions ####
barplotsplit<-function(vec,labs,breaks,dim2=NULL,legtxt=FALSE,...){#note:  less than or equal to breaks
	labvec<-unique(labs)
	breaks2<-c(breaks,max(vec,na.rm=T)+1)
	outstuff<-matrix(0,length(labvec),length(breaks2))
	for(i in 1:length(labvec)){
		lastj<--1
		for(j in 1:length(breaks2)){
			outstuff[i,j]<-sum(vec<breaks2[j] & vec>=lastj & labs==labvec[i],na.rm=T)
			lastj<-breaks2[j]
			#print(c(j,breaks[j]))
			}		
		}
		dimnames(outstuff)[[1]]<-labvec
		dimnames(outstuff)[[2]]<-dim2
  		print(outstuff)
  		barplot(t(100*outstuff/apply(outstuff,1,sum)),las=2, legend.text=legtxt,...)  	}
	
###########

source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

basen<-getmany(c("P207","P209","P208"))
daysall<-getmany(c("P208","P210","P209"))
give7<-getmany(c("P210","P211","P210"))
give1<-getmany(c("P211","P212", "P211"))

# Cleaning Data to remove logical inconsistencies
give7basen<-list()
give1basen<-list()
for(i in 1:12){
	give7basen[[i]]<-give7$dat[[i]]/basen$dat[[i]]
	give1basen[[i]]<-give1$dat[[i]]/basen$dat[[i]]	}

give7c<-list()
give1c<-list()
meangive7<-rep(0,12)
meangive1<-rep(0,12)	
	
for(i in 1:12){	
give7c[[i]]<-give7basen[[i]]
give7c[[i]][give7c[[i]]>1]<-1
give1c[[i]]<-give1basen[[i]]
give1c[[i]][give1c[[i]]>1]<-1

print(legtext[i])
print(sum(give7c[[i]]!=give7basen[[i]], na.rm=T))
print(sum(give1c[[i]]!=give1basen[[i]], na.rm=T))

meangive7[i]<-mean(give7c[[i]], na.rm=T)
meangive1[i]<-mean(give1c[[i]], na.rm=T)
}

cex<-1.8

## Figure S9
pdf(file="propgive71.pdf")
par(mar=c(7.5,5,2,2))
plot(c(1:12), 100*meangive7, ylim=c(0,105), axes=F, xlab="", ylab="Percent of Coupons",cex.axis=cex, cex.lab=cex)
points(c(1:12),100*meangive1, pch=16)
axis(1, at=1:length(legtext), labels=legtext, las=2,cex.axis=cex)
axis(side=2,cex.axis=cex)	
legend("bottom", horiz=T, pch=c(1,16), legend=c("7 Days", "1 Day"),cex=cex, bty="n")
text(1:12,(100*meangive1+3), labels=(round(100*meangive1,0)),cex=1.3)
text(1:12,(100*meangive7+3), labels=(round(100*meangive7,0)),cex=1.3)
dev.off()

mean(meangive7)
mean(meangive1)

rm(list=ls())

## Figure S10:  days to distribute coupons
source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

re1<-getmany2("RE_P1",legtext)
re3a<-getmany2("RE_P3A",legtext)
re3b<-getmany2("RE_P3B",legtext)
re3c<-getmany2("RE_P3C",legtext)

re3<-cbind(re3a,re3b,re3c)

summarymtx<-t(table(c(re3a$dat,re3b$dat,re3c$dat),rep(re3a$names,3)))

alltimes<-c(re3a$dat,re3b$dat,re3c$dat)
allnames<-rep(re3a$names,3)

summarymtx2<-summarymtx[c(12,11,9,10,4,3,1,2,8,7,5,6),]

summtx3<-matrix(0,12,4)
summtx3[,1]<-apply(summarymtx2[,1:4],1,sum)
summtx3[,2]<-summarymtx2[,5]
summtx3[,3]<-apply(summarymtx2[,6:8],1,sum)
summtx3[,4]<-apply(summarymtx2[,9:dim(summarymtx2)[2]],1,sum)


#0,1,2,3,4-7,8+
summtx4<-matrix(0,12,6)
summtx4[,1:4]<-summarymtx2[,1:4]
summtx4[,5]<-apply(summarymtx2[,5:8],1,sum)
summtx4[,6]<-apply(summarymtx2[,9:dim(summarymtx2)[2]],1,sum)
dimnames(summtx4)[[1]]<-legtext
dimnames(summtx4)[[2]]<-c("0","1","2","3","5-7",">7")

#percent within 1 day:
mean(apply(summtx4[,1:2],1,sum)/apply(summtx4,1,sum))
#[1] 0.636523
nw1<-sum(summtx4[,1:2])
ntotal<-sum(summtx4)
nw1/ntotal
#[1] 0.6430595

#percent within 7 days:
mean(apply(summtx4[,1:5],1,sum)/apply(summtx4,1,sum))
#[1] 0.9528449
nw7<-sum(summtx4[,1:5])
nw7/ntotal
#[1] 0.9541592

summtx5<-matrix(0,12,3)
summtx5[,1]<-apply(summarymtx2[,1:4],1,sum)
summtx5[,2:3]<-summtx4[,5:6]
dimnames(summtx5)[[1]]<-legtext
dimnames(summtx5)[[2]]<-c("0-3","5-7",">7")

pdf(file="daysgiven.pdf")
barplot(t(100*summtx5/apply(summtx5,1,sum)),las=2, legend.text=c("0-3","4-7",">7"), args.legend=list(x="top",horiz=T, bty="n"), ylim=c(0,108))
dev.off()

#,col=c("white","gray","black")
pdf(file="daysgivenfew.pdf")
barplot(t(100*summtx4[,1:4]/apply(summtx5,1,sum)),las=2,legend.text=c("0","1","2","3"),
args.legend=list(x="top",horiz=T, bty="n"), ylim=c(0,108))
dev.off()


rm(list=ls())

## Figure S11 - gap in recruitment dates
source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

mycup<-getmany3("NUM_CUP",legtext)
#mycup2<-getmany3("InCoupon",legtext) #(same)
#table(mycup$dat==mycup2$dat)

oc1<-getmany3("OutCoupon1",legtext)
oc2<-getmany3("OutCoupon2",legtext)
oc3<-getmany3("OutCoupon3",legtext)

parents<-rep(NA,length(mycup$dat))

for(i in 1:length(mycup$dat)){
	ptmp<-NULL
	mine<-mycup$dat[i]
	myst<-mycup$names[i]
	ptmp<-c(ptmp,which(oc1$dat==mine & oc1$names==myst))
	ptmp<-c(ptmp,which(oc2$dat==mine & oc2$names==myst))
	ptmp<-c(ptmp,which(oc3$dat==mine & oc3$names==myst))
	if(length(ptmp)==1){parents[i]<-ptmp
		}else{if(length(ptmp)>1){
				print(paste("Error on ",i))}}	
	}
	
isseed<-getmany3("IsSeed",legtext)	
#note there are two non-seeds - in GTM-SA and SW-SA - that don't have parents
sdate<-getmany3("StartDate",legtext)
dateparent<-rep(NA,length(parents))
dateparent[which(!is.na(parents))]<-sdate$dat[parents[which(!is.na(parents))]]
dategap<-sdate$dat-dateparent
seed<-getmany3("Seed",legtext)	
bads<-which(dategap<0)
dategapc<-dategap[-bads]
namesc<-sdate$names[-bads]

cex<-1.5

pdf(file="bargapdata.pdf")
par(mar=c(6.5,3.5,1.5,1.5))
barplotsplit(dategapc, namesc,breaks=c(1,2,3,4,8,15), legtxt=c("0","1","2","3","4-7","8-14","15+"), args.legend=list(x="top",horiz=T,bty="n",cex=cex, x.intersp=.4), ylim=c(0,115),cex.axis=cex, cex.names=cex)
dev.off()

sum(dategapc<=7,na.rm=T)/sum(!is.na(dategapc))
#[1] 0.7946262

propleq7<-rep(0,12)

for(i in 1:12){
	propleq7[i]<-sum(dategapc[namesc==legtext[i]]<=7,na.rm=T)/sum(!is.na(dategapc[namesc==legtext[i]]))
}	
mean(propleq7)
#[1] 0.796505	

rm(list=ls())
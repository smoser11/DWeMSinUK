# ParticipationBias.R
# Krista Gile
# This file begins with functions to be used later, then proceeds to general code.


####  Functions #####
plotrecruits3<-function(rec1, rec2, names=c("HIV -", "HIV +"), ...){
  rec1b<-100*rec1/(sum(rec1))
  rec2b<-100*rec2/(sum(rec2))
  mean1<-sum(rec1*c(0,1,2,3))/sum(rec1)
  mean2<-sum(rec2*c(0,1,2,3))/sum(rec2)
  lab1<-paste(names[1], "\n n=",sum(rec1), " mean=",round(mean1,2),sep="")
  lab2<-paste(names[2], "\n n=",sum(rec2), " mean=",round(mean2,2),sep="")
  cex<-1.8
 par(mar=(c(4,4.5,1,1)+.1))
  barplot(cbind(rev(rec1b),rev(rec2b)), legend.text=rev(c("0","1","2","3")), args.legend=list(x="top", bty="n", horiz=T,cex=cex), ylim=c(-7, 115), names.arg=c(lab1,lab2), ylab="Percent of respondents each # recruits", cex.axis=cex, cex.names=cex, cex.lab=cex)
}


source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")


####  Recruitment Effectiveness

HIVraw<-getmany("HIVPositive")
Nkids<-getmany("NChildren")

#kidshiv<-array(0, dim=c(2,4,12))
meankids<-matrix(0,2,12)

for(i in 1:12){
#	kidshiv[,,i]<-t(table(Nkids$dat[[i]],HIVraw$dat[[i]]))
	meankids[,i]<-c(mean(Nkids$dat[[i]][which(HIVraw$dat[[i]]==0)], na.rm=T),mean(Nkids$dat[[i]][which(HIVraw$dat[[i]]==1)], na.rm=T))
	}

hivkidstable<-matrix(0,12,4)

for(i in 1:12){
	mmm<-t.test(Nkids$dat[[i]]~HIVraw$dat[[i]])
	hivkidstable[i,]<-c(mmm$estimate, mmm$statistic, mmm$p.value)
	}

## Figure 9

pdf(file="receff.pdf", width=8, height=2)
#par(pin=c(8,2), mar=c(3,2,2,1))
par(mar=c(3,2,2,1))
mmm<-barplot(t(hivkidstable[,1:2]), beside=T, axes=F, ylim=c(0,1.6), args.legend=list(x=22, y=2.0, bty="n", horiz=T), legend.text=c("HIV-", "HIV+"))
axis(side=2, cex=.9)
 text(apply(mmm,2,mean), par("usr")[3], labels=legtext, srt=45, adj=c(1.1, 1.1), xpd=T, cex=.9)
 text(apply(mmm,2,mean)+.8, par("usr")[3], labels=
 paste("ratio",round(hivkidstable[,2]/hivkidstable[,1],2)), srt=45, adj=c(1.1, 1.1), xpd=T, cex=.9)
text(mmm[1,],hivkidstable[,1]+.1, labels=round(hivkidstable[,1],2), cex=.7)
text(mmm[2,],hivkidstable[,2]+.1, labels=round(hivkidstable[,2],2), cex=.7)
dev.off()


#for(i in 1:12){
#	filename<-paste(legtext[i],"recruit.pdf",sep="")
#	pdf(file=filename)
#    plotrecruits3(kidshiv[1,,i],kidshiv[2,,i])	
#	dev.off()
#}

rm(list=ls())

#####  Recruitment Bias
#Recruitment Bias (both main paper and appendix)
source(file="prepare_functions.R")
library(xtable)

#Employment Status:

is.employ<-getmanyG("P113",G=2)

num.friends<-getmanyG("P203",G=2)
num.employ<-getmanyG("P207",G=2)
prop.employA<-list()
for(i in 1:4){
	prop.employA[[i]]<-num.employ$dat[[i]]/num.friends$dat[[i]]
	}

#clean:  if more employed than friends, cap proportion at 1
prop.employ<-list()
for(i in 1:4){
	prop.employ[[i]]<-prop.employA[[i]]
	print(sum(prop.employ[[i]]>1,na.rm=T))
	prop.employ[[i]][prop.employ[[i]]>1]<-1
	}
#this involves changing 5 in Santiago and 1 in Higuey

for(i in 1:4){	
	print(sum(is.na(num.employ$dat[[i]])))
	}

for(i in 1:4){	
	print(sum(is.na(prop.employ[[i]])))
	}

for(i in 1:4){	
	print(sum(num.friends$dat[[i]]==0))
	}

which(is.na(prop.employ[[2]]))
which(num.friends$dat[[2]]==0)
num.employ$dat[[2]][c(114,226,272)]
#these are all inconsistent - if you don't know any idu, then none of them are working!!  so remove them.

badsSA<-which(num.friends$dat[[2]]==0)

num.friendsC<-num.friends
num.friendsC$dat[[2]]<-num.friends$dat[[2]][-badsSA]
num.employC<-num.employ
num.employC$dat[[2]]<-num.employ$dat[[2]][-badsSA]
prop.employC<-prop.employ
prop.employC[[2]]<-prop.employ[[2]][-badsSA]

temp<-cleanmany(prop.employ,num.friends$dat,0)
for(i in 1:4){
	print(table(temp[[i]]==prop.employC[[i]]))
	}

pnames<-c("Santo Domingo", "Santiago", "Barahona", "Higuey")

is.emp<-getmanyG("P113",G=2)
is.empC<-is.emp
is.empC[[2]]<-is.emp[[2]][-badsSA]

#are each of the coupons given to employed people?  from return survey
c1.emp<-getmanyG("RE_P4_A",G=2)
c2.emp<-getmanyG("RE_P4_B",G=2)
c3.emp<-getmanyG("RE_P4_C",G=2)
c1.empC<-c1.emp
c1.empC$dat[[2]]<-c1.emp$dat[[2]][-badsSA]
c2.empC<-c2.emp
c2.empC$dat[[2]]<-c2.emp$dat[[2]][-badsSA]
c3.empC<-c3.emp
c3.empC$dat[[2]]<-c3.emp$dat[[2]][-badsSA]

#count number of coupons reported on and number of employed among them.
num.cup<-list()
num.cup.emp<-list()
for(i in 1:4){
	allcupemp<-cbind(c1.empC$dat[[i]],c2.empC$dat[[i]],c3.empC$dat[[i]])
	repcup<-!is.na(allcupemp)
    num.cup[[i]]<-apply(repcup,1,sum)
    empcup<-allcupemp==1
    num.cup.emp[[i]]<-apply(empcup,1,sum,na.rm=T)	
}
    
emp.cup.prop<-list()
for(i in 1:4){
	emp.cup.prop[[i]]<-num.cup.emp[[i]]/num.cup[[i]]
	}

#"G" variables include only those who reported on at least one coupon
num.friendsG<-cleanmany(num.friendsC$dat,num.cup,0)
num.employG<-cleanmany(num.employC$dat,num.cup,0)
prop.employG<-cleanmany(prop.employC,num.cup,0)
num.cupG<-cleanmany(num.cup,num.cup,0)
num.cup.empG<-cleanmany(num.cup.emp,num.cup,0)
emp.cup.propG<-cleanmany(emp.cup.prop,num.cup,0)

# get self-reported employment for children...
incup<-getmanyG("InCoupon",G=2)
oc1<-getmanyG("OutCoupon1",G=2)
oc2<-getmanyG("OutCoupon2",G=2)
oc3<-getmanyG("OutCoupon3",G=2)

empch1<-list()
empch2<-list()
empch3<-list()
for(i in 1:4){
	n<-length(incup$dat[[i]])
	empch1[[i]]<-rep(NA,n)
	empch2[[i]]<-rep(NA,n)
	empch3[[i]]<-rep(NA,n)
	for(j in 1:n){
        if(any(incup$dat[[i]][j]==oc1$dat[[i]], na.rm=T)){
        	empch1[[i]][which(oc1$dat[[i]]==incup$dat[[i]][j])]<-is.emp$dat[[i]][j]-1
        	}
        if(any(incup$dat[[i]][j]==oc2$dat[[i]], na.rm=T)){
        	empch2[[i]][which(oc2$dat[[i]]==incup$dat[[i]][j])]<-is.emp$dat[[i]][j]-1
        	}
        if(any(incup$dat[[i]][j]==oc3$dat[[i]], na.rm=T)){
        #	print(empch3[[i]][which(oc3$dat[[i]]==incup$dat[[i]][j])])
        	empch3[[i]][which(oc3$dat[[i]]==incup$dat[[i]][j])]<-is.emp$dat[[i]][j]-1
        	}
	}	
}
	
empchi<-list() #  number of employed offspring
nchi<-list()   # number of offspring
propempchi<-list() # proportion of offspring that are employed
for(i in 1:4){
	allkids<-cbind(empch1[[i]],empch2[[i]],empch3[[i]])
	nchi[[i]]<-apply(!is.na(allkids),1,sum,na.rm=T)
    empchi[[i]]<-apply(allkids,1,sum,na.rm=T)
    propempchi[[i]]<-empchi[[i]]/nchi[[i]]
	}

#first step removes those with no reported friends.  second step removes those with no reported coupons
propempchiC<-cleanmany(propempchi,num.friends$dat,0)
nchiC<-cleanmany(nchi,num.friends$dat,0)
empchiC<-cleanmany(empchi,num.friends$dat,0)
propempchiG<-cleanmany(propempchiC,num.cup,0)
nchiG<-cleanmany(nchiC,num.cup,0)
empchiG<-cleanmany(empchiC,num.cup,0)

#"K" variables also drop those with no recruits
todropnokids<-list()
for(i in 1:4){
	todropnokids[[i]]<-as.numeric(is.na(propempchiG[[i]]))
	}

propempchiK<-cleanmany(propempchiG,todropnokids,1)
prop.employK<-cleanmany(prop.employG,todropnokids,1)
emp.cup.propK<-cleanmany(emp.cup.propG,todropnokids,1)
num.cupK<-cleanmany(num.cupG, todropnokids,1)
nchiK<-cleanmany(nchiG,todropnokids,1)
empchiK<-cleanmany(empchiG,todropnokids,1)
num.cup.empK<-cleanmany(num.cup.empG,todropnokids,1)
num.friendsK<-cleanmany(num.friendsG,todropnokids,1)
num.employK<-cleanmany(num.employG,todropnokids,1)

toplot<-matrix(0,3,4)
for(i in 1:4){
	toplot[1,i]<-weighted.mean(prop.employK[[i]],nchiK[[i]])
	toplot[2,i]<-weighted.mean(emp.cup.propK[[i]],nchiK[[i]])
	toplot[3,i]<-weighted.mean(propempchiK[[i]],nchiK[[i]])
	}

## Figure 10
pdf(file="refbiasplot.pdf", width=6, height=3)
par(fin=c(6,3), mar=c(3,4,2,1))
#par(mar=c(3,2,2,1))
mmm<-barplot(100*toplot, beside=T, axes=F, ylim=c(0,100), args.legend=list(x=13.5, y=105, bty="n", horiz=T), legend.text=c("Contacts", "Coupons", "Recruits"), ylab="Mean percent employed")
axis(side=2, cex=.9)
 text(apply(mmm,2,mean), par("usr")[3], labels=legtext[5:8], srt=45, adj=c(1.1, 1.1), xpd=T, cex=.9)
# text(apply(mmm,2,mean)+1, par("usr")[3], labels=legtext[5:8], adj=c(1.1, 1.1), xpd=T, cex=.9)
text(mmm[1,],100*toplot[1,]+3, labels=round(100*toplot[1,],1), cex=.9)
#, srt=45)
text(mmm[2,],100*toplot[2,]+3, labels=round(100*toplot[2,],1), cex=.9)
#, srt=45)
text(mmm[3,],100*toplot[3,]+3, labels=round(100*toplot[3,],1), cex=.9)
#, srt=45)
mtext(text=c("SD","SA","BA","HI"), side=1, line=0, at=mmm[2,])
dev.off()


###  Non-parametric Testing: (for Supporting Information)
####  Functions #####
manysamples<-function(Nvec, pvec, nvec, length){
	samps<-rep(0,length)
	for(mm in 1:length){
		samps[mm]<-drawsample(Nvec, pvec, nvec)
	}
	samps
}

drawsample<-function(Nvec, pvec, nvec){
	ninf<-0
	for(w in 1:length(Nvec)){
	  pop<-rep(c(0,1), times=round(Nvec[w]*c(1-pvec[w],pvec[w]),0))
	  #print(pop)
	  if(Nvec[w]>=nvec[w]){jjj<-sample(pop,nvec[w])
	  	}else{jjj<-sample(pop,nvec[w], replace=T)}
	  #print(jjj)
	  ninf<-ninf+sum(jjj)	
	  #print(ninf)
	}
	ninf	
}
 
cbindmany<-function(a,b,c,d){
	propbad<-rep(0,4)
	for(j in 1:4){
		nn<-length(c[[j]])
		bads<-which(d[[j]]>c[[j]])
		mmm<-rep("", nn)
		mmm[bads]<-"***"
		print(cbind(a[[j]],b[[j]],c[[j]], d[[j]],mmm))
		propbad[j]<-length(bads)/nn
		print(paste(nn,length(bads), length(bads)/nn))
	}
	propbad
}

#####  Non-parametric test testing
truecupsum<-rep(0,4)
truekidsum<-rep(0,4)
for(i in 1:4){
	truecupsum[i]<-sum(num.cup.empG[[i]])
	truekidsum[i]<-sum(empchiK[[i]])
}

set.seed(1)
N<-9999

allcupdraws<-list()
allkiddraws<-list()

for(i in 1:4){
  allcupdraws[[i]]<-manysamples(num.friendsG[[i]], prop.employG[[i]], num.cupG[[i]],N)
  allkiddraws[[i]]<-manysamples(num.cupK[[i]], emp.cup.propK[[i]], nchiK[[i]],N)
}  

allkiddrawsfull<-list()

for(i in 1:4){
  allkiddrawsfull[[i]]<-manysamples(num.friendsK[[i]], prop.employK[[i]], nchiK[[i]],N)
}  

pvalues<-matrix(0,3,4)
allbadpct<-matrix(0,3,4)

allbadpct[1,]<-cbindmany(prop.employG, num.friendsG, num.employG, num.cup.empG)
allbadpct[2,]<-cbindmany(emp.cup.propK, num.cupK, num.cup.empK, empchiK)
allbadpct[3,]<-cbindmany(prop.employK, num.friendsK, num.employK, empchiK)

## Table S3
xtable(cbind(pvalues,allbadpct),digits=3)

rm(list=ls())

###  Non-Response Rates

#  Get number distributed
ndist<-getmany("RE_P1")

#data cleaning:  ndist should be at least as big as nkids
Nkids<-getmany("NChildren")

#in these cases, set ndist=Nkids  (only 12 occurances over all sites)
ndistc<-ndist

for(i in 1:12){
	bads<-which(ndist$dat[[i]]<Nkids$dat[[i]])
	ndistc$dat[[i]][bads]<-Nkids$dat[[i]][bads]
}

#  Get number not accept
nrefuse<-getmany("RE_P8")

#  Subtract number not accept because alredy/not eligible

refuse1<-getmany("RE_P8A")
refuse2<-getmany("RE_P8B")
refuse3<-getmany("RE_P8C")
refuse4<-getmany("RE_P8D")
refuse5<-getmany("RE_P8E")

refuseOK<-list() #refusals because ineligible or already participated/coupon
for(i in 1:12){
	reftable<-cbind(refuse1$dat[[i]],refuse2$dat[[i]],refuse3$dat[[i]], refuse4$dat[[i]],refuse5$dat[[i]])
	refOK<-reftable==12 | reftable==13 | reftable==14 | reftable==15 | reftable==16	
	refuseOK[[i]]<-apply(refOK,1,sum, na.rm=T)
	}

refnotOK<-list()
for(i in 1:12){
	refnotOK[[i]]<-nrefuse$dat[[i]]-refuseOK[[i]]
}

#Coupon Refusal rate: =1- #distributed/#distribute + #refused
crrate<-rep(0,12)
for(i in 1:12){
#	goods<-which(!is.na(ndistc$dat[[i]]) & !is.na(refnotOK[[i]]))
	goods<-which(!is.na(ndistc$dat[[i]]) & !is.na(refnotOK[[i]]) & !is.na(Nkids$dat[[i]]))
	crrate[i]<-1- sum(ndistc$dat[[i]][goods])/(sum(ndistc$dat[[i]][goods])+sum(refnotOK[[i]][goods]))
	print(paste(i,length(goods)))
}


#Non-return rate: 1- #kids/#distributed
nrrate<-rep(0,12)
for(i in 1:12){
#	goods<-which(!is.na(Nkids$dat[[i]]) & !is.na(ndistc$dat[[i]]))
	goods<-which(!is.na(ndistc$dat[[i]]) & !is.na(refnotOK[[i]]) & !is.na(Nkids$dat[[i]]))
	nrrate[i]<-1- sum(Nkids$dat[[i]][goods])/sum(ndistc$dat[[i]][goods])
	print(paste(i,length(goods)))
}

#Total non-response rate:  1-#kids/(#distributed + # refused)
nok<-rep(0,12)
tnrate<-rep(0,12)
for(i in 1:12){
	goods<-which(!is.na(ndistc$dat[[i]]) & !is.na(refnotOK[[i]]) & !is.na(Nkids$dat[[i]]))
	tnrate[i]<-1- sum(Nkids$dat[[i]][goods])/(sum(ndistc$dat[[i]][goods])+sum(refnotOK[[i]][goods]))
	print(paste(i,length(goods)))
	nok[i]<-length(goods)
}


## Table 4
xtable(rbind(100*crrate,100*nrrate, 100*tnrate, nok), digits=1)

rm(list=ls())


####  Coupon Refusal
source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

library(xtable)

# 5 questions asked why each of up to 5 potential respondents refused to participate.
refuse1<-getmany("RE_P8A")
refuse2<-getmany("RE_P8B")
refuse3<-getmany("RE_P8C")
refuse4<-getmany("RE_P8D")
refuse5<-getmany("RE_P8E")

refuseall<-list()
for(i in 1:12){
	refuseall[[i]]<-c(refuse1$dat[[i]], refuse2$dat[[i]], refuse3$dat[[i]], refuse4$dat[[i]], refuse5$dat[[i]])
	}

refusetable<-matrix(0,17,12)

for(i in 1:12){
	temp<-tabulate(refuseall[[i]])
	refusetable[c(1:length(temp)),i]<-temp
	}

refusetotal<-apply(refusetable,2,sum)

refusepct<-refusetable
for(i in 1:17){
	refusepct[i,]<-refusetable[i,]/refusetotal
	}

goodmtx<-matrix(0,9,12)
goodmtx[1:2,]<-refusepct[1:2,]
goodmtx[3,]<-refusepct[3,]+refusepct[4,]
goodmtx[4,]<-refusepct[5,]
goodmtx[5,]<-refusepct[6,]+refusepct[7,]+refusepct[9,]+refusepct[10,] #8 is invalid
goodmtx[6,]<-refusepct[11,]
goodmtx[7,]<-refusepct[12,]+refusepct[13,]+refusepct[14,]
goodmtx[8,]<-refusepct[15,]+refusepct[16,]
goodmtx[9,]<-refusepct[17,]

## Table 5	
xtable(rbind(round(100*goodmtx,digits=1),refusetotal),digits=1)

	

#####  Why accept coupon and participate
source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")


whyraw<-getmany(c("P214", "P215","P214"))
HIVraw<-getmany("HIVPositive")

allwhy<-matrix(0,12,12)

for(i in 1:12){
	rawall<-whyraw$dat[[i]]
    for(j in 1:10){
    	allwhy[j,i]<-sum(rawall==j, na.rm=T)
      	}
    allwhy[11,i]<-sum(rawall==88, na.rm=T)
    howmany<-sum(allwhy[,i])
    allwhy[1:11,i]<-100*allwhy[1:11,i]/howmany
    allwhy[12,i]<-howmany
	}

goodmtx<-matrix(0,7,12)
goodmtx[1:2,]<-allwhy[1:2,]
goodmtx[3,]<-allwhy[3,]+allwhy[4,]+allwhy[5,]+allwhy[9,]
goodmtx[4,]<-allwhy[6,]
goodmtx[5,]<-allwhy[7,]
goodmtx[6,]<-allwhy[8,]+ allwhy[10,]+allwhy[11,]
goodmtx[7,]<-allwhy[12,]

library(xtable)

## Table 6
xtable(goodmtx, digits=1)


### Motivation-Outcome

forHIV<-whyraw
for(i in 1:12){
	temp<-forHIV$dat[[i]]
	temp[temp==1]<-0
	temp[temp==2]<-1
	temp[temp==9]<-1
	temp[temp>2]<-0
	forHIV$dat[[i]]<-temp
	}

for(i in 1:12){
print(chisq.test(forHIV$dat[[i]],HIVraw$dat[[i]]))
	}
#none significant at .05 level.

library(exact2x2)

for(i in 1:12){
print(fisher.exact(forHIV$dat[[i]],HIVraw$dat[[i]]))
	}

#last one significant, in wrong direction...

cis<-matrix(0,2,12)
oddrat<-rep(0,12)
pvalues<-rep(0,12)

for(i in 1:12){
	temp<-fisher.exact(forHIV$dat[[i]],HIVraw$dat[[i]], conf.level=.95)
    cis[,i]<-temp$conf.int
    pvalues[i]<-temp$p.value
    zzz<-table(forHIV$dat[[i]],HIVraw$dat[[i]])
    oddrat[i]<-(zzz[2,2]/(zzz[2,2]+zzz[2,1]))/(zzz[1,2]/(zzz[1,2]+zzz[1,1]))
	}

ymin<--2.5
ymax<-4

## Figure 11
pdf(file="oddsofHIV.pdf")	
plot(c(1:12),c(1:12), ylim=range(ymin,ymax), type='n', ylab="Odds Ratio (Log Scale)", axes=F, xlab="")
for(i in 1:12){
	points(i,log(oddrat[i]))
	lines(x=c(i,i), y=c(log(cis[,i])))
	}	
abline(h=0)
mmm<-axis(side=2, col=0, labels=F)
mmm<-log(c(1/8, 1/4, 1/2,1,2,4,8,16,32))
axis(side=2, labels=round(exp(mmm),2), at=mmm)
 text(c(1:12), par("usr")[3], labels=legtext, srt=45, adj=c(1.1, 1.1), xpd=T, cex=.9)
dev.off() 


hadhivtest<-getmany(c("P1323","P822","P1222"))
whenhivtest<-getmany(c("P1327","P826","P1227"))

for(i in 1:12){
	hadhivtest$dat[[i]][which(hadhivtest$dat[[i]]==97)]<-NA
}

fisherrestricted<-function(HIV, motivation, restriction){
	print(fisher.exact(HIV[restriction],motivation[restriction]))
}

for(i in 1:12){
	fisherrestricted(HIVraw$dat[[i]],forHIV$dat[[i]],which(hadhivtest$dat[[i]]==1))
}
# none significant
for(i in 1:12){
	fisherrestricted(HIVraw$dat[[i]],forHIV$dat[[i]],which(hadhivtest$dat[[i]]==1 | whenhivtest$dat[[i]]>2))
}
# none significant
for(i in 1:12){
	fisherrestricted(HIVraw$dat[[i]],forHIV$dat[[i]],which(hadhivtest$dat[[i]]==1 | whenhivtest$dat[[i]]>1))
}
#none significant







# WithReplacement.R
# Krista Gile
# This file begins with functions to be used later, then proceeds to general code.


##### Functions ######


propalreadyplot<-function(nknow, nalready, seeds, title){
	
   nn<-length(nknow)
   navail<-nknow-1
   navail[navail<1]<-1
   propalready<-nalready/navail
   propalready[propalready>1]<-1

   scaling.factor <- 200; # trial and error

   #rainbow.cols<-col2rgb(rainbow(n=100, start=0, end=9/12))
   #rainbow.cols<-(rainbow(n=101, start=0, end=9/12, alpha=.3))
   rainbow.cols<-rev(rainbow(n=101, start=0, end=9/12, alpha=.5))
   
   #col.str <- rgb(0,100,0,50,maxColorValue=255);
   #col.str<-c()
   #for(i in 1:length(propalready)){
   #	col.str<-c(col.str, rainbow.cols[floor])
   #	}
  col.str<-rainbow.cols[c(floor(propalready*100))+1]
  
   plot(1:nn, rep(1, nn), xaxt="n", yaxt="n", xlab="Sample Element", ylab="Seed", type="n", main=paste("Proportion of Contacts Participated, ", legtext[i]), ylim=c(0,max(seeds)+1))
      
   axis(side=1, at=c(1,seq(from=50, to=9999, by=50)));
   axis(side=2, at=1:max(seeds), las=2);

   scale.new<-8
   fuzz.new<-.01
  
   #radii.vec<-scale.new*sqrt(propalready) + fuzz.new
   radii.vec<-rep(scale.new*sqrt(.2) + fuzz.new, length(propalready))
   radii.vec[is.na(radii.vec)] <- 0; # set NA degree to 0 radii because these cases don't enter estimate
   #symbols(x=1:nn, y=seeds, circles=radii.vec, fg=col.str, bg=col.str, inches=FALSE, add=TRUE);
   #symbols(x=1:nn, y=seeds, circles=radii.vec, fg=col.str, bg=col.str, inches=FALSE, add=TRUE);

   points(x=1:nn, y=seeds, col=col.str, cex=2)

    legones<-c(0, .25, .5, .75, 1)
    legcols<-rainbow.cols[c(floor(legones*100))+1]
    legtext<-c("0%", "25%", "50%", "75%", "100%")
    legend("topleft", fill=legcols,legend=legtext)
	
	}	

propalreadygroupline<-function(nknow, nalready, lty, col){
   nn<-length(nknow)
   navail<-nknow-1
   navail[navail<1]<-1
   propalready<-nalready/navail
   propalready[propalready>1]<-1
	
	aaa<-lm(propalready~c(1:nn))
	#print(summary(aaa))
	lines(c(1:nn), aaa$fitted.values, lty=lty, col=col)
	}

 degtime2<-function(degs, title=NULL){
	flag<-0
	degs[which(degs==0)]<-1
    nn<-length(degs)
	#aaa<-lm(degs~c(1:nn))
	
	bbb<-lm(log(degs)~c(1:nn))$fitted.values
	aaa<-lm(degs~c(1:nn))$fitted.values 
    legtext<-c("Linear", "Log")

	#if(summary(aaa)$coefficients[2,1]<0){flag<-1}
	#print(flag)
	#print(aaa$coefficients)
	
 
    scaling.factor <- 200; # trial and error
    col.str <- rgb(0,100,0,50,maxColorValue=255);
    
    cutoff<-quantile(degs, .95)
    scale.new<-8
    fuzz.new<-.01
    cols<-rep(col.str, length=nn)
    cols[which(degs>cutoff)]<-rgb(100,0,0,50,maxColorValue=255)


    plot(1:nn, rep(1, nn), xaxt="n", yaxt="n", xlab="Sample Element", ylab="Degree", 
      type="n", main=paste("Degree, ", title), ylim=c(0,cutoff+1))  
    axis(side=1, at=c(1,seq(from=50, to=9999, by=50)));
    axis(side=2, at=c(0,seq(from=10, to=9999, by=10)));
    #axis(side=2, at=1:max(tmp2), las=2);

    radii.vec <- sqrt( scaling.factor / (pi*rep(10, times=nn)));  
    radii.vec[is.na(radii.vec)] <- 0; # set NA degree to 0 radii because these cases don't enter estimate
    tmp<-degs
    tmp[which(tmp>cutoff)]<-cutoff+1
    symbols(x=1:nn, y=tmp, circles=radii.vec, fg=col.str, bg=cols, inches=FALSE, add=TRUE);
	
	#if(flag==1){lines(c(1:nn),aaa$fitted.values,type='l',col=2)
	#	}else{
	#		lines(c(1:nn),aaa$fitted.values,type='l',col=3)}
	lines(c(1:nn), aaa, col=2)
	lines(c(1:nn), exp(bbb), col=3)
	
	legend("topleft", fill=c(2,3), legend=legtext)		
		 	}

##### End Functions ######


### FAILED ATTMEMPTS  ###

source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

re7<-getmany(rep("RE_P7",3))

#99 here is a missing data value

for(i in 1:12){
	re7$dat[[i]][re7$dat[[i]]==99]<-NA
	}
	
combinefail<-matrix(0,3,12)

for(j in 1:12){
	nfail<-re7$dat[[j]]
	collapse<-rep(0,3)
	breaks<-c(0,1,4,1000000)
	for(i in 1:3){
		collapse[i]<-sum((nfail>=breaks[i] & nfail<breaks[i+1]), na.rm=T)
		}
	combinefail[,j]<-100*collapse/sum(collapse)	
}

combinefail

cex<-1.3

## Figure 3
pdf(file="failedcountsall.pdf")
par(mar=c(6,3,1,1))
barplot(combinefail, names.arg=legtext, ylim=c(-10,100), las=2,
legend.text=c("0","1-3","4+"), args.legend=list(x="bottom", bty="n", horiz=T, cex=cex))	
#legend.text=c("0","1","2","3-4","5+"))	
dev.off()

rm(list=ls())

####  CONTACTS PARTICIPATED #####

source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

with1a<-getmany(c("P212","P213","P212")) #This is the number known who participated in the study

nknow<-getmany(c("P203","P203","P203")) #This is the number known (without time restriction - not the personal net size question)

seeds<-getmany(c("Seed","Seed","Seed"))

alreadyc<-list() #cleaned number already participated
nknowc<-list()  # cleaned number known (p203)
for(i in 1:12){
	alreadyc[[i]]<-with1a$dat[[i]][!is.na(with1a$dat[[i]]) & !is.na(nknow$dat[[i]])]
	nknowc[[i]]<-nknow$dat[[i]][!is.na(with1a$dat[[i]]) & !is.na(nknow$dat[[i]])]
	}

alreadycall<-c()
nknowcall<-c()
for(i in 1:12){
	alreadycall<-c(alreadycall,alreadyc[[i]])
	nknowcall<-c(nknowcall,nknowc[[i]])
	}

table(alreadycall)/length(alreadycall)

allprop<-alreadycall/(nknowcall-1)
allprop[allprop>1]<-1
allprop[allprop<0]<-0
allprop[is.na(allprop)]<-0 #these are NaNs' caused by 0/0

mean(allprop)

## Figure S1
for(i in 1:12){
	pdf(file=paste("propalreadyplots",i,".pdf", sep=""))
    tmp1<-with1a$dat[[i]][!is.na(nknow$dat[[i]])&!is.na(with1a$dat[[i]])]
	tmp2<-nknow$dat[[i]][!is.na(nknow$dat[[i]])&!is.na(with1a$dat[[i]])]
	seed1<-seeds$dat[[i]][!is.na(nknow$dat[[i]])&!is.na(with1a$dat[[i]])]
    propalreadyplot(tmp2,tmp1,seed1,legtext[i])
    dev.off()
    }

linecols<-rep(c("red","green","blue"),each=4)
ltys<-rep(c(2,3,4,5),times=3)

## Figure S2
pdf(file="fittedprobalready.pdf")
xpts<-seq(from=0,to=500,length=200)
plot(c(1:10),xlim=c(0,500),ylim=c(.05,.65),type='n',xlab="Sample Order",ylab="Fitted Probability of Alter in Study")
for(i in 1:12){
	degs<-nknow$dat[[i]][!is.na(nknow$dat[[i]])&!is.na(with1a$dat[[i]])]
    already<-with1a$dat[[i]][!is.na(nknow$dat[[i]])&!is.na(with1a$dat[[i]])]
	propalreadygroupline(degs, already, col=linecols[i],lty=ltys[i])
	}
legend('topright',lty=ltys,col=linecols,legend=legtext)
dev.off()

rm(list=ls())

#####  Degree over time ######
source(file="prepare_functions.R")

legtext<-c("FSW-SD","FSW-SA", "FSW-BA", "FSW-HI", "DU-SD","DU-SA", "DU-BA","DU-HI", "MSM-SD", "MSM-SA", "MSM-BA", "MSM-HI")

deg<-getmany(c("P204","P204","P204"))

seeds<-getmany(c("Seed","Seed","Seed"))

dates<-getmany(c("StartDate", "StartDate", "StartDate"))

degs<-list()

## Figure S3
for(i in 1:12){
	pdf(file=paste("degtime",legtext[i],".pdf", sep=""))
    #degs<-nknow$dat[[i]][!is.na(nknow$dat[[i]])]
    degs[[i]]<-deg$dat[[i]][!is.na(deg$dat[[i]])]
	degtime2(degs[[i]],legtext[i])
	dev.off()
	}	



dates<-getmany(c("StartDate", "StartDate", "StartDate"))
deg<-getmany(c("P204","P204","P204"))
degs<-list()  # exclude NA's and 0's
degsc<-list() # exclude NA's and 0's and NA dates
datesc<-list() #exclude NA's and 0's on deg and NA dates

for(i in 1:12){
  degs[[i]]<-deg$dat[[i]][!is.na(deg$dat[[i]]) & deg$dat[[i]]!=0]
  degsc[[i]]<-deg$dat[[i]][!is.na(deg$dat[[i]]) & deg$dat[[i]]!=0 & !is.na(dates$dat[[i]])]
  datesc[[i]]<-dates$dat[[i]][!is.na(deg$dat[[i]]) & deg$dat[[i]]!=0 & !is.na(dates$dat[[i]])]
}    


orderout<-matrix(NA,9,12)
dimnames(orderout)[[2]]<-legtext
dimnames(orderout)[[1]]<-c("linear","poisson","log deg","LTS .5", "LTS .9","M-reg","med-reg","Kendall tau","Spearman rho")

dateout<-matrix(NA,9,12)
dimnames(dateout)[[2]]<-legtext
dimnames(dateout)[[1]]<-c("linear","poisson","log deg","LTS .5", "LTS .9","M-reg","med-reg","Kendall tau","Spearman rho")


library(MASS)
library(robustbase)


for(i in 1:12){
  thisdeg<-degs[[i]]
  nn<-1:length(thisdeg)
  thisdegc<-degsc[[i]]
  thisdatesc<-datesc[[i]]
  
  mmm<-lm(thisdeg~nn)
  orderout[1,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)
  
  mmm<-glm(thisdeg~nn,family=poisson)
  orderout[2,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)  
 
  mmm<-lm(log(thisdeg)~nn)
  orderout[3,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)
 
  mmm<-ltsReg(thisdeg ~nn)
  orderout[4,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)   

  mmm<-lqs(thisdeg~nn, method="lts", quantile=floor(.9*length(nn)))
  orderout[5,i]<-ifelse(mmm$coefficients[2]<0,1,0)   

  mmm<-lmrob(thisdeg~nn)
  orderout[6,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)   

  mmm<-lqs(thisdeg~nn, method="lms")
  orderout[7,i]<-ifelse(mmm$coefficients[2]<0,1,0)   
     
  mmm<-cor.test(thisdeg, nn, alternative="l", method="kendall")   
  orderout[8,i]<-ifelse(mmm$estimate<0,1,0)     

  mmm<-cor.test(thisdeg, nn, alternative="l", method="spearman", exact=F)   
  orderout[9,i]<-ifelse(mmm$estimate<0,1,0)     

  mmm<-lm(thisdegc~thisdatesc)
  dateout[1,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)
  
  mmm<-glm(thisdegc~thisdatesc,family=poisson)
  dateout[2,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)  
 
  mmm<-lm(log(thisdegc)~thisdatesc)
  dateout[3,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)
 
  mmm<-ltsReg(thisdegc~thisdatesc)
  dateout[4,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)   

  mmm<-lqs(thisdegc~thisdatesc, method="lts", quantile=floor(.9*length(nn)))
  dateout[5,i]<-ifelse(mmm$coefficients[2]<0,1,0)   

  mmm<-lmrob(thisdegc~thisdatesc)
  dateout[6,i]<-ifelse(summary(mmm)$coefficients[2,1]<0,1,0)   

  mmm<-lqs(thisdegc~thisdatesc, method="lms")
  dateout[7,i]<-ifelse(mmm$coefficients[2]<0,1,0)   
     
  mmm<-cor.test(thisdegc, thisdatesc, alternative="l", method="kendall")   
  dateout[8,i]<-ifelse(mmm$estimate<0,1,0)     

  mmm<-cor.test(thisdegc, thisdatesc, alternative="l", method="spearman", exact=F)   
  dateout[9,i]<-ifelse(mmm$estimate<0,1,0)     

}  

orderout
dateout

apply(orderout,1,sum)
apply(dateout,1,sum)
apply(orderout[2:9,],2,sum)
apply(dateout[2:9,],2,sum)

orderout-dateout



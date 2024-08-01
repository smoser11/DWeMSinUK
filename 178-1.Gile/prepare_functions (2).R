# prepare_functions.R
# Krista Gile


#General Functions:

#Most other sections
getmany<-function(varnames,require=NULL){#note:  sw,du,msm
	if(length(varnames==1)){varnames<-rep(varnames,3)}
	manydatas<-list()
	manydatanames<-list()
	count<-1
    for(i in 1:3){
    	varname<-varnames[i]
    	for(j in 1:4){
    		manydatanames[[count]]<-paste("g",i,"p",j,sep="")
    		#load(file=paste(manydatanames[[count]],".RData",sep=""))
load(file=paste(manydatanames[[count]],"v03.RData",sep=""))
			if(is.null(require)){manydatas[[count]]<-DF[,varname]
				}else{manydatas[[count]]<-DF[,varname][!is.na(DF[,require])]}
    		count<-count+1
    		}
    	}	
	list(dat=manydatas,names=manydatanames)
	}
	
#TimeDynamics - 	
getmany2<-function(varnames,labels){#note:  sw,du,msm
	if(length(varnames==1)){varnames<-rep(varnames,3)}
	alldata<-NULL
	allnames<-NULL
	count<-1
    for(i in 1:3){
    	varname<-varnames[i]
    	for(j in 1:4){
    		tmpname<-paste("g",i,"p",j,sep="")
#    		load(file=paste(tmpname,".RData",sep=""))
    		load(file=paste(tmpname,"v03.RData",sep=""))
    		tmp2<-DF[,varname]
    		alldata<-c(alldata,tmp2)
    		allnames<-c(allnames,rep(labels[count],length(tmp2)))
    		count<-count+1
    		}
    	}	
	list(dat=alldata,names=allnames)
	}
	
#TimeDynamics	
getmany3<-function(varnames,labels){#note:  sw,du,msm
	if(length(varnames==1)){varnames<-rep(varnames,3)}
	alldata<-NULL
	allnames<-NULL
#	alldrop<-NULL
	count<-1
    for(i in 1:3){
    	varname<-varnames[i]
    	for(j in 1:4){
    		tmpname<-paste("g",i,"p",j,sep="")
#    		load(file=paste(tmpname,".RData",sep=""))
    		load(file=paste(tmpname,"v03.RData",sep=""))
    		tmp2<-DF[,varname]
    #		alldrop<-c(alldrop,is.na(DF$P203))
    		alldata<-c(alldata,tmp2)
    		allnames<-c(allnames,rep(labels[count],length(tmp2)))
    		count<-count+1
    		}
    	}	
	list(dat=alldata,names=allnames)#,drop=alldrop)
	}


	
#ParticipationBias
getmanyG<-function(varname,G=2,require="P203"){#note:  sw,du,msm
	#if(length(varnames==1)){varnames<-rep(varnames,3)}
	manydatas<-list()
	manydatanames<-list()
	count<-1
	i<-G
#    	varname<-varnames[i]
    	for(j in 1:4){
    		manydatanames[[count]]<-paste("g",i,"p",j,sep="")
    		#load(file=paste(manydatanames[[count]],".RData",sep=""))
load(file=paste(manydatanames[[count]],"v03.RData",sep=""))
			if(is.null(require)){manydatas[[count]]<-DF[,varname]
				}else{manydatas[[count]]<-DF[,varname][!is.na(DF[,require])]}
    		count<-count+1
    		}
	list(dat=manydatas,names=manydatanames)
	}

cleanmany<-function(listtoclean,listbasedon,bad){
	outlist<-list()
	for(i in 1:length(listtoclean)){
		outlist[[i]]<-clean1(listtoclean[[i]], listbasedon[[i]],bad)
		}
	outlist		
	}

clean1<-function(vartoclean,basedon,bad){
	vartoclean[basedon!=bad]	
	}



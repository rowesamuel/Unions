#Testing Clusters Errors in R
#Samuel Rowe
#6/22/2017

rm(list=ls())

#load libraries
library(foreign)
library(haven)
library(plm)
library(clusterSEs)

#Get MORG Data
#setwd("O:\\CEO- Chief Evluation Office\\CEO Analytics\\CPS\\NBER MORG")
setwd("/Users/Sam/Desktop/UMBC/Proposal/Data/CPS MORG")

#######################################################################################
#Pull CPS MORGs
#######################################################################################
#years<-c(2010:2013)
years<-c(2010:2017)
#filenames<-paste0("http://www.nber.org/morg/annual/morg",years,".dta")
filenames<-paste0("morg",years,".dta")

#http://www.nber.org/morg/annual/morg17.dta
cpsvars<-c("hhid","hrhhid2","lineno","minsamp","hrlonglk","intmonth","minsamp",
           "stfips","lfsr94","race","ethnic","vet1","sex","age","marital",
           "class94","earnwke","unionmme","unioncov","docc00","dind02","year",
           "hourslw","earnwt","grade92")

cps<-do.call(rbind,lapply(filenames,function(x) {
  print(x)
  cps1<-read.dta(x,convert.factors=T,warn.missing.labels=F)
  cps1<-cps1[cpsvars]
  }
))

######################################################################################
#Modify CPS Data
######################################################################################
############
#Unique 22-digit ID
############
cps$ID<-paste0(as.character(cps$hhid),as.character(cps$hrhhid2),as.character(cps$lineno))
cps<-cps[order(cps$ID,cps$minsamp),]

cps$sexdiff<-with(cps,ave(sex,ID,FUN=function(x) head(x,1)-tail(x,1)))
cps$racediff<-with(cps,ave(race,ID,FUN=function(x) head(x,1)-tail(x,1)))

#Union Status
cps$union<-NA
cps$union[cps$unionmme=="Yes"]<-1
cps$union[cps$unionmme=="No"]<-0

#Employed
cps$employed<-0
cps$employed[cps$lfsr94=="Employed-At Work"|cps$lfsr94=="Employed-Absent"]<-1

#Unemployed
cps$unemployed<-0
cps$unemployed[cps$lfsr94=="Unemployed-On Layoff"|cps$lfsr94=="Unemployed-Looking"]<-1

#Date
cps$hrmonth<-as.numeric(cps$intmonth)
table(cps$intmonth,cps$hrmonth)
cps$date<-as.Date(with(cps,paste(hrmonth,"12",year,sep=".")),format="%m.%d.%Y")

#States
cps$gestfips<-as.numeric(cps$stfips)
#####################################################################################
#Pre-Post Varibles
#####################################################################################
#Observations
cps$inob<-0
cps$inob[cps$stfips=="IN"|cps$stfips=="OH"|cps$stfips=="PA"|
          cps$stfips=="IL"|cps$stfips=="MO"|cps$stfips=="MN"|
          cps$stfips=="MI"|cps$stfips=="WI"]<-1
cps$miob<-0
cps$miob[cps$stfips=="MI"|cps$stfips=="OH"|cps$stfips=="PA"|
          cps$stfips=="IL"|cps$stfips=="MO"|cps$stfips=="MN"|
          cps$stfips=="WI"]<-1
cps$wiob<-0
cps$wiob[cps$stfips=="WI"|cps$stfips=="OH"|cps$stfips=="PA"|
          cps$stfips=="IL"|cps$stfips=="MO"|cps$stfips=="MN"]<-1

#Treatment
cps$INRTW<-0
cps$INRTW[cps$stfips=="IN"]<-1

cps$MIRTW<-0
cps$MIRTW[cps$stfips=="MI"]<-1

cps$WIRTW<-0
cps$WIRTW[cps$stfips=="WI"]<-1

#Post
cps$INPOST<-0
cps$INPOST[cps$date>="2012-03-05"]<-1

cps$MIPOST<-0
cps$MIPOST[cps$date>="2013-12-01"]<-1

cps$WIPOST<-0
cps$WIPOST[cps$date>="2015-03-10"]<-1

#######################################################################################
#Difference-in-Differences
#######################################################################################
IN<-cps[cps$inob==1,]
did1<-glm(union~INRTW*INPOST,data=IN)
summary(did1)

#Cluster Standard Errors at State
cdid1<-cluster.im.glm(did1,cps,~stfips,report=T)
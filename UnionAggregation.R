#Union Aggregation
#Samuel Rowe
#Oct 20, 2018

rm(list=ls())

###############
#Load Libraries
###############
library(haven)
library(ggplot2)
library(tigris)
library(leaflet)

###############
#Load CPS
###############
filelocation<-"/Users/Sam/Desktop/UMBC/Proposal/Data/CPS MORG/"
years<-c(2016:2017)
filenames<-paste0(filelocation,"morg",years,".dta")
cpsvars<-c("intmonth","year","stfips","cbsafips",
           "class94","lfsr94","unionmme","cmpwgt",
           "earnwt","dind02","occ2012","race","ethnic",
           "sex","grade92")
cps<-do.call(rbind,lapply(filenames,function(x) {
  tryCatch({
    print(x)
    cpsi<-read_dta(x)
    cpsi<-cpsi[,which(names(cpsi) %in% cpsvars)]
    },
    error=function(e){cat("Error: ",conditionMessage(e),"\n",print(x))})
  }
))

################
#Wage & Salary
################
#Employed
cps$emp<-ifelse(cps$lfsr94>=1 & cps$lfsr94<=2,1,0)
#Self-Employed
cps$selfemp<-ifelse(cps$class94 >= 6 & cps$class94 <= 8,1,0)
#Wage&Salary
cps$w_s<-ifelse(cps$emp==1 & cps$selfemp==0,1,0)
#Union
cps$union<-ifelse(cps$unionmme==1,1,0)

#Adjust weights
#NBER MORG weights are 3 times population
#http://www.nber.org/data/morg.html
#Composite Weight
cps$cmpwgt2<-cps$cmpwgt/3
#Earnings Weights (for unions)
cps$earnwt2<-cps$earnwt/(3*4)

################
#Aggregate
################
wage_salary<-aggregate(cps$cmpwgt2,by=list(
  year=cps$year,
  wage_salary=cps$w_s),
  FUN=sum
)
union<-aggregate(cps$earnwt2,by=list(
  year=cps$year,
  union=cps$union),
  FUN=sum
)

union_industry<-aggregate(cps$earnwt2,by=list(
  year=cps$year,
  union=cps$union,
  naics=cps$dind02),
  FUN=sum
)
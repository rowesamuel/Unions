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
#Wage and Salary
wage_salary<-aggregate(cps$cmpwgt2,by=list(
  year=cps$year,
  wage_salary=cps$w_s),
  FUN=sum
)
#Wage and Salary by State
wage_salary_state<-aggregate(cps$cmpwgt2,by=list(
  year=cps$year,
  wage_salary=cps$w_s,
  state=cps$stfips),
  FUN=sum
)

#Union
union<-aggregate(cps$earnwt2,by=list(
  year=cps$year,
  union=cps$union),
  FUN=sum
)

#Union by Industry
union_industry<-aggregate(cps$earnwt2,by=list(
  year=cps$year,
  union=cps$union,
  naics=cps$dind02),
  FUN=sum
)

#Union by State
union_state<-aggregate(cps$earnwt2,by=list(
  year=cps$year,
  union=cps$union,
  state=cps$stfips),
  FUN=sum
)

###########
#Merge
###########
union_state<-union_state[union_state$union==1,]
wage_salary_state<-wage_salary_state[wage_salary_state$wage_salary==1,]
state<-merge(wage_salary_state,union_state,by=c("year","state"))
colnames(state)[c(4,6)]<-c("wage_salary_ct","union_ct")
state<-state[,-which(names(state) %in% c("wage_salary","union"))]
state$density<-round((state$union_ct/state$wage_salary_ct)*100,2)
state<-state[order(state$state,state$year),]
###########
#Graph
###########

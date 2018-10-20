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
filelocation<-"/Users/Sam/Desktop/UMBC/Proposal/Data/CPS MORG"
years<-c(2000:2017)
filenames<-vector(outer(paste0("morg",years,".dta")))
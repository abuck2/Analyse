## setup
rm(list=ls())
setwd("/home/alexis/Documents/stats/annee2/analyse/projet/")
#wine<-read.csv("winequality-red.csv", sep = ";")
library(FactoMineR)
library(factoextra)
library(car)
library(ggplot2)
library(xtable)
library(pastecs)
library(Rcmdr)
library(ade4)
library(pastecs)
source("FDA_function.R")
source("plot.FDA_function.R")
source("dimdesc.FDA_function.R")

## Echantillonage
#set.seed(5)
#index<-sample(dim(wine)[1], 200)
#small_wine<-wine[index,]
#dim(wine)
#dim(small_wine)
#write.csv(x = small_wine, file="small_wine.csv")
#str(small_wine)

#wine<-small_wine

wine<-read.csv("small_wine.csv", sep = ",")
wine <- wine[,2:13]

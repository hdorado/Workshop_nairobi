

# file data-analysis-AEPS-BigData.R
# 
# This file contains a script to develop regressions with machine learning methodologies
#
#
# author: Hugo Andres Dorado 02-16-2015
#  
#This script is free: you can redistribute it and/or modify
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

#-----------------------------------------------------------------------------------------------------------------


#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES
rm(list=ls())
require(gtools)
require(gridBase)
require(caret)
require(party)
require(randomForest)
require(snowfall)
require(earth)
require(agricolae)
require(cowplot)
require(reshape)
require(stringr)
require(gbm)
require(plyr)
library(gridExtra)

#Work Directory

dirFol  <- "C:/Users/hadorado/Desktop/Nairobi_exchange/Workshop_Nairobi/regression/"

setwd(dirFol)

#Load functions

load("All-Functions-AEPS_BD.RData")

#read databaase

datNam  <- "tolima_maiz_csm_2016.csv"

dataSet   <- read.csv(datNam,row.names = 1)


summary(dataSet)

#DataBase structure

inputs  <- 1:51   #inputs columns
segme   <- 52      #split column
output  <- 53     #output column

namsDataSet <- names(dataSet)

#Creating the split factors


contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))


if(length(variety0)==0){variety = variety0 }else{variety = factor(c(variety0))}


#creating folders

createFolders(dirFol,variety)

#Descriptive Analysis


for(var in variety)
    descriptiveGraphics(var,dataSet,inputs = inputs,segme = segme,output = output,smooth=T,ylabel = "Rendimiento (kg/ha)",smoothInd = NULL,ghrp="box",res=80)

#DataSets ProcesosF

dataSetProces(variety ,dataSet,segme,corRed="caret")

#RANDOM FOREST

randomForestFun(variety,nb.it=20, ncores= 2,saveWS=T,barplot = T)

#MULTILAYER PERCEPTRON

multilayerPerceptronFun(variety,dirLocation=paste0(getwd(),"/"),nb.it=20,ylabs="Yield (Acre/HA)",pertuRelevance=T,ncores=2)

#CONDITIONAL FOREST; especify if you have categorical variables

for(var in variety)
 
conditionalForestFun(var,nb.it=10, ncores= 2,saveWS=T,barplot = T)

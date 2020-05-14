# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 28.04.2020
library(class)
library(knnGarden)
library(dplyr)
library(pROC)
library(e1071)
library(ramify)
# Create hexbin object and plot
source("merge.R")
source("bayes.R")
source("knnFunc.R")
source("plotting.R")
fillNAs <- function (dataset){
  for (i in 1:nrow(dataset)){
    if(is.na(dataset$G1.x[i])){
      dataset$G1.x[i] <- dataset$G1.y[i]
      dataset$G2.x[i] <- dataset$G2.y[i]
      dataset$G3.x[i] <- dataset$G3.y[i]
      dataset$failures.x[i] <- dataset$failures.y[i]
      dataset$paid.x[i] <- dataset$paid.y[i]
      dataset$absences.x[i] <- dataset$absences.y[i]
    }
  }
  for (i in 1:nrow(dataset)){
    if(is.na(dataset$G1.y[i])){
      dataset$G1.y[i] <- dataset$G1.x[i]
      dataset$G2.y[i] <- dataset$G2.x[i]
      dataset$G3.y[i] <- dataset$G3.x[i]
      dataset$failures.y[i] <- dataset$failures.x[i]
      dataset$paid.y[i] <- dataset$paid.x[i]
      dataset$absences.y[i] <- dataset$absences.x[i]
    }

  }
  return(dataset)
}


d3<- mergeDataFromFiles("student-mat.csv","student-por.csv")
d3<- fillNAs(d3)

accBayesWalc <- performBayesWalc(d3)
accBayesDalc <- performBayesDalc(d3)
accBayesWalc
accBayesDalc
acc <- performKNNChangingK(50,20,d3,"Walc")
outputWalc <- colMeans(acc)
acc <- performKNNChangingK(20,20,d3,"Dalc")
outputDalc <- colMeans(acc)
NumberOfNeighboursWalc<- linspace(1,length(outputWalc),length(outputWalc))
NumberOfNeighboursDalc<- linspace(1,length(outputDalc),length(outputDalc))
plotResultsVertically(NumberOfNeighboursWalc,outputWalc,NumberOfNeighboursDalc, outputDalc,
                      accBayesWalc,accBayesDalc,"Number of neighbours","Number of neighbours")

pValues <-c(0.5, 1, 1.5, 2, 2.5, 3)

acc <- performKNNChangingP(10,2,d3,"Walc",vectorOfP = pValues)
outputWalcP <- colMeans(acc)
acc <- performKNNChangingP(5,2,d3,"Dalc",vectorOfP = pValues)
outputDalcP <- colMeans(acc)

plotResultsVertically(pValues,outputWalcP,pValues,outputDalcP,
                      accBayesWalc,accBayesDalc,"p parametr value", "p parametr value")

plotResultsInBox(NumberOfNeighboursWalc,outputWalc,NumberOfNeighboursDalc, outputDalc,
                 pValues,outputWalcP,pValues,outputDalcP,'No of neighbours',
                 'No of neighbours','p','p')

rm(list = ls())

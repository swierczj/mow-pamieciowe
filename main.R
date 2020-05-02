# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 28.04.2020
library(class)
library(dplyr)
library(pROC)
library(e1071)
# Create hexbin object and plot
source("merge.R")
source("bayes.R")
source("knnFunc.R")
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

accBayesWalce <- performBayesWalc(d3)
accBayesDalc <- performBayesDalc(d3)
accBayesWalce
accBayesDalc
acc <- performKNN(100,100,d3,"Walc")
outputWalc <- colMeans(acc)
acc <- performKNN(100,100,d3,"Dalc")
outputDalc <- colMeans(acc)
par(mfrow = c(2, 1))
plot(outputWalc,
  col = "#cc0000",
  lwd = 10,
  main = "Accuracy of the $Walc classification using KNN method",
  xlab = "No. of neighbours",
  ylab = "% of accurate fits")

plot(outputDalc,
  col = "#ff00ff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using KNN method",
  xlab = "No. of neighbours",
  ylab = "% of accurate fits")

par(mfrow = c(1, 1))


rm(list = ls())

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
source("svmFunc.R")
source("plotting.R")

d3<- mergeDataFromFiles("student-mat.csv","student-por.csv")
d3<- fillNAs(d3)

# Bayes
accBayesWalc <- performBayesWalc(d3)
accBayesDalc <- performBayesDalc(d3)


# SVM
kernels <- c("linear","polynomial","sigmoid","radial")
outputWalcSVM <- performSVMWalcForKernels(d3,kernels)
outputDalcSVM <- performSVMDalcForKernels(d3,kernels)
ker <- as.factor(kernels)
plotResultsVerticallySVM(ker,outputWalcSVM,ker,outputDalcSVM,
                      accBayesWalc,accBayesDalc, "Kernel Type")

# KNN
outputWalc <- performKNNChangingK(50,d3,"Walc")
outputDalc <- performKNNChangingK(20,d3,"Dalc")
NumberOfNeighboursWalc<- linspace(1,length(outputWalc),length(outputWalc))
NumberOfNeighboursDalc<- linspace(1,length(outputDalc),length(outputDalc))
plotResultsVerticallyKNN(NumberOfNeighboursWalc,outputWalc,NumberOfNeighboursDalc, outputDalc,
                      accBayesWalc,accBayesDalc, "Number of neighbours")

pValues <-c(0.5, 1, 1.5, 2, 2.5, 3)

outputWalcP <- performKNNChangingP(10,d3,"Walc",vectorOfP = pValues)
outputDalcP <- performKNNChangingP(5,d3,"Dalc",vectorOfP = pValues)

plotResultsVerticallyKNN(pValues,outputWalcP,pValues,outputDalcP,
                      accBayesWalc,accBayesDalc,"p parametr value")

# TODO: KNN for window

rm(list = ls())

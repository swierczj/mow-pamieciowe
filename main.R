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
plotResultsVerticallySVMFactor(ker,outputWalcSVM,ker,outputDalcSVM,
                      accBayesWalc,accBayesDalc, "Kernel Type")
# cost value
for(k in 1:length(kernels)){
costs <- c(0.1,0.2,0.4,0.8,1,1.5,2,3,4,5,6,7,8,9,10)
outputWalcSVMCost <- performSVMWalcForCost(d3,costs,kernels[k])
outputDalcSVMCost <- performSVMDalcForCost(d3,costs,kernels[k])
plotResultsVerticallySVMNumeric(costs,outputWalcSVMCost,costs,outputDalcSVMCost,
                      accBayesWalc,accBayesDalc, c("Cost Value with kernel",kernels[k]))
}
# poly degree
degrees <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
outputWalcSVMPoly <- performSVMWalcForPoliDegree(d3,degrees)
outputDalcSVMPoly <- performSVMDalcForPoliDegree(d3,degrees)
plotResultsVerticallySVMNumeric(degrees,outputWalcSVMPoly,degrees,outputDalcSVMPoly,
                      accBayesWalc,accBayesDalc, "Polynomial Degree")


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

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

rm(list = ls())

source("merge.R")
source("bayes.R")
source("knnFunc.R")
source("svmFunc.R")
source("plotting.R")

d3<- mergeDataFromFiles("student-mat.csv","student-por.csv")
d3<- fillNAs(d3)

###################################################### Bayes ###########################################################
accBayesWalc <- performBayesWalc(d3)
accBayesDalc <- performBayesDalc(d3)


####################################################### SVM ############################################################
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
degrees <- c(1,2,3,4,5,6,7,8,9,10)
outputWalcSVMPoly <- performSVMWalcForPoliDegree(d3,degrees)
outputDalcSVMPoly <- performSVMDalcForPoliDegree(d3,degrees)
plotResultsVerticallySVMNumeric(degrees,outputWalcSVMPoly,degrees,outputDalcSVMPoly,
                      accBayesWalc,accBayesDalc, "Polynomial Degree")


####################################################### KNN ############################################################
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


########################################################################################################################
################################################ Less dimensions #######################################################
########################################################################################################################
# Decide with columns to drop -> filled columns may introduce false data
#                             -> "paid", "falitures", "absences","G1.x","G2.x","G3.x","G1.y","G2.y","G3.y"
# To further decrease dimensions one has to look at the graphs to drop least important columns

par(mfrow = c(5, 5))
plot(d3$school, d3$Walc, xlab = "d3$school", ylab = "Walc")
plot(d3$sex, d3$Walc, xlab = "sex", ylab = "Walc")
plot(d3$age, d3$Walc, xlab = "age", ylab = "Walc")
plot(d3$address, d3$Walc, xlab = "address", ylab = "Walc")
plot(d3$famsize, d3$Walc, xlab = "famsize", ylab = "Walc")
plot(d3$Pstatus, d3$Walc, xlab = "Pstatus", ylab = "Walc")
plot(d3$Medu, d3$Walc, xlab = "Medu", ylab = "Walc")
plot(d3$Fedu, d3$Walc, xlab = "Fedu", ylab = "Walc")
plot(d3$Mjob, d3$Walc, xlab = "Mjob", ylab = "Walc")
plot(d3$Fjob, d3$Walc, xlab = "Fjob", ylab = "Walc")
plot(d3$reason, d3$Walc, xlab = "reason", ylab = "Walc")
plot(d3$guardian, d3$Walc, xlab = "guardian", ylab = "Walc")
plot(d3$nursery, d3$Walc, xlab = "nursery", ylab = "Walc")
plot(d3$studytime, d3$Walc, xlab = "studytime", ylab = "Walc")
plot(d3$famsup, d3$Walc, xlab = "famsup", ylab = "Walc")
plot(d3$schoolsup, d3$Walc, xlab = "schoolsup", ylab = "Walc")
plot(d3$activities, d3$Walc, xlab = "activities", ylab = "Walc")
plot(d3$higher, d3$Walc, xlab = "higher", ylab = "Walc")
plot(d3$internet, d3$Walc, xlab = "internet", ylab = "Walc")
plot(d3$romantic, d3$Walc, xlab = "romantic", ylab = "Walc")
plot(d3$famrel, d3$Walc, xlab = "famrel", ylab = "Walc")
plot(d3$freetime, d3$Walc, xlab = "freetime", ylab = "Walc")
plot(d3$goout, d3$Walc, xlab = "goout", ylab = "Walc")
plot(d3$health, d3$Walc, xlab = "health", ylab = "Walc")
par(mfrow = c(1, 1))

#-> after looking on the plots it is seen that "school", "address", "famsize", "Pstatus", "guardian", "activities",
# "internet", "romantic", "Mjob" do not affect Walc
########################################################################################################################
columnsToDrop <- c("G1.x","G2.x","G3.x","G1.y","G2.y","G3.y",
                   "paid.x", "failures.x", "absences.x","paid.y", "failures.y", "absences.y",
                   "school", "address", "famsize", "Pstatus", "guardian", "activities",
                   "internet", "romantic", "Mjob")
d4 <- dropDimensions(d3, columnsToDrop)

# Bayes
accBayesWalc <- performBayesWalc(d4)
accBayesDalc <- performBayesDalc(d4)


####################################################### SVM ############################################################
kernels <- c("linear","polynomial","sigmoid","radial")
outputWalcSVM <- performSVMWalcForKernels(d4,kernels)
outputDalcSVM <- performSVMDalcForKernels(d4,kernels)
ker <- as.factor(kernels)
plotResultsVerticallySVMFactor(ker,outputWalcSVM,ker,outputDalcSVM,
                      accBayesWalc,accBayesDalc, "Kernel Type")
# cost value
for(k in 1:length(kernels)){
costs <- c(0.1,0.2,0.4,0.8,1,1.5,2,3,4,5,6,7,8,9,10)
outputWalcSVMCost <- performSVMWalcForCost(d4,costs,kernels[k])
outputDalcSVMCost <- performSVMDalcForCost(d4,costs,kernels[k])
plotResultsVerticallySVMNumeric(costs,outputWalcSVMCost,costs,outputDalcSVMCost,
                      accBayesWalc,accBayesDalc, c("Cost Value with kernel",kernels[k]))
}
# poly degree
degrees <- c(1,2,3,4,5,6,7,8,9,10)
outputWalcSVMPoly <- performSVMWalcForPoliDegree(d4,degrees)
outputDalcSVMPoly <- performSVMDalcForPoliDegree(d4,degrees)
plotResultsVerticallySVMNumeric(degrees,outputWalcSVMPoly,degrees,outputDalcSVMPoly,
                      accBayesWalc,accBayesDalc, "Polynomial Degree")


####################################################### KNN ############################################################
outputWalc <- performKNNChangingK(50,d4,"Walc")
outputDalc <- performKNNChangingK(20,d4,"Dalc")
NumberOfNeighboursWalc<- linspace(1,length(outputWalc),length(outputWalc))
NumberOfNeighboursDalc<- linspace(1,length(outputDalc),length(outputDalc))
plotResultsVerticallyKNN(NumberOfNeighboursWalc,outputWalc,NumberOfNeighboursDalc, outputDalc,
                      accBayesWalc,accBayesDalc, "Number of neighbours")

pValues <-c(0.5, 1, 1.5, 2, 2.5, 3)

outputWalcP <- performKNNChangingP(10,d4,"Walc",vectorOfP = pValues)
outputDalcP <- performKNNChangingP(5,d4,"Dalc",vectorOfP = pValues)

plotResultsVerticallyKNN(pValues,outputWalcP,pValues,outputDalcP,
                      accBayesWalc,accBayesDalc,"p parametr value")

rm(list = ls())

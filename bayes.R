# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 01.05.2020
accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
performBayesWalc <- function (dataset){
  ind <- which(colnames(dataset)=="Walc")
  dataset[,ind] <- as.factor(dataset[,ind])
  model <- naiveBayes(Walc ~.,data = dataset)
  pred <- predict(model, dataset[,-ind])
  tb <- table(pred, dataset$Walc)
  accBayesWalc<- accuracy(tb)
  return(accBayesWalc)
}
performBayesDalc <- function (dataset){
  ind <- which(colnames(dataset)=="Dalc")
  dataset[,ind] <- as.factor(dataset[,ind])
  model <- naiveBayes(Dalc ~.,data = dataset)
  pred <- predict(model, dataset[,-ind])
  tb <- table(pred, dataset$Dalc)
  accBayesDalc<- accuracy(tb)
  return(accBayesDalc)
}
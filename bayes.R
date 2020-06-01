# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 01.05.2020
accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
performBayesWalc <- function (dataset){
  smp_size <- floor(0.7 * nrow(dataset))
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
  ind <- which(colnames(dataset)=="Walc")
  dataset[,ind] <- as.factor(dataset[,ind])
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  model <- naiveBayes(Walc ~.,data = train)
  pred <- predict(model, test)
  tb <- table(pred, test$Walc)
  acc <- accuracy(tb)
  library(pROC)
  y_pred<-as.ordered(pred)
  auc <- multiclass.roc(response = test$Walc, predictor = y_pred,direction = "<")
  print(auc)
  return(acc)
}
performBayesDalc <- function (dataset){
  smp_size <- floor(0.7 * nrow(dataset))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
  ind <- which(colnames(dataset)=="Dalc")
  dataset[,ind] <- as.factor(dataset[,ind])
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  model <- naiveBayes(Dalc ~.,data = train)
  pred <- predict(model, test)
  tb <- table(pred, test$Dalc)
  y_pred<-as.ordered(pred)
  auc <- multiclass.roc(response = test$Dalc, predictor = y_pred,direction = "<")
  print(auc)
  acc<- accuracy(tb)
  return(acc)
}
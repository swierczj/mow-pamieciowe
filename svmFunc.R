# Title     : Funkcje dla algorytmu SVM
# Objective :
# Created by: jeczm
# Created on: 14.05.2020

accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
printAUC <- function (pred,testClass){
  y_pred<-as.ordered(pred)
  auc <- multiclass.roc(response = testClass, predictor = y_pred,direction = "<")
  print(auc)
}

splitIntoTrainAndTest <- function (dataset,ratio){
  smp_size <- floor(ratio * nrow(dataset))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  return(list(v1=train,v2=test))
}


performSVMWalcForKernels <- function (dataset,kernels){
  acc <- matrix(0,ncol = 1, nrow = length(kernels))
  for(k in 1:length(kernels)){
    print(c("Calculating Walc for kernel of type",kernels[k]))
      ind <- which(colnames(dataset)=='Walc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Walc~.,data = train,kernel = kernels[k])
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Walc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Walc)
  }
  return(acc)
}

performSVMWalcForPoliDegree <- function (dataset,degrees){
  acc <- matrix(0,ncol = 1, nrow = length(degrees))
  for(k in 1:length(degrees)){
    print(c("Calculating Walc using SVM for ",degrees[k]," degree polynomial"))
      ind <- which(colnames(dataset)=='Walc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Walc~.,data = train,kernel = "polynomial", degree = degrees[k] )
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Walc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Walc)
  }
  return(acc)
}
performSVMDalcForPoliDegree <- function (dataset,degrees){
  acc <- matrix(0,ncol = 1, nrow = length(degrees))
  for(k in 1:length(degrees)){
    print(c("Calculating Dalc using SVM for ",degrees[k]," degree polynomial"))
      ind <- which(colnames(dataset)=='Dalc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Dalc~.,data = train,kernel = "polynomial", degree = degrees[k] )
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Dalc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Dalc)
  }
  return(acc)
}

performSVMWalcForCost <- function (dataset,costs,kernel){
  acc <- matrix(0,ncol = 1, nrow = length(costs))
  for(k in 1:length(costs)){
      print(c("Calculating Dalc using SVM for ",costs[k]," cost of constraints violation ->",kernel))
      ind <- which(colnames(dataset)=='Walc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Walc~.,data = train,kernel = kernel,  cost = costs[k] )
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Walc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Walc)
  }
  return(acc)
}
performSVMDalcForCost <- function (dataset, costs, kernel){
  acc <- matrix(0,ncol = 1, nrow = length(costs))
  for(k in 1:length(costs)){
    print(c("Calculating Dalc using SVM for ",costs[k]," cost of constraints violation ->",kernel))
      ind <- which(colnames(dataset)=='Dalc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Dalc~.,data = train,kernel = kernel, cost = costs[k] )
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Dalc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Dalc)
  }
  return(acc)
}
performSVMDalcForKernels <- function (dataset,kernels){
  acc <- matrix(0,ncol = 1, nrow = length(kernels))
  for(k in 1:length(kernels)){
    print(c("Calculating Dalc for kernel of type",kernels[k]))
      ind <- which(colnames(dataset)=='Dalc')
      dataset[,ind] <- as.factor(dataset[,ind])
      smp_size <- floor(0.7 * nrow(dataset))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      svmModel <- svm(Dalc~.,data = train,kernel = kernels[k])
      pred <- predict(svmModel,test[,-ind])
      x <- table(pred, test$Dalc)
      acc[k]<-accuracy(x)
      printAUC(pred = pred, testClass = test$Dalc)
  }
  return(acc)
}
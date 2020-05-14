# Title     : Funkcje dla algorytmu SVM
# Objective :
# Created by: jeczm
# Created on: 14.05.2020

accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
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
      output<-sum(diag(x)/(sum(rowSums(x)))) * 100
      acc[k]<-output
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
      output<-sum(diag(x)/(sum(rowSums(x)))) * 100
      acc[k]<-output
  }
  return(acc)
}
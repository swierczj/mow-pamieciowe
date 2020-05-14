# Title     : Funkcje dla algorytmu Knn
# Objective : Przygotowanie danych i wykonanie klasyfikacji z wykorzystaniem KNN
# Created by: jeczm
# Created on: 01.05.2020
accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
performKNNChangingK <- function (maxKNumber,dataset,NameOfClassToPredict){
  #Switch all to numeric
  for(i in 1:ncol(dataset)){
    dataset[,i] <- as.numeric(dataset[,i])
  }
  maxk <- maxKNumber
  acc <- matrix(0,ncol = maxk , nrow = 1)
  lastindx <- maxk
  smp_size <- floor(0.7 * nrow(dataset))
  set.seed(123)
  brokenLoop <-0
  for(knum in 1:maxk){
      print(c("Calculating for knum",knum))
      #Split datasets into train and test
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      #Find weekend alcohol consumption
      ind <- which(colnames(dataset)==NameOfClassToPredict)
      TK<-sort(as.matrix(table(train[,ind])),decreasing=F)
      if(knum>TK[1])
      {
        brokenLoop <- 1
        print(c('kValue too big, max k value for ',NameOfClassToPredict))
        lastindx<-knum-1
        print(knum-1)
        break

      }
      predicted <- knn(train[,-ind],test[,-ind],cl=train[,ind],k=knum)
      tb <- table(predicted,test[,ind])
      tempAcc <- accuracy(tb)
      acc[knum] <- tempAcc
    if (brokenLoop)
      break
  }
  return(acc[0:lastindx])
}

performKNNChangingP<- function (K,dataset,NameOfClassToPredict,vectorOfP){
  for(i in 1:ncol(dataset)){
    dataset[,i] <- as.numeric(dataset[,i])
  }
  k <- K
  p <- vectorOfP
  acc <- matrix(0,ncol = length(p), nrow = 1)
  smp_size <- floor(0.7 * nrow(dataset))
  set.seed(123)
  for(i in 1:length(p)){
      print(c("Calculating",NameOfClassToPredict, "for p",p[i]))
      #Split datasets into train and test
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      #Find weekend alcohol consumption
      ind <- which(colnames(dataset)==NameOfClassToPredict)
      TK <- sort(as.matrix(table(train[,ind])),decreasing=F)
      if(k>TK[1])
      {
        print(c('kValue too big, max k value for ',NameOfClassToPredict))
        k<-TK[1]
        print(TK[1])
      }
      predicted <- knnVCN(train[,-ind],train[,ind],test[,-ind],K=k,method = "minkowski",p =p[i])
      tb <- table(predicted$TstXIBelong,test[,ind])
      tempAcc <- accuracy(tb)
      acc[i] <- tempAcc
  }
  return(acc)
}

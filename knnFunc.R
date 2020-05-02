# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 01.05.2020
accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
performKNN <- function (maxKNumber,noOfSamples,dataset,NameOfClassToPredict){
  #Switch all to numeric
  for(i in 1:ncol(dataset)){
    dataset[,i] <- as.numeric(dataset[,i])
  }
  maxk <- maxKNumber
  noSam <- noOfSamples
  acc <- matrix(0,ncol = maxk , nrow = noSam)
  smp_size <- floor(0.7 * nrow(dataset))
  ## set the seed to make your partition reproducible
  set.seed(123)
  for(knum in 1:maxk)
    for(s in 1:noSam){
      #Split datasets into train and test
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      test <- dataset[-train_ind, ]
      #Find weekend alcohol consumption
      ind <- which(colnames(dataset)==NameOfClassToPredict)
      predicted <- knn(train[,-ind],test[,-ind],cl=train[,ind],k=knum)
      tb <- table(predicted,test[,ind])
      tempAcc <- accuracy(tb)
      acc[s,knum] <- tempAcc
    }
  return(acc)
}

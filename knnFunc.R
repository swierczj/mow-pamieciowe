# Title     : Funkcje dla algorytmu Knn
# Objective : Przygotowanie danych i wykonanie klasyfikacji z wykorzystaniem KNN
# Created by: jeczm
# Created on: 01.05.2020
accuracy <- function(x){
  output<-sum(diag(x)/(sum(rowSums(x)))) * 100
  return(output)
}
printAUC <- function (pred,testClass){
  y_pred<-as.ordered(pred)
  auc <- multiclass.roc(response = testClass, predictor = y_pred,direction = "<")
  print(auc)
}

getNumeric <- function (ds) {
  tmp <- ds
  for(i in 1:ncol(ds)) {
    ds[, i] <- as.numeric(ds[, i])
    if(is.na(ds[, i])) {
      if(isBinary(tmp[, i])) {
        ds[, i] <- setBinaryValues(tmp[, i])
      }
      else {
        ds[, i] <- setNominalNumerical(tmp[, i])
      }
    } 
  }
  return(ds)
}

isBinary <- function (column) {
  values <- unique(column)
  dbg_len <- length(values)
  bool_val <- (dbg_len == 2)
  return(bool_val)
}

setBinaryValues <- function (column) {
  values <- unique(column)
  lookup <- c()
  if (values[1] == "GP" || values[1] == "MS") {
    lookup <- c("GP" = 1, "MS" = 0)
  }
  else if (values[1] == "LE3" || values[1] == "GT3") {
    lookup <- c("LE3" = 1, "GT3" = 0)
  }
  else if (values[1] == "F" || values[1] == "M") {
    lookup <- c("F" = 1, "M" = 0)
  }
  else if (values[1] == "U" || values[1] == "R") {
    lookup <- c("U" = 1, "R" = 0)
  }
  else if (values[1] == "T" || values[1] == "A") {
    lookup <- c("T" = 1, "A" = 0)
  }
  else if (values[1] == "yes" || values[1] == "no") {
    lookup <- c("yes" = 1, "no" = 0)
  }
  res <- lookup[column]
  return(res)
}

setNominalNumerical <- function (column) {
  values <- unique(column)
  lookup <- c()
  # Mjob, Fjob
  if ("other" %in% values && "services" %in% values && "at_home" %in% values 
     && "teacher" %in% values && "health" %in% values) {
    lookup <- c("other" = 0, "services" = 1, "at_home" = 2, "teacher" = 3
               , "health" = 4)
  }
  # reason attr
  else if ("other" %in% values && "reputation" %in% values && "home" %in% values
          && "course" %in% values) {
    lookup <- c("other" = 0, "reputation" = 1, "home" = 2, "course" = 3)
  }
  # guardian
  else if ("other" %in% values && "mother" %in% values && "father" %in% values){
    lookup <- c("other" = 0, "mother" = 1, "father" = 2)
  }
  res <- lookup[column]
  return(res) 
}

performKNNChangingK <- function (maxKNumber, dataset, NameOfClassToPredict){
  #Switch all to numeric
  #for(i in 1:ncol(dataset)){
  #  dataset[,i] <- as.numeric(dataset[,i])
  #}
  dataset <- getNumeric(dataset)
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
      printAUC(pred = predicted, testClass = test[,ind])
      tempAcc <- accuracy(tb)
      acc[knum] <- tempAcc
    if (brokenLoop)
      break
  }
  return(acc[0:lastindx])
}

performKNNChangingP<- function (K,dataset,NameOfClassToPredict,vectorOfP){
  #for(i in 1:ncol(dataset)){
  #  dataset[,i] <- as.numeric(dataset[,i])
  #}
  dataset <- getNumeric(dataset)
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
      printAUC(pred = predicted$TstXIBelong, testClass = test[,ind])
      tempAcc <- accuracy(tb)
      acc[i] <- tempAcc
  }
  return(acc)
}

performKNNSlidingWindow <- function(dataset, pred_class_name, padding_perc) {
  best_performance <- matrix(0, ncol = length(padding_perc), nrow = 1)
  for (p in 1:length(padding_perc)){
    padding_size <- floor(padding_perc[p]*(ncol(dataset) - 1))
    if (padding_size %% 2 != 0)
      padding_size <- padding_size - 1
  
    dataset <- getNumeric(dataset)
    smp_size <- floor(0.7 * nrow(dataset))
    k_val <- floor(sqrt(smp_size)) # according to papers it's recommended value
    acc <- matrix(0, ncol = padding_size + 1, nrow = 1)
    set.seed(123)
    test_pred_ind <- which(colnames(dataset) == pred_class_name)
    iterations <- padding_size + 1 # sliding window for padding and also for provided data
    for(i in 1:iterations){
      if(i <= padding_size)
        print(c("Calculating knn for", getWindowStage(i, padding_size)))
      else
        print(c("Calculating knn for window without padding"))
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      train <- dataset[train_ind, ]
      train_labels <- train[, test_pred_ind]
      test <- dataset[-train_ind, ]
      train <- adjustSetToWindow(train, i, padding_size, pred_class_name)
      train_pred_ind <- which(colnames(train) == pred_class_name)
      if (length(train_pred_ind) == 0){
        if (i <= padding_size/2)
          train_set <- train[, 1:(ncol(train) - 1)]
        else
          train_set <- train[, 2:ncol(train)]
      }
      else
        train_set <- train[, -train_pred_ind]
      #predicted <- knn(train[, -train_pred_ind], test[, -test_pred_ind], cl = train[, train_pred_ind], k = k_val)
    
      TK <- sort(as.matrix(table(train_labels)), decreasing=F)
      if(k_val>TK[1])
      {
        print(c('kValue too big, max k value for ', pred_class_name))
        k_val <- TK[1]
        print(TK[1])
      }
      predicted <- knn(train_set, test[, -test_pred_ind], cl = train_labels, k = k_val)
      tb <- table(predicted, test[, test_pred_ind])
      printAUC(pred = predicted, testClass = test[, test_pred_ind])
      tempAcc <- accuracy(tb)
      acc[i] <- tempAcc
    }
    best_performance[p] <- max(acc[1:(length(acc) - 1)])
    #comp_val <- acc[length(acc)]
  }
  #best_performance <- cbind(best_performance, comp_val)
  return(best_performance)
}

getWindowStage <- function(curr_val, whole_size){
  str <- c()
  if (curr_val <= whole_size/2)
    str <- c((whole_size/2) - (curr_val - 1), "padding attributes before begin")
  else
    str <- c((curr_val - whole_size/2), "padding attributes after end")
  return(str)
}

adjustSetToWindow <- function(tr_set, window_stg, whole_padding, to_pred){
  if (window_stg > whole_padding)
    return(tr_set)
  ncol_to_add <- 0
  to_prepend <- 1
  if (window_stg <= whole_padding/2)
    ncol_to_add <- ((whole_padding/2) - (window_stg - 1))
  else {
    ncol_to_add <- (window_stg - (whole_padding/2))
    to_prepend <- 0
  }
  padding <- matrix(0, ncol = ncol_to_add, nrow = nrow(tr_set))
  window_set <- tr_set
  tr_set_sub_ncol <- ncol(tr_set) - ncol_to_add
  if (to_prepend == 1) {
    window_set <- window_set[, 1:tr_set_sub_ncol]
    window_set <- cbind(padding, window_set)
  }
  else {
    window_set <- window_set[, (ncol_to_add + 1):ncol(window_set)]
    window_set <- cbind(window_set, padding)
  }
  return(window_set)
}

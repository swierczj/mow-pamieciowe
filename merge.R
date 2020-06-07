# Title     : TODO
# Objective : TODO
# Created by: jeczm
# Created on: 01.05.2020

mergeDataFromFiles <- function (file1,file2){
d1<-read.table(file1,sep = ',',header=TRUE)
d2<-read.table(file2,sep = ',',header=TRUE)

#Outer join based on atributes independent from the course
d3<-merge(d2,d1,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian",
                    "nursery","studytime","famsup","schoolsup","activities","higher","internet",
                    "traveltime","romantic","famrel","freetime","goout","Dalc","Walc","health"),all=TRUE)
#"paid", "failures", "absences" - zalezne od kursu
return(d3)
}
fillNAs <- function (dataset){
  for (i in 1:nrow(dataset)){
    if(is.na(dataset$G1.x[i])){
      dataset$G1.x[i] <- dataset$G1.y[i]
      dataset$G2.x[i] <- dataset$G2.y[i]
      dataset$G3.x[i] <- dataset$G3.y[i]
      dataset$failures.x[i] <- dataset$failures.y[i]
      dataset$paid.x[i] <- dataset$paid.y[i]
      dataset$absences.x[i] <- dataset$absences.y[i]
    }
  }
  for (i in 1:nrow(dataset)){
    if(is.na(dataset$G1.y[i])){
      dataset$G1.y[i] <- dataset$G1.x[i]
      dataset$G2.y[i] <- dataset$G2.x[i]
      dataset$G3.y[i] <- dataset$G3.x[i]
      dataset$failures.y[i] <- dataset$failures.x[i]
      dataset$paid.y[i] <- dataset$paid.x[i]
      dataset$absences.y[i] <- dataset$absences.x[i]
    }

  }
  return(dataset)
}

dropDimensions <- function (dataset,columnsToDrop){
  drops <- columnsToDrop
  newDF <-  dataset[ , !(names(dataset) %in% drops)]
  return(newDF)
}


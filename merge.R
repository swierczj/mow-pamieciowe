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
#"paid", "falitures", "absences" - zalezne od kursu
return(d3)
}
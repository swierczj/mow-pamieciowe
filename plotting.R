# Title     : Wykresy
# Objective : Latwe wykreslanie wielu obrazków w jednym oknie
# Created by: jeczm
# Created on: 14.05.2020
plotResultsVertically <- function (x1,y1,x2,y2,line1,line2,xLabel1,xLabel2){
  par(mfrow = c(2, 1))
  plot(x1, y1,
  col = "#cc0000",
  lwd = 10,
  main = "Accuracy of the $Walc classification using KNN method",
  xlab = xLabel1,
  ylab = "% of accurate fits")
  abline(h=line1)
  plot(x2, y2,
  col = "#ff00ff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using KNN method",
  xlab = xLabel2,
  ylab = "% of accurate fits")
abline(h=line2)
par(mfrow = c(1, 1))
}

plotResultsInBox <- function (x1,y1,x2,y2,x3,y3,x4,y4,xLabel1,xLabel2,xLabel3,xLabel4){
  par(mfrow = c(2, 2))
  plot(x1, y1,
  col = "#cc0000",
  lwd = 10,
  main = "Accuracy of the $Walc classification using KNN method",
  xlab = xLabel1,
  ylab = "% of accurate fits")
  plot(x2, y2,
  col = "#ff00ff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using KNN method",
  xlab = xLabel2,
  ylab = "% of accurate fits")
  plot(x3, y3,
  col = "#ffff00",
  lwd = 10,
  main = "Accuracy of the $Walc classification using KNN method",
  xlab = xLabel3,
  ylab = "% of accurate fits")
  plot(x4, y4,
  col = "#00ffff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using KNN method",
  xlab = xLabel4,
  ylab = "% of accurate fits")
par(mfrow = c(1, 1))
}


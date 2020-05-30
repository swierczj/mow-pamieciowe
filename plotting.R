# Title     : Wykresy
# Objective : Latwe wykreslanie wielu obrazków w jednym oknie
# Created by: jeczm
# Created on: 14.05.2020

plotResultsVerticallyKNN <- function (x1,y1,x2,y2,line1,line2,xLabel){
  par(mfrow = c(2, 1))
  plot(x1, y1,
  col = "#cc0000",
  lwd = 10,
  main = "Accuracy of the $Walc classification using KNN method",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
  abline(h=line1)
  plot(x2, y2,
  col = "#ff00ff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using KNN method",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
abline(h=line2)
par(mfrow = c(1, 1))
}
plotResultsVerticallySVMFactor <- function (x1,y1,x2,y2,line1,line2,xLabel){
  par(mfrow = c(2, 1))
  plot(x1, y1,
  col = "#cc0000",
  lwd = 2,
  main = "Accuracy of the $Walc classification using SVM",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
  abline(h=line1)
  plot(x2, y2,
  col = "#ff00ff",
  lwd = 2,
  main = "Accuracy of the $Dalc classification using SVM",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
abline(h=line2)
par(mfrow = c(1, 1))
}
plotResultsVerticallySVMNumeric <- function (x1,y1,x2,y2,line1,line2,xLabel){
  par(mfrow = c(2, 1))
  plot(x1, y1,
  col = "#cc0000",
  lwd = 10,
  main = "Accuracy of the $Walc classification using SVM",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
  abline(h=line1)
  plot(x2, y2,
  col = "#ff00ff",
  lwd = 10,
  main = "Accuracy of the $Dalc classification using SVM",
  xlab = xLabel,
  ylab = "% of accurate fits",
  ylim = c(0,100))
abline(h=line2)
par(mfrow = c(1, 1))
}
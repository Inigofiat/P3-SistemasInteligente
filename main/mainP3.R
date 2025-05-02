library(lattice)
library(ggplot2)
library(caret)

rm(list=ls())
cat("\014")

if(!is.null(dev.list())) dev.off()
graphics.off()

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


source("linear-regression-utils.R")


data <- read.csv("../data/2025_Advertising.csv")
# data[, 1] <- NULL
# print.data.summary(data)

# col(is.na(data$TV))
# 
# entrenamientoPorcentaje <- 75
# testPorcentaje <- 100 - entrenamientoPorcentaje 

# scatter.smooth(x=data$TV, y=data$Sales, main="TV vs Sales", 
#                xlab = "Inversión en TV (miles $)", 
#                ylab = "Ventas (miles $)")
# scatter.smooth(x=data$Radio, y=data$Sales, main="Radio vs Sales", 
#                xlab = "Inversión en Radio (miles $)", 
#                ylab = "Ventas (miles $)")
# scatter.smooth(x=data$Newspaper, y=data$Sales, main="Newspaper vs Sales", 
#                xlab = "Inversión en Newspaper (miles $)", 
#                ylab = "Ventas (miles $)")
# 
# cor(data$Sales, data$TV)
# cor(data$Sales, data$Radio)
# cor(data$Sales, data$Newspaper)

contNullVac <- colSums(is.na(data))
print("Cantidad de valores nulos por columna:")
print(contNullVac)

sustitucionNull <- function(dataframe) {
  for(col in colnames(dataframe)) {
    if(any(is.na(dataframe[[col]]))) {
      valorComun <- names(sort(table(dataframe[[col]], useNA = "no"), decreasing = TRUE))[1]
      
      if(is.numeric(dataframe[[col]])) {
        valorComun <- as.numeric(valorComun)
      }
      
      dataframe[[col]][is.na(dataframe[[col]])] <- valorComun
      
      cat("Columna", col, "- NAs reemplazados con:", valorComun, "\n")
    }
  }
  return(dataframe)
}

data <- sustitucionNull(data)

matrizCorrelacion <- cor(data[, c("TV", "Radio", "Newspaper", "Sales")])
print("Matriz de correlación:")
print(matrizCorrelacion)

par(mfrow=c(2,2))
dev.new()
scatter.smooth(x=data$TV, y=data$Sales, main="TV vs Sales", 
                              xlab = "Inversión en TV (miles $)",
                              ylab = "Ventas (miles $)")

dev.new()
scatter.smooth(x=data$Radio, y=data$Sales, main="Radio vs Sales", 
                              xlab = "Inversión en Radio (miles $)",
                              ylab = "Ventas (miles $)")

dev.new()
scatter.smooth(x=data$Newspaper, y=data$Sales, main="Newspaper vs Sales", 
                              xlab = "Inversión en Newspaper (miles $)",
                              ylab = "Ventas (miles $)")
par(mfrow=c(1,1))

pruebasIndices <- createDataPartition(y=data$Sales, p=0.75, list=FALSE)
pruebaData <- data[pruebasIndices,]
testData <- data[pruebasIndices,]

calculoErrores <- function(actual, nuevo){
  mae <- mean(abs(actual-nuevo))
  mse <- mean((actual-nuevo)^2)
  mape <- mean(abs((actual - nuevo) / actual)) * 100
  
  return(list(MAE = mae, MSE = mse, MAPE = mape))
  
}

modelos <- list()
erroresModelos <- data.frame(Model=character(), MAE=numeric(), MSE=numeric(),
                             MAPE=numeric(), stringsAsFactors = FALSE)

modelos[[1]] <- lm(Sales ~ TV, data=pruebaData) 
predicciones <- predict(modelos[[1]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[2]] <- lm(Sales ~ Radio, data=pruebaData) 
predicciones <- predict(modelos[[2]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Radio", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[3]] <- lm(Sales ~ Newspaper, data=pruebaData) 
predicciones <- predict(modelos[[3]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[4]] <- lm(Sales ~ TV + Radio, data=pruebaData) 
predicciones <- predict(modelos[[4]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV + Radio", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[5]] <- lm(Sales ~ TV + Radio, data=pruebaData) 
predicciones <- predict(modelos[[5]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV + Neswpaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[6]] <- lm(Sales ~ Radio + Newspaper, data=pruebaData) 
predicciones <- predict(modelos[[6]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Radio + Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[7]] <- lm(Sales ~ TV + Radio + Newspaper, data=pruebaData) 
predicciones <- predict(modelos[[7]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV + Radio + Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

print("Errores de cada modelo:")
print(erroresModelos)

mejorModeloIndexMAE <- which.min(erroresModelos$MAE)
mejorModeloNombreMAE <- erroresModelos$Model[mejorModeloIndexMAE]
mejorModeloMAE <- modelos[[mejorModeloIndexMAE]]

mejorModeloIndexMSE <- which.min(erroresModelos$MSE)
mejorModeloNombreMSE <- erroresModelos$Model[mejorModeloIndexMSE]
mejorModeloMSE <- modelos[[mejorModeloIndexMSE]]

mejorModeloIndexMAPE <- which.min(erroresModelos$MAPE)
mejorModeloNombreMAPE <- erroresModelos$Model[mejorModeloIndexMAPE]
mejorModeloMAPE <- modelos[[mejorModeloIndexMAPE]]

cat("\n El mejor modelo basado en MAE es:", mejorModeloNombreMAE, "\n")
cat("MAE:", erroresModelos$MAE[mejorModeloIndexMAE], "\n")

cat("\n El mejor modelo basado en MSE es:", mejorModeloNombreMSE, "\n")
cat("MSE:", erroresModelos$MSE[mejorModeloIndexMSE], "\n")

cat("\n El mejor modelo basado en MAPE es:", mejorModeloNombreMAPE, "\n")
cat("MAPE:", erroresModelos$MAPE[mejorModeloIndexMAPE], "\n")
















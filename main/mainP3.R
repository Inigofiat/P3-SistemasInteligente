library(lattice)
library(ggplot2)
library(caret)
library(dplyr)


rm(list=ls())
cat("\014")

if(!is.null(dev.list())) dev.off()
graphics.off()

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("linear-regression-utils.R")

data <- read.csv("../data/2025_Advertising.csv")

contarNulosYVacios <- function(data) {
  conteos <- numeric(length(colnames(data)))
  names(conteos) <- colnames(data)

  for(col in colnames(data)) {
    if(is.character(data[[col]])) {
      conteos[col] <- sum(is.na(data[[col]]) |
                            data[[col]] == "" |
                            tolower(data[[col]]) == "null")
    } else {
      conteos[col] <- sum(is.na(data[[col]]))
    }
  }

  return(conteos)
}

procesarDataset <- function(data) {
  contNullVac <- contarNulosYVacios(data)
  print("Cantidad de valores nulos y vacíos por columna:")
  print(contNullVac)

  for(col in colnames(data)) {
    if(is.character(data[[col]])) {
      data[[col]][data[[col]] == "" | tolower(data[[col]]) == "null"] <- NA
    }

    if(any(is.na(data[[col]]))) {
      valorComun <- names(sort(table(data[[col]], useNA = "no"), decreasing = TRUE))[1]

      if(is.numeric(data[[col]])) {
        valorComun <- as.numeric(valorComun)
      }

      data[[col]][is.na(data[[col]])] <- valorComun
      cat("Columna", col, "- Valores nulos/vacíos reemplazados con:", valorComun, "\n")
    }
  }

  return(data)
}

data <- procesarDataset(data)

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

set.seed(123) #para reproducibilidad
pruebasIndices <- createDataPartition(y=data$Sales, p=0.75, list=FALSE)
pruebaData <- data[pruebasIndices,]
testData <- data[-pruebasIndices,]

cat("\nTamaño del conjunto de entrenamiento:", nrow(pruebaData), "observaciones\n")
cat("Tamaño del conjunto de prueba:", nrow(testData), "observaciones\n")

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

modelos[[5]] <- lm(Sales ~ TV + Newspaper, data=pruebaData)
predicciones <- predict(modelos[[5]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV + Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[6]] <- lm(Sales ~ Radio + Newspaper, data=pruebaData)
predicciones <- predict(modelos[[6]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Radio + Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[7]] <- lm(Sales ~ TV + Radio + Newspaper, data=pruebaData)
predicciones <- predict(modelos[[7]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV + Radio + Newspaper", MAE = errores$MAE, MSE = errores$MSE, MAPE = errores$MAPE))

modelos[[8]] <- lm(Sales ~ TV * Radio * Newspaper, data=pruebaData)
predicciones <- predict(modelos[[8]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "TV * Radio * Newspaper (Interacciones)", 
                                                   MAE = errores$MAE, 
                                                   MSE = errores$MSE, 
                                                   MAPE = errores$MAPE))

modelos[[9]] <- lm(Sales ~ TV + I(TV^2) + Radio + I(Radio^2) + Newspaper + I(Newspaper^2), 
                    data=pruebaData)
predicciones <- predict(modelos[[9]], testData)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Polinómico (términos cuadráticos)", 
                                                   MAE = errores$MAE, 
                                                   MSE = errores$MSE, 
                                                   MAPE = errores$MAPE))

pruebaDataLog <- pruebaData
testDataLog <- testData

pruebaDataLog$TV <- log(pruebaDataLog$TV + 1)
pruebaDataLog$Radio <- log(pruebaDataLog$Radio + 1)
pruebaDataLog$Newspaper <- log(pruebaDataLog$Newspaper + 1)
testDataLog$TV <- log(testDataLog$TV + 1)
testDataLog$Radio <- log(testDataLog$Radio + 1)
testDataLog$Newspaper <- log(testDataLog$Newspaper + 1)

modelos[[10]] <- lm(Sales ~ TV + Radio + Newspaper, data=pruebaDataLog)
predicciones <- predict(modelos[[10]], testDataLog)
errores <- calculoErrores(testData$Sales, predicciones)
erroresModelos <- rbind(erroresModelos, data.frame(Model = "Log-transformado", 
                                                   MAE = errores$MAE, 
                                                   MSE = errores$MSE, 
                                                   MAPE = errores$MAPE))

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

cat("\nCoeficientes del mejor modelo (MAE):\n")
print(coef(mejorModeloMAE))

cat("\n Significación estadística del modelo (p-value):\n")
print(summary(mejorModeloMAE)$fstatistic)
pvalue <- pf(summary(mejorModeloMAE)$fstatistic[1],
             summary(mejorModeloMAE)$fstatistic[2],
             summary(mejorModeloMAE)$fstatistic[3],
             lower.tail = FALSE)
cat("p-value:", pvalue, "\n")

totalPredicciones <- predict(mejorModeloMAE, data)
errorAbsoluto <- abs(data$Sales - totalPredicciones)
maximoErrorIndex <- which.max(errorAbsoluto)

cat("\nMuestra con mayor error absoluto:\n")
print(data[maximoErrorIndex, ])
cat("Valor real:", data$Sales[maximoErrorIndex], "\n")
cat("Valor predicho:", totalPredicciones[maximoErrorIndex], "\n")
cat("Error absoluto:", errorAbsoluto[maximoErrorIndex], "\n")


data$totalInversion <- data$TV + data$Radio + data$Newspaper
data$ROI <- data$Sales / data$totalInversion

topCampanias <- data %>%
  arrange(desc(ROI)) %>%
  head(10)

cat("\n10 Campañas con más beneficio (mayor relación ventas/inversión):\n")
print(topCampanias[, c("id", "TV", "Radio", "Newspaper", "Sales", "ROI")])


data$Prediccion <- predict(mejorModeloMAE, data)
data$Diferencia <- data$Sales - data$Prediccion

cat("\n10 Campañas más 'sorprendentes'\n")

cat("\n5 Campañas con mayor diferencia 'postiva' entre el valor real y la predicción:\n")
topPositivas <- data %>%
  arrange(desc(Diferencia)) %>%
  head(5)
print(topPositivas[, c("id", "TV", "Radio", "Newspaper", "Sales", "Prediccion", "Diferencia")])

cat("\n5 Campañas con mayor diferencia 'negativa' entre el valor real y la predicción:\n")
topNegativas <- data %>%
  arrange(Diferencia) %>%
  head(5)
print(topNegativas[, c("id", "TV", "Radio", "Newspaper", "Sales", "Prediccion", "Diferencia")])

cat("\nOptimización de presupuesto de 50.000$ (en miles):\n")
presupuesto <- 50
pasos <- 5
minimoPorCanal <- 5

prediccionVentas <- function(tv, radio, newspaper, model) {
  nuevoData <- data.frame(TV = tv, Radio = radio, Newspaper = newspaper)
  return(predict(model, nuevoData))
}

resultados <- data.frame(TV = numeric(), Radio = numeric(), Newspaper = numeric(),
                         PrediccionVentas = numeric(), stringsAsFactors = FALSE)

for (tv in seq(minimoPorCanal, presupuesto - 2*minimoPorCanal, pasos)) {
  for (radio in seq(minimoPorCanal, presupuesto - tv - minimoPorCanal, pasos)) {
    newspaper <- presupuesto - tv - radio
    if (newspaper >= minimoPorCanal) {
      pVentas <- prediccionVentas(tv, radio, newspaper, mejorModeloMAE)
      resultados <- rbind(resultados, data.frame(TV = tv, Radio = radio,
                                           Newspaper = newspaper,
                                           PrediccionVentas = pVentas))
    }
  }
}

mejorPresupuesto <- resultados %>%
  arrange(desc(PrediccionVentas)) %>%
  head(1)

cat("Distribución óptima del presupuesto de 50.000$:\n")
cat("TV:", mejorPresupuesto$TV * 1000, "$\n")
cat("Radio:", mejorPresupuesto$Radio * 1000, "$\n")
cat("Newspaper:", mejorPresupuesto$Newspaper * 1000, "$\n")
cat("Ventas previstas:", mejorPresupuesto$PrediccionVentas * 1000, "$\n")

print(mejorModeloMAE)

cat("\nResumen completo del mejor modelo:\n")
print(summary(mejorModeloMAE))
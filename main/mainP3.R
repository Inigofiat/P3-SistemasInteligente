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
    # Si hay valores NA en la columna
    if(any(is.na(dataframe[[col]]))) {
      # Encontrar el valor más común (moda)
      valorComun <- names(sort(table(dataframe[[col]], useNA = "no"), decreasing = TRUE))[1]
      
      # Convertir a número si la columna es numérica
      if(is.numeric(dataframe[[col]])) {
        valorComun <- as.numeric(valorComun)
      }
      
      # Reemplazar NAs con el valor común
      dataframe[[col]][is.na(dataframe[[col]])] <- valorComun
      
      # Mostrar mensaje
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


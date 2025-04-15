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
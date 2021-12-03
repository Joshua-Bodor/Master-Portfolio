library(dplyr)
library(rgl)

setwd("D:/Documents/WGU_MSDA/NBM2")

data <- read.csv(file="medical_clean.csv", stringsAsFactors = TRUE)

data_sel <- data[,c(11,12,15:19, 21:42)]
summary(data_sel)

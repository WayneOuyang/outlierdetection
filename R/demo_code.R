library(dtwclust)
library(ggplot2)
library(lubridate)
library(dplyr)
library(quadprog)
source("/Users/wayne/workspace/PCGProject/R/AnormalyDetection.R")
source("/Users/wayne/workspace/PCGProject/R/DetectAnoms.R")
source("/Users/wayne/workspace/PCGProject/R/util.R")

geo <- 'na'
raw_data <- read.csv(paste0('/Users/wayne/workspace/PCG service parts/data/TopmostPN_filter_weekly_consumption(change)/TopmostPN_', geo, '_weekly_consumption_filter(change).csv'), header = TRUE)
raw_data2 <- read.csv(paste0('/Users/wayne/workspace/PCG service parts/data/TopmostPN_ib_filter(change)/TopmostPN_', geo, '_ib_withdate_filter.csv'), header = FALSE)
PN <- '89Y0902'
PN_usage <- raw_data[raw_data$TopmostPN==PN,]
PN_usage$Data <- as.Date(PN_usage$Data)
PN_usage$DataNum <- as.numeric(as.vector(PN_usage$DataNum))
PN_usage <- PN_usage[, c('Data', 'DataNum')]
colnames(PN_usage) <- c('Date', 'Magnitude')
PN_IB <- raw_data2[raw_data2$V1==PN,]
PN_IB$V3 <- as.Date(PN_IB$V3)
PN_IB$V4 <- as.numeric(as.vector(PN_IB$V4))
PN_IB <- PN_IB[, c('V3', 'V4')]
colnames(PN_IB) <- c('Date', 'Magnitude')
res <- Anomaly_Detection(PN_usage, PN_IB, alpha = 0.1)




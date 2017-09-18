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


# dir.create("/Users/wayne/workspace/PCGProject/plot", showWarnings = FALSE)
# dir.create("/Users/wayne/workspace/PCGProject/plot/similarity", showWarnings = FALSE)
# dir.create("/Users/wayne/workspace/PCGProject/plot/similarity/similar", showWarnings = FALSE)
# geo_list <- c('emea', 'ap', 'na', 'las')
# pplt <- list()
# for(geo in geo_list){
#   dir.create(sprintf("/Users/wayne/workspace/PCGProject/plot/similarity/similar/%s", geo), showWarnings = FALSE)
#   raw_data <- read.csv(paste0('/Users/wayne/workspace/PCG service parts/data/TopmostPN_filter_weekly_consumption(change)/TopmostPN_', geo, '_weekly_consumption_filter(change).csv'), header = TRUE)
#   raw_data2 <- read.csv(paste0('/Users/wayne/workspace/PCG service parts/data/TopmostPN_ib_filter(change)/TopmostPN_', geo, '_ib_withdate_filter.csv'), header = FALSE)
#   PN_list_10_data <- read.csv(paste0('/Users/wayne/workspace/PCGProject/baseline_', geo, '.csv'), header = TRUE)
#   PN_list_10 <- as.vector(unique(PN_list_10_data$PN))
#   for(PN in PN_list_10){
#     PN <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", PN)
#     PN_usage <- raw_data[raw_data$TopmostPN==PN,]
#     PN_usage$Data <- as.Date(PN_usage$Data)
#     PN_usage$DataNum <- as.numeric(as.vector(PN_usage$DataNum))
#     PN_usage <- PN_usage[, c('Data', 'DataNum')]
#     colnames(PN_usage) <- c('Date', 'Magnitude')
#     PN_IB <- raw_data2[raw_data2$V1==PN,]
#     PN_IB$V3 <- as.Date(PN_IB$V3)
#     PN_IB$V4 <- as.numeric(as.vector(PN_IB$V4))
#     PN_IB <- PN_IB[, c('V3', 'V4')]
#     colnames(PN_IB) <- c('Date', 'Magnitude')
#     PN_IB <- PN_IB[index(PN_IB) >= ss.estimate(PN_IB),]
#     PN_IB <- PN_IB[PN_IB[[1L]] <= PN_usage[[1L]][dim(PN_usage)[1]],]
#     PN_usage <- PN_usage[index(PN_usage) >= ss.estimate(PN_usage),]
#     query <- reinterpolate(PN_IB[[2L]], new.length = round(dim(PN_IB)[1]*4.34524))
#     query <- zscore(query)
#     PN_usage[[2L]] <- lowess(PN_usage[[2L]], f=0.2)$y
#     reference <- reinterpolate(PN_usage[[2L]], new.length = length(query))
#     reference <- zscore(reference)
#     PN_usage[[2L]] <- zscore(PN_usage[[2L]])
#     PN_IB[[2L]] <- zscore(PN_IB[[2L]])
#     score <- lb_improved(reference, query, window.size = 10, norm = 'L2')
#     if(score < 10) {
#       plot_title <-  paste(round(score, digits=2), "score", sep="")
#       pplt <- ggplot(PN_IB, aes(Date, Magnitude)) + geom_line(aes(color="Important line")) + geom_point(aes(color="My points"))
#       pplt <- pplt + geom_line(data=PN_usage, aes(Date, Magnitude), colour = 'black')
#       pplt <- pplt + theme_bw()
#       pplt <- pplt + labs(x="date", y="usage", title=plot_title)
#       pplt <- pplt + theme(legend.position="none")
#       ggsave(filename=sprintf("%s.PNG", PN), plot=pplt, path = sprintf("/Users/wayne/workspace/PCGProject/plot/similarity/similar/%s", geo), dpi = 300)
#
#     }
#   }
#
#
# }




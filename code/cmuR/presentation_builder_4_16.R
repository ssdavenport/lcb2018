rm(list=ls())
setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(reshape2) #melt
library(ggplot2)


breakdown = read.csv("issdp_breakdown_csv.csv")

breakdownTranspose = as.data.frame(matrix(NA,nrow = 5,ncol=7))
names(breakdownTranspose) = c('type','total_observation','price_observations','weight_thc_observations',
                              'expenditure','usable_weight','thc_weight')
breakdownTranspose[,1] <- c('Usable','Extract','Solid','Liquid','Other')
breakdownTranspose[,2] <- as.numeric(as.matrix(breakdown[1,2:6]))
breakdownTranspose[,3] <- as.numeric(as.matrix(breakdown[2,2:6]))
breakdownTranspose[,4] <- as.numeric(as.matrix(breakdown[3,2:6]))
breakdownTranspose[,5] <- as.numeric(as.matrix(breakdown[4,2:6]))
breakdownTranspose[,6] <- as.numeric(as.matrix(breakdown[5,2:6]))
breakdownTranspose[,7] <- as.numeric(as.matrix(breakdown[8,2:6]))

#convert to %
breakdownTransposePCT = breakdownTranspose

for(i in 2:ncol(breakdownTransposePCT)){
  breakdownTransposePCT[,i] <-  round(breakdownTransposePCT[,i]/sum(breakdownTransposePCT[,i]),4)
}


#thc excluded from other in thc_weight

plotter <- melt(breakdownTransposePCT, id.var="type")

#plot
plotter = ggplot(plotter, aes(x = variable, y = value, fill = type)) + 
  geom_bar(stat = "identity") +
  ylab("Percentage") +
  xlab("Type of Measurement") +
  ggtitle("Breakdown of Type of Marijuana") +
  theme(plot.title = element_text(hjust = 0.5,size=30), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20))


axa = c(97.58297, 0.48307, -0.18297, 97.91215, 1.24099, -0.80786, 0.14868, 98.00992, 3.11211,-2.14072,  0.41294, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0)

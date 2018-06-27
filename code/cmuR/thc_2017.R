#clear
rm(list=ls())

#setwd
setwd("~/CMU_Y2/RAND")

#imports
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

#options
options(scipen=999)

#import data ####

#name of data
dataName ='thc_24_28_all.csv'

#data
thcOverall = read.csv(dataName)

names(thcOverall) = toupper(names(thcOverall))

#factor adjustment
thcOverall$D_INVENTORYTYPE = as.factor(as.character(thcOverall$D_INVENTORYTYPE))

#thctons
thcOverall$THCTONS = thcOverall$THCGRAMS/1000000

#assign terms to levels
levels(thcOverall$D_INVENTORYTYPE) <- c("Marijuana Extract for Inhalation","Usable Marijuana")  

#names renmae
names(thcOverall) <- c("THCGRAMS","Inventory_Type","MONTH","YEAR","THCTONS")
thcOverall$TIME = lubridate::ymd(paste0(thcOverall$YEAR,"/",thcOverall$MONTH,"/01"))

thcOverall = thcOverall[ order(thcOverall[,'YEAR'], thcOverall[,'MONTH']),]

#trim off 2016
thcOverall = thcOverall[thcOverall$YEAR==2016,]

#trim off last month
thcOverall = thcOverall[1:(nrow(thcOverall)-2),]

#plot
axa = ggplot(data=thcOverall, aes(x=TIME, y=THCTONS,fill=Inventory_Type,group=Inventory_Type)) +
  geom_bar(stat="identity") + 
  xlab("Time : January 2016 - Febuary 2017") +
  ylab("Monthly Total of THC sold, Metric Tons") +
  ggtitle("Total THC in Metric Tons Sold of Usable Marijuana and Marijuana Extract for Inhalation") +
  theme(plot.title = element_text(size=22, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  scale_fill_brewer(palette="Set1") +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face="bold", size=15),
        legend.title=element_text(size=20),
        legend.text =element_text(size=15) )
axa

#general counts
counter = read.csv("disp_total_counts.csv")
counter$per = counter$f0_/sum(counter$f0_)

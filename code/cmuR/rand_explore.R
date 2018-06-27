#clear
rm(list=ls())

#setwd
setwd("~/CMU_Y2/RAND")

#imports
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

#data
toyRaw = read.csv('retail_toy.csv')

#basic facts
dim(toyRaw) #100000     48

#focus on Usable Marijuana - largest section 
toyFlower = toyRaw[toyRaw$invtype=='Usable Marijuana',]

#for consumption factors: weight, useable weight, price, THC, CBD, year-month-mday-hour,gram_price_x

#better time
toyFlower$time = lubridate::ymd_hms(paste0(as.character(toyFlower$year),"-", #year
                                         as.character(toyFlower$month),"-", #month
                                         as.character(toyFlower$mday)," ",   #day
                                         as.character(toyFlower$hour),":00:00")) #hour

#monthly mean
smallFlower = toyFlower %>% 
  group_by(Year=year, Month=month) %>%
  summarise(weightMean=mean(weight,na.rm=TRUE),
    usableweightMean=mean(usableweight,na.rm=TRUE),
    priceMean=mean(price,na.rm=TRUE),
    THCMean=mean(THC,na.rm=TRUE),
    CBDMean=mean(CBD,na.rm=TRUE),
    gramMean=mean(gram_price_x,na.rm=TRUE))

smallFlower = as.data.frame(smallFlower)
smallFlower$time = smallFlower$Year+smallFlower$Month/12

#for dev
#smallFlower = dplyr::sample_n(toyFlower, round(nrow(toyFlower)/10,0))

#plot
weightPlot = ggplot(data = smallFlower,aes(x=time)) +
  geom_line(aes(y=usableweightMean),colour="red",size=2,linetype=1) +
  geom_line(aes(y=priceMean),colour="darkgreen",size=2,linetype=1)+
  geom_line(aes(y=gramMean),colour="purple",size=2,linetype=1)+
  ggtitle('Monthly Mean of Price, Gram Price, and Useable Weight for Usable Marijuana(Flower)') +
  ylab("Absolute Number") +
  xlab("Time - Years and Months") +
  geom_vline(xintercept = 2017)

weightPlot

#notes - weight has random spikes
#usableweight - seems to have better images 



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

#isolates things of interest
toyTHC = toyRaw[,c('usableweight','THC','year','month','invtype','price')]

#summarize average THC and average Price in toyTHC
THCPRICE <- toyTHC %>% 
  select(THC,price,invtype,year,month) %>%
  dplyr::group_by(year,month,invtype) %>%
  dplyr::summarise(avgTHC = mean(THC,na.rm=TRUE),
                avgPRICE= mean(price,na.rm=TRUE),
                Count_of_Transactions= n())

THCPRICE = as.data.frame(THCPRICE)
THCPRICE

#adds averageprice/averageTHC
THCPRICE$avgTHCoverPrice=THCPRICE$avgTHC/THCPRICE$avgPRICE

#adds time
THCPRICE$time = THCPRICE$year+THCPRICE$month/12-1/12

#gets the counts
measure_count_holder <- toyTHC %>%
  select(invtype) %>%
  dplyr::group_by(invtype) %>%
  dplyr::summarise(measure_count = n()) 

measure_count_holder = measure_count_holder[order(-measure_count_holder$measure_count),c(1,2)]
measure_count_holder = as.data.frame(measure_count_holder)
measure_count_holder$measure_percent = paste0(as.character(100*round(measure_count_holder$measure_count/sum(measure_count_holder$measure_count),4))," %")
measure_count_holder

#eliminate Capsule and Tincture from analysis, too few too track
THCPRICE = filter(THCPRICE, invtype != 'Capsule', invtype != 'Tincture')

#rename
names(THCPRICE)[names(THCPRICE)=="invtype"] = "Type_of_Inventory"

#ggplot2

axa = ggplot(data=THCPRICE,aes(x=time,y=avgTHCoverPrice,colour=Type_of_Inventory,size=Count_of_Transactions)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2017) +
  ggtitle("Washington State: Average THC(%) / Average Price($) for Each Product Type") +
  theme(plot.title = element_text(size=30, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  ylab("Average THC(%) / Average Price($)") +
  xlab("Time")
  
axa

#bxb average THC/ average PRICE OVERALL no catergories
allTHC <- toyTHC %>% 
  select(THC,price,invtype,year,month) %>%
  dplyr::group_by(year,month) %>%
  dplyr::summarise(avgTHC = mean(THC,na.rm=TRUE),
                   avgPRICE= mean(price,na.rm=TRUE),
                   Count_of_Transactions= n())

allTHC = as.data.frame(allTHC)

#adds averageprice/averageTHC
allTHC$avgOverall=allTHC$avgTHC/allTHC$avgPRICE

#adds time
allTHC$time = allTHC$year+allTHC$month/12-1/12

#average THC/ average PRICE OVERALL no catergories
bxb = ggplot(data=allTHC,aes(x=time,y=avgOverall,size=Count_of_Transactions)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2017) +
  ggtitle("Washington State: Average THC(%) / Average Price($)")+
  theme(plot.title = element_text(size=30, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  ylab("Average THC(%) / Average Price($)") +
  xlab("Time")

bxb

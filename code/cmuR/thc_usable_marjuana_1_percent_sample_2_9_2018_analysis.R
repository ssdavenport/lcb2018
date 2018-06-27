#clear
rm(list=ls())

#setwd
setwd("~/CMU_Y2/RAND")

#imports
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

#import data ####

#name of data
dataName ='mgturner_2_10_2018_thc_useable_marijuana.csv'

#data
thcRaw = read.csv(dataName)

print(dim(thcRaw))
# D = from dispensing table, P = from sample_potency table

#D_ID = integer
#D_SESSIONTIME = integer
#D_TRANSACTIONID = integer
#D_PRICE = numeric, float
#D_USABLEWEIGHT = numeric, float
#D_INVENTORYTYPE = integer, all 28 = useable marjuana 
#P_NAME = factor, all THC 
#P_Value = numeric, float
#P_LAB_PROVIDED = integer

#rename for standardized all caps

names(thcRaw) = toupper(names(thcRaw))

#drop P_NAME and D_INVENTORYTYPE - no information
thcRaw = thcRaw[,(!(names(thcRaw) %in% c("P_NAME","D_INVENTORYTYPE")))]

#convert D_sessiontime to mdy_hms - time originally in Unix 32-bit integer timestamp
thcRaw$D_SESSIONTIME = as.POSIXct(as.numeric(as.character(thcRaw$D_SESSIONTIME)),origin="1970-01-01")

#adds columns for year,month,week
thcRaw$YEAR = year(thcRaw$D_SESSIONTIME)
thcRaw$MONTH = month(thcRaw$D_SESSIONTIME)
thcRaw$WEEK = week(thcRaw$D_SESSIONTIME)

#data alterations ####

#implement price adjustment - for loop only used as progress indicator - mapply works by itself
#setup information

taxConversionTime = ymd_hms("2015-07-01 00:00:00")
thcRaw$D_PRICE <- ifelse(thcRaw$D_SESSIONTIME>=taxConversionTime,thcRaw$D_PRICE*1.37,thcRaw$D_PRICE)

#outlier removal
#identify prices that are in the top and bottom quartile for each month and year

#idnetifies months and years of service
outlierFrame = as.data.frame(unique(thcRaw[,c('YEAR','MONTH')]))
outlierFrame = outlierFrame[ order(outlierFrame[,'YEAR'], outlierFrame[,'MONTH']), ]
rownames(outlierFrame) <- NULL
outlierFrame$one_q = 0
outlierFrame$ninenine_q = 0

#populates with 1% and 99% thresholds 
for(i in 1:nrow(outlierFrame)){
  outlierFrame[i,c('one_q','ninenine_q')] = (thcRaw %>% 
    select(YEAR,MONTH,D_PRICE) %>%
    filter(YEAR==outlierFrame$YEAR[i] & MONTH==outlierFrame$MONTH[i]) %>%
    summarise(one = quantile(x=D_PRICE,probs = c(0.01)),
              ninenine = quantile(x=D_PRICE,probs = c(0.99))))
}

#purges data frame of rows that are above and threshold 
for(i in 1:nrow(outlierFrame)){
  thcRaw = thcRaw[!(thcRaw$YEAR == outlierFrame$YEAR[i] & thcRaw$MONTH == outlierFrame$MONTH[i] & (thcRaw$D_PRICE <= outlierFrame$one_q[i]| thcRaw$D_PRICE >= outlierFrame$ninenine_q[i])),]
}

#price per gram generation
thcRaw$PRICE_PER_GRAM = thcRaw$D_PRICE/thcRaw$D_USABLEWEIGHT

#eliminate rows of THC that are too high - 100 for now, but remove the rest
thcRaw = thcRaw[thcRaw$P_VALUE <=100.00,]

#graphics

#thcRaw - thc density
thcDensity = ggplot(data=thcRaw, aes(x=P_VALUE)) +
  geom_density(fill="darkgreen") +
  ggtitle("Washington State, Usable Marijuana: Average THC(%) Density from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=30, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("Average THC(%)") +
  geom_vline(xintercept = mean(thcRaw$P_VALUE))

thcDensity

#Total THC concentration / Total Useable Weight
ratioTHCFrame = thcRaw %>%
  select(YEAR,MONTH,D_PRICE,D_USABLEWEIGHT) %>%
  group_by(YEAR,MONTH) %>%
  summarise(monthRatio = sum(D_PRICE)/sum(D_USABLEWEIGHT))

ratioTHCFrame = as.data.frame(ratioTHCFrame)
ratioTHCFrame$time = lubridate::ymd(paste0(ratioTHCFrame$YEAR,"/",ratioTHCFrame$MONTH,"/01"))


ratioPlot = ggplot(data = ratioTHCFrame, aes(x=time, y=monthRatio)) +
  geom_rect(data = ratioTHCFrame, mapping=aes(ymin= 0,
                                              ymax = max(averageTHCFrame$monthRatio),
                                              xmin=lubridate::ymd("2015-08-01"), xmax=lubridate::ymd("2015-11-01"), 
                                              fill="2015/08/01\n        -      \n2015/11/01"), alpha=0.25)+
  geom_line(size=3) +
  ggtitle("Washington State, Usable Marijuana: Average THC(%) to Usable Weight Ratio from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=25, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("Date") +
  ylab("Monthly Total THC Concentration/Total Usable Weight") +
  guides(fill=guide_legend(title="Missing Data"))
  
ratioPlot

#for each product, THC concentration/ Total Useable Weight
thcRaw2 = thcRaw
thcRaw2$itemAverage = thcRaw2$P_VALUE/thcRaw2$D_USABLEWEIGHT

averageTHCFrame = thcRaw2 %>%
  select(YEAR,MONTH,D_PRICE,itemAverage) %>%
  group_by(YEAR,MONTH) %>%
  summarise(monthRatio = mean(itemAverage))

averageTHCFrame = as.data.frame(averageTHCFrame)
averageTHCFrame$time = lubridate::ymd(paste0(averageTHCFrame$YEAR,"/",averageTHCFrame$MONTH,"/01"))

averagePlot = ggplot(data = averageTHCFrame, aes(x=time, y=monthRatio)) +
  geom_rect(data = averageTHCFrame, mapping=aes(ymin= 0,
                                              ymax = max(averageTHCFrame$monthRatio),
                                              xmin=lubridate::ymd("2015-08-01"), xmax=lubridate::ymd("2015-11-01"), 
                                              fill="2015/08/01\n        -      \n2015/11/01"), alpha=0.25)+
  geom_point(size=4) +
  geom_line(size=2.5) +
  ggtitle("Washington State, Usable Marijuana: Average THC(%) to Usable Weight Mean from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=25, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("Date") +
  ylab("Monthly Mean of THC Concentration/Usable Weight for all Items") +
  guides(fill=guide_legend(title="Missing Data"))

averagePlot
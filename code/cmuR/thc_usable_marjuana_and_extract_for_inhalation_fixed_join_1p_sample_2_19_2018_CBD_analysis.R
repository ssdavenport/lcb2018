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
dataName ='cbd.csv'

#data
thcOverall = read.csv(dataName)

print(dim(thcOverall))
print(summary(as.factor(thcOverall$d_INVENTORYTYPE)))

thcRaw = thcOverall

#thcRaw = 28 = useable marijuana
#thcExtract = 24 = marijuana extract for inhalation

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

#USABLE MARIJUANA ##############################

#rename for standardized all caps

names(thcRaw) = toupper(names(thcRaw))

#drop P_NAME and D_INVENTORYTYPE - no information
thcRaw = thcRaw[,(!(names(thcRaw) %in% c("P_NAME")))]

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
thcDensity = ggplot(data=thcRaw[thcRaw$D_INVENTORYTYPE==28,], aes(x=P_VALUE)) +
  geom_density(data=thcRaw[thcRaw$D_INVENTORYTYPE==28,], fill = "steelblue",aes(x=P_VALUE), alpha=0.33) +
  geom_density(data=thcRaw[thcRaw$D_INVENTORYTYPE==24,], fill= "darkgreen", aes(x=P_VALUE), alpha=0.33) +
  ggtitle("Washington State, Usable Marijuana and Marijuana for Extract: CBD Density from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=20, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("CBD (%)") +scale_fill_brewer(palette="Spectral") +
  ylab("Density") +
  xlim(c(0,5))

#scale_colour_manual(name="Area Color",values=c(myline1="red", myline2="blue"))

thcDensity

#AVERAGE THC CONTRACTION
averageFrame = thcRaw %>%
  select(YEAR,MONTH,P_VALUE,D_INVENTORYTYPE) %>%
  group_by(YEAR,MONTH,D_INVENTORYTYPE) %>%
  summarise(monthRatio = mean(P_VALUE,na.rm=TRUE),
            monthRatio_m2sd = mean(P_VALUE,na.rm=TRUE)-2*sd(P_VALUE,na.rm=TRUE),
            monthRatio_p2sd = mean(P_VALUE,na.rm=TRUE)+2*sd(P_VALUE,na.rm=TRUE))

averageFrame = as.data.frame(averageFrame)
averageFrame$time = lubridate::ymd(paste0(averageFrame$YEAR,"/",averageFrame$MONTH,"/01"))

averageFrame = averageFrame[!is.nan(averageFrame$monthRatio_m2sd),]

averagePlot = ggplot(data = averageFrame, aes(x=time, colour=as.factor(D_INVENTORYTYPE),  group=D_INVENTORYTYPE)) +
  geom_rect(data = averageFrame, mapping=aes(ymin= min(averageFrame$monthRatio_m2sd),
                                             ymax = max(averageFrame$monthRatio_p2sd),
                                             xmin=lubridate::ymd("2015-08-01"), xmax=lubridate::ymd("2015-11-01"), 
                                             colour="grey50"), alpha=0.25) +
  geom_line(aes(y=monthRatio),size=3) +
  geom_line(aes(y=monthRatio_m2sd),size=1) +
  geom_line(aes(y=monthRatio_p2sd),size=1) +
  ggtitle("Washington State, Usable Marijuana and Extract for Inhalation: Mean CBD from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=20, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("Date") +
  ylab("Monthly Mean of CBD Concentration (%)") 

averagePlot + scale_color_brewer(palette="Accent")
#Total THC concentration / Total Useable Weight
#do not include 
ratioTHCFrame = thcRaw %>%
  select(YEAR,MONTH,P_VALUE,D_USABLEWEIGHT,D_INVENTORYTYPE) %>%
  group_by(YEAR,MONTH,D_INVENTORYTYPE) %>%
  summarise(monthRatio = sum(P_VALUE)/sum(D_USABLEWEIGHT))

ratioTHCFrame = as.data.frame(ratioTHCFrame)
ratioTHCFrame$time = lubridate::ymd(paste0(ratioTHCFrame$YEAR,"/",ratioTHCFrame$MONTH,"/01"))


ratioPlot = ggplot(data = ratioTHCFrame, aes(x=time, y=monthRatio, colour=as.factor(D_INVENTORYTYPE),  group=D_INVENTORYTYPE)) +
  geom_rect(data = ratioTHCFrame, mapping=aes(ymin= 0,
                                              ymax = max(ratioTHCFrame$monthRatio),
                                              xmin=lubridate::ymd("2015-08-01"), xmax=lubridate::ymd("2015-11-01"), 
                                              colour="grey50"), alpha=0.25)+
  geom_line(size=3) +
  ggtitle("Washington State, Usable Marijuana and Extract for Inhalation: Sum of Total THC(%) to Sum of Usable Weight Ratio from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=15, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) +
  xlab("Date") +
  ylab("Monthly Weighted Total of THC Concentration, Weighted By Total Usable Weight") +
  guides(fill=guide_legend(title="Missing Data"))

ratioPlot

# total usable weight
uwFrame = as.data.frame(thcRaw %>%
                          select(YEAR,MONTH,P_VALUE,D_USABLEWEIGHT,D_INVENTORYTYPE,D_USABLEWEIGHT) %>%
                          group_by(YEAR,MONTH,D_INVENTORYTYPE) %>%
                          summarise(total_usable_weight = sum(D_USABLEWEIGHT)))

tuw = ggplot(data = uwFrame,aes(x=ymd(paste0(as.character(uwFrame$YEAR),"/",
                                             as.character(uwFrame$MONTH),"/01"))
                                ,y=total_usable_weight,group = as.factor(D_INVENTORYTYPE),
                                colour = as.factor(D_INVENTORYTYPE))) +
  
  geom_rect(data = uwFrame, mapping=aes(ymin= 0,ymax = max(uwFrame$total_usable_weight),
                                        xmin=lubridate::ymd("2015-08-01"), 
                                        xmax=lubridate::ymd("2015-11-01"), 
                                        colour="grey50"), alpha=0.25) +
  
  geom_line(size =2.5) +
  xlab("Time") +
  ylab("Total Usable Weight - grams") + 
  ggtitle("Washington State, Usable Marijuana and Extract for Inhalation: Total Usable Weight from October 1,2015 - March 30,2017") +
  theme(plot.title = element_text(size=20, hjust = 0.5),axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))

tuw
tuw + scale_color_brewer(palette="Accent")

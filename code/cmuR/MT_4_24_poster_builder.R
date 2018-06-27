rm(list=ls())
setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(ggthemes)

options(scipen = 7)

#gather all MT data
MT_UM = read.csv('MT_4_24_usable_marijuana_weight_thc.csv')
MT_X = read.csv('MT_4_24_extract_weight_thc.csv')

MT = rbind(MT_UM,MT_X)

#gather all AV data
AV = read.csv('AV_4_24_edibles_weight_thc.csv')

#time converter
yearC = function(timeInput){
  year = as.integer(substr(timeInput,1,4))
  return(year)
}

monthC = function(timeInput){
  month = as.integer(substr(timeInput,6,nchar(timeInput)))
  return(month)
}

#convert
AV$Time = as.character(AV$Time)

AV$Month =mapply(monthC,AV$Time)
AV$Year = mapply(yearC,AV$Time)

#totals - join back together on Year, Month
AV2 = as.data.frame(AV %>% group_by(Year,Month) %>% summarize(invtype = 'Marijuana Infused Edible',
                                                thc = sum(THC),
                                                uw = sum(UW)))


#merger
MT = MT[,c('invtype','YEAR','MONTH','totalTHCGrams','UsableWeight')]
names(MT) = c('invtype','year','month','thc','uw')

AV2 = AV2[, c('invtype','Year','Month','thc','uw')]
names(AV2) =  c('invtype','year','month','thc','uw')

mtav = rbind(MT,AV2)

#convert names 
mtav$invtype = as.character(mtav$invtype)

#develope time interval
mtav$time = mtav$year+mtav$month/12

#remove liquids 

#reshape
mtavGraph = melt(mtav, id.vars = "invtype", measure.vars = c("thc"))

#rejoin times
mtavGraph= as.data.frame(left_join(mtavGraph, mtav[,c('thc','time')], by = c("value" = "thc")))
mtavGraphHolder = mtavGraph

#snip off times before and after issdp year
#((year(sale_time) = 2016 and month(sale_time) >= 7) OR (year(sale_time) = 2017 and month(sale_time) <= 6))

mtavGraph = mtavGraph[mtavGraph$time>=2016.583 & mtavGraph$time<=2017.5,]

names(mtavGraph)[1] = 'Product_Type'

#THC plot

xLabels = c('7/16','8/16','9/16','10/16','11/16','12/16','1/17','2/17','3/17','4/17','5/17','6/17')

axa = ggplot(data= mtavGraph, aes(x=time,y=value/1000000,fill=Product_Type)) +
  geom_area(stat='identity') +
  theme_minimal() + 
  theme(text = element_text(size=20)) +
  scale_color_discrete(name = "Tier") +
  scale_x_continuous(limits = c(2016.583,2017.5),
                     breaks=seq(2016.583,2017.5,(1/12)),
                     labels=xLabels) +
  xlab('Time') +
  ylab('Metric Tons of THC') +
  theme(plot.title = element_text(hjust = 0.5) , legend.position=c(.80,.33)) +
  guides(fill=guide_legend(title="Product Type")) +
  scale_colour_tableau("tableau10medium") +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.background = element_rect(fill="white"))

axa

#THC Percentages
THCArrayMin = mtavGraph[mtavGraph$time==min(mtavGraph$time),c('Product_Type','value')]
THCArrayMin$name = 'min'
THCArrayMin[3,'value'] = 23706

THCArrayMin$pct = round(THCArrayMin$value/sum(THCArrayMin$value),4)*100

THCArrayMax = mtavGraph[mtavGraph$time==max(mtavGraph$time),c('Product_Type','value')]
THCArrayMax$name = 'max'

THCArrayMax[3,'value'] = 29885
THCArrayMax$pct = round(THCArrayMax$value/sum(THCArrayMax$value),4)*100

THCArray = rbind(THCArrayMin,THCArrayMax)
THCArray

#Will be suing: 
#July 2016 UW 27557.92 G; 
#June 2017 UW 34002.27 G; 
#July 2016 THC 16914+789+5519+484 
#= 23706 
#June 2017 THC  = 21928+1725+5606+626
#=  29885

######## here are the THCs ########  #min - july, max = june

THCArray

#total THC
sum(mtavGraph$value)

#### usable weight

#reshape
mtavGraph = melt(mtav, id.vars = "invtype", measure.vars = c("uw"))

#rejoin times
mtavGraph= as.data.frame(left_join(mtavGraph, mtav[,c('uw','time')], by = c("value" = "uw")))
mtavGraphHolder = mtavGraph

#snip off times before and after issdp year
#((year(sale_time) = 2016 and month(sale_time) >= 7) OR (year(sale_time) = 2017 and month(sale_time) <= 6))

mtavGraph = mtavGraph[mtavGraph$time>=2016.583 & mtavGraph$time<=2017.5,]

names(mtavGraph)[1] = 'Product_Type'

#THC plot

xLabels = c('7/16','8/16','9/16','10/16','11/16','12/16','1/17','2/17','3/17','4/17','5/17','6/17')

bxb = ggplot(data= mtavGraph, aes(x=time,y=value/1000000,fill=Product_Type)) +
  geom_area(stat='identity') +
  theme_minimal() + 
  theme(text = element_text(size=20)) +
  scale_color_discrete(name = "Tier") +
  scale_x_continuous(limits = c(2016.583,2017.5),
                     breaks=seq(2016.583,2017.5,(1/12)),
                     labels=xLabels) +
  xlab('Time') +
  ylab('Metric Tons of Usable Weight') +
  theme(plot.title = element_text(hjust = 0.5) , legend.position=c(.80,.33)) +
  guides(fill=guide_legend(title="Product Type")) +
  scale_colour_tableau("tableau10medium") +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.background = element_rect(fill="white"))

bxb

#UW Percentages
THCArrayMin = mtavGraph[mtavGraph$time==min(mtavGraph$time),c('Product_Type','value')]
THCArrayMin$name = 'min'

THCArrayMin[3 ,'value'] = 27557.92

THCArrayMin$pct = round(THCArrayMin$value/sum(THCArrayMin$value),4)*100

THCArrayMax = mtavGraph[mtavGraph$time==max(mtavGraph$time),c('Product_Type','value')]
THCArrayMax$name = 'max'

THCArrayMin[3 ,'value'] = 34002.27 

THCArrayMax$pct = round(THCArrayMax$value/sum(THCArrayMax$value),4)*100

THCArray = rbind(THCArrayMin,THCArrayMax)

######## here are the Usable Weight ########  #min - july, max = june

THCArray

#total usable weight
sum(mtavGraph$value)

#Will be suing: 
#July 2016 UW 27557.92 G; 
#June 2017 UW 34002.27 G; 
#July 2016 THC 16914.4+ 78908 +5519.958 + 484.7154 
#= 101827.1; 
#June 2017 THC  = 21928.33716 + 1725.2 +5606.09 + 626.51 
#=  29886.1

rm(list=ls())
setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(ggthemes)

options(scipen = 7)

#input raw data
MT = read.csv('retail_2017_weight_THC.csv')
AV = read.csv('AV_THC_NUMBERS.csv')

#month conversion
monthC = function(mText){
  if(mText=='January'){
    mText=1    
  } else if(mText =='February') {
    mText=2    
  } else if(mText =='March') {
    mText=3    
  } else if(mText =='April') {
    mText=4    
  } else if(mText =='May') {
    mText=5
  } else if(mText =='June') {
    mText=6    
  } else if(mText =='July') {
    mText=7    
  } else if(mText =='August') {
    mText=8    
  } else if(mText =='September') {
    mText=9    
  } else if(mText =='October') {
    mText=10
  } else if(mText =='November') {
    mText=11    
  } else if(mText =='December') {
    mText=12
  } else {
    print('Error!')
    print(mText)
    break
  }
  return(mText)
}

#convert
AV$Month = sapply(as.character(AV$Month),monthC)

#totals
AV$total = AV$THC_THCDominant_Grams+AV$THC_CBDDominant_Grams

#merger
names(MT) = c('invtype','year','month','counter', 'uw' ,'thc')
names(AV) = c('year','month','invtype','thc_only','cbd_only','thc')
mtav = rbind(MT[,c('invtype','year','month','thc')],AV[,c('invtype','year','month','thc')])

#convert names 
mtav$invtype = as.character(mtav$invtype)

#develope time interval
mtav$time = mtav$year+mtav$month/12

#formalize names
mtav[mtav$invtype=='Liquid','invtype'] = 'Marijuana Infused Edible'
mtav[mtav$invtype=='Solid','invtype'] = 'Marijuana Infused Edible'

#add edibles together
mtavEdibles = as.data.frame(mtav[mtav$invtype=='Marijuana Infused Edible',] %>% group_by(invtype,time) %>% summarize(thc = sum(thc)))
mtavElse = mtav[mtav$invtype!='Marijuana Infused Edible',c('invtype','time','thc')]

mtav = rbind(mtavEdibles,mtavElse)

#remove liquids 
#mtav  = mtav[mtav$invtype!='Liquid Marijuana Infused Edible',]

#reshape
mtavGraph = melt(mtav, id.vars = "invtype", measure.vars = c("thc"))

#rejoin times
mtavGraph= as.data.frame(left_join(mtavGraph, mtav[,c('thc','time')], by = c("value" = "thc")))
mtavGraphHolder = mtavGraph

#snip off times before and after issdp year

mtavGraph = mtavGraph[mtavGraph$time>=2016.583 & mtavGraph$time<=2017.5,]

names(mtavGraph)[1] = 'Product_Type'

#plot

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
  ggtitle('Increases in Production of THC in Washington State Over Time') +
  theme(plot.title = element_text(hjust = 0.5) , legend.position=c(.80,.33)) +
  guides(fill=guide_legend(title="Product Type")) +
  ggthemes::scale_colour_ptol('cyl') +
  ggthemes::scale_fill_ptol('cyl') +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.background = element_rect(fill="white"))

axa





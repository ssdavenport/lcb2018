#set ups 
rm(list=ls())

setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

options(scipen = 7)

#input data
counter = read.csv('retail_2017_weight_THC.csv')

#data cleaning
counter$DATE = lubridate::ymd(paste0(as.character(counter$YEAR),"-",as.character(counter$MONTH),"-01"))

#indicators of types of product
counter$extract = if_else(as.character(counter$invtype)=='Marijuana Extract for Inhalation',1,0)
counter$usable = if_else(as.character(counter$invtype)=='Usable Marijuana',1,0)

#time cleaning
lrCounter = counter

#trim before october 2014 - market to erratic
lrCounter =lrCounter[lrCounter$DATE>=lubridate::ymd(as.character("2014-10-01")),]

#trim november and december 2017 - insufficient data
lrCounter =lrCounter[lrCounter$DATE<lubridate::ymd(as.character("2017-11-01")),]

#factor
lrCounter$invtype = as.factor(lrCounter$invtype)

#### linear regression model data builder ####
usable = counter[counter$usable==1,]
extract = counter[counter$extract==1,]

#makes prediction frame
lrPredict = lrCounter[0,]

#add in november and december
lrPredict[1,] = c('Marijuana Extract for Inhalation',2017,11, NA,NA,NA,"2017-11-01",1,0)
lrPredict[2,] = c('Marijuana Extract for Inhalation',2017,12, NA,NA,NA,"2017-12-01",1,0)

lrPredict[3,] = c('Usable Marijuana',2017,11, NA,NA,NA,"2017-11-01",0,1)
lrPredict[4,] = c('Usable Marijuana',2017,12, NA,NA,NA,"2017-12-01",0,1)

#converts time to time
lrPredict$DATE = lubridate::ymd(lrPredict$DATE)
lrPredict$invtype = as.factor(lrPredict$invtype )

#tap on
lrCounter = rbind(lrCounter,lrPredict)

#### feature engineering for train annd test sets ####

#time from start of market
elapsed_months <- function(end_date, start_date) {
  ed <- ymd(as.character(end_date))
  sd <- ymd(as.character(start_date))
  return(12 * (year(sd) - year(ed)) + (month(sd) - month(ed)))
}

#number of months elasped
lrCounter$sinceStart = elapsed_months("2014-7-01", as.character(lrCounter$DATE))

#mean of several months prior data function
preMonth= function(numMon,date,type,input){  #input - counter, UsableWeight, totalTHC,
  answer = (mean(as.numeric(lrCounter[lrCounter$DATE>=date-months(numMon) & 
                                      lrCounter$DATE<date & 
                                      lrCounter$invtype==type,input]), na.rm=TRUE))
  
  if(is.nan(answer)){
    return(NA)
  } else {
    return(answer)
  }
  
}

#preMonth to conduct feature engineering for all 1:12 months 
for(i in paste0("month",1:12)){
  for(typeHolder in c('counter','UsableWeight','totalTHCGrams')){
  lrCounter[,ncol(lrCounter)+1] <- mapply(preMonth,as.numeric(substr(i,6,nchar(i))),
                          lrCounter$DATE,
                          lrCounter$invtype,
                          typeHolder)
  names(lrCounter)[ncol(lrCounter)] = paste0(i,"_",typeHolder)
  }
}

#corrects formating
lrCounter$YEAR = as.numeric(lrCounter$YEAR)
lrCounter$MONTH = as.numeric(lrCounter$MONTH )

#order
lrCounter = lrCounter[ order(lrCounter$invtype,lrCounter$YEAR,lrCounter$MONTH), ]

##spilt out train and prediction sets 
trainUsable = lrCounter[!is.na(lrCounter$counter) & as.character(lrCounter$invtype) ==("Usable Marijuana"),]
predUsable = lrCounter[is.na(lrCounter$counter) & as.character(lrCounter$invtype) =="Usable Marijuana",]

trainExtract = lrCounter[!is.na(lrCounter$counter) & as.character(lrCounter$invtype) =="Marijuana Extract for Inhalation",]
predExtract = lrCounter[is.na(lrCounter$counter) & as.character(lrCounter$invtype) =="Marijuana Extract for Inhalation",]

##### build models #####

## build model  ##

#counter - dispensing rows 
rm(fitUsableCounter)
fitUsableCounter = stats::lm(counter ~ month3_counter, data=trainUsable)
summary(fitUsableCounter)

rm(fitExtractCounter)
fitExtractCounter = stats::lm(counter ~ month3_counter, data=trainExtract)
summary(fitExtractCounter)

#usable weight
rm(fitUsableWeight)
fitUsableWeight = stats::lm(UsableWeight ~ month3_UsableWeight, data=trainUsable)
summary(fitUsableWeight)

rm(fitExtractWeight)
fitExtractWeight = stats::lm(UsableWeight ~ month3_UsableWeight, data=trainExtract)
summary(fitExtractWeight)

#THC
rm(fitUsableTHC)
fitUsableTHC = stats::lm(totalTHCGrams ~ month3_totalTHCGrams, data=trainUsable)
summary(fitUsableTHC)

rm(fitExtractTHC)
fitExtractTHC = stats::lm(totalTHCGrams ~ month3_totalTHCGrams, data=trainExtract)
summary(fitExtractTHC)

#prediction assignment into unknowns
#counts
predUsable$counter = predict(fitUsableCounter,predUsable)
predExtract$counter = predict(fitExtractCounter,predExtract)

#usable weights
predUsable$UsableWeight = predict(fitUsableWeight,predUsable)
predExtract$UsableWeight = predict(fitExtractWeight,predExtract)

#THC
predUsable$totalTHCGrams = predict(fitUsableTHC,predUsable)
predExtract$totalTHCGrams = predict(fitExtractTHC,predExtract)

#### updated model and combine models  ####

#prediction indicator
trainUsable$predicted="NO"
trainExtract$predicted="NO"
predUsable$predicted="YES"
predExtract$predicted="YES"

lrCounterFinal = rbind(trainUsable,trainExtract,predUsable,predExtract)

#plots
#row dispensing plot
counterPlot = ggplot(data=lrCounterFinal, aes(x=as.POSIXct(DATE),y=as.numeric(counter),colour=invtype))+
  geom_vline(xintercept=as.POSIXct(ymd("2017-12-01")),size=30,color="grey75") +
  geom_point(size=1.5) +
  geom_line() +
  xlab("Time") +
  ylab("Retail Dispensed Counts, By Month") +
  ggtitle("Dispensing Counts : Usable Marijuana and Marijuana Extract Counts for All Months - Including Prediction") +
  theme(plot.title = element_text(hjust = 0.5))

counterPlot

#usable weight plot
weightPlot = ggplot(data=lrCounterFinal, aes(x=as.POSIXct(DATE),y=as.numeric(UsableWeight)/1000000,colour=invtype))+
  geom_vline(xintercept=as.POSIXct(ymd("2017-12-01")),size=30,color="grey75") +
  geom_point(size=1.5) +
  geom_line() +
  xlab("Time") +
  ylab("Retail Dispensed Counts, By Month") +
  ggtitle("Usable Weight Metric Tons : Usable Marijuana and Marijuana Extract for All Months - Including Prediction") +
  theme(plot.title = element_text(hjust = 0.5))

weightPlot

#thc plot
thcPlot = ggplot(data=lrCounterFinal, aes(x=as.POSIXct(DATE),y=as.numeric(totalTHCGrams)/1000000,colour=invtype))+
    geom_vline(xintercept=as.POSIXct(ymd("2017-12-01")),size=30,color="grey75") +
    geom_point(size=1.5) +
    geom_line() +
    xlab("Time") +
    ylab("Retail Dispensed Counts, By Month") +
    ggtitle("Total THC Metric Tons : Usable Marijuana and Marijuana Extract for All Months - Including Prediction") +
    theme(plot.title = element_text(hjust = 0.5))
 
thcPlot



#number holder - generates the numbers for the tables in the memo
result = data.frame(year=rep(c(2015,2016,2017),2),
                    type=as.character(c(rep('Usable Marijuana',3),rep('Marijuana Extract for Inhalation',3))),
                    count = 0,
                    weight = 0,
                    thc = 0)

#format correction
levels(result$type) = c("Marijuana Extract for Inhalation","Usable Marijuana","Total" )
result$count = as.numeric(result$count)
result$weight = as.numeric(result$weight)
result$thc = as.numeric(result$thc)

#counts
result[ 1, "count"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"counter"]))
result[ 2, "count"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"counter"]))
result[ 3, "count"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"counter"]))

result[ 4, "count"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"counter"]))
result[ 5, "count"] =sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"counter"]))
result[ 6, "count"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"counter"]))


#usable weight - metric tons 
result[ 1, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"UsableWeight"]))/1000000
result[ 2, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"UsableWeight"]))/1000000
result[ 3, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"UsableWeight"]))/1000000

result[ 4, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"UsableWeight"]))/1000000
result[ 5, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"UsableWeight"]))/1000000
result[ 6, "weight"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"UsableWeight"]))/1000000

#thc -metric tons 
result[ 1, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"totalTHCGrams"]))/1000000
result[ 2, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"totalTHCGrams"]))/1000000
result[ 3, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Usable Marijuana',"totalTHCGrams"]))/1000000

result[ 4, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2015& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"totalTHCGrams"]))/1000000
result[ 5, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2016& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"totalTHCGrams"]))/1000000
result[ 6, "thc"] = sum(as.numeric(lrCounterFinal[lrCounterFinal$YEAR==2017& as.character(lrCounterFinal$invtype)=='Marijuana Extract for Inhalation',"totalTHCGrams"]))/1000000


#totals
result[7,] = c(2015,('Total'),sum(as.numeric(result[c(1,4),"count"])),sum(as.numeric(result[c(1,4),"weight"])),sum(as.numeric(result[c(1,4),"thc"])))
result[8,] = c(2016,('Total'),sum(as.numeric(result[c(2,5),"count"])),sum(as.numeric(result[c(2,5),"weight"])),sum(as.numeric(result[c(2,5),"thc"])))
result[9,] = c(2017,('Total'),sum(as.numeric(result[c(3,6),"count"])),sum(as.numeric(result[c(3,6),"weight"])),sum(as.numeric(result[c(3,6),"thc"])))

result$count = round(as.numeric(result$count),0)
result$count = as.numeric(result$count)
result$weight = as.numeric(result$weight)
result$thc = as.numeric(result$thc)

#growth indicator
pointer = "count"
result$countShare = c(NA,
                      (result[,pointer][2]-result[,pointer][1])/result[,pointer][1]*100,
                      (result[,pointer][3]-result[,pointer][2])/result[,pointer][2]*100,
                      NA,
                      (result[,pointer][5]-result[,pointer][4])/result[,pointer][4]*100,
                      (result[,pointer][6]-result[,pointer][5])/result[,pointer][5]*100,
                      NA,
                      (result[,pointer][8]-result[,pointer][7])/result[,pointer][7]*100,
                      (result[,pointer][9]-result[,pointer][8])/result[,pointer][8]*100)

pointer = "weight"
result$weightShare = c(NA,
                      (result[,pointer][2]-result[,pointer][1])/result[,pointer][1]*100,
                      (result[,pointer][3]-result[,pointer][2])/result[,pointer][2]*100,
                      NA,
                      (result[,pointer][5]-result[,pointer][4])/result[,pointer][4]*100,
                      (result[,pointer][6]-result[,pointer][5])/result[,pointer][5]*100,
                      NA,
                      (result[,pointer][8]-result[,pointer][7])/result[,pointer][7]*100,
                      (result[,pointer][9]-result[,pointer][8])/result[,pointer][8]*100)

pointer = "thc"
result$thcShare = c(NA,
                       (result[,pointer][2]-result[,pointer][1])/result[,pointer][1]*100,
                       (result[,pointer][3]-result[,pointer][2])/result[,pointer][2]*100,
                       NA,
                       (result[,pointer][5]-result[,pointer][4])/result[,pointer][4]*100,
                       (result[,pointer][6]-result[,pointer][5])/result[,pointer][5]*100,
                       NA,
                       (result[,pointer][8]-result[,pointer][7])/result[,pointer][7]*100,
                       (result[,pointer][9]-result[,pointer][8])/result[,pointer][8]*100)

print(result)


############ THC Threshold analysis

#answers key question about what threshold should be used for the 
thcX = read.csv("extract_weight.csv") #17282451 total, 405524 no usable weights ~2%
thcX_nonNA = thcX[!is.na(thcX$UW),] #16876927 = - 405524
thcX_less7 = thcX_nonNA [thcX_nonNA$UW<=7.0, ] #16855906 = -426545, but only 21021 above 7.0
thcX_less7_above0 =thcX_less7[thcX_less7$UW>0.0000,] # 16854571 = 1335 with zero (impossible)
thcXOrder = thcX_less7_above0[order(thcX_less7_above0$UW),]

#amount elimianted under the 0.1 threshold
sum(thcXOrder[thcXOrder$UW<0.1, "items"]) #42218 = 0.002442825 of orignal


#plot showing the counts of weights between 0 and 1 grams
#counts aggreated to the nearest ten thousandth
small_plot = ggplot(data=thcXOrder[thcXOrder$UW<=1.0 &thcXOrder$items<=6295,],
                    aes(x=UW,y=items)) +
  geom_point() +
  geom_vline(xintercept = 0.1, colour="red")+
  ylab("Count of Items Dispensed at Rounded Weight") +
  xlab("Usable Weight, Rounded to Nearest Thousandth") + 
  ggtitle("Usable Weights of Marijuana Extract for Inhalation between Zero and One Grams") +
  theme(plot.title = element_text(hjust = 0.5, size = 25))

small_plot

small_plot_2 = ggplot(data=thcXOrder[thcXOrder$UW<=1.0 &thcXOrder$items<=6295,],
                      aes(x=UW,y=items)) +
  geom_col() +
  geom_vline(xintercept = 0.1, colour="red")+
  ylab("Count of Items Dispensed at Rounded Weight") +
  xlab("Usable Weight, Rounded to Nearest Thousandth") + 
  ggtitle("Usable Weights of Marijuana Extract for Inhalation between Zero and One Grams") +
  theme(plot.title = element_text(hjust = 0.5, size = 25))

small_plot_2




############ end ################

#### other features - unused ####
#months squared
lrCounter$sinceStartSquared = lrCounter$sinceStart **2


#economic, unemployment_rate and labor_force 
econ = read.csv("washington_economy_facts.csv")
econ$date = ymd(paste0(econ$YEAR,"-",econ$MONTH,"-","01"))

lrCounter = dplyr::left_join(x=lrCounter,y=econ,by="date")

#april indicator - 4/20
lrCounter$April = if_else(lrCounter$MONTH==4,1,0)

#Adjusted R-squared:  0.8632

#features explored and dropped
#
#Coefficients:           Estimate  Std. Error t value  Pr(>|t|) 
#sinceStartSquared       -21.06      532.23  -0.040    0.96854
#UNEMPLOYMENT_RATE       -163547     632057  -0.259    0.79658
#LABOR_FORCE             0.4852      5.8221   0.083    0.9338 
#MONTH                    10722      15651    0.685    0.496 
#YEAR                    -128662     187816  -0.685    0.496
#April                   -17736      198588  -0.089    0.929  



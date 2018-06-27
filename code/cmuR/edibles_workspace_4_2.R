rm(list=ls())
setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
source('CMU_Code_share_av.R')

#developement
edibles = read.csv("AD_edibles2017_columnsabbreviated100k.csv")
edibles$id = 1:nrow(edibles)
edibles$inv_productname= as.character(edibles$inv_productname)

#subset data frame for speed
microEdibles = edibles

#run UW extraction function - Davenport
edibles_ExtractionUsableWeight <- search_productname(microEdibles$inv_productname)

#merge onto dataset
microEdibles <- cbind(microEdibles, edibles_ExtractionUsableWeight$mg_text_max)
microEdibles = as.data.frame(microEdibles)

#read nmaes
names(microEdibles)[11] <- "UsableWeight"

#fix to size
microEdibles$UsableWeight <- microEdibles$UsableWeight / 1000 #converts to grams 

#potency correction
#need to make this consistent in above procedures??
microEdibles$Potency_THC <- ifelse(microEdibles$Potency_THC > 100, 100, microEdibles$Potency_THC)

#input contents - grams of THC in each product
microEdibles$Content_THC <- microEdibles$UsableWeight * microEdibles$Potency_THC/100
microEdibles$Content_CBD <- microEdibles$UsableWeight * microEdibles$Potency_CBD/100

#feature engineering
microEdibles$sale_time <- as.numeric(as.POSIXct(as.character(microEdibles$sale_time)))
microEdibles$pricetime <- (microEdibles$sale_time - mean(microEdibles$sale_time)) * microEdibles$price_x



#liquid
liquid = microEdibles[microEdibles$inventorytype==23,]
liquidWeightSum = sum(liquid$UsableWeight, na.rm=TRUE)
liquidTHCSum = sum(liquid$Content_THC, na.rm=TRUE)

liquidNARow = nrow(liquid[(is.na(liquid$UsableWeight)),])
liquidNonNARow = nrow(liquid[!(is.na(liquid$UsableWeight)),])
liquidRow = nrow(liquid)

liquidWeight19703 = liquidWeightSum * liquidRow/liquidNonNARow
liquidTHC19703 = liquidTHCSum * liquidRow/liquidNonNARow 


liquidWeight967833 = liquidWeight19703 * 967833/19703 #total weight = 61,175 grams, 
liquidTHC967833 = liquidTHC19703 * 967833/19703 #total THC = 23,515 grams,

#solid#
solid = microEdibles[microEdibles$inventorytype==22,]
solidWeightSum = sum(solid$UsableWeight, na.rm=TRUE)
solidTHCSum = sum(solid$Content_THC, na.rm=TRUE)

#row counts
solidNARow = nrow(solid[(is.na(solid$UsableWeight)),])
solidNonNARow = nrow(solid[!(is.na(solid$UsableWeight)),])
solidRow = nrow(solid)

#for the dataset
solidWeight34168 = solidWeightSum * solidRow/solidNonNARow
solidTHC34168 = solidTHCSum * solidRow/solidNonNARow 

#for all
solidWeight4003433 = solidWeight34168 * 4003433/34168 #total weight = 614,515 grams, 
solidTHC4003433 = solidTHC34168 * 4003433/34168 #total THC = 41,069 grams,



##talking points 
#what am I doing wrong, why is the weight so much higher than the THC?
#what really is usable weight - grams? if so why does it consistently lower
#why a mixed model, rather than two seperate models
#the imputed data is predicted to have much higher THC content. WHY?

### text process and training data - ignore ####
microEdiblesTrain = microEdibles[!(is.na(microEdibles$Content_THC)),]

#cleaning
microEdiblesTrain$inv_productname = tolower(microEdiblesTrain$inv_productname)

#find all unique terms
unique_words = c()

#for(i in 1:nrow(microEdiblesTrain)){
for(i in 1:100){
  splitter = c(unlist(strsplit(microEdiblesTrain$inv_productname[i]," ")))
  print(splitter)
  increment = 0
  for(s in splitter){
    #remove punctations
    s = gsub("[[:punct:]]", " ", s)
    
    #split again
    s = unlist(strsplit(s, " "))

    #for each item
    for(x in s){
    
      #trim ws    
      x =trimws(x)  

      #append to list if not in list
      if (!(x %in% unique_words)){
        unique_words=c(unique_words,x)
        }
      }
    }
}

print(unique_words)
print(length(unique_words))

unique_words = unique_words[unique_words!=""]

for(z in unique_words){
  zSplit = strsplit(z,"")
  #checks each character
  for(y in zSplit){
    if (y %in% c("0","1","2","3","4","5","6","7","8","9")){
      print(z)
    }
    break
  }
  
}

units = c("mg","cgg","mg","pack","cmc","cpm","hsd","hpb","sdc","htc","sfc","spm","ifc","icb","idc","icc","hwt","hmp","ibb","scb","mg","igb")





#set ups 
rm(list=ls())

setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

options(scipen = 7)

#import time datasets, year and month
usable = read.csv("AR_4_29_usable.csv")
usableParent = read.csv("AR_4_29_usable_mult_parent.csv")
usableAllCounter = read.csv("AR_4_29_usable_all_count.csv")
extract = read.csv("AR_4_29_extract.csv")
edible = read.csv("AR_4_29_edibles.csv")


#meta
months = 6 #for zoo lagging
timeHolder = "2017-10-01" #for AR models 

#### usables ####

#merge usable and usable missing parent counts
usableAll = as.data.frame(dplyr::inner_join(x=usable, y= usableParent[,c("YEAR","MONTH","count_of_mult_parent")], by=c("YEAR","MONTH")))

#merge in total counts
usableAll = as.data.frame(dplyr::inner_join(x=usableAll, y= usableAllCounter[,c("YEAR" ,"MONTH","count_all")],by=c("YEAR","MONTH")))

#find the percentage of excluded parents over all entries
usableAll$mult = usableAll$count_all/(usableAll$count_all-usableAll$count_of_mult_parent)

#find revised thc gram amounts
usableAll$thcFinal = usableAll$totalTHCGrams* usableAll$mult

#convert to time frame
usableAll$time = ymd(paste0(as.character(usableAll$YEAR),"/",as.character(usableAll$MONTH),"/","01"))

#reduce to columns of interest
usableAllFinalResults = usableAll[,c("time","thcFinal")]
#usableAllFinalResults$thcFinal = round(usableAllFinalResults$thcFinal,0)
names(usableAllFinalResults)[2] = "thc"

#snip off months where we are not interest in studying
offenders = c("2014-07-01","2014-08-01", "2014-09-01", "2017-11-01")
offenders = as.Date(offenders)

usableAllFinalResults = usableAllFinalResults[!(usableAllFinalResults$time %in% offenders),]

#zoo holder - lag up to 1 year
#months = 6
usableLagFrame = as.data.frame(matrix(nrow=nrow(usableAllFinalResults),ncol=months))

for(i in 1:months){
  names(usableLagFrame)[i] = paste0("lag_",i)
  usableLagFrame[,i] = lag(usableAllFinalResults$thc,i)
}

#append lags
usableAllFinal = as.data.frame(cbind(usableAllFinalResults,usableLagFrame))



#### usable machine learning ####

#snip off those that cannot be used for traiin
usableAllFinal = usableAllFinal[complete.cases(usableAllFinal),]

#develop autoregressive models and capture BICs , revised for Fstatistics 
bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(bicHolder) = c("months","usable_BIC")
bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(usableAllFinal)[2], "~",
                             paste(colnames(usableAllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=usableAllFinal))
  
  bicHolder$usable_BIC[i] = BIC(axa)
  
}

bicHolder[bicHolder$usable_BIC==min(bicHolder$usable_BIC),]

# for usable marijuana - i will use 1 lag period and extrapolate off.

#### extract ####
extractOG = extract

extractOG$time = ymd(paste0(as.character(extractOG$YEAR),"/",as.character(extractOG$MONTH),"/","01"))

#reduce to columns of interest
extractAll = extractOG[,c("time","totalTHCGrams")]
#extractAll$thcFinal = round(extractAll$totalTHCGrams,0)
names(extractAll)[2] = "thc"

#snip off months where we are not interest in studying (including the 2014-06???)
offenders = c("2014-06-01","2014-07-01","2014-08-01", "2014-09-01", "2017-11-01")
offenders = as.Date(offenders)

extractAll = extractAll[!(extractAll$time %in% offenders),]
extractAll = extractAll[,!(names(extractAll) %in% c("thcFinal"))]

#zoo holder - lag up to 1 year
#months = 6
extractLagFrame = as.data.frame(matrix(nrow=nrow(extractAll),ncol=months))

for(i in 1:months){
  names(extractLagFrame)[i] = paste0("lag_",i)
  extractLagFrame[,i] = lag(extractAll$thc,i)
}

#append lags
extractAllFinal = as.data.frame(cbind(extractAll,extractLagFrame))

extractAllFinal= extractAllFinal[,!(names(extractAllFinal)%in% c("thcFinal"))]

extractAllFinalAll = extractAllFinal


##snip off those that cannot be used for traiin
extractAllFinal = extractAllFinal[complete.cases(extractAllFinal),]

#develop autoregressive models and capture BICs , revised for Fstatistics 
bicHolderExtract = as.data.frame(matrix(ncol=2,nrow=months))
names(bicHolderExtract) = c("months","usable_BIC")
bicHolderExtract$months = 1:months

for(i in 1:months){
  upper = i + 2
  bxb = (lm(as.formula(paste(colnames(extractAllFinal)[2], "~",
                             paste(colnames(extractAllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=extractAllFinal))
  
  bicHolderExtract$usable_BIC[i] = BIC(bxb)
}

#### solid edbibles ####
edible2 = edible

edible2 = edible2[as.character(edible2$ProductType) %in% c("SolidTHCDominant","SolidCBDDominant"),]

timeBuilder = function(string){
  string = as.character(string)
  y = substr(string,1,4)
  m = substr(string,6,7)
  ymdHolder = paste0(y,"/",m,"/01")
  return(ymdHolder)
}

#apply time conversion
edible2$time = mapply(timeBuilder,edible2$Month)

#reduce to the month
edible3  = as.data.frame(edible2 %>% group_by(time) %>% summarise(thc = sum(THCGrams)))

#start in December 2014 for edibles.  In october there are 54.36 grams of THC predicted and zero predicted for november 2014;
#the market is not mature enough at this point to model reliably

#convert to time variable
edible3$time = ymd(edible3$time)

#add in lagged times
#zoo holder - lag up to 1 year
#months = 12
ediblesLagFrame = as.data.frame(matrix(nrow=nrow(edible3),ncol=months))

for(i in 1:months){
  names(ediblesLagFrame)[i] = paste0("lag_",i)
  ediblesLagFrame[,i] = lag(edible3$thc,i)
}

#append lags
ediblesAllFinal = as.data.frame(cbind(edible3,ediblesLagFrame))

#edible offenders
offendersEdibles = c("2014/10/01")
offendersEdibles = as.Date(offendersEdibles)

edible4 = ediblesAllFinal[!(ediblesAllFinal$time %in% offendersEdibles),]
solidEdibleLarge = edible4
solidEdible4 = edible4


#autregressive model for edibles

#snip off those that cannot be used for traiin
solidEdibleAnalysis = solidEdible4[complete.cases(solidEdible4),]

#develop autoregressive models and capture BICs , revised for Fstatistics 
bicHolderEdibles = as.data.frame(matrix(ncol=2,nrow=months))
names(bicHolderEdibles) = c("months","usable_BIC")
bicHolderEdibles$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(solidEdibleAnalysis)[2], "~",
                             paste(colnames(solidEdibleAnalysis)[c(3:upper)], collapse = "+"),
                             sep = "")),data=solidEdibleAnalysis))
  
  bicHolderEdibles$usable_BIC[i] = BIC(axa)
  
}

bicHolderEdibles[bicHolderEdibles$usable_BIC==min(bicHolderEdibles$usable_BIC),]

#### liquid edbibles ####
edible2 = edible

edible2 = edible2[as.character(edible2$ProductType) %in% c("LiquidTHCDominant","LiquidCBDDominant"),]


#apply time conversion
edible2$time = mapply(timeBuilder,edible2$Month)

#reduce to the month
edible3  = as.data.frame(edible2 %>% group_by(time) %>% summarise(thc = sum(THCGrams)))

#start in December 2014 for edibles.  In october there are 54.36 grams of THC predicted and zero predicted for november 2014;
#the market is not mature enough at this point to model reliably

#convert to time variable
edible3$time = ymd(edible3$time)

#add in lagged times
#zoo holder - lag up to 1 year
#months = 12
ediblesLagFrame = as.data.frame(matrix(nrow=nrow(edible3),ncol=months))

for(i in 1:months){
  names(ediblesLagFrame)[i] = paste0("lag_",i)
  ediblesLagFrame[,i] = lag(edible3$thc,i)
}

#append lags
ediblesAllFinal = as.data.frame(cbind(edible3,ediblesLagFrame))

#edible offenders
offendersEdibles = c("2014/10/01")
offendersEdibles = as.Date(offendersEdibles)

edible4 = ediblesAllFinal[!(ediblesAllFinal$time %in% offendersEdibles),]
liquidEdibleLarge = edible4
liquidEdible4 = edible4

#autregressive model for liquid edibles

#snip off those that cannot be used for traiin
liquidEdibleAnalysis = liquidEdible4[complete.cases(liquidEdible4),]

#develop autoregressive models and capture BICs , revised for Fstatistics 
bicHolderLiquidEdibles = as.data.frame(matrix(ncol=2,nrow=months))
names(bicHolderLiquidEdibles) = c("months","usable_BIC")
bicHolderLiquidEdibles$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(liquidEdibleAnalysis)[2], "~",
                             paste(colnames(liquidEdibleAnalysis)[c(3:upper)], collapse = "+"),
                             sep = "")),data=liquidEdibleAnalysis))
  
  bicHolderLiquidEdibles$usable_BIC[i] = BIC(axa)
  
}

bicHolderLiquidEdibles[bicHolderLiquidEdibles$usable_BIC==min(bicHolderLiquidEdibles$usable_BIC),]


#### analysis ####

#comparion and analysis
bicHolder[bicHolder$usable_BIC==min(bicHolder$usable_BIC),]
bicHolderExtract[bicHolderExtract$usable_BIC==min(bicHolderExtract$usable_BIC),]
bicHolderEdibles[bicHolderEdibles$usable_BIC==min(bicHolderEdibles$usable_BIC),]
bicHolderLiquidEdibles[bicHolderLiquidEdibles$usable_BIC==min(bicHolderLiquidEdibles$usable_BIC),]

#analysis with more time periods - models 
#um = usables
um = lm(thc~lag_1,data=usableAllFinal) #with at least 6 months
BIC(um)

#em extracts
em = lm(thc~lag_1+lag_2+lag_3,data=extractAllFinal) #with at least 6 months 
BIC(em)

#dm edilbes
dm = lm(thc~lag_1, data=solidEdibleAnalysis) #with at least 6 months 
BIC(dm)

#lm - liquid
liqm = lm(thc~lag_1, data=liquidEdibleAnalysis)
BIC(liqm)



#because of differences in when each of the products were encodable into the database and the neccesity for a 
#size months to have passed before the new month of data can be used each of the models analyszed a different
#period of time based on product type. Usable Marijuana analyzed October 2014 until October 2017.
#Marijuan Extracts for inhalation analyzed December 2015 until October 2017.
#Edibles analysied May 2015 until October 2017.

# november 2017 and december 2017 calculations usables

#nov
umOct = usableAllFinal[usableAllFinal$time==as.Date(timeHolder),]

umNov = umOct
umNov[1:nrow(umNov),1:ncol(umNov)] = NA

umNov$time = as.Date("2017-11-01")
umNov[1,3:ncol(umNov)] = umOct[1,2:(ncol(umOct)-1)]

umNov$thc = predict(um,umNov) #predict and input

#dec
umDec = umNov
umDec[1:nrow(umDec),1:ncol(umDec)] = NA

umDec$time = as.Date("2017-12-01")
umDec[1,3:ncol(umDec)] = umNov[1,2:(ncol(umNov)-1)]
umDec$thc = predict(um,umDec) #predict and input

# november 2017 and december 2017 calculations extracts

#nov
emOct = extractAllFinal[extractAllFinal$time==as.Date(timeHolder),]

emNov = emOct
emNov[1:nrow(emNov),1:ncol(emNov)] = NA

emNov$time = as.Date("2017-11-01")
emNov[1,3:ncol(emNov)] = emOct[1,2:(ncol(emOct)-1)]

emNov$thc = predict(em,emNov) #predict and input

#dec
emDec = emNov
emDec[1:nrow(emDec),1:ncol(emDec)] = NA

emDec$time = as.Date("2017-12-01")
emDec[1,3:ncol(emDec)] = emNov[1,2:(ncol(emNov)-1)]
emDec$thc = predict(em,emDec) #predict and input

# november 2017 and december 2017 calculations solid edibles

#nov
dmOct = solidEdibleAnalysis[solidEdibleAnalysis$time==as.Date(timeHolder),]

dmNov = dmOct
dmNov[1:nrow(dmNov),1:ncol(dmNov)] = NA

dmNov$time = as.Date("2017-11-01")
dmNov[1,3:ncol(dmNov)] = dmOct[1,2:(ncol(dmOct)-1)]

dmNov$thc = predict(dm,dmNov) #predict and input

#dec
dmDec = dmNov
dmDec[1:nrow(dmDec),1:ncol(dmDec)] = NA

dmDec$time = as.Date("2017-12-01")
dmDec[1,3:ncol(dmDec)] = dmNov[1,2:(ncol(dmNov)-1)]
dmDec$thc = predict(dm,dmDec) #predict and input

# november 2017 and december 2017 calculations liquid edibles

#nov
liqmOct = liquidEdibleAnalysis[liquidEdibleAnalysis$time==as.Date(timeHolder),]

liqmNov = liqmOct
liqmNov[1:nrow(liqmNov),1:ncol(liqmNov)] = NA

liqmNov$time = as.Date("2017-11-01")
liqmNov[1,3:ncol(liqmNov)] = liqmOct[1,2:(ncol(liqmOct)-1)]

liqmNov$thc = predict(liqm,liqmNov) #predict and input

#dec
liqmDec = liqmNov
liqmDec[1:nrow(liqmDec),1:ncol(liqmDec)] = NA

liqmDec$time = as.Date("2017-12-01")
liqmDec[1,3:ncol(liqmDec)] = liqmNov[1,2:(ncol(liqmNov)-1)]
liqmDec$thc = predict(liqm,liqmDec) #predict and input

#### presentation table ####

#append predictions
usPres = as.data.frame(rbind(usableAllFinal,umNov,umDec))
emPres = as.data.frame(rbind(extractAllFinal,emNov,emDec))
dmPres = as.data.frame(rbind(solidEdibleAnalysis,dmNov,dmDec))
liqmPres = as.data.frame(rbind(liquidEdibleAnalysis,liqmNov,liqmDec))

#reduce to rows of interents
usPres = usPres[,c("time","thc")]
names(usPres)[2] = "usable"

emPres = emPres[,c("time","thc")]
names(emPres)[2] = "extract"

dmPres = dmPres[,c("time","thc")]
names(dmPres)[2] = "solid_edible"

liqmPres = liqmPres[,c("time","thc")]
names(liqmPres)[2] = "liquid_edible"

#merger
pres = as.data.frame(dplyr::inner_join(x=usPres,y=emPres,by = "time"))
pres = as.data.frame(dplyr::inner_join(x=pres,y=dmPres,by = "time"))
pres = as.data.frame(dplyr::inner_join(x=pres,y=liqmPres,by = "time"))

presOther = pres

#round and metric tonage
for(i in 2:5){
  pres[,i] = round(pres[,i]/1000000,10)
}

#just2017
pres2017 = pres[pres$time>=as.Date("2017-01-01"),]

pres2017$total = pres2017$usable+pres2017$extract+pres2017$edibles
pres2017$time = as.character(pres2017$time)
pres2017$time = substr(pres2017$time,1,7)

productTotal = cbind(("2017"),sum(pres2017$usable),sum(pres2017$extract),sum(pres2017$solid_edible), sum(pres2017$liquid_edible))
productTotal = as.data.frame(productTotal)
names(productTotal) = names(pres2017)

pres2017 = as.data.frame(rbind(pres2017,productTotal))

for(i in 2:5){
  pres2017[,i] = round(as.numeric(pres2017[,i]),3)
}

pres2017 



#### other ####
#merge in grams of marijuana
#names(presOther) =  c("time","thc_usable","thc_extract","thc_solid_edible","thc_liquid_edible")

other = read.csv("AR_5_5_other_count_price.csv")
nonOther = read.csv("AR_5_5_nonOther_count_price.csv")

other$time = ymd(paste0(as.character(other$YEAR),"/",as.character(other$MONTH),"/","01"))
other = as.data.frame(other %>% group_by(time) %>%summarize(price=sum(price_x,na.rm=TRUE)))


nonOther$time = ymd(paste0(as.character(nonOther$YEAR),"/",as.character(nonOther$MONTH),"/","01"))
nonOther$invtype = as.character(nonOther$invtype)
names(nonOther)[names(nonOther)=="price_x"] = "price"

#snip off months where we are not interest in studying
offenders = c("2014-07-01","2014-08-01", "2014-09-01", "2017-11-01")
offenders = as.Date(offenders)

nonOther = nonOther[!(nonOther$time %in% offenders),]
other = other[!(other$time %in% offenders),]

ooo = sum(other[other$time >= as.Date("2017-01-01"),"price"])
noo = sum(nonOther[nonOther$time >= as.Date("2017-01-01"),"price"])
nooooo = ooo + noo

lmie_holder = nonOther[nonOther$invtype=='Liquid Marijuana Infused Edible',c("time","price")]
smie_holder = nonOther[nonOther$invtype=='Solid Marijuana Infused Edible',c("time","price")]
um_holder = nonOther[nonOther$invtype=='Usable Marijuana',c("time","price")]
ex_holder = nonOther[nonOther$invtype=='Marijuana Extract for Inhalation',c("time","price")]
other_holder =other

#### price_AR ####

####lmie_holder
lmie_LagFrame = as.data.frame(matrix(nrow=nrow(lmie_holder),ncol=months))

for(i in 1:months){
  names(lmie_LagFrame)[i] = paste0("lag_",i)
  lmie_LagFrame[,i] = lag(lmie_holder$price,i)
}

#append lags
lmie_AllFinal = as.data.frame(cbind(lmie_holder,lmie_LagFrame))

#repmoves NAs
lmie_AllFinal =lmie_AllFinal[complete.cases(lmie_AllFinal),]

####smie_holder
smie_LagFrame = as.data.frame(matrix(nrow=nrow(smie_holder),ncol=months))

for(i in 1:months){
  names(smie_LagFrame)[i] = paste0("lag_",i)
  smie_LagFrame[,i] = lag(smie_holder$price,i)
}

#append lags
smie_AllFinal = as.data.frame(cbind(smie_holder,smie_LagFrame))

#repmoves NAs
smie_AllFinal =smie_AllFinal[complete.cases(smie_AllFinal),]

####um_holder
um_LagFrame = as.data.frame(matrix(nrow=nrow(um_holder),ncol=months))

for(i in 1:months){
  names(um_LagFrame)[i] = paste0("lag_",i)
  um_LagFrame[,i] = lag(um_holder$price,i)
}

#append lags
um_AllFinal = as.data.frame(cbind(um_holder,um_LagFrame))

#repmoves NAs
um_AllFinal =um_AllFinal[complete.cases(um_AllFinal),]

####ex_holder
ex_LagFrame = as.data.frame(matrix(nrow=nrow(ex_holder),ncol=months))

for(i in 1:months){
  names(ex_LagFrame)[i] = paste0("lag_",i)
  ex_LagFrame[,i] = lag(ex_holder$price,i)
}

#append lags
ex_AllFinal = as.data.frame(cbind(ex_holder,ex_LagFrame))

#repmoves NAs
ex_AllFinal =ex_AllFinal[complete.cases(ex_AllFinal),]

####other_holder
other_LagFrame = as.data.frame(matrix(nrow=nrow(other_holder),ncol=months))

for(i in 1:months){
  names(other_LagFrame)[i] = paste0("lag_",i)
  other_LagFrame[,i] = lag(other_holder$price,i)
}

#append lags
other_AllFinal = as.data.frame(cbind(other_holder,other_LagFrame))

#repmoves NAs
other_AllFinal =other_AllFinal[complete.cases(other_AllFinal),]

#zoo'd : lmie_AllFinal,smie_AllFinal,um_AllFinal,ex_AllFinal,other_AllFinal

#AR predictions 
#develop autoregressive models and capture BICs , revised for Fstatistics 

#lmie_AllFinal
lmie_price_bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(lmie_price_bicHolder) = c("months","lmie_price_BIC")
lmie_price_bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(lmie_AllFinal)[2], "~",
                             paste(colnames(lmie_AllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=lmie_AllFinal))
  
  lmie_price_bicHolder$lmie_price_BIC[i] = BIC(axa)
  
}

lmie_price_bicHolder[lmie_price_bicHolder$lmie_price_BIC==min(lmie_price_bicHolder$lmie_price_BIC),]

#smie_AllFinal
smie_price_bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(smie_price_bicHolder) = c("months","smie_price_BIC")
smie_price_bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(smie_AllFinal)[2], "~",
                             paste(colnames(smie_AllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=smie_AllFinal))
  
  smie_price_bicHolder$smie_price_BIC[i] = BIC(axa)
  
}

smie_price_bicHolder[smie_price_bicHolder$smie_price_BIC==min(smie_price_bicHolder$smie_price_BIC),]

#um_AllFinal
um_price_bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(um_price_bicHolder) = c("months","um_price_BIC")
um_price_bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(um_AllFinal)[2], "~",
                             paste(colnames(um_AllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=um_AllFinal))
  
  um_price_bicHolder$um_price_BIC[i] = BIC(axa)
  
}

um_price_bicHolder[um_price_bicHolder$um_price_BIC==min(um_price_bicHolder$um_price_BIC),]

#ex_AllFinal
ex_price_bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(ex_price_bicHolder) = c("months","ex_price_BIC")
ex_price_bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(ex_AllFinal)[2], "~",
                             paste(colnames(ex_AllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=ex_AllFinal))
  
  ex_price_bicHolder$ex_price_BIC[i] = BIC(axa)
  
}

ex_price_bicHolder[ex_price_bicHolder$ex_price_BIC==min(ex_price_bicHolder$ex_price_BIC),]

#other_AllFinal

other_price_bicHolder = as.data.frame(matrix(ncol=2,nrow=months))
names(other_price_bicHolder) = c("months","other_price_BIC")
other_price_bicHolder$months = 1:months

for(i in 1:months){
  upper = i + 2
  axa = (lm(as.formula(paste(colnames(other_AllFinal)[2], "~",
                             paste(colnames(other_AllFinal)[c(3:upper)], collapse = "+"),
                             sep = "")),data=other_AllFinal))
  
  other_price_bicHolder$other_price_BIC[i] = BIC(axa)
  
}

other_price_bicHolder[other_price_bicHolder$other_price_BIC==min(other_price_bicHolder$other_price_BIC),]

#top of price AR models

lmie_price_bicHolder[lmie_price_bicHolder$lmie_price_BIC==min(lmie_price_bicHolder$lmie_price_BIC),]
smie_price_bicHolder[smie_price_bicHolder$smie_price_BIC==min(smie_price_bicHolder$smie_price_BIC),]
um_price_bicHolder[um_price_bicHolder$um_price_BIC==min(um_price_bicHolder$um_price_BIC),]
ex_price_bicHolder[ex_price_bicHolder$ex_price_BIC==min(ex_price_bicHolder$ex_price_BIC),]
other_price_bicHolder[other_price_bicHolder$other_price_BIC==min(other_price_bicHolder$other_price_BIC),]

#final models 

#analysis with more time periods - models 
#um = usables
um_AR = lm(price~lag_1,data=um_AllFinal) #with at least 6 months
BIC(um_AR)

#ex extracts
ex_AR = lm(price~lag_1,data=ex_AllFinal) #with at least 6 months 
BIC(ex_AR)

#smie edilbes
smie_AR = lm(price~lag_1, data=smie_AllFinal) #with at least 6 months 
BIC(smie_AR)

#lmie - liquid
lmie_AR = lm(price~lag_1, data=lmie_AllFinal)
BIC(lmie_AR)

#other
other_AR  = lm(price~lag_1, data=other_AllFinal)
BIC(other_AR)

#ar models: um_AR,ex_AR, smie_AR,lmie_AR, other_AR

#prediction for imputation for lmie_AllFinal,smie_AllFinal,um_AllFinal,ex_AllFinal,other_AllFinal

#um
#nov
other_price_Oct = other_AllFinal[other_AllFinal$time==as.Date(timeHolder),]

other_price_Nov = other_price_Oct
other_price_Nov[1:nrow(other_price_Nov),1:ncol(other_price_Nov)] = NA

other_price_Nov$time = as.Date("2017-11-01")
other_price_Nov[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= other_price_Oct[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
other_price_Nov$price = predict(other_AR,other_price_Nov) #predict and input

#dec
other_price_Dec = other_price_Nov
other_price_Dec[1:nrow(other_price_Dec),1:ncol(other_price_Dec)] = NA

other_price_Dec$time = as.Date("2017-12-01")
other_price_Dec[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= other_price_Nov[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
other_price_Dec$price = predict(other_AR,other_price_Dec)

#um
#nov
um_price_Oct = um_AllFinal[um_AllFinal$time==as.Date(timeHolder),]

um_price_Nov = um_price_Oct
um_price_Nov[1:nrow(um_price_Nov),1:ncol(um_price_Nov)] = NA

um_price_Nov$time = as.Date("2017-11-01")
um_price_Nov[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= um_price_Oct[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
um_price_Nov$price = predict(um_AR,um_price_Nov) #predict and input

#dec
um_price_Dec = um_price_Nov
um_price_Dec[1:nrow(um_price_Dec),1:ncol(um_price_Dec)] = NA

um_price_Dec$time = as.Date("2017-12-01")
um_price_Dec[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= um_price_Nov[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
um_price_Dec$price = predict(um_AR,um_price_Dec) #predict and input

#ex
#nov
ex_price_Oct = ex_AllFinal[ex_AllFinal$time==as.Date(timeHolder),]

ex_price_Nov = ex_price_Oct
ex_price_Nov[1:nrow(ex_price_Nov),1:ncol(ex_price_Nov)] = NA

ex_price_Nov$time = as.Date("2017-11-01")
ex_price_Nov[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= ex_price_Oct[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
ex_price_Nov$price = predict(ex_AR,ex_price_Nov) #predict and input

#dec
ex_price_Dec = ex_price_Nov
ex_price_Dec[1:nrow(ex_price_Dec),1:ncol(ex_price_Dec)] = NA

ex_price_Dec$time = as.Date("2017-12-01")
ex_price_Dec[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= ex_price_Nov[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
ex_price_Dec$price = predict(ex_AR,ex_price_Dec) #predict and input

#smie
#nov
smie_price_Oct = smie_AllFinal[smie_AllFinal$time==as.Date(timeHolder),]

smie_price_Nov = smie_price_Oct
smie_price_Nov[1:nrow(smie_price_Nov),1:ncol(smie_price_Nov)] = NA

smie_price_Nov$time = as.Date("2017-11-01")
smie_price_Nov[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= smie_price_Oct[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
smie_price_Nov$price = predict(smie_AR,smie_price_Nov) #predict and input

#dec
smie_price_Dec = smie_price_Nov
smie_price_Dec[1:nrow(smie_price_Dec),1:ncol(smie_price_Dec)] = NA

smie_price_Dec$time = as.Date("2017-12-01")
smie_price_Dec[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= smie_price_Nov[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
smie_price_Dec$price = predict(lmie_AR,smie_price_Dec) #predict and input

#lmie
#nov
lmie_price_Oct = lmie_AllFinal[lmie_AllFinal$time==as.Date(timeHolder),]

lmie_price_Nov = lmie_price_Oct
lmie_price_Nov[1:nrow(lmie_price_Nov),1:ncol(lmie_price_Nov)] = NA

lmie_price_Nov$time = as.Date("2017-11-01")
lmie_price_Nov[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= lmie_price_Oct[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
lmie_price_Nov$price = predict(lmie_AR,lmie_price_Nov) #predict and input

#dec
lmie_price_Dec = lmie_price_Nov
lmie_price_Dec[1:nrow(lmie_price_Dec),1:ncol(lmie_price_Dec)] = NA

lmie_price_Dec$time = as.Date("2017-12-01")
lmie_price_Dec[1,c("lag_1","lag_2","lag_3","lag_4","lag_5","lag_6")]= lmie_price_Nov[1,c("price","lag_1","lag_2","lag_3","lag_4","lag_5")]
lmie_price_Dec$price = predict(lmie_AR,lmie_price_Dec) #predict and input

#append
lmie_holder = as.data.frame(rbind(lmie_holder,lmie_price_Nov[,c("time","price")],lmie_price_Dec[,c("time","price")]))
smie_holder =as.data.frame(rbind(smie_holder,smie_price_Nov[,c("time","price")],smie_price_Dec[,c("time","price")]))
um_holder =as.data.frame(rbind(um_holder,um_price_Nov[,c("time","price")],um_price_Dec[,c("time","price")]))
ex_holder = as.data.frame(rbind(ex_holder,ex_price_Nov[,c("time","price")],ex_price_Dec[,c("time","price")]))
other_holder = as.data.frame(rbind(other_holder,other_price_Nov[,c("time","price")],other_price_Dec[,c("time","price")]))

#recombine with names 
names(lmie_holder) = c("time","lmie_price")
names(smie_holder) = c("time","smie_price")
names(um_holder) = c("time","um_price")
names(ex_holder) = c("time","ex_price")
names(other_holder) = c("time","other_price")

#merge on time 
nonOther2 = as.data.frame(dplyr::left_join(x=um_holder,y=ex_holder, by="time"))
nonOther2 = as.data.frame(dplyr::left_join(x=nonOther2,y=smie_holder, by="time"))
nonOther2 = as.data.frame(dplyr::left_join(x=nonOther2,y=lmie_holder, by="time"))
nonOther2 = as.data.frame(dplyr::left_join(x=nonOther2,y=other_holder, by="time"))

#merge in usable weights 
names(presOther) = c("time","thc_usable","thc_extract","thc_solid","thc_liquid")
nonOther2 = as.data.frame(dplyr::left_join(x=nonOther2,y=presOther,by="time"))
nonOther3 = nonOther2

#ratio of spending to marijuana 
nonOther3$um_t_o_p = nonOther3$thc_usable/nonOther3$um_price
nonOther3$ex_t_o_p = nonOther3$thc_extract/nonOther3$ex_price
nonOther3$smie_t_o_p = nonOther3$thc_solid/nonOther3$smie_price
nonOther3$lmie_t_o_p = nonOther3$thc_liquid/nonOther3$lmie_price

#nonOther holds all except for conversions
nonOther4 = nonOther3

nonOther4$other_t_by_um = nonOther4$other_price*nonOther4$um_t_o_p
nonOther4$other_t_by_ex = nonOther4$other_price*nonOther4$ex_t_o_p
nonOther4$other_t_by_smie = nonOther4$other_price*nonOther4$smie_t_o_p
nonOther4$other_t_by_lmie = nonOther4$other_price*nonOther4$lmie_t_o_p

#2017 cut
nonOther4_2017 = nonOther4[nonOther4$time>=as.Date("2017-01-01"),]

# sum(nonOther4_2017$um_price+nonOther4_2017$ex_price+nonOther4_2017$smie_price+nonOther4_2017$lmie_price+nonOther4_2017$other_price)

#### area plot ####
library(reshape2)

areaPlot = pres
nonOther5 = nonOther4[,c("time","other_t_by_um")]
nonOther5$other_t_by_um = nonOther5$other_t_by_um/1000000
areaPlot = as.data.frame(dplyr::inner_join(x=areaPlot,y=nonOther5,by="time"))

areaPlot2 = melt(areaPlot, id.vars ="time",measure.vars=c("usable","extract","solid_edible","liquid_edible", "other_t_by_um"))
names(areaPlot2) = c("time","invtype","value")
areaPlot2$value = as.numeric(areaPlot2$value )
areaPlot2$time = year(areaPlot2$time) +  month(areaPlot2$time)/12

tester = data.frame(invtype=areaPlot2$invtype,
                    value=areaPlot2$value,
                    time=areaPlot2$time)

zzz = ggplot() + geom_area(data=tester, aes(x=time, y=value, group =invtype, fill=invtype)) +
  ylab("Metric Tons of Total THC Per Month") +
  ggtitle("Weight of THC is Rapidly Increasing With Time")
zzz







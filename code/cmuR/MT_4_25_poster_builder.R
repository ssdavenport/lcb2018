rm(list=ls())
setwd("~/CMU_Y2/RAND")

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(ggthemes)

options(scipen = 7)

#develop numbers

devNum = data.frame(Product_Type = c('Marijuana Extracts for Inhalation',
                                     'Marijuana Infused Edible',
                                     'Usable Marijuana'),
                    Expenditure = as.numeric(c(264.6,102.9,763.9)),
                    UW= as.numeric(c(7.58,4.06,85.32)),
                    THC= as.numeric(c(5.352,0.298,18.89)))

#share numbers
devNum[,2] = round(devNum[,2]/sum(devNum[,2]),4)*100
devNum[,3] = round(devNum[,3]/sum(devNum[,3]),4)*100
devNum[,4] = round(devNum[,4]/sum(devNum[,4]),4)*100

#plot results
group.colors <- c("#ec665d","#ff984b", "#6dbc57")

cxc = ggplot(devNum, aes(fill=Product_Type)) + 
  geom_col(aes(x='Expenditure', y= Expenditure), position="stack") +
  #geom_col(aes(x='Usable\n Weight', y= UW), position="stack") +
  geom_col(aes(x='THC', y= THC), position="stack") +
  theme_minimal() + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(size =20),
        axis.text.y = element_text(size= 20),
        legend.key.size = unit(2.5, 'lines'))+
  xlab('Measurement') +
  ylab('Share of Market from July 1,2016 \n until June 30, 2017 (%)') +
  guides(fill=guide_legend(title="Product Type")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.background = element_rect(fill="white")) +
  scale_fill_manual(values=group.colors) +
  theme(legend.position="none")
cxc



#With percentages
posExpenditure = c(mean(c((67.52+9.09),(67.52+9.09+23.39))), 
                   mean(c(67.52,(67.52+9.09))),
                   mean(c(0,67.52)))

cxc = ggplot(devNum, aes(fill=Product_Type)) + 
  geom_col(aes(x='Expenditure', y = Expenditure), position="stack") +
  geom_text(aes(x='Expenditure', y = posExpenditure, label=paste0(Expenditure," %")),size = 15) +
  theme(text = element_text(size=20))

cxc



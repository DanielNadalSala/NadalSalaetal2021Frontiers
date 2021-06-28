#### Authors: Daniel Nadal-Sala

#### Description

#This script plots the meteorological data inside the greenhouse during experiment

#### Libraries

library(dplyr); library(tidyr); library (ggplot2); library(gridExtra); library(cowplot);library (gam)

#### Functions

#### Data path

Directory<-"C:/Users/sala-d/Desktop/Accounting for NonStomatal Limitations/Data/"

#### Data Files

MData<- read.table(file=paste(Directory,"MeteoDriver.csv",sep=""),
                   header=T,sep=";",dec=".")

#################################################

#### Data processing

### To obtain daily Average, maximum and minimum D, PAR and TAIR.

MeteoData<- MData %>% filter(PAR>0) %>% group_by (DOY) %>% summarise(MeanPAR= mean(PAR),
                                                                  MeanT=mean(Tair),
                                                                  MaxT=max(Tair),
                                                                  MinT=min(Tair),
                                                                  MeanD=mean(VPD),
                                                                  MaxD=max(VPD),
                                                                  MinD=min(VPD))

#### Plots

### To plot the environmental drivers:

Min<-min(MeteoData$DOY,na.rm=T)
Max<-max(MeteoData$DOY,na.rm=T)

(PlotUp<-ggplot(MeteoData, aes(x=DOY, y=MeanPAR)) + 
  geom_line(color="orange",size=2,linetype="dotted")+
  theme_bw(30)+
  xlim(Min,Max)+
  labs(y = expression(PAR~(mu~mol~m^{-2}~s^{-1})),
       x = expression(DOY~2020),family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=0,color="white",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=12,color="black",family="sans"))+
  theme(axis.title.x = element_text ( size=1,color="white",family="sans"),
        axis.title.y = element_text ( size=12,color="black",family="sans")))


(PlotCenter<-ggplot(MeteoData, aes(y=MeanT,x=DOY))+
  theme_bw(30)+
  geom_line(color="orange",size=2)+
  geom_line(color="blue",size=1,aes(y=MinT,x=DOY))+
  geom_line(color="tomato",size=1,aes(y=MaxT,x=DOY))+
    xlim(Min,Max)+
    labs(y = expression(Air~temperature~('°C')),
         x = expression(DOY~2020),family="sans")+
    theme(axis.text.x = element_text(hjust = 1, size=0,color="white",family="sans"))+
    theme(axis.text.y = element_text(hjust = 1, size=12,color="black",family="sans"))+
    theme(axis.title.x = element_text ( size=1,color="white",family="sans"),
          axis.title.y = element_text ( size=12,color="black",family="sans")))

(PlotBottom<-ggplot(MeteoData, aes(y=MeanD,x=DOY))+
  theme_bw(30)+
  geom_line(color="orange",size=2)+
  geom_line(color="blue",size=1,aes(y=MinD,x=DOY))+
  geom_line(color="tomato",size=1,aes(y=MaxD,x=DOY))+
  xlim(Min,Max)+
  labs(y = expression(VPD~(kPa)),
       x = expression(DOY~2020),family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=12,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=12,color="black",family="sans"))+
  theme(axis.title = element_text ( size=12,color="black",family="sans")))

plot_grid(PlotUp,PlotCenter,PlotBottom,align = "v", nrow = 3, rel_heights = c(1/10, 1/10, 1/10))


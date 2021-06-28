#Author: Daniel Nadal-Sala

#Correlate sap flow to decrease in soil water content.

#Segmented correlation? 
#Finding the breaking point. Then filtering by the point where the relationship changes.
#Filtering for such point.
#Then, using "Sapflow * Sapwood = Transpiration", obtain the transpiration per the whole tree.

#Calculate the amount of leaf area required for the sap flow to match SWC depletion daily.

##Packages:

library(dplyr); library(ggplot2); library (purrr); library(tm);
library(gridExtra); library(ggExtra); library(tidyr);library(RCurl);
library(broom);library(tidyverse); library(Metrics);library(nlme);library(MuMIn)

###Functions:

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

###File source:

Directory<-"C:/Users/sala-d/Desktop/Accounting for NonStomatal Limitations/Data/"

###Files:

DataSWCSapFlowLA<-load_object(paste(Directory,"TranspirationLALS.rda",sep=""))

################################################################

#To plot tree transpiration:

MinDOY<-min(DataSWCSapFlowLA$DOY)
MaxDOY<-max(DataSWCSapFlowLA$DOY)

DOYseq<-round(seq(MinDOY,MaxDOY,(MaxDOY-MinDOY)/5))

XCoordinates<-round(c(min(DataSWCSapFlowLA$DOY,na.rm=T),
                max(DataSWCSapFlowLA$DOY,na.rm=T),
                (max(DataSWCSapFlowLA$DOY,na.rm=T)+min(DataSWCSapFlowLA$DOY,na.rm=T))/2),0)

YCoordinates<-round(c(min(DataSWCSapFlowLA$T_kg_day,na.rm=T),
                max(DataSWCSapFlowLA$T_kg_day,na.rm=T),
                (max(DataSWCSapFlowLA$T_kg_day,na.rm=T)+min(DataSWCSapFlowLA$T_kg_day,na.rm=T))/2),2)

ggplot(DataSWCSapFlowLA,aes(x=DOY,y=T_kg_day))+
  theme_minimal()+
  geom_line(aes(color=Individual),linetype="dashed")+
  scale_color_manual(values=c("black","tomato","wheat","steelblue","darkgreen","yellow","orange"))+
  geom_smooth(span = 0.2,stat = 'smooth',alpha=0.3,color="steelblue",size=2)+
  scale_x_continuous(breaks=seq(min(FinalDataFrame$DOY), max(FinalDataFrame$DOY), 2))+
  labs(y = expression(Transpiration~(kg~day^{-1})),
       x = "DOY (2020)",family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title = element_text ( size=16,color="black",family="sans"))+
  scale_x_continuous(breaks=DOYseq)


###################################################################


# How to determine the tippling point in the trend? 
# V-model to determine the change in the trend

#To plot the leaf shedding and the transpiration together

DataFrameModified<-DataSWCSapFlowLA

SeqSWC<-round(seq(min(DataFrameModified$SWC),max(DataFrameModified$SWC),
                  (max(DataFrameModified$SWC)-min(DataFrameModified$SWC)) /5),2)

ggplot(DataFrameModified) +
  theme_bw(30)+
  geom_line(mapping = aes(x = DOY, y = ProportionShed*6,color=Individual), size = 1,linetype="dashed",alpha=0.5)+
  geom_line(mapping = aes(x = DOY, y = T_kg_day,color=Individual), size = 1,linetype="dotted",alpha=0.5)+
  scale_color_manual(values=c(rep("black",6)))+
  geom_smooth(aes(x=DOY,y=T_kg_day),span = 0.1,stat = 'smooth',alpha=0.3,color="steelblue",size=2)+
  geom_smooth(aes(x=DOY,y=ProportionShed*6),span = 0.3,stat = 'smooth',alpha=0.3,color="tomato",size=2)+
  labs(x = expression(DOY~(2020)))+
  scale_y_continuous(name = expression(Transpiration~(kg~tree^{-1}~day^{-1})), 
                     sec.axis = sec_axis(~./6, name = "Leaf shedding (%)", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")}))+ 
  theme(
    legend.position="none",
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"))+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title = element_text ( size=16,color="black",family="sans"))

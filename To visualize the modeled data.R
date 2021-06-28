#### Authors: Daniel Nadal-Sala

#### Description

#This script uses daily SOX+ model outputs to generate the analysis and the figures for the paper.

##Packages:

library(dplyr); library(ggplot2); library (purrr); library(tm);
library(gridExtra); library(ggExtra); library(tidyr);library(RCurl);
library(broom); library(nlme)

#########Functions


#to load ".rda" objects directly
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#To calculate the Median and the Standard Deviation for all the registers in the model output
CalculateMedianSD<-function(Matrix){
  
  Return<-matrix(data=NA,ncol=2,nrow=length(Matrix[1,]))
  Return<-data.frame(Return)
  names(Return)<-c("Median","SD")
  
  for(i in 1:length(Matrix[1,])){
    Sample<-Matrix[,i]
    Return[i,1]<-median(Sample)
    Return[i,2]<-sd(Sample)
  }
  return(Return)
}

#To calculate the Median and the 95% confidence intervals for all the registers in the model output
CalculateMedianQuantile<-function(Matrix){
  
  Return<-matrix(data=NA,ncol=3,nrow=length(Matrix[1,]))
  Return<-data.frame(Return)
  names(Return)<-c("Median","Quantile25","Quantile975")
  
  for(i in 1:length(Matrix[1,])){
    Sample<-Matrix[,i]
    Return[i,1]<-median(Sample)
    Return[i,2]<-quantile(Sample,probs=c(0.025))
    Return[i,3]<-quantile(Sample,probs=c(0.975))
  }
  return(Return)
}

#######################################

#### Data path
Directory<-"C:/Users/sala-d/Desktop/Accounting for NonStomatal Limitations/Data to upload/"

#### Data Files

#Meteorological data
MData<- read.table(file=paste(Directory,"MeteoData.csv",sep=""),
                   header=T,sep=";",dec=".")

#Transpiration and leaf shedding data
TData<- load_object(file=paste(Directory,"TranspirationLALS.rda",sep=""))

#### To select a given period:

ChosenTree<-c("C15", "C25", "C28", "EL13", "L24", "L16")
ChosenDOY<-265

#We remove from the Likelihood analysis the DOY where there is problems in the register

TranspirationData<-TData %>% filter(Individual %in% ChosenTree) %>% filter (DOY<=ChosenDOY)

TranspirationData<-TranspirationData %>% group_by(DOY) %>% mutate (SD_Tr=sd(T_kg_day)) %>% summarise(T_kg_day = mean(T_kg_day),
                                                                                                     SD_T_kg_day = mean(SD_Tr),
                                                                                                     SWC = mean(SWC),
                                                                                                     LeafArea_m2_qlower=quantile(LeafArea_m2,c(0.025)),
                                                                                                     LeafArea_m2_qupper=quantile(LeafArea_m2,c(0.975)),
                                                                                                     LeafArea_m2 = mean(LeafArea_m2))
MeteoData<-MData %>% filter(DOY<=ChosenDOY)

##############################

#### Figures

###Figure 6

#### Plotting modeled transpiration against observed transpiration accounting or not for NSL

ResultsHydraulicLeafSheddingNSL<-load_object(paste(Directory,"ResultsHydraulicLeafSheddingNSL.rda",sep=""))
ResultsHydraulic<-load_object(paste(Directory,"ResultsHydraulic.rda",sep=""))

ModeledTranspirationComplete<-CalculateMedianSD(ResultsHydraulicLeafSheddingNSL$Tr_daily)
ModeledTranspirationHydraulic<-CalculateMedianSD(ResultsHydraulic$Tr_daily)

#######This is for the Complete simulation -i.e. accounting for NSL- (Figure 6b)

Tr_mod<-data.frame(Observed=TranspirationData$T_kg_day,
                   Simulated=ModeledTranspirationComplete$Median/1000,
                   SDObserved=TranspirationData$SD_T_kg_day,
                   SDSimulated=ModeledTranspirationComplete$SD/1000,
                    DOY=TranspirationData$DOY)

#Modeling the correlation between observed and simulated:

model<-gls(Observed~Simulated,data=Tr_mod,correlation=corAR1(form=~DOY),method="ML")
model_null<-gls(Observed~1,data=Tr_mod,correlation=corAR1(form=~DOY),method="ML")

TranspirationSeq<-data.frame(Simulated=Tr_mod$Simulated)

Predicted<-predict(model,newdata=TranspirationSeq,se.fit=TRUE)

#Plotting

ymin_Pred<-Predicted$fit-1.96*Predicted$se.fit
ymax_Pred<-Predicted$fit+1.96*Predicted$se.fit
Xlab_Pred<-TranspirationSeq$Simulated


#Cox and Snell's R2
#Cox, D.R. and E.J. Snell (1989) Analysis of Binary Data. Second Edition. Chapman & Hall.
#Based on deviance criteria.
(R2Cs=round(as.numeric(1-exp((-2*model$logLik-(-2*model_null$logLik))/length(Tr_mod$Observed))),2))
RMSE<-sqrt(mean(model$residuals^2))

ggplot(Tr_mod, aes(x=Simulated, y=Observed)) +
  theme_bw(30)+
  geom_ribbon(aes(x=Xlab_Pred,ymin=ymin_Pred,ymax=ymax_Pred),alpha=0.2,col="grey")+
  geom_line(aes(x=Xlab_Pred,y=Predicted$fit),lty="dashed")+
  geom_point(col="black")+
  ylim(c(-0.1,2.7))+
  xlim(c(-0.1,2.7))+
  geom_errorbar(aes(ymin=Observed-SDObserved, ymax=Observed+SDObserved),col="steelblue")+
  geom_errorbarh(aes(xmin=Simulated-SDSimulated, xmax=Simulated+SDSimulated),col="steelblue")+
  geom_abline(slope=1,linetype="dotted")+
  geom_text(x=2,y=0.5,label=as.expression(bquote(R[CS]^2~"="~.(R2Cs))),
            size=5,family="sans")+
  geom_text(x=2, y= 0.3, label=as.expression(bquote(RMSE~"="~.(round(RMSE,2)))), size=5,family="sans")+
  labs(x = expression(Simulated~transpiration~(kg~tree^-1~day^-1)), y= expression(Observed~transpiration~(kg~tree^-1~day^-1)))+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

#####This is for the only hydraulic simulation (Figure 6a)

Tr_mod<-data.frame(Observed=TranspirationData$T_kg_day,
                   Simulated=ModeledTranspirationHydraulic$Median/1000,
                   SDObserved=TranspirationData$SD_T_kg_day,
                   SDSimulated=ModeledTranspirationHydraulic$SD/1000,
                   DOY=TranspirationData$DOY)

#Modeling the correlation between observed and simulated:

model<-gls(Observed~Simulated,data=Tr_mod,correlation=corAR1(form=~DOY),method="ML")
model_null<-gls(Observed~1,data=Tr_mod,correlation=corAR1(form=~DOY),method="ML")

TranspirationSeq<-data.frame(Simulated=Tr_mod$Simulated)

Predicted<-predict(model,newdata=TranspirationSeq,se.fit=TRUE)

#Plotting

ymin_Pred<-Predicted$fit-1.96*Predicted$se.fit
ymax_Pred<-Predicted$fit+1.96*Predicted$se.fit
Xlab_Pred<-TranspirationSeq$Simulated

#Cox and Snell's R2
#Cox, D.R. and E.J. Snell (1989) Analysis of Binary Data. Second Edition. Chapman & Hall.
#Based on deviance criteria.
(R2Cs=round(as.numeric(1-exp((-2*model$logLik-(-2*model_null$logLik))/length(Tr_mod$Observed))),2))
RMSE<-sqrt(mean(model$residuals^2))

ggplot(Tr_mod, aes(x=Simulated, y=Observed)) +
  theme_bw(30)+
  geom_ribbon(aes(x=Xlab_Pred,ymin=ymin_Pred,ymax=ymax_Pred),alpha=0.2,col="grey")+
  geom_line(aes(x=Xlab_Pred,y=Predicted$fit),lty="dashed")+
  geom_point(col="black")+
  ylim(c(-0.1,2.7))+
  xlim(c(-0.1,2.7))+
  geom_errorbar(aes(ymin=Observed-SDObserved, ymax=Observed+SDObserved),col="darkred")+
  geom_errorbarh(aes(xmin=Simulated-SDSimulated, xmax=Simulated+SDSimulated),col="darkred")+
  geom_abline(slope=1,linetype="dotted")+
  geom_text(x=2,y=0.5,label=as.expression(bquote(R[CS]^2~"="~.(R2Cs))),
            size=5,family="sans")+
  geom_text(x=2, y= 0.3, label=as.expression(bquote(RMSE~"="~.(round(RMSE,2)))), size=5,family="sans")+
  labs(x = expression(Simulated~transpiration~(kg~tree^-1~day^-1)), y= expression(Observed~transpiration~(kg~tree^-1~day^-1)))+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

# Figure 6b

a<-which(TranspirationData$DOY<=230)

DataFrameToPlot<-data.frame(Hydraulic=(ModeledTranspirationHydraulic$Median[a]/1000-TranspirationData$T_kg_day[a])/TranspirationData$T_kg_day[a]*100,
                            HydraulicNSL = (ModeledTranspirationComplete$Median[a]/1000-TranspirationData$T_kg_day[a])/TranspirationData$T_kg_day[a]*100,
                            DOY = TranspirationData$DOY[a])

ggplot(DataFrameToPlot, aes(x=DOY, y=HydraulicNSL))+
  theme_bw(30)+
  geom_line(size=2,col="steelblue")+
  geom_line(aes(y=Hydraulic),col="darkred", size=2,linetype="dashed")+
  geom_hline(yintercept=median(DataFrameToPlot$HydraulicNSL), color="steelblue")+
  geom_hline(yintercept=median(DataFrameToPlot$Hydraulic), color="darkred",linetype="dashed")+
  labs(x = expression(DOY~(2020)), y= expression(Transpiration~simulation~error~("%")))+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

### Figure 8: Evolution of PLC and leaf area.

Results<-load_object(paste(Directory,"ResultsHydraulicLeafSheddingNSL.rda",sep=""))

PLC<-CalculateMedianQuantile(Results$PLC_daily)

ResultsToPlot<-data.frame(PLC_Median=PLC$Median,PLC_Q25=PLC$Quantile25,PLC_Q975=PLC$Quantile975,
                          DOY=TranspirationData$DOY,Leaf_Area_Median = TranspirationData$LeafArea_m2,
                          Leaf_Area_Q25 =TranspirationData$LeafArea_m2_qlower,
                          Leaf_Area_Q975=TranspirationData$LeafArea_m2_qupper)

Psi50<-50/(100/max(ResultsToPlot$Leaf_Area_Q975))
coeff<-(100/max(ResultsToPlot$Leaf_Area_Q975))

ggplot(ResultsToPlot) +
  theme_bw(30)+
  geom_line(mapping = aes(x = DOY, y = Leaf_Area_Median), size = 1,color="steelblue",linetype="dotted")+
  geom_line(mapping = aes(x = DOY, y = PLC_Median/(100/max(ResultsToPlot$Leaf_Area_Q975))), size = 1,color="black")+
  geom_ribbon(aes(x=DOY,ymin=Leaf_Area_Q25,ymax=Leaf_Area_Q975),alpha=0.3, fill="steelblue")+
  geom_ribbon(aes(x=DOY,ymin=PLC_Q25/(100/max(ResultsToPlot$Leaf_Area_Q975)),ymax=PLC_Q975/(100/max(ResultsToPlot$Leaf_Area_Q975))),alpha=0.3, fill="black")+
  ylim(0,max(ResultsToPlot$Leaf_Area_Q975))+
  geom_hline(yintercept = Psi50,linetype="dashed")+
  labs(x = expression(DOY~(2020)))+
  scale_y_continuous(name=expression(Leaf~area~(m^{2}~tree^{-1})),
                          sec.axis = sec_axis(~.*coeff, name=expression(Loss~of~root-to-canopy~conductance~('%'))))+
  theme(
    legend.position="none",
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"))+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title = element_text ( size=16,color="black",family="sans"))


### Figure 9: Evolution of PLC according to the different model assumptions

ResultsHydraulicLeafSheddingNSL<-load_object(paste(Directory,"ResultsHydraulicLeafSheddingNSL.rda",sep=""))
ResultsHydraulicNSL<-load_object(paste(Directory,"ResultsHydraulicNSL.rda",sep=""))
ResultsHydraulic<-load_object(paste(Directory,"ResultsHydraulic.rda",sep=""))

PLC_HLSN<-CalculateMedianQuantile(ResultsHydraulicLeafSheddingNSL$PLC_daily)
PLC_HN<-CalculateMedianQuantile(ResultsHydraulicNSL$PLC_daily)
PLC_H<-CalculateMedianQuantile(ResultsHydraulic$PLC_daily)

ResultsToPlot<-data.frame(PLC_HLSN_Median = PLC_HLSN$Median, PLC_HLSN_Q25 = PLC_HLSN$Quantile25, PLC_HLSN_Q975 = PLC_HLSN$Quantile975,
                          PLC_HN_Median = PLC_HN$Median, PLC_HN_Q25 = PLC_HN$Quantile25, PLC_HN_Q975 = PLC_HN$Quantile975,
                          PLC_H_Median = PLC_H$Median, PLC_H_Q25 = PLC_H$Quantile25, PLC_H_Q975 = PLC_H$Quantile975,
                          DOY = TranspirationData$DOY)

ggplot(ResultsToPlot)+
  theme_bw(30)+
  geom_hline(yintercept = 50,linetype="dashed")+
  geom_hline(yintercept = 80,linetype="dotted")+ #From Hammond et al., (2019) New Phytologist
  geom_ribbon(aes(x=DOY,ymin=PLC_H_Q25,ymax=PLC_H_Q975),alpha=0.5, fill="black")+
  geom_ribbon(aes(x=DOY,ymin=PLC_HN_Q25,ymax=PLC_HN_Q975),alpha=0.3, fill="darkgreen")+
  geom_ribbon(aes(x=DOY,ymin=PLC_HLSN_Q25,ymax=PLC_HLSN_Q975),alpha=0.3, fill="orange")+
  geom_line(aes(x=DOY,y=PLC_H_Median), color="black")+
  geom_line(aes(x=DOY,y=PLC_HN_Median), color="darkgreen")+
  geom_line(aes(x=DOY,y=PLC_HLSN_Median), color="orange")+
  labs(x = expression(DOY~(2020)),
       y = expression(Loss~of~root-to-canopy~conductance~('%')))+
  ylim(c(0,100))+
  theme( legend.position="none",
         axis.title.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "black"))+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title = element_text ( size=16,color="black",family="sans"))


### Figure 9: Comparison of PLC difference between trees whith and whithout leaf shedding

#Increased gs = 0.003 mols m-2 s-1; Identified as _Increasedgs

ResultsBAU<-load_object(paste(Directory,"ResultsHydraulicLeafSheddingNSL.rda",sep=""))
ResultsNoLeafBAU<-load_object(paste(Directory,"ResultsHydraulicNSL.rda",sep=""))
Resultsgs<-load_object(paste(Directory,"ResultsHydraulicLeafSheddingNSL_Increasedgs.rda",sep=""))
ResultsNoLeafgs<-load_object(paste(Directory,"ResultsHydraulicNSL_Increasedgs.rda",sep=""))

#### We will plot the cumulated difference between two random samples for each scenario.

NSim<-500

set.seed(13)

Difference_BAU<-NULL
CumDif<-NULL

for(i in 1:NSim){

SampBAU<-ResultsBAU$PLC_daily[sample(nrow(ResultsBAU$PLC_daily),1),]
SampNLS<-ResultsNoLeafBAU$PLC_daily[sample(nrow(ResultsNoLeafBAU$PLC_daily),1),]

Dif<-SampNLS-SampBAU

CumDif[1]<-SampNLS[1]-SampBAU[1]

    for(j in 2:length(Dif)){
    
    CumDif[j]<-CumDif[j-1]+Dif[j]
    
    }

Difference_BAU<-rbind(Difference_BAU,CumDif)

}

MQDifferenceBAU<-CalculateMedianQuantile(Difference_BAU) #This is for BAU scenario

NSim<-500

set.seed(13)

Difference_gs<-NULL
CumDif<-NULL

for(i in 1:NSim){
  
  SampBAU<-Resultsgs$PLC_daily[sample(nrow(Resultsgs$PLC_daily),1),]
  SampNLS<-ResultsNoLeafgs$PLC_daily[sample(nrow(ResultsNoLeafgs$PLC_daily),1),]
  
  Dif<-SampNLS-SampBAU
  
  CumDif[1]<-SampNLS[1]-SampBAU[1]
  
  for(j in 2:length(Dif)){
    
    CumDif[j]<-CumDif[j-1]+Dif[j]
    
  }
  
  Difference_gs<-rbind(Difference_gs,CumDif)
  
}

MQDifference_gs<-CalculateMedianQuantile(Difference_gs) #This is forincreased gs scenario

NormalizedDoy<-TranspirationData$DOY-min(TranspirationData$DOY)

DataFrame<-data.frame(MedianBAU=MQDifferenceBAU$Median,Q25BAU=MQDifferenceBAU$Quantile25,Q975BAU=MQDifferenceBAU$Quantile975,
                      Mediangs=MQDifference_gs$Median,Q25gs=MQDifference_gs$Quantile25,Q975gs=MQDifference_gs$Quantile975,
                      DOY = TranspirationData$DOY, NormalDOY=NormalizedDoy)

ggplot(DataFrame,aes(x=DOY,y=MedianBAU/NormalDOY))+
  theme_bw(30)+
  geom_ribbon(aes(ymin=Q25BAU/NormalDOY,ymax=Q975BAU/NormalDOY),fill="steelblue",alpha=0.3)+
  geom_ribbon(aes(ymin=Q25gs/NormalDOY,ymax=Q975gs/NormalDOY),fill="darkred",alpha=0.3)+
  geom_line(color="steelblue",size=2)+
  geom_line(aes(x=DOY,y=Mediangs/NormalDOY),size=2,color="darkred", linetype="dashed")+
  labs(x = expression(DOY~(2020)),
       y = expression(Benefit~of~leaf~shedding~('-PLk'[rc]~','~'%'~day^{-1})))+
  theme( legend.position="none",
         axis.title.y = element_text(color = "black"),
         axis.title.y.right = element_text(color = "black"))+
  geom_vline(xintercept=250,color="steelblue",size=1)+
  geom_vline(xintercept=244,linetype="dashed",color="darkred",size=1)+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title = element_text ( size=16,color="black",family="sans"))

ks.test(Difference_BAU[,54]/54,Difference_gs[,54]/54)

#The sample distributions at DOY 265 are not equal with p<0.01 according to a K-S nonparametric test

#Kolmogorov-Smirnov non-parametric test for daily values:

Res<-NULL
for(i in 1:54){
  a<-ks.test(Difference_BAU[,i]/i,Difference_gs[,i]/i)
Res[i]<-unlist(a$p.value)
}

Res

#The sample distributions at from DOY 230 and onwards are not equal with p<0.05 according to a K-S nonparametric test

#################################################
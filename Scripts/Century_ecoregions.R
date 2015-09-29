#script to produce predictions for ecosystem services and biodivesrity for each of
#Elena's Landis-II 'ecoregions' using data from Paul's gradient plots

#author: Phil martin
#Date 2015/09/24

#open packages
library(raster)
library(ggplot2)
library(lme4)
library(reshape2)
library(plyr)
library(MuMIn)
library(gtools)

#clear previous R objects
rm(list=ls())


#load in Paul's biomass data
BM<-read.csv("Data/BMLan.csv",header = T)
BM<-subset(BM,AGB<600)
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass

#import Elena's ecoregion data
EcoR<-read.csv("Century_outputs/Century_tables/Century-succession-log6_r1.csv")


#run a loop to test each variable and produce predictions
Coefficients<-NULL
for (i in 4:5){
  M1<-lmer(BM[[i]]~AGB_std+(1|Site),data=BM)
  M2<-lmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
  M0<-lmer(BM[[i]]~1+(1|Site),data=BM)
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=coef(Mod_average)[1],
                                 AGB=coef(Mod_average)[2],
                                 AGB_sq=coef(Mod_average)[3]))
}
for (i in 6:8){
  M1<-glmer(BM[[i]]~AGB_std+(1|Site),data=BM,family="poisson")
  M2<-glmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
  M0<-glmer(BM[[i]]~1+(1|Site),data=BM,family="poisson")
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=coef(Mod_average)[1],
                                 AGB=coef(Mod_average)[2],
                                 AGB_sq=coef(Mod_average)[3]))
}

#tidy data
rownames(Coefficients)<-NULL

#create predictions from the model coefficients using Elena's ecoregion data
EcoR2<-data.frame(EcoR[-6],SRR=NA,Min_rate=NA,Fungi=NA,GF=NA,Lichen=NA)
str(EcoR2)

ptm <- proc.time()
Mean_summary<-NULL
for (j in 1:nrow(Coefficients)){
  if (j<=3){
    Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                    (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
    EcoR2[[5+j]]<-Prediction
    colnames(EcoR2)[5+j]<-as.character(Coefficients[j,1])
} else {
  Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                  (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
  EcoR2[[5+j]]<-exp(Prediction)
  colnames(EcoR2)[5+j]<-as.character(Coefficients[j,1])
}
}
proc.time() - ptm

head(EcoR3)

#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion
EcoR3<-ddply(EcoR2,.(Time,EcoregionName),summarise,mean_AGB=(AGB*NumSites)/100,sum_sites=sum(NumSites))
ddply(EcoR3,.(Time),summarise,mean_AGB2=mean_AGB/sum(sum_sites))
ddply()

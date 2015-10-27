#script to produce predictions for ecosystem services and biodivesrity for each of
#Elena's Landis-II 'ecoregions' using data from Paul's gradient plots

#author: Phil martin
#Date 2015/09/24

#open packages
library(raster)
library(ggplot2)
library(lme4)
library(reshape)
library(reshape2)
library(plyr)
library(MuMIn)
library(gtools)
library(SDMTools)
library(tidyr)
library(dplyr)

#clear previous R objects
rm(list=ls())

#load in Paul and Arjan's  data
BM<-read.csv("Data/BMLan.csv",header = T)
Rec_aes<-read.csv("Data/Rec_aes_reorg.csv",header = T)
BM<-subset(BM,AGB<600)#subset to remove one site with extreme AGB value from Paul's data
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass
#tranform recreation data
Rec_aes$Rec_trans<-((Rec_aes$Recreation-1)/4)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_trans==0,Rec_aes$Rec_trans+0.01,Rec_aes$Rec_trans)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_transM==1,Rec_aes$Rec_transM-0.01,Rec_aes$Rec_transM)
Rec_aes$Rec_transM<-qlogis(Rec_aes$Rec_transM)
Rec_aes$Aes_trans<-((Rec_aes$Aesthetic-1)/4)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_trans==0,Rec_aes$Aes_trans+0.01,Rec_aes$Aes_trans)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_transM==1,Rec_aes$Aes_transM-0.01,Rec_aes$Aes_transM)
Rec_aes$Aes_transM<-qlogis(Rec_aes$Aes_transM)

head(Rec_aes)

#run a loop to test each model and produce a coefficient for each 
#variable
#first using SRR and mineralisation rate
Coefficients<-NULL
for (i in 4:5){
  M1<-lmer(BM[[i]]~AGB_std+(1|Site),data=BM)
  M2<-lmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
  M0<-lmer(BM[[i]]~1+(1|Site),data=BM)
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=summary(Mod_average)$coefmat.full[1,1],
                                 AGB=summary(Mod_average)$coefmat.full[2,1],
                                 AGB_sq=summary(Mod_average)$coefmat.full[3,1]))
}
#and then for count data - fungi, ground floa and lichen species richness
for (i in 6:8){
  M1<-glmer(BM[[i]]~AGB_std+(1|Site),data=BM,family="poisson")
  M2<-glmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
  M0<-glmer(BM[[i]]~1+(1|Site),data=BM,family="poisson")
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=summary(Mod_average)$coefmat.full[1,1],
                                 AGB=summary(Mod_average)$coefmat.full[2,1],
                                 AGB_sq=summary(Mod_average)$coefmat.full[3,1]))
}
for (i in c(7,9)){
  M1<-lmer(Rec_aes[[i]]~mean_AGB+(1|ID),data=Rec_aes)
  M2<-lmer(Rec_aes[[i]]~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
  M0<-lmer(Rec_aes[[i]]~1+(1|ID),data=Rec_aes)
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(Rec_aes[i]),
                                 Intercept=summary(Mod_average)$coefmat.full[1,1],
                                 AGB=summary(Mod_average)$coefmat.full[2,1],
                                 AGB_sq=summary(Mod_average)$coefmat.full[3,1]))
}



#tidy data
rownames(Coefficients)<-NULL


###############################################################################
#create predictions from the model coefficients using Elena's ecoregion data###
###############################################################################

#import data
Eco_regions<-list.files(pattern="Century-succession-log",recursive=T)

#make prediction for each time step of each ecoregion of each scenario
Eco_summary<-NULL
for (i in 1:length(Eco_regions)){#for each file in the list Eco_regions run this code
  EcoR<-read.csv(Eco_regions[i])
  N_col<-ncol(EcoR)
  EcoR2<-data.frame(EcoR[-c(5:6,(8:N_col))],SRR=NA,Min_rate=NA,Fungi=NA,GF=NA,Lichen=NA,Aesthetic=NA,Recreation=NA)
  Mean_summary<-NULL
for (j in 1:nrow(Coefficients)){
  if (j<=3){
    Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                    (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
    EcoR2[[5+j]]<-Prediction
  } else if (j>=6) {
  Prediction<-(((((EcoR2$AGB/100)*Coefficients[j,3])+ #use coefficients to predict ES and biodiversity values
                  (((EcoR2$AGB/100)^2)*Coefficients[j,4])+Coefficients[j,2])))
  EcoR2[[5+j]]<-((plogis(Prediction))*4)+1  
} else {
  Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                  (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
  EcoR2[[5+j]]<-exp(Prediction)
}}
EcoR2$Scenario<-paste("Scenario ",gsub( "_r.*$", "", gsub("^.*?Century-succession-log","", Eco_regions[i])),sep="")
EcoR2$Replicate<-gsub( ".csv.*$", "", gsub("^.*?_r","", Eco_regions[i]))
Eco_summary<-rbind(Eco_summary,EcoR2)
}

#calculate AGB in Mg per Ha
Eco_summary$AGB<-Eco_summary$AGB/100

#produce mean for each variable in each ecoregion at each time step, in each scenario
Eco_summary2<-ddply(Eco_summary,.(Time,EcoregionName,EcoregionIndex,Scenario),numcolwise(mean,na.rm=T))
write.csv(x=Eco_summary2,"Data/R_output/Ecoregion_means.csv")

#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion

Eco_summary3<-ddply(Eco_summary,.(Time,Scenario),summarise,
              AGB_M=weighted.mean(AGB,NumSites,na.rm = T),AGB_SD=wt.sd(AGB,NumSites),
              SRR_M=weighted.mean(SRR,NumSites,na.rm = T),SRR_SD=wt.sd(SRR,NumSites),
              Min_rate_M=weighted.mean(Min_rate,NumSites,na.rm = T),Min_rate_SD=wt.sd(Min_rate,NumSites),
              Fungi_M=weighted.mean(Fungi,NumSites,na.rm = T),Fungi_SD=wt.sd(Fungi,NumSites),
              GF_M=weighted.mean(GF,NumSites,na.rm = T),GF_SD=wt.sd(GF,NumSites),
              Lichen_M=weighted.mean(Lichen,NumSites,na.rm = T),Lichen_SD=wt.sd(Lichen,NumSites),
              Aesthetic_M=weighted.mean(Aesthetic,NumSites,na.rm = T),Aesthetic_SD=wt.sd(Aesthetic,NumSites),
              Recreation_M=weighted.mean(Recreation,NumSites,na.rm = T),Recreation_SD=wt.sd(Recreation,NumSites))
write.csv(x=Eco_summary3,"Data/R_output/Ecoregion_summary.csv")

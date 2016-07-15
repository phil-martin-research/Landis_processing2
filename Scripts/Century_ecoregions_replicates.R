#script to produce predictions for ecosystem services and biodiversity for each of
#Elena's Landis-II 'ecoregions' using data from Paul's gradient plots

#author: Phil martin
#Date 2015/11/04

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
library(GGally)

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
#and then for count data - fungi, ground flora and lichen species richness
for (i in 6:9){
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
  EcoR2<-data.frame(EcoR[-c(5:6,(8:N_col))],SRR=NA,Min_rate=NA,Fungi=NA,GF=NA,Lichen=NA,Fungi_val=NA,Aesthetic=NA,Recreation=NA)
  Mean_summary<-NULL
for (j in 1:nrow(Coefficients)){
  if (j<3){
    Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                    (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
    EcoR2[[5+j]]<-Prediction
  } else if (j>=3 & j<=6){
    Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                    (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
    EcoR2[[5+j]]<-exp(Prediction)
  }else{
  Prediction<-(((((EcoR2$AGB/100)*Coefficients[j,3])+ #use coefficients to predict ES and biodiversity values
                  (((EcoR2$AGB/100)^2)*Coefficients[j,4])+Coefficients[j,2])))
  EcoR2[[5+j]]<-((plogis(Prediction))*4)+1  
}}
EcoR2$Scenario<-paste("Scenario ",gsub( "_r.*$", "", gsub("^.*?Century-succession-log","", Eco_regions[i])),sep="")
EcoR2$Replicate<-as.numeric(gsub( ".csv.*$", "", gsub("^.*?_r","", Eco_regions[i])))
Eco_summary<-rbind(Eco_summary,EcoR2)
}


#calculate AGB in Mg per ha
Eco_summary$AGB<-Eco_summary$AGB/100

Eco_summary2<-ddply(Eco_summary,.(Time,EcoregionName,EcoregionIndex,Scenario,Replicate),numcolwise(mean,na.rm=T))


#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion

Eco_summary3<-ddply(Eco_summary,.(Scenario,Time,Replicate),summarise,
                    AGB_M=weighted.mean(AGB,NumSites,na.rm = T),
                    SRR_M=weighted.mean(SRR,NumSites,na.rm = T),
                    Min_rate_M=weighted.mean(Min_rate,NumSites,na.rm = T),
                    Fungi_M=weighted.mean(Fungi,NumSites,na.rm = T),
                    GF_M=weighted.mean(GF,NumSites,na.rm = T),
                    Lichen_M=weighted.mean(Lichen,NumSites,na.rm = T),
                    Aesthetic_M=weighted.mean(Aesthetic,NumSites,na.rm = T),
                    Recreation_M=weighted.mean(Recreation,NumSites,na.rm = T),
                    Fungi_val_M=weighted.mean(Fungi_val,NumSites,na.rm = T)
                    )



#################################################
#run calculations for carbon and nitrogen stocks###
#################################################

#find all the century carbon and nitrogen files
C_N<-list.files(pattern="Century-succession-log",recursive=T)

#run a loop to read in each .csv containing tree species biomass values
#remove columns that are not useful and ass a column to give details of
#the scenario being run
CN_ER<-NULL
for (i in 1:length(C_N)){
  #read in .csv
  File<-read.csv(C_N[i])
  #remove blank column
  File_sub<-File[-c(5:12,14,29:ncol(File))]
  #remove rows containing NAs
  File_sub2<-File_sub[complete.cases(File_sub),]
  #calculate total carbon
  Total_C<-rowSums (File_sub2[6:ncol(File_sub2)], na.rm = FALSE, dims = 1)/100
  File_sub3<-cbind(File_sub2[,1:5],Total_C)
  #insert a column with the scenario number
  File_sub3$Scenario<-paste("Scenario ",gsub(".*-log|_r.*","", C_N[i]),sep="")
  File_sub3$Replicate<-sub(".*?_r(.*?).csv.*", "\\1", C_N[i])
  CN_ER<-rbind(CN_ER,File_sub3)
}

#summarise carbon and nitrogen flux
CN_ER_sum<-ddply(CN_ER,.(Time,EcoregionName,Scenario,Replicate),summarise,Carbon_stock=mean(Total_C,na.rm = T),Nitrogen_stock=mean(TotalN,na.rm = T))
WM_CN<-ddply(CN_ER,.(Scenario,Time,Replicate),summarise,Carbon_stock_M=weighted.mean(Total_C,NumSites,na.rm = T),
             Nitrogen_stock_M=weighted.mean(TotalN,NumSites,na.rm = T))

##########################
#calculate timber value### 
#& tree species richness##
##########################

#find all the century species biomass files
Eco_region_BM<-list.files(pattern="spp-biomass-log",recursive=T)

#run a loop to read in each .csv containing tree species biomass values
#remove columns that are not useful and as a column to give details of
#the scenario being run
BM_ER<-NULL
for (i in 1:length(Eco_region_BM)){
  #read in .csv
  File<-read.csv(Eco_region_BM[i])
  File_sub1<-File[-c(1:4,ncol(File))]
  #remove columns that we are not interested in
  File_sub2<-File[-c(5:12,14:25,27:ncol(File))]
  #remove rows containing NAs
  File_sub1<-File_sub1[complete.cases(File_sub1),]
  File_sub1[File_sub1 > 0] <- 1 
  Sp_R<-as.vector(rowSums(x=File_sub1))
  File_sub2<-File_sub2[complete.cases(File_sub2),]
  #insert a column with the scenario number
  File_sub2$Scenario<-paste("Scenario",gsub(".*-log|_r.*","", Eco_region_BM[i]))
  File_sub2$Replicate<-gsub(".*_r|.csv.*","", Eco_region_BM[i])
  File_sub2$EcoregionName<-File_sub2$Ecoregion
  File_sub3<-cbind(File_sub2,Sp_R)
  #bind all these outputs together
  BM_ER<-rbind(File_sub3,BM_ER)
}

#convert biomass to volume using expansion factor of 0.55 for beech and 0.56 for oak
BM_ER$Vol<-((BM_ER$SppBiomass_fagusylv/0.55)+(BM_ER$SppBiomass_querrobu/0.56))/100

Trees_sum<-ddply(BM_ER,.(Time,EcoregionName,Scenario,Replicate),summarise,Timber=mean(Vol,na.rm = T),Tree_richness=mean(Sp_R,na.rm = T))
Trees<-ddply(BM_ER,.(Scenario,Time,Replicate),summarise,Timber_M=weighted.mean(Vol,NumSites,na.rm = T),
             Tree_richness_M=weighted.mean(Sp_R,NumSites,na.rm = T))
###################################################################################################################
#merge all different ecosystem services and biodiversity measures together into two dataframes#####################
###################################################################################################################

Eco_summary_means<-merge(merge(Eco_summary2,CN_ER_sum,by=c("EcoregionName","Scenario","Time")),Trees_sum,by=c("EcoregionName","Scenario","Time"))
write.csv(x=Eco_summary_means,"Data/R_output/Ecoregion_means_replicates.csv")

#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion

Eco_summary_weighted<-merge(merge(Eco_summary3,WM_CN,by=c("Scenario","Time","Replicate")),Trees,by=c("Scenario","Time","Replicate"))
write.csv(x=Eco_summary_weighted,"Data/R_output/Ecoregion_summary_replicates.csv")

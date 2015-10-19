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

#load in Paul's biomass data
BM<-read.csv("Data/BMLan.csv",header = T)
BM<-subset(BM,AGB<600)#subset to remove one site with extreme AGB value
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass


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
                                 Intercept=coef(Mod_average)[1],
                                 AGB=coef(Mod_average)[2],
                                 AGB_sq=coef(Mod_average)[3]))
}
#and then for count data - fungi, ground floa and lichen species richness
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
  EcoR2<-data.frame(EcoR[-c(5:6,(8:N_col))],SRR=NA,Min_rate=NA,Fungi=NA,GF=NA,Lichen=NA)
  Mean_summary<-NULL
for (j in 1:nrow(Coefficients)){
  if (j<=3){
    Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                    (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
    EcoR2[[5+j]]<-Prediction
} else {
  Prediction<-((((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
                  (((((EcoR2$AGB)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
  EcoR2[[5+j]]<-exp(Prediction)
}
}
EcoR2$Scenario<-paste("Scenario ",gsub( "_r.*$", "", gsub("^.*?Century-succession-log","", Eco_regions[i])),sep="")
EcoR2$Replicate<-gsub( ".csv.*$", "", gsub("^.*?_r","", Eco_regions[i]))
Eco_summary<-rbind(Eco_summary,EcoR2)
}

#calculate AGB in Mg per Ha
Eco_summary$AGB<-Eco_summary$AGB/100

#melt data to give one column with all variable values 
Eco_summary_melt<-melt(Eco_summary,id.vars = c("Time","EcoregionName","EcoregionIndex","NumSites","Scenario","Replicate"))

#produce mean for each variable in each ecoregion at each time step, in each scenario
Eco_summary2<-ddply(Eco_summary,.(Time,EcoregionName,EcoregionIndex,Scenario),numcolwise(mean,na.rm=T))

#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion

Eco_summary3<-ddply(Eco_summary_melt,.(Time,EcoregionName,EcoregionIndex,Scenario),summarise,
                    W_M=weighted.mean(value,NumSites,na.rm = T),SD=wt.sd(value,NumSites))


head(Eco_summary3)
head(spread(data = Eco_summary,variable,.(W_M,SD)))
head(dcast(data = Eco_summary,Time + Scenario ~variable))


#calculate mean of the results for each time step for each ecoregion

head(Eco_summary)

Eco_summary2<-ddply(Eco_summary,.(Time,Scenario),summarise,
              AGB_M=weighted.mean(AGB,NumSites,na.rm = T),AGB_SD=wt.sd(AGB,NumSites),
              SRR_M=weighted.mean(SRR,NumSites,na.rm = T),SRR_SD=wt.sd(SRR,NumSites),
              Min_rate_M=weighted.mean(Min_rate,NumSites,na.rm = T),Min_rate_SD=wt.sd(Min_rate,NumSites),
              Fungi_M=weighted.mean(Fungi,NumSites,na.rm = T),Fungi_SD=wt.sd(Fungi,NumSites),
              GF_M=weighted.mean(GF,NumSites,na.rm = T),GF_SD=wt.sd(GF,NumSites),
              Lichen_M=weighted.mean(Lichen,NumSites,na.rm = T),Lichen_SD=wt.sd(Lichen,NumSites))
head(Eco_summary2,200)
write.csv(x=Eco_summary2,"Data/R_output/Ecoregion_summary.csv")




#plot results of this
theme_set(theme_bw(base_size=12))
P1<-ggplot(Eco_summary2,aes(x=Time,y=AGB_M))+geom_line()+facet_wrap(~Scenario,nrow = 1)
P2<-P1+geom_ribbon(data=Eco_summary2,aes(y=AGB_M,ymax=AGB_M+AGB_SD,ymin=AGB_M-AGB_SD,group=NULL),alpha=0.5)
P3<-P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3+ylab("Value")+xlim(0,100)+xlab("Time(Years)")
ggsave("Figures/Ecoregion_ES.pdf",dpi = 400,height=8,width=10,units="in")

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


#run a loop to test each model and produce a coefficient for each 
#variable
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


###############################################################################
#create predictions from the model coefficients using Elena's ecoregion data###
###############################################################################

#import data
Eco_regions<-list.files(pattern="Century-succession-log",recursive=T)

#make prediction for each time step of each ecoregion of each scenario

Eco_summary<-NULL
ptm <- proc.time()
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
EcoR2$Scenario<-gsub( "_r.*$", "", gsub("^.*?Century-succession-log","", Eco_regions[i]))
EcoR2$Replicate<-gsub( ".csv.*$", "", gsub("^.*?_r","", Eco_regions[i]))
Eco_summary<-rbind(Eco_summary,EcoR2)
}

proc.time() - ptm

Eco_summary$AGB<-Eco_summary$AGB/100
head(Eco_summary,200)



Eco_summary_melt<-melt(Eco_summary,id.vars = c("Time","EcoregionName","EcoregionIndex","NumSites","Scenario","Replicate"))
head(Eco_summary_melt)
Eco_summary_melt2<-ddply(Eco_summary_melt,.(Time,EcoregionName,Scenario,variable),summarise,mean_var=mean(value))


Plot1<-ggplot(Eco_summary_melt2,aes(x=Time,y=mean_var,group=interaction(EcoregionName)))+geom_line(size=0.5,alpha=0.2)+facet_grid(variable~Scenario,scales="free_y")
Plot1+geom_smooth(aes(group=NULL),size=3)

#calculate mean of the results for each time step, weighting by number of pixels in each 
#ecoregion

Eco_summary2<-ddply(Eco_summary,.(Time,Scenario),mutate,function(X) data.frame(AGB_m=weighted.mean(X$AGB,X$NumSites,na.rm = T),
                                          SRR_m=weighted.mean(X$SRR,X$NumSites,na.rm = T),
                                          Min_rate_m=weighted.mean(X$Min_rate,X$NumSites,na.rm = T),
                                          Fungi_m=weighted.mean(X$Fungi,X$NumSites,na.rm = T),
                                          GF_m=weighted.mean(X$GF,X$NumSites,na.rm = T),
                                          Lichen_m=weighted.mean(X$Lichen,X$NumSites,na.rm = T)))


head(Eco_summary)
head(Eco_summary2)
write.csv(x=Eco_summary2,"Data/R_output/Ecoregion_summary.csv")

#calculate mean of the results for each time step for each ecoregion

Eco_summary3<-ddply(Eco_summary,.(Time,Scenario,EcoregionName),function(X) data.frame(AGB=mean(X$AGB,na.rm = T),
                                                                        SRR=mean(X$SRR,na.rm = T),
                                                                        Min_rate=mean(X$Min_rate,na.rm = T),
                                                                        Fungi=mean(X$Fungi,na.rm = T),
                                                                        GF=mean(X$GF,na.rm = T),
                                                                        Lichen=mean(X$Lichen,na.rm = T)))
head(Eco_summary3,200)
write.csv(x=Eco_summary3,"Data/R_output/Ecoregion_means.csv")


Eco_summary_melt3<-melt(Eco_summary2,id.vars = c("Time","Scenario"))
head(Eco_summary_melt3)

#plot results of this
theme_set(theme_bw(base_size=12))
P1<-ggplot(Eco_summary_melt2,aes(x=Time,y=mean_var,group=EcoregionName))+geom_line(size=0.2,alpha=0.1)+facet_grid(variable~Scenario,scales="free_y")
P2<-P1+geom_line(data=Eco_summary_melt3,aes(x=Time,y=value,group=NULL),size=1.5,alpha=1)
P3<-P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3+ylab("Value")+xlim(0,100)+xlab("Time(Years)")
ggsave("Figures/Ecoregion_ES.pdf",dpi = 400,height=8,width=10,units="in")

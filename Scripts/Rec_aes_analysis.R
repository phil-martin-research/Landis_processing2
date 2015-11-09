##############################################################
#script to produce analysis for recreation and aesthetic data
#############################################################

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(gridExtra)
library(gtools)
library(raster)
library(tidyr)

#remove any R objects
rm(list=ls())

#import recreation data
Rec_aes<-read.csv("Data/Rec_aes_reorg.csv")

#model recreation as a function of biomass
#first tranform this data
Rec_aes$Rec_trans<-((Rec_aes$Recreation-1)/4)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_trans==0,Rec_aes$Rec_trans+0.01,Rec_aes$Rec_trans)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_transM==1,Rec_aes$Rec_transM-0.01,Rec_aes$Rec_transM)
Rec_aes$Rec_transM<-qlogis(Rec_aes$Rec_transM)

#run models
M1_rec<-lmer(Rec_transM~mean_AGB+(1|ID),data=Rec_aes)
M2_rec<-lmer(Rec_transM~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
M0_rec<-lmer(Rec_transM~1+(1|ID),data=Rec_aes)
AICc(M1_rec,M2_rec,M0_rec)

summary(M1_rec)
summary(Rec_aes)
df<-data.frame(mean_AGB=seq(0,396,by=1))

df$Recreation<-((plogis(predict(M1_rec,newdata = df,re.form=NA)))*4)+1

Rec_sum<-ddply(Rec_aes,.(Recreation,mean_AGB),summarise,count_un=length(ID))

#plot results
theme_set(theme_bw(base_size=12))
Rec_plot1<-ggplot(Rec_sum,aes(x=mean_AGB,y=Recreation,size=count_un))+geom_point(shape=1)+geom_line(data=df,size=1)
Rec_plot2<-Rec_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),legend.position="none")
Rec_plot3<-Rec_plot2+ylab("Recreational rating")+xlab("Aboveground biomass")


#model aesthetic appreciation as a function of biomass
#first tranform this data
Rec_aes$Aes_trans<-((Rec_aes$Aesthetic-1)/4)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_trans==0,Rec_aes$Aes_trans+0.01,Rec_aes$Aes_trans)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_transM==1,Rec_aes$Aes_transM-0.01,Rec_aes$Aes_transM)
Rec_aes$Aes_transM<-qlogis(Rec_aes$Aes_transM)

#run models
M1_aes<-lmer(Aes_transM~mean_AGB+(1|ID),data=Rec_aes)
M2_aes<-lmer(Aes_transM~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
M0_aes<-lmer(Aes_transM~1+(1|ID),data=Rec_aes)
AICc(M1_aes,M2_aes,M0_aes)

summary(M1_aes)
summary(Rec_aes)
df<-data.frame(mean_AGB=seq(0,396,by=1))

df$Aesthetic<-((plogis(predict(M1_aes,newdata = df,re.form=NA)))*4)+1

Aes_sum<-ddply(Rec_aes,.(Aesthetic,mean_AGB),summarise,count_un=length(ID))

#plot results
Aes_plot1<-ggplot(Aes_sum,aes(x=mean_AGB,y=Aesthetic,size=count_un))+geom_point(shape=1)+geom_line(data=df,size=1)
Aes_plot2<-Aes_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),legend.position="none")
Aes_plot3<-Aes_plot2+ylab("Aesthetic appreciation rating")+xlab("Aboveground biomass")

pdf("Figures/Rec_aes.pdf",width = 8,height = 4)
grid.arrange(Rec_plot3,Aes_plot3,ncol=2)

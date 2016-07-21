#script to analyse the change in resilience of biodiversity and ecosystem services 
#over a gradient of increasing disturbance for Elena's work
library(plyr)
library(reshape2)
library(MuMIn)
library(ggplot2)
library(lme4)

#load functions
std <- function(x) sd(x)/sqrt(length(x))


Resistence_data<-read.csv("Data/R_output/Resistence_replicates.csv")

colnames(Resistence)
head(Resistence_data)

Resistence_summary<-ddply(Resistence,.(Scen_lab,Replicate,Scen_lab2,variable),summarise,m_value=mean(Resistance))
Pulse<-subset(Resistence_summary,Scen_lab2=="Pulse")
Pulse_press<-subset(Resistence_summary,Scen_lab2=="Pulse + Press")

Pulse$Diff<-Pulse$m_value-Pulse_press$m_value

(mean(Pulse$Diff))*100
std(Pulse$Diff)*100


#run regressions for all the variables for resistance to see if the slopes of pulse and pulse+presss are the same
ggplot(Resistence_data,aes(x=Scen_lab,y=Resistance,colour=as.factor(Scen_lab2)))+geom_point()+facet_wrap(~variable)


Aesthetic_sub<-subset(Resistence_data,variable=="Aesthetic_M")
Aesthetic_sub$logis_Res<-plogis(Aesthetic_sub$Resistance)

M0<-lmer(logis_Res~1+(1|Replicate),data=Aesthetic_sub)
M1<-lmer(logis_Res~Scen_lab+(1|Replicate),data=Aesthetic_sub)
M2<-lmer(logis_Res~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Aesthetic_sub)
M3<-lmer(logis_Res~Scen_lab*Scen_lab2+(1|Replicate),data=Aesthetic_sub)
M4<-lmer(logis_Res~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Aesthetic_sub)

AICc(M0,M1,M2,M3,M4)


#to do this I need to run all models for each of the different variables reporting the the model with the lowest AICc value

ES_BD_unique<-unique(Resistence_data$variable)

for(i in 1:length(ES_BD_unique)){
  i<-1
  Res_sub<-subset(Resistence_data,variable==ES_BD_unique[i])
  M0<-lmer(logis_Res~1+(1|Replicate),data=Res_sub)
  M1<-lmer(logis_Res~Scen_lab+(1|Replicate),data=Res_sub)
  M2<-lmer(logis_Res~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Res_sub)
  M3<-lmer(logis_Res~Scen_lab*Scen_lab2+(1|Replicate),data=Res_sub)
  M4<-lmer(logis_Res~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Res_sub)
  
}
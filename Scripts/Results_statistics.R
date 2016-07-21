#script to analyse the change in resilience of biodiversity and ecosystem services 
#over a gradient of increasing disturbance for Elena's work
library(plyr)
library(reshape2)
library(MuMIn)
library(ggplot2)
library(lme4)

#load functions
std <- function(x) sd(x)/sqrt(length(x))

#load data and transform resistence variable to conform to model assumptions 
Resistence_data<-read.csv("Data/R_output/Resistence_replicates.csv")
Resistence_data$logis_Res<-plogis(Resistence_data$Resistance)

#to do this I need to run all models for each of the different variables reporting the the model with the lowest AICc value

ES_BD_unique<-unique(Resistence_data$variable)
Top_model_summary<-NULL
for(i in 1:length(ES_BD_unique)){
  #i<-1
  Res_sub<-subset(Resistence_data,variable==ES_BD_unique[i])
  M0<-lmer(logis_Res~1+(1|Replicate),data=Res_sub)
  M1<-lmer(logis_Res~Scen_lab+(1|Replicate),data=Res_sub)
  M2<-lmer(logis_Res~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Res_sub)
  M3<-lmer(logis_Res~Scen_lab*Scen_lab2+(1|Replicate),data=Res_sub)
  M4<-lmer(logis_Res~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Res_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i],AICc(M0,M1,M2,M3,M4),
             Model=c("Null", "Linear", "Linear interaction","Non-linear","Non-linear interaction"))
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

write.csv(Top_model_summary,"Tables/Resistance_top_models.csv",row.names = F)


#now do the same for persistence data

#load data and transform resistence variable to conform to model assumptions 
Persistence_data<-read.csv("Data/R_output/Persistence_replicates.csv")
Persistence_data$logis_Per<-plogis(Persistence_data$Resistance2)


ES_BD_unique<-unique(Persistence_data$variable)
Top_model_summary<-NULL
for(i in 1:length(ES_BD_unique)){
  #i<-1
  Per_sub<-subset(Persistence_data,variable==ES_BD_unique[i])
  M0<-lmer(logis_Per~1+(1|Replicate),data=Per_sub)
  M1<-lmer(logis_Per~Scen_lab+(1|Replicate),data=Per_sub)
  M2<-lmer(logis_Per~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Per_sub)
  M3<-lmer(logis_Per~Scen_lab*Scen_lab2+(1|Replicate),data=Per_sub)
  M4<-lmer(logis_Per~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Per_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i],AICc(M0,M1,M2,M3,M4),
                              Model=c("Null", "Linear", "Linear interaction","Non-linear","Non-linear interaction"))
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

write.csv(Top_model_summary,"Tables/Persistence_top_models.csv",row.names = F)


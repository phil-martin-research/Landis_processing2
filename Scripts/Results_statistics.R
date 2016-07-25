#script to analyse the change in resilience of biodiversity and ecosystem services 
#over a gradient of increasing disturbance for Elena's work
library(plyr)
library(reshape2)
library(MuMIn)
library(ggplot2)
library(lme4)

#load functions
std <- function(x) sd(x)/sqrt(length(x))

#load data on resistance and transform resistence variable to conform to model assumptions 
Resistence_data<-read.csv("Data/R_output/Resistence_replicates.csv")
Resistence_data$logis_Res<-qlogis(Resistence_data$Resistance-0.0001)
Resistence_data$Scen_lab_trans<-(Resistence_data$Scen_lab-mean(Resistence_data$Scen_lab))/sd(Resistence_data$Scen_lab)

#to do this I need to run all models for each of the different variables reporting the the model with the lowest AICc value
head(Resistence_data)
ES_BD_unique<-unique(Resistence_data$ESLab)
Top_model_summary<-NULL
for(i in 1:length(ES_BD_unique)){
  Res_sub<-subset(Resistence_data,ESLab==ES_BD_unique[i])
  M0<-lmer(logis_Res~1+(1|Replicate),data=Res_sub)
  M1<-lmer(logis_Res~Scen_lab_trans+(1|Replicate),data=Res_sub)
  M2<-lmer(logis_Res~Scen_lab_trans*Scen_lab2+(1|Replicate),data=Res_sub)

  Model_selection<-data.frame(Variable=ES_BD_unique[i],AICc(M0,M1,M2),
                              Intercept=round(c(coef(summary(M0))[1,1],coef(summary(M1))[1,1],coef(summary(M2))[1,1]),3),
                              Slope=round(c(NA,coef(summary(M1))[2,1],coef(summary(M2))[2,1]),3),
                              Interaction=c(NA,NA,coef(summary(M2))[3,1]),
             R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2),
             Model=c("Null", "Linear", "Linear interaction"))
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

Top_model_summary2<-Top_model_summary[with(Top_model_summary, order(Variable)), ]

write.csv(Top_model_summary2,"Tables/Resistance_top_models.csv",row.names = F)


################################################################
#this section does the same as above but for recovery results
################################################################

#load data on persistence and transform persistence variable to conform to model assumptions 
Recovery_data<-read.csv("Data/R_output/Recovery_replicates.csv")
Recovery_data_sub<-subset(Recovery_data,Scen_lab<=60)
Recovery_data_sub$logis_Per<-qlogis((Recovery_data_sub$Time/100)-0.001)
Recovery_data_sub$Scen_lab_trans<-(Recovery_data_sub$Scen_lab-mean(Recovery_data_sub$Scen_lab))/sd(Recovery_data_sub$Scen_lab)

ES_BD_unique<-unique(Recovery_data$ESLab)
Top_model_summary<-NULL
for(i in 1:length(ES_BD_unique)){
  i<-1
  Rec_sub<-subset(Recovery_data_sub,ESLab==ES_BD_unique[i])
  M0<-lmer(logis_Per~1+(1|Replicate),data=Rec_sub)
  M1<-lmer(logis_Per~Scen_lab+(1|Replicate),data=Rec_sub)
  M2<-lmer(logis_Per~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Rec_sub)
  M3<-lmer(logis_Per~Scen_lab*Scen_lab2+(1|Replicate),data=Rec_sub)
  M4<-lmer(logis_Per~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Rec_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i],
                              AICc(M0,M1,M2,M3,M4),
                              Intercept=round(c(coef(summary(M0))[1,1],coef(summary(M1))[1,1],coef(summary(M2))[1,1],coef(summary(M3))[1,1],coef(summary(M4))[1,1]),3),
                              Slope=round(c(NA,coef(summary(M1))[2,1],coef(summary(M2))[2,1],coef(summary(M3))[3,1],coef(summary(M4))[2,1]),3),
                              Interaction=round(c(NA,NA,NA,coef(summary(M3))[3,1],coef(summary(M4))[3,1]),3),
                              Quadratic=round(c(NA,NA,coef(summary(M2))[3,1],NA,coef(summary(M4))[4,1]),3),
                              Quadratic_interaction=c(NA,NA,NA,NA,coef(summary(M4))[5,1]),
                              R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2),
                              Model=c("Null", "Linear", "Linear interaction"))
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

write.csv(Top_model_summary,"Tables/Recovery_top_models.csv",row.names = F)





################################################################
#this section does the same as above but for persistence results
################################################################

#load data on persistence and transform persistence variable to conform to model assumptions 
Persistence_data<-read.csv("Data/R_output/Persistence_replicates.csv")
Persistence_data$logis_Per<-plogis(Persistence_data$Resistance2)


ES_BD_unique<-unique(Persistence_data$variable)
Top_model_summary<-NULL
for(i in 1:length(ES_BD_unique)){
  Per_sub<-subset(Persistence_data,variable==ES_BD_unique[i])
  M0<-lmer(logis_Per~1+(1|Replicate),data=Per_sub)
  M1<-lmer(logis_Per~Scen_lab+(1|Replicate),data=Per_sub)
  M2<-lmer(logis_Per~Scen_lab+I(Scen_lab^2)+(1|Replicate),data=Per_sub)
  M3<-lmer(logis_Per~Scen_lab*Scen_lab2+(1|Replicate),data=Per_sub)
  M4<-lmer(logis_Per~Scen_lab*Scen_lab2+I(Scen_lab^2)*Scen_lab2+(1|Replicate),data=Per_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i],
                              round(AICc(M0,M1,M2,M3,M4),2),
                              R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M3)[1],r.squaredGLMM(M4)[1]),2),
                              Model=c("Null", "Linear", "Linear interaction","Non-linear","Non-linear interaction"))
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

write.csv(Top_model_summary,"Tables/Persistence_top_models.csv",row.names = F)


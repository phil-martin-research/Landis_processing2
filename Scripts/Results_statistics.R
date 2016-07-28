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
ES_BD_unique<-expand.grid(unique(Resistence_data$ESLab),unique(Resistence_data$Scen_lab2),stringsAsFactors = F)
ES_BD_unique$Order<-rep(c(13,11,9,12,3,8,2,5,6,4,7,10,1,14),2)

Top_model_summary<-NULL
for(i in 1:nrow(ES_BD_unique)){
  ggplot(Res_sub,aes(x=Scen_lab_trans,y=Resistance))+geom_point()
  Res_sub<-subset(Resistence_data,ESLab==ES_BD_unique[i,1]&Scen_lab2==ES_BD_unique[i,2])
  M0<-lmer(Resistance~1+(1|Replicate),data=Res_sub)
  M1<-lmer(Resistance~Scen_lab_trans+(1|Replicate),data=Res_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i,1],
                              Pulse=ES_BD_unique[i,2],
                              Model=c("Null", "Linear"),
                              round(AICc(M0,M1),2),
                              Intercept=round(c(coef(summary(M0))[1,1],coef(summary(M1))[1,1]),3),
                              Slope=round(c(NA,coef(summary(M1))[2,1]),3),
                              R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1]),2),
                              Order=ES_BD_unique[i,3][1])
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

Top_model_summary_sort<-Top_model_summary[with(Top_model_summary, order(Order)), ]

write.csv(Top_model_summary_sort,"Tables/Resistance_top_models.csv",row.names = F)


################################################################
#this section does the same as above but for recovery results
################################################################

#load data on persistence and transform recovery variable to conform to model assumptions 
Recovery_data<-read.csv("Data/R_output/Recovery_replicates.csv")
Recovery_data_sub<-subset(Recovery_data,Scen_lab<=60)
Recovery_data_sub$logis_Per<-qlogis((Recovery_data_sub$Time/100)-0.001)
Recovery_data_sub$Scen_lab_trans<-(Recovery_data_sub$Scen_lab-mean(Recovery_data_sub$Scen_lab))/sd(Recovery_data_sub$Scen_lab)

ES_BD_unique<-expand.grid(unique(Recovery_data$ESLab),unique(Recovery_data$Scen_lab2),stringsAsFactors = F)
ES_BD_unique$Order<-rep(c(1,10,4,6,5,2,8,3,12,9,11,13,7),2)


Top_model_summary<-NULL
for(i in 1:nrow(ES_BD_unique)){
  Rec_sub<-subset(Recovery_data_sub,ESLab==ES_BD_unique[i,1]&Scen_lab2==ES_BD_unique[i,2])
  M0<-lmer(Time~1+(1|Replicate),data=Rec_sub)
  M1<-lmer(Time~Scen_lab+(1|Replicate),data=Rec_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i,1],
                              Pulse=ES_BD_unique[i,2],
                              Model=c("Null", "Linear"),
                              round(AICc(M0,M1),2),
                              Intercept=round(c(coef(summary(M0))[1,1],coef(summary(M1))[1,1]),3),
                              Slope=round(c(NA,coef(summary(M1))[2,1]),3),
                              R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1]),2),
                              Order=ES_BD_unique[i,3][1])
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

Top_model_summary_sort<-Top_model_summary[with(Top_model_summary, order(Order)), ]


write.csv(Top_model_summary_sort,"Tables/Recovery_top_models.csv",row.names = F)


#this section is the same as above but looking at persistence

#load data on resistance and transform resistence variable to conform to model assumptions 
Persistence_data<-read.csv("Data/R_output/Persistence_replicates.csv")
Persistence_data$logis_Res<-qlogis(Persistence_data$Resistance2-0.0001)
ggplot(Persistence_data,aes(x=Scen_lab_trans,y=logis_Res,colour=Scen_lab2))+geom_point()+facet_wrap(~ESLab)

Persistence_data$Scen_lab_trans<-(Persistence_data$Scen_lab-mean(Persistence_data$Scen_lab))/sd(Persistence_data$Scen_lab)

#to do this I need to run all models for each of the different variables reporting the the model with the lowest AICc value
ES_BD_unique<-expand.grid(unique(Persistence_data$ESLab),unique(Persistence_data$Scen_lab2),stringsAsFactors = F)
ES_BD_unique$Order<-rep(c(1,10,7,4,6,5,2,8,3,12,9,11,13),2)


Top_model_summary<-NULL
for(i in 1:nrow(ES_BD_unique)){
  Res_sub<-subset(Persistence_data,ESLab==ES_BD_unique[i,1]&Scen_lab2==ES_BD_unique[i,2])
  M0<-lmer(Resistance2~1+(1|Replicate),data=Res_sub)
  M1<-lmer(Resistance2~Scen_lab_trans+(1|Replicate),data=Res_sub)
  M2<-lmer(Resistance2~Scen_lab_trans+I(Scen_lab_trans^2)+(1|Replicate),data=Res_sub)
  Model_selection<-data.frame(Variable=ES_BD_unique[i,1],
                              Pulse=ES_BD_unique[i,2],
                              Model=c("Null", "Linear","Quadratic"),
                              round(AICc(M0,M1,M2),2),
                              Intercept=round(c(coef(summary(M0))[1,1],coef(summary(M1))[1,1],coef(summary(M2))[1,1]),3),
                              Slope=round(c(NA,coef(summary(M1))[2,1],coef(summary(M2))[2,1]),3),
                              Slope_sq=round(c(NA,NA,coef(summary(M2))[3,1]),3),
                              R_sq=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2),
                              Order=ES_BD_unique[i,3][1])
  Model_selection_sorted<-Model_selection[with(Model_selection, order(AICc)), ]
  Top_model<-Model_selection_sorted[1,]
  Top_model_summary<-rbind(Top_model_summary,Top_model)
}

Top_model_summary_sort<-Top_model_summary[with(Top_model_summary, order(Order)), ]

write.csv(Top_model_summary_sort,"Tables/Persistence_top_models.csv",row.names = F)

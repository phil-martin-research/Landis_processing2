#script to run regressions on relationship between biomass and 
#biodivesrity and ecosystem service metrics based on Paul's gradient plots

#author: Phil martin
#Date 2016/04/14

#open packages
library(ggplot2)
library(lme4)
library(reshape)
library(reshape2)
library(plyr)
library(MuMIn)


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
Sel_table_summary<-NULL
for (i in 4:5){
  M1<-lmer(BM[[i]]~AGB_std+(1|Site),data=BM)
  M2<-lmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
  M0<-lmer(BM[[i]]~1+(1|Site),data=BM)
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=round(summary(Mod_average)$coefmat.full[1,1],2),
                                 Intercept_SE=round(summary(Mod_average)$coefmat.full[1,2],2),
                                 Intercept_P=round(summary(Mod_average)$coefmat.full[1,5],3),
                                 AGB=round(summary(Mod_average)$coefmat.full[2,1],2),
                                 AGB_SE=round(summary(Mod_average)$coefmat.full[2,2],2),
                                 AGB_P=round(summary(Mod_average)$coefmat.full[2,5],3),
                                 AGB_sq=round(summary(Mod_average)$coefmat.full[3,1],2),
                                 AGB_sq_SE=round(summary(Mod_average)$coefmat.full[3,2],2),
                                 AGB_sq_P=round(summary(Mod_average)$coefmat.full[3,5],3)))
  Sel_table<-data.frame(Var=colnames(BM[i]),Model=c("Null","Linear","Quadratic"),
             AICc=round(AICc(M0,M1,M2)[,2],2),
             logLik=round(c(logLik(M0),logLik(M1),logLik(M2)),2),
               R2=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2))
  
  Sel_table$delta<-round(Sel_table$AICc-min(Sel_table$AICc),2)
  Sel_table$AIC_w<-round((exp(-0.5*(Sel_table$delta)))/(sum(exp(-0.5*(Sel_table$delta)))),2)
  Sel_table<-Sel_table[with(Sel_table, order(AICc)), ]
  Sel_table_summary<-rbind(Sel_table_summary,Sel_table)
  
  }

#and then for count data - fungi, ground flora and lichen species richness
for (i in 6:9){
  M1<-glmer(BM[[i]]~AGB_std+(1|Site),data=BM,family="poisson")
  M2<-glmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
  M0<-glmer(BM[[i]]~1+(1|Site),data=BM,family="poisson")
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=round(summary(Mod_average)$coefmat.full[1,1],2),
                                 Intercept_SE=round(summary(Mod_average)$coefmat.full[1,2],2),
                                 Intercept_P=round(summary(Mod_average)$coefmat.full[1,5],3),
                                 AGB=round(summary(Mod_average)$coefmat.full[2,1],2),
                                 AGB_SE=round(summary(Mod_average)$coefmat.full[2,2],2),
                                 AGB_P=round(summary(Mod_average)$coefmat.full[2,5],3),
                                 AGB_sq=round(summary(Mod_average)$coefmat.full[3,1],2),
                                 AGB_sq_SE=round(summary(Mod_average)$coefmat.full[3,2],2),
                                 AGB_sq_P=round(summary(Mod_average)$coefmat.full[3,5],3)))
  Sel_table<-data.frame(Var=colnames(BM[i]),Model=c("Null","Linear","Quadratic"),
                        AICc=round(AICc(M0,M1,M2)[,2],2),
                        logLik=round(c(logLik(M0),logLik(M1),logLik(M2)),2),
                        R2=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2))
  
  Sel_table$delta<-round(Sel_table$AICc-min(Sel_table$AICc),2)
  Sel_table$AIC_w<-round((exp(-0.5*(Sel_table$delta)))/(sum(exp(-0.5*(Sel_table$delta)))),2)
  Sel_table<-Sel_table[with(Sel_table, order(AICc)), ]
  Sel_table_summary<-rbind(Sel_table_summary,Sel_table)
  
}

for (i in c(7,9)){
  M1<-lmer(Rec_aes[[i]]~mean_AGB+(1|ID),data=Rec_aes)
  M2<-lmer(Rec_aes[[i]]~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
  M0<-lmer(Rec_aes[[i]]~1+(1|ID),data=Rec_aes)
  Mod_average<-model.avg(M1,M2,M0)
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(Rec_aes[i]),
                      Intercept=round(summary(Mod_average)$coefmat.full[1,1],3),
                      Intercept_SE=round(summary(Mod_average)$coefmat.full[1,2],3),
                      Intercept_P=round(summary(Mod_average)$coefmat.full[1,5],3),
                      AGB=round(summary(Mod_average)$coefmat.full[2,1],3),
                      AGB_SE=round(summary(Mod_average)$coefmat.full[2,2],3),
                      AGB_P=round(summary(Mod_average)$coefmat.full[2,5],3),
                      AGB_sq=round(summary(Mod_average)$coefmat.full[3,1],3),
                      AGB_sq_SE=round(summary(Mod_average)$coefmat.full[3,2],3),
                      AGB_sq_P=round(summary(Mod_average)$coefmat.full[3,5],3)))
  Sel_table<-data.frame(Var=colnames(Rec_aes[i]),Model=c("Null","Linear","Quadratic"),
                        AICc=round(AICc(M0,M1,M2)[,2],2),
                        logLik=round(c(logLik(M0),logLik(M1),logLik(M2)),2),
                        R2=round(c(r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1]),2))
  
  Sel_table$delta<-round(Sel_table$AICc-min(Sel_table$AICc),2)
  Sel_table$AIC_w<-round((exp(-0.5*(Sel_table$delta)))/(sum(exp(-0.5*(Sel_table$delta)))),2)
  Sel_table<-Sel_table[with(Sel_table, order(AICc)), ]
  Sel_table_summary<-rbind(Sel_table_summary,Sel_table)
  
}



#tidy data
rownames(Coefficients)<-NULL


write.csv(Sel_table_summary,"Tables/Model_selection.csv",row.names=F)
write.csv(Coefficients,"Tables/Model_coefficients.csv",row.names=F)

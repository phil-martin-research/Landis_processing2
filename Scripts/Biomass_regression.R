#script to produce regressions of ecosystem services/functions vs biomass
#fruom Paul's gradient plots

#author: Phil martin
#Date 2015/09/24

#open packages
library(raster)
library(ggplot2)
library(lme4)
library(reshape2)
library(plyr)
library(MuMIn)


#load in Paul's biomass data
BM<-read.csv("Data/BMLan.csv",header = T)
BM<-subset(BM,AGB<450) # remove very large biomass value
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass


#run a loop to test each variable and produce predictions
df<-NULL
df<-data.frame(AGB_std=seq(-1.35,1.8,0.01))
df$AGB<-(df$AGB_std*sd(BM$AGB))+mean(BM$AGB)
df<-data.frame(df,SRR=NA,Min_rate=NA,Fungi=NA,GF=NA,Lichen=NA)
Coefficients<-NULL
for (i in 4:5){
  M1<-lmer(BM[[i]]~AGB_std+(1|Site),data=BM)
  M2<-lmer(BM[[i]]~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
  M0<-lmer(BM[[i]]~1+(1|Site),data=BM)
  Mod_average<-model.avg(M1,M2,M0)
  df[i-1]<-(predict(Mod_average,re.form=NA,newdata=df))
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
  df[i-1]<-exp(predict(Mod_average,re.form=NA,newdata=df))
  Coefficients<-rbind(Coefficients,
                      data.frame(Var=colnames(BM[i]),
                                 Intercept=coef(Mod_average)[1],
                                 AGB=coef(Mod_average)[2],
                                 AGB_sq=coef(Mod_average)[3]))
}


BM_melt<-melt(BM,id.vars = c("Site","Plot","AGB_std","AGB"))
df_melt<-melt(df,id.vars = c("AGB_std","AGB"))


#now plot all predictions
theme_set(theme_bw(base_size=12))
P1<-ggplot(BM_melt,aes(x=AGB,y=value,group=Site))+geom_point(size=3,shape=1)+facet_wrap(~variable,scales = "free_y")
P1+geom_line(data=df_melt,aes(group=NULL),size=2)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                           
ggsave("Figures/AGB_regressions.pdf",dpi=400,height=6,width=8,units='in')

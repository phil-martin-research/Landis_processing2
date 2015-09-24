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
library(grid)
library(gridExtra)


#load in Paul's biomass data
BM<-read.csv("Data/BMLan.csv",header = T)
BM<-subset(BM,AGB<450) # remove very large biomass value
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass


#model these relationships statistically
#first soil respiration rate
M1<-lmer(SRR~AGB_std+(1|Site),data=BM)
M2<-lmer(SRR~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
AICc(M1,M2)

Mod_average<-model.avg(M1,M2)

df<-data.frame(AGB_std=seq(-1.35,1.8,0.01))
df$AGB<-(df$AGB_std*sd(BM$AGB))+mean(BM$AGB)
df$SRR<-(predict(Mod_average,re.form=NA,newdata=df))

r.squaredGLMM(M2)


#net mineralisation
M1<-lmer(Min_rate~AGB_std+(1|Site),data=BM)
M2<-lmer(Min_rate~AGB_std+I(AGB_std^2)+(1|Site),data=BM)
AICc(M1,M2)

Mod_average<-model.avg(M1,M2)

df$Min_rate<-(predict(Mod_average,re.form=NA,newdata=df))

r.squaredGLMM(M2)

#fungi
M1<-glmer(Fungi~AGB_std+(1|Site),data=BM,family="poisson")
M2<-glmer(Fungi~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
AICc(M1,M2)

Mod_average<-model.avg(M1,M2)

df$Fungi<-exp(predict(Mod_average,re.form=NA,newdata=df))

#ground flora
M1<-glmer(GF~AGB_std+(1|Site),data=BM,family="poisson")
M2<-glmer(GF~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
AICc(M1,M2)

Mod_average<-model.avg(M1,M2)

df$GF<-exp(predict(Mod_average,re.form=NA,newdata=df))


#lichen richness
M1<-glmer(Lichen~AGB_std+(1|Site),data=BM,family="poisson")
M2<-glmer(Lichen~AGB_std+I(AGB_std^2)+(1|Site),data=BM,family="poisson")
AICc(M1,M2)

Mod_average<-model.avg(M1,M2)

df$Lichen<-exp(predict(Mod_average,re.form=NA,newdata=df))

BM_melt<-melt(BM,id.vars = c("Site","Plot","AGB_std","AGB"))
df_melt<-melt(df,id.vars = c("AGB_std","AGB"))


#now plot all predictions
P1<-ggplot(BM_melt,aes(x=AGB,y=value,group=Site))+geom_point(size=3,shape=1)+facet_wrap(~variable,scales = "free_y")
P1+geom_line(data=df_melt,aes(group=NULL),size=2)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                           
ggsave("Figures/AGB_regressions.pdf",dpi=400,height=6,width=8,units='in')

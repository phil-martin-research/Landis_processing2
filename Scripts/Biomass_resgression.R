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
head(BM)

#look at relationships between biomass and variables
theme_set(theme_bw(base_size=12))
P1<-ggplot(melt(BM,id.vars = c("Plot","Site","AGB")),aes(x=AGB,y=value,group=Site))+geom_point(shape=1)+geom_line(alpha=0.2)+facet_wrap(~variable,scales = "free_y")
P1+geom_smooth(aes(group=NULL),method="lm")
#some relationships appear to much stronger than others

#model these relationships statistically
#first soil respiration rate
M1<-lmer(SRR~AGB+(1|Site),data=BM)
BM$SSR_pred<-predict(M1,re.form=NA)
r.squaredGLMM(M1)

P1<-ggplot(BM,aes(x=AGB,y=SRR,group=Site))+geom_point(shape=1)+geom_line(alpha=0.1)+geom_line(aes(y=SSR_pred,group=NULL),size=2)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P2+xlab(expression(paste("Aboveground biomass (Mg ", ha^-1,")",sep="")))+
  ylab(expression(paste("Soil respiration rate","(",mu,mol," ",m^2, " ", s^-1,")")))
ggsave("Figures/Soil_resp_AGB.pdf",width=6,height=4,dpi=400,units="in")

#net mineralisation
M1<-lmer(Min_rate~AGB+(1|Site),data=BM)
df<-data.frame(AGB=seq(0,757,1))
df$Min_rate<-predict(M1,newdata=df,re.form=NA)
r.squaredGLMM(M1)# this is very low

P1<-ggplot(BM,aes(x=AGB,y=Min_rate,group=Site))+geom_point(shape=1)+geom_line(alpha=0.1)+geom_line(data=df,aes(group=NULL),size=2)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P2+xlab(expression(paste("Aboveground biomass (Mg ", ha^-1,")",sep="")))+
  ylab(expression(paste("Net mineralisation in situ ","(",NO[3]," + ",NH[4]," ",capsule^-1,")")))
ggsave("Figures/Min_rate_AGB.pdf",width=6,height=4,dpi=400,units="in")

#fungi
M1<-lmer(Fungi~AGB+(1|Site),data=BM)
BM$Fungi_pred<-predict(M1,re.form=NA)
r.squaredGLMM(M1)

P1<-ggplot(BM,aes(x=AGB,y=Fungi,group=Site))+geom_point(shape=1)+geom_line(alpha=0.1)+geom_line(aes(y=Fungi_pred,group=NULL),size=2)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P2+xlab(expression(paste("Aboveground biomass (Mg ", ha^-1,")",sep="")))+
  ylab("Fungi species richness")
ggsave("Figures/Fungi_AGB.pdf",width=6,height=4,dpi=400,units="in")


#ground flora
M1<-lmer(GF~AGB+(1|Site),data=BM)
BM$GF_pred<-predict(M1,re.form=NA)
r.squaredGLMM(M1)

P1<-ggplot(BM,aes(x=AGB,y=GF,group=Site))+geom_point(shape=1)+geom_line(alpha=0.1)+geom_line(aes(y=GF_pred,group=NULL),size=2)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P2+xlab(expression(paste("Aboveground biomass (Mg ", ha^-1,")",sep="")))+
  ylab("Ground flora species richness")
ggsave("Figures/GF_AGB.pdf",width=6,height=4,dpi=400,units="in")

#lichen richness
M1<-lmer(Lichen~AGB+(1|Site),data=BM)
BM$Lichen_pred<-predict(M1,re.form=NA)
r.squaredGLMM(M1)

P1<-ggplot(BM,aes(x=AGB,y=Lichen,group=Site))+geom_point(shape=1)+geom_line(alpha=0.1)+geom_line(aes(y=Lichen_pred,group=NULL),size=2)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P2+xlab(expression(paste("Aboveground biomass (Mg ", ha^-1,")",sep="")))+
  ylab("Lichen species richness")
ggsave("Figures/Lichen_AGB.pdf",width=6,height=4,dpi=400,units="in")

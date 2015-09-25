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

#load in standard error function
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

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

#tidy data for plotting
rownames(Coefficients)<-NULL
BM_melt<-melt(BM,id.vars = c("Site","Plot","AGB_std","AGB"))
df_melt<-melt(df,id.vars = c("AGB_std","AGB"))


#now plot all predictions
theme_set(theme_bw(base_size=12))
P1<-ggplot(BM_melt,aes(x=AGB,y=value,group=Site))+geom_point(size=3,shape=1)+facet_wrap(~variable,scales = "free_y")
P1+geom_line(data=df_melt,aes(group=NULL),size=2)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                           
ggsave("Figures/AGB_regressions.pdf",dpi=400,height=6,width=8,units='in')

###################################################
#run this section to use coefficients to predict ##
#values for landscape scale Landis models##########
###################################################

#load data Landis II outputs
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("*.dbf",File_names)]
File_names<-File_names[grepl("TotalBiomass",File_names)]

#for each replicate of each scenario predict values of each ecosystem service
#or biodiversity value and output this as a raster and summarise values as 
#the mean pixel value, with stadard deviation and standard error
j<-1
i<-1
n<-0
Mean_summary<-NULL
for (j in 1:nrow(Coefficients)){
for (i in 1:length(File_names)){
  Biomass<-raster(File_names[i])
  Biomass[Biomass==0]<-NA #remove all zeros from raster and replace with NAs
  Prediction<-((((((Biomass)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
          (((((Biomass)/100)-mean(BM$AGB))/sd(BM$AGB))^2)*Coefficients[j,4])+Coefficients[j,2])
  
  #The bit below creates a new data frame summarising each raster to give the mean cell value,
  #the standard deviation of this, the variable (e.g. ground flora richness), the year modelled
  #the model replicate and the scenario number
  Mean_sub<-data.frame(Mean=cellStats(Prediction,'mean',na.rm=T),
  Std_dev=cellStats(Prediction,'sd',na.rm=T),
  Var=Coefficients[j,1],
  Year=as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i]))),
  Replicate=gsub( "/TotalBiomass.*$", "", gsub("^.*?_r","", File_names[i])),
  Scenario=gsub( "_r.*$", "", gsub("^.*?-biomass","", File_names[i]))
  )
  Mean_summary<-rbind(Mean_sub,Mean_summary) #this binds all iterations together
  
  
  #the bit below produces a new .img raster in the directory Data/Maps_from_R/
  #the file names are in the format Variablename_scenarionumber_replicate_year.img
  #e.g.Lichen_1_1_170.img
  writeRaster(Prediction,
   filename=paste("Data/Maps_from_R/",
   Coefficients[j,1],"_",
   gsub( "_r.*$", "", gsub("^.*?-biomass","", File_names[i]))
   ,"_",
   gsub( "/TotalBiomass.*$", "", gsub("^.*?_r","", File_names[i])),"_",
   Year=as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i]))),
   ".img",
   sep = ""),
   format="HFA",overwrite=T)
   plot(Prediction)
   n<-n+1
   print(paste("percent done=", n/(210*5),"%"))
}
}




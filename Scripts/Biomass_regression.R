#script to produce regressions of ecosystem services/functions vs biomass
#from Paul's gradient plots

#author: Phil martin
#Date 2015/09/24


setInternet2(TRUE)
install.packages('MuMIn')
install.packages("raster")
install.packages("ggplot2")
install.packages("lme4")
install.packages("reshape2")
install.packages("plyr")
install.packages("gtools")
install.packages("rgdal")
install.packages("tidyr")


#open packages
library(raster)
library(ggplot2)
library(lme4)
library(reshape2)
library(plyr)
library(MuMIn)
library(gtools)
library(tidyr)
library(rgdal)

#clear previous R objects
rm(list=ls())

#load in Paul's biomass data
BM<-read.csv("Data/BMLan.csv",header = T)
BM<-subset(BM,AGB<600)
BM$AGB_std<-(BM$AGB-mean(BM$AGB))/sd(BM$AGB) #standardise biomass

summary(BM$AGB_std)

#run a loop to test each variable and produce predictions
df<-NULL
df<-data.frame(AGB_std=seq(-1.3,3.0,0.01))
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
File_names<-rev(mixedsort(File_names))

#a loop to check what is going on with biomass
BM<-NULL
for (i in 1:length(File_names)){
  Biomass<-raster(File_names[i])
  Biomass[Biomass==0]<-NA
  BM_freq<-data.frame(freq(Biomass/100))
  BM_freq$bin<-cut(BM_freq$value,seq(0,600,by = 10),labels=as.numeric(seq(10,600,by=10)))
  BM_freq$bin<-as.numeric(as.character(BM_freq$bin))
  BM_sum<-ddply(BM_freq,.(bin),summarise,AGB=sum(count))
  BM_sum$Scenario<-paste("Sc =",gsub( "_r.*$", "", gsub("^.*?-biomass","", File_names[i])))
  BM_sum$Replicate<-paste("Replicate =",gsub( "/TotalBiomass.*$", "", gsub("^.*?_r","", File_names[i])))
  BM_sum$Year<-paste("Year =",as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i]))))
  BM_sum$Year2<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i])))
  BM_sum$Mean<-cellStats(Biomass,'mean',na.rm=T)
  BM_sum<-subset(BM_sum,!is.na(bin))
  BM<-rbind(BM,BM_sum)
}


BM_summary<-ddply(BM,.(Scenario,Year,bin,Year2),summarise,pixel_count=sum(AGB)/3,mean_AGB=mean(Mean/100))
head(BM_summary)
BM_summary2<-subset(BM_summary,Year2<=100)
BM_summary2$Year3<-factor(BM_summary2$Year,
                    c("Year = 0","Year = 10","Year = 20","Year = 30","Year = 40","Year = 50",
                      "Year = 60","Year = 70","Year = 80","Year = 90","Year = 100"))

BM_summary4<-ddply(BM_summary2,.(Scenario,Year3),summarise,mean_AGB=sum(mean_AGB*pixel_count)/sum(pixel_count),pixel_count2=sum(pixel_count))
head(BM_summary4)

theme_set(theme_bw(base_size=12))
P1<-ggplot(BM_summary2,aes(x=bin,y=pixel_count))+geom_bar(stat = "identity",fill="grey")+geom_vline(data=BM_summary4,aes(xintercept=mean_AGB),lty=2,size=0.5)+facet_grid(Scenario~Year3)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Number of pixels")+xlab("Aboveground biomass")
ggsave("Figures/landis_histogram.pdf",dpi = 400,height=6,width=18,units="in")


#for each replicate of each scenario predict values of each ecosystem service
#or biodiversity value and output this as a raster and summarise values as 
#the mean pixel value, with stadard deviation and standard error


n<-0
ptm <- proc.time()
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
  AGB=cellStats(Biomass,'mean',na.rm=T),
  Var=Coefficients[j,1],
  Year=as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i]))),
  Replicate=gsub( "/TotalBiomass.*$", "", gsub("^.*?_r","", File_names[i])),
  Scenario=gsub( "_r.*$", "", gsub("^.*?-biomass","", File_names[i]))
  )
  Mean_summary<-rbind(Mean_sub,Mean_summary) #this binds all iterations together
  
  #the bit below produces a new .img raster in the directory Data/Maps_from_R/
  #the file names are in the format Variablename_scenarionumber_replicate_year.img
  #e.g.Lichen_1_1_170.img
  #currently this is not functional
  #writeRaster(Prediction,
   #filename=paste("Data/Maps_from_R/",
   #Coefficients[j,1],"_",
   #gsub( "_r.*$", "", gsub("^.*?-biomass","", File_names[i]))
   #,"_",
   #gsub( "/TotalBiomass.*$", "", gsub("^.*?_r","", File_names[i])),"_",
   #Year=as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?Biomass-","", File_names[i]))),
   #".img",
   #sep = ""),
   #format="HFA",overwrite=T)
   
   #this bit of code just tells you how much of the loop is done
   n<-n+1
   print(paste("percent done=", (n/(210*5))*100,"%"))
}
}
proc.time() - ptm

Mean_summary$Mean2<-ifelse(Mean_summary$Var=="Fungi"|Mean_summary$Var=="GF"|Mean_summary$Var=="Lichen",exp(Mean_summary$Mean),Mean_summary$Mean)

head(Mean_summary)

Summary_var<-ddply(Mean_summary,.(Scenario,Year,Var),summarise,mean_var=mean(Mean2),mean_AGB=mean(AGB)/100)
Summary_var<-subset(Summary_var,Year<=100)

Summary_table<-spread(Summary_var,Var,mean_var)
write.csv(x=Summary_table,"Data/R_output/Landis_ES.csv",row.names=F)

#plot the results of this
theme_set(theme_bw(base_size=12))
P1<-ggplot(Summary_var,aes(x=Year,y=mean_var,colour=Scenario))+geom_line(alpha=0.5)+facet_wrap(~Var,scales = "free_y")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Value")+scale_colour_brewer("Scenario",palette="Set1")+xlim(0,100)
ggsave("Figures/ES_landis.pdf",dpi = 400,height=6,width=8,units="in")

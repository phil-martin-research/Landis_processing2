#script to produce analysis for recreation and aesthetic data

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(gridExtra)
library(gtools)
library(raster)

#remove any R objects
rm(list=ls())

#import recreation data
Rec_aes<-read.csv("Data/Rec_aes_reorg.csv")

head(Rec_aes)
#model recreation as a function of biomass
#first tranform this data
Rec_aes$Rec_trans<-((Rec_aes$Recreation-1)/4)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_trans==0,Rec_aes$Rec_trans+0.01,Rec_aes$Rec_trans)
Rec_aes$Rec_transM<-ifelse(Rec_aes$Rec_transM==1,Rec_aes$Rec_transM-0.01,Rec_aes$Rec_transM)
Rec_aes$Rec_transM<-qlogis(Rec_aes$Rec_transM)

M1_rec<-lmer(Rec_transM~mean_AGB+(1|ID),data=Rec_aes)
M2_rec<-lmer(Rec_transM~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
M0_rec<-lmer(Rec_transM~1+(1|ID),data=Rec_aes)
AICc(M1_rec,M2_rec,M0_rec)

summary(M1_rec)
summary(Rec_aes)
df<-data.frame(mean_AGB=seq(0,396,by=1))

df$Recreation<-((plogis(predict(M1_rec,newdata = df,re.form=NA)))*4)+1

Rec_sum<-ddply(Rec_aes,.(Recreation,mean_AGB),summarise,count_un=length(ID))

theme_set(theme_bw(base_size=12))
Rec_plot1<-ggplot(Rec_sum,aes(x=mean_AGB,y=Recreation,size=count_un))+geom_point(shape=1)+geom_line(data=df,size=1)
Rec_plot2<-Rec_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),legend.position="none")
Rec_plot3<-Rec_plot2+ylab("Recreational rating")+xlab("Aboveground biomass")


#model aesthetic appreciation as a function of biomass
#first tranform this data
Rec_aes$Aes_trans<-((Rec_aes$Aesthetic-1)/4)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_trans==0,Rec_aes$Aes_trans+0.01,Rec_aes$Aes_trans)
Rec_aes$Aes_transM<-ifelse(Rec_aes$Aes_transM==1,Rec_aes$Aes_transM-0.01,Rec_aes$Aes_transM)
Rec_aes$Aes_transM<-qlogis(Rec_aes$Aes_transM)

M1_aes<-lmer(Aes_transM~mean_AGB+(1|ID),data=Rec_aes)
M2_aes<-lmer(Aes_transM~mean_AGB+I(mean_AGB^2)+(1|ID),data=Rec_aes)
M0_aes<-lmer(Aes_transM~1+(1|ID),data=Rec_aes)
AICc(M1_aes,M2_aes,M0_aes)

summary(M1_aes)
summary(Rec_aes)
df<-data.frame(mean_AGB=seq(0,396,by=1))

df$Aesthetic<-((plogis(predict(M1_aes,newdata = df,re.form=NA)))*4)+1

Aes_sum<-ddply(Rec_aes,.(Aesthetic,mean_AGB),summarise,count_un=length(ID))

Aes_plot1<-ggplot(Aes_sum,aes(x=mean_AGB,y=Aesthetic,size=count_un))+geom_point(shape=1)+geom_line(data=df,size=1)
Aes_plot2<-Aes_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),legend.position="none")
Aes_plot3<-Aes_plot2+ylab("Aesthetic appreciation rating")+xlab("Aboveground biomass")

pdf("Figures/Rec_aes.pdf",width = 8,height = 4)
grid.arrange(Rec_plot3,Aes_plot3,ncol=2)


###################################################
#run this section to use coefficients to predict ##
#values for landscape scale Landis models##########
###################################################

Coefs<-data.frame(intercept=c(coef(summary(M1_rec))[1,1],coef(summary(M1_aes))[1,1]),
                  coef=c(coef(summary(M1_rec))[2,1],coef(summary(M1_aes))[2,1]))

#load data Landis II outputs
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("*.dbf",File_names)]
File_names<-File_names[grepl("TotalBiomass",File_names)]
File_names<-rev(mixedsort(File_names))


#for each replicate of each scenario predict values of each ecosystem service
#or biodiversity value and output this as a raster and summarise values as 
#the mean pixel value, with stadard deviation and standard error

n<-0
ptm <- proc.time()
Mean_summary<-NULL
for (j in 1:2){
  j<-1
  for (i in 1:length(File_names)){
    i<-1
    Biomass<-raster(File_names[i])
    Biomass[Biomass==0]<-NA #remove all zeros from raster and replace with NAs
    Biomass<-Biomass/100 #divide biomass by 100 to produce values in Mg per Ha
    plot(calc(x=Biomass, fun=function(x) ((plogis(Coefs[j,1]+(x*Coefs[j,2])))*4)+1))
    ?calc
    plogis(1.6)
    
    Prediction<-(Coefs[j,1]+(Biomass*Coefs[j,2]))
    summary(Prediction)
    
    
    
    ((((((Biomass)/100)-mean(BM$AGB))/sd(BM$AGB))*Coefficients[j,3]+ #use coefficients to predict ES and biodiversity values
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


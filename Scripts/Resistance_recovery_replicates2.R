#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

#load packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(grid)
library(gridExtra)
library(plyr)

#organise data
Eco_summary<-read.csv("Data/R_output/Ecoregion_summary_replicates.csv")
Eco_summary<-Eco_summary[order(Eco_summary[,3]),]
head(Eco_summary)

#functions to make plotting of figures easier

ES_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="variable") {
    value[value=="AGB_M"]   <- "Aboveground biomass"
    value[value=="Timber_M"]   <- "Timber volume"
    value[value=="Nitrogen_stock_M"]   <- "Nitrogen stock"
    value[value=="Carbon_stock_M"]   <- "Carbon stock"
    value[value=="Recreation_M"]   <- "Recreation value"
    value[value=="Aesthetic_M"]   <- "Aesthetic value"
    value[value=="Lichen_M"]   <- "Lichen species \nrichness"
    value[value=="GF_M"]   <- "Ground flora \nspecies richness"
    value[value=="Fungi_M"]   <- "Fungi species richness"
    value[value=="Min_rate_M"]   <- "Nitrogen mineralisation \nrate"
    value[value=="SRR_M"]   <- "Soil respiration rate"
    value[value=="Tree_richness_M"]   <- "Tree species richness"
    value[value=="Fungi_val_M"]   <- "Commercially valuable \nfungi richness"
  }
  return(value)
}

Scenario_labeller <- function(var, value){ # lifted bodily from the R Cookbook
  value <- as.character(value)
  if (var=="Scenario") {
    value[value=="Scenario 1"]   <- 0
    value[value=="Scenario 2"]   <- 20
    value[value=="Scenario 3"]   <- 40
    value[value=="Scenario 4"]   <- 60
    value[value=="Scenario 5"]   <- 80
    value[value=="Scenario 6"]   <- 100
    value[value=="Scenario 7"]   <- 0
    value[value=="Scenario 8"]   <- 20
    value[value=="Scenario 9"]   <- 40
    value[value=="Scenario 10"]   <- 60
    value[value=="Scenario 11"]   <- 80
    value[value=="Scenario 12"]   <- 100
  }
  return(value)
}

Scenario_labeller2 <- function(var, value){ # lifted bodily from the R Cookbook
  value <- as.character(value)
  if (var=="Scenario") {
    value[value=="Scenario 1"]   <- "Pulse"
    value[value=="Scenario 2"]   <- "Pulse"
    value[value=="Scenario 3"]   <- "Pulse"
    value[value=="Scenario 4"]   <- "Pulse"
    value[value=="Scenario 5"]   <- "Pulse"
    value[value=="Scenario 6"]   <- "Pulse"
    value[value=="Scenario 7"]   <- "Pulse + Press"
    value[value=="Scenario 8"]   <- "Pulse + Press"
    value[value=="Scenario 9"]   <- "Pulse + Press"
    value[value=="Scenario 10"]   <- "Pulse + Press"
    value[value=="Scenario 11"]   <- "Pulse + Press"
    value[value=="Scenario 12"]   <- "Pulse + Press"
  }
  return(value)
}


#############################################################
#1 - RESISTANCE##############################################
#This part of the script calculates resistance of the #######
#different biodiversity/ecosystem function/ecosystem service#
#metrics used for each of the different scenarios of dieback#
#############################################################


#produce vector with details of different scenarios
Sc<-expand.grid(Scenario=Eco_summary$Scenario,Replicate=Eco_summary$Replicate)
Un_Scen<-unique(Sc)
#loop to calculate resistance based on Shade et al. 2012
Res_summary<-NULL
for (i in 1:nrow(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i,1]&Replicate==Un_Scen[i,2])#subset data to only inlude data from one scenario
  for (y in seq(4,ncol(Eco_summary))){
    #produce data frame with details of scenario, the variable assessed and its resistance
    Resistance<-data.frame(Scenario=unique(Scen_sub$Scenario),Replicate=unique(Scen_sub$Replicate),variable=colnames(Scen_sub[y]),
                           Resistance=1-((2*(Scen_sub[2,y]-Scen_sub[6,y]))/(Scen_sub[2,y]+(Scen_sub[2,y]-Scen_sub[6,y]))))
    #bind all dataframes together
    Res_summary<-rbind(Resistance,Res_summary)
  }
}

#set resistance as equal to 1 if variable increases
Res_summary$Resistance<-ifelse(Res_summary$Resistance>1,1,Res_summary$Resistance) 
#set Scenario 1 resistance as equal to 1
Res_summary$Resistance<-ifelse(Res_summary$Scenario=="Scenario 1",1,Res_summary$Resistance)
#set resistance as equal to 0 if it is less than 0
Res_summary$Resistance<-ifelse(Res_summary$Resistance<0,0,Res_summary$Resistance)

#prepare descriptive labels for different figures
Res_summary$ESLab <- ES_labeller('variable',Res_summary$variable)
Res_summary$Scen_lab <- as.numeric(Scenario_labeller('Scenario',Res_summary$Scenario))
Res_summary$Scen_lab2 <- Scenario_labeller2('Scenario',Res_summary$Scenario)

#recalculate statistics for figure
Res_summary2<-ddply(Res_summary,.(Scenario,ESLab,Scen_lab,Scen_lab2),summarise,m_Res=mean(Resistance),sd_Res=sd(Resistance))


Res_summary2<-subset(Res_summary2,ESLab!="Replicate")


Res_summary2$ESLab2<-factor(as.factor(Res_summary2$ESLab), c("Aboveground biomass","Carbon stock","Nitrogen stock","Soil respiration rate","Nitrogen mineralistation \nrate",
                               "Commercially valuable \nfungi richness","Timber volume","Aesthetic value","Recreation value","Fungi species richness",
                               "Ground flora \nspecies richness","Lichen species \nrichness","Tree species richness"))


unique(Res_summary2$ESLab)
#output this as a .csv file for Elena
write.csv(Res_summary,"Data/R_output/Resistence_replicates.csv",row.names=F)

head(Res_summary2)

#plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Res_summary2,aes(x=Scen_lab,y=m_Res,ymax=m_Res+sd_Res,ymin=m_Res-sd_Res,shape=Scen_lab2,colour=Scen_lab2))+facet_wrap(~ESLab2,ncol=5)+geom_hline(yintercept=1,lty=2,alpha=0.5,size=0.5)+geom_pointrange(alpha=0.5)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+geom_line(data=Res_summary2,aes(group=Scen_lab2),alpha=0.5)
P3<-P2+ylab("Resistance")+scale_x_continuous(breaks=c(0,20,40,60,80,100))+scale_y_continuous(limits=c(0,1.1),breaks=c(0,0.2,0.4,0.6,0.8,1))
P4<-P3+scale_colour_manual("Disturbance type",values = c("black","red"))+scale_shape_manual("Disturbance type",values = c(15, 17))
P4+xlab("Degree of disturbance")+theme(strip.text.x = element_text(size = 8))+guides(color = guide_legend(override.aes = list(linetype = 0)))
ggsave("Figures/Resistance_replicates.pdf",width = 10,height = 5,units = "in",dpi = 400)

#############################################################
#2 - Recovery################################################
#This part of the script calculates recovery of the #######
#different biodiversity/ecosystem function/ecosystem service#
#metrics used for each of the different scenarios of dieback#
#############################################################

#loop to calculate the recovery rate, resistance and value of variable relative to time==1 for all variables under all scenarios
Recovery_summary<-NULL
n<-0
for (i in 1:nrow(Un_Scen)){#run this part of loop for all scenarios
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i,1]&Replicate==Un_Scen[i,2])#subset to give different scenarios
  for (j in 5:ncol(Scen_sub)){#for each variable (e.g. Aboveground biomass) run this part of the loop
    for (k in 7:nrow(Scen_sub)){#for each row after time==5 run this loop
      #produce dataframe with all the resistance and recovery information we are interested in
      Recovery<-data.frame(Scenario=unique(Scen_sub$Scenario[i]),#name of scenario
                           Replicate=unique(Scen_sub$Replicate[i]),#replicate number
                           Variable=colnames(Scen_sub[j]),#variable name
                           Time=Scen_sub[k,3],#time step
                           #resistance at time step 5
                           Resistance=1-((2*(Scen_sub[2,j]-Scen_sub[6,j]))/(Scen_sub[2,j]+(Scen_sub[2,j]-Scen_sub[6,j]))),
                           #value of variable relative to time step 1
                           Resistance2=1-((2*(Scen_sub[2,j]-Scen_sub[k,j]))/(Scen_sub[2,j]+(Scen_sub[2,j]-Scen_sub[k,j]))),
                           #rate of recovery  based on Shade et al. 2012
                           Recovery=(((2*(Scen_sub[2,j]-Scen_sub[6,j]))/
                                        ((Scen_sub[2,j]-Scen_sub[6,j])+
                                           (Scen_sub[2,j]-Scen_sub[k,j])))-1)/
                             (Scen_sub[k,3]-Scen_sub[6,3]),
                           value=Scen_sub[k,j])#raw value of variable
      Recovery_summary<-rbind(Recovery_summary,Recovery)#bind all these dataframes into one dataframe
      n<-n+1
      print(n)
    }
  }
}
head(Recovery_summary)
subset(Recovery_summary,Scenario=="Scenario 3"&Variable=="AGB_M")


#set resistance as==1 for Scenario 1 and if Reistance>=1
Recovery_summary$Resistance<-ifelse(Recovery_summary$Scenario=="Scenario 1",1,Recovery_summary$Resistance)#set Scenario 1 resistance as equal to 1
Recovery_summary$Resistance<-ifelse(Recovery_summary$Resistance>=1,1,Recovery_summary$Resistance) #set resistance as equal to 1 if variable increases

#subset data so that data before time step==5, data from Scenario 1 and where resistance>1 are removed
Recovery_sub<-subset(Recovery_summary,Time>5&Scenario!="Scenario 1"|Variable!="Min_rate_M")



#not sure what is going on with carbon here - this will need fixing
ggplot(Recovery_sub,aes(x=Time,y=Resistance2,colour=Scenario,group=interaction(Replicate,Scenario)))+geom_line()+facet_wrap(~Variable,scales="free_y")

#now work out the first time point at which Resistance2>=1, thereby working out the time
#taken for each ecosystem service/biodiversity variable to recover

#for each scenario, for each service/biodiversity metric work out the time at which resistance>=1
Un_Sce_ES<-expand.grid(unique(Recovery_sub$Scenario),unique(Recovery_sub$Replicate),unique(Recovery_sub$Variable))#unique combinations of scenario and variable
R_summ<-NULL
for (i in 1:nrow(Un_Sce_ES)){
  Scen_sub<-subset(Recovery_sub,Scenario==Un_Sce_ES[i,1]&Replicate==Un_Sce_ES[i,2]&Variable==Un_Sce_ES[i,3])#subset for each row of Un_Sce_ES
  First_recov<-which(Scen_sub$Resistance2>=1)[1]#work out row index of first time at which variable recovered to value at time==1
  if (!is.na(First_recov)){
    Recov_summary<-Scen_sub[(First_recov),]#get row at which variable has recovered
  }else{#if it didn't recover, set "Time" as==100
    Recov_summary<-Scen_sub[(First_recov),]#give the first row at which the variable has recovered
    Recov_summary$Scenario<-Un_Sce_ES[i,1]#include scenario number
    Recov_summary$Replicate<-Un_Sce_ES[i,2]#include replicate number
    Recov_summary$Variable<-Un_Sce_ES[i,3]#include variable name
    Recov_summary$Time<-100#set time as 100 if there is no recvery within time-frame
  }
  R_summ<-rbind(R_summ,Recov_summary)
}

head(R_summ)

R_summ$variable<-R_summ$Variable#tidy data
R_summ$ESLab <- ES_labeller('variable',R_summ$variable)#relabel variables for ease of plotting
R_summ$Scen_lab <- as.numeric(Scenario_labeller('Scenario',R_summ$Scenario))
R_summ$Scen_lab2 <- Scenario_labeller2('Scenario',R_summ$Scenario)


R_summ2<-ddply(R_summ,.(Scenario,ESLab,Scen_lab,Scen_lab2),summarise,m_time=mean(Time),sd_time=sd(Time))
R_summ2$m_time<-ifelse(R_summ2$ESLab=="Ground flora \nspecies richness"|
                         R_summ2$ESLab=="Nitrogen mineralisation \nrate"|
                         R_summ2$ESLab=="Tree species richness"
                         ,NA,R_summ2$m_time)

R_summ2$ESLab2<-factor(as.factor(R_summ2$ESLab), c("Aboveground biomass","Carbon stock","Nitrogen stock","Soil respiration rate","Nitrogen mineralisation \nrate",
                                                             "Commercially valuable \nfungi richness","Timber volume","Aesthetic value","Recreation value","Fungi species richness",
                                                             "Ground flora \nspecies richness","Lichen species \nrichness","Tree species richness"))



#output this as a .csv file for Elena
write.csv(R_summ,"Data/R_output/Recovery_replicates.csv",row.names=F)


#plot of time taken for recovery
P1<-ggplot(R_summ2,aes(x=Scen_lab,y=m_time,ymax=m_time+sd_time,ymin=m_time-sd_time,colour=Scen_lab2,shape=Scen_lab2))+geom_pointrange(alpha=0.5)+geom_line(aes(group=Scen_lab2))+facet_wrap(~ESLab,ncol=5)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Time taken for recovery (Years)")+ theme(strip.text.x = element_text(size = 8))+xlab("Degree of disturbance")
P4<-P3+scale_colour_manual("Disturbance type",values = c("black","red"))+scale_shape_manual("Disturbance type",values = c(15, 17))
P4+guides(color = guide_legend(override.aes = list(linetype = 0)))
ggsave("Figures/Recovery_time_replicates.pdf",width = 10,height = 5,units = "in",dpi = 400)


##################################################################
#3 - PERSISTENCE##################################################
#Currently I am a little bit unclear about how we will measure####
#Peristance, it could be :########################################
#(i) the change after 100 years###################################
#(ii) the similarity of all ES and BD using quantitative sorensen#
#(iii) something else?############################################
##################################################################
#


#loop to get values or ecosystem services/biodiversity relative to first time step, after 100 years
Un_Sce_ES<-expand.grid(unique(Recovery_summary$Scenario),unique(Recovery_sub$Replicate),unique(Recovery_summary$Variable))#unique combinations of scenario and variable
Pers_summ<-NULL
for (i in 1:nrow(Un_Sce_ES)){
  Scen_sub<-subset(Recovery_summary,Scenario==Un_Sce_ES[i,1]&Replicate==Un_Sce_ES[i,2]&Variable==Un_Sce_ES[i,3])#subset for each row of Un_Sce_ES
  Pers_sub<-tail(Scen_sub,1)#get value after 100 years
  Pers_summ<-rbind(Pers_summ,Pers_sub)#bind all dataframes together
}

#set value as 1 if value is greater after 100 years at time zero
Pers_summ$Resistance2<-ifelse(Pers_summ$Resistance2>1,1,Pers_summ$Resistance2)
Pers_summ$variable<-Pers_summ$Variable
#relabel bits for plotting of figures
Pers_summ$ESLab <- ES_labeller('variable',Pers_summ$variable)
Pers_summ$Scen_lab <- as.numeric(Scenario_labeller('Scenario',Pers_summ$Scenario))
Pers_summ$Scen_lab2 <- Scenario_labeller2('Scenario',Pers_summ$Scenario)
Pers_summ<-subset(Pers_summ,Scen_lab2!="Press"&Scenario!="Scenario 1")


Pers_summ<-subset(Pers_summ,ESLab=="Carbon stock"|ESLab=="Nitrogen stock"|ESLab=="Recreation value"|ESLab=="Timber volume"|ESLab=="Fungi species richness"|ESLab=="Ground flora \nspecies richness"|ESLab=="Lichen species \nrichness"|ESLab=="Tree species richness")
Pers_summ$ESLab <- factor(Pers_summ$ESLab, c("Carbon stock", "Nitrogen stock", "Recreation value", "Timber volume",
                                             "Fungi species richness","Ground flora \nspecies richness",
                                             "Lichen species \nrichness","Tree species richness"))
head(Pers_summ)
Pers_summ2<-ddply(Pers_summ,.(Scenario,Variable),transform,m_var=mean(Resistance2),sd_var=sd(Resistance2))

#output this as a .csv for elena
write.csv(Pers_summ,"Data/R_output/Persistence_replicates.csv",row.names=F)

P1<-ggplot(Pers_summ2,aes(x=Scen_lab,y=m_var,ymax=m_var+sd_var,ymin=m_var-sd_var,colour=Scen_lab2,shape=Scen_lab2))+geom_pointrange(alpha=0.5)+facet_wrap(~ESLab,ncol=4)+geom_line(aes(group=Scen_lab2))
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Persistance")+ theme(strip.text.x = element_text(size = 8))+geom_hline(yintercept=1,lty=2,alpha=0.5,size=0.5)
P4<-P3+scale_colour_manual("Disturbance type",values = c("black","red"))+scale_shape_manual("Disturbance type",values = c(15, 17))
P4+xlab("Degree of disturbance")
ggsave("Figures/Persistence_replicates.pdf",width = 8,height = 6,units = "in",dpi = 400)

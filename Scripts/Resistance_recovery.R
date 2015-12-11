#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

#load packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(grid)
library(gridExtra)

#organise data
Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")
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
    value[value=="Min_rate_M"]   <- "Nitrogen mineralistation \nrate"
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
    value[value=="Scenario 1"]   <- "No disturbance"
    value[value=="Scenario 2"]   <- "Pulse"
    value[value=="Scenario 3"]   <- "Pulse"
    value[value=="Scenario 4"]   <- "Pulse"
    value[value=="Scenario 5"]   <- "Pulse"
    value[value=="Scenario 6"]   <- "Pulse"
    value[value=="Scenario 7"]   <- "Press"
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
Sc<-Eco_summary$Scenario
Un_Scen<-unique(Sc)
#loop to calculate resistance based on Shade et al. 2012
Res_summary<-NULL
for (i in 1:length(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i])#subset data to only inlude data from one scenario
  for (y in seq(4,ncol(Eco_summary)-1,by = 2)){
    #produce data frame with details of scenario, the variable assessed and its resistance
    Resistance<-data.frame(Scenario=unique(Scen_sub$Scenario),variable=colnames(Scen_sub[y]),
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

#output this as a .csv file for Elena
write.csv(Res_summary,"Data/R_output/Resistence.csv",row.names=F)


#plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Res_summary,aes(x=Scen_lab,y=Resistance,shape=Scen_lab2,colour=Scen_lab2))+facet_wrap(~ESLab)+geom_hline(yintercept=1,lty=2,alpha=0.5,size=0.5)+geom_point(size=2,alpha=0.5)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+xlab("Percentage beech and oak biomass lost in disturbance")+ylab("Resistance of biodivesrity and ecosystem services to disturbance")
P3+scale_colour_manual("Disturbance type",values = c("green","purple","blue","red"))+scale_shape_manual("Disturbance type",values = c(0, 1, 15, 16))
ggsave("Figures/Resistance.pdf",width = 8,height = 6,units = "in",dpi = 400)

#############################################################
#2 - Recovery################################################
#This part of the script calculates recovery of the #######
#different biodiversity/ecosystem function/ecosystem service#
#metrics used for each of the different scenarios of dieback#
#############################################################

#loop to calculate the recovery rate, resistance and value of variable relative to time==1 for all variables under all scenarios
Recovery_summary<-NULL
for (i in 1:length(Un_Scen)){#run this part of loop for all scenarios
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i])#subset to give different scenarios
  for (j in seq(4,ncol(Eco_summary)-1,by = 2)){#for each variable (e.g. Aboveground biomass) run this part of the loop
    for (k in 7:nrow(Scen_sub)){#for each row after time==5 run this loop
      #produce dataframe with all the resistance and recovery information we are interested in
    Recovery<-data.frame(Scenario=unique(Un_Scen[i]),#name of scenario
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
  }
}
}

#set resistance as==1 for Scenario 1 and if Reistance>=1
Recovery_summary$Resistance<-ifelse(Recovery_summary$Scenario=="Scenario 1",1,Recovery_summary$Resistance)#set Scenario 1 resistance as equal to 1
Recovery_summary$Resistance<-ifelse(Recovery_summary$Resistance>=1,1,Recovery_summary$Resistance) #set resistance as equal to 1 if variable increases

#subset data so that data before time step==5, data from Scenario 1 and where resistance>1 are removed
Recovery_sub<-subset(Recovery_summary,Time>5&Scenario!="Scenario 1"&Resistance<1)
Recovery_sub<-subset(Recovery_sub,Variable!="Min_rate_M"&Variable!="GF_M")


#not sure what is going on with carbon here - this will need fixing
ggplot(Recovery_sub,aes(x=Time,y=Resistance2,colour=Scenario))+geom_line()+facet_wrap(~Variable,scales="free_y")

#now work out the first time point at which Resistance2>=1, thereby working out the time
#taken for each ecosystem service/biodiversity variable to recover

#for each scenario, for each service/biodiversity metric work out the time at which resistance>=1
Un_Sce_ES<-expand.grid(unique(Recovery_sub$Scenario),unique(Recovery_sub$Variable))#unique combinations of scenario and variable
R_summ<-NULL
for (i in 1:nrow(Un_Sce_ES)){
  Scen_sub<-subset(Recovery_sub,Scenario==Un_Sce_ES[i,1]&Variable==Un_Sce_ES[i,2])#subset for each row of Un_Sce_ES
  First_recov<-which(Scen_sub$Resistance2>=1)[1]#work out row index of first time at which variable recovered to value at time==1
  if (!is.na(First_recov)){
  Recov_summary<-Scen_sub[(First_recov),]#get row at which variable has recovered
  }else{#if it didn't recover, set "Time" as==100
   Recov_summary<-Scen_sub[(First_recov),]#give the first row at which the variable has recovered
   Recov_summary$Scenario<-Un_Sce_ES[i,1]#include scenario number
   Recov_summary$Variable<-Un_Sce_ES[i,2]#include variable name
   Recov_summary$Time<-100#set time as 100 if there is no recvery within time-frame
  }
  R_summ<-rbind(R_summ,Recov_summary)
}

R_summ$variable<-R_summ$Variable#tidy data
R_summ$ESLab <- ES_labeller('variable',R_summ$variable)#relabel variables for ease of plotting
R_summ$Scen_lab <- as.numeric(Scenario_labeller('Scenario',R_summ$Scenario))
R_summ$Scen_lab2 <- Scenario_labeller2('Scenario',R_summ$Scenario)
R_summ<-subset(R_summ,Scen_lab2!="Press")

#output this as a .csv file for Elena
write.csv(R_summ,"Data/R_output/Recovery.csv",row.names=F)


#plot of time taken for recovery
P1<-ggplot(R_summ,aes(x=Scen_lab,y=Time,colour=Scen_lab2,shape=Scen_lab2))+geom_point(size=2,alpha=0.5)+facet_wrap(~ESLab,ncol=5)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Time taken for recovery (Years)")+ theme(strip.text.x = element_text(size = 8))+xlab("Percentage beech and oak biomass lost in disturbance")
P4<-P3+scale_colour_manual("Disturbance type",values = c("blue","red"))+scale_shape_manual("Disturbance type",values = c(15, 16))
P4+theme(panel.margin = unit(1, "lines"))
ggsave("Figures/Recovery_time.pdf",width = 10,height = 5,units = "in",dpi = 400)


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
Un_Sce_ES<-expand.grid(unique(Recovery_summary$Scenario),unique(Recovery_summary$Variable))#unique combinations of scenario and variable
Pers_summ<-NULL
for (i in 1:nrow(Un_Sce_ES)){
  Scen_sub<-subset(Recovery_summary,Scenario==Un_Sce_ES[i,1]&Variable==Un_Sce_ES[i,2])#subset for each row of Un_Sce_ES
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


#output this as a .csv for elena
write.csv(Pers_summ,"Data/R_output/Persistence.csv",row.names=F)

P1<-ggplot(Pers_summ,aes(x=Scen_lab,y=Resistance2,colour=Scen_lab2,shape=Scen_lab2))+geom_point(size=2,alpha=0.5)+facet_wrap(~ESLab,ncol=4)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Value after 100 years relative to year 0")+ theme(strip.text.x = element_text(size = 8))+geom_hline(yintercept=1,lty=2,alpha=0.5,size=0.5)
P4<-P3+scale_colour_manual("Disturbance type",values = c("blue","red"))+scale_shape_manual("Disturbance type",values = c(15, 16))
P4+xlab("Percentage beech and oak biomass lost in disturbance")+ theme(panel.margin = unit(1, "lines"))
ggsave("Figures/Persistence.pdf",width = 8,height = 6,units = "in",dpi = 400)

#now if we treat each variable as if it was a species we can produce a similarity index to see summarise which 
#scenario is most similar to year==0 after 100 years

#names(Eco_summ_clean)[3:ncol(Eco_summ_clean)]

#Sor_sim_sum<-NULL
#Eco_summ_clean<-Eco_summary[-c(1,seq(5,27,by=2),22,24)]
#head(Eco_summary)
#Uni_Scen<-unique(Eco_summ_clean$Scenario)
#for (i in 1:length(Uni_Scen)){
  #Eco_sum_sub<-subset(Eco_summ_clean,Scenario==Uni_Scen[i])
  #Eco_sum_sub2<-(Eco_sum_sub)[-c(1:2)]
  #Eco_sum_sub3<-(Eco_sum_sub2)[-1,]
  #Eco_sum_sub[101,]
  #Sor_sim<-data.frame(Scenario=Uni_Scen[i],Sim=1-vegdist(Eco_sum_sub3)[100])
  #Sor_sim_sum<-rbind(Sor_sim_sum,Sor_sim)
#}
          
#ggplot(Sor_sim_sum,aes(x=Scenario,y=Sim))+geom_point(shape=1,size=2)+geom_hline(yintercept=1,lty=2,alpha=0.5)

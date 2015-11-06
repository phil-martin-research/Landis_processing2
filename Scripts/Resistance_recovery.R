#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

#load packages
library(ggplot2)

#organise data
Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")
Eco_summary<-Eco_summary[order(Eco_summary[,3]),]


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
  for (y in seq(4,26,by = 2)){
    #produce data frame with details of scenario, the variable assessed and its resistance
    Resistance<-data.frame(Scenario=unique(Scen_sub$Scenario),variable=colnames(Scen_sub[y]),
                             Resistance=1-((2*(Scen_sub[2,y]-Scen_sub[6,y]))/(Scen_sub[2,y]+(Scen_sub[2,y]-Scen_sub[6,y]))))
    #bind all dataframes together
    Res_summary<-rbind(Resistance,Res_summary)
  }
}
Res_summary$Resistance<-ifelse(Res_summary$Resistance>1,1,Res_summary$Resistance) #set resistance as equal to 1 if variable increases
Res_summary$Resistance<-ifelse(Res_summary$Scenario=="Scenario 1",1,Res_summary$Resistance)#set Scenario 1 resistance as equal to 1


#prepare descriptive labels for different facets

ES_labeller <- function(var, value){ # lifted bodily from the R Cookbook
  value <- as.character(value)
  if (var=="variable") {
    value[value=="Timber_M"]   <- "Timber volume"
    value[value=="Nitrogen_flux_M"]   <- "Nitrogen flux"
    value[value=="Carbon_flux_M"]   <- "Carbon flux"
    value[value=="Recreation_M"]   <- "Recreation value"
    value[value=="Aesthetic_M"]   <- "Aesthetic value"
    value[value=="Lichen_M"]   <- "Lichen species \nrichness"
    value[value=="GF_M"]   <- "Ground flora \nspecies richness"
    value[value=="Fungi_M"]   <- "Fungi species richness"
    value[value=="Min_rate_M"]   <- "Nitrogen mineralistation \nrate"
    value[value=="SRR_M"]   <- "Soil respiration rate"
    value[value=="AGB_M"]   <- "Above ground biomass"
    value[value=="Fungi_val_M"]   <- "Commercially valuable \nfungi richness"
  }
  return(value)
}
Res_summary$ESLab <- ES_labeller('variable',Res_summary$variable)


#plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Res_summary,aes(x=Scenario,y=Resistance))+facet_wrap(~ESLab,scales = "free_y")+geom_hline(yintercept=1,lty=2,alpha=0.5,size=0.5)+geom_point(size=2,shape=1)
P1+theme(axis.text.x = element_text(angle = 90))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Resistance.pdf",width = 8,height = 6,units = "in",dpi = 400)

#############################################################
#2 - Recovery################################################
#This part of the script calculates recovery of the #######
#different biodiversity/ecosystem function/ecosystem service#
#metrics used for each of the different scenarios of dieback#
#############################################################



#recovery
#calculate the rate of recovery and how long it takes

Recovery_summary<-NULL
Recovery_summary2<-NULL
for (i in 1:length(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i])
  head(Scen_sub)
  for (j in seq(4,26,by = 2)){

    for (k in 7:nrow(Scen_sub)){
    Recovery<-data.frame(Scenario=unique(Un_Scen[i]),Variable=colnames(Scen_sub[j]),
                         Time=Scen_sub[k,3],
                         Resistance=1-((2*(Scen_sub[2,j]-Scen_sub[6,j]))/(Scen_sub[2,j]+(Scen_sub[2,j]-Scen_sub[6,j]))),
                         Resistance2=1-((2*(Scen_sub[2,j]-Scen_sub[k,j]))/(Scen_sub[2,j]+(Scen_sub[2,j]-Scen_sub[k,j]))),
                           Recovery=(((2*(Scen_sub[2,j]-Scen_sub[6,j]))/
                               ((Scen_sub[2,j]-Scen_sub[6,j])+
                                  (Scen_sub[2,j]-Scen_sub[k,j])))-1)/
                           (Scen_sub[k,3]-Scen_sub[6,3]),
                         value=Scen_sub[k,j])
    Recovery_summary<-rbind(Recovery_summary,Recovery)
  }
}
}

Recovery_summary$Resistance<-ifelse(Recovery_summary$Scenario=="Scenario 1",1,Recovery_summary$Resistance)#set Scenario 1 resistance as equal to 1
Recovery_summary$Resistance<-ifelse(Recovery_summary$Resistance>=1,1,Recovery_summary$Resistance) #set resistance as equal to 1 if variable increases

Recovery_sub<-subset(Recovery_summary,Time>5&Scenario!="Scenario 1"&Resistance<1)
head(Recovery_sub)

ggplot(Recovery_sub,aes(x=Time,y=Resistance2,colour=Scenario))+geom_line()+facet_wrap(~Variable,scales="free_y")

#now work out the first time point at which Resistance2>=1, thereby working out the time
#taken for each ecosystem service/biodiversity variable to recover

#for each scenario, for each service/biodiversity metric work out the time at which resistance>=1
Un_Sce_ES<-expand.grid(unique(Recovery_sub$Scenario),unique(Recovery_sub$Variable))

R_summ<-NULL
for (i in 1:nrow(Un_Sce_ES)){
  Scen_sub<-subset(Recovery_sub,Scenario==Un_Sce_ES[i,1]&Variable==Un_Sce_ES[i,2])
  First_recov<-which(Scen_sub$Resistance2>=1)[1]
  if (!is.na(First_recov)){
  Recov_summary<-Scen_sub[(First_recov),]
  }else{
   Recov_summary<-Scen_sub[(First_recov),]
   Recov_summary$Scenario<-Un_Sce_ES[i,1]
   Recov_summary$variable<-Un_Sce_ES[i,2]
   Recov_summary$Time<-100
  }
  R_summ<-rbind(R_summ,Recov_summary)
}

R_summ$ESLab <- ES_labeller('variable',R_summ$variable)

#plot of time taken for recovery
P1<-ggplot(R_summ,aes(x=Scenario,y=Time))+geom_point()+facet_wrap(~Variable,scales="free_y",ncol=5)
P2<-P1+theme(axis.text.x = element_text(angle = 90))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Time taken for recovery (Years)")

#plot of recovery rate
P1<-ggplot(R_summ,aes(x=Scenario,y=Recovery*100))+geom_point()+facet_wrap(~Variable,scales="free_y",ncol=5)
P2<-P1+theme(axis.text.x = element_text(angle = 90))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Annual percentage increase in variable")


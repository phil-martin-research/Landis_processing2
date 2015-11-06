#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

#load packages
library(ggplot2)

#organise data
Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")
Eco_summary$Scenario<-factor(Eco_summary$Scenario,c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6", "Scenario 12"))

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

#recovery
#calculate the rate of recovery and how long it takes

Recovery_summary<-NULL
for (i in 1:length(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i])
  for (j in seq(4,18,by = 2)){
    for (k in 2:nrow(Scen_sub)){
    Recovery<-data.frame(Scenario=unique(Scen_sub$Scenario),Variable=colnames(Scen_sub[j]),
                         Time=Scen_sub[k,2],
                           Resistance=1-((2*(Scen_sub[1,j]-Scen_sub[5,j]))/(Scen_sub[1,j]+(Scen_sub[1,j]-Scen_sub[5,j]))),
                           Recovery=(((2*(Scen_sub[1,j]-Scen_sub[5,j]))/
                               ((Scen_sub[1,j]-Scen_sub[5,j])+
                                  (Scen_sub[1,j]-Scen_sub[k,j])))-1)/
                           (Scen_sub[k,2]-Scen_sub[j,2]))
    Recovery_summary<-rbind(Recovery,Recovery_summary)
  }
}
}

head(Recovery_summary)

#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")

Sc<-Eco_summary$Scenario
Vars<-as.factor(c(colnames(Eco_summary)[c(4,6,8,10,12,14,16,18)]))
Un_Scen<-unique(x=expand.grid(Sc,Vars))

for (i in 1:nrow(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen$Var1[i])
  head(cbind(Scen_sub[-c(1,4:ncol(Scen_sub))],Scen_sub[[]])
  Resistance<-1-((2*(Scen_sub[1,4]-Scen_sub[6,4]))/(Scen_sub[1,4]+(Scen_sub[1,4]-Scen_sub[6,4])))
}
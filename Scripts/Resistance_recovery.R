#script to calculate the resistance and recovery for different ecosystem services
#at the landscape scale

#load packages
library(ggplot2)

Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")

Sc<-Eco_summary$Scenario
Un_Scen<-unique(Sc)

Res_summary<-NULL
for (i in 1:length(Un_Scen)){
  Scen_sub<-subset(Eco_summary,Scenario==Un_Scen[i])
  for (y in seq(4,18,by = 2)){
    Resistance<-data.frame(Scenario=unique(Scen_sub$Scenario),Variable=colnames(Scen_sub[y]),
                             Resistance=1-((2*(Scen_sub[1,y]-Scen_sub[6,y]))/(Scen_sub[1,y]+(Scen_sub[1,y]-Scen_sub[6,y]))))
    Res_summary<-rbind(Resistance,Res_summary)
  }
}


P1<-ggplot(Res_summary,aes(x=Scenario,y=Resistance))+geom_point()+facet_wrap(~Variable,scales = "free_y")+geom_hline(yintercept=1,lty=2)
P1+theme(axis.text.x = element_text(angle = 90))
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

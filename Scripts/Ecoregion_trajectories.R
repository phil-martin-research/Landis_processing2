library(ggplot2)
library(gridExtra)
library(grid)

Eco_summary<-read.csv("Data/R_output/Ecoregion_summary.csv")
Eco_means<-read.csv("Data/R_output/Ecoregion_means.csv")

Eco_means<-Eco_means[,-16]

Eco_means$Scenario<-factor(Eco_means$Scenario,c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6",
                                                "Scenario 7","Scenario 8","Scenario 9","Scenario 10","Scenario 11","Scenario 12"))
Eco_summary$Scenario<-factor(Eco_summary$Scenario,c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6",
                                                    "Scenario 7","Scenario 8","Scenario 9","Scenario 10","Scenario 11","Scenario 12"))

out<-NULL
t<-1
theme_set(theme_bw(base_size=8))
for (i in names(Eco_means)[-c(1:6)]){
  Y_var<-i
  Y_var2<-paste(i,"_M",sep = "")
  Var_SD<-paste(i,"_SD",sep = "")
  P1<-ggplot(Eco_means,aes_string(x="Time",y=Y_var,group="EcoregionIndex"))+geom_line(size=0.5,alpha=0.2)+facet_wrap(~Scenario,nrow=1,scales="free")
  P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
  P3<-P2+geom_line(data=Eco_summary,aes_string(x="Time",y=Y_var2,group=NULL),size=1.1) +theme(plot.margin = unit(c(0,0.5,0,0), "line"))
  out[[t]]<-P3
  t<-t+1
}


pdf(file = "Figures/Ecosystem_trajectories_1.pdf",width = 10,height=8)
do.call(grid.arrange, c(out[1],out[2],out[3],out[4],out[5],out[6],
                        list(ncol=1)))
dev.off()

pdf(file = "Figures/Ecosystem_trajectories_2.pdf",width = 10,height=8)
do.call(grid.arrange, c(out[7],out[8],out[9],out[10],out[11],out[12],
                        list(ncol=1)))
dev.off()

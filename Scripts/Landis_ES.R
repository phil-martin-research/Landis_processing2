#script to produce figures for different ecosystem services under different scenarios

#load packages
library(ggplot2)
library(gridExtra)
library(dplyr)

ES<-read.csv("Data/R_output/Landis_ES_all.csv")

head(ES)

P1<-ggplot(ES,aes(x=Year,y=SRR,colour=as.factor(Scenario),group=as.factor(Scenario)))+geom_point(shape=1)+geom_line()
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Value")+scale_colour_brewer("Scenario",palette="Set1")+xlim(0,100)

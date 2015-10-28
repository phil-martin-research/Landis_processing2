#script to produce community composition statistics for trees in each ecoregion
#in this script we use biomass as the metric for abundance of different species

#load packages
library(ggplot2)
library(plyr)
library(reshape)
library(grid)
library(SDMTools)
library(vegan)

#clear objects
rm(list=ls())

#find all the century species biomass files
Eco_region_BM<-list.files(pattern="spp-biomass-log",recursive=T)

#run a loop to read in each .csv containing tree species biomass values
#remove columns that are not useful and ass a column to give details of
#the scenario being run
BM_ER<-NULL
for (i in 1:length(Eco_region_BM)){
  #read in .csv
  File<-read.csv(Eco_region_BM[i])
  #remove blank column
  File_sub<-File[-ncol(File)]
  hist(File_sub$Time)
  #remove rows containing NAs
  File_sub2<-File_sub[complete.cases(File_sub),]
  #insert a column with the scenario number
  File_sub2$Scenario<-gsub(".*-log|_r.*","", Eco_region_BM[i])
  File_sub2$Replicate<-sub(".*?_r(.*?).csv.*", "\\1", Eco_region_BM[i])
  #bind all these outputs together
  BM_ER<-rbind(File_sub2,BM_ER)
}

BM_ER<-subset(BM_ER,Time<=100)
head(BM_ER)
Uniq_combs<-unique(BM_ER[c("Scenario", "Replicate","EcoregionIndex")])
str(Sub_CC)
Com_sim<-NULL
for (i in 1:nrow(Uniq_combs)){#for each unique combination of scenario and replicate
  #subset species composition to give all time steps
  Sub_CC<-BM_ER[with(BM_ER, Scenario==Uniq_combs$Scenario[i] & Replicate==Uniq_combs$Replicate[i] & EcoregionIndex==Uniq_combs$EcoregionIndex[i]), ]
  #drop columns that do not refer to species abundance
  head(Sub_CC)
  Sub_CC2<-Sub_CC[,-c(1:4,ncol(Sub_CC)-1,ncol(Sub_CC))]
  head(Sub_CC2)
  #create dataframe including scenario, replicate and time
  #along with sorensen index using time 0 as a comparitor
  #currently all of these equal 1, as no species are lost or gained
  Community<-data.frame(Sub_CC[,c(1:4,ncol(Sub_CC)-1,ncol(Sub_CC))],similarity=c(1,(1-vegdist(Sub_CC2))[1:10]))
  #bind results to last set of loop results
  Com_sim<-rbind(Community,Com_sim)
}

Com_sim_sum<-ddply(Com_sim,.(Time,Ecoregion,Scenario),summarise,m_sim=mean(similarity),Num_sum=sum(NumSites))
WM_Com_sim<-ddply(Com_sim_sum,.(Scenario,Time),summarise,Mean=weighted.mean(m_sim,Num_sum,na.rm = T),SD=wt.sd(m_sim,Num_sum))


P1<-ggplot(Com_sim_sum,aes(x=Time,y=m_sim,group=Ecoregion))+geom_line(alpha=0.2)+facet_wrap(~Scenario)
P2<-P1+geom_ribbon(data=WM_Com_sim,aes(y=Mean,ymax=Mean+SD,ymin=Mean-SD,group=NULL),alpha=0.5)+geom_line(data=WM_Com_sim,aes(y=Mean,ymax=Mean+SD,ymin=Mean-SD,group=NULL),size=2,alpha=0.8)
P3<-P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3+ylab("Quantitative Sorensen similarity")+xlab("Time (Years)")+theme(panel.margin = unit(1, "lines"))
ggsave("Figures/Tree_community.pdf",dpi = 400,height=6,width=8,units="in")

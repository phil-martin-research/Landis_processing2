#script to produce statistics for rates of change in Carbon and Nitrogen in each ecoregion
#in this script we use biomass as the metric for abundance of different species

#load packages
library(ggplot2)
library(plyr)
library(reshape)
library(grid)
library(SDMTools)
library(gtools)

#clear objects
rm(list=ls())

#find all the century carbon and nitrogen files
C_N<-list.files(pattern="Century-succession-log",recursive=T)

#run a loop to read in each .csv containing tree species biomass values
#remove columns that are not useful and ass a column to give details of
#the scenario being run
CN_ER<-NULL
for (i in 1:length(C_N)){
  #read in .csv
  File<-read.csv(C_N[i])
  head(File)
  #remove blank column
  File_sub<-File[-c(5:12,14,29:ncol(File))]
  #remove rows containing NAs
  File_sub2<-File_sub[complete.cases(File_sub),]
  head(File_sub2)
  #calculate total carbon
  Total_C<-rowSums (File_sub2[6:ncol(File_sub2)], na.rm = FALSE, dims = 1)/100
  File_sub3<-cbind(File_sub2[,1:5],Total_C)
  #insert a column with the scenario number
  File_sub3$Scenario<-gsub(".*-log|_r.*","", C_N[i])
  File_sub3$Replicate<-sub(".*?_r(.*?).csv.*", "\\1", C_N[i])
  #
  File_sub3$C_change<-NA
  File_sub3$N_change<-NA
  File_sub3<-File_sub3[with(File_sub3, order(Time)), ]
  UN_ER<-unique(File_sub3$EcoregionName)
  
  for (j in 1:length(UN_ER)){
    ER_sub<-subset(File_sub3,EcoregionName==UN_ER[j])
    for (k in 2:nrow(ER_sub)){
      ER_sub$C_change[k]<-ER_sub$Total_C[k]-ER_sub$Total_C[k-1]
      ER_sub$N_change[k]<-ER_sub$TotalN[k]-ER_sub$TotalN[k-1]  
  }
  CN_ER<-rbind(ER_sub,CN_ER)
  }
}

head(CN_ER)
CN_ER_sum<-ddply(CN_ER,.(Time,EcoregionName,Scenario),summarise,m_c=mean(C_change),m_n=mean(N_change),Num_sum=sum(NumSites))
WM_CN<-ddply(CN_ER_sum,.(Scenario,Time),summarise,Mean_C=weighted.mean(m_c,Num_sum,na.rm = T),
             Mean_N=weighted.mean(m_n,Num_sum,na.rm = T),SD_C=wt.sd(m_c,Num_sum),SD_N=wt.sd(m_n,Num_sum))
head(CN_ER_sum)
head(WM_CN)

#make plot of carbon change
theme_set(theme_bw(base_size=12))
P1<-ggplot(CN_ER_sum,aes(x=Time,y=m_c,group=EcoregionName))+geom_line(alpha=0.2)+facet_wrap(~Scenario,scales = "free")
P2<-P1+geom_ribbon(data=WM_CN,aes(y=Mean_C,ymax=Mean_C+SD_C,ymin=Mean_C-SD_C,group=NULL),alpha=0.5)+geom_line(data=WM_CN,aes(y=Mean_C,group=NULL),size=2,alpha=0.8)
P3<-P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3+ylab(expression(paste("Carbon flux (Mg",~Ha^-1,")")))+xlab("Time (Years)")
ggsave("Figures/Carbon_flux.pdf",dpi = 400,height=6,width=8,units="in")

#make plot of nitrogen change
theme_set(theme_bw(base_size=12))
P1<-ggplot(CN_ER_sum,aes(x=Time,y=m_n,group=EcoregionName))+geom_line(alpha=0.2)+facet_wrap(~Scenario,scales="free")
P2<-P1+geom_ribbon(data=WM_CN,aes(y=Mean_N,ymax=Mean_N+SD_N,ymin=Mean_N-SD_N,group=NULL),alpha=0.5)+geom_line(data=WM_CN,aes(y=Mean_N,group=NULL),size=2,alpha=0.8)
P3<-P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3+ylab(expression(paste("Nitrogen flux")))+xlab("Time (Years)")
ggsave("Figures/Nitrogen_flux.pdf",dpi = 400,height=6,width=8,units="in")

#script to look at changes in biomass of each the four most abundant tree species, for each scenario, 
#for each time step, for each ecoregion

library(ggplot2)
library(plyr)
library(reshape)

Eco_region_BM<-list.files(pattern="spp-biomass-log",recursive=T)

BM_ER<-NULL
for (i in 1:length(Eco_region_BM)){
File<-read.csv(Eco_region_BM[i])
File_sub<-File[-c(5:7,9:12,14:15,17:21,23:25,27:ncol(File))]
File_sub2<-File_sub[complete.cases(File_sub),]
File_sub2$Scenario<-gsub(".*-log|_r.*","", Eco_region_BM[i])
BM_ER<-rbind(File_sub2,BM_ER)
}

head(BM_ER)

Mean_ER<-ddply(BM_ER,.(Time,Ecoregion,Scenario),summarise,m_betu_pend=mean(SppBiomass_betupend)/100,
      m_quercus=mean(SppBiomass_querrobu)/100,
      m_pinus_sylv=mean(SppBiomass_pinusylv)/100,
      m_fagus_sylv=mean(SppBiomass_fagusylv)/100,
      m_ilex_aqui=mean(SppBiomass_ilexaqui)/100,
      Num_sum=sum(NumSites))


Mean_ER2<-melt(Mean_ER,id.vars=c("Time","Ecoregion","Scenario","Num_sum"))

head(Mean_ER2)
WM_ER2<-ddply(Mean_ER2,.(Scenario,variable,Time),summarise,Mean=weighted.mean(value,Num_sum,na.rm = T))
head(WM_ER2)

theme_set(theme_bw(base_size=12))
P1<-ggplot(Mean_ER2,aes(x=Time,y=value,group=Ecoregion,colour=variable))+geom_line(alpha=0.5)+facet_grid(variable~Scenario,scales="free")
P2<-P1+geom_line(data=WM_ER2,aes(y=Mean,group=NULL),colour="black",size=2,alpha=0.8)+scale_colour_brewer("Species",palette = "Set1")
P2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Tree_sp_biomass.pdf",dpi = 400,height=8,width=10,units="in")

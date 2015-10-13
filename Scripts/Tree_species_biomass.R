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
      m_ilex_aqui=mean(SppBiomass_ilexaqui)/100)


Mean_ER2<-melt(Mean_ER,id.vars=c("Time","Ecoregion","Scenario"))

Mean_ER3<-ddply(BM_ER,.(Time,Scenario),summarise,
                function(X) data.frame(m_betu_pend=weighted.mean(X$SppBiomass_betupend,X$NumSites,na.rm = T)
                ))

weighted.mean(File_sub2$SppBiomass_betupend,File_sub2$NumSites,na.rm = T)/100
                                       m_quercus=weighted.mean(X$SppBiomass_querrobu,X$NumSites,na.rm = T),
                                       m_pinus_sylv=weighted.mean(X$SppBiomass_pinusylv,X$NumSites,na.rm = T),
                                       m_fagus_sylv=weighted.mean(X$SppBiomass_fagusylv,X$NumSites,na.rm = T),
                                       m_ilex_aqui=weighted.mean(X$SppBiomass_ilexaqui,X$NumSites,na.rm = T)
                                       ))



theme_set(theme_bw(base_size=12))
P1<-ggplot(Mean_ER2,aes(x=Time,y=value,group=Ecoregion,colour=variable))+geom_line(alpha=0.5)+facet_grid(variable~Scenario,scales="free")
P1+

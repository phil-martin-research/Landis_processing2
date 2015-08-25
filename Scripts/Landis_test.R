#load packages
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(ggplot2)

#clear objects
rm(list=ls())

#need to produce:

#(1) - A loop to give figures showing the percentage cover of different age classes for each year, for each scenario
#(2) - Relative cover for species richness of each year for each of the different scenarios
#(3) - Percentage cover for each species for each year of each scenario

par(mfrow=c(1,1))
ER<-raster("Landis run8/ecoregionsNF.gis")
freq(ER)

max(ER)
ER[ER%in%c(1:30)] <- 1
ER[!ER%in%c(1:30)] <- NA
plot(ER)
freq(ER*File)
res(ER)

File[File%in%c(1:490)] <- 1
freq(File)

plot(ER)

#resolution of this differs drastically from ages produced by Landis, why?

#load one dataset
File_names<-list.files("Landis run8/ScenarioLandis7/replicate1/output/cohort-stats/")
File_names<-gsub("-[0-9][0-9].img", "", File_names)
File_names<-gsub("-[0-9].img", "", File_names)
File_names<-gsub("MIN", "MAX", File_names)#crappy fix to remove minimum age files
File_names<-File_names[!grepl("AGE-MAX",File_names)]
File_names<-File_names[!grepl("SPP-RICH",File_names)]


list.files("Landis run8/ScenarioLandis7/")

#to do this properly I need to loop through the folders for the different replicates and different scenarios
Un_files<-unique(File_names)
Ages<-seq(0,50,10)
Cell_stats2<-NULL
for (i in 1:length(Un_files)){
  Cell_stats<-NULL
  File_name<-Un_files[i]
  Mad_stacks<-NULL
  Cell_stats<-NULL
 for (j in 1:length(Ages)){
   File<-raster(paste("Landis run8/ScenarioLandis7/replicate1/output/cohort-stats/",File_name,"-",Ages[j],".img",sep=""))
   Cell_freq<-data.frame(freq(File))
   Cell_freq$Age<-Ages[j]
   Cell_stats<-rbind(Cell_freq,Cell_stats) 
 }
 Cell_stats$species<-gsub("-MAX","",Un_files[i])
 Cell_stats2<-rbind(Cell_stats,Cell_stats2)
}

bins<-seq(1,max(Cell_stats2$value),length.out=5)
head(Cell_stats3)
Cell_stats2$bin<-cut(Cell_stats2$value,bins,include.lowest=F)
Cell_stats3<-ddply(Cell_stats2,.(bin,Age,species),summarise,Count=sum(value))
Cell_stats4<-ddply(Cell_stats3,.(species),mutate,Total=sum(Count),Prop=Count/Total)
ggplot(Cell_stats3,aes(x=Age,y=Count/113297,fill=bin))+geom_bar(stat="identity")+facet_wrap(~species,scale="free_y")

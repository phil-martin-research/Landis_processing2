#script to look at changes in community composition

#name: Phil Martin
#date: 2015-07-22


#load packages
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(scales)
library(vegan)
library(tidyr)


#clear objects
rm(list=ls())

#load file names 
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("*.dbf",File_names)]
File_names<-File_names[!grepl("AGE-MAX",File_names)]
File_names<-File_names[!grepl("SPP-RICH",File_names)]
File_names<-File_names[!grepl("MIN",File_names)]
File_names<-File_names[!grepl("Biomass",File_names)]
File_names<-File_names[!grepl("reclass",File_names)]
File_names<-File_names[!grepl("initial",File_names)]

#loop through all scenarios, replicates, and ages for all species
#and return whether the species was present or absent in the study area
Comm_comp<-NULL
for (j in 1:length(File_names)){
  #tells you how much of the task has been done
  #load in raster file
  File<-raster(File_names[j])
  #produce data table with abundance of age groups
  Cell_freq<-data.frame(freq(File))
  #produce dataframe with presence absence for trees >10 years old for each time step,
  #for each replicate of each scenario
  Pres_abs<-data.frame(Pres=ifelse(sum(Cell_freq[-(1:2),2])>1,1,0),
  species=sub(".*?/(.*?)-MAX.*", "\\1", File_names[j]),
  scenario=gsub("_r.*","",sub(".*?cohort-stats(.*?)/.*", "\\1", File_names[j])),
  replicate=sub(".*?_r(.*?)/.*", "\\1", File_names[j]),
  age=as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?MAX-","", File_names[j]))))
  #bind results to last set of loop results
  Comm_comp<-rbind(Pres_abs,Comm_comp) 
  print(paste("Percent done:",100*(j/length(File_names)),"%")) 
}

#put this data into a matrix so that species are columns and the values in the columns
#are species presence or absence
CC_cast<-cast(Comm_comp,scenario+replicate+age~species,value="Pres",fun=sum)

head(CC_cast)

#now work out changes in community composition using Sorensen index

#create dataframe for each unique combination of scenario and replicate
Uniq_combs<-unique(CC_cast[c("scenario", "replicate")])
Com_sim<-NULL
for (i in 1:nrow(Uniq_combs)){#for each unique combination of scenario and replicate
  #subset species composition to give all time steps
  Sub_CC<-subset(CC_cast,scenario==Uniq_combs$scenario[i]&replicate==Uniq_combs$replicate[i])
  #drop columns that do not refer to species presence/absence
  Sub_CC2<-Sub_CC[,-(1:3)]
  #create dataframe including scenario, replicate and time
  #along with sorensen index using time 0 as a comparitor
  #currently all of these equal 1, as no species are lost or gained
  Community<-data.frame(Sub_CC[,(1:3)],similarity=c(1,(1-vegdist(Sub_CC2))[1:10]))
  #bind results to last set of loop results
  Com_sim<-rbind(Community,Com_sim)
}

#reverse order of scenario factor levels for plotting
Com_sim$scenario <- factor(Com_sim$scenario, levels=rev(levels(Com_sim$scenario)))

#now create plots to show changes in community similarity for different scenarios
theme_set(theme_bw(base_size=12))
P1<-ggplot(Com_sim,aes(x=age,y=similarity))+geom_point()+geom_line()+facet_wrap(~scenario)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Sorensen similarity")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Community_similarity.pdf",height=4,width=8,units="in",dpi=400)

#script to give figures showing the percentage cover 
#of different species > 10years in age for each year, for each landis scenario

#load packages
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(ggplot2)
library(scales)

#clear objects
rm(list=ls())

#load one dataset
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("AGE-MAX",File_names)]
File_names<-File_names[!grepl("SPP-RICH",File_names)]
File_names<-File_names[!grepl("MIN",File_names)]

#loop to get statistics for each species at each time point for each scenario
Cell_stats<-NULL
for (j in 1:length(File_names)){
  print(j)
  File<-raster(File_names[j])
  Cell_freq<-data.frame(freq(File))
  Cell_freq$count<-Cell_freq$count+round(rnorm(1,0,100),0) #this line is just so we can produce error bars for the moment
  Cell_freq$scenario<-sub(".*?Landis run8/(.*?)/.*", "\\1", File_names[j])
  Cell_freq$replicate<-gsub("^.*?/","",sub(".*?replicate(.*?)/output.*", "\\1", File_names[j]))
  Cell_freq$age<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?MAX-","", File_names[j])))
  Cell_freq$species<-gsub("^.*?/","",sub(".*?cohort-stats/(.*?)-MAX.*", "\\1", File_names[j]))
  Cell_stats<-rbind(Cell_freq,Cell_stats) 
}

#now create summary of these results so they can be plotted
bins<-c(10,500)
Cell_stats$bin_cut<-cut(Cell_stats$value,bins,include.lowest=T,labels =c(500))
Cell_stats<-subset(Cell_stats,!is.na(bin_cut))
Cell_stats2<-ddply(Cell_stats,.(species,age,scenario,replicate),summarise,Total=sum(count)/3)
Cell_stats3<-ddply(Cell_stats2,.(species,age,scenario),summarise,Av=mean(Total),std.dev=sd(Total))


#now plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats3,aes(x=age,y=((Av)/113321)*100,ymax=((Av+std.dev)/113321)*100,ymin=((Av-std.dev)/113321)*100,T,colour=scenario))+geom_line(alpha=0.8)+geom_pointrange(shape=1,alpha=0.8)+facet_wrap(~species)+scale_colour_brewer("Species",palette="Set2")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Species_cover_10yrs.pdf",height=6,width=8,units="in",dpi=400)

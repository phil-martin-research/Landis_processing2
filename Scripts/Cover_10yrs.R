#script to give figures showing the total cover  
#of trees >10 years old for each year, for each landis scenario

#load packages
library(raster)
library(rgdal)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

#clear objects
rm(list=ls())

#load one dataset
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[grepl("AGE-MAX",File_names)]

n<-0
Cell_stats<-NULL
for (j in 1:length(File_names)){
  j<-1
  File<-raster(File_names[j])
  plot(File)
  Cell_freq<-data.frame(freq(File))
  Cell_freq$scenario<-gsub("_r.*","",sub(".*?cohort-stats(.*?)/.*", "\\1", File_names[j]))
  Cell_freq$replicate<-sub(".*?_r(.*?)/.*", "\\1", File_names[j])
  Cell_freq$age<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?AGE-MAX-","", File_names[j])))
  Cell_stats<-rbind(Cell_freq,Cell_stats)
  n<-n+1
  print((n/length(File_names))*100)
}


#summarise these results
bins<-c(10,500)
Cell_stats$bin_cut<-cut(Cell_stats$value,bins,include.lowest=T,labels =c(">10"))
Cell_stats2<-ddply(Cell_stats,.(bin_cut,age,scenario,replicate),summarise,Total=((sum(count))/27636)*100)
Cell_stats3<-ddply(Cell_stats2,.(bin_cut,age,scenario),summarise,Total2=mean(Total),std.dev=sd(Total))
Cell_stats3<-subset(Cell_stats3,!is.na(bin_cut))

#now plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats3,aes(x=age,y=Total2,ymax=Total2+std.dev,ymin=Total2-std.dev,colour=scenario))+geom_line(alpha=0.5)+geom_pointrange(shape=1,alpha=0.5)+scale_colour_brewer("Scenario",palette="Set1")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage of area")+xlab("Year")+scale_y_continuous(labels = comma)+facet_wrap(~scenario)
ggsave("Figures/Cover_10years.pdf",height=4,width=8,units="in",dpi=400)

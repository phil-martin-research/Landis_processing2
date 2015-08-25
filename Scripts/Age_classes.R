#script to give figures showing the percentage cover 
#of different age classes for each year, for each landis scenario


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
File_names<-File_names[!grepl("*.dbf",File_names)]
File_names<-File_names[grepl("AGE-MAX",File_names)]

Cell_stats<-NULL
  for (j in 1:length(File_names)){
    print(paste("Percent done:",100*(j/length(File_names)),"%"))
    File<-raster(File_names[j])
    Cell_freq<-data.frame(freq(File))
    Cell_freq$scenario<-sub(".*?Landis_output/(.*?)/.*", "\\1", File_names[j])
    Cell_freq$replicate<-gsub("^.*?/","",sub(".*?run_(.*?)/output.*", "\\1", File_names[j]))
    Cell_freq$age<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?AGE-MAX-","", File_names[j])))
    Cell_stats<-rbind(Cell_freq,Cell_stats) 
}

head(Cell_stats)

bins<-c(10,30,60,100,500)
Cell_stats$bin_cut<-cut(Cell_stats$value,bins,include.lowest=T,labels =c("10-30","31-60","60-100",">100"))
Cell_stats2<-ddply(Cell_stats,.(replicate,bin_cut,age,scenario),summarise,Total=sum(count))
Cell_stats3<-ddply(Cell_stats2,.(bin_cut,age,scenario),summarise,Total2=mean(Total),sd=sd(Total))
Cell_stats3<-subset(Cell_stats3,!is.na(bin_cut))

head(Cell_stats)

#now plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats3,aes(x=as.factor(age),y=(Total2/113321)*100,fill=bin_cut))+geom_bar(stat="identity")+facet_wrap(~scenario)+scale_fill_brewer("Age",palette="Blues")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Age_class.pdf",height=4,width=8,units="in",dpi=400)

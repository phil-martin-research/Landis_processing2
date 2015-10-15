#script to give figures showing the percentage cover 
#of different species richness classes for each year, for each landis scenario

#load packages
#i have changed this
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(ggplot2)
library(scales)

#clear objects
rm(list=ls())

#returns all .img files in any folders in the same directory
File_names<-list.files(pattern="*.img",recursive=T)
#selects species richness .img files
Mask_names<-File_names[grepl("AGE-MAX",File_names)]
Mask_names<-Mask_names[!grepl("dbf",Mask_names)]
File_names<-File_names[grepl("SPP-RICH",File_names)]
File_names<-File_names[grepl("SPP-RICH",File_names)]

plot(raster(Mask_names[1]))

#create matrix for reclassification
m <- c(0, 10, -NA,  11, 500, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
#loop to give output for analysis of species richness
Cell_stats<-NULL
for (j in 1:length(File_names)){
  print(paste("Percent done:",100*(j/length(File_names)),"%"))
  #loads individual .img file
  File<-raster(File_names[j])
  #reclassify age raster so that pixel with age >10 = NA
  rc <- reclassify(raster(Mask_names[j]), rclmat)
  File<-(mask(File,rc))
  #produces table showing frequency of different levels of spp richness
  Cell_freq<-data.frame(freq(File)) 
  #puts scenario name in a column
  Cell_freq$scenario<-gsub("_r.*","",sub(".*?cohort-stats(.*?)/.*", "\\1", File_names[j])) 
  #puts replicate name in column
  Cell_freq$replicate<-as.numeric(sub(".*?_r(.*?)/.*", "\\1", File_names[j]))
  #puts age in column
  Cell_freq$age<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?SPP-RICH-","", File_names[j])))
  #binds together all frequency tables
  Cell_stats<-rbind(Cell_freq,Cell_stats) 
}





head(Cell_stats)
#now bin data and calculate means

#use this to define the bins you want to cut the data into
bins<-c(1,2,4,6,8,10,100) 
#this puts in the labels for the bins
Cell_stats$bin_cut<-cut(Cell_stats$value,bins,include.lowest=T,labels =c("1-2","3-4","5-6","7-8","9-10",">10")) 
#remove na values from the dataset
Cell_stats<-subset(Cell_stats,!is.na(bin_cut))
#this sums the number of cells for each different combination of bin, age, scenario and replicate
Cell_stats2<-ddply(Cell_stats,.(bin_cut,age,scenario,replicate),summarise,Total=sum(count))
#calculates mean and standard deviation for each combination of bin, age and scenario
Cell_stats3<-ddply(Cell_stats2,.(bin_cut,age,scenario),summarise,Total2=mean(Total),SD=sd(Total))


#now plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats3,aes(x=as.factor(age),y=(Total2/27636)*100,fill=bin_cut))+geom_bar(stat="identity")+facet_wrap(~scenario)+scale_fill_brewer("Species richness",palette="Blues")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Sp_R_class.pdf",height=6,width=8,units="in",dpi=400)

#an alternative way of showing the same data
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats3,aes(x=age,y=(Total2/27636)*100,ymax=((Total2+SD)/27636)*100,ymin=((Total2-SD)/27636)*100,colour=bin_cut))+geom_pointrange(stat="identity")+geom_line()+facet_wrap(~scenario)+scale_colour_brewer("Species richness",palette="Set1")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Sp_R_class2.pdf",height=6,width=8,units="in",dpi=400)



#script to produce maps of biodiversity, ecosystem function and ecosystem services
#based on Paul's gradient plots

#author: Phil martin
#Date 2015/09/24

#open packages
library(raster)
library(ggplot2)
library(reshape2)
library(plyr)



#load data Landis II outputs
raster("Century_outputs/")

File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("*.dbf",File_names)]
File_names<-File_names[grepl("TotalBiomass",File_names)]


plot((((raster(File_names[1]))/100)*0.005345)+4.358917)

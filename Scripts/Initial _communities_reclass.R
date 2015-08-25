library(raster)
library(SDMTools)
library(ggplot2)

#import data
Init_comm<-raster("initial-communities_102.img")



Un_vals<-unique(values(Init_comm)) #produce vector of unique values for groups IDs
R2<-raster(Init_comm)
for (i in 2:length(Un_vals)){
  print(paste("percent done = ",round((i/length(Un_vals))*100,digits = 2),sep=""))
  Init_comm2<-Init_comm
  Init_comm2[Init_comm2==Un_vals[i]]<-1
  Init_comm2[Init_comm2!=1]<-NA
  plot(Init_comm2)
  R1<-ConnCompLabel(Init_comm2)
  rclmat<-matrix(cbind(unique(R1),as.numeric(paste(Un_vals[i],unique(R1),sep=""))),ncol=2)
  R1_reclass<-reclassify(R1,rclmat)
  R2<-merge(R1_reclass,R2)
}


plot(R2)

writeRaster(R2,"Inital_comm_reclass.img",format="HFA")



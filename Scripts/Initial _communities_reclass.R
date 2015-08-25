library(raster)
library(SDMTools)
library(ggplot2)

#import data
Init_comm<-raster("initial-communities_102.img")


plot(Init_comm)


Un_vals<-unique(values(Init_comm)) #produce vector of unique values for groups IDs
R2<-raster(Init_comm)
for (i in 2:length(Un_vals)){
  print(paste("percent done = ",round((i/length(Un_vals))*100,digits = 2),sep=""))
  Init_comm2<-Init_comm
  Init_comm2[Init_comm2==Un_vals[i]]<-9999999
  Init_comm2[Init_comm2!=9999999]<-NA
  Init_comm2[Init_comm2==9999999]<-1
  R1<-ConnCompLabel(Init_comm2)
  rclmat<-matrix(cbind(unique(R1),as.numeric(paste(Un_vals[i],".",unique(R1),sep=""))),ncol=2)
  R1_reclass<-reclassify(R1,rclmat)
  R2<-merge(R1_reclass,R2)
}

New_unique<-seq(1:length(unique(R2)))

New_matrix<-matrix(c(unique(R2),New_unique),ncol=2)

R2_reclass<-reclassify(R2,New_matrix)
plot(R2_reclass)


writeRaster(R2_reclass,"Inital_comm_reclass.img",format="HFA",overwrite=T)

Comm_reclass<-raster("Inital_comm_reclass.img")

length(unique(Comm_reclass))
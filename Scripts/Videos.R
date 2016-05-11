#script to produce animations of maps from landis outputs

library(raster)
library(animation)
library(gtools)
library(ggplot2)
library(ggmap)
?get_map

M1<-get_map(location = c(lat = 50.8633459, lon = -1.6207238), zoom = 11, maptype = 'terrain')

ggmap(M1)

oopts<-if (.Platform$OS.type == "windows") {
  ani.options(ffmpeg = "C:/Program Files/ffmpeg/bin/ffmpeg.exe")
}

#find rasters for species richness
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[grepl("SPP-RICH",File_names)]
File_names<-mixedsort(File_names,decreasing = T)

#produce a list of the unique scenarios
scenarios<-unique(sub(".*?Species_ric/(.*?)/.*", "\\1", File_names))

for (i in 1:length(scenarios)){
  scen_sub<-File_names[grepl(scenarios[i],File_names)]
  saveVideo(for(j in 1:length(scen_sub))
  {
    map.p <- rasterToPoints((raster(scen_sub[j])))
    df <- data.frame(map.p)
    colnames(df) <- c("x", "y", "SpR")
    year<-unique(na.omit(as.numeric(unlist(strsplit(unlist(scen_sub[j]), "[^0-9]+")))))
    P1<-ggplot(data=df, aes(y=y, x=x)) +
      geom_raster(aes(fill=SpR))+
      scale_fill_gradient("Species richness",low="white",high="green",limits=c(0, 15))+
      coord_equal()+
      theme_bw()+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())+
      ggtitle(label = paste(year,"years"))
    print(P1)
  }
  ,video.name=paste("Videos/",scenarios[i],".mp4",sep=""),
  ffmpeg="C:/Program Files/ffmpeg/bin/ffmpeg.exe",
  interval=2,
  title = "Change in New Forest")
}
 

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
    head(df)
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
            panel.grid.minor=element_blank(),plot.background=element_blank())
    print(P1)
    ani.options(interval = 5)   
  }
  ,video.name=paste("Videos/",scenarios[i],".mp4",sep=""),
  ffmpeg="C:/Program Files/ffmpeg/bin/ffmpeg.exe")
}
 
?saveVideo

## usually Linux users do not need to worry about the ffmpeg path as long as
## FFmpeg or avconv has been installed
saveVideo({
  par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8,
      cex.lab = 0.8, cex.main = 1)
  ani.options(interval = 0.05, nmax = 300)
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, video.name = "BM.mp4", other.opts = "-pix_fmt yuv420p -b 300k")
# higher bitrate, better quality
ani.options(oopts)
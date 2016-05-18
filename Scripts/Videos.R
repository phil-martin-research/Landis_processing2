#script to produce animations of maps from landis outputs


library(raster)
library(animation)
library(gtools)
library(ggplot2)
library(ggmap)
library(gganimate)


#find rasters for species richness
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[grepl("TotalBiomass",File_names)]
File_names<-mixedsort(File_names,decreasing = T)


#produce a list of the unique scenarios
scenarios<-unique(sub(".*?Species_ric/(.*?)/.*", "\\1", File_names))

df_all<-NULL
for (i in 1:length(scenarios)){
  scen_sub<-File_names[grepl(scenarios[i],File_names)]
  for(j in 1:length(scen_sub))
  {
    map.p <- rasterToPoints((raster(scen_sub[j])))
    df <- data.frame(map.p)
    colnames(df) <- c("x", "y", "SpR")
    year<-unique(na.omit(as.numeric(unlist(strsplit(unlist(scen_sub[j]), "[^0-9]+")))))
    P1<-ggplot(data=df, aes(y=y, x=x)) +
      geom_raster(aes(fill=SpR))+
      scale_fill_gradient("Tree biomass",low="white",high="dark green",limits=c(0, 50000))+
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
    ggsave(paste("Videos/pngs/",scenarios[i],
                 ifelse(j<10,"_00","_0")
                 ,j,".png",sep=""))
    #df$year<-unique(na.omit(as.numeric(unlist(strsplit(unlist(scen_sub[j]), "[^0-9]+")))))
  }
}
 

#try to get landis to plot over a google map

Landis<-raster(File_names[1])

plot(Landis)

al1<-get_map(location = c(lon = -1.6141243, lat = 50.8814146), zoom = 10, maptype = 'roadmap')
ggmap(al1)

map.p <- rasterToPoints(Landis)
df <- data.frame(map.p)
colnames(df) <- c("x", "y", "SpR")
df$lon<-(df$x/111248.23835493479)+(-1.753189)
df$lat<-(df$y/111248.23835493479)+50.754803

ggmap(al1)+geom_raster(data=df,aes(x=lon,y=lat,fill=SpR))+
  scale_fill_gradient("Number of tree species",low="white",high="dark red",limits=c(0, 15))+coord_cartesian()

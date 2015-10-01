
#load packages
library(ggplot2)
library(plyr)
library(tidyr)

Rec_aes<-read.csv("Data/Arjan_recreation_aesthestic_PM.csv")
Rec_aes_melt<-melt(Rec_aes,id.vars = "ID")
head(Rec_aes_melt)

#classify by collapse stage
Collapse<-seq(1,5,by = 1)
Rec_aes_melt$Collapse<-NA
for (i in 1:length(Collapse)){
  Rec_aes_melt$Collapse<-ifelse(grepl(i,Rec_aes_melt$variable)==T,i,Rec_aes_melt$Collapse)
}

#classify by receration/aesthetic
Rec_aes_melt$Type<-ifelse(grepl("aes",Rec_aes_melt$variable)==T,"Aesthetic","Recreation")
Rec_aes_melt<-Rec_aes_melt[-2]

Aes_reorg<-spread(Rec_aes_melt,Type,value)
head(Aes_reorg)
head(Rec_aes_melt)
ggplot(Rec_aes_melt,aes(x=Collapse,y=value,group=ID))+geom_point()+facet_wrap(~Type)+geom_smooth(method="glm",family="poisson",aes(group=NULL))


#import Paul's gradient data
BM<-read.csv("Data/BMLan.csv",header = T)
head(BM<-read.csv("Data/BMLan.csv",header = T))
BM$Collapse<-as.numeric(substring(BM$Plot, 1, 1))
AGB_collapse<-ddply(BM,.(Collapse),summarise,mean_AGB=mean(AGB))

Aes_reorg2<-merge(Aes_reorg,AGB_collapse,by = "Collapse")
head(Aes_reorg2)
write.csv(Aes_reorg2,"Data/Rec_aes_reorg.csv",row.names=F)


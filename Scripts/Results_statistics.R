#script to analyse the change in resilience of biodiversity and ecosystem services 
#over a gradient of increasing disturbance for Elena's work
library(plyr)


Persistence<-read.csv("Data/R_output/Persistence_replicates.csv")

colnames(Persistence)

<-ddply(Persistence,.(Scen_lab,Replicate,Scen_lab2,variable),summarise,m_value=mean(value))

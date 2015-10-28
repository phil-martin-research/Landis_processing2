################## SUMMARIZE MAPS FROM LANDIS-II RUNS #####################
########################## Summarizes across multiple LANDIS-II scenarios  ################################
#################################Created by M. Lucash, adapted from M. Creutzburg's file

# Load libraries
install.packages("sqldf")
install.packages("plotrix")

library(sqldf)
library(plyr)
library(rgdal)   #Needed for rasters
library(raster)  #Needed for rasters
library(plotrix) #Needed for SE
library(ggplot2)

ecoregion_map<-raster("E:/NERC_BESS/Landis_processing2/ecoregions_woodext.img")
#plot(ecoregion_map)
ecoregion_DF_initial<-as.data.frame(ecoregion_map)

colnames(ecoregion_DF_initial)<-'Ecoregion'
subset_eco<-subset(ecoregion_DF_initial, ecoregion_DF_initial$Ecoregion > 0 & ecoregion_DF_initial$Ecoregion <31)
active_cells<-nrow(subset_eco)
print(nrow(ecoregion_DF_initial))
print(nrow(subset_eco))

# Set working directory for output
dir <- "K:/Research_Faculty/AFRI_Chippewa_Project/Output_From_Sims/"
setwd(dir)

# Set up empty objects
no_reps <- 1  # input nummber of replicates here
all_data <- NULL
#all_month_data <- NULL
#all_harv_data <- NULL

# List out scenario folder names in scenario_list object. Only use non-Adapt scenarios
#scenario_list <- c("Results_041915_CanESM2_rcp45_Wind", "Results_041915_CanESM2_rcp45", "Results_041915_ACCESS_rcp45", "Results_041915_ACCESS_rcp45_Wind")
#scenario_list <- c( "CNF_Landscape_NovMeeting_ACCESS_45-BAU", "CNF_Landscape_NovMeeting_ACCESS_45-EcoGoods", "CNF_Landscape_NovMeeting_ACCESS_45-EcosystemServices", 
                "CNF_Landscape_NovMeeting_CCSM4_rcp-85-BAU", "CNF_Landscape_NovMeeting_CCSM4_rcp-85-EcoGoods", "CNF_Landscape_NovMeeting_CCSM4_rcp-85-EcoServices", "CNF_Landscape_NovMeeting_CSIRO_85-BAU", "CNF_Landscape_NovMeeting_CSIRO_85-EcoGoods", 
                "CNF_Landscape_NovMeeting_CSIRO_85-EcoServices", "CNF_Landscape_NovMeeting_GFDL_ESM285-BAU", "CNF_Landscape_NovMeeting_GFDL_ESM285-EcoGoods", 
                "CNF_Landscape_NovMeeting_GFDL_ESM285-EcosystemServices", "CNF_Landscape_NovMeeting_Historic-BAU", "CNF_Landscape_NovMeeting_Historic-EcosystemGoods", "CNF_Landscape_NovMeeting_Historic-EcosystemServices")
#Testing only
scenario_list<-c("Results_101615_Validation_CNF_v1")

# Load lookup tables for joining to data.  
#Scenario_LUT <- read.csv ("LUT_Scenarios.csv")
Year_LUT <- read.csv ("K:/Research_Faculty/AFRI_Chippewa_Project/Output_From_Sims/LUT_Year.csv")
Time<-Year_LUT[,"Time"]
Time_unique<-unique(Time)
#Time_unique_nozero<-subset(Time_unique, Time_unique>0)

##################################Output Species Biomass Loop#############################
#Now I read in all the LANDIS maps.  For species looping, I used the truncated species name (8 letters) because it matches the filenames in LANDIS.
spplist<-read.csv("I:/Research/Shares/scheller_lab/Lucash/AFRI_Chippewa_Project/GIS/Model_Validation/spp_list_total.csv")
unique_spp_L<-sort(spplist[,1])

#Set up a null matrix
LANDIS_spp_output_matrix<-NULL
for (r in 1:length(scenario_list)){   # for each scenario and replicate, compiles data into a single data frame
  scenario<-(scenario_list[r])
    
for (m in 1:length(Time_unique)){   # for each scenario and replicate, compiles data into a single data frame  
  time<-(Time_unique[m])
  
for (k in 1:length(unique_spp_L)){#for each species...
  spp<-(unique_spp_L[k])
  #print(spp)
  #spp_LANDIS<-raster(paste(dir,scenario, "/output-leaf-biomass/",spp,"-", time,".img",sep=""))#LANDIS unique spp biomass.
  spp_LANDIS<-as.data.frame(raster(paste(dir,scenario, "/output-leaf-biomass/",spp,"-", time,".img",sep="")))#LANDIS unique spp biomass.
  #LANDIS_manage_df<-cbind(spp_LANDIS, management_DF_initial)
  colnames(spp_LANDIS)<-c("LANDIS_Biomass")
  #avg_biomass <-mean(subset_Manage_area$LANDIS)
     sum_biomass <-sum(spp_LANDIS$LANDIS)
     avg_biomass<-(sum_biomass/active_cells)
    SE_biomass <-std.error(spp_LANDIS$LANDIS)
    LANDIS_spp_output_row<-cbind(scenario, time, spp, avg_biomass, SE_biomass)
    LANDIS_spp_output_matrix<-rbind(LANDIS_spp_output_matrix,LANDIS_spp_output_row)
  }#closes management loop
} #closes species loop
} #closes time loop
} #closes scenario loop
colnames(LANDIS_spp_output_matrix)<-c("Scenario", "Time", "Species", "Avg_Biomass_gm2", "SE_Biomass")
write.csv(LANDIS_spp_output_matrix,"I:/Research/Shares/scheller_lab/Lucash/AFRI_Chippewa_Project/Output_Analysis/Results_Meeting_Nov_2015/LANDIS_SppBiomass_ByManagementArea_oldnames_101615.csv", row.names=FALSE)  
head(LANDIS_spp_output_matrix)

spp_crosswalk<-read.csv("I:/Research/Shares/scheller_lab/Lucash/AFRI_Chippewa_Project/GIS/Model_Validation/spp_list_numbered_total.csv")
nrow(spp_crosswalk)

old_names<-read.csv("I:/Research/Shares/scheller_lab/Lucash/AFRI_Chippewa_Project/Output_Analysis/Results_Meeting_Nov_2015/LANDIS_SppBiomass_ByManagementArea_oldnames_101615.csv")
head(old_names)

spp_matrix_LANDIS_final<-NULL
for (r in 1:nrow(old_names)){
  each_spp_row<-old_names[r,]
  spp_short_code<-each_spp_row[,"Species"]
    
  for (o in 1:nrow(spp_crosswalk)){
    spp_crosswalk_num<-spp_crosswalk[o,"number"]
    if (spp_crosswalk_num == spp_short_code){
      new_spp<-(spp_crosswalk[o,"species"])
      new_spp_m<-as.matrix(new_spp)
      colnames(new_spp_m)<-("LANDIS_Species")     
    }    
  }#close loop around lookup table
  spp_added_to_LANDIS<-cbind(each_spp_row,new_spp_m)   
  spp_matrix_LANDIS_final<-rbind(spp_matrix_LANDIS_final,spp_added_to_LANDIS)
}

write.csv(spp_matrix_LANDIS_final,"I:/Research/Shares/scheller_lab/Lucash/AFRI_Chippewa_Project/Output_Analysis/Results_Meeting_Nov_2015/LANDIS_SppBiomass_ByManagementArea_101615.csv", row.names=FALSE)



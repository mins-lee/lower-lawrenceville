rm(list=ls())
library(dplyr)
library(tidyr)
library(sf)
library(tidycensus)


#read in event study files
load("quantitative analysis/files for event study.Rdata")

#create bnp treatment zones - block groups minus parcles

#read in block groups
load("quantitative analysis/block groups.Rdata")
blocks<-alle_co_block_groups%>%
  st_transform(crs="+init=epsg:4326")

#make join blocks to bnps to turn bnp geometry into block polygons
bnp_blocks<-blocks%>%
  st_join(bnps,st_contains,left=FALSE)

#loop through each row in bnp_blocks, subtract out the corresponding parcel
for(i in 1:nrow(bnp_blocks)){
  #limit to just row i
  temp<-bnp_blocks[i,]
  #match to bnp_parcels
  temp_parcel<-bnp_parcels[bnp_parcels$Development.Projects==temp$Development.Projects,"geometry"]
  temp2<-st_difference(temp,temp_parcel)
  
  if(i==1){
    blocks_and_parcels<-temp2
  }else{
    blocks_and_parcels<-rbind(blocks_and_parcels,temp2)
  }
  
}

#save the datasets needed for shiny
save(voucher_stays,bnp_blocks,bnp_parcels,blocks_and_parcels,file="for shiny/visualize_event_study/data for event study visualizer.Rdata")
load("for shiny/visualize_event_study/data for event study visualizer.Rdata")

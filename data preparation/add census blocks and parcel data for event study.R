rm(list=ls())
library(dplyr)
library(sf)
library(tidyr)
library(tidycensus)

#read in allegheny county blocks 

load("quantitative analysis/block groups.Rdata")
blocks<-alle_co_block_groups%>%
  st_transform(crs="+init=epsg:4326")
  
#read in parcel boundaries
parcels<-st_read("quantitative analysis/Allegheny_County_Parcel_Boundaries/Allegheny_County_Parcel_Boundaries.shx")%>%
  st_transform(crs="+init=epsg:4326")

# load geocoded voucher data
load("cleaned and geocoded data 01-April.Rdata")

# load the dataframe of moves (for calculating disadvantages)

load("east end moves.Rdata")

# load bnp data
load("quantitative analysis/combined apartments commercial bnps.Rdata")


# create a dataframe of parcels corresponding to bnps/apartments (used to exclude treatment matches)
bnp_parcels<-parcels%>%
  st_join(apartments_commercial_bnps,st_intersects,left=FALSE)%>%
  #some parcels overlap (subparcels on parcels), so we want to use the parcel with the largest area
  group_by(`Development.Projects`,`Date-Completed`,Type)%>%
  slice(which.max(SHAPE_Area))%>%
  ungroup()


# add census blocks to bnps (this is separate from parcels), used to find treatment matches
bnps<-apartments_commercial_bnps%>%
  st_join(blocks,st_within)

#
voucher_stays<-cleaned_and_geocoded%>%
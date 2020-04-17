rm(list=ls())
library(dplyr)
library(sf)
library(tidyr)
library(tidycensus)
library(leaflet)

#read in allegheny county blocks 

load("quantitative analysis/block groups.Rdata")
blocks<-alle_co_block_groups%>%
  st_transform(crs="+init=epsg:4326")
  
# read in neighborhood data
hoods<-st_read("data preparation/geocode addresses/Neighborhoods_/Neighborhoods_.shx")%>%
  select(hood)

#read in parcel boundaries
parcels<-st_read("quantitative analysis/Allegheny_County_Parcel_Boundaries/Allegheny_County_Parcel_Boundaries.shx")%>%
  st_transform(crs="+init=epsg:4326")

# load geocoded voucher data
load("cleaned and geocoded data 01-April.Rdata")

# load the dataframe of moves (for calculating disadvantages)

load("quantitative analysis/east end moves.Rdata")

# load bnp data
load("quantitative analysis/combined apartments commercial bnps.Rdata")

#read in zoning data
#read in zoning data
zoning<-read_sf("quantitative analysis/zoning/zoning.geojson")%>%
  select(full_zoning_type)

# create a dataframe of parcels corresponding to bnps/apartments (used to exclude treatment matches)
bnp_parcels<-parcels%>%
  st_join(apartments_commercial_bnps,st_intersects,left=FALSE)%>%
  #some parcels overlap (subparcels on parcels), so we want to use the parcel with the largest area
  group_by(`Development.Projects`,`Date-Completed`,Type)%>%
  slice(which.max(SHAPE_Area))%>%
  ungroup()%>%
  #get rid of duplicate projects while preserving type, 
  group_by(`Development.Projects`,`Date-Completed`)%>%
  summarise(Type=max(Type))%>%
  ungroup()%>%
  #get rid of Google Offices since its located on the same parcel / geoid as bakery square
  filter(!grepl("Google",`Development.Projects`))


# add census blocks to bnps (this is separate from parcels), used to find treatment matches
bnps<-apartments_commercial_bnps%>%
  st_join(blocks,st_within)%>%
  #join in neighborhoods
  st_join(hoods,st_within)%>%
  #limit only to bnps for which we have a parcel match
  filter(`Development.Projects`%in%bnp_parcels$`Development.Projects`)%>%
  #get rid of duplicates while preserving type
  group_by(`Development.Projects`,`Date-Completed`,GEOID,hood)%>%
  summarise(Type=max(Type))%>%
  ungroup()

#change type for Rippey street to "mixed income"
bnps[bnps$Development.Projects=="Rippey Apartment Complex",]$Type<-"Mixed-income"

# add census blocks to voucher stay data
voucher_stays<-cleaned_and_geocoded%>%
  st_join(blocks,st_within)%>%
  #also add zoning data
  st_join(zoning,st_within)

# add census blocks to voucher moves
voucher_moves<-moves%>%
  st_as_sf()%>%
  st_join(blocks,st_within)%>%
  #also add zoning data
  st_join(zoning,st_within)

#save all the datasets we just created
save(bnps,bnp_parcels,voucher_stays,voucher_moves,
     file="quantitative analysis/files for event study.Rdata")

# test1<-bnp_parcels%>%
#   filter(grepl('Bakery Square|Google',`Development.Projects`))
# 
# leaflet()%>%
#   addProviderTiles(providers$CartoDB.Positron)%>%
#   addPolylines(data=st_as_sf(test1))
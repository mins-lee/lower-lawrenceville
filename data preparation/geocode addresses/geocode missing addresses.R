rm(list=ls())
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggmap)
library(rgdal)
library(tidycensus)
options(scipen = 999)

load("data preparation/geocode addresses/ all recipient addresses geocoded.Rdata")
load("data preparation/all data clean 29-feb-2020.RData")
load("data preparation/geocode addresses/api_key.RData")


#geocode anything that wasn't geocoded before

not_geocoded<-dat_cleaned5%>%
  #create the for_geocode_addr column
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  #identify addresses that haven't been geocoded yet
  filter(!for_geocode_addr%in%all_geocoded_addr$for_geocode_addr)

#296 addresses haven't been geocoded, so geocode those

missing_addresses_geocoded<-mutate_geocode(not_geocoded,
                                           for_geocode_addr,
                                           override_limit=TRUE)

#add missing addresses to all_geocoded_addr
all_geocoded_addr<-rbind(missing_addresses_geocoded%>%
                           select(for_geocode_addr,lon,lat),
                         all_geocoded_addr%>%
                           select(-geo_batch))

#download Allegheny county boundaries from ACS to identify addresses
alle_co_boundaries<-get_acs(year=2017,variables="B01001_001",state="PA",county="Allegheny",
                            geography="county",
                            geometry=TRUE)

#identify addresses in allegheny county, assume they're correctly geocoded
correctly_coded<-all_geocoded_addr%>%
  #remove missing geocode values
  filter(!is.na(lat))%>%
  st_as_sf(coords=c("lon","lat"),crs = "+init=epsg:4326")%>%
  st_join(alle_co_boundaries%>%
            #change crs for census to match Google's
            st_transform(crs="+init=epsg:4326"),
          join=st_within,left=FALSE)

#subtract out correctly coded to identify incorrectly coded
incorrectly_coded<-all_geocoded_addr%>%
  filter(!for_geocode_addr%in%correctly_coded$for_geocode_addr)

#save geocoded addresses
save(all_geocoded_addr,
     correctly_coded,
     file="data preparation/geocode addresses/ all recipient addresses geocoded.Rdata")

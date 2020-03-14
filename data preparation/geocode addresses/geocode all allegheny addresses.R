rm(list=ls())
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggmap)
library(rgdal)
library(tidycensus)
options(scipen = 999)
#load dataset
load("data preparation/all data clean 29-feb-2020.RData")
#load google api key
load("data preparation/geocode addresses/api_key.RData")

#load previously geocoded addresses, store in separate file (since "geocode_results" will be overwritten)
# combine the previously geocoded results with the newly geocoded ones
load("data preparation/geocode addresses/geocoded addresses all 15206 individuals.Rdata")
#store previously coded in separate dataframe
previously_geocoded<-geocode_results
#remove geocode_results just to be safe
rm(geocode_results)

#load api key for geocoder
register_google(key=api_key,account_type="standard")

#read in the east lib addresses that we've already geocoded
potential_east_lib_addresses<-read_csv("data preparation/geocode addresses/addr for potential east lib res.csv")

#since the potential east lib addresses have already been geocoded, geocode everything else
to_geocode<-dat_cleaned5%>%
  #create the for_geocode_addr column
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  #filter out previously geocoded addresses
  filter(!for_geocode_addr%in%potential_east_lib_addresses$for_geocode_addr)%>%
  # limit to just unique addresses
  select(for_geocode_addr)%>%
  unique%>%
  #create a column to be iterated through for geocoding
  mutate(geo_batch=round(row_number()/100))

write_csv(to_geocode,"remaining addresses to geocode.csv")

#iterate through geo batch numbers, bind results at each batch (that way, if it breaks partway through,
# we at least have some geocoded addresses)
for(i in 1:max(to_geocode$geo_batch)){
  temp<-mutate_geocode(to_geocode%>%
                         filter(geo_batch==i),
                       for_geocode_addr,
                       override_limit=TRUE)
  if(i==1){
    geocode_results=temp
  }else{
    geocode_results=rbind(geocode_results,temp)
  }
}


#save geocode results 
save(geocode_results,file="data preparation/geocode addresses/geocoded remaining addresses.Rdata")

#combine previously geocoded with geocode results
all_geocoded_addr<-rbind(geocode_results,previously_geocoded)


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



  
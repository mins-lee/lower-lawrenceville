rm(list=ls())
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggmap)
library(rgdal)
library(tidycensus)
library(openxlsx)
options(scipen = 999)

#read in the bnp projects
bnps<-read.xlsx("Quantitative Analysis/Redevelopmentprojects_3.24.20.xlsx",sheet="Combined wgeocode")%>%
  #filter out missing xs
  filter(!is.na(X))%>%
  select(`Development.Projects`,`Date-Started`,`Date-Completed`,`Development.Cost`,
         `Address`,`Zip`,`Developers/.Real.Estate.Owners`,
         `Type`)%>%
  mutate(for_geocode_addr=paste(Address,Zip,sep=", "))

#load api key
load("data preparation/geocode addresses/api_key.RData")

register_google(api_key)
#geocode addresses based on city and zip
bnps_recoded<-bnps%>%
  mutate_geocode(for_geocode_addr)

bnps_recoded<-bnps_recoded%>%
  st_as_sf(coords=c("lon","lat"),crs=4326)

#save bnps recoded to quantitative analysis folder
save(bnps_recoded,file="quantitative analysis/bnps re-geocoded.Rdata")

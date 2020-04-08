rm(list=ls())
library(dplyr)
library(tidyr)
library(sf)
library(tidycensus)
load("cleaned and geocoded data 01-April.Rdata")

#pull geoids from tidycensus, tiger would be more efficient but this the quickest way i know how

geoids<-get_acs(year=2018,variables="B01001_001",state="PA",county="Allegheny",geography="tract",geometry=TRUE)%>%
  select(GEOID)%>%
  #change crs to match geocoded data
  st_transform(crs="+init=epsg:4326")

with_geocode<-cleaned_and_geocoded%>%
  st_join(geoids,st_within)

save(with_geocode,file="voucher data with geoid field.Rdata")
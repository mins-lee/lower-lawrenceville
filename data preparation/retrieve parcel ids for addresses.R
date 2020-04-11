rm(list=ls())
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)

#read in voucher resident data
load("cleaned and geocoded data 01-April.Rdata")

#read in parcel data
parcel_data<-read_sf("quantitative analysis/Allegheny_County_Parcel_Boundaries/Allegheny_County_Parcel_Boundaries.shx")%>%
  st_transform(crs="+init=epsg:4326")

#join voucher data with parcel data
with_parcel<-st_join(cleaned_and_geocoded,
                     parcel_data,left=TRUE)

#find the parcel ID associated with penn plaza
penn_plaza<-with_parcel%>%
  filter(grepl("5704 Penn ave|5600 Penn ave",PRIMARYSTREET,ignore.case=TRUE))%>%
  select(PRIMARYSTREET,PIN)%>%
  unique()

#find parcel associated with euclid
euclid<-with_parcel%>%
  filter(grepl("euclid",PRIMARYSTREET,ignore.case=TRUE))%>%
  select(PRIMARYSTREET,PIN)%>%
  unique()
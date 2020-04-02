#merge geocoded data with with cleaned addresses

library(dplyr)
library(sf)
#read in cleaned voucher data
load("data preparation/all data clean 29-feb-2020.Rdata")
#read in geocoded addresses
load("data preparation/geocode addresses/all recipient addresses geocoded.Rdata")
#read in neighborhood boundaries
#load pittsburgh neighborhood shapefile
neighborhoods<-read_sf("data preparation/geocode addresses/Neighborhoods_/Neighborhoods_.shx")%>%
  select(hood,geometry)

cleaned_and_geocoded<-
  #start with the cleaned voucher data
  dat_cleaned5%>%
  #join in geocode information for each address
  mutate(for_geocode_addr=paste(PRIMARYSTREET,ZIP,sep=", "))%>%
  left_join(correctly_coded%>%
              select(for_geocode_addr)%>%
              unique())%>%
  st_as_sf()%>%
  #spatial join neighborhood data
  st_join(neighborhoods,st_within,left=TRUE)

save(cleaned_and_geocoded,file="cleaned and geocoded data 01-April.Rdata")

# test<-cleaned_and_geocoded%>%
#   filter(!is.na(geometry))


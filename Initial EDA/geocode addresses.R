library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggmap)
library(rgdal)
library(tidycensus)
options(scipen = 999)
#load dataset
load("cleaned data.RData")
#load google api key
load("api_key.RData")

register_google(key=api_key,account_type="standard")

#create a list of addresses to be geocoded out of the cleaned data
#first, identify set of rows containing all of east liberty
#### 1.) Zip codes 15206
#### 2.) Addresses in the 300 block of S negley

#find individuals who live in 15206 at any point

zip_15206_clients<-unique(dat_cleaned3[dat_cleaned3$ZIP==15206,]$CLIENT_ID)

s_negley_300_clients<-dat_cleaned3[grep("3[0-9][0-9] S Negley",dat_cleaned3$PRIMARYSTREET,ignore.case=TRUE),]$CLIENT_ID

potential_east_lib_clients<-c(zip_15206_clients, s_negley_300_clients)

#find all addresses associated with those individuals
potential_east_lib_addresses<-dat_cleaned3%>%
  # limit just to clients who may have lived in east liberty at one point (from previous step)
  filter(CLIENT_ID %in% potential_east_lib_clients)%>%
  # change addresses to geocode ready format
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  # limit to just unique addresses
  select(for_geocode_addr)%>%
  unique%>%
  #create a column to be iterated through for geocoding
  mutate(geo_batch=round(row_number()/100))

write_csv(potential_east_lib_addresses,"addr for potential east lib res.csv")



#iterate through geo batch numbers, bind results at each batch (that way, if it breaks partway through,
# we at least have some geocoded addresses)

for(i in 1:44){
  temp<-mutate_geocode(potential_east_lib_addresses%>%
                         filter(geo_batch==i),
                       for_geocode_addr,
                       override_limit=TRUE)
  if(i==1){
    geocode_results=temp
  }else{
    geocode_results=rbind(geocode_results,temp)
  }
}

#download Allegheny county boundaries from ACS to identify addresses
alle_co_boundaries<-get_acs(year=2017,variables="B01001_001",state="PA",county="Allegheny",
                            geography="county",
                            geometry=TRUE)

#identify addresses in allegheny county, assume they're correctly geocoded
correctly_coded<-geocode_results%>%
  st_as_sf(coords=c("lon","lat"),crs = "+init=epsg:4326")%>%
  st_join(alle_co_boundaries%>%
            #change crs for census to match Google's
            st_transform(crs="+init=epsg:4326"),
          join=st_within,left=FALSE)

#subtract out correctly coded to identify incorrectly coded
incorrectly_coded<-geocode_results%>%
  filter(!for_geocode_addr%in%correctly_coded$for_geocode_addr)

#save geocoded addresses
save(geocode_results,file="geocoded addresses all 15206 individuals.Rdata")

#load pittsburgh neighborhood shapefile
neighborhoods<-read_sf("Neighborhoods_/Neighborhoods_.shx")


#limit neighborhoods to just east liberty
east_liberty_poly<-neighborhoods%>%
  filter(grepl("east liberty",hood,ignore.case=TRUE))

#use east lib poly to limit geocoded addresses to just east liberty
east_lib_addresses<-dat_cleaned%>%
  #create merge field to bring in geocoded addresess
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  #bring in geocoded addresses
  inner_join(geocode_results,by="for_geocode_addr")%>%
  #make spatial
  st_as_sf(coords=c("lon","lat"),crs = "+init=epsg:4326")%>%
  #inner join with east liberty poly to limit to east lib
  st_join(east_liberty_poly,join=st_within,left=FALSE)

#retrieve list of clients who lived in east liberty at some point
east_lib_clients<-east_lib_addresses$CLIENT_ID%>%unique()

#limit the correctly coded addresses down to those who lived in east liberty at some point
east_lib_any<-dat_cleaned%>%
  #limit to those who lived in east liberty at some point
  filter(CLIENT_ID%in%east_lib_clients)%>%
  #limit to just the correctly coded addresses
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  inner_join(correctly_coded,by="for_geocode_addr")%>%
  #add a marker for if the address is specifically in east liberty
  mutate(east_lib_address=ifelse(for_geocode_addr%in%east_lib_addresses$for_geocode_addr,
                                 TRUE,FALSE))%>%
  select(HA, CLIENT_ID, GENDER, RACE, MOVEINDATE, MOVEOUTDATE, PRIMARYSTREET, SECONDARYSTREET,
         CITY, ZIP,east_lib_address,geometry)%>%
  st_as_sf()

write_csv(east_lib_any%>%
            mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
            inner_join(geocode_results),
          "east_lib_res_addr_lat_lon.csv")

st_write(east_lib_any,"east_lib_res_addr.gpkg")
st_write(east_lib_any,"east_lib_res_addr.csv",layer_options = "GEOMETRY=AS_XY")
test_read<-st_read("addresses associated with east lib res.gpkg")
names(east_lib_any)

#save geocoded east lib any as rData

save(east_lib_any,file="geocoded east lib res addresses.Rdata")



test<-dat_cleaned%>%filter(grepl("highland",PRIMARYSTREET,ignore.case=TRUE))



#test neighborhoods
leaflet()%>%
  addProviderTiles(provider = "CartoDB.Positron", group = "Positron")%>%
  addPolygons(data=alle_co_boundaries)%>%
  # addPolygons(data=neighborhoods%>%
  #               filter(grepl("east liberty",hood,ignore.case=TRUE)),stroke=FALSE)%>%
  addCircleMarkers(data=east_lib_any,
                   radius=2,stroke=FALSE)

class(east_lib_any)
#test geocoding a few addresses
test_geo<-mutate_geocode(potential_east_lib_addresses[1:10,],for_geocode_addr,
                         override_limit=TRUE)

test_geo2<-dat_cleaned%>%
  mutate(for_geocode_addr = paste(PRIMARYSTREET,ZIP,sep = ", "))%>%
  inner_join(test_geo)%>%
  st_as_sf(coords=c("lon","lat"),crs = "+init=epsg:4326")
  
leaflet()%>%
  addProviderTiles(provider = "CartoDB.Positron", group = "Positron")%>%
  addCircleMarkers(data=test_geo2)
st_write(test_geo2,"test_write_shape.gpkg")


test_read<-st_read("test_write_shape.gpkg")

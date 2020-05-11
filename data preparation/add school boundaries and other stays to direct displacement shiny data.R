rm(list=ls())
library(sf)
library(dplyr)
library(leaflet)


#load existing direct displacement files
load("visualize_direct_displacements/files for direct displacement viz.Rdata")

#load all the voucher data
load("cleaned and geocoded data 01-April.Rdata")
#create a dataset of all other addresses of residents for each parcels
for(i in 1:nrow(bnp_parcels)){
  # iterate through each parcel
  temp_parcel<-bnp_parcels[i,]%>%
    #change projection to a different crs joins will be more accurate
    st_transform("+init=epsg:2163")
  
  # identify clients who lived on that parcel
  parcel_clients <- cleaned_and_geocoded%>%
    #change projection to a different crs joins will be more accurate
    st_transform("+init=epsg:2163")%>%
    st_join(temp_parcel,st_within,left=FALSE)%>%
    as.data.frame()%>%
    select(CLIENT_ID)
  
  # create a dataframe of places those clients stayed that aren't on the parcel
  temp_stays<-cleaned_and_geocoded%>%
    #change projection to a different crs joins will be more accurate
    st_transform("+init=epsg:2163")%>%
    filter(CLIENT_ID%in%parcel_clients$CLIENT_ID)%>%
    # eliminate rows for same client
    st_difference(temp_parcel)%>%
    # add column indicating parcel
    mutate(Development.Projects = temp_parcel$Development.Projects)
  
  #bind results
  if(i == 1){
    other_stays<-temp_stays%>%
      #change projection back to 4326 for mapping
      st_transform("+init=epsg:4326")
  }else{
    other_stays<-rbind(other_stays,temp_stays%>%
                         #change projection back to 4326 for mapping
                         st_transform("+init=epsg:4326"))
  }
  
}



#read in existing displacement data
school_boundaries<-read_sf("visualize_direct_displacements/Allegheny_County_School_District_Boundaries.geojson")%>%
  st_transform(crs="+init=epsg:4326")

#read in neighborhoods
hoods<-st_read("data preparation/geocode addresses/Neighborhoods_/Neighborhoods_.shx")

save(bnp_parcels,parcel_resident_panel,school_boundaries,other_stays,hoods,
     file = "visualize_direct_displacements/files for direct displacement viz.Rdata")

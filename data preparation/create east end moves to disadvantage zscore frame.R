
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggmap)
library(rgdal)
library(tidycensus)
library(ggplot2)
library(tidyr)
library(scales)
library(units)

#read in all geocoded data
load("voucher data with geoid field.Rdata")
#read in disadvantage indices data
load("data retrieval/index files/indices.Rdata")
#grab indices as of 2018
indices_2018<-indices_list[["2018"]]%>%
  mutate()

#limit dataset to just neighborhoods that will be  included in event study
#set neighborhoods to include
included_neighborhoods<-c("East Liberty", "Homewood","Homewood West", "Homewood South","Homewood North",
                          "Larimer","Point Breeze","Point Breeze North", "East Hills", "Swisshelm Park",
                          "Shadyside", "Squirrel Hill North", "Squirrel Hill South", "Garfield", "Bloomfield",
                          "Highland Park", "Morningside",
                          "Greenfield", "Hazelwood","Glen Hazel", "Lincoln-Lemington-Belmar",
                          "Lower Lawrenceville", "Upper Lawrenceville", "Central Lawrenceville",
                          "Morningside", "Stanton Heights")
included_clients<-with_geocode%>%
  filter(hood%in%included_neighborhoods)%>%
  select(CLIENT_ID)

with_geocode2<-with_geocode%>%
  filter(CLIENT_ID %in% included_clients$CLIENT_ID)

#create one column for just index
just_index=indices_2018%>%
  rowwise()%>%
  mutate(z_disadvantage = mean(z_pov,z_single_fem, z_male_unemp,z_bachelors,na.rm=TRUE))%>%
  ungroup()%>%
  select(GEOID, z_disadvantage)
#create a function that takes a client as an argument, and returns a dataframe of moves
make_moves<-function(client){
  # create a blank result frame to store results in
  # pull from east_lib_any2 to get the types right
  result_frame=data.frame(client_id=with_geocode2[1,]$CLIENT_ID,
                          race = with_geocode2[1,]$RACE,
                          start_street= with_geocode2[1,]$PRIMARYSTREET,
                          start_zip= with_geocode2[1,]$ZIP,
                          start_geo=with_geocode2[1,]$geometry,
                          moveout_date=with_geocode2[1,]$MOVEOUTDATE,
                          start_geoid=with_geocode2[1,]$GEOID,
                          end_geoid=with_geocode2[1,]$GEOID,
                          start_hood=with_geocode2[1,]$hood,
                          start_disadvantage=just_index[1,]$z_disadvantage,
                          end_disadvantage=just_index[1,]$z_disadvantage)[0,]
  # limit dataset to just given client
  client_frame<-with_geocode2%>%
    filter(CLIENT_ID==client)%>%
    #sort by moveindate
    arrange(MOVEINDATE)

  #find race (just grab first instance of race, though it shouldn't matter)
  race = client_frame$RACE[[1]]
  if(nrow(client_frame)>1){
    #go through client_frame, grab needed info
    for(i in 1:(nrow(client_frame)-1)){
      #retrieve needed columns from client_frame
      start_street = client_frame[i,]$PRIMARYSTREET
      start_zip = client_frame[i,]$ZIP
      start_geo=client_frame[i,]$geometry
      moveout_date=client_frame[i,]$MOVEOUTDATE
      start_geoid = client_frame[i,]$GEOID
      end_geoid = client_frame[i+1,]$GEOID
      start_hood = client_frame[i,]$hood
      
      #store results in frame
      result_row=data.frame(client_id=client,
                            race = race,
                            start_street = start_street,
                            start_zip = start_zip,
                            start_geo = start_geo,
                            moveout_date=moveout_date,
                            start_geoid = start_geoid,
                            end_geoid = end_geoid,
                            start_hood = start_hood)%>%
        #join in disadvantage index for start geo
        left_join(just_index,
                  by=c("start_geoid"="GEOID"))%>%
        #rename the disadvantage index column
        rename("start_disadvantage" = "z_disadvantage")%>%
        #join in disadvantage index for end geography
        left_join(just_index,
                  by = c("end_geoid" = "GEOID"))%>%
        #rename the disadvantage index column
        rename("end_disadvantage" = "z_disadvantage")
      
      #add result row to result frame
      #only do so if primary address is different for start and end
      if(client_frame[i,]$PRIMARYSTREET!=client_frame[i+1,]$PRIMARYSTREET){
        result_frame = rbind(result_frame,result_row)
      }
      
      
    }
    
  }
  result_frame
  
}
clients<-unique(with_geocode2$CLIENT_ID)
#apply function over clients
move_list<-lapply(clients,make_moves)

moves<-do.call(rbind,move_list)

#save data set to quantitative analysis folder since that's where the event study will look for it
save(moves,file="quantitative analysis/east end moves zscores.Rdata")


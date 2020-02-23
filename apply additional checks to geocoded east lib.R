#### update the geocoded addresses with the additional checks added after initial geocoding
## 1.) delete rows that are identical except for one having a null moveoutdate
## 2.) if moveout is null and there is a later record for same individual, impute moveout date as subsequent move in

library(dplyr)
library(sf)
library(readr)
library(ggmap)
library(rgdal)
library(tidycensus)

setwd("initial eda")

#load east lib any
load("geocoded east lib res addresses.Rdata")

#accidentally overwrote old geocoded addresses, so load from gpkg file
east_lib_any<-st_read("east_lib_res_addr.gpkg")%>%
  #change moveout dates to dates
  mutate(MOVEINDATE=paste0(MOVEINDATE),
         MOVEOUTDATE=paste0(MOVEOUTDATE))%>%
  mutate(MOVEINDATE=as.Date(MOVEINDATE,format="%h/%d/%Y"),
         MOVEOUTDATE=as.Date(MOVEOUTDATE,format="%h/%d/%Y"))
#################################################### REMOVE ADDRESSES WITH DUPLICATES EXCEPT NULL MOVEOUT###############
##  Several rows are exactly identical except one will have a move-out date and one will be null
##  Steps:
##  1.) create separate dataset, "no_null_moveouts" that just contains client, movein, moveout, address, with moveout removed
##  2.) remove move_out from cleaned_data frame, delete non-unique rows
##  3.) left join cleaned_data with "no_null_moveouts"

no_null_moveouts<-east_lib_any%>%
  as.data.frame()%>%
  select(CLIENT_ID,MOVEINDATE,MOVEOUTDATE,PRIMARYSTREET)%>%
  #delete null moveouts
  filter(!is.na(MOVEOUTDATE))

east_lib_any2<-east_lib_any%>%
  as.data.frame()%>%
  #remove moveoutdate column
  select(-MOVEOUTDATE)%>%
  unique()%>%
  left_join(no_null_moveouts,by=c("CLIENT_ID","MOVEINDATE","PRIMARYSTREET"),all.x=TRUE)%>%
  st_as_sf()
############################################## Impute subsequent move-in dates as move-out dates ######
## For some records, a move-out date was not recorded even though a subsequent address / move-in date exists
## Goal: for each client, replace null moveoutdates with the next moveindate
## Steps:
## 1.) Iterate through clients, create subset of frame for given client
## 2.) iterate through 1: number of rows-1. Create row for first_address and second_address
## 3.) if moveoutdate is null for first_address, replace it with moveindate from second address
#create a unique identifier in dat_cleaned, that will be used to overwrite records
east_lib_any3<-east_lib_any2%>%
  as.data.frame()%>%
  mutate(ROW_ID=row_number())

#create counter for number of changes
changes<-0
for(client in east_lib_any3$CLIENT_ID){
  client_subset<-east_lib_any3%>%
    filter(CLIENT_ID==client)%>%
    arrange(MOVEINDATE)
  #only apply the for loop below if there's more than one row in client_subset
  if(nrow(client_subset)>1){
    #iterate through rows, up until one before last one
    for(i in 1:(nrow(client_subset)-1)){
      first_address<-client_subset[i,]
      next_address<-client_subset[i+1,]
      if(is.na(first_address$MOVEOUTDATE)){
        #increment change counter up
        changes<-changes+1
        #replace missing moveout date with new moveindate
        first_address$MOVEOUTDATE<-next_address$MOVEINDATE
        #update row of east_lib_any3
        east_lib_any3[first_address$ROW_ID,]<-first_address
        
      }
    }
    
  }
  
}

east_lib_any4<-east_lib_any3%>%
  st_as_sf()

#save rdata file
save(east_lib_any4,file="geocoded east lib res addresses imputed moveouts.Rdata")

#export as x/y coordinates
st_write(east_lib_any4,"east_lib_res_addr.csv",layer_options = "GEOMETRY=AS_XY")


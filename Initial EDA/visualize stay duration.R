#initial vizualizations

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

#load the east lib address dataset
load("initial eda/geocoded east lib res addresses imputed moveouts.Rdata")
load("initial eda/cleaned data.RData")
#add a column for duration of stay

east_lib_any5<-east_lib_any4%>%
  #replace null move-out dates with Jan 23, the day Nick gave us the data
  mutate(MOVEOUTDATE = if_else(is.na(MOVEOUTDATE),
                              as.Date("Jan/23/2020",format="%h/%d/%Y"),
                              MOVEOUTDATE))%>%
  # #convert the dates from character
  # mutate(MOVEINDATE=as.Date(MOVEINDATE,format="%h/%d/%Y"),
  #        MOVEOUTDATE=as.Date(MOVEOUTDATE,format="%h/%d/%Y"))%>%
  #if the move in date is less than jan 1 1970, make it jan 1 1970
  mutate(MOVEINDATE = dplyr::if_else(MOVEINDATE<as.Date("Jan/01/1970",format="%h/%d/%Y"),
                             as.Date("Jan/01/1970",format="%h/%d/%Y"),
                             MOVEINDATE)%>%as.Date(origin=),
         test=as.Date("Jan/01/1970",format="%h/%d/%Y"))%>%
  #calculate duration of stay
  mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE))


for_median<-east_lib_any5%>%
  mutate(` `=ifelse(!east_lib_address,"Outside East Liberty","East Liberty Address")%>%
           factor(levels=c("Outside East Liberty","East Liberty Address")))%>%
  mutate(`Length of Stay (years)` = stay_duration/365)%>%
  #limit only to move-ins after 1995
  filter(format(MOVEINDATE,"%Y")>=1995)

median_duration<-median(for_median$stay_duration)/365
# mean(for_median$stay_duration)/365


#find the geometric distribution of stay durations
east_lib_any5%>%
  mutate(` `=ifelse(!east_lib_address,"Outside East Liberty","East Liberty Address")%>%
           factor(levels=c("Outside East Liberty","East Liberty Address")))%>%
  mutate(`Length of Stay (years)` = stay_duration/365)%>%
  #limit only to move-ins after 1995
  filter(format(MOVEINDATE,"%Y")>=1995)%>%
  
  #filter(`Length of Stay (years)`<=15)%>%
  ggplot()+
  geom_density(aes(x=`Length of Stay (years)`,fill=` `),alpha=.5)+
  geom_vline(xintercept=median_duration)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title = "Distribution of Address Durations")


#for each year, find the number of people moving in and out of east liberty
#create list of years
years=c(format(east_lib_any5$MOVEINDATE,"%Y"),
        format(east_lib_any5$MOVEOUTDATE,"%Y"))%>%unique()

#create a frame that will count the number of people moving in and out of east lib by year
east_lib_movements = data.frame(year = years,
                                total = 0,
                                move_ins = 0,
                                move_outs = 0)%>%
  arrange(year)

#iterate through clients, for each client check for each year if they moved into east lib or out of east lib in that 
  #year. If they did, up the counter by one for the given year
# year<-2009
# client<-2435849681
for(client in east_lib_any5$CLIENT_ID%>%unique){
  #limit data to just given client
  client_frame<-east_lib_any5%>%filter(CLIENT_ID==client)%>%
    arrange(MOVEINDATE)%>%
    mutate(move_in_year = format(MOVEINDATE,"%Y")%>%as.numeric,
           move_out_year = format(MOVEOUTDATE,"%Y")%>%as.numeric)
  
  #set a marker for whether or not the "previous" address was in east liberty based on first address
  previous_address_east_lib=client_frame[1,]$east_lib_address
  #set range of years to iterate through
  min_year = min(client_frame$move_in_year,na.rm=TRUE)
  max_year = max(client_frame$move_out_year,na.rm=TRUE)
  #iterate through years
  for(year in min_year:max_year){
    #create a subset of the client's frame for the given year
    year_subset = client_frame%>%
      filter(year>=move_in_year & year<=move_out_year)
    #add 1 to "total" in east_lib_movements if they have any addresses in east liberty
    if(any(year_subset$east_lib_address)){
      east_lib_movements[east_lib_movements$year==year,]$total=
        east_lib_movements[east_lib_movements$year==year,]$total+1}
    if(nrow(year_subset)>0){
      for(row_num in 1:nrow(year_subset)){
        #add 1 to "move_in" in east_lib_movements if: 
        #  1.) the previous_address flag is FALSE
        #  2.) the given rows east_lib_addr flag is TRUE
        #  3.) the given move_in_year == year
        if(previous_address_east_lib==FALSE &
           year_subset[row_num,]$east_lib_address==TRUE &
           year_subset[row_num,]$move_in_year==year){
          
          east_lib_movements[east_lib_movements$year==year,]$move_ins=
            east_lib_movements[east_lib_movements$year==year,]$move_ins+1
        }
        #add 1 to "move_out" in east_lib_movements if:
        # 1.) the previous address flag is TRUE
        # 2.) the given rows east lib address flag is FALSE
        # 3.) the given rows move_in_year==year
        #intuition: observing move-outs does not tell us where their new address is, but the previous address
        #           flag tells us if they were in east liberty, so moving in to a non-east lib address is a move out
        if(previous_address_east_lib==TRUE &
           year_subset[row_num,]$east_lib_address==FALSE &
           year_subset[row_num,]$move_in_year==year){
          
          east_lib_movements[east_lib_movements$year==year,]$move_outs=
            east_lib_movements[east_lib_movements$year==year,]$move_outs+1
          
        }
        # change the previous address flag to the current rows address
        previous_address_east_lib <- year_subset[row_num,]$east_lib_address
      }
      
    }
    #iterate throw the rows in year_subset

      
  }
  
}
library(tidyr)
library(scales)
#create a bar chart showing movement in / out of east liberty by year
east_lib_movements%>%
  #make moves out negative
  mutate(move_outs = (-1)*move_outs)%>%
  #make percentage columns
  mutate(`Moving In` = move_ins/total,
         `Moving Out` = move_outs/total)%>%
  select(year,`Moving In`,`Moving Out`)%>%
  #only keep years starting 2003
  filter(as.numeric(paste0(year))>=2003 &
           as.numeric(paste0(year))<2020)%>%
  gather(key = ` `, value = value, -year)%>%
  #reverse the levels of key to swap colors
  mutate(` ` = factor(` `,levels=c("Moving Out","Moving In")))%>%
  ggplot(aes(x=year,y=value,fill=` `))+
  geom_bar(stat="identity",position="stack",width=.75)+
  coord_flip()+
  labs(title="Movement to/from East Liberty",
       y = "Percentage of Year's East Liberty Voucher Households",
       x = "Year")+
  scale_y_continuous(label=percent,limits=c(-.17,.17),breaks=c(-.15,-.1,-.05,0,.05,.1,.15))+
  #scale_fill_brewer(palette="Dark2")+
  theme()

east_lib_movements%>%
  #make moves out negative
  mutate(move_outs = (-1)*move_outs)%>%
  #make percentage columns
  mutate(`Moving In` = move_ins/total,
         `Moving Out` = move_outs/total)%>%
  select(year,`Moving In`,`Moving Out`)%>%
  #only keep years starting 2003
  filter(as.numeric(paste0(year))>=2003 &
           as.numeric(paste0(year))<2020)%>%
  gather(key = ` `, value = value, -year)%>%
  #reverse the levels of key to swap colors
  mutate(` ` = factor(` `,levels=c("Moving Out","Moving In")))%>%
  ggplot(aes(x=year,y=value,fill=` `))+
  geom_bar(stat="identity",position="stack",width=.7,alpha=.8)+
  coord_flip()+
  labs(title="Movement to/from East Liberty",
       y = "Percentage of Year's East Liberty Voucher Households",
       x = "Year")+
  #scale_fill_brewer(palette="Dark2")+
  theme(legend.position="bottom")






######################################################## TEST CODE BELOW HERE ##############################################

# test<-east_lib_movements%>%
#   mutate(change = move_ins - move_outs)%>%
#   filter(as.numeric(paste0(year))>=2003)
# 
# test<-east_lib_any%>%
#   filter(is.na(MOVEOUTDATE))
# 
test2<-east_lib_any2%>%
  mutate(move_in_year = format(MOVEINDATE,"%Y")%>%as.numeric,
         move_out_year = format(MOVEOUTDATE,"%Y")%>%as.numeric)%>%
  select(-test)%>%
  filter(2008>=move_in_year & 2008<=move_out_year)%>%
  filter(east_lib_address==TRUE)
# 
# test3<-east_lib_any2%>%
#   mutate(move_in_year = format(MOVEINDATE,"%Y")%>%as.numeric,
#          move_out_year = format(MOVEOUTDATE,"%Y")%>%as.numeric)%>%
#   select(-test)%>%
#   filter(2020>=move_in_year & 2020<=move_out_year)%>%
#   filter(east_lib_address==TRUE)
# 
# length(unique(test2$CLIENT_ID))

length(dat_cleaned2[is.na(dat_cleaned2$MOVEOUTDATE),]$CLIENT_ID)
length(dat_cleaned2[format(dat_cleaned2$MOVEINDATE,"%Y")<=1994,]$CLIENT_ID)

## create a plot comparing the distributions for a.) dropping moveout nulls and b.) replace with jan 23 2020
dat_cleaned3%>%
  mutate(MOVEOUTDATE = if_else(is.na(MOVEOUTDATE),
                              as.Date("Jan/23/2020",format="%h/%d/%Y"),
                              MOVEOUTDATE))%>%
  #create a marker for if nulls were dropped
  mutate(null_moveout = "impute Jan/23/2020")%>%
  rbind(dat_cleaned3%>%
          filter(!is.na(MOVEOUTDATE))%>%
          mutate(null_moveout = "drop null moveouts"))%>%
  mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE))%>%
  mutate(`Length of Stay (years)` = stay_duration/365)%>%
  #limit only to move-ins after 1995
  filter(format(MOVEINDATE,"%Y")>=1995)%>%
  #filter(`Length of Stay (years)`<=15)%>%
  ggplot()+
  geom_density(aes(x=`Length of Stay (years)`,fill=null_moveout),alpha=.35)+
  theme(legend.position="bottom")+
  labs(title = "Impact of strategy for null move-outs\non distribution of stay durations")+
  scale_fill_brewer(palette="Dark2")

## compare averaging at the stay level to averaging at the individual level
rbind(dat_cleaned3%>%
        filter(!is.na(MOVEOUTDATE))%>%
        filter(format(MOVEINDATE,"%Y")>=1995)%>%
        mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE)/365)%>%
        group_by(CLIENT_ID)%>%
        summarise(stays=length(MOVEINDATE),stay_duration=mean(stay_duration))%>%
        ungroup()%>%
        select(stay_duration)%>%
        mutate(method="individual level average"),
      dat_cleaned3%>%
        filter(!is.na(MOVEOUTDATE))%>%
        filter(format(MOVEINDATE,"%Y")>=1995)%>%
        mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE)/365)%>%
        select(stay_duration)%>%
        mutate(method="address level average"))%>%
  filter(stay_duration>0)%>%
  ggplot()+
  geom_density(aes(x=stay_duration, fill = method),alpha=.35)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title = "Distribution of Address Durations",
       x = "Length of Stay (years)")+
  scale_fill_viridis_d()
  

# create function that returns a 4 column, 1 row dataframe for a given client id:
 #col 1: client_id
 #col 2: first east lib address
 #col 3: first post east lib address (or null)
 #col4:  first post col 3 east lib address
returners<-function(client_id){
  client_frame<-east_lib_any2%>%
    filter(CLIENT_ID==client_id)%>%
    #create row index
    mutate(id=row_number())
  #grab index of first east liberty address (should never return NA for this subset)
  first_east_lib<-min(client_frame[client_frame$east_lib_address==TRUE,]$id)
  #store result
  col2<-client_frame[first_east_lib,]$geometry
  #subset to only rows after first_east_lib, east_lib_address==FALSE
  post_east_lib<-client_frame%>%
    filter(id>first_east_lib)%>%
    filter(east_lib_address==FALSE)
  if(nrow(post_east_lib)>=1){
    first_post_east_lib<-min(post_east_lib$id)
    #store result
    col3<-client_frame[first_post_east_lib,]$geometry
    # limit client frame to only east lib from after the first post_east_lib
    back_to_east_lib<-client_frame%>%
      filter(id>first_post_east_lib)%>%
      filter(east_lib_address==TRUE)
    #check to make sure rows are returned
    if(nrow(back_to_east_lib)>=1){
      east_lib_return_id<-min(back_to_east_lib$id)
      #store result
      col4<-client_frame[east_lib_return_id,]$geometry
    }else{
      col4<-NA
    }
  }else{
    col3<-NA
    col4<-NA
  }
  return(data.frame(CLIENT_ID = client_id,
                    geometry = col2,
                    geometry.1 = col3,
                    geometry.2 = col4))
}
#apply returners function over list
clients<-unique(east_lib_any2$CLIENT_ID)
return_list<-lapply(clients,function(x) returners(x))
east_lib_returns<-do.call(bind_rows,return_list)
#replace nulls with NAs for filtering
for(i in 1:nrow(east_lib_returns)){
  if(is.null(east_lib_returns[i,]$geometry.1[[1]])){
    east_lib_returns[i,]$geometry.1<-NA
  }
  if(is.null(east_lib_returns[i,]$geometry.2[[1]])){
    east_lib_returns[i,]$geometry.2<-NA
  }
}

#replace NULL with na
east_lib_returns[is.null(east_lib_returns$geometry.1[[1]]),]$geometry.1<-NA
#store null geo object for testing
test<-east_lib_returns[17,]
is.null(test$geometry.1[[1]])
is.null(test$geometry.2[[1]])
# reshape east_lib_returns for mapping
east_lib_returns_reshape<-rbind(
  #first group: left and returned, first east lib address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
    mutate(geometry.4 = geometry,
           group = "Returners: 1st address")%>%
    select(group,geometry.4),
  #second group: left and returned, outside east lib address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
    mutate(geometry.4 = geometry.1,
           group = "Returners: 2nd address")%>%
    select(group,geometry.4),
  #third group: left and returned, return address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
    mutate(geometry.4 = geometry.2,
           group = "Returners: Return Address")%>%
    select(group,geometry.4),
  #fourth group: left, never returned, 1st address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & is.na(geometry.2))%>%
    mutate(geometry.4 = geometry,
           group = "Never Returners: First Address")%>%
    select(group,geometry.4),
  #fifth group: left, never returned, 2nd address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & is.na(geometry.2))%>%
    mutate(geometry.4 = geometry,
           group = "Never Returners: 2nd Address")%>%
    select(group,geometry.4)
)%>%rename(geometry=geometry.4)


east_lib_returns_reshape<-rbind(
  #first group: left and returned, first east lib address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
    mutate(geometry.4 = geometry,
           group = "Orig. East Lib Address")%>%
    select(group,geometry.4),
  # #second group: left and returned, outside east lib address
  # east_lib_returns%>%
  #   filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
  #   mutate(geometry.4 = geometry.1,
  #          group = "Outside East Lib")%>%
  #   select(group,geometry.4),
  #third group: left and returned, return address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & !is.na(geometry.2))%>%
    mutate(geometry.4 = geometry.2,
           group = "Return to East Lib Address")%>%
    select(group,geometry.4),
  # #fourth group: left, never returned, 1st address
  east_lib_returns%>%
    filter(!is.na(geometry.1) & is.na(geometry.2))%>%
    mutate(geometry.4 = geometry,
           group = "Never Returners: First Address")%>%
    select(group,geometry.4)#,
  #fifth group: left, never returned, 2nd address
  # east_lib_returns%>%
  #   filter(!is.na(geometry.1) & is.na(geometry.2))%>%
  #   mutate(geometry.4 = geometry,
  #          group = "Never Returners: 2nd Address")%>%
  #   select(group,geometry.4)
)%>%rename(geometry=geometry.4)
#defne palette
pal <- colorFactor(
  palette = "viridis",
  domain = east_lib_returns_reshape$group%>%unique())

leaflet()%>%
  addProviderTiles(provider = "CartoDB.Positron", group = "Positron")%>%
  addCircleMarkers(data = st_as_sf(east_lib_returns_reshape),
                   stroke=FALSE,
                   fillOpacity = .5,
                   color=~pal(group),
                   radius=8)%>%
  addLegend("bottomright", pal = pal, values = unique(east_lib_returns_reshape$group),
            title = "Residents leaving and returning",
            opacity = 1)

east_lib_any%>%
  select(-geometry)%>%
  group_by(PRIMARYSTREET)%>%
  summarise(stays = length(client_id))%>%
  ungroup()%>%
  arrange(desc(stays) )


?addCircleMarkers
library(sf)
?addCircleMarkers

test<-dat_cleaned2%>%
  filter(!is.na(MOVEOUTDATE))%>%
  filter(format(MOVEINDATE,"%Y")>=1995)%>%
  mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE)/365)%>%
  group_by(CLIENT_ID)%>%
  summarise(stay_duration=mean(stay_duration))%>%
  select(stay_duration)

median(test$stay_duration)
mean(test$stay_duration)

min_client<-east_lib_any5[east_lib_any5$stay_duration<0,]$CLIENT_ID
test<-east_lib_any5%>%
  filter(CLIENT_ID%in%min_client)

test2<-east_lib_any2%>%
  filter(CLIENT_ID=="718716662")



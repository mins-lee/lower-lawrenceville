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

#load in overall cleaned dataset
load("data preparation/all data clean 29-feb-2020.Rdata")

#load in geocoded east liberty addresses
load("data preparation/geocode addresses/geocoded east lib res addresses.Rdata")

###########################################################################################################################  
############################################ FIRST FIGURE: STAY DURATIONS IN EAST LIBERTY #################################
###########################################################################################################################  
#imputations for east_lib_any_data
east_lib_any2<-east_lib_any%>%
  #replace null move-out dates with Jan 23, the day Nick gave us the data
  mutate(MOVEOUTDATE = if_else(is.na(MOVEOUTDATE),
                               as.Date("Jan/23/2020",format="%h/%d/%Y"),
                               MOVEOUTDATE))%>%
  # #convert the dates from character
  mutate(MOVEINDATE = dplyr::if_else(MOVEINDATE<as.Date("Jan/01/1970",format="%h/%d/%Y"),
                                     as.Date("Jan/01/1970",format="%h/%d/%Y"),
                                     MOVEINDATE),
         test=as.Date("Jan/01/1970",format="%h/%d/%Y"))%>%
  #calculate duration of stay
  mutate(stay_duration=as.numeric(MOVEOUTDATE-MOVEINDATE))%>%
  # replace NA neighborhoods with "outside pittsburgh"
  replace_na(list(hood="Outside Pittsburgh"))%>%
  #create an east liberty flag
  mutate(east_lib_address=(hood=="East Liberty"))


#save the overall median duration
median_duration=median(east_lib_any2$stay_duration)/365


east_lib_any2%>%
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

###########################################################################################################################  
############################################ SECOND FIGURE: GROSS MIGRATION #################################
###########################################################################################################################  
years=c(format(east_lib_any2$MOVEINDATE,"%Y"),
        format(east_lib_any2$MOVEOUTDATE,"%Y"))%>%unique()

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
for(client in east_lib_any2$CLIENT_ID%>%unique){
  #limit data to just given client
  client_frame<-east_lib_any2%>%filter(CLIENT_ID==client)%>%
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
  scale_y_continuous(label=percent,limits=c(-.17,.17),breaks=c(-.15,-.1,-.05,0,.05,.1,.15),
                     minor_breaks=NULL)+
  #scale_fill_brewer(palette="Dark2")+
  theme_minimal()
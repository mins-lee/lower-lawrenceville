#combine BNPs and apartment data

#start with apartments
#add in: 
 # commercial BNPS
 # the following missing residential buildings
    #Rippey Street
library(dplyr)
library(sf)
library(readr)
library(readr)
market_rate<-read_csv("quantitative analysis/New Pgh Apartments - Large market-rate apts since 1999 - cleaned.csv")%>%
  #rename the units column
  mutate(units = `# mkt rate units`)%>%
  mutate(name=Notes)%>%
  select(lon,lat,units,name,Opened)%>%
  #remove NAS
  filter(!is.na(lon))%>%
  #add type label
  mutate(type="Market Rate")

mixed_income<-read_csv("quantitative analysis/New Mixed-Income Developments - Base List.csv")%>%
  # create units column
  mutate(units=`# mkt rate units`+`aff units?`)%>%
  mutate(name=`Notes / Building Name`)%>%
  select(lon,lat,units,name,Opened)%>%
  #remove NAS
  filter(!is.na(lon))%>%
  #add type label
  mutate(type="Mixed-income")

affordable<-read_csv("quantitative analysis/New Affordable Only Apts - Base List.csv")%>%
  mutate(units=`aff units?`)%>%
  mutate(name=Notes)%>%
  select(lon,lat,units,name,Opened)%>%
  #remove NAS
  filter(!is.na(lon))%>%
  #add type label
  mutate(type="Affordable Only")

apartments<-bind_rows(market_rate,mixed_income,affordable)%>%
  #make it sf object
  st_as_sf(coords=c("lon","lat"),crs = "+init=epsg:4326")%>%
  select(name,Opened,type)


load("quantitative analysis/bnps re-geocoded.Rdata")

commercial_bnps<-bnps_recoded%>%
  filter(Type=="Commercial" | grepl("Rippey",`Development.Projects`))%>%
  select(`Development.Projects`,`Date-Completed`,Type)%>%
  #make completion date numeric
  mutate(`Date-Completed`=as.numeric(`Date-Completed`))%>%
  #remove NA completion dates
  filter(!is.na(`Date-Completed`))

names(apartments)<-names(commercial_bnps)

apartments_commercial_bnps<-rbind(apartments,commercial_bnps)

#write out a summary of the apartments by type
write_csv(apartments_commercial_bnps%>%
            group_by(Type)%>%
            summarise(developments = n())%>%
            ungroup(),
          "bnp type count.csv"
            )
save(apartments_commercial_bnps,file="quantitative analysis/combined apartments commercial bnps.Rdata")

#pull median rent and recent

library(dplyr)
library(tidycensus)
?load_variables

v17<-load_variables(year=2017,dataset="acs1")

rent<-v17%>%
  mutate(label = gsub("!","",label),
         label = gsub("ESTIMATETOTAL","",label))%>%
  filter(grepl("median", label,ignore.case=TRUE))

#real quick, graphic of rental rates by tenure
rent_tenure_cols<-c("2015 or later" = "B25113_002",
                    "2010 to 2015" = "B25113_003",
                    "2000 to 2009" = "B25113_004",
                    "1990 to 1999" = "B25113_005",
                    "1980 to 1989" = "B25113_006")

#list of east lib tracts
east_lib_tracts<-c("42003111300","42003111500")
rent_by_movein<-get_acs(geography = "tract",
                        state = "PA",
                        county = "Allegheny",
                        variables = rent_tenure_cols,
                        year=2017,
                        survey="acs5")%>%
  filter(GEOID%in%east_lib_tracts)

#create bar chart for two census tracts
rent_by_movein%>%
  ggplot(aes(x=variable,y=estimate,fill=GEOID))+
  geom_bar(stat="identity",position="dodge",width=.5)+
  theme(legend.position="bottom")+
  labs(x="year",y="median rent",fill="census tract",title="median rent by census tract")

#retrieve tract data level median rent data for each year from
years<-c(2012,2014,2017)

years<-2012:2017

for(year in years){
  print(year)
  temp_data<-get_acs(geography="county",
                     state = "PA",
                     county = "Allegheny",
                     variables = c("Median year structure built" = "B25035_001"),
                     year = year,
                     survey="acs1")%>%
    filter(GEOID%in%east_lib_tracts)%>%
    mutate(low=estimate-moe,
           high=estimate+moe)%>%
    select(GEOID,estimate,low,high)
  if(year==2012){
    year_built<-temp_data
  }else{
    year_built<-rbind(year_built,temp_data)
  }
}


dat<-get_acs(geography = "tract",
             variables=c("Median Individual Income" = "B06011_001",
                         "Median Family Income" = "B19113_001"),
             year=2017,
             state="PA",
             county="Allegheny",
             geometry=FALSE)

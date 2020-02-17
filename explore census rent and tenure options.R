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
years<-c(2011,2014,2017)
for(year in years){
  temp_data<-get_acs(geography="tract",
                     state = "PA",
                     county = "Allegheny",
                     variables = c("Median Rent" = "B25113_002"),
                     year = year,
                     survey="acs3")%>%
    filter(GEOID%in%east_lib_tracts)%>%
    mutate(low=estimate-moe,
           high=estimate+moe)%>%
    select(GEOID,estimate,low,high)
  if(year==2012){
    rent_by_year<-temp_data
  }else{
    rent_by_year<-rbind(rent_by_year,temp_data)
  }
}



#pull median rent and recent

library(dplyr)
library(tidycensus)
library(openxlsx)
#create excel workbook where each sheet is a year's worth of columns
years<-2010:2018

#following process here: https://www.r-bloggers.com/easily-make-multi-tabbed-xlsx-files-with-openxlsx/
#create blank workbook
wb <- createWorkbook()

subject_1<-get_acs(geography="tract",
              table="S0101",
              year=2017,
              state="PA",
              county="Allegheny",
              output="wide")%>%
  filter(grepl("1113",GEOID,ignore.case=FALSE))
subject_vars<-load_variables(year=2017,dataset="acs5/subject",cache=FALSE)%>%
  filter(grepl("S0101|S1101|S2301",name,ignore.case=FALSE))



#iterate through years, adding the variables for each year to the sheet
for(year in years){
  #pull acs5columns for given year
  dat<-load_variables(year=year,dataset="acs5")%>%
    mutate(label=gsub("Estimate!!Total!!","",label))%>%
    mutate(label=gsub("!!","",label))
  #write columns to excel worksheet
  addWorksheet(wb,paste0(year))
  writeData(wb,paste0(year),dat)
}

saveWorkbook(wb, file = "all acs columns.xlsx", overwrite = TRUE)

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

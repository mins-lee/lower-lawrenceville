rm(list=ls())

library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(openxlsx)

#turn on option to cache tigris
options(tigris_use_cache = TRUE)
#this code references the excel file Meredith made breaking down the different columns available by year

#goal: create a separate dataframe for each year that contains all of the columns needed

years<-2010:2018

# specify the geographic format for exporting the files: 
# see "Guessing a Driver for Output" at this link for possible formats: 
  #https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
export_format<-".shp"


#create a list to store the dataframes
year_indices<-list()
#create a directory to store index files

dir.create("data retrieval/index files")

#create a named list of columns that can be pulled for all years from acs5
to_retrieve_acs5<-c(
  # population in occupied housing units
  "pop_in_units" = "B25008_001",
  # population in owner-occupied units
  "pop_in_owner_units" = "B25008_002",
  # population in renter-occupied units
  "pop_in_renter_units" = "B25008_003",
  # total population
  "total_pop" = "B01001_001",
  # non-hispanic white population
  "white_pop" = "B01001H_001",
  # block population
  "black_pop" = "B01001B_001",
  # american indian / alaska native population
  "native_pop" = "B01001C_001",
  # asian population
  "asian_pop" = "B01001D_001",
  # native hawaiian and other pacific islander pop
  "islander_pop" = "B01001E_001",
  # other race population
  "other_pop" = "B01001F_001",
  # two or more races
  "multi_race_pop" = "B01001G_001",
  # hispanic population
  "hispanic_pop" = "B01001I_001",
  # median household income (in given year's dollars)
  "median_income" = "B19013_001",
  # median age
  "median_age" = "B01002_001",
  # male population
  "male_pop" = "B01001_002",
  # female population
  "female_pop" = "B01001_026",
  # median home value
  "median_home_value" = "B25077_001"
  )

# define variables to retrieve from acs profile
to_retrieve_profile<-c(
  # percentage of families in poverty
  "family_poverty_pct" = "DP03_0119P",
  # percentage of individuals in poverty
  "all_poverty_pct" = "DP03_0128P")

# define variables to retrieve from acs studies
# ids change between 2014 and 2015, so create two separate lists
to_retrieve_study_10_14<-c(
  # total households
  "total_households" = "S1101_C01_001",
  "single_male_hh" = "S1101_C03_001",
  "single_female_hh" = "S1101_C04_001",
  #males participating in labor force (for 2013 and 2014, it specifies ages 20 to 64, unclear on 2009-2012)
  "male_employment_rate" = "S2301_C03_020",
  # population over 25 to 64
  "over_25_pop" = "S2301_C01_025",
  # population over 25 to 64 with bachelors degree
  "over_25_bachelors" = "S2301_C01_029")

# columns to retrieve for 15-18
to_retrieve_study_15_18<-c(
  # total households
  "total_households" = "S1101_C01_001",
  "single_male_hh" = "S1101_C03_001",
  "single_female_hh" = "S1101_C04_001",
  #ratio of employed males to male populations: 20-64 years old
  "male_employment_rate" = "S2301_C03_022",
  # population over 25 to 64
  "over_25_pop" = "S2301_C01_031",
  # population over 25 to 64 with bachelors degree
  "over_25_bachelors" = "S2301_C01_035")

#create a zscore function that takes a vector as an argument
zscore<-function(vector){
  mu<-mean(vector,na.rm=TRUE)
  sigma<-sd(vector,na.rm=TRUE)
  vector2<-(vector-mu)/sigma
}
#create a function that takes a year as an argument, and returns a single dataframe at the tract level with all of the colums
retrieve_census_data<-function(year){
  # set the list of study columns to retrieve based on the year
  if(year>=2015){
    to_retrieve_study<-to_retrieve_study_15_18
  }else{
    to_retrieve_study<-to_retrieve_study_10_14
  }
  
  # pull data from normal acs5
  acs5<-get_acs(year = year,
                variables = to_retrieve_acs5,
                geography = "tract",
                state = "PA",
                county = "Allegheny"#,
                #retrieve geometry for spatial mapping, only need to retrieve for one since they'll all be joined
                #geometry = TRUE
                )%>%
    select(variable, estimate, GEOID)%>%
    #use spread to make the data horizontal (each row is one census tract)
    spread(key = "variable", value= "estimate")
  
  # pull data for acs profiles
  acs_profile<-get_acs(year = year, 
                       variables = to_retrieve_profile,
                       geography = "tract",
                       state = "PA",
                       county = "Allegheny")%>%
    select(variable, estimate, GEOID)%>%
    #use spread to make the data horizontal (each row is one census tract)
    spread(key = "variable", value= "estimate")
  
  
  # pull data for acs study columns
  acs_study<-get_acs(year = year,
                     variables = to_retrieve_study,
                     geography = "tract",
                     state = "PA",
                     county = "Allegheny")%>%
    select(variable, estimate, GEOID)%>%
    #use spread to make the data horizontal (each row is one census tract)
    spread(key = "variable", value= "estimate")
  
  # # if the year is less than 2015, calculate unemployment rate
  # if(year<2015){
  #   acs_study$male_employment_rate<-acs_study$male_employed/acs_study$male_pop_20_64
  # }
  
  #pull cbsa level median income (for gentrification eligibility)
  cbsa_data<-get_acs(year=year,geography="cbsa",
                       variables = c("cbsa_income" = "B19013_001",
                                     "cbsa_home_value" = "B25077_001"))%>%
    #only limit to Pittsburgh cbsa
    filter(grepl("pittsburgh",NAME,ignore.case=TRUE))%>%
    # get rid of moe column so spread will work
    select(-moe)%>%
    #make data wide
    spread(key="variable",value="estimate")

  # join columns together
  acs_data<-acs5%>%
    full_join(acs_profile)%>%
    full_join(acs_study)%>%
    # calculate columns we need
    # percentage of households that are single male/single female
    mutate(pct_single_female_hh =single_female_hh/total_households,
           pct_single_male_hh = single_male_hh/total_households,
           pct_bachelors = over_25_bachelors/over_25_pop)%>%
    #calculate zscores for each disadvantage index input
    mutate(z_pov=zscore(all_poverty_pct/100),
           z_single_fem=zscore(pct_single_female_hh),
           z_male_unemp=zscore(1-(male_employment_rate/100)),
           z_bachelors=zscore(1-pct_bachelors))%>%
    # make data rowwise to take the horizontal mean to calculate disadvantage index
    rowwise()%>%
    #calculate disadvantage index
    mutate(disadvantage_index = mean(c(all_poverty_pct/100,
                                     pct_single_female_hh,
                                     1-(male_employment_rate/100),
                                     1-pct_bachelors),na.rm=TRUE)#,
           # #calculate z score disadvantage index
           # zscore_disadvantage = mean(z_pov,
           #                            z_single_fem,
           #                            z_male_unemp,
           #                            z_bachelors,
           #                            na.rm=TRUE)
           )%>%
    #add in cbsa level variables
    mutate(cbsa_income=cbsa_data$cbsa_income,
           cbsa_home_value=cbsa_data$cbsa_home_value)%>%
    #calculate income and home values as percentage of cbsa, to determine gentrification "elgibility"
    mutate(pct_cbsa_income = median_income/cbsa_income,
           pct_cbsa_value = median_home_value/cbsa_home_value)%>%
    #add a column indicating the year
    mutate(year=year)%>%
    #remove rowwise with ungroup
    ungroup()
  
}



# apply function to years, using lapply will return a list, where each item is the dataframe for a year
  # this is equivalent to a for loop over all years
rm(indices_list)
indices_list<-lapply(years,retrieve_census_data)
names(indices_list)<-years
#create a workbook that will be exported
wb<-createWorkbook()
#iterate through years, adding indices for given year to workbook
for(year in years){
  #create the worksheet
  addWorksheet(wb,paste0(year))
  #write data to worksheet
  writeData(wb,paste0(year),indices_list[[paste0(year)]])
}
saveWorkbook(wb,file="data retrieval/index files/indices with zscore.xlsx",overwrite=TRUE)
test<-test%>%
  mutate(test=zscore(pct_bachelors))%>%
  mutate(test2=zscore(1-(male_employment_rate/100)))
#save indices to r file
save(indices_list,file = "data retrieval/index files/indices.RData")

#bind all files to one dataset, export as shapefile
#first, create directory for shapefiles
dir.create("data retrieval/index files/shapefile")

st_write(obj = do.call(rbind,indices_list), # bind all years into one dataset
         dsn = paste0("data retrieval/index files/shapefile/all_year_indices",export_format))











#####################################################################################################################
########################################## test / verification code below here:######################################
#####################################################################################################################

#verify that B01001B_001 and B02001_003 (which should both be black population) are the same
# verify_pop_black<-get_acs(year=2018,
#                           variables=c("black1" = "B01001B_001",
#                                       "black2" = "B02001_003"),
#                           geography="tract",
#                           state="PA",
#                           county="Allegheny",geometry=TRUE)%>%
#   #limit only to east liberty tracts
#   filter(GEOID%in%c(42003111300,42003111500))

# #verify that B25008_002 and B25010_002 (owner-occupied units) are both the same 
# verify_owner_occupied<-get_acs(year=2018,
#                           variables=c("owner1" = "B25008_002",
#                                       "owner2" = "B25032_002"),
#                           geography="tract",
#                           state="PA",
#                           county="Allegheny")%>%
#   #limit only to east liberty tracts
#   filter(GEOID%in%c(42003111300,42003111500))
# 
# #verify that B01002_001 and S0101_C01_030 give the same number
# verify_median_age<-bind_rows(get_acs(year=2018,
#                                      variables=c("age1" = "S0101_C01_032"),
#                                      geography="tract",
#                                      state="PA",
#                                      county="Allegheny"),
#                              get_acs(year=2018,
#                                      variables=c("age2" = "B01002_001"),
#                                      geography="tract",
#                                      state="PA",
#                                      county="Allegheny"))%>%
#   #limit only to east liberty tracts
#   filter(GEOID%in%c(42003111300,42003111500))
# ?load_variables
# variables<-load_variables(year=2010,dataset="acs5")%>%
#   filter(grepl("B25077",name))
# 
# 
# east_lib2010<-indices_list[["2010"]]%>%
#   filter(grepl("1113|1115",GEOID))
# 
# east_lib2018<-indices_list[["2018"]]%>%
#   filter(grepl("1113|1115",GEOID))
# 
# 
# #figure out if east liberty is gentrification eligible
# for(year in years){
#   temp<-indices_list[[paste0(year)]]%>%
#     filter(grepl("1113|1115",GEOID))
#   print(year)
#   print(temp[,c("GEOID","pct_cbsa_income","pct_cbsa_value")])
# }
# 
# # add the years as names for indices list
# test<-indices_list[1]
# names(test)
# test2<-test[[1]]


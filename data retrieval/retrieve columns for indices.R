library(tidycensus)
library(dplyr)

#this code references the excel file Meredith made breaking down the different columns available by year

#goal: create a separate dataframe for each year that contains all of the columns needed

years<-2000:2018

#create a list to store the dataframes
year_indices<-list()

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
  "female_pop" = "B01001_026"
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
  "male_pop_20_64" = "S2301_C01_020",
  "male_employed" = "S2301_C03_020",
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



###### test / verification code below here:

#verify that B01001B_001 and B02001_003 (which should both be black population) are the same
# verify_pop_black<-get_acs(year=2018,
#                           variables=c("black1" = "B01001B_001",
#                                       "black2" = "B02001_003"),
#                           geography="tract",
#                           state="PA",
#                           county="Allegheny")%>%
#   #limit only to east liberty tracts
#   filter(GEOID%in%c(42003111300,42003111500))
# 
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


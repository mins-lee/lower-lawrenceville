library(dplyr)
library(sf)
library(tidyr)
library(tidycensus)

#retrieve block group data
alle_co_block_groups<-get_acs(year=2018,
                              variables=c("B01001_001"),
                              geography="block group",
                              state="PA",
                              county="Allegheny",
                              geometry=TRUE)%>%
  select(GEOID)


#save block group data
save(alle_co_block_groups,file="quantitative analysis/block groups.Rdata")

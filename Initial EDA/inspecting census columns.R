library(tidycensus)


all_tables<-tidycensus::load_variables(2015,"acs5")

race<-all_tables%>%filter(grepl("race",concept,ignore.case=TRUE))

education<-all_tables%>%filter(grepl("educational",concept,ignore.case=TRUE))
unique(education$concept)
test<- all_tables%>%filter(grepl("B01001",name,ignore.case=TRUE))
test2<-all_tables%>%filter(grepl("B03002",name,ignore.case=TRUE))

income<-all_tables%>%filter(grepl("median income",concept,ignore.case=TRUE))
native<-all_tables%>%filter(grepl("place.*birth",concept,ignore.case=TRUE))

income[11,]$concept

test_call<-get_acs(year=2015,variables=c("B07011_001"),state="NC",county="Yancey",geography="block group")

education<-all_tables%>%filter(grepl("B15003",name,ignore.case=TRUE))
poverty<-all_tables%>%filter(grepl("B17010",name,ignore.case=TRUE))%>%
  mutate(label=gsub('Estimate!!Total!!','',label))

income<-all_tables%>%filter(grepl("B19013",name,ignore.case=TRUE))%>%
  mutate(label=gsub('Estimate!!Total!!','',label))

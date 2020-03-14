rm(list=ls())
#Nick's datafiles have been downloaded into the working directory
library(readr)
library(dplyr)
library(stringr)
#turn off scientific notation
options(scipen=999)
#store list of files
#setwd("initial eda")
file_names<-list.files(path=paste0(getwd(),"/data preparation"),pattern="*.txt")
#use lapply to read in each file (using the read_tsv function) and store them in a list
data_list<-lapply(file_names,function(x) read_tsv(paste0(getwd(),"/data preparation/",x)))

#use do.call to rbind the three files
dat<-do.call(rbind,data_list)

#change the name of the last column to "Zip"
names(dat)[ncol(dat)]<-"ZIP"

#export as csv file
write_csv(dat,"./data preparation/HCV Data.csv",na="")
#####################################################################################################
############################################ Cleaning addresses######################################
#####################################################################################################
dat_cleaned<-dat%>%
  ####### Check 0: remove quotation marks
  mutate(PRIMARYSTREET = gsub('"',"",PRIMARYSTREET))%>%
  ####### Check 0b: remove # sign at the beginning of primary street addresses
  mutate(PRIMARYSTREET=gsub('^#','',PRIMARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 1: if there's a comma in the primary address, move everything after comma to secondary address
  mutate(SECONDARYSTREET=ifelse(grepl(",",PRIMARYSTREET),
                                paste(str_extract(PRIMARYSTREET,",.*"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after comma in primary address
         PRIMARYSTREET=gsub(",.*","",PRIMARYSTREET),
         #remove comma and space at the beginning of secondary street
         SECONDARYSTREET=gsub("^,","",SECONDARYSTREET),
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 2: If there's a number sign, extract and move
  mutate(SECONDARYSTREET=ifelse(grepl("#",PRIMARYSTREET),
                                paste(str_extract(PRIMARYSTREET,"#.*"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after # in primary address
         PRIMARYSTREET=gsub("#.*","",PRIMARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 3: If it contains "Apt"...
  mutate(SECONDARYSTREET=ifelse(grepl(" ap(artmen)?t",PRIMARYSTREET,ignore.case=TRUE),
                                paste(str_extract(tolower(PRIMARYSTREET)," ap(artmen)?t.*$"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after # in primary address
         PRIMARYSTREET=gsub(" ap(artmen)?t.*$","",PRIMARYSTREET,ignore.case=TRUE),
         #remove leading spaces
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 4: If it contains "Unit"...
  mutate(SECONDARYSTREET=ifelse(grepl(" unit .*$",PRIMARYSTREET,ignore.case=TRUE),
                                paste(str_extract(tolower(PRIMARYSTREET)," unit .*$"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after # in primary address
         PRIMARYSTREET=gsub(" unit .*$","",PRIMARYSTREET,ignore.case=TRUE),
         #remove leading spaces
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 5: If it contains "bldg" or "building...
  mutate(SECONDARYSTREET=ifelse(grepl(" b(ui)?ld(in)?g ",PRIMARYSTREET,ignore.case=TRUE),
                                paste(str_extract(tolower(PRIMARYSTREET)," b(ui)?ld(in)?g.*$"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after bldgin primary address
         PRIMARYSTREET=gsub(" b(ui)?ld(in)?g.*$","",PRIMARYSTREET,ignore.case=TRUE),
         #remove leading spaces
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 6: check for pattern of "number...floor
  mutate(SECONDARYSTREET=ifelse(grepl(" [0-9].*fl(oor)?$| [0-9].*fl(oor)? ",PRIMARYSTREET,ignore.case=TRUE),
                                paste(str_extract(tolower(PRIMARYSTREET)," [0-9].*fl(oor)?$| [0-9].*fl(oor)? "),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after [0-9].*(floor) in primary address
         PRIMARYSTREET=gsub(" [0-9].*fl(oor)?$| [0-9].*fl(oor)? ","",PRIMARYSTREET,ignore.case=TRUE),
         #remove leading spaces
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
      ### the code above erroneously moves numbered streets/aves to secondary address, so move them back
  mutate(PRIMARYSTREET=ifelse(grepl("street",SECONDARYSTREET,ignore.case=TRUE),
                              paste(PRIMARYSTREET,str_extract(SECONDARYSTREET,'[0-9]*.* street')),
                              PRIMARYSTREET),
         SECONDARYSTREET=gsub('[0-9]*.* street','',SECONDARYSTREET),
         PRIMARYSTREET=ifelse(grepl("ave",SECONDARYSTREET,ignore.case=TRUE),
                              paste(PRIMARYSTREET,str_extract(SECONDARYSTREET,'[0-9]*.* ave')),
                              PRIMARYSTREET),
         SECONDARYSTREET=gsub('[0-9]*.* ave','',SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ######## Check 7: check for pattern "flo?o?r? [0-9]$" (exactly one space between fl or floor and number)
  mutate(SECONDARYSTREET=ifelse(grepl("flo?o?r? [0-9]$",PRIMARYSTREET,ignore.case=TRUE),
                                paste(str_extract(tolower(PRIMARYSTREET),"flo?o?r? [0-9]$"),SECONDARYSTREET),
                                SECONDARYSTREET),
         #remove everything after bldgin primary address
         PRIMARYSTREET=gsub("flo?o?r? [0-9]$","",PRIMARYSTREET,ignore.case=TRUE),
         #remove leading spaces
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 8: remove parentheses
  mutate(PRIMARYSTREET = gsub("\\(","",PRIMARYSTREET),
         SECONDARYSTREET = gsub("\\(","",SECONDARYSTREET),
         PRIMARYSTREET = gsub("\\)","",PRIMARYSTREET),
         SECONDARYSTREET = gsub("\\)","",SECONDARYSTREET))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 9: delete addresses marked "do not use"
  filter(!grepl("do ?not ?use", PRIMARYSTREET,ignore.case=TRUE))%>%
  filter(!grepl("do ?not ?use", SECONDARYSTREET,ignore.case=TRUE))%>%
  ####### Check 10: move the words front and rear to secondary address
  mutate(SECONDARYSTREET=ifelse(grepl("front$",PRIMARYSTREET,ignore.case=TRUE),
                                paste("front",SECONDARYSTREET),
                                SECONDARYSTREET),
         PRIMARYSTREET=gsub("front$","",PRIMARYSTREET,ignore.case=TRUE),
         SECONDARYSTREET = ifelse(grepl("rear$",PRIMARYSTREET,ignore.case=TRUE),
                                paste("rear",SECONDARYSTREET),
                                SECONDARYSTREET),
         PRIMARYSTREET=gsub("rear$","",PRIMARYSTREET,ignore.case=TRUE))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ####### Check 11: move lower, upper, "lower level", and "upper level" to secondary
  mutate(SECONDARYSTREET = ifelse(grepl("lower$|(lower level)$",PRIMARYSTREET,ignore.case=TRUE),
                                paste("lower",SECONDARYSTREET),
                                SECONDARYSTREET),
         PRIMARYSTREET = gsub("lower$|(lower level)$","",PRIMARYSTREET,ignore.case=TRUE),
         SECONDARYSTREET = ifelse(grepl("upper$|(upper level)$",PRIMARYSTREET,ignore.case=TRUE),
                                  paste("upper",SECONDARYSTREET),
                                  SECONDARYSTREET),
         PRIMARYSTREET = gsub("upper$|(upper level)$","",PRIMARYSTREET,ignore.case=TRUE))%>%
  ######## Recurring Check: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  ######## Check 12: remove spaces dashes, and periods at the end of the street address
  mutate(PRIMARYSTREET=gsub("- ?$|\\. ?$| +$|, ?$","",PRIMARYSTREET))%>%
  #make date columns into date formats
  mutate(MOVEINDATE=as.Date(MOVEINDATE,format="%h/%d/%Y"),
         MOVEOUTDATE=as.Date(MOVEOUTDATE,format="%h/%d/%Y"))%>%
  #remove NAs at the end of secondarystreet
  mutate(SECONDARYSTREET=gsub('(NA)*','',SECONDARYSTREET))%>%
  #filter to only heads of households
  filter(RELATIONSHIP=="Head")%>%
  #remove columns for secondary address and city, since these are causing false matches
  select(-c("SECONDARYSTREET","CITY"))%>%
  #make zip code character
  mutate(ZIP=as.character(as.integer(ZIP)))%>%
  #replace periods in primary address with spaces, then replace double spaces with single spaces
  mutate(PRIMARYSTREET=gsub("\\."," ",PRIMARYSTREET),
         PRIMARYSTREET=gsub("  "," ",PRIMARYSTREET))%>%
  #make primary street all lower case
  mutate(PRIMARYSTREET=tolower(PRIMARYSTREET))%>%
  #replace full name road, street, avenue with abbreviations
  mutate(PRIMARYSTREET=gsub(" road$"," rd",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" avenue$"," ave",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" street$"," st",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" boulevard$"," blvd",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" court$"," ct",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" place$"," pl",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" drive$"," dr",PRIMARYSTREET),
         PRIMARYSTREET=gsub(" lane$", " ln",PRIMARYSTREET))%>%
  #replace " north " with " n " and " south " with " s ", making sure 
  mutate(PRIMARYSTREET=gsub("( south)[ &^(st$| ave$)]", " s ", PRIMARYSTREET),
         PRIMARYSTREET=gsub("( north)[ &^(st$| ave$)]", " n ", PRIMARYSTREET),
         PRIMARYSTREET=gsub("( west)[ &^(st$| ave$)]", " w ", PRIMARYSTREET),
         PRIMARYSTREET=gsub("( east)[ &^(st$| ave$)]", " e ",PRIMARYSTREET))%>%
  #Replace "5th" with "fifth" and "4th" with "fourth"
  mutate(PRIMARYSTREET=gsub("5th", "fifth",PRIMARYSTREET),
         PRIMARYSTREET=gsub("4th", "fourth",PRIMARYSTREET))%>%
  #replace
  unique()
#################################################### REMOVE ADDRESSES WITH DUPLICATES Program / Moveout###############
## Several rows are almost identical except one will be HACP, one will be ACHA, and the move-out dates will be different
## Solution: impute program as "both", keep later moveout
## Steps:
## 1.) Create frame of CLIENT_ID, MOVEINDATE, PRIMARYSTREET, GENDER, RACE, RELATIONSHIP, ZIP, with multiple HA's
## 2.) For rows that match the above frame on those variables, update HA to read both
## 3.) Group by all columns (including HA), summarise max moveout (removing NAs) (this might take care of other issue too?)
different_HAs<-dat_cleaned%>%
  #group by everything but HA, Race, Moveoutdate (race is excluded because many records used different race for different HA)
  group_by(CLIENT_ID,MOVEINDATE,PRIMARYSTREET,GENDER,RELATIONSHIP,ZIP)%>%
  summarise(count=length(unique(HA)))%>%
  ungroup()%>%
  #only keep rows with count >=2
  filter(count>=2)%>%
  select(-count)

dat_cleaned2<-dat_cleaned%>%
  #limit to just rows that match different_HAs
  inner_join(different_HAs)%>%
  #update HA to be "both"
  mutate(HA="Both")%>%
  #make unique
  unique()%>%
  #add the rest of the rows back in
  bind_rows(dat_cleaned%>%
              anti_join(different_HAs))

#################################################### Identical except different race ###############
#identify rows that are identical except for different races
different_race<-dat_cleaned2%>%
  #group by everything but moveoutdate
  group_by_at(setdiff(names(.),c("RACE","MOVEOUTDATE")))%>%
  summarise(count=length(unique(RACE)))%>%
  filter(count>=2)%>%
  select(-count)%>%
  ungroup()

#replace those rows with multiracial
dat_cleaned3<-dat_cleaned2%>%
  #identify rows with multiple races
  inner_join(different_race)%>%
  #update race to multi-racial
  mutate(RACE="Multi-Racial")%>%
  #make unique
  unique()%>%
  #join in rows that don't match again
  bind_rows(dat_cleaned2%>%
              anti_join(different_race))


#################################################### REMOVE ADDRESSES WITH DUPLICATES EXCEPT NULL MOVEOUT###############
##  Several rows are exactly identical except one will have a move-out date and one will be null
##  Realized this is a specific case of differing move-out dates, so just keeping max moveout dates solves both problems
dat_cleaned4<-dat_cleaned3%>%
  #group by everything but moveoutdate
  group_by_at(setdiff(names(.),"MOVEOUTDATE"))%>%
  #keep max moveout
  summarise(MOVEOUTDATE=max(MOVEOUTDATE,na.rm=TRUE))%>%
  #max function returns -Inf if all are na, so replace those with NAs again
  mutate(MOVEOUTDATE=na_if(MOVEOUTDATE,-Inf))%>%
  ungroup()


  


  

#test remaining duplicate records
remaining_dups<-dat_cleaned4%>%
  group_by(CLIENT_ID,MOVEINDATE)%>%
  summarise(count=n())%>%
  filter(count>=2)%>%
  select(-count)%>%
  left_join(dat_cleaned4)


############################################## Impute subsequent move-in dates as move-out dates ######
## For some records, a move-out date was not recorded even though a subsequent address / move-in date exists
## Goal: for each client, replace null moveoutdates with the next moveindate
## Steps:
## 1.) Iterate through clients, create subset of frame for given client
## 2.) iterate through 1: number of rows-1. Create row for first_address and second_address
## 3.) if moveoutdate is null for first_address, replace it with moveindate from second address
#create a unique identifier in dat_cleaned, that will be used to overwrite records
dat_cleaned5<-dat_cleaned4%>%
  mutate(ROW_ID=row_number())
#create counter for number of changes
changes<-0
for(client in dat_cleaned5$CLIENT_ID){
  client_subset<-dat_cleaned5%>%
    filter(CLIENT_ID==client)%>%
    arrange(MOVEINDATE)
  #only apply the for loop below if there's more than one row in client_subset
  if(nrow(client_subset)>1){
    #iterate through rows, up until one before last one
    for(i in 1:(nrow(client_subset)-1)){
      first_address<-client_subset[i,]
      next_address<-client_subset[i+1,]
      if(is.na(first_address$MOVEOUTDATE)){
        #increment change counter up
        changes<-changes+1
        #replace missing moveout date with new moveindate
        first_address$MOVEOUTDATE<-next_address$MOVEINDATE
        #update row of dat_cleaned4
        dat_cleaned5[first_address$ROW_ID,]<-first_address
        
      }
    }
    
  }

}

#find addresses where moveout date is less than movein date
negative_tenure<-dat_cleaned5%>%
  mutate(tenure=MOVEOUTDATE-MOVEINDATE)%>%
  filter(tenure<=0)

#find addresses corresponding to individuals with negative tenure
negative_tenure_clients<-dat_cleaned5%>%
  filter(CLIENT_ID%in% negative_tenure$CLIENT_ID)

#save as rdata file
save(dat_cleaned5,file="data preparation/all data clean 29-feb-2020.Rdata")

#save as csv file
write_csv(dat_cleaned5,"data preparation/all data clean 29-feb-2020.csv")

########################################################################################################
####################################### Test Code below here ###########################################
########################################################################################################

# length(unique(dat[dat$RELATIONSHIP=="Head" & dat$ZIP==15206,]$CLIENT_ID))
# test<-dat_cleaned%>%
#   filter(grepl("unit",PRIMARYSTREET,ignore.case=TRUE))
# length(unique(dat_cleaned$PRIMARYSTREET))
# 
# test<-dat_cleaned%>%filter(grepl(" upper",PRIMARYSTREET,ignore.case=TRUE))
# #### testing code below here
# #test to make sure the date conversion doesn't drop any
# test<-dat_cleaned%>%
#   filter(!is.na(MOVEOUTDATE))
# 
# 
# test<-dat%>%filter(grepl(" [0-9].*fl(oor)?$| [0-9].*fl(oor)? ",PRIMARYSTREET,ignore.case=TRUE))
# 
# test3<-dat_cleaned[dat_cleaned$PRIMARYSTREET=="",]
# test4<-dat[grep("43 South 15th",dat$PRIMARYSTREET),]
# 
# length(unique(dat[!(grepl("do ?not ?use",dat$PRIMARYSTREET,ignore.case=TRUE) |
#                       grepl("do ?not ?use",dat$SECONDARYSTREET,ignore.case=TRUE)),]$PRIMARYSTREET))
# 
# length(unique(dat$PRIMARYSTREET))
# length(unique(dat_cleaned$PRIMARYSTREET))
# test<-dat[grep("Bridgeview",dat$SECONDARYSTREET,ignore.case=TRUE),]
# relationship_breakdown<-dat_cleaned%>%
#   group_by(RELATIONSHIP)%>%
#   summarise(count=length(unique(CLIENT_ID)))
# 
# write_csv(relationship_breakdown,"number of clients per householder status.csv")
# 
# 
# #find remaining duplicates: same client and moveindate, or same client and primary address
# same_client_movein<-dat_cleaned5%>%
#   group_by(CLIENT_ID,MOVEINDATE)%>%
#   summarise(count=n())%>%
#   filter(count>=2)%>%
#   select(-count)%>%
#   left_join(dat_cleaned5)
# 
# write_csv(same_client_movein,"same client and moveindate.csv")
# 
# same_client_primary<-dat_cleaned5%>%
#   group_by(CLIENT_ID,PRIMARYSTREET)%>%
#   summarise(count=n())%>%
#   filter(count>=2)%>%
#   select(-count)%>%
#   left_join(dat_cleaned5)
# write_csv(same_client_primary,"same client and primary address.csv")
# test<-dat_cleaned5%>%
#   filter(CLIENT_ID%in%same_client_primary$CLIENT_ID)%>%
#   arrange(CLIENT_ID,MOVEINDATE)
# 
# test2<-dat_cleaned%>%
#   filter(CLIENT_ID==3276631)
# save(dat_cleaned5,file="cleaned data .RData")
# load("cleaned data.RData")
# 
# unique_addr<-dat_cleaned3%>%
#   group_by(PRIMARYSTREET)%>%
#   summarise(units=paste(unique(SECONDARYSTREET),collapse=", "))
# 
# unique_15206_addr<-dat_cleaned3%>%
#   filter(ZIP==15206)%>%
#   group_by(PRIMARYSTREET)%>%
#   summarise(units=paste(unique(SECONDARYSTREET),collapse=", "))
# 
# write_csv(unique_addr,"unique cleaned addr.csv")
# write_csv(unique_15206_addr,"unique cleaned addresses - 15206.csv")
# write_csv(dat_cleaned3,"cleaned data.csv")
# 
# 
# east_lib<-dat_cleaned%>%filter(ZIP==15206)
# length(unique(east_lib$PRIMARYSTREET))
# 
# #for now: heads of house
# unique(dat_cleaned$RELATIONSHIP)
# #find rows from full dataset associated only with individuals that at some point lived in east liberty
# east_lib_sometime<-dat_cleaned%>%
#   filter(CLIENT_ID%in%east_lib$CLIENT_ID)
# length(unique(east_lib_sometime$PRIMARYSTREET))
# 
# 
# length(unique(dat_cleaned$CLIENT_ID))
# length(unique(east_lib))

############################ OLD CODE BEFORE HERE ###################################
##  Steps:
##  1.) create separate dataset, "no_null_moveouts" that just contains client, movein, moveout, address, with moveout removed
##  2.) remove move_out from cleaned_data frame, delete non-unique rows
##  3.) left join cleaned_data with "no_null_moveouts"

# no_null_moveouts<-dat_cleaned2%>%
#   select(CLIENT_ID,MOVEINDATE,MOVEOUTDATE,PRIMARYSTREET)%>%
#   #delete null moveouts
#   filter(!is.na(MOVEOUTDATE))
# 
# dat_cleaned3<-dat_cleaned2%>%
#   #remove moveoutdate column
#   select(-MOVEOUTDATE)%>%
#   unique()%>%
#   left_join(no_null_moveouts,by=c("CLIENT_ID","MOVEINDATE","PRIMARYSTREET"))




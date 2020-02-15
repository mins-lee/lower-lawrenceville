#Nick's datafiles have been downloaded into the working directory
library(readr)
library(dplyr)
library(stringr)
#store list of files
file_names<-list.files(pattern="*.txt")
#use lapply to read in each file (using the read_tsv function) and store them in a list
data_list<-lapply(file_names,function(x) read_tsv(x))

#use do.call to rbind the three files
dat<-do.call(rbind,data_list)

#change the name of the last column to "Zip"
names(dat)[ncol(dat)]<-"ZIP"

#export as csv file
write_csv(dat,"HCV Data.csv",na="")

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
  unique()

#################################################### REMOVE ADDRESSES WITH DUPLICATES EXCEPT NULL MOVEOUT###############
##  Several rows are exactly identical except one will have a move-out date and one will be null
##  Steps:
##  1.) create separate dataset, "no_null_moveouts" that just contains client, movein, moveout, address, with moveout removed
##  2.) remove move_out from cleaned_data frame, delete non-unique rows
##  3.) left join cleaned_data with "no_null_moveouts"

no_null_moveouts<-dat_cleaned%>%
  select(CLIENT_ID,MOVEINDATE,MOVEOUTDATE,PRIMARYSTREET)%>%
  #delete null moveouts
  filter(!is.na(MOVEOUTDATE))

dat_cleaned2<-dat_cleaned%>%
  #remove moveoutdate column
  select(-MOVEOUTDATE)%>%
  unique()%>%
  left_join(no_null_moveouts,by=c("CLIENT_ID","MOVEINDATE","PRIMARYSTREET"))

check<-dat_cleaned2%>%
  filter(CLIENT_ID==3972573268)

save(dat_cleaned2,file="cleaned data.RData")
load("cleaned data.RData")

unique_addr<-dat_cleaned%>%
  group_by(PRIMARYSTREET)%>%
  summarise(units=paste(unique(SECONDARYSTREET),collapse=", "))

unique_15206_addr<-dat_cleaned%>%
  filter(ZIP==15206)%>%
  group_by(PRIMARYSTREET)%>%
  summarise(units=paste(unique(SECONDARYSTREET),collapse=", "))

write_csv(unique_addr,"unique cleaned addr.csv")
write_csv(unique_15206_addr,"unique cleaned addresses - 15206.csv")
write_csv(dat_cleaned,"cleaned data.csv")

east_lib<-dat_cleaned%>%filter(ZIP==15206)
length(unique(east_lib$PRIMARYSTREET))

#for now: heads of house
unique(dat_cleaned$RELATIONSHIP)
#find rows from full dataset associated only with individuals that at some point lived in east liberty
east_lib_sometime<-dat_cleaned%>%
  filter(CLIENT_ID%in%east_lib$CLIENT_ID)
length(unique(east_lib_sometime$PRIMARYSTREET))


length(unique(dat_cleaned$CLIENT_ID))
length(unique(east_lib))

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
test<-dat[grep("Bridgeview",dat$SECONDARYSTREET,ignore.case=TRUE),]
relationship_breakdown<-dat_cleaned%>%
  group_by(RELATIONSHIP)%>%
  summarise(count=length(unique(CLIENT_ID)))

write_csv(relationship_breakdown,"number of clients per householder status.csv")

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

#####################################################################################################
############################################ Cleaning addresses######################################
#####################################################################################################
test<-dat[grep(",",dat$SECONDARYSTREET),]
dat_cleaned<-dat%>%
  ####### Check 0: remove quotation marks
  mutate(PRIMARYSTREET = gsub('"',"",PRIMARYSTREET))%>%
  ####### Check 1: if there's a comma in the primary address, move everything after comma to secondary address
  mutate(SECONDARYSTREET=ifelse(grepl(",",PRIMARYSTREET) & is.na(SECONDARYSTREET),
                                str_extract(PRIMARYSTREET,",.*"),
                                SECONDARYSTREET),
         #remove everything after comma in primary address
         PRIMARYSTREET=gsub(",.*","",PRIMARYSTREET),
         #remove comma and space at the beginning of secondary street
         SECONDARYSTREET=gsub("^,","",SECONDARYSTREET),
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))%>%
  ####### Check 2: If there's a number sign, extract and move
  mutate(SECONDARYSTREET=ifelse(grepl("#",PRIMARYSTREET) & is.na(SECONDARYSTREET),
                                str_extract(PRIMARYSTREET,"#.*"),
                                SECONDARYSTREET),
         #remove everything after comma in primary address
         PRIMARYSTREET=gsub("#.*","",PRIMARYSTREET),
         #remove comma and space at the beginning of secondary street
         SECONDARYSTREET=gsub("^#","",SECONDARYSTREET),
         SECONDARYSTREET=gsub("^ ","",SECONDARYSTREET))
  

test<-dat_cleaned[grep('#',dat$PRIMARYSTREET),]

#export as csv file
write_csv(dat,"HCV Data.csv",na="")

length(unique(dat[dat$RELATIONSHIP=="Head" & dat$ZIP==15206,]$CLIENT_ID))

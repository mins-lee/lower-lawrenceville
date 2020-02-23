#pull median rent and recent

library(dplyr)
library(tidycensus)
library(openxlsx)
#create excel workbook where each sheet is a year's worth of columns
years<-2010:2018


#following process here: https://www.r-bloggers.com/easily-make-multi-tabbed-xlsx-files-with-openxlsx/
#create blank workbook
wb <- createWorkbook()

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

#same as above for the study tables
wb2<-createWorkbook()

#iterate through years, adding the variables for each year to the sheet
for(year in years){
  #pull acs5columns for given year
  dat2<-load_variables(year=year,dataset="acs5/subject")%>%
    mutate(label=gsub("Estimate!!Total!!","",label))%>%
    mutate(label=gsub("!!","",label))%>%
    #limit just to tables Meredith identified
    filter(grepl("S0101|S1101|S2301",name,ignore.case=FALSE))
  #write columns to excel worksheet
  addWorksheet(wb2,paste0(year))
  writeData(wb2,paste0(year),dat2)
}

saveWorkbook(wb2, file = "acs study columns for index.xlsx", overwrite = TRUE)

#same as above for decennial sf1 ()
wb3<-createWorkbook()

dec_years<-c(2000,2010)
#iterate through years, adding the variables for each year to the sheet
for(year in dec_years){
  #pull acs5columns for given year
  dat3<-load_variables(year=year,dataset="sf1")%>%
    mutate(label=gsub("Estimate!!Total!!","",label))%>%
    mutate(label=gsub("!!","",label))
  
  #write columns to excel worksheet
  addWorksheet(wb3,paste0(year))
  writeData(wb3,paste0(year),dat3)
}


saveWorkbook(wb3, file = "decennial sf1 columns.xlsx", overwrite = TRUE)

rm(wb3)

#same as above for decennial sf1 ()
wb4<-createWorkbook()

dec_years<-c(2000,2010)
#iterate through years, adding the variables for each year to the sheet
for(year in dec_years){
  print(year)
  #pull acs5columns for given year
  dat4<-load_variables(year=year,dataset="sf3")%>%
    mutate(label=gsub("Estimate!!Total!!","",label))%>%
    mutate(label=gsub("!!","",label))
  
  #write columns to excel worksheet
  addWorksheet(wb4,paste0(year))
  writeData(wb4,paste0(year),dat4)
}


saveWorkbook(wb4, file = "decennial sf3 columns.xlsx", overwrite = TRUE)

rm(wb4)




#same as above for profile columns

#create excel workbook where each sheet is a year's worth of columns

wb5<-createWorkbook()

#iterate through years, adding the variables for each year to the sheet
for(year in years){
  #pull acs5columns for given year
  dat5<-load_variables(year=year,dataset="acs5/profile")%>%
    mutate(label=gsub("Estimate!!Total!!","",label))%>%
    mutate(label=gsub("!!","",label))
  
  #write columns to excel worksheet
  addWorksheet(wb5,paste0(year))
  writeData(wb5,paste0(year),dat5)
}

saveWorkbook(wb5, file = "acs profile columns.xlsx", overwrite = TRUE)


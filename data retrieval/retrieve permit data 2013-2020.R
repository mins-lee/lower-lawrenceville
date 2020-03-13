rm(list=ls())
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(httr)
library(readxl)
library(stringr)
# declare url for permit data
permit_url<-"https://data.wprdc.org/dataset/city-of-pittsburgh-building-permit-summary"


#fetch page
page<-read_html(permit_url)
# retrieve site, find all xlsx links
resource_list<-page%>%
  #pull resource list
  html_node(xpath='//*[@id="dataset-resources"]/ul')%>%
  #pull rows in list
  html_nodes("div")%>%
  html_nodes("a")
# drill down to character list
all_links<-resource_list%>%
  html_attrs()%>%
  unlist()

#find only xl links
xl_links<-all_links[grepl("xls",all_links)]

#retrieve lines containing names of the months
month_name_lines<-page%>%
  #pull resource list
  html_node(xpath='//*[@id="dataset-resources"]/ul')%>%
  html_nodes("a")%>%
  html_attr("title")
# remove nas in month_name_lines
month_name_lines2<-month_name_lines[!is.na(month_name_lines)]
# remove everything before the word summary
month_names<-gsub(".* Summary ","",
                  #remove everything before the colon
                  gsub(".*: ","",month_name_lines2))


#vector of column names to assign
permit_colnames<-c("PERMIT NUMBER", "ISSUE DATE", "PARCEL", "ADDRESS", "SNP_NEIGHBORHOOD", "WARD", "OWNER NAME",
                   "CONTRACTOR NAME", "TYPE OF WORK", "TYPE OF STRUCTURE")
#create a function that takes a link, and returns a dataframe
link<-xl_links[[24]]
download_permits<-function(link){
  #set the extension based 
  extension<-str_match(link,"\\.xlsx?")[[1]]
  # following code from here to read xl files https://stackoverflow.com/questions/41368628/read-excel-file-from-a-url-using-the-readxl-package
  GET(link, write_disk(tf <- tempfile(fileext = "extension")))
  
  dat<-tryCatch({read_excel(tf)},
           warning=function(cond){
             read_excel(tf)},
           error=function(cond){
             NA})
  # if(extension==".xlsx"){
  #   dat<-read_xlsx(tf)
  # }else{
  #   dat<-read_xls(tf)
  #   
  # }
  #make the last column character so it doesn't mess up bind
  #dat[,ncol(dat)]<-as.character(dat[,ncol(dat)])
  #names(dat)<-permit_colnames
  dat
}
for(i in 1:length(xl_links)){
  print(i)
  download_permits(xl_links[[i]])
}



#apply link to download all xlsx links
permit_list<-lapply(xl_links,download_permits)

#obtain a list of files that couldn't be read in correctly, these will need to be read in manually
to_add_manually<-c()
for(i in 1:length(permit_list)){
  if(length(permit_list[[i]])==1){
    to_add_manually<-c(to_add_manually, month_names[i])
  }
}
#create new permit list with dataframes that didn't load properly removed
permit_list2<-list()
month_names2<-c()
for(i in 1:length(permit_list)){
  if(length(permit_list[[i]])!=1){
    permit_list2<-append(permit_list2,list(permit_list[[i]]))
    #also update month_names2
    month_names2<-c(month_names2,month_names[i])
  }
}



# some of the months have 3 blank rows before column names, so need to delete those rows and fix names
name_list<-list()
for(i in 1:length(permit_list2)){
  name_list<-append(name_list,list(names(permit_list2[[i]])))
}

#identify dataframes where the columns didn't read in because of blanks at the top
# these will be indicated by an "X__[0-9]" in one of the columns
colnames_wrong<-c()
for(i in 1:length(permit_list2)){
  if(any(grepl("X__[0-9]",names(permit_list2[[i]])))){
    colnames_wrong<-c(colnames_wrong,i)
  }
}

#remove rows where every column is na
for(i in 1:length(permit_list2)){
  temp_frame<-permit_list2[[i]]
  permit_list2[[i]]<-temp_frame%>%
    filter_all(any_vars(!is.na(.)))
}

test<-permit_list2[[colnames_wrong[10]]]

for(n in colnames_wrong){
  print(permit_list2[[n]][2,])
}
test<-permit_list2[[73]]
#identify all months where there are blanks in the first row
unique(name_list)
# first, create a function that takes a a frame and row number, changes the names to be that row, and deletes previous rows
fix_names<-function(frame,row_num){
  #create a new frame to be fixed
  new_frame<-frame
  #change row name
  names(frame)<-frame[row_num,]
  #only keep everything after column frame
  new_frame[(row_num+1):nrow(new_frame),]
  
}

#go through permit list, and apply the fix_names function if it col 2 has the name "X__1"


#identify unique names of different data-frames for binding
name_list<-lapply(permit_list,names)

permit_data13_20<-bind_rows(permit_list)

####################################################### TEST CODE BELOW HERE
# for(i in 1:length(permit_list)){
#   print(names(permit_list[[i]]))
# }


#create a function to extract names from permit list
test1<-permit_list[[1]]
test2<-permit_list[[8]]
test3<-permit_list[[11]]
for(x in permit_list){
  print(ncol(x))
}


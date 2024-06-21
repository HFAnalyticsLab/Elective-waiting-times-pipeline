##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Delete temporary files along the way
#Use s3sync() to sync into a bucket when ready
#delete file 1a and file 2 from AWS branch

##############################################
################### SETUP ####################
##############################################

###### Libraries ######

#Some of these might not be needed
library(tidyverse)
library(rvest)
library(downloader)
library(stringr)
library(qdapRegex)
library(tidyr)
library(purrr)
library(pbapply)
library(data.table)
library(readr)
library(readxl)
library(aws.s3)

rm(list = ls()) #Clear the working environment
source('setup.R') #get project locations in s3 and working directory

# IHT_bucket: s3 project bucket
# RTT_subfolder: folder to place data
# R_workbench: R server working directory

#Create folder in local workbench for temporary files

setwd(R_workbench)
temp_folder <- "RTT_temp_data"

#Main folder
if (file.exists(temp_folder)) {
  cat("The folder already exists")
} else {
  dir.create(temp_folder)
}

#Folder for temp files
if (file.exists(paste0(temp_folder,"/temp files"))) {
  cat("The folder already exists")
} else {
  dir.create(paste0(temp_folder,"/temp files"))
}

#####################################################
################### Web-scraping ####################
#####################################################
year_lkup <- function(y, l=12){
  
  m <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
  year <- c(rep(y,9),rep(y+1,3))
  series <- rep(as.character(paste0(y, y+1)), 12)
  return(
    cbind.data.frame(month=paste0(m,year),series=series) %>%
      head(l)
  )
  
}

#All together

inputs <- plyr::rbind.fill(year_lkup(24,1),year_lkup(23),year_lkup(22),year_lkup(21),year_lkup(20),year_lkup(19),year_lkup(18))

#Function that reports links to 3 files for each month

return_links_rtt <- function(month,series){
  
  #Find landing page for the appropriate financial year
  if (series=="2425"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/")
  } else if (series=="2324"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/")
  } else if (series=="2223"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/")
  } else if (series=="2122"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/")
  } else if (series=="2021"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/")
  } else if (series=="1920") {
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/")
  } else if (series=="1819") {
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/")
  }
  
  #This expression will extract all links associated with any text that contains the name of the month (e.g. "Jan")
  #See xpath cheat sheet here 'https://cheatography.com/alexsiminiuc/cheat-sheets/xpath/'
  #Or tutorial here 'https://levelup.gitconnected.com/master-the-art-of-writing-xpath-for-web-scraping-c14e2f7ee130'
  
  xpath_month <- paste0("//a[contains(text(),'",month,"')]/@href")
  
  #These are all the links associated with this given month
  
  links <- read.first.page %>%
    html_nodes(xpath=xpath_month) %>%
    html_text()
  
  #We're only interested in 5 of those files
  
  #ZIP
  full.csv.link <- links[str_detect(links, "Full-CSV")][1]
  
  #Provider-level files
  providers.link.incomp <- links[str_detect(links, "Incomplete-Provider")][1]
  providers.link.adm <- links[str_detect(links, "Admitted-Provider")][1]
  providers.link.nonadm <- links[str_detect(links, "NonAdmitted-Provider")][1]
  providers.link.new <- links[str_detect(links, "New-Periods-Provider")][1]
  
  #Data frame of files to download
  out <- data.frame(month=month,
                    series=series,
                    full.csv.link,
                    providers.link.incomp,
                    providers.link.new,
                    providers.link.adm,
                    providers.link.nonadm)
  
  return(out)
  
}

#Example
#return_links_rtt("Jul","2122")

#Apply for all months to get all links, and store there is 'links.out.df'

links.out <- mapply(return_links_rtt,
                    month=inputs$month,
                    series = inputs$series)
links.out.df <- as.data.frame(t(links.out)) %>%
  filter(.,!is.na(full.csv.link)) #Filter out months that haven't been uploaded yet or don't exist
rm(inputs,links.out)

#links.out.df <- head(links.out.df,n=3) #For now, check that it works for the first 3 months

## this bit not true now as NHS must have fixed the link
# April 2022 does not follow the same pattern for links so add manually
# missing <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/06/NonAdmitted-Provider-Apr-22-XLS-8573K-57873.xls'
# links.out.df$providers.link.nonadm$Apr22 <- missing
###########################################################
################### Download all files ####################
###########################################################

#Which months have we already downloaded locally?

already_there <- list.dirs(path = paste0(temp_folder,"/temp files"), full.names = TRUE, recursive = TRUE) %>%
  str_replace_all(.,paste0(temp_folder,"/temp files/"),"")

#Download a set of files for each month (unless already there locally)
## now xlsx files turning up in more recent data as well as xls previously
## use tools::file_ext() to extract and assign correctly
for (k in 1:nrow(links.out.df)){
  
if (links.out.df$month[k] %in% already_there){
  cat("The files are already there")
}
  else{
    cat("Downloading new files")
    
    #Download Full CSV in workbench
    download(as.character(links.out.df$full.csv.link[k]),
             dest=paste0("RTT_temp_data/temp files/",links.out.df$month[k],".zip"), mode="wb")
    #Unzip Full CSV in workbench
    unzip(paste0("RTT_temp_data/temp files/",links.out.df$month[k],".zip"),
          exdir = paste0("RTT_temp_data/temp files/",links.out.df$month[k]))
    #Delete zip file
    file.remove(paste0("RTT_temp_data/temp files/",links.out.df$month[k],".zip"))
    
    #Download New Providers
    download(as.character(links.out.df$providers.link.new[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-newproviders.", 
                         tools::file_ext(links.out.df$providers.link.new[k])),
             mode="wb")
    
    #Download Admitted Providers
    download(as.character(links.out.df$providers.link.adm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-admitted.",
                         tools::file_ext(links.out.df$providers.link.adm[k])),
             mode="wb")
    
    #Download Non-Admitted Providers
    download(as.character(links.out.df$providers.link.nonadm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-nonadmitted.",
                         tools::file_ext(links.out.df$providers.link.nonadm[k])), 
             mode="wb")
    
    ### Incomplete providers
    
    #Download Incomplete Providers
    download(as.character(links.out.df$providers.link.incomp[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-incomplete.",
                         tools::file_ext(links.out.df$providers.link.incomp[k])),
             mode="wb") 
  }
}

#Clean up files
rm(already_there)

###########################################################################
################### Append all IS provider files into one #################
###########################################################################

for (s in 1:nrow(links.out.df)){
  
  #Open all provider files for one month and append

  setwd(paste0(R_workbench,"/",temp_folder,"/temp files/"))
  setwd(as.character(links.out.df$month[s]))
  
  incomplete <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.",
                                  tools::file_ext(links.out.df$providers.link.incomp[s])),
                           sheet = "IS Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.",
                                     tools::file_ext(links.out.df$providers.link.incomp[s])),
                           sheet = "IS Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links.out.df$month[s],"-newproviders.",
                                    tools::file_ext(links.out.df$providers.link.new[s])),
                             sheet = "IS Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-admitted.",
                                    tools::file_ext(links.out.df$providers.link.adm[s])),
                             sheet = "IS Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-nonadmitted.",
                                       tools::file_ext(links.out.df$providers.link.nonadm[s])),
                                sheet = "IS Provider",skip=13)
  
  #IS providers for that month
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
              incomplete$`Region Code`,incompleteDTA$`Region Code`)
  summary_month <- data.frame(monthyr=rep(as.character(links.out.df$month[s]),length(codes)),codes,names,region)
  rm(incomplete,incompleteDTA,new_provider,adm_provider,nonadm_provider,codes,names,region)
  
  #Successively append files
  
  if (s==1) {
    storage <- summary_month
  } else {
    storage <- plyr::rbind.fill(storage,summary_month)
  }
}

#Remove duplicates
IS_providers_allmonths <- storage[!duplicated(storage), ]
rm(storage,summary_month)

#Save
setwd(paste0(R_workbench,"/",temp_folder,"/"))
fwrite(IS_providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                             "/IS_providers_allmonths.csv"), sep = ",")
rm(IS_providers_allmonths)

#### Get all NHS provider locations / region
for (s in 1:nrow(links.out.df)){
  
  #Open all provider files for one month and append
  
  setwd(paste0(R_workbench,"/",temp_folder,"/temp files/"))
  setwd(as.character(links.out.df$month[s]))
  
  incomplete <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.",
                                  tools::file_ext(links.out.df$providers.link.incomp[s])),
                           sheet = "Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.",
                                     tools::file_ext(links.out.df$providers.link.incomp[s])),
                              sheet = "Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links.out.df$month[s],"-newproviders.",
                                    tools::file_ext(links.out.df$providers.link.new[s])),
                             sheet = "Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-admitted.",
                                    tools::file_ext(links.out.df$providers.link.adm[s])),
                             sheet = "Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-nonadmitted.",
                                       tools::file_ext(links.out.df$providers.link.nonadm[s])),
                                sheet = "Provider",skip=13)
  
  #IS providers for that month
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
              incomplete$`Region Code`,incompleteDTA$`Region Code`)
  summary_month <- data.frame(monthyr=rep(as.character(links.out.df$month[s]),length(codes)),codes,names,region)
  rm(incomplete,incompleteDTA,new_provider,adm_provider,nonadm_provider,codes,names,region)
  
  #Successively append files
  
  if (s==1) {
    storage <- summary_month
  } else {
    storage <- plyr::rbind.fill(storage,summary_month)
  }
}

#Remove duplicates
providers_allmonths <- storage[!duplicated(storage), ]
rm(storage,summary_month)

#Save
setwd(paste0(R_workbench,"/",temp_folder,"/"))
fwrite(providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                             "/providers_allmonths.csv"), sep = ",")
rm(IS_providers_allmonths)


########################################################################################
################### Append all monthly waiting times files into one ####################
########################################################################################

#Re-load IS provider by month

IS_providers_allmonths <- fread(paste0(R_workbench,"/",temp_folder,"/IS_providers_allmonths.csv"),
                                header=TRUE, sep=",", check.names=T)

#Append all files

for (j in 1:nrow(links.out.df)){
  
  setwd(paste0(R_workbench,"/",temp_folder,"/temp files/"))
  setwd(as.character(links.out.df$month[j]))
  
  #Display progress
  #cat(links.out.df$month[j]))

  #Find name of large CSV
  file.name <- list.files()[str_detect(list.files(),"full-extract")][1] #To make sure there's only one file - is it the right one?
  
  #Read in
  RTT_month <- fread(file.name, header=TRUE, sep=",", check.names=T)
  rm(file.name)
  
  #Add month-year indicator
  RTT_month$monthyr <- as.character(links.out.df$month[j])
  
  #New indicator variable to flag independent providers
  RTT_month$IS_provider <- ifelse(RTT_month$Provider.Org.Code %in% filter(IS_providers_allmonths,monthyr==links.out.df$month[j])$codes,1,0)
  
  #Successively append files
  if (j==1) {
    storage.rtt <- RTT_month
  } else {
    storage.rtt <- plyr::rbind.fill(storage.rtt,RTT_month)
  }
}

#Save
setwd(paste0(R_workbench,"/",temp_folder,"/"))
fwrite(storage.rtt, file = "RTT_allmonths_new.csv", sep = ",")

#Clean up files
rm(links.out.df,IS_providers_allmonths,RTT_month,storage.rtt)

#################################################################
################### Delete temporary folders ####################
#################################################################

setwd(paste0(R_workbench,"/",temp_folder,"/"))
unlink("temp files",recursive=TRUE)

#######################################################################
################### Sync local folder to S3 bucket ####################
#######################################################################

setwd(paste0(R_workbench,"/",temp_folder,"/"))

put_object(file = 'RTT_allmonths_new.csv',
           object = paste0(RTT_subfolder,"/","RTT_allmonths_new.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)

put_object(file = 'IS_providers_allmonths.csv',
           object = paste0(RTT_subfolder,"/","IS_providers_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)

put_object(file = 'providers_allmonths.csv',
           object = paste0(RTT_subfolder,"/","providers_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)

##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Delete temporary files along the way
#Use s3sync() to sync into a bucket when ready
#delete file 1a and file 2 from AWS branch

#Other useful AWS functions

#dummy <- data.frame(var1=as.character(1:100))
# s3write_using(dummy # What R object we are saving
#               , FUN = write.csv # Which R function we are using to save
#               , object = 'RTT waiting times data/dummy.csv' # Name of the file to save to (include file type)
#               , bucket = "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp") # Bucket name defined above
# 

#Create sub-folder in S3
# put_folder(paste0(RTT_subfolder,"/",as.character(links.out.df$month[k])),
#            bucket = IHT_bucket)

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

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")

#Create folder in local workbench for temporary files

setwd(R_workbench)
temp_folder <- "RTT_temp_data"
if (file.exists(temp_folder)) {
  cat("The folder already exists")
} else {
  dir.create(temp_folder)
}

#####################################################
################### Web-scraping ####################
#####################################################

#2020-2021
months2122 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years2122 <- c(rep(21,9),rep(22,3))
series2122 <- rep(2122,length(months2122))
input2122 <- cbind.data.frame(month=paste0(months2122,years2122),series=series2122)
rm(months2122,years2122,series2122)

#2019-2020
months2021 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years2021 <- c(rep(20,9),rep(21,3))
series2021 <- rep(2021,length(months2021))
input2021 <- cbind.data.frame(month=paste0(months2021,years2021),series=series2021)
rm(months2021,years2021,series2021)

#2019-2020
months1920 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years1920 <- c(rep(19,9),rep(20,3))
series1920 <- rep(1920,length(months1920))
input1920 <- cbind.data.frame(month=paste0(months1920,years1920),series=series1920)
rm(months1920,years1920,series1920)

#2018-2019
months1819 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years1819 <- c(rep(18,9),rep(19,3))
series1819 <- rep(1819,length(months1819))
input1819 <- cbind.data.frame(month=paste0(months1819,years1819),series=series1819)
rm(months1819,years1819,series1819)

#All together

inputs <- plyr::rbind.fill(input2122,input2021,input1920,input1819)
rm(input2122,input2021,input1920,input1819)

#Function that reports links to 3 files for each month

return_links_rtt <- function(month,series){
  
  #Find landing page for the appropriate financial year
  
  if (series=="2122"){
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

#links.out.df <- head(links.out.df,n=3) #For now, check that it works for the first 5 months

###########################################################
################### Download all files ####################
###########################################################

#Which months have we already downloaded locally?

already_there <- list.dirs(path = temp_folder, full.names = TRUE, recursive = TRUE) %>%
  str_replace_all(.,paste0(temp_folder,"/"),"")

#Download a set of files for each month (unless already there locally)

for (k in 1:nrow(links.out.df)){
  
if (links.out.df$month[k] %in% already_there){
  cat("The files are already there")
}
  else{
    cat("Downloading new files")
    ### Full CSV
    
    #Download Full CSV in workbench
    download(as.character(links.out.df$full.csv.link[k]),
             dest=paste0("RTT_temp_data/",links.out.df$month[k],".zip"), mode="wb")
    #Unzip Full CSV in workbench
    unzip(paste0("RTT_temp_data/",links.out.df$month[k],".zip"),
          exdir = paste0("RTT_temp_data/",links.out.df$month[k]))
    #Delete zip file
    file.remove(paste0("RTT_temp_data/",links.out.df$month[k],".zip"))
    
    ### New providers
    
    #Download New Providers
    download(as.character(links.out.df$providers.link.new[k]),
             dest=paste0("RTT_temp_data/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-newproviders.xls"), mode="wb")
    
    ### Admitted providers
    
    #Download Admitted Providers
    download(as.character(links.out.df$providers.link.adm[k]),
             dest=paste0("RTT_temp_data/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-admitted.xls"), mode="wb")
    
    ### Non-admitted providers
    
    #Download Non-Admitted Providers
    download(as.character(links.out.df$providers.link.nonadm[k]),
             dest=paste0("RTT_temp_data/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-nonadmitted.xls"), mode="wb")
    
    ### Incomplete providers
    
    #Download Incomplete Providers
    download(as.character(links.out.df$providers.link.incomp[k]),
             dest=paste0("RTT_temp_data/",
                         paste(links.out.df$month[k]),"/",
                         links.out.df$month[k],"-providers-incomplete.xls"), mode="wb") 
  }
}

#Clean up files
rm(already_there)

###########################################################################
################### Append all provider files into one ####################
###########################################################################

for (s in 1:nrow(links.out.df)){
  
  #Open all provider files for one month and append

  setwd(paste0(R_workbench,"/",temp_folder,"/"))
  setwd(as.character(links.out.df$month[s]))
  
  incomplete <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.xls"),
                           sheet = "IS Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.xls"),
                           sheet = "IS Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links.out.df$month[s],"-newproviders.xls"),
                             sheet = "IS Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-admitted.xls"),
                             sheet = "IS Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-nonadmitted.xls"),
                                sheet = "IS Provider",skip=13)
  
  #IS providers for that month
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  summary_month <- data.frame(monthyr=rep(as.character(links.out.df$month[s]),length(codes)),codes,names)
  rm(incomplete,incompleteDTA,new_provider,adm_provider,nonadm_provider,codes,names)
  
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

########################################################################################
################### Append all monthly waiting times files into one ####################
########################################################################################

#Re-load IS provider by month

IS_providers_allmonths <- fread(paste0(R_workbench,"/",temp_folder,"/IS_providers_allmonths.csv"),
                                header=TRUE, sep=",", check.names=T)

#Append all files

for (j in 1:nrow(links.out.df)){
  
  setwd(paste0(R_workbench,"/",temp_folder,"/"))
  setwd(as.character(links.out.df$month[j]))

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
fwrite(storage.rtt, file = "RTT_allmonths.csv", sep = ",")

#Clean up files
rm(links.out.df,IS_providers_allmonths,RTT_month,storage.rtt)

#######################################################################
################### Sync local folder to S3 bucket ####################
#######################################################################

setwd(paste0(R_workbench,"/",temp_folder,"/"))

put_object(file = 'RTT_allmonths.csv',
           object = paste0(RTT_subfolder,"/","RTT_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           mulitpart=TRUE)

put_object(file = 'IS_providers_allmonths.csv',
           object = paste0(RTT_subfolder,"/","IS_providers_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           mulitpart=TRUE)
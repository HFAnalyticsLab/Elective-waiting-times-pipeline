##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Some of these search results probably contain mistakes
#Next, map to LSOA so we can get IMD and regions

#Run search for all combinations of name-code, not just the first that appears
#That way it would be less random

#Other RESOURCES TO GET locations
#https://healthgps.co.uk/ has org codes and addresses

##############################################
################### SETUP ####################
##############################################

#Load packages

library(tidyverse)
library(stringr)
library(tidyr)
library(pbapply)
library(data.table)
library(readr)
library(readxl)
library(janitor)
library(aws.s3)
library(googleway)

#Clean up the global environment

rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")

#Google Maps API

source("API keys.R")

#####################################################
################### Load all providers ##############
#####################################################

# RTT_allmonths <- s3read_using(fread
#                               , object = paste0(RTT_subfolder,"/","RTT_allmonths.csv") # File to open
#                               , bucket = IHT_bucket) # Bucket name defined above
# 
# all_providers <- RTT_allmonths %>%
#   select(.,Provider.Org.Code,Provider.Org.Name) %>%
#   distinct()
# 
# rm(RTT_allmonths)
# 
# s3write_using(all_providers # What R object we are saving
#               , FUN = write.csv # Which R function we are using to save
#               , object = paste0(RTT_subfolder,"/","all_providers.csv") # Name of the file to save to (include file type)
#               , bucket = IHT_bucket) # Bucket name defined above

all_providers <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/Locating providers/","all_providers.csv") # File to open
                              , bucket = IHT_bucket, header=TRUE, drop=1) # Bucket name defined above

# all_providers_sample <- all_providers[sample(1:nrow(all_providers),50,replace=FALSE),]
all_providers_sample <- all_providers

#################################################################
################### Function to return coordinates ##############
#################################################################

################### Write function that calls the Google Maps API

#Google Way function

search_hospitals_google <- function(orgcode,type){
  
  #Provider name and code
  orgname <- filter(all_providers_sample,Provider.Org.Code==orgcode) %>%
    head(.,n=1) %>% 
    pull(Provider.Org.Name) %>%
    unlist()
  
  #Carry out search
  search_term <- paste0(orgname," in England")
  
  if(type=="none"){
    search_result <- google_places(search_string =  search_term,
                                   key = api_key)
  } else (
    search_result <- google_places(search_string =  search_term, place_type = type,
                                   key = api_key)
  )
  
  #Status
  status <- search_result$status[1]
  
  #Number of results
  number_results <- length(search_result$results$name)
  
  #First name
  name <- ifelse(number_results==0,"NA",search_result$results$name[1])
  
  #Place ID
  id <- ifelse(number_results==0,"NA",search_result$results$place_id[1])
  
  #Address
  address <- ifelse(number_results==0,"NA",search_result$results$formatted_address[1])
  
  #Latitude
  lat <- ifelse(number_results==0,"NA",search_result$results$geometry$location$lat[1])
  
  #Longitude
  long <- ifelse(number_results==0,"NA",search_result$results$geometry$location$lng[1])
  
  #Return
  search_results_table <- data.frame(Provider.Org.Code=as.character(orgcode),
                                     Provider.Org.Name=as.character(orgname),
                                     place_type_provided=type,
                                     status=status,
                                     number_results=number_results,
                                     name=name,
                                     id=id,
                                     address=address,
                                     lat=lat,
                                     long=long)
  
  return(as.data.frame(search_results_table))
}

#Fail-proof Google Way function

search_hospitals_google_catch <- function(orgcode,type){
  tryCatch(
    {
      return(search_hospitals_google(orgcode,type))
    },
    error=function(error_message) {
      
      #Provider name and code
      orgname <- filter(all_providers_sample,Provider.Org.Code==orgcode) %>%
        head(.,n=1) %>% 
        pull(Provider.Org.Name) %>%
        unlist()
      
      #Results in API fails
      search_results_table_fail <- data.frame(Provider.Org.Code=as.character(orgcode),
                                              Provider.Org.Name=as.character(orgname),
                                              place_type_provided=type,
                                              status="fail",
                                              number_results="fail",
                                              name=NA,
                                              id=NA,
                                              address=NA,
                                              lat=NA,
                                              long=NA)
      
      return(search_results_table_fail)
    }
  )
}

################### First loop, using hospital tag

first.args <- all_providers_sample$Provider.Org.Code

out.search.hospitals <- pbmapply(search_hospitals_google_catch,
                                 orgcode=first.args,
                                 MoreArgs = list(type="hospital"))
out.search.hospitals.df <- as.data.frame(t(out.search.hospitals))

# s3write_using(out.search.hospitals.df # What R object we are saving
#               , FUN = fwrite # Which R function we are using to save
#               , object = paste0(RTT_subfolder,"/Locating providers/","Google_round1_typehospitals.csv") # Name of the file to save to (include file type)
#               , bucket = IHT_bucket) # Bucket name defined above

###################  Second loop: run on the ones that failed, don't use tags this time to widen search

rm(out.search.hospitals,out.search.hospitals.df)

Google_round1_typehospitals <- s3read_using(fread
                                            , object = paste0(RTT_subfolder,"/Locating providers/","Google_round1_typehospitals.csv") # File to open
                                            , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

second.args <- Google_round1_typehospitals %>%
  filter(., status %in% c("ZERO_RESULTS","fail")) %>%
  pull(.,Provider.Org.Code) %>%
  unique(.)

out.search.hospitals.two <- pbmapply(search_hospitals_google_catch,
                                 orgcode=second.args,
                                 MoreArgs = list(type="none"))
out.search.hospitals.two.df <- as.data.frame(t(out.search.hospitals.two))

# s3write_using(out.search.hospitals.two.df # What R object we are saving
#               , FUN = fwrite # Which R function we are using to save
#               , object = paste0(RTT_subfolder,"/Locating providers/","Google_round2_typenone.csv") # Name of the file to save to (include file type)
#               , bucket = IHT_bucket) # Bucket name defined above

###################  Clean up outputs from loops

rm(out.search.hospitals.two,out.search.hospitals.two.df)

Google_round1_typehospitals <- s3read_using(fread
                                            , object = paste0(RTT_subfolder,"/Locating providers/","Google_round1_typehospitals.csv") # File to open
                                            , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

Google_round2_typenone <- s3read_using(fread
                                            , object = paste0(RTT_subfolder,"/Locating providers/","Google_round2_typenone.csv") # File to open
                                            , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

#Remove round 2 from round 2

Google_round1_typehospitals <- Google_round1_typehospitals %>%
  filter(.,!(Provider.Org.Code %in% Google_round2_typenone$Provider.Org.Code))

#All rounds in one file

Google_all_rounds <- plyr::rbind.fill(Google_round1_typehospitals,Google_round2_typenone)

#Show duplicates

#These shouldn't be there - to fix in next iteration
Google_all_rounds_nodup <- Google_all_rounds %>%
  distinct()

#Remove duplicates there by mistake
Google_all_rounds_nodup <- Google_all_rounds_nodup %>%
  mutate(number_results=as.numeric(number_results)) %>% 
  group_by(Provider.Org.Code,id) %>%
  top_n(1, abs(number_results)) %>%
  ungroup()

#Save
s3write_using(Google_all_rounds_nodup # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Locating providers/","RTT provider locations from Google Maps.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

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
library(rgdal)

#Clean up the global environment

rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")
git_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

#Google Maps API

setwd(git_directory)
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

#Remove duplicates: all 500 are here
all_providers <- all_providers %>%
  group_by(Provider.Org.Code) %>%
  summarise(Provider.Org.Name=first(Provider.Org.Name)) %>%
  ungroup()

################################################
################### Load NHS data ##############
################################################

#etrust
etrust <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","etrust.csv") # File to open
                              , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
etrust <- etrust %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#ephpsite
ephpsite <- s3read_using(fread
                       , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","ephpsite.csv") # File to open
                       , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
ephpsite <- ephpsite %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#ephp
ephp <- s3read_using(fread
                       , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","ephp.csv") # File to open
                       , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
ephp <- ephp %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#enonnhs
enonnhs <- s3read_using(fread
                     , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","enonnhs.csv") # File to open
                     , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
enonnhs <- enonnhs %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#ets
ets <- s3read_using(fread
                        , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","ets.csv") # File to open
                        , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
ets <- ets %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#etr
etr <- s3read_using(fread
                    , object = paste0(RTT_subfolder,"/Locating providers/NHS data/","etr.csv") # File to open
                    , bucket = IHT_bucket, header=FALSE) # Bucket name defined above
etr <- etr %>%
  select(V1,V2,V10) %>%
  rename(provider.code=V1,provider.name=V2,pcode=V10)

#Appended
nhs.providers <- plyr::rbind.fill(etrust,ephpsite,ephp,ets,etr) %>%
  distinct(.)

#Merge into provider list
all_providers_with_nhs_loc <- left_join(all_providers,nhs.providers,by=c("Provider.Org.Code"="provider.code"))
#all_providers_with_nhs_loc <- all_providers_with_nhs_loc[sample(1:nrow(all_providers_with_nhs_loc),10,replace=FALSE),]

missing_providers <- all_providers_with_nhs_loc %>%
  filter(.,is.na(pcode)) %>%
  pull(Provider.Org.Code)

#Search for postcodes

search_postcode_google <- function(orgcode,type){
  
  #Provider name and code
  org_pcode <- filter(all_providers_with_nhs_loc,Provider.Org.Code==orgcode) %>%
    head(.,n=1) %>% 
    pull(pcode) %>%
    unlist()
  
  #Carry out search
  search_term <- paste0(org_pcode," in England")
  
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
                                     Provider.Postcode=as.character(org_pcode),
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

search_postcode_google_catch <- function(orgcode,type){
  tryCatch(
    {
      return(search_postcode_google(orgcode,type))
    },
    error=function(error_message) {
      
      #Results in API fails
      search_results_table_fail <- data.frame(Provider.Org.Code=as.character(orgcode),
                                              Provider.Org.Pcode=as.character(NA),
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

#Run this function on a a loop - will take about 5min

pcode.args <- all_providers_with_nhs_loc$Provider.Org.Code

out.search.pcodes <- pbmapply(search_postcode_google_catch,
                                 orgcode=pcode.args,
                                 MoreArgs = list(type="none"))
out.search.pcodes.df <- as.data.frame(t(out.search.pcodes)) %>%
  filter(.,!is.na(Provider.Postcode))
  
#Add those that weren't in NHS database
#This uses a search based on provider names (see next section of code)

RTT_google_locations <- s3read_using(fread
                    , object = paste0(RTT_subfolder,"/Locating providers/RTT provider locations from Google Maps.csv") # File to open
                    , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

RTT_google_locations <- RTT_google_locations %>%
  filter(.,Provider.Org.Code %in% missing_providers) %>%
  select(.,-"Provider.Org.Name")

out.search.pcodes.df <- plyr::rbind.fill(out.search.pcodes.df,RTT_google_locations) %>%
  apply(.,2,as.character)

#Save

s3write_using(out.search.pcodes.df # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Locating providers/","Google_NHS_postcodes.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#################################################################
################### Function to return coordinates ##############
#################################################################

#We only used this for the 20 or so providers that weren't included in the NHS lookups
#and therefore didn't already have a postcode

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

##############################################################
################### Map locations to MSOA ####################
##############################################################

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Import MSOA shapefile
#Source: https://www.data.gov.uk/dataset/f06cd2d7-92df-4dbb-a82d-c4b86412dec4/middle-layer-super-output-areas-december-2011-generalised-clipped-boundaries-in-england-and-wales

setwd(paste0(R_workbench,"/Shapefiles/MSOA"))
MSOA_shapefile <- readOGR(dsn=".", layer="Middle_Layer_Super_Output_Areas_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales") 
MSOA_shapefile <- spTransform(MSOA_shapefile, CRS(latlong))

#Create shapefile with provider data

RTT_provider_locations <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Locating providers/","Google_NHS_postcodes.csv") # File to open
                                       , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

RTT_providers_shapefile <- SpatialPointsDataFrame(cbind(RTT_provider_locations$long,
                                                        RTT_provider_locations$lat),
                                                  data=RTT_provider_locations,
                                                  proj4string = CRS(latlong))

#Overlay points over MSOAs

plot(MSOA_shapefile)
points(RTT_providers_shapefile,col="red")
providers_over_MSOA <- over(RTT_providers_shapefile,MSOA_shapefile)
rm(RTT_providers_shapefile,MSOA_shapefile)

#Merge this back into main provider file

RTT_provider_locations <- cbind(RTT_provider_locations,
                                select(providers_over_MSOA,msoa11cd,msoa11nm))
rm(providers_over_MSOA)

#Load IMD by MSOA lookup
#From https://research.mysociety.org/sites/imd2019/about/

MSOA_to_IMD19 <- s3read_using(fread, object = paste0(RTT_subfolder,"/IMD 2019/","imd2019_msoa_level_data.csv"),
                             bucket = IHT_bucket, header=TRUE)

# MSOA_to_IMD19 %>% 
#   janitor::tabyl(MSOAQUINTILE)
  
MSOA_to_IMD19 <- MSOA_to_IMD19 %>%
  select(.,MSOAC,MSOADECILE,MSOAQUINTILE,REG) %>%
  rename(.,msoa11cd=MSOAC,IMD19_decile=MSOADECILE,IMD19_quintile=MSOAQUINTILE,region=REG)
  
##########################################################
################### All-in-one lookup ####################
##########################################################

#In a single lookup
provider_to_IMD_region <- RTT_provider_locations %>%
  select(.,Provider.Org.Code,msoa11cd) %>%
  left_join(.,MSOA_to_IMD19,by="msoa11cd")

#Save
s3write_using(provider_to_IMD_region # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Custom RTT lookups/","provider_to_IMD_region.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above
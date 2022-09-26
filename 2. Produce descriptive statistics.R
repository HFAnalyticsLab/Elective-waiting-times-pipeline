##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Proportion 52 weeks or more, why 0% before ~March 2021?
#Chart chart of IS/non-IS by specialty?
#There are probably faster ways to reproduce dashboard metrics than using custom-written functions

##############################################
################### SETUP ####################
##############################################

###### Libraries ######

#Some of these might not be needed
library(tidyverse)
library(stringr)
library(tidyr)
library(purrr)
library(pbapply)
library(data.table)
library(readr)
library(readxl)
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Directories in S3
source('setup.R') #get project locations in s3 and working directory

# IHT_bucket: s3 project bucket
# RTT_subfolder: folder to place data
# R_workbench: R server working directory

#Useful functions
only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }
sumnarm <- function(x) { sum(x,na.rm=TRUE) }
not_all_na <- function(x) any(!is.na(x))

#######################################################################################
################### Import CCG-level deprivation data and region lookups ##############
#######################################################################################

# IMD_by_CCG_wide <- s3read_using(fread
#                               , object = paste0(RTT_subfolder,"/Custom RTT lookups/","IMD_by_CCG_wide.csv") # File to open
#                               , bucket = IHT_bucket) # Bucket name defined above
# 
# Region_by_CCG_wide <- s3read_using(fread
#                                 , object = paste0(RTT_subfolder,"/Custom RTT lookups/","CCG_NHSER_joined_wide.csv") # File to open
#                                 , bucket = IHT_bucket) # Bucket name defined above

provider_to_IMD_region <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Custom RTT lookups/","provider_to_IMD_region.csv") # File to open
                                       , bucket = IHT_bucket) # Bucket name defined above

#############################################################
################### Import monthly RTT data #################
#############################################################

RTT_allmonths <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/","RTT_allmonths_new.csv") # File to open
                              , bucket = IHT_bucket) # Bucket name defined above

##########################################################
################### Create new variables #################
##########################################################

#### Merge in provider lookup

RTT_allmonths <- RTT_allmonths %>% 
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code")
  
#### Create pathways variable

RTT_allmonths <- RTT_allmonths %>%
  mutate(.,pathways=case_when(
    RTT.Part.Description=="Incomplete Pathways" ~ "incomplete",
    RTT.Part.Description=="Completed Pathways For Admitted Patients" ~ "completeadmitted",
    RTT.Part.Description=="Completed Pathways For Non-Admitted Patients" ~ "completenonadmitted",
    RTT.Part.Description=="Incomplete Pathways with DTA" ~ "incompleteDTA",
    RTT.Part.Description=="New RTT Periods - All Patients" ~ "newRTT",
    TRUE ~ "NA"
  ))

#### Clean up specialty names

RTT_allmonths <- RTT_allmonths %>%
  mutate(.,Treatment.Function.Name=str_replace_all(Treatment.Function.Name," Service","")) %>%
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Ear, Nose & Throat (ENT)","Ear Nose and Throat",Treatment.Function.Name)) %>%
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Geriatric Medicine","Elderly Medicine",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Neurosurgical","Neurosurgery",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Trauma & Orthopaedics","Trauma and Orthopaedic",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Medicals","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Mental Healths","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Others","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Paediatrics","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Surgicals","Other",Treatment.Function.Name))

#### COVID-period (takes a long time to run)

#### Capture names of providers and specialties

all_months <- RTT_allmonths %>%
  pull(monthyr) %>%
  unique(.)

all_providers <- RTT_allmonths %>%
  pull(Provider.Org.Name) %>%
  unique(.) %>%
  c(.,"ENGLAND")

all_providers_trusts <- all_providers %>%
  data.frame() %>% rename(.,all_providers_trusts=".") %>% 
  filter(.,str_detect(all_providers_trusts, "TRUST")) %>%
  pull() %>% c(.,"ENGLAND")
  
all_specialties <- RTT_allmonths %>%
  pull(Treatment.Function.Name) %>%
  unique(.)

all_ccgs <-  RTT_allmonths %>%
  filter(.,!(Commissioner.Org.Code %in% c("","NONC"))) %>% 
  pull(Commissioner.Org.Code) %>%
  unique(.) %>% c(.,"ENGLAND")

pathways <- c("incomplete","completeadmitted","completenonadmitted","newRTT","incompleteDTA")

###############################################################################
################### Reproduce dashboard metrics with functions ################
###############################################################################

############### By provider

dashboard_stats_provider <- function(monthyear,provider,specialty,quantiles,type){
  
  # monthyear="Dec18"
  # provider="ENGLAND"
  # specialty="Total"
  # quantiles=c(0.50)
  # type="incomplete"
  
  #Pick relevant month-year
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear)
  
  #Which wait-time columns are filled in?
  
  GT_names <- dataset %>%
    dplyr::select(starts_with("Gt")) %>%
    dplyr::select(where(not_all_na)) %>%
    names(.)
    
  #If provider is England, use all rows and all providers
  
  dataset$Provider.Org.Name <- ifelse(rep(provider,nrow(dataset))=="ENGLAND",
                                      rep("ENGLAND",nrow(dataset)),
                                      dataset$Provider.Org.Name)
  
  #Aggregate relevant variables based on type of pathway
  
  if (type=="incomplete"){
    datasubset <- filter(dataset,Provider.Org.Name==provider&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways"&
                           Commissioner.Org.Code!="NONC")
  } else if (type=="completeadmitted"){
    datasubset <- filter(dataset,Provider.Org.Name==provider&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients"&
                           Commissioner.Org.Code!="NONC")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(dataset,Provider.Org.Name==provider&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients"&
                           Commissioner.Org.Code!="NONC")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,Provider.Org.Name==provider&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA"&
                           Commissioner.Org.Code!="NONC")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,Provider.Org.Name==provider&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients"&
                           Commissioner.Org.Code!="NONC")
  }
  
  #Is it an IS provider (that month)?
  
  IS_provider <- ifelse(provider=="ENGLAND",
                        0,
                        max(datasubset$IS_provider))
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    #For complete pathways, reported totals include those with unknown start date
    #But medians (and other stats) are computed using only the subset of patients
    #with a known start state
    
    datasubset_sum <-  datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(dplyr::across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    #For all incomplete pathways (waiting to start), reported totals omit those with unknown start date
    #AND medians (and other stats) are also computed using only the subset of patients
    #with a known start state
    
    datasubset_sum <-  datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
    
  } else if (type=="newRTT"){
    
    #For New RTT (clock start this month) there are no data points in the waiting times
    #columns
    
    total <- datasubset %>% dplyr::select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #and the pathway isn't New RTT
  
  if (total.nonmiss>=20&type!="newRTT"){
    
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:nrow(datasubset_sum),counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% dplyr::select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_less <- datasubset_sum[1:52,] %>% sum(.,na.rm=TRUE) %>%  unlist()
    number_52_or_more <- total.nonmiss - number_52_or_less
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         provider=as.character(provider),
                         IS=as.character(IS_provider),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         total.patients=total,
                         number.18.or.less=number_18_or_less,
                         rate.18wks.or.less=rate_18_or_less,
                         number.52.or.more=number_52_or_more,
                         rate.52wks.or.more=rate_52_or_more,
                         weeks)
    
  } else if (total.nonmiss<20|type=="newRTT") {
    
    #Do not return descriptive statistics if there are fewer than 20 patients
    
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         provider=as.character(provider),
                         IS=as.character(IS_provider),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         total.patients=total,
                         number.18.or.less=NA,
                         rate.18wks.or.less=NA,
                         number.52.or.more=NA,
                         rate.52wks.or.more=NA,
                         weeks)
  }
  
  return(output)
}

#Example
# dashboard_stats_provider(monthyear="Feb22",
#                          provider="ENGLAND",
#                          specialty="Total",
#                          quantiles=c(0.50,0.95),
#                          type="incomplete")

############### By CCG

#Independent sector: 0 non-IS, 1 IS and 2 All

dashboard_stats_ccg <- function(monthyear,ccg_code,specialty,quantiles,type,independent){
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
  #Which wait-time columns are filled in?
  
  GT_names <- dataset %>%
    dplyr::select(starts_with("Gt")) %>%
    dplyr::select(where(not_all_na)) %>%
    names(.)
  
  #If CCG is England, use all rows and all CCGs
  
  dataset$Commissioner.Org.Code <- ifelse(rep(ccg_code,nrow(dataset))=="ENGLAND",
                                          rep("ENGLAND",nrow(dataset)),
                                          dataset$Commissioner.Org.Code)
  
  #Fetch provider name (assume it doesn't vary within same month)
  
  if (ccg_code=="ENGLAND"){
    ccg_name <- "ENGLAND"
  } else {
    ccg_name <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code==ccg_code) %>%
      dplyr::select(.,Commissioner.Org.Name) %>% unique(.) %>% unlist(.) %>% first(.)
  }
  
  #Filter based on type of provider
  
  if (independent==0){
    monthly_data <- filter(dataset,IS_provider==0)
    IS <- "Non-IS"
  } else if (independent==1) {
    monthly_data <- filter(dataset,IS_provider==1)
    IS <- "IS"
  } else if (independent==2) {
    monthly_data <- dataset
    IS <- "All"
  }
  
  #Aggregate relevant variables
  
  if (type=="incomplete"){
    datasubset <- filter(monthly_data,Commissioner.Org.Code==ccg_code&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,Commissioner.Org.Code==ccg_code&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,Commissioner.Org.Code==ccg_code&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,Commissioner.Org.Code==ccg_code&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,Commissioner.Org.Code==ccg_code&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% dplyr::select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:nrow(datasubset_sum),counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% dplyr::select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_less <- datasubset_sum[1:52,] %>% sum(.,na.rm=TRUE) %>%  unlist()
    number_52_or_more <- total.nonmiss - number_52_or_less
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         ccg=as.character(ccg_code),
                         ccg_name=as.character(ccg_name),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=number_18_or_less,
                         rate.18wks.or.less=rate_18_or_less,
                         number.52.or.more=number_52_or_more,
                         rate.52wks.or.more=rate_52_or_more,
                         weeks)
  } else if (total.nonmiss<20|type=="newRTT") {
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         ccg=as.character(ccg_code),
                         ccg_name=as.character(ccg_name),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=NA,
                         rate.18wks.or.less=NA,
                         number.52.or.more=NA,
                         rate.52wks.or.more=NA,
                         weeks)
  }
  
  return(output)
}

#Example
# dashboard_stats_ccg(monthyear="Feb22",
#                     ccg_code="ENGLAND",
#                     specialty="Total",
#                     quantiles=c(0.50,0.92),
#                     type="incomplete",
#                     independent=2)

############### By Region

#Independent sector: 0 non-IS, 1 IS and 2 All

dashboard_stats_region <- function(monthyear,regionname,specialty,quantiles,type,independent){
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
  #Which wait-time columns are filled in?
  
  GT_names <- dataset %>%
    dplyr::select(starts_with("Gt")) %>%
    dplyr::select(where(not_all_na)) %>%
    names(.)
  
  #If regionname is England, use all rows and all regionname
  
  dataset$region <- ifelse(rep(regionname,nrow(dataset))=="ENGLAND",
                                          rep("ENGLAND",nrow(dataset)),
                                          dataset$region)
  
  #Filter based on type of provider
  
  if (independent==0){
    monthly_data <- filter(dataset,IS_provider==0)
    IS <- "Non-IS"
  } else if (independent==1) {
    monthly_data <- filter(dataset,IS_provider==1)
    IS <- "IS"
  } else if (independent==2) {
    monthly_data <- dataset
    IS <- "All"
  }
  
  #Aggregate relevant variables
  
  if (type=="incomplete"){
    datasubset <- filter(monthly_data,region==regionname&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,region==regionname&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,region==regionname&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,region==regionname&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,region==regionname&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% dplyr::select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:nrow(datasubset_sum),counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% dplyr::select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_less <- datasubset_sum[1:52,] %>% sum(.,na.rm=TRUE) %>%  unlist()
    number_52_or_more <- total.nonmiss - number_52_or_less
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         regionname=as.character(regionname),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=number_18_or_less,
                         rate.18wks.or.less=rate_18_or_less,
                         number.52.or.more=number_52_or_more,
                         rate.52wks.or.more=rate_52_or_more,
                         weeks)
  } else if (total.nonmiss<20|type=="newRTT") {
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         regionname=as.character(regionname),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=NA,
                         rate.18wks.or.less=NA,
                         number.52.or.more=NA,
                         rate.52wks.or.more=NA,
                         weeks)
  }
  
  return(output)
}

#Example
# dashboard_stats_region(monthyear="Feb22",
#                        regionname="London",
#                     specialty="Total",
#                     quantiles=c(0.50,0.92),
#                     type="incomplete",
#                     independent=2)

############### By IMD quintile

#Independent sector: 0 non-IS, 1 IS and 2 All

dashboard_stats_imd_quintile <- function(monthyear,imd,specialty,quantiles,type,independent){
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
  #Which wait-time columns are filled in?
  
  GT_names <- dataset %>%
    dplyr::select(starts_with("Gt")) %>%
    dplyr::select(where(not_all_na)) %>%
    names(.)
  
  #If regionname is England, use all rows and all regionname
  
  dataset$IMD19_quintile <- ifelse(rep(imd,nrow(dataset))=="ENGLAND",
                           rep("ENGLAND",nrow(dataset)),
                           dataset$IMD19_quintile)
  
  #Filter based on type of provider
  
  if (independent==0){
    monthly_data <- filter(dataset,IS_provider==0)
    IS <- "Non-IS"
  } else if (independent==1) {
    monthly_data <- filter(dataset,IS_provider==1)
    IS <- "IS"
  } else if (independent==2) {
    monthly_data <- dataset
    IS <- "All"
  }
  
  #Aggregate relevant variables
  
  if (type=="incomplete"){
    datasubset <- filter(monthly_data,IMD19_quintile==imd&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,IMD19_quintile==imd&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,IMD19_quintile==imd&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,IMD19_quintile==imd&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,IMD19_quintile==imd&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>%
      dplyr::select(GT_names) %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% dplyr::select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:nrow(datasubset_sum),counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% dplyr::select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_less <- datasubset_sum[1:52,] %>% sum(.,na.rm=TRUE) %>%  unlist()
    number_52_or_more <- total.nonmiss - number_52_or_less
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         imd_quintile=as.character(imd),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=number_18_or_less,
                         rate.18wks.or.less=rate_18_or_less,
                         number.52.or.more=number_52_or_more,
                         rate.52wks.or.more=rate_52_or_more,
                         weeks)
  } else if (total.nonmiss<20|type=="newRTT") {
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         imd_quintile=as.character(imd),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         number.18.or.less=NA,
                         rate.18wks.or.less=NA,
                         number.52.or.more=NA,
                         rate.52wks.or.more=NA,
                         weeks)
  }
  
  return(output)
}

#Example
# dashboard_stats_imd_quintile(monthyear="Feb22",
#                        imd="5",
#                        specialty="Total",
#                        quantiles=c(0.50,0.92),
#                        type="incomplete",
#                        independent=2)
##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Cannot QA those pathways that are not part of the dashboard?

#Don't run if combination with pathway doesn't exist?

#Removing commsionner code NONC removes private patients
#This is what the dashboard does

#check that Totals are not just totals of trusts

#QA of code and approach
#Does all deprivation data merge? (No, e.g. Cheshire)
#Do totals for England in CCG-level statistics match totals from dashboard?

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,DescTools,data.table,rgdal,
               tibble,leaflet,raster,plotly,
               pbapply,pbmcapply,here,readxl,varhandle,
               openxlsx)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved

#rawdatadir <- "/Users/sgpeytrignet/Documents"
rawdatadir <- "M:/Analytics/Elective waiting times data"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Useful functions
only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }
sumnarm <- function(x) { sum(x,na.rm=TRUE) }

###################################################################################
################### Import CCG-level deprivation data and geo lookup ##############
###################################################################################

CCG_to_IMD19_data <- fread(paste0(rawdatadir,"/Clean/CCG_to_IMD19_data.csv"), header=TRUE, sep=",", check.names=T)

#############################################################
################### Import monthly RTT data #################
#############################################################

RTT_allmonths <- fread(paste0(rawdatadir,"/Clean/RTT_allmonths.csv"), header=TRUE, sep=",", check.names=T)

##### Identify commissioners from data that don't map to IMD or region
##### (e.g. commissionning hubs)

all_commisioners <- select(RTT_allmonths,Commissioner.Org.Name,Commissioner.Org.Code) %>%
  distinct(.)

ccgs_with_imd <- filter(CCG_to_IMD19_data,!is.na(IMD19_decile)) %>% select(.,CCG19CDH) %>% unique()

unmatched_ccgs <- filter(RTT_allmonths,(Commissioner.Org.Code %in% unlist(ccgs_with_imd))==FALSE) %>%
  select(.,Commissioner.Org.Name,Commissioner.Org.Code) %>%
  distinct(.)
  
#NHS_regions <- data.frame(NHSER19NM=unique(CCG_to_IMD19_data$NHSER19NM))
# fwrite(unmatched_ccgs, file = paste0(rawdatadir,"/Clean/unmatched_ccgs.csv"), sep = ",")
# fwrite(NHS_regions, file = paste0(rawdatadir,"/Clean/NHS_regions.csv"), sep = ",")

##### Merge in location and deprivation

RTT_allmonths <- left_join(RTT_allmonths,select(CCG_to_IMD19_data,CCG19CDH,IMD19_decile,IMD19_quintile,NHSER19NM),
                           by=c("Commissioner.Org.Code"="CCG19CDH"))

#### Create pathways variable

RTT_allmonths$pathways <- mapvalues(RTT_allmonths$RTT.Part.Description,
                                    from = c("Incomplete Pathways",
                                             "Completed Pathways For Admitted Patients",
                                             "Completed Pathways For Non-Admitted Patients",
                                             "Incomplete Pathways with DTA",
                                             "New RTT Periods - All Patients"),
                                    to = c("incomplete",
                                           "completeadmitted",
                                           "completenonadmitted",
                                           "incompleteDTA",
                                           "newRTT"))

#### Capture names of providers and specialties

all_months <- unique(RTT_allmonths$monthyr)
all_providers <- unique(RTT_allmonths$Provider.Org.Name) %>% c(.,"ENGLAND")
providers_trusts <- all_providers[str_detect(all_providers, "TRUST")] %>% c(.,"ENGLAND")
all_specialties <- unique(RTT_allmonths$Treatment.Function.Name)
all_ccgs <- unique(RTT_allmonths$Commissioner.Org.Code[-which(RTT_allmonths$Commissioner.Org.Code==""|RTT_allmonths$Commissioner.Org.Code=="NONC")]) %>%
  c(.,"ENGLAND")
pathways <- c("incomplete","completeadmitted","completenonadmitted","newRTT","incompleteDTA")
regions <- unique(RTT_allmonths$NHSER19NM[-which(is.na(RTT_allmonths$NHSER19NM))])

###############################################################################
################### Reproduce dashboard metrics with functions ################
###############################################################################

############### By provider

#Select ENGLAND as Provider name for England as summary
#Select Total for all specialties within a single provider

#Types are:

#newRTT **NEW**
#incomplete
#incompleteDTA **NEW**
#completeadmitted
#completenonadmitted

return_week_lower <- function(monthyear,provider,specialty,quantiles,type){

# monthyear <- "Dec18"
# provider <- "LANCASHIRE CARE NHS FOUNDATION TRUST"
# specialty <- "Total"
# quantiles <- c(0.95)
# type <- "completeadmitted"
  
  #Pick relevant month-year
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear)
  
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
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    #For complete pathways, reported totals include those with unknown start date
    #But medians (and other stats) are computed using only the subset of patients
    #with a known start state
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(dplyr::across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    #For all incomplete pathways (waiting to start), reported totals omit those with unknown start date
    #AND medians (and other stats) are also computed using only the subset of patients
    #with a known start state

    datasubset_sum <- datasubset %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
    
  } else if (type=="newRTT"){
    
    #For New RTT (clock start this month) there are no data points in the waiting times
    #columns
    
    total <- datasubset %>% select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #and the pathway isn't New RTT
  
  if (total.nonmiss>=20&type!="newRTT"){
    
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:53,counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_more <- datasubset_sum[53] %>% unlist()
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         provider=as.character(provider),
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

#This version of the function makes sure it still produces a result
#if anything goes wrong

return_week_lower_catch <- function(monthyear,provider,specialty,quantiles,type){
  tryCatch(
    {
      return(return_week_lower(monthyear,provider,specialty,quantiles,type))
    },
    error=function(error_message) {
      
      output <- data.frame(monthyear=as.character(monthyear),
                           provider=as.character(provider),
                           specialty=as.character(specialty),
                           type=as.character(type),
                           total.patients=NA,
                           number.18.or.less=NA,
                           rate.18wks.or.less=NA,
                           number.52.or.more=NA,
                           rate.52wks.or.more=NA)
      
      weeks <- data.frame(rep(NA,length(quantiles))) %>% t()
      colnames(weeks) <- paste0("weeks.",quantiles*100)
      output <- cbind.data.frame(output,weeks)
      rownames(output) <- 1
      
      return(output)
    }
  )
}

# return_week_lower_catch(monthyear="Dec18",
#                   provider="LANCASHIRE CARE NHS FOUNDATION TRUST",
#                   specialty="Total",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="completeadmitted")

# return_week_lower_catch(monthyear="Jun20",
#                   provider="ENGLAND",
#                   specialty="Dermatology",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="completenonadmitted")

############### By CCG

#Independent: 0 non-IS, 1 IS and 2 All

return_week_lower_ccg <- function(monthyear,ccg_code,specialty,quantiles,type,independent){
  
  # monthyear <- "Aug20"
  # ccg_code <- "10C"
  # specialty <- "Total"
  # quantiles <- c(0.50)
  # type <- "completenonadmitted"
  # independent <- 2
  
  #Fetch deprivation values and region from lookup
  
  IMD19_decile <- ifelse(length(filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$IMD19_decile)>0,
                         filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$IMD19_decile,NA)
  IMD19_quintile <- ifelse(length(filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$IMD19_quintile)>0,
                           filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$IMD19_quintile,NA)
  NHSER19NM <- ifelse(length(filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$NHSER19NM)>0,
                      filter(CCG_to_IMD19_data,CCG19CDH==ccg_code)$NHSER19NM,NA)
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
  #If CCG is England, use all rows and all CCGs
  
  dataset$Commissioner.Org.Code <- ifelse(rep(ccg_code,nrow(dataset))=="ENGLAND",
                                          rep("ENGLAND",nrow(dataset)),
                                          dataset$Commissioner.Org.Code)
  
  #Fetch provider name (assume it doesn't vary within same month)
  
  if (ccg_code=="ENGLAND"){
    ccg_name <- "ENGLAND"
  } else {
    ccg_name <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code==ccg_code) %>%
      select(.,Commissioner.Org.Name) %>% unique(.) %>% unlist(.) %>% first(.)
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
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:53,counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_more <- datasubset_sum[53] %>% unlist()
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
                         weeks,
                         IMD19_decile=as.character(IMD19_decile),
                         IMD19_quintile=as.character(IMD19_quintile),
                         NHSER19NM=as.character(NHSER19NM))
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
                         weeks,
                         IMD19_decile=as.character(IMD19_decile),
                         IMD19_quintile=as.character(IMD19_quintile),
                         NHSER19NM=as.character(NHSER19NM))
  }
  
  return(output)
}

#This version of the function makes sure it still produces a result
#if anything goes wrong

return_week_lower_ccg_catch <- function(monthyear,ccg_code,specialty,quantiles,type,independent){
  tryCatch(
    {
      return(return_week_lower_ccg(monthyear,ccg_code,specialty,quantiles,type,independent))
    },
    error=function(error_message) {
      
      output <- data.frame(monthyear=as.character(monthyear),
                           ccg=as.character(ccg_code),
                           ccg_name=as.character(ccg_name),
                           specialty=as.character(specialty),
                           type=as.character(type),
                           independent=NA,
                           total.patients=NA,
                           number.18.or.less=NA,
                           rate.18wks.or.less=NA,
                           number.52.or.more=NA,
                           rate.52wks.or.more=NA)
      
      weeks <- data.frame(rep(NA,length(quantiles))) %>% t()
      colnames(weeks) <- paste0("weeks.",quantiles*100)
      output <- cbind.data.frame(output,weeks,IMD19_decile=NA,
                                 IMD19_quintile=NA,
                                 NHSER19NM=NA)
      rownames(output) <- 1
      
      return(output)
    }
  )
}

# return_week_lower_ccg_catch(monthyear="Jun20",
#                       ccg="NHS CENTRAL LONDON (WESTMINSTER) CCG",
#                       specialty="Total",quantiles=c(0.5,0.92,0.95),
#                       type="incomplete",independent=2)

# return_week_lower_ccg_catch(monthyear="Aug20",
#                       ccg="ENGLAND",
#                       specialty="Total",quantiles=c(0.5,0.92,0.95),
#                       type="newRTT",independent=2)

############### By region

return_week_lower_region <- function(monthyear,region_name,specialty,quantiles,type,independent){
  
  # monthyear <- "Aug20"
  # ccg_code <- "10C"
  # specialty <- "Total"
  # quantiles <- c(0.50)
  # type <- "completenonadmitted"
  # independent <- 2
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
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
    datasubset <- filter(monthly_data,NHSER19NM==region_name&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,NHSER19NM==region_name&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,NHSER19NM==region_name&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,NHSER19NM==region_name&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,NHSER19NM==region_name&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:53,counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_more <- datasubset_sum[53] %>% unlist()
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         region_name=as.character(region_name),
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
                         region_name=as.character(region_name),
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

return_week_lower_region_catch <- function(monthyear,region_name,specialty,quantiles,type,independent){
  tryCatch(
    {
      return(return_week_lower_region(monthyear,region_name,specialty,quantiles,type,independent))
    },
    error=function(error_message) {
      
      output <- data.frame(monthyear=as.character(monthyear),
                           region_name=as.character(region_name),
                           specialty=as.character(specialty),
                           type=as.character(type),
                           independent=NA,
                           total.patients=NA,
                           number.18.or.less=NA,
                           rate.18wks.or.less=NA,
                           number.52.or.more=NA,
                           rate.52wks.or.more=NA)
      
      weeks <- data.frame(rep(NA,length(quantiles))) %>% t()
      colnames(weeks) <- paste0("weeks.",quantiles*100)
      output <- cbind.data.frame(output,weeks)
      rownames(output) <- 1
      
      return(output)
    }
  )
}

# return_week_lower_region(monthyear="Dec18",
#                       region_name="South West",
#                       specialty="Oral Surgery",quantiles=c(0.5,0.92,0.95),
#                       type="completeadmitted",independent=1)

############### By deprivation decile

return_week_lower_dep <- function(monthyear,decile,specialty,quantiles,type,independent){
  
  # monthyear <- "Aug20"
  # ccg_code <- "10C"
  # specialty <- "Total"
  # quantiles <- c(0.50)
  # type <- "completenonadmitted"
  # independent <- 2
  
  #Pick relevant month-year and filter out private patients
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear&Commissioner.Org.Code!="NONC")
  
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
    datasubset <- filter(monthly_data,IMD19_decile==decile&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,IMD19_decile==decile&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,IMD19_decile==decile&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  } else if (type=="incompleteDTA"){
    datasubset <- filter(dataset,IMD19_decile==decile&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways with DTA")
  } else if (type=="newRTT"){
    datasubset <- filter(dataset,IMD19_decile==decile&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="New RTT Periods - All Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete"|type=="incompleteDTA") {
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sumnarm)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  } else if (type=="newRTT"){
    
    total <- datasubset %>% select(.,Total.All) %>% sum(.,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
  
  if (total.nonmiss>=20&type!="newRTT"){
    #Return weeks (for a given quantile)
    
    weeks <- rep(NA,length(quantiles))
    
    for (j in 1:length(quantiles)){
      
      target <- quantiles[j]*total.nonmiss
      
      auxmat <- data.frame(weeks=1:53,counts=datasubset_sum,
                           cumsum=cumsum(datasubset_sum)) %>%
        mutate(.,above=ifelse(cumsum>=target,1,0),
               difference=(cumsum-target))
      
      weeks[j] <- (filter(auxmat,above==1) %>% select(.,weeks) %>% min())-1
    }
    
    weeks <- as.data.frame(weeks) %>% t()
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    #Return % of patients waiting 18 weeks or less
    
    number_52_or_more <- datasubset_sum[53] %>% unlist()
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         decile=as.character(decile),
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
                         decile=as.character(decile),
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

return_week_lower_dep_catch <- function(monthyear,decile,specialty,quantiles,type,independent){
  tryCatch(
    {
      return(return_week_lower_dep(monthyear,decile,specialty,quantiles,type,independent))
    },
    error=function(error_message) {
      
      output <- data.frame(monthyear=as.character(monthyear),
                           decile=as.character(decile),
                           specialty=as.character(specialty),
                           type=as.character(type),
                           independent=NA,
                           total.patients=NA,
                           number.18.or.less=NA,
                           rate.18wks.or.less=NA,
                           number.52.or.more=NA,
                           rate.52wks.or.more=NA)
      
      weeks <- data.frame(rep(NA,length(quantiles))) %>% t()
      colnames(weeks) <- paste0("weeks.",quantiles*100)
      output <- cbind.data.frame(output,weeks)
      rownames(output) <- 1
      
      return(output)
    }
  )
}

# return_week_lower_dep_catch(monthyear="Aug20",
#                       decile=5,
#                       specialty="Total",quantiles=c(0.5,0.92,0.95),
#                       type="incomplete",independent=2)

########################################################################################
################### Compute descriptive statistics for all combinations ################
########################################################################################

# ############### Create combinations for providers
# 
# combinations <- expand.grid(all_months,
#                                     pathways,
#                                     all_providers,all_specialties) %>% varhandle::unfactor()
# names(combinations) <- c("monthyr","pathways","Provider.Org.Name","Treatment.Function.Name")
# 
# observed_combinations <- paste(RTT_allmonths$monthyr,
#                                RTT_allmonths$Provider.Org.Name,
#                                RTT_allmonths$Treatment.Function.Name,
#                                RTT_allmonths$pathways,sep=" ")
# 
# combinations <- filter(combinations, Provider.Org.Name=="ENGLAND" |
# paste(combinations$monthyr,combinations$Provider.Org.Name,
#       combinations$Treatment.Function.Name,
#       combinations$pathways,sep=" ") %in% observed_combinations)
# 
# ############### File 1: National and by specialty
# 
# combinations.one <- filter(combinations,Provider.Org.Name=="ENGLAND")
# 
# #combinations.one <- combinations.one[sample(1:nrow(combinations.one),10),]
# 
# out.combinations.one <- pbmapply(return_week_lower_catch,
#                                  monthyear=combinations.one$monthyr,
#                                  type=combinations.one$pathways,
#                                  provider=combinations.one$Provider.Org.Name,
#                                  specialty=combinations.one$Treatment.Function.Name,
#                                  MoreArgs = list(quantiles=c(0.5,0.92,0.95)))
# 
# out.combinations.one.df <- as.data.frame(t(out.combinations.one))
# rownames(out.combinations.one.df) <- 1:nrow(out.combinations.one.df)
# rm(combinations.one,out.combinations.one)
# 
# #Clean up dates for Excel
# out.combinations.one.df$month <- only_letters(out.combinations.one.df$monthyear)
# out.combinations.one.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.one.df$monthyear))
# out.combinations.one.df$monthyear <- NULL
# 
# ############### File 2: National and by trust, combined specialties
# 
# combinations.two <- filter(combinations,Provider.Org.Name %in% providers_trusts) %>%
#   filter(.,Treatment.Function.Name=="Total")
# 
# #combinations.two <- combinations.two[sample(1:nrow(combinations.two),10),]
# 
# out.combinations.two <- pbmapply(return_week_lower_catch,
#                                  monthyear=combinations.two$monthyr,
#                                  type=combinations.two$pathways,
#                                  provider=combinations.two$Provider.Org.Name,
#                                  specialty=combinations.two$Treatment.Function.Name,
#                                  MoreArgs = list(quantiles=c(0.5,0.92,0.95)))
# 
# out.combinations.two.df <- as.data.frame(t(out.combinations.two))
# rownames(out.combinations.two.df) <- 1:nrow(out.combinations.two.df)
# rm(combinations.two,out.combinations.two)
# 
# #Clean up dates for Excel
# out.combinations.two.df$month <- only_letters(out.combinations.two.df$monthyear)
# out.combinations.two.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.two.df$monthyear))
# out.combinations.two.df$monthyear <- NULL
# 
# ############### By CCG
# 
# ############### Create combinations for CCGs
# 
# providertypes <- 0:2
# 
# combinations.ccg <- expand.grid(all_months,
#                             pathways,
#                             all_ccgs,all_specialties,providertypes) %>% varhandle::unfactor()
# names(combinations.ccg) <- c("monthyr","pathways","Commissioner.Org.Code","Treatment.Function.Name","providertypes")
# 
# observed_combinations.ccg <- paste(RTT_allmonths$monthyr,
#                                    RTT_allmonths$Commissioner.Org.Code,
#                                    RTT_allmonths$Treatment.Function.Name,
#                                    RTT_allmonths$pathways,sep=" ")
# 
# combinations.ccg <- filter(combinations.ccg, Commissioner.Org.Code=="ENGLAND" |
#                          paste(combinations.ccg$monthyr,combinations.ccg$Commissioner.Org.Code,
#                                combinations.ccg$Treatment.Function.Name,
#                                combinations.ccg$pathways,sep=" ") %in% observed_combinations.ccg)
# 
# ############### File 3: National and by CCG, all specialties
# 
# combinations.three <- filter(combinations.ccg,(Commissioner.Org.Code %in% all_ccgs)&
#                                Commissioner.Org.Code=="ENGLAND")
# 
# #combinations.three <- combinations.three[sample(1:nrow(combinations.three),10),]
# 
# out.combinations.three <- pbmapply(return_week_lower_ccg_catch,
#                                    monthyear=combinations.three$monthyr,
#                                    independent=combinations.three$providertypes,
#                                    type = combinations.three$pathways,
#                                    ccg_code=combinations.three$Commissioner.Org.Code,
#                                    specialty=combinations.three$Treatment.Function.Name,
#                                    MoreArgs = list(quantiles=c(0.5,0.92,0.95)))
# 
# out.combinations.three.df <- as.data.frame(t(out.combinations.three))
# rownames(out.combinations.three.df) <- 1:nrow(out.combinations.three.df)
# rm(combinations.three,out.combinations.three)
# 
# #Clean up dates
# out.combinations.three.df$month <- only_letters(out.combinations.three.df$monthyear)
# out.combinations.three.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.three.df$monthyear))
# out.combinations.three.df$monthyear <- NULL

############### Create combinations for regions

providertypes <- 0:2

combinations.region <- expand.grid(all_months,
                                pathways,
                                regions,all_specialties,providertypes) %>% varhandle::unfactor()
names(combinations.region) <- c("monthyr","pathways","NHSER19NM","Treatment.Function.Name","providertypes")

combinations.region <- filter(combinations.region,Treatment.Function.Name=="Total")

#combinations.region <- combinations.region[sample(1:nrow(combinations.region),10),]

############### File 4: By region, all specialties

out.combinations.four <- pbmapply(return_week_lower_region_catch,
                                   monthyear=combinations.region$monthyr,
                                   independent=combinations.region$providertypes,
                                   type = combinations.region$pathways,
                                   region_name=combinations.region$NHSER19NM,
                                   specialty=combinations.region$Treatment.Function.Name,
                                   MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.four.df <- as.data.frame(t(out.combinations.four))
rownames(out.combinations.four.df) <- 1:nrow(out.combinations.four.df)
rm(combinations.region,out.combinations.four)

#Clean up dates
out.combinations.four.df$month <- only_letters(out.combinations.four.df$monthyear)
out.combinations.four.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.four.df$monthyear))
out.combinations.four.df$monthyear <- NULL

############### Create combinations for deprivation decile

providertypes <- 0:2

deciles <- 1:10

combinations.deciles <- expand.grid(all_months,
                                   pathways,
                                   deciles,all_specialties,providertypes) %>% varhandle::unfactor()
names(combinations.deciles) <- c("monthyr","pathways","IMD19_decile","Treatment.Function.Name","providertypes")

combinations.deciles <- filter(combinations.deciles,Treatment.Function.Name=="Total")

#combinations.deciles <- combinations.deciles[sample(1:nrow(combinations.deciles),10),]

############### File 5: By deprivation, all specialties

out.combinations.five <- pbmapply(return_week_lower_dep_catch,
                                  monthyear=combinations.deciles$monthyr,
                                  independent=combinations.deciles$providertypes,
                                  type = combinations.deciles$pathways,
                                  decile=combinations.deciles$IMD19_decile,
                                  specialty=combinations.deciles$Treatment.Function.Name,
                                  MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.five.df <- as.data.frame(t(out.combinations.five))
rownames(out.combinations.five.df) <- 1:nrow(out.combinations.five.df)
rm(combinations.deciles,out.combinations.five)

#Clean up dates
out.combinations.five.df$month <- only_letters(out.combinations.five.df$monthyear)
out.combinations.five.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.five.df$monthyear))
out.combinations.five.df$monthyear <- NULL

########################################################
################### Save to a worksheet ################
########################################################

# list_of_datasets <- list("National, by specialty" = out.combinations.one.df,
#                          "National and Trust, comb spec" = out.combinations.two.df,
#                          "National, by IS and spec" = out.combinations.three.df,
#                          "Regional, by IS and all spec" = out.combinations.four.df,
#                          "By deprivation, by IS and all" = out.combinations.five.df)

list_of_datasets <- list("Regional, by IS and all spec" = out.combinations.four.df,
                         "By deprivation, by IS and all" = out.combinations.five.df)

write.xlsx(list_of_datasets, file = paste0(rawdatadir,"/Clean/RTT - monthly series summarised 10 15.xlsx"))
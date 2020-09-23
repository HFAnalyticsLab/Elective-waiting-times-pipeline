##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Make sure functions run even it they fail
#Run for total rather than specialty

#Note: Incomplete Pathways with DTA not part of the dashboard statistics

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
               pbapply,pbmcapply,here,readxl,varhandle)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Elective waiting times data"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Useful function
only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }

###################################################################################
################### Import CCG-level deprivation data and geo lookup ##############
###################################################################################

CCG_to_IMD19_data <- fread(paste0(rawdatadir,"/Clean/CCG_to_IMD19_data.csv"), header=TRUE, sep=",", check.names=T)

#############################################################
################### Import monthly RTT data #################
#############################################################

RTT_allmonths <- fread(paste0(rawdatadir,"/Clean/RTT_allmonths.csv"), header=TRUE, sep=",", check.names=T)

##### Merge in location and deprivation

RTT_allmonths <- left_join(RTT_allmonths,select(CCG_to_IMD19_data,ccg19nm,IMD19_decile,IMD19_quintile,NHSER19NM),
                       by=c("Commissioner.Org.Name"="ccg19nm"))

###########################################################################################
################### Reproduce dashboard metrics: Provider-level statistics ################
###########################################################################################

#Select ENGLAND as Provider name for England as summary
#Select Total for all specialties within a single provider

#Types are:
#incomplete
#completeadmitted
#completenonadmitted

return_week_lower <- function(monthyear,provider,specialty,quantiles,type){

  # monthyear <- "Jun20"
  # provider <- "ENGLAND"
  # specialty <- "Other"
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
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sum)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sum)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete") {
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sum)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  
  if (total.nonmiss>=20){
    
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
    
    rate_18_or_less <- round(cumsum(datasubset_sum)[18]/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         provider=as.character(provider),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         total.patients=total,
                         rate.18wks.or.less=rate_18_or_less,
                         weeks)
  } else {
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         provider=as.character(provider),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         total.patients=total,
                         rate.18wks.or.less=NA,
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
                           rate.18wks.or.less=NA)
      
      weeks <- data.frame(rep(NA,length(quantiles))) %>% t()
      colnames(weeks) <- paste0("weeks.",quantiles*100)
      output <- cbind.data.frame(output,weeks)
      rownames(output) <- 1
      
      return(output)
    }
  )
}

# return_week_lower_catch(monthyear="Jul20",
#                   provider="LANCASHIRE & SOUTH CUMBRIA NHS FOUNDATION TRUST",
#                   specialty="Total",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="incomplete")
# 
# return_week_lower_catch(monthyear="Jul20",
#                   provider="ENGLAND",
#                   specialty="Dermatology",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="incomplete")

#FILE 1: Run for all trusts

providers_trusts <- unique(RTT_allmonths$Provider.Org.Name)[str_detect(unique(RTT_allmonths$Provider.Org.Name),"TRUST")]
all_specialties <- unique(RTT_allmonths$Treatment.Function.Name)
all_months <- unique(RTT_allmonths$monthyr)

monthyears <- all_months
pathways <- c("incomplete","completeadmitted","completenonadmitted")
providers <- c(providers_trusts,"ENGLAND")
specialties <- c("Total")

combinations <- expand.grid(monthyears,pathways,providers,specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyears","pathways","providers","specialties")

out.combinations <- pbmapply(return_week_lower_catch,
                           monthyear=combinations$monthyears,
                           type=combinations$pathways,
                           provider=combinations$providers,
                           specialty=combinations$specialties,
       MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.df <- as.data.frame(t(out.combinations))
rownames(out.combinations.df) <- 1:nrow(out.combinations.df)

#Clean up dates
out.combinations.df$month <- only_letters(out.combinations.df$monthyear)
out.combinations.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.df$monthyear))
out.combinations.df$monthyear <- NULL

#Save
setwd(rawdatadir)
fwrite(out.combinations.df, file = paste0(rawdatadir,"/Clean/Monthly - By trust and national.csv"), sep = ",")

#FILE 1: Run for all specialties (national)

monthyears <- all_months
pathways <- c("incomplete","completeadmitted","completenonadmitted")
providers <- c("ENGLAND")
specialties <- all_specialties

combinations <- expand.grid(monthyears,pathways,providers,specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyears","pathways","providers","specialties")

out.combinations2 <- pbmapply(return_week_lower_catch,
                             monthyear=combinations$monthyears,
                             type=combinations$pathways,
                             provider=combinations$providers,
                             specialty=combinations$specialties,
                             MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.df2 <- as.data.frame(t(out.combinations2))
rownames(out.combinations.df2) <- 1:nrow(out.combinations.df2)

#Clean up dates
out.combinations.df2$month <- only_letters(out.combinations.df2$monthyear)
out.combinations.df2$year <- paste0("20",tidyr::extract_numeric(out.combinations.df2$monthyear))
out.combinations.df2$monthyear <- NULL

#Save
setwd(rawdatadir)
fwrite(out.combinations.df2, file = paste0(rawdatadir,"/Clean/Monthly - By specialty and national.csv"), sep = ",")

#########################################################
################### CCG level statistics ################
#########################################################

#Independent: 0 non-IS, 1 IS and 2 All

return_week_lower_ccg <- function(monthyear,ccg,specialty,quantiles,type,independent){
  
    # ccg <- "NHS LEEDS CCG"
    # specialty <- "Total"
    # quantiles <- c(0.50)
    # type <- "completenonadmitted"
    # independent <- 1
  
  #Fetch deprivation values and region from lookup
  
  IMD19_decile <- ifelse(length(filter(CCG_to_IMD19_data,ccg19nm==ccg)$IMD19_decile)>0,
                      filter(CCG_to_IMD19_data,ccg19nm==ccg)$IMD19_decile,NA)
  IMD19_quintile <- ifelse(length(filter(CCG_to_IMD19_data,ccg19nm==ccg)$IMD19_quintile)>0,
                           filter(CCG_to_IMD19_data,ccg19nm==ccg)$IMD19_quintile,NA)
  NHSER19NM <- ifelse(length(filter(CCG_to_IMD19_data,ccg19nm==ccg)$NHSER19NM)>0,
                      filter(CCG_to_IMD19_data,ccg19nm==ccg)$NHSER19NM,NA)
  
  #Pick relevant month-year
  
  dataset <- filter(RTT_allmonths,monthyr==monthyear)
  
  #If CCG is England, use all rows and all CCGs
  
  dataset$Commissioner.Org.Name <- ifelse(rep(ccg,nrow(dataset))=="ENGLAND",
                                      rep("ENGLAND",nrow(dataset)),
                                      dataset$Commissioner.Org.Name)
  
  #Filter based o type of provider
  
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
    datasubset <- filter(monthly_data,Commissioner.Org.Name==ccg&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Incomplete Pathways")
  } else if (type=="completeadmitted"){
    datasubset <- filter(monthly_data,Commissioner.Org.Name==ccg&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Admitted Patients")
  } else if (type=="completenonadmitted"){
    datasubset <- filter(monthly_data,Commissioner.Org.Name==ccg&
                           Treatment.Function.Name==specialty&
                           RTT.Part.Description=="Completed Pathways For Non-Admitted Patients")
  }
  
  #Aggregate rows and compute total patients
  
  if (type=="completeadmitted"|type=="completenonadmitted"){
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sum)) %>% t()
    
    datasubset_unknown <- datasubset %>% 
      summarise(across(starts_with(c("Patients.with.unknown.clock.start.date")), sum)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete") {
    
    datasubset_sum <- datasubset %>% 
      summarise(across(starts_with(c("Gt")), sum)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE)
    
    total.nonmiss <- total
  }
  
  #Only compute quantiles if there are more than 20 patients
  #Compute them in 3 different ways: Total, IS and non-IS
    
  if (total.nonmiss>=20){
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
    
    rate_18_or_less <- round(cumsum(datasubset_sum)[18]/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         ccg=as.character(ccg),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         rate.18wks.or.less=rate_18_or_less,
                         weeks,
                         IMD19_decile=as.character(IMD19_decile),
                         IMD19_quintile=as.character(IMD19_quintile),
                         NHSER19NM=as.character(NHSER19NM))
  } else {
    weeks <- rep(NA,length(quantiles))
    
    weeks <- as.data.frame(weeks) %>% t()
    
    colnames(weeks) <- paste0("weeks.",quantiles*100)
    
    output <- data.frame(monthyear=as.character(monthyear),
                         ccg=as.character(ccg),
                         specialty=as.character(specialty),
                         type=as.character(type),
                         independent=as.character(IS),
                         total.patients=total,
                         rate.18wks.or.less=NA,
                         weeks,
                         IMD19_decile=as.character(IMD19_decile),
                         IMD19_quintile=as.character(IMD19_quintile),
                         NHSER19NM=as.character(NHSER19NM))
  }
  
  return(output)
}

#This version of the function makes sure it still produces a result
#if anything goes wrong

return_week_lower_ccg_catch <- function(monthyear,ccg,specialty,quantiles,type,independent){
  tryCatch(
    {
      return(return_week_lower_ccg(monthyear,ccg,specialty,quantiles,type,independent))
    },
    error=function(error_message) {
      
      output <- data.frame(monthyear=as.character(monthyear),
                           ccg=as.character(ccg),
                           specialty=as.character(specialty),
                           type=as.character(type),
                           independent=NA,
                           total.patients=NA,
                           rate.18wks.or.less=NA)
      
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
# 
# return_week_lower_ccg_catch(monthyear="Jun20",
#                       ccg="NHS LEEDS CCG",
#                       specialty="Total",quantiles=c(0.5,0.92,0.95),
#                       type="completenonadmitted",independent=2)

#Run for all combinations: inputs

all_ccgs <- unique(RTT_allmonths$Commissioner.Org.Name)

monthyears <- all_months
providertypes <- 0:2
pathways <- c("incomplete","completeadmitted","completenonadmitted")
ccgs <- c("ENGLAND",all_ccgs)
specialties <- c("Total")

combinations <- expand.grid(monthyears,providertypes,pathways,ccgs,specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyears","providertypes","pathways","ccgs","specialties")

#Run for all combinations: run function

ccg.out.combinations3 <- pbmapply(return_week_lower_ccg_catch,
                           monthyear=combinations$monthyears,
                           independent=combinations$providertypes,
                           type = combinations$pathways,
                           ccg=combinations$ccgs,
                           specialty=combinations$specialties,
                           MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

ccg.out.combinations.df3 <- as.data.frame(t(ccg.out.combinations3))
rownames(ccg.out.combinations.df3) <- 1:nrow(ccg.out.combinations.df3)

#Clean up dates
ccg.out.combinations.df3$month <- only_letters(ccg.out.combinations.df3$monthyear)
ccg.out.combinations.df3$year <- paste0("20",tidyr::extract_numeric(ccg.out.combinations.df3$monthyear))
ccg.out.combinations.df3$monthyear <- NULL

#Save
setwd(rawdatadir)
fwrite(ccg.out.combinations.df3, file = paste0(rawdatadir,"/Clean/Monthly - By ccg and national.csv"), sep = ",")
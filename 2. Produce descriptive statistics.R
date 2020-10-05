##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Removing commsionner code NONC removes private patients
#This is what the dashboard does

#check that Totals are not just totals of trusts

#Note: 'Incomplete Pathways with DTA' not part of the dashboard statistics

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

#### Capture names of providers and specialties

all_months <- unique(RTT_allmonths$monthyr)
all_providers <- unique(RTT_allmonths$Provider.Org.Name) %>% c(.,"ENGLAND")
providers_trusts <- all_providers[str_detect(all_providers, "TRUST")] %>% c(.,"ENGLAND")
all_specialties <- unique(RTT_allmonths$Treatment.Function.Name)
all_ccgs <- unique(RTT_allmonths$Commissioner.Org.Name[which(RTT_allmonths$Commissioner.Org.Name!="")]) %>% c(.,"ENGLAND")
pathways <- c("incomplete","completeadmitted","completenonadmitted")

###############################################################################
################### Reproduce dashboard metrics with functions ################
###############################################################################

############### By provider

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
      summarise(dplyr::across(starts_with(c("Patients.with.unknown.clock.start.date")), sum)) %>% t()
    
    total <- sum(datasubset_sum,na.rm=TRUE) + sum(datasubset_unknown,na.rm=TRUE)
    
    total.nonmiss <- sum(datasubset_sum,na.rm=TRUE)
    
  } else if (type=="incomplete") {
    
    datasubset_sum <- datasubset %>% 
      summarise(dplyr::across(starts_with(c("Gt")), sum)) %>% t()
    
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
  } else {
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

# return_week_lower_catch(monthyear="Jul20",
#                   provider="LANCASHIRE & SOUTH CUMBRIA NHS FOUNDATION TRUST",
#                   specialty="Total",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="incomplete")
# 

# return_week_lower_catch(monthyear="Jun20",
#                   provider="ENGLAND",
#                   specialty="Dermatology",
#                   quantiles=c(0.5,0.92,0.95),
#                   type="completenonadmitted")

############### By CCG

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
    
    number_52_or_more <- datasubset_sum[53] %>% unlist()
    rate_52_or_more <- round(number_52_or_more/total.nonmiss*100,1) %>% unlist()
    
    number_18_or_less <- cumsum(datasubset_sum)[18] %>% unlist()
    rate_18_or_less <- round(number_18_or_less/total.nonmiss*100,1) %>% unlist()
    
    #Function output
    
    output <- data.frame(monthyear=as.character(monthyear),
                         ccg=as.character(ccg),
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
# 
# return_week_lower_ccg_catch(monthyear="Jun20",
#                       ccg="NHS LEEDS CCG",
#                       specialty="Total",quantiles=c(0.5,0.92,0.95),
#                       type="completenonadmitted",independent=2)

########################################################################################
################### Compute descriptive statistics for all combinations ################
########################################################################################

############### Create combinations for providers

combinations <- expand.grid(all_months,
                                    pathways,
                                    all_providers,all_specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyr","pathways","Provider.Org.Name","Treatment.Function.Name")

observed_combinations <- paste(RTT_allmonths$monthyr,RTT_allmonths$Provider.Org.Name,RTT_allmonths$Treatment.Function.Name,sep=" ")

combinations <- filter(combinations, Provider.Org.Name=="ENGLAND" |
paste(combinations$monthyr,combinations$Provider.Org.Name,
      combinations$Treatment.Function.Name,sep=" ") %in% observed_combinations)

############### File 1: National and by specialty

combinations.one <- filter(combinations,Provider.Org.Name=="ENGLAND")

#combinations.one <- combinations.two[sample(1:nrow(combinations.one),10),]

out.combinations.one <- pbmapply(return_week_lower_catch,
                                 monthyear=combinations.one$monthyr,
                                 type=combinations.one$pathways,
                                 provider=combinations.one$Provider.Org.Name,
                                 specialty=combinations.one$Treatment.Function.Name,
                                 MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.one.df <- as.data.frame(t(out.combinations.one))
rownames(out.combinations.one.df) <- 1:nrow(out.combinations.one.df)
rm(combinations.one,out.combinations.one)

#Clean up dates for Excel
out.combinations.one.df$month <- only_letters(out.combinations.one.df$monthyear)
out.combinations.one.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.one.df$monthyear))
out.combinations.one.df$monthyear <- NULL

############### File 2: National and by trust, combined specialties

combinations.two <- filter(combinations,Provider.Org.Name %in% providers_trusts) %>%
  filter(.,Treatment.Function.Name=="Total")

#combinations.two <- combinations.two[sample(1:nrow(combinations.two),10),]

out.combinations.two <- pbmapply(return_week_lower_catch,
                                 monthyear=combinations.two$monthyr,
                                 type=combinations.two$pathways,
                                 provider=combinations.two$Provider.Org.Name,
                                 specialty=combinations.two$Treatment.Function.Name,
                                 MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.two.df <- as.data.frame(t(out.combinations.two))
rownames(out.combinations.two.df) <- 1:nrow(out.combinations.two.df)
rm(combinations.two,out.combinations.two)

#Clean up dates for Excel
out.combinations.two.df$month <- only_letters(out.combinations.two.df$monthyear)
out.combinations.two.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.two.df$monthyear))
out.combinations.two.df$monthyear <- NULL

############### By CCG

############### Create combinations for CCGs

providertypes <- 0:2

combinations.ccg <- expand.grid(all_months,
                            pathways,
                            all_ccgs,all_specialties,providertypes) %>% varhandle::unfactor()
names(combinations.ccg) <- c("monthyr","pathways","Commissioner.Org.Name","Treatment.Function.Name","providertypes")

observed_combinations.ccg <- paste(RTT_allmonths$monthyr,RTT_allmonths$Commissioner.Org.Name,RTT_allmonths$Treatment.Function.Name,sep=" ")

combinations.ccg <- filter(combinations.ccg, Commissioner.Org.Name=="ENGLAND" |
                         paste(combinations.ccg$monthyr,combinations.ccg$Commissioner.Org.Name,
                               combinations.ccg$Treatment.Function.Name,sep=" ") %in% observed_combinations.ccg)

############### File 3: National and by CCG, combined specialties

combinations.three <- filter(combinations.ccg,Commissioner.Org.Name %in% all_ccgs) %>%
  filter(.,Treatment.Function.Name=="Total")

#combinations.three <- combinations.three[sample(1:nrow(combinations.three),10),]

out.combinations.three <- pbmapply(return_week_lower_ccg_catch,
                                   monthyear=combinations.three$monthyr,
                                   independent=combinations.three$independent,
                                   type = combinations.three$pathways,
                                   ccg=combinations.three$Commissioner.Org.Name,
                                   specialty=combinations.three$Treatment.Function.Name,
                                   MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.three.df <- as.data.frame(t(out.combinations.three))
rownames(out.combinations.three.df) <- 1:nrow(out.combinations.three.df)
rm(combinations.three,out.combinations.three)

#Clean up dates
out.combinations.three.df$month <- only_letters(out.combinations.three.df$monthyear)
out.combinations.three.df$year <- paste0("20",tidyr::extract_numeric(out.combinations.three.df$monthyear))
out.combinations.three.df$monthyear <- NULL

########################################################
################### Save to a worksheet ################
########################################################

list_of_datasets <- list("National, by specialty" = out.combinations.one.df,
                         "National and Trust, comb spec" = out.combinations.two.df,
                         "National and CCG, comb spec" = out.combinations.three.df)

write.xlsx(list_of_datasets, file = paste0(rawdatadir,"/Clean/RTT - monthly series summarised.xlsx"))
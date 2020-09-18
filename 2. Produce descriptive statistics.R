##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

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
    
  } else {
    
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

return_week_lower(monthyear="Jun20",
                  provider="WHITTINGTON HEALTH NHS TRUST",
                  specialty="Total",
                  quantiles=c(0.5,0.92,0.95),
                  type="incomplete")

return_week_lower(monthyear="Jun20",
                  provider="ENGLAND",
                  specialty="Plastic Surgery",
                  quantiles=c(0.5,0.92,0.95),
                  type="completenonadmitted")

#Run for all combinations: inputs

monthyears <- c("Jun20")

pathways <- c("incomplete","completeadmitted","completenonadmitted")

providers <- c("ENGLAND",
               "GUY'S AND ST THOMAS' NHS FOUNDATION TRUST",
               "THE CHRISTIE NHS FOUNDATION TRUST")

specialties <- c("General Surgery","Total","Plastic Surgery")

combinations <- expand.grid(monthyears,pathways,providers,specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyears","pathways","providers","specialties")

#Run for all combinations: run function

out.combinations <- mapply(return_week_lower,
                           monthyear=combinations$monthyears,
                           type=combinations$pathways,
                           provider=combinations$providers,
                           specialty=combinations$specialties,
       MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.df <- as.data.frame(t(out.combinations))
rownames(out.combinations.df) <- 1:nrow(out.combinations.df)

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

return_week_lower_ccg(monthyear="Jun20",
                      ccg="NHS CENTRAL LONDON (WESTMINSTER) CCG",
                      specialty="Total",quantiles=c(0.5,0.92,0.95),
                      type="incomplete",independent=2)

return_week_lower_ccg(monthyear="Jun20",
                      ccg="NHS LEEDS CCG",
                      specialty="Total",quantiles=c(0.5,0.92,0.95),
                      type="completenonadmitted",independent=1)

#Run for all combinations: inputs

monthyears <- "Jun20"

providertypes <- 0:2

pathways <- c("incomplete","completeadmitted","completenonadmitted")

ccgs <- c("NHS LEEDS CCG","NHS CENTRAL LONDON (WESTMINSTER) CCG")

specialties <- c("Total","General Surgery","Plastic Surgery")

combinations <- expand.grid(monthyears,providertypes,pathways,ccgs,specialties) %>% varhandle::unfactor()
names(combinations) <- c("monthyears","providertypes","pathways","ccgs","specialties")

#Run for all combinations: run function

ccg.out.combinations <- mapply(return_week_lower_ccg,
                           monthyear=combinations$monthyears,
                           independent=combinations$providertypes,
                           type = combinations$pathways,
                           ccg=combinations$ccgs,
                           specialty=combinations$specialties,
                           MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

ccg.out.combinations.df <- as.data.frame(t(ccg.out.combinations))
rownames(ccg.out.combinations.df) <- 1:nrow(ccg.out.combinations.df)
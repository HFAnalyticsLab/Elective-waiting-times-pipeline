##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

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
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatabucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/RTT waiting times data/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#########################################################################
################### Mapping CCGs to deprivation and region ##############
#########################################################################

# Columns in this file are as follows: 
#   • CCG Code
# • CCG Name
# • RAvgRank = Rank of Average Rank
# • RAvgScor = Rank of Average Score
# • RPLMD10 = Rank of Proportion of LSOAs Most Deprived 10%

#Lookups

#CCG_codes_NHS_ONS_lookup <- fread(paste0(rawdatadir,"/Lookups/Clinical_Commissioning_Groups_(April_2019)_Names_and_Codes_in_England.csv"), header=TRUE, sep=",", check.names=T,drop=1)

CCG_to_higher <- fread(paste0(rawdatadir,"/Lookups/Clinical_Commissioning_Group_to_NHS_England_(Region,_Local_Office)_and_NHS_England_(Region)_(April_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

manually_added <- fread(paste0(rawdatadir,"/Lookups/unmatched_ccgs_matched_central.csv"), header=TRUE, sep=",", check.names=T) %>%
  filter(.,NHSER19NM!="") %>% rename(.,ccg19nm=Commissioner.Org.Name,CCG19CDH=Commissioner.Org.Code)

#Deprivation data

CCG_to_IMD19 <- readOGR(dsn=paste0(rawdatadir,"/Shapefiles/Clinical_Commissioning_Group_(CCG)_IMD_2019_(OSGB1936)"), layer="c8aa66b8-d408-44b9-9641-ea45fb3344f02020315-1-68y7uz.trdmv")

CCG_to_IMD19 <- spTransform(CCG_to_IMD19, CRS(latlong)) #Set to the same projection

CCG_to_IMD19_data <- CCG_to_IMD19@data

#Create quintiles

CCG_to_IMD19_data <- CCG_to_IMD19_data %>%
  mutate(.,IMD19_decile = ntile(RAvgScor, 10),
         IMD19_quintile=ntile(RAvgScor, 5)) %>%
  mutate(.,ccg19nm=toupper(ccg19nm))

#Merge in region data and NHS CCG codes

CCG_to_IMD19_data <- left_join(CCG_to_IMD19_data,select(CCG_to_higher,CCG19CD,CCG19CDH,NHSER19CD,NHSER19NM),
                               by=c("ccg19cd"="CCG19CD"))

CCG_to_IMD19_data <- rbind.fill(CCG_to_IMD19_data,manually_added)

#######################################
################### Save ##############
#######################################

fwrite(CCG_to_IMD19_data, file = paste0(rawdatadir,"/Clean/CCG_to_IMD19_data.csv"), sep = ",")
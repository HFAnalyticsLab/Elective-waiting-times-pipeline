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
library(janitor)
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Directories in S3
source('setup.R') #get project locations in s3 and working directory

# IHT_bucket: s3 project bucket
# RTT_subfolder: folder to place data
# R_workbench: R server working directory

##############################################################
################### Mapping CCGs to deprivation ##############
##############################################################

#Load LSOA to CCG lookups

LSOA11CD_to_CCG21 <-  s3read_using(fread
             , object = paste0(RTT_subfolder,"/LSOA to CCG lookups/","Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2021)_Lookup_in_England.csv") # File to open
             , bucket = IHT_bucket)

LSOA11CD_to_CCG19 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/LSOA to CCG lookups/","Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2019)_Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket)

LSOA11CD_to_CCG18 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/LSOA to CCG lookups/","Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2018)_Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket)

LSOA11CD_to_CCG17 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/LSOA to CCG lookups/","Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2017)_Lookup_in_England_(Version_4).csv") # File to open
                                   , bucket = IHT_bucket)

#Join all CCG lookups

#Wide format
LSOA11CD_to_CCG_joined_wide <- LSOA11CD_to_CCG21 %>%
  select(.,LSOA11CD,CCG21CDH) %>% 
  left_join(.,select(LSOA11CD_to_CCG19,LSOA11CD,CCG19CDH),by="LSOA11CD") %>% 
  left_join(.,select(LSOA11CD_to_CCG18,LSOA11CD,CCG18CDH),by="LSOA11CD") %>%
  left_join(.,select(LSOA11CD_to_CCG17,LSOA11CD,CCG17CDH),by="LSOA11CD")
rm(LSOA11CD_to_CCG21,LSOA11CD_to_CCG19,LSOA11CD_to_CCG18,LSOA11CD_to_CCG17)

#Long format

LSOA11CD_to_CCG_joined_long <- LSOA11CD_to_CCG_joined_wide %>%
  pivot_longer(!LSOA11CD, names_to = "lookup_code", values_to = "CCGCDH") %>%
  mutate(.,ccg_year=case_when(lookup_code %in% c("CCG21CDH") ~ "2021",
                              lookup_code %in% c("CCG19CDH") ~ "2019",
                              lookup_code %in% c("CCG18CDH") ~ "2018",
                              lookup_code %in% c("CCG17CDH") ~ "2017",
                              TRUE ~ "NA"))

#Investigate duplicates

# LSOA11CD_to_CCG_joined %>%
#   get_dupes(LSOA11CD)

#Load IMD scores by LSOA

LSOA11CD_to_IMD2019_raw <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/IMD 2019/","imd2019lsoa.csv") # File to open
                                   , bucket = IHT_bucket)
#High scores are decile 1

# LSOA11CD_to_IMD2019_raw %>%
#   filter(.,Measurement %in% c("Score","Decile"),
#          DateCode=="2019",
#          `Indices of Deprivation`=="a. Index of Multiple Deprivation (IMD)") %>%
#   select(.,FeatureCode,Measurement,Value) %>%
#   pivot_wider(names_from = Measurement,
#               names_sep = ".",
#               values_from = c(Value)) %>%
#   arrange(.,desc(Decile))
  
LSOA11CD_to_IMD2019 <- LSOA11CD_to_IMD2019_raw

LSOA11CD_to_IMD2019 <- LSOA11CD_to_IMD2019 %>%
  filter(.,Measurement=="Score",DateCode=="2019",`Indices of Deprivation`=="a. Index of Multiple Deprivation (IMD)") %>%
  select(.,FeatureCode,Value) %>%
  rename(.,LSOA11CD="FeatureCode",
         IMD19_score=Value)

#Load population LSOA

LSOA11CD_to_pop19 <-  s3read_using(read_excel
                                     , object = paste0(RTT_subfolder,"/Population/","SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx") # File to open
                                     , bucket = IHT_bucket,
                                   sheet="Mid-2019 Persons",skip=4) 

LSOA11CD_to_pop19 <- LSOA11CD_to_pop19 %>%
  select(.,`LSOA Code`, `All Ages`) %>%
  rename(.,LSOA11CD=`LSOA Code`,pop19= `All Ages`)

#Merge lookups, IMD scores and population

IMD_by_CCG_long <- LSOA11CD_to_CCG_joined_long %>%
  left_join(.,LSOA11CD_to_IMD2019,by="LSOA11CD") %>%
  left_join(.,LSOA11CD_to_pop19,by="LSOA11CD")

#Aggregate by year and CCG

IMD_by_CCG_long <- IMD_by_CCG_long %>%
  group_by(ccg_year,lookup_code,CCGCDH) %>%
  summarise(weighted_imd_score=weighted.mean(IMD19_score,pop19)) %>%
  ungroup() %>%
  group_by(ccg_year,lookup_code) %>%
  mutate(IMD19_decile= 11 - ntile(weighted_imd_score, 10),
         IMD19_quintile= 6 - ntile(weighted_imd_score, 5)) %>% #the '11 - x' transformation inverts the 1:10 scale
  ungroup() %>%
  select(.,ccg_year,lookup_code,CCGCDH,IMD19_decile,IMD19_quintile)

IMD_by_CCG_wide <- IMD_by_CCG_long %>%
  select(.,-"lookup_code") %>%
  pivot_wider(
    names_from = ccg_year,
    names_sep = "_",
    values_from = c("IMD19_decile","IMD19_quintile")
  )
  
#Check direction of deciles is right
# IMD_by_CCG_long %>%
#   filter(.,ccg_year=="2021") %>%
#   pull(IMD19_decile) %>%
#   table(.,useNA="always")

#######################################
################### Save ##############
#######################################

s3write_using(IMD_by_CCG_wide # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Custom RTT lookups/","IMD_by_CCG_wide.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

##############################################################
################### Mapping CCGs to NHS Regions ##############
##############################################################

#Load LSOA to CCG lookups

CCG_to_REG21 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/CCG to NHSE lookups/","Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket) %>%
  mutate(.,ccg_year="2021") %>% rename(., CCGCDH=CCG21CDH,NHSERNM=NHSER21NM,STPNM=STP21NM)

CCG_to_REG20 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/CCG to NHSE lookups/","Clinical_Commissioning_Group_to_STP_and_NHS_England__Region___April_2020__Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket) %>%
  mutate(.,ccg_year="2020") %>% rename(., CCGCDH=CCG20CDH,NHSERNM=NHSER20NM,STPNM=STP20NM)

CCG_to_REG19 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/CCG to NHSE lookups/","Clinical_Commissioning_Group_to_NHS_England_(Region,_Local_Office)_and_NHS_England_(Region)_(April_2019)_Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket) %>%
  mutate(.,ccg_year="2019") %>% rename(., CCGCDH=CCG19CDH,NHSERNM=NHSER19NM) #No STPs in 2019 and back

CCG_to_REG18 <-  s3read_using(fread
                                   , object = paste0(RTT_subfolder,"/CCG to NHSE lookups/","Clinical_Commissioning_Group_to_NHS_England_(Region,_Local_Office)_and_NHS_England_(Region)_(April_2018)_Lookup_in_England.csv") # File to open
                                   , bucket = IHT_bucket) %>%
  mutate(.,ccg_year="2018") %>% rename(., CCGCDH=CCG18CDH,NHSERNM=NHSER18NM)

#2017 lookup doesn't have the CCG codes we need
# CCG_to_REG17 <-  s3read_using(fread
#                               , object = paste0(RTT_subfolder,"/CCG to NHSE lookups/","Clinical_Commissioning_Group_to_NHS_England_(Region,_Local_Office)_and_NHS_England_(Region)_(April_2017)_Lookup_in_England_(Version_3).csv") # File to open
#                               , bucket = IHT_bucket)

#Join all CCG lookups

#Long format
CCG_NHSER_joined_long <- CCG_to_REG21 %>%
  select(.,CCGCDH,ccg_year, NHSERNM,STPNM) %>%
  plyr::rbind.fill(.,select(CCG_to_REG20,CCGCDH,ccg_year, NHSERNM,STPNM)) %>%
  plyr::rbind.fill(.,select(CCG_to_REG19,CCGCDH,ccg_year, NHSERNM)) %>%
  plyr::rbind.fill(.,select(CCG_to_REG18,CCGCDH,ccg_year, NHSERNM))

#Wide format
CCG_NHSER_joined_wide <- CCG_NHSER_joined_long %>%
  pivot_wider( names_from = ccg_year,
               names_sep = "_",
               values_from = c(NHSERNM,STPNM))

#######################################
################### Save ##############
#######################################

s3write_using(CCG_NHSER_joined_long # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Custom RTT lookups/","CCG_NHSER_joined_long.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

s3write_using(CCG_NHSER_joined_wide # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Custom RTT lookups/","CCG_NHSER_joined_wide.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above
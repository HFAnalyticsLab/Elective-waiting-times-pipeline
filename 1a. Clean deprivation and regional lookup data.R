##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Dynamic or static approach

#https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(LUP_LSOA_CCG_LAD)

#https://healthgps.co.uk/
#has org codes and addresses

#or use https://www.linkedin.com/pulse/using-data-from-google-r-studio-caleb-aguiar
#Google API
#or OMS

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

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")

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
              , object = paste0(RTT_subfolder,"/","IMD_by_CCG_wide.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above
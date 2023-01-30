##########################################
################### TO DO ################
##########################################

# National results 
# 
# Chart 2a: 1.3.2 
# 
# Chart 2b: 1.3.1 
# 
# Chart 4a: 4.1.2 
# 
# Chart 4b: 4.2.2 
# 
# Chart 5a: 4.1.3 
# 
# Chart 5b: 4.2.3 
# 
# Chart 6 a-b-c: 3.1.1.1 
# 
# Chart 7 a-b: 3.2.1.1 
# 
# Chart 8a: 5.1 
# 
# Chart 8b: 5.2 
# 
# Sub-national eye 
# 
# Chart 3a: 1.1.1 and 1.1.2 (please also include England average) 
# 
# Sub-national orthopedic 
# 
# Chart 3b: 1.1.1 and 1.1.2 (please also include England average)

###########################################
################### Set-up ################
###########################################

###### Libraries ######

#Some of these might not be needed
library(tidyverse)
library(stringr)
library(tidyr)
library(purrr)
library(pbapply)
library(data.table)
library(readxl)
library(ggrepel)
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Directories in S3
source('setup.R') #get project locations in s3 and working directory

# IHT_bucket: s3 project bucket
# RTT_subfolder: folder to place data
# R_workbench: R server working directory
localgit <- dirname(rstudioapi::getSourceEditorContext()$path)

#Get raw data

setwd(localgit)
source('2. Produce descriptive statistics.R')

###########################################
################### Charts ################
###########################################

#Chart 2a

SU_1_3_2_raw <- read_csv("Strategy Unit Update/Data/1-3-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_1_3_2_part_a <- SU_1_3_2 %>%
  filter(name!="Cost (£)"&speciality=="Ophthalmology") %>%
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(value)) %>%
  mutate(Grid="Volume of treatments",
         `Independent sector share of total treatments`=NA) %>%
  arrange(name,Grid,der_activity_month) %>%
  select(name,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

SU_1_3_2_part_b <- SU_1_3_2_part_a %>%
  mutate(`Independent sector share of total treatments`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Share of treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(name,Grid,der_activity_month) %>%
  select(name,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

chart2a <- plyr::rbind.fill(SU_1_3_2_part_a,SU_1_3_2_part_b) %>%
  arrange(name,desc(Grid),der_activity_month)
  
rm(SU_1_3_2_part_a,SU_1_3_2_part_b)

fwrite(chart2a,paste0(localgit,"/Strategy Unit Update/chart2a.csv"))

#Chart 2b

SU_1_3_1_raw <- read_csv("Strategy Unit Update/Data/1-3-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_1_3_1_part_a <- SU_1_3_1_raw %>%
  filter(name!="Cost (£)"&speciality=="Orthopaedic") %>%
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(value)) %>%
  mutate(Grid="Volume of treatments",
         `Independent sector share of total treatments`=NA) %>%
  arrange(name,Grid,der_activity_month) %>%
  select(name,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

SU_1_3_1_part_b <- SU_1_3_1_part_a %>%
  mutate(`Independent sector share of total treatments`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Share of treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(name,Grid,der_activity_month) %>%
  select(name,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

chart2b <- plyr::rbind.fill(SU_1_3_1_part_a,SU_1_3_1_part_b) %>%
  arrange(name,desc(Grid),der_activity_month)

rm(SU_1_3_1_part_a,SU_1_3_1_part_b)

fwrite(chart2b,paste0(localgit,"/Strategy Unit Update/chart2b.csv"))

#Chart 3a: eye

SU_sub_eye_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national ophthalmic trends in independent sector provision of elective care.csv") %>%
  select(-1)

eye_subnat_england <- SU_sub_eye_1_1_1_raw %>%
  mutate(value_ind=ifelse(sector=="Independent Sector",value,NA)) %>% 
  group_by(der_activity_month,name) %>%
  summarise(value=sum(value,na.rm=TRUE),
            value_ind=sum(value_ind,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(prop=value_ind/value*100,
         var_1="England") %>%
  select(der_activity_month,var_1,name,prop)

chart3a <- SU_sub_eye_1_1_1_raw %>%
  filter(sector=="Independent Sector") %>%
  plyr::rbind.fill(.,eye_subnat_england) %>%
  select(der_activity_month,var_1,name,prop) %>%
  rename(region=var_1) %>%
  pivot_wider(
    names_from = region,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month)

fwrite(chart3a,paste0(localgit,"/Strategy Unit Update/chart3a.csv"))

#ALT Chart 3a: eye

SU_sub_eye_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national ophthalmic trends in independent sector provision of elective care.csv") %>%
  select(-1)

chart3a_heat <- SU_sub_eye_1_1_1_raw %>%
  filter(sector=="Independent Sector") %>%
  select(der_activity_month,var_1,name,prop) %>%
  rename(region=var_1) %>%
  mutate(month=lubridate::month(der_activity_month, label=TRUE, abbr = TRUE),
         year=lubridate::year(der_activity_month) %>% substr(., start = 3, stop = 4)) %>%
  mutate(monthyear=paste(month,year,sep=" ")) %>%
  select(-c("month","year")) %>% 
  arrange(name,region,der_activity_month)

fwrite(chart3a_heat,paste0(localgit,"/Strategy Unit Update/chart3a_heat.csv"))

#Chart 3b: ortho

SU_sub_ortho_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national orthopaedic trends in independent sector provision of elective care.csv") %>%
  select(-1)

ortho_subnat_england <- SU_sub_ortho_1_1_1_raw %>%
  mutate(value_ind=ifelse(sector=="Independent Sector",value,NA)) %>% 
  group_by(der_activity_month,name) %>%
  summarise(value=sum(value,na.rm=TRUE),
            value_ind=sum(value_ind,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(prop=value_ind/value*100,
         var_1="England") %>%
  select(der_activity_month,var_1,name,prop)

chart3b <- SU_sub_ortho_1_1_1_raw %>%
  filter(sector=="Independent Sector") %>%
  plyr::rbind.fill(.,ortho_subnat_england) %>%
  select(der_activity_month,var_1,name,prop) %>%
  rename(region=var_1) %>%
  pivot_wider(
    names_from = region,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month)

fwrite(chart3b,paste0(localgit,"/Strategy Unit Update/chart3b.csv"))

#Chart 4a: orthopedic

SU_4_1_2_raw <- read_csv("Strategy Unit Update/Data/4-1-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart4a <- SU_4_1_2_raw %>%
  filter(sector=="Independent Sector") %>%
  select(name,var_1,der_activity_month,prop) %>%
  rename(ethnicity=var_1) %>%
  pivot_wider(
    names_from = ethnicity,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month)

fwrite(chart4a,paste0(localgit,"/Strategy Unit Update/chart4a.csv"))

#Chart 4b: ophthalmology

SU_4_2_2_raw <- read_csv("Strategy Unit Update/Data/4-2-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart4b <- SU_4_2_2_raw %>%
  filter(sector=="Independent Sector") %>%
  select(name,var_1,der_activity_month,prop) %>%
  rename(ethnicity=var_1) %>%
  pivot_wider(
    names_from = ethnicity,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month)

fwrite(chart4b,paste0(localgit,"/Strategy Unit Update/chart4b.csv"))

#Chart 5a: orthopedic

SU_4_1_3_raw <- read_csv("Strategy Unit Update/Data/4-1-3 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart5a <- SU_4_1_3_raw %>%
  filter(sector=="Independent Sector") %>%
  select(name,var_1,der_activity_month,prop) %>%
  rename(deprivation=var_1) %>%
  pivot_wider(
    names_from = deprivation,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month) %>%
  rename(`1 (most deprived)`=`1`,`5 (least deprived)`=`5`)

fwrite(chart5a,paste0(localgit,"/Strategy Unit Update/chart5a.csv"))

#Chart 5b: ophthalmic

SU_4_2_3_raw <- read_csv("Strategy Unit Update/Data/4-2-3 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart5b <- SU_4_2_3_raw %>%
  filter(sector=="Independent Sector") %>%
  select(name,var_1,der_activity_month,prop) %>%
  rename(deprivation=var_1) %>%
  pivot_wider(
    names_from = deprivation,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(name,der_activity_month) %>%
  rename(`1 (most deprived)`=`1`,`5 (least deprived)`=`5`)

fwrite(chart5b,paste0(localgit,"/Strategy Unit Update/chart5b.csv"))

#Chart 6-a-b-c: orthopedic

SU_3_1_1_1_raw <- read_csv("Strategy Unit Update/Data/3-1-1-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_3_1_1_1_part_a <- SU_3_1_1_1_raw %>%
  filter(speciality=="Orthopaedic"&(procedure_desc_short %in% c("Hip","Knee","Hand"))) %>%
  select(-c("n_spells_IP","n_spells_OP")) %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(all_activity)) %>%
  mutate(Grid="Volume of treatments",
         `Independent sector share of total treatments`=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

SU_3_1_1_1_part_b <- SU_3_1_1_1_part_a %>%
  mutate(`Independent sector share of total treatments`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Share of treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

chart6abc <- plyr::rbind.fill(SU_3_1_1_1_part_a,SU_3_1_1_1_part_b) %>%
  arrange(procedure_desc_short,desc(Grid),der_activity_month)

rm(SU_3_1_1_1_part_a,SU_3_1_1_1_part_b)

fwrite(chart6abc,paste0(localgit,"/Strategy Unit Update/chart6abc.csv"))

#Chart 7-a-b: ophthalmic

SU_3_2_1_1_raw <- read_csv("Strategy Unit Update/Data/3-2-1-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_3_2_1_1_part_a <- SU_3_2_1_1_raw %>%
  filter(speciality=="Ophthalmology"&(procedure_desc_short %in% c("Cataract","Vitreous Retinal"))) %>%
  select(-c("n_spells_IP","n_spells_OP")) %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(all_activity)) %>%
  mutate(Grid="Volume of treatments",
         `Independent sector share of total treatments`=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

SU_3_2_1_1_part_b <- SU_3_2_1_1_part_a %>%
  mutate(`Independent sector share of total treatments`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Share of treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,`Independent sector share of total treatments`,`Independent Sector`,NHS)

chart7ab <- plyr::rbind.fill(SU_3_2_1_1_part_a,SU_3_2_1_1_part_b) %>%
  arrange(procedure_desc_short,desc(Grid),der_activity_month)

rm(SU_3_2_1_1_part_a,SU_3_2_1_1_part_b)

fwrite(chart7ab,paste0(localgit,"/Strategy Unit Update/chart7ab.csv"))

#Chart 8a static: orhopaedic

SU_5_1_raw <- read_csv("Strategy Unit Update/Data/5-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8a_static <- SU_5_1_raw %>%
  mutate(first_bound=word(var_1,1,sep="-") %>% as.numeric(.)) %>% 
  filter(year=="2022") %>%
  select(-c("value")) %>% 
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(year,first_bound,name) %>%
  select(-c("first_bound")) %>% 
  rename(days=var_1)

fwrite(chart8a_static,paste0(localgit,"/Strategy Unit Update/chart8a_static.csv"))

#Chart 8b static: ophthalmic

SU_5_2_raw <- read_csv("Strategy Unit Update/Data/5-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8b_static <- SU_5_2_raw %>%
  mutate(first_bound=word(var_1,1,sep="-") %>% as.numeric(.)) %>% 
  filter(year=="2022") %>%
  select(-c("value")) %>% 
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(year,first_bound,name) %>%
  select(-c("first_bound")) %>% 
  rename(days=var_1)

fwrite(chart8b_static,paste0(localgit,"/Strategy Unit Update/chart8b_static.csv"))

#Chart 8a dynamic: orhopaedic

SU_5_1_raw <- read_csv("Strategy Unit Update/Data/5-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8a_dyn_year <- SU_5_1_raw %>%
  mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                             var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                             TRUE ~ "NA")) %>%
  group_by(year,name,sector,time_year) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_year=="Under a year") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(name,year)

chart8a_dyn_3m <- SU_5_1_raw %>%
  mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                           var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                           TRUE ~ "NA")) %>%
  group_by(year,name,sector,time_3m) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_3m=="Under 3m") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(name,year)

fwrite(chart8a_dyn_year,paste0(localgit,"/Strategy Unit Update/chart8a_dyn_year.csv"))
fwrite(chart8a_dyn_3m,paste0(localgit,"/Strategy Unit Update/chart8a_dyn_3m.csv"))

#Chart 8b dynamic: ophthalmic

SU_5_2_raw <- read_csv("Strategy Unit Update/Data/5-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8b_dyn_year <- SU_5_2_raw %>%
  mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                           var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                           TRUE ~ "NA")) %>%
  group_by(year,name,sector,time_year) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_year=="Under a year") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(name,year)

chart8b_dyn_3m <- SU_5_2_raw %>%
  mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                           var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                           TRUE ~ "NA")) %>%
  group_by(year,name,sector,time_3m) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_3m=="Under 3m") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(name,year)

fwrite(chart8b_dyn_year,paste0(localgit,"/Strategy Unit Update/chart8b_dyn_year.csv"))
fwrite(chart8b_dyn_3m,paste0(localgit,"/Strategy Unit Update/chart8b_dyn_3m.csv"))

##########################################
################### TO DO ################
##########################################

###########################################
################### Set-up ################
###########################################

###### Libraries ######

library(tidyverse)
library(data.table)
library(readxl)
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

# setwd(localgit)
# setwd("../")
# source('2. Produce descriptive statistics.R')

###########################################
################### Charts ################
###########################################

#Chart 2a

SU_1_3_2_raw <- read_csv("Strategy Unit Update/Data/1-3-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_1_3_2_part_a <- SU_1_3_2_raw %>%
  filter(name=="Inpatient admissions") %>%
  select(-"name") %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(value)) %>%
  mutate(Grid="Number of treatments",
         `Independent sector share of total treatments (%)`=NA) %>%
  arrange(speciality,Grid,der_activity_month) %>%
  select(speciality,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,
         `Independent Sector`,NHS)

SU_1_3_2_part_b <- SU_1_3_2_part_a %>%
  mutate(`Independent sector share of total treatments (%)`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Independent sector share of total treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(speciality,Grid,der_activity_month) %>%
  select(speciality,der_activity_month,Grid,`Independent sector share of total treatments (%)`,`Independent Sector`,NHS)

chart2a <- plyr::rbind.fill(SU_1_3_2_part_a,SU_1_3_2_part_b) %>%
  arrange(speciality,desc(Grid),der_activity_month)
rm(SU_1_3_2_part_a,SU_1_3_2_part_b)

#Chart 2b

SU_1_3_1_raw <- read_csv("Strategy Unit Update/Data/1-3-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_1_3_1_part_a <- SU_1_3_1_raw %>%
  filter(name=="Inpatient admissions") %>%
  select(-"name") %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(value)) %>%
  mutate(Grid="Number of treatments",
         `Independent sector share of total treatments (%)`=NA) %>%
  arrange(speciality,Grid,der_activity_month) %>%
  select(speciality,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,
         `Independent Sector`,NHS)

SU_1_3_1_part_b <- SU_1_3_1_part_a %>%
  mutate(`Independent sector share of total treatments (%)`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Independent sector share of total treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(speciality,Grid,der_activity_month) %>%
  select(speciality,der_activity_month,Grid,`Independent sector share of total treatments (%)`,`Independent Sector`,NHS)

chart2b <- plyr::rbind.fill(SU_1_3_1_part_a,SU_1_3_1_part_b) %>%
  arrange(speciality,desc(Grid),der_activity_month)
rm(SU_1_3_1_part_a,SU_1_3_1_part_b)

#Combine

chart2 <- plyr::rbind.fill(chart2a,chart2b) %>%
  arrange(speciality,desc(Grid),der_activity_month)
rm(chart2a,chart2b)
fwrite(chart2,paste0(localgit,"/chart2.csv"))

#Chart 3a: eye
# 
# SU_sub_eye_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national ophthalmic trends in independent sector provision of elective care.csv") %>%
#   select(-1)
# 
# eye_subnat_england <- SU_sub_eye_1_1_1_raw %>%
#   mutate(value_ind=ifelse(sector=="Independent Sector",value,NA)) %>% 
#   group_by(der_activity_month,name) %>%
#   summarise(value=sum(value,na.rm=TRUE),
#             value_ind=sum(value_ind,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(prop=value_ind/value*100,
#          var_1="England") %>%
#   select(der_activity_month,var_1,name,prop)
# 
# chart3a <- SU_sub_eye_1_1_1_raw %>%
#   filter(sector=="Independent Sector") %>%
#   plyr::rbind.fill(.,eye_subnat_england) %>%
#   select(der_activity_month,var_1,name,prop) %>%
#   rename(region=var_1) %>%
#   pivot_wider(
#     names_from = region,
#     names_sep = ".",
#     values_from = c(prop)
#   ) %>%
#   arrange(name,der_activity_month)
# 
# fwrite(chart3a,paste0(localgit,"/Strategy Unit Update/chart3a.csv"))

#ALT Chart 3a: eye

SU_sub_eye_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national ophthalmic trends in independent sector provision of elective care.csv") %>%
  select(-1)

chart3a_heat <- SU_sub_eye_1_1_1_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  select(der_activity_month,var_1,name,prop) %>%
  rename(region=var_1) %>%
  mutate(month=lubridate::month(der_activity_month, label=TRUE, abbr = TRUE),
         year=lubridate::year(der_activity_month) %>% substr(., start = 3, stop = 4)) %>%
  mutate(monthyear=paste(month,year,sep=" "),
         specialty="Ophthalmology") %>%
  select(der_activity_month,region,specialty,prop,monthyear) %>% 
  arrange(specialty,region,der_activity_month)

#ALT Chart 3a: eye

SU_sub_ortho_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national orthopaedic trends in independent sector provision of elective care.csv") %>%
  select(-1)

chart3b_heat <- SU_sub_ortho_1_1_1_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  select(der_activity_month,var_1,name,prop) %>%
  rename(region=var_1) %>%
  mutate(month=lubridate::month(der_activity_month, label=TRUE, abbr = TRUE),
         year=lubridate::year(der_activity_month) %>% substr(., start = 3, stop = 4)) %>%
  mutate(monthyear=paste(month,year,sep=" "),
         specialty="Orthopaedic") %>%
  select(der_activity_month,region,specialty,prop,monthyear) %>% 
  arrange(specialty,region,der_activity_month)

#ALT Chart 3a

chart3_heat <- plyr::rbind.fill(chart3a_heat,chart3b_heat)

order_regions <- chart3_heat %>%
  filter(der_activity_month>=lubridate::dmy("01-01-2022")&specialty=="Ophthalmology") %>%
  group_by(region) %>%
  summarise(prop=mean(prop)) %>% 
  ungroup() %>%
  arrange(desc(prop)) %>%
  pull(region)

chart3_heat <- chart3_heat %>%
  mutate(region=fct_relevel(region,order_regions)) %>%
  arrange(specialty,region,der_activity_month)

fwrite(chart3_heat,paste0(localgit,"/chart3_heat.csv"))

# SU_sub_ortho_1_1_1_raw <- read_csv("Strategy Unit Update/Data/1-1-1 Sub-national orthopaedic trends in independent sector provision of elective care.csv") %>%
#   select(-1)
# 
# ortho_subnat_england <- SU_sub_ortho_1_1_1_raw %>%
#   mutate(value_ind=ifelse(sector=="Independent Sector",value,NA)) %>% 
#   group_by(der_activity_month,name) %>%
#   summarise(value=sum(value,na.rm=TRUE),
#             value_ind=sum(value_ind,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(prop=value_ind/value*100,
#          var_1="England") %>%
#   select(der_activity_month,var_1,name,prop)
# 
# chart3b <- SU_sub_ortho_1_1_1_raw %>%
#   filter(sector=="Independent Sector") %>%
#   plyr::rbind.fill(.,ortho_subnat_england) %>%
#   select(der_activity_month,var_1,name,prop) %>%
#   rename(region=var_1) %>%
#   pivot_wider(
#     names_from = region,
#     names_sep = ".",
#     values_from = c(prop)
#   ) %>%
#   arrange(name,der_activity_month)

#Chart 4

#Chart 4a: ophthalmic

SU_4_2_3_raw <- read_csv("Strategy Unit Update/Data/4-2-3 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart4a <- SU_4_2_3_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  select(var_1,der_activity_month,prop) %>%
  rename(deprivation=var_1) %>%
  pivot_wider(
    names_from = deprivation,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  mutate(specialty="Ophthalmology") %>%  
  arrange(specialty,der_activity_month) %>%
  select(specialty,der_activity_month,`1`,`2`,`3`,`4`,`5`) %>% 
  rename(`1 (most deprived)`=`1`,`5 (least deprived)`=`5`)

#Chart 4b: orthopedic

SU_4_1_3_raw <- read_csv("Strategy Unit Update/Data/4-1-3 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart4b <- SU_4_1_3_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  select(var_1,der_activity_month,prop) %>%
  rename(deprivation=var_1) %>%
  pivot_wider(
    names_from = deprivation,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  mutate(specialty="Orthopaedics") %>%  
  arrange(specialty,der_activity_month) %>%
  select(specialty,der_activity_month,`1`,`2`,`3`,`4`,`5`) %>% 
  rename(`1 (most deprived)`=`1`,`5 (least deprived)`=`5`)

#Combined

chart4 <- plyr::rbind.fill(chart4a,chart4b)
rm(chart4a,chart4b)

fwrite(chart4,paste0(localgit,"/chart4.csv"))

#Chart 5a: ophthalmology

SU_4_2_2_raw <- read_csv("Strategy Unit Update/Data/4-2-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart5a <- SU_4_2_2_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  mutate(specialty="Ophthalmology") %>% 
  select(specialty,var_1,der_activity_month,prop) %>%
  rename(ethnicity=var_1) %>%
  pivot_wider(
    names_from = ethnicity,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(specialty,der_activity_month)

#Chart 5b: orthopedic

SU_4_1_2_raw <- read_csv("Strategy Unit Update/Data/4-1-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart5b <- SU_4_1_2_raw %>%
  filter(sector=="Independent Sector"&name=="Inpatient admissions") %>%
  mutate(specialty="Orthopaedic") %>% 
  select(specialty,var_1,der_activity_month,prop) %>%
  rename(ethnicity=var_1) %>%
  pivot_wider(
    names_from = ethnicity,
    names_sep = ".",
    values_from = c(prop)
  ) %>%
  arrange(specialty,der_activity_month)

chart5 <- plyr::rbind.fill(chart5a,chart5b)
rm(chart5a,chart5b)

fwrite(chart5,paste0(localgit,"/chart5.csv"))

#Chart 6-a-b: ophthalmic

SU_3_2_1_1_raw <- read_csv("Strategy Unit Update/Data/3-2-1-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_3_2_1_1_part_a <- SU_3_2_1_1_raw %>%
  filter(speciality=="Ophthalmology"&(procedure_desc_short %in% c("Cataract","Vitreous Retinal"))) %>%
  select(-c("all_activity","n_spells_OP")) %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(n_spells_IP)) %>%
  mutate(Grid="Number of treatments",
         `Independent sector share of total treatments (%)`=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,`Independent Sector`,NHS)

SU_3_2_1_1_part_b <- SU_3_2_1_1_part_a %>%
  mutate(`Independent sector share of total treatments (%)`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Independent sector share of total treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,
         `Independent Sector`,NHS)

chart6ab <- plyr::rbind.fill(SU_3_2_1_1_part_a,SU_3_2_1_1_part_b) %>%
  arrange(procedure_desc_short,desc(Grid),der_activity_month)

rm(SU_3_2_1_1_part_a,SU_3_2_1_1_part_b)

fwrite(chart6ab,paste0(localgit,"/chart6ab.csv"))

#Chart 7-a-b-c: orthopedic

SU_3_1_1_1_raw <- read_csv("Strategy Unit Update/Data/3-1-1-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

SU_3_1_1_1_part_a <- SU_3_1_1_1_raw %>%
  filter(speciality=="Orthopaedic"&(procedure_desc_short %in% c("Hip","Knee","Hand"))) %>%
  select(-c("all_activity","n_spells_OP")) %>% 
  pivot_wider(names_from = type,
              names_sep = ".",
              values_from = c(n_spells_IP)) %>%
  mutate(Grid="Number of treatments",
         `Independent sector share of total treatments (%)`=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,`Independent Sector`,NHS)

SU_3_1_1_1_part_b <- SU_3_1_1_1_part_a %>%
  mutate(`Independent sector share of total treatments (%)`=`Independent Sector`/(`Independent Sector`+NHS)*100,
         Grid="Independent sector share of total treatments (%)",
         `Independent Sector`=NA,
         NHS=NA) %>%
  arrange(procedure_desc_short,Grid,der_activity_month) %>%
  select(procedure_desc_short,der_activity_month,Grid,
         `Independent sector share of total treatments (%)`,`Independent Sector`,NHS)

chart7abc <- plyr::rbind.fill(SU_3_1_1_1_part_a,SU_3_1_1_1_part_b) %>%
  arrange(procedure_desc_short,desc(Grid),der_activity_month)

rm(SU_3_1_1_1_part_a,SU_3_1_1_1_part_b)

fwrite(chart7abc,paste0(localgit,"/chart7abc.csv"))

#Chart 8 static

#Ophthalmic

SU_5_2_raw <- read_csv("Strategy Unit Update/Data/5-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8a_static <- SU_5_2_raw %>%
  mutate(first_bound=word(var_1,1,sep="-") %>% as.numeric(.),
         specialty="Ophthalmology") %>% 
  filter(year=="2022"&name=="Inpatient admissions") %>%
  select(-c("value","name")) %>% 
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(year,first_bound,specialty) %>%
  select(-c("first_bound")) %>% 
  rename(days=var_1)

#Orthopaedic

SU_5_1_raw <- read_csv("Strategy Unit Update/Data/5-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8b_static <- SU_5_1_raw %>%
  mutate(first_bound=word(var_1,1,sep="-") %>% as.numeric(.),
         specialty="Orthopaedic") %>% 
  filter(year=="2022"&name=="Inpatient admissions") %>%
  select(-c("value","name")) %>% 
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(year,first_bound,specialty) %>%
  select(-c("first_bound")) %>% 
  rename(days=var_1)

#Combined

chart8_static <- plyr::rbind.fill(chart8a_static,chart8b_static)
fwrite(chart8_static,paste0(localgit,"/chart8_static.csv"))

#Chart 8 dynamic

#Ophthalmic

SU_5_2_raw <- read_csv("Strategy Unit Update/Data/5-2 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8a_dyn_3m <- SU_5_2_raw %>%
  filter(name=="Inpatient admissions") %>% 
  mutate(specialty="Ophthalmology",
         time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                           var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                           TRUE ~ "NA")) %>%
  group_by(year,specialty,sector,time_3m) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_3m=="Under 3m") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(specialty,year)

#Orthopaedic

SU_5_1_raw <- read_csv("Strategy Unit Update/Data/5-1 National trends in independent sector trends in provision of elective care.csv") %>%
  select(-1)

chart8b_dyn_3m <- SU_5_1_raw %>%
  filter(name=="Inpatient admissions") %>% 
  mutate(specialty="Orthopaedic",
         time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
                             var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
                             TRUE ~ "NA"),
         time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
                           var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
                           TRUE ~ "NA")) %>%
  group_by(year,specialty,sector,time_3m) %>%
  summarise(prop=sum(prop,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(time_3m=="Under 3m") %>%
  pivot_wider(names_from = sector,
              names_sep = ".",
              values_from = c(prop)) %>%
  arrange(specialty,year)

#Combined

chart8_dynamic <- plyr::rbind.fill(chart8a_dyn_3m,chart8b_dyn_3m)
fwrite(chart8_dynamic,paste0(localgit,"/chart8_dynamic.csv"))

# chart8b_dyn_year <- SU_5_2_raw %>%
#   mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
#                              var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
#                              TRUE ~ "NA"),
#          time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
#                            var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
#                            TRUE ~ "NA")) %>%
#   group_by(year,name,sector,time_year) %>%
#   summarise(prop=sum(prop,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   filter(time_year=="Under a year") %>%
#   pivot_wider(names_from = sector,
#               names_sep = ".",
#               values_from = c(prop)) %>%
#   arrange(name,year)

# chart8a_dyn_year <- SU_5_1_raw %>%
#   mutate(time_year=case_when(var_1 %in% c("0-50","50-100","100-150","150-200","200-250","250-300","300-350") ~ "Under a year",
#                              var_1 %in% c("350-400","400-450","450-500") ~ "Over a year",
#                              TRUE ~ "NA"),
#          time_3m=case_when(var_1 %in% c("0-50","50-100") ~ "Under 3m",
#                              var_1 %in% c("100-150","150-200","200-250","250-300","300-350","350-400","400-450","450-500") ~ "Over 3m",
#                              TRUE ~ "NA")) %>%
#   group_by(year,name,sector,time_year) %>%
#   summarise(prop=sum(prop,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   filter(time_year=="Under a year") %>%
#   pivot_wider(names_from = sector,
#               names_sep = ".",
#               values_from = c(prop)) %>%
#   arrange(name,year)
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
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")

#Get raw data

source('2. Produce descriptive statistics.R')

#################################################################
################### By specialty, IS and pathway ################
#################################################################

#### Combinations to run on

combinations.one <- expand.grid(months=all_months,
                            specialties=all_specialties,
                            pathways=pathways[2:3],
                            independent=c(0:2),
                            ccg="ENGLAND") %>% varhandle::unfactor()
#### Outputs

# out.combinations.one <- pbmapply(ccg_code=combinations.one$ccg,
#                                  dashboard_stats_ccg,
#                                  monthyear=combinations.one$months,
#                                  type=combinations.one$pathways,
#                                  specialty=combinations.one$specialties,
#                                  independent=combinations.one$independent,
#                                  MoreArgs = list(quantiles=c(0.5,0.92,0.95)))
# 
# out.combinations.one.df <- as.data.frame(t(out.combinations.one))
# rownames(out.combinations.one.df) <- 1:nrow(out.combinations.one.df)
# rm(combinations.one,out.combinations.one)

#### Save

# s3write_using(out.combinations.one.df # What R object we are saving
#               , FUN = fwrite # Which R function we are using to save
#               , object = paste0(RTT_subfolder,"/Summary tables/","Admitted by specialty and IS.csv") # Name of the file to save to (include file type)
#               , bucket = IHT_bucket) # Bucket name defined above

#### Load

admitted.by.spec.is <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/Summary tables/","Admitted by specialty and IS.csv") # File to open
                              , bucket = IHT_bucket) # Bucket name defined above

#### Remove combinations without patients

admitted.by.spec.is <- admitted.by.spec.is %>%
  filter(.,total.patients>0)

#### Add date indicator

admitted.by.spec.is <- admitted.by.spec.is %>%
  mutate(.,year_clean=paste0("20",str_sub(monthyear, start= -2)),
         month_clean=substr(monthyear, 1, 3)) %>%
  mutate(.,date_clean=lubridate::ymd(paste(year_clean,month_clean,"01",sep="-"))) %>%
  dplyr::select(.,-c("month_clean","year_clean")) %>%
  mutate(.,COVID_timing=case_when(date_clean<lubridate::dmy("01-03-2020") ~ "Pre-COVID",
                                    date_clean>=lubridate::dmy("01-03-2020")&date_clean<lubridate::dmy("01-06-2021") ~ "COVID",
                                    date_clean>=lubridate::dmy("01-06-2021") ~ "Post-COVID",
                                    TRUE ~ "NA"))
  
#### Summary one

mini_months_count <- admitted.by.spec.is %>%
  filter(.,specialty=="Total",type=="completenonadmitted",independent=="All") %>%
  arrange(date_clean) %>%
  group_by(COVID_timing) %>%
  summarise(n_months=n()) %>% #months_n is the number of months that contributed observations
  ungroup()

summary_one_wide <- admitted.by.spec.is %>%
  filter(.,independent!="All") %>%
  mutate(.,independent=ifelse(independent=="Non-IS","NHS",independent)) %>%
  left_join(.,mini_months_count,by="COVID_timing") %>% 
  group_by(ccg_name,COVID_timing,specialty,type,independent) %>% #Add totals by periods, IS, specialty and pathway
  summarise(n_months=first(n_months),
            total.patients=sum(total.patients,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(ccg_name,COVID_timing,specialty,type) %>%
  mutate(total.is.nhs=sum(total.patients,na.rm=TRUE)) %>% #Add aggregated total NHS/IS so we can compute share
  ungroup() %>%
  mutate(.,share=total.patients/total.is.nhs*100,
         patients_per_month=total.patients/n_months) %>% #Compute patient per month so we can compare periods
  select(.,-c("total.is.nhs","total.patients","n_months")) %>% 
  pivot_wider(
    names_from = independent,
    names_sep = ".",
    values_from = c(patients_per_month,share)
  ) %>%
  pivot_wider(
    names_from = COVID_timing,
    names_sep = ".",
    values_from = c('share.IS','share.NHS','patients_per_month.IS','patients_per_month.NHS')
  ) %>%
  mutate(.,
         stat_specialty_growth_prepost=((`patients_per_month.NHS.Post-COVID`+`patients_per_month.IS.Post-COVID`)/
           (`patients_per_month.NHS.Pre-COVID`+`patients_per_month.IS.Pre-COVID`)-1)*100,
         stat_is_growth_prepost=(`patients_per_month.IS.Post-COVID`-`patients_per_month.IS.Pre-COVID`)/
           `patients_per_month.IS.Pre-COVID`*100,
         stat_nhs_growth_prepost=(`patients_per_month.NHS.Post-COVID`-`patients_per_month.NHS.Pre-COVID`)/
           `patients_per_month.NHS.Pre-COVID`*100,
         stat_is_growth_vs_all_prepost=(`patients_per_month.IS.Post-COVID`-`patients_per_month.IS.Pre-COVID`)/
           (`patients_per_month.IS.Pre-COVID`+`patients_per_month.NHS.Pre-COVID`)*100,
         stat_nhs_growth_vs_all_prepost=(`patients_per_month.NHS.Post-COVID`-`patients_per_month.NHS.Pre-COVID`)/
           (`patients_per_month.IS.Pre-COVID`+`patients_per_month.NHS.Pre-COVID`)*100,
         stat_delta_share_IS=`share.IS.Post-COVID`-`share.IS.Pre-COVID`) %>%
  select(.,specialty,type,starts_with("stat")) %>%
  arrange(.,type,desc(stat_delta_share_IS))

#stat_specialty_growth_prepost 'how much has the specialty grown pre/post COVID'
#stat_is_growth_prepost 'percentage growth of volume in IS sector pre/post COVID'
#stat_is_growth_vs_all_prepost 'how much volume in IS sector has changed, relative to size of specialty pre-COVID'
#stat_delta_share_IS 'change in percentage points of IS share pre/post COVID'

#Biggest change in % IS (in points)
stats_one <- summary_one_wide %>%
  arrange(type,desc(stat_delta_share_IS)) %>%
  select(.,specialty,type,stat_delta_share_IS)

#Biggest change in IS (relative to size of specialty pre-COVID)
stats_two <- summary_one_wide %>%
  arrange(type,desc(stat_is_growth_vs_all_prepost)) %>%
  select(.,specialty,type,stat_is_growth_vs_all_prepost,stat_delta_share_IS)

#################################################################
################### By deprivation, IS and pathway ################
#################################################################

#### Combinations to run on

combinations.two <- expand.grid(months=all_months,
                                imd=1:5,
                                pathways=pathways[2:3],
                                specialty=c("Total","Trauma and Orthopaedic","Ophtalmology","Dermatology"),
                                independent=c(0:2)) %>% varhandle::unfactor()

#### Outputs

out.combinations.two <- pbmapply(dashboard_stats_imd_quintile,
                                 imd=combinations.two$imd,
                                 monthyear=combinations.two$months,
                                 type=combinations.two$pathways,
                                 independent=combinations.two$independent,
                                 specialty=combinations.two$specialty,
                                 MoreArgs = list(quantiles=c(0.5,0.92,0.95)))

out.combinations.two.df <- as.data.frame(t(out.combinations.two))
rownames(out.combinations.two.df) <- 1:nrow(out.combinations.two.df)
rm(combinations.two,out.combinations.two)

#### Save

s3write_using(out.combinations.two.df # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Summary tables/","Admitted by IMD and IS.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#### Load

admitted.by.imd.is <- s3read_using(fread
                                    , object = paste0(RTT_subfolder,"/Summary tables/","Admitted by IMD and IS.csv") # File to open
                                    , bucket = IHT_bucket) # Bucket name defined above

#### Remove combinations without patients

admitted.by.imd.is <- admitted.by.imd.is %>%
  filter(.,total.patients>0)

#### Add date indicator

admitted.by.imd.is <- admitted.by.imd.is %>%
  mutate(.,year_clean=paste0("20",str_sub(monthyear, start= -2)),
         month_clean=substr(monthyear, 1, 3)) %>%
  mutate(.,date_clean=lubridate::ymd(paste(year_clean,month_clean,"01",sep="-"))) %>%
  dplyr::select(.,-c("month_clean")) %>%
  mutate(.,COVID_timing=case_when(date_clean<lubridate::dmy("01-03-2020") ~ "Pre-COVID",
                                  date_clean>=lubridate::dmy("01-03-2020")&date_clean<lubridate::dmy("01-06-2021") ~ "COVID",
                                  date_clean>=lubridate::dmy("01-06-2021") ~ "Post-COVID",
                                  TRUE ~ "NA"))

#### Summary one

mini_months_count <- admitted.by.imd.is %>%
  filter(.,specialty=="Total",type=="completenonadmitted",independent=="All",imd_quintile==1) %>%
  arrange(date_clean) %>%
  group_by(COVID_timing) %>%
  summarise(n_months=n()) %>% #months_n is the number of months that contributed observations
  ungroup()

summary_two_wide <- admitted.by.imd.is %>%
  mutate(.,independent=ifelse(independent=="Non-IS","NHS",independent)) %>%
  left_join(.,mini_months_count,by="COVID_timing") %>%
  group_by(specialty,type,imd_quintile,independent,year_clean) %>%
  summarise(total.patients=sum(total.patients,na.rm=TRUE)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = independent,
    names_sep = ".",
    values_from = c(total.patients)
  ) %>%
  mutate(.,pct_IS=IS/All*100)
  
#Chart 2.1 and chart 2.2

#Chart 2.1

chart_2_1_data <- summary_two_wide %>%
  select(.,specialty,type,imd_quintile,year_clean,pct_IS) %>%
  filter(.,specialty=="Total")

chart_2_1 <- chart_2_1_data %>% 
  ggplot(., aes(x=imd_quintile, y=pct_IS,col=type)) +
  geom_line(size=1) +
  facet_wrap(~year_clean, ncol=5) +
  theme_bw() +
  xlab("IMD quintile of provider - 1 (worst) to 5 (best)") +
  ylab("% of patients treated in independent sector") +
  theme(legend.position="bottom",
        panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

ggsave(plot=chart_2_1, paste0(R_workbench,"/Charts/","chart_2_1.png"), width = 20, height = 10, units = "cm")

#Chart 2.2

chart_2_2 <- chart_2_1_data %>%
  mutate(year_clean=as.numeric(year_clean),
         imd_quintile=ifelse(imd_quintile==1,"1 (most deprived)",imd_quintile),
         imd_quintile=ifelse(imd_quintile==5,"5 (least deprived)",imd_quintile)) %>% 
  ggplot(., aes(x=year_clean, y=pct_IS,col=type)) +
  geom_line(size=1) +
  facet_wrap(~imd_quintile, ncol=5) +
  theme_bw() +
  xlab("Year") +
  ylab("% of patients treated in independent sector") +
  theme(legend.position="bottom",
        panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

ggsave(plot=chart_2_2, paste0(R_workbench,"/Charts/","chart_2_2.png"), width = 20, height = 10, units = "cm")
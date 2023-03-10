##########################################
################### TO-DO ################
##########################################

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
library(sp)
library(leaflet)
library(RColorBrewer)
library(mapview)
library(rgdal)
library(stringi)
library(ggrepel)

#Clean up the global environment
rm(list = ls())

#Directories in S3
source('setup.R') #get project locations in s3 and working directory

# IHT_bucket: s3 project bucket
# RTT_subfolder: folder to place data
# R_workbench: R server working directory
localgit <- dirname(rstudioapi::getSourceEditorContext()$path)

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Months
fy_202122 <- c("RTT-APRIL-2021","RTT-MAY-2021","RTT-JUNE-2021","RTT-JULY-2021",
  "RTT-AUGUST-2021","RTT-SEPTEMBER-2021","RTT-OCTOBER-2021","RTT-NOVEMBER-2021","RTT-DECEMBER-2021",
  "RTT-JANUARY-2022","RTT-FEBRUARY-2022","RTT-MARCH-2022")
pre_COVID <- c(paste("RTT",toupper(month.name),"2017",sep="-"),
  paste("RTT",toupper(month.name),"2018",sep="-"),
  paste("RTT",toupper(month.name),"2019",sep="-"),
  paste("RTT",toupper(month.name)[1:2],"2020",sep="-"))
during_COVID <- c(paste("RTT",toupper(month.name)[3:12],"2020",sep="-"),
                  paste("RTT",toupper(month.name)[1:5],"2021",sep="-"))
post_COVID <- c(paste("RTT",toupper(month.name)[6:12],"2021",sep="-"),
                paste("RTT",toupper(month.name),"2022",sep="-"),
                paste("RTT",toupper(month.name),"2023",sep="-"))

###############################################
################### Load files ################
###############################################

#Provider locations
RTT_provider_locations <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Locating providers/","Google_NHS_postcodes.csv") # File to open
                                       , bucket = IHT_bucket, header=TRUE) # Bucket name defined above

#Provider names
provider_names <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Locating providers/","all_providers.csv") # File to open
                                       , bucket = IHT_bucket, header=TRUE) # Bucket name defined above
provider_names <- provider_names %>%
  group_by(Provider.Org.Code) %>%
  summarise(Provider.Org.Name=first(Provider.Org.Name)) %>%
  ungroup() %>%
  mutate(.,Provider.Org.Name=str_to_title(Provider.Org.Name))

#Provider region
provider_to_IMD_region <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Custom RTT lookups/","provider_to_IMD_region.csv") # File to open
                                       , bucket = IHT_bucket) # Bucket name defined above

#Monthly data
RTT_allmonths <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/","RTT_allmonths_new.csv") # File to open
                              , bucket = IHT_bucket) # Bucket name defined above

#Number of months in data depending on COVID timing

mini_months_COVID <- RTT_allmonths %>%
  mutate(COVID_timing=case_when((toupper(Period) %in% pre_COVID) ~ "Pre",
                                (toupper(Period) %in% during_COVID) ~ "During",
                                (toupper(Period) %in% post_COVID) ~ "Post",
         TRUE ~ "NA")) %>%
  group_by(COVID_timing) %>%
  summarise(n_months=n_distinct(toupper(Period))) %>% 
  ungroup()

#Clean up treatment names

RTT_allmonths <- RTT_allmonths %>%
  filter(.,Commissioner.Org.Code!="NONC") %>% 
  mutate(.,Treatment.Function.Name=str_replace_all(Treatment.Function.Name," Service","")) %>%
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Ear, Nose & Throat (ENT)","Ear Nose and Throat",Treatment.Function.Name)) %>%
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Geriatric Medicine","Elderly Medicine",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Neurosurgical","Neurosurgery",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Trauma & Orthopaedics","Trauma and Orthopaedic",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Medicals","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Mental Healths","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Others","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Paediatrics","Other",Treatment.Function.Name)) %>% 
  mutate(.,Treatment.Function.Name=ifelse(Treatment.Function.Name=="Other - Surgicals","Other",Treatment.Function.Name))

#Provider-level aggregates

#All specialties
number_specialties_by_provider <- RTT_allmonths %>%
  filter(.,Treatment.Function.Name!="Total") %>% #Total is not a specialty, we don't need to count it
  mutate(.,vol_2122=ifelse(toupper(Period) %in% fy_202122,Total.All,NA)) %>% 
  group_by(Provider.Org.Code) %>%
  summarise(IS_status=ifelse(max(IS_provider)==1,"IS","NHS"),
            Provider.Org.Name=first(Provider.Org.Name),
            number_specialties=n_distinct(Treatment.Function.Name), #Number of specialties
            specialties=paste(unique(Treatment.Function.Name),collapse=', '), #List of specialties
            has_opht=max(ifelse(Treatment.Function.Name=="Ophthalmology",1,0)), #Has ophthalmology
            total_vol_2122=sum(vol_2122,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,spec_mix_map=case_when(number_specialties<=1 ~ specialties,
                                number_specialties>1 ~ "Multi-specialty"))

#Merge names and volumes in
RTT_provider_locations <- RTT_provider_locations %>%
  left_join(.,provider_names,by="Provider.Org.Code") %>% #Add names
  left_join(.,select(provider_to_IMD_region,Provider.Org.Code,region),by="Provider.Org.Code") %>% #Add region
  left_join(.,select(number_specialties_by_provider,Provider.Org.Code,IS_status,
                     number_specialties,spec_mix_map,has_opht,total_vol_2122),by="Provider.Org.Code")

#Turn into a shapefile
RTT_providers_shapefile <- SpatialPointsDataFrame(cbind(RTT_provider_locations$long,
                                                        RTT_provider_locations$lat),
                                                  data=RTT_provider_locations,
                                                  proj4string = CRS(latlong))

#####################################################
################### Flourish chart 1 ################
#####################################################

##### Timelines

timelines_raw <- RTT_allmonths %>%
  filter((Treatment.Function.Name %in% c("Total","Ophthalmology"))&
           (RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
                                        "Completed Pathways For Non-Admitted Patients"))) %>%
  group_by(Period,Treatment.Function.Name,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(IS_provider=ifelse(IS_provider=="1","IS","NHS")) %>% 
  pivot_wider(
    names_from = IS_provider,
    names_sep = ".",
    values_from = c(Total.All)
  ) %>%
  mutate(month=stringr::word(Period,2,sep="-") %>% tolower(),
         year=stringr::word(Period,3,sep="-")) %>%
  mutate(Date=lubridate::dmy(paste("01",month,year,sep="-"))) %>%
  mutate(Share=IS/(IS+NHS)*100,
         Total=(IS+NHS)) %>%
  arrange(desc(Treatment.Function.Name),Date) %>%
  select(Period,Date,Treatment.Function.Name,Share,NHS,IS,Total)

  #Pre-post COVID comparisons

timelines_raw %>%
  filter(Treatment.Function.Name=="Total") %>%
  mutate(Period=toupper(Period)) %>% 
  mutate(covid_period=case_when(Period %in% pre_COVID ~ "pre",
                                Period %in% during_COVID ~ "during",
                                Period %in% post_COVID ~ "post",
                                TRUE ~ "NA")) %>%
  group_by(covid_period,Treatment.Function.Name) %>%
  summarise(Share=weighted.mean(Share,Total)) %>% 
  ungroup()

  #Total

flourish1_a <- timelines_raw %>%
  filter(Treatment.Function.Name=="Total") %>% 
  mutate(Grid="Volume of treatments") %>%
  select(Date,Grid,Share,IS,NHS) %>%
  rename(`Independent sector share of total treatments`=Share,
         `Independent sector`=IS) %>%
  mutate(`Independent sector share of total treatments`=NA)

flourish1_b <- timelines_raw %>%
  filter(Treatment.Function.Name=="Total") %>% 
  mutate(Grid="Share of treatments (%)") %>%
  select(Date,Grid,Share,IS,NHS) %>%
  rename(`Independent sector share of total treatments`=Share,
         `Independent sector`=IS) %>%
  mutate(`Independent sector`=NA,NHS=NA)

flourish1 <- plyr::rbind.fill(flourish1_a,flourish1_b)
rm(flourish1_a,flourish1_b)

  #Ophthalmology

flourish2_a <- timelines_raw %>%
  filter(Treatment.Function.Name=="Ophthalmology") %>% 
  mutate(Grid="Number of treatments") %>%
  select(Date,Grid,Share,IS,NHS) %>%
  rename(`Independent sector share of total treatments`=Share,
         `Independent sector`=IS) %>%
  mutate(`Independent sector share of total treatments`=NA)

flourish2_b <- timelines_raw %>%
  filter(Treatment.Function.Name=="Ophthalmology") %>% 
  mutate(Grid="Share of treatments delivered by the independent sector (%)") %>%
  select(Date,Grid,Share,IS,NHS) %>%
  rename(`Independent sector share of total treatments`=Share,
         `Independent sector`=IS) %>%
  mutate(`Independent sector`=NA,NHS=NA)

flourish2 <- plyr::rbind.fill(flourish2_a,flourish2_b)
rm(flourish2_a,flourish2_b)

fwrite(flourish1,
       paste0(R_workbench,"/Charts/","timeline_all.csv"))

fwrite(flourish2,
       paste0(R_workbench,"/Charts/","timeline_ophth.csv"))

##### Pre/post analysis

prepost_raw <- RTT_allmonths %>%
  mutate(Timing=case_when((toupper(Period) %in% pre_COVID) ~ "Pre",
                                (toupper(Period) %in% during_COVID) ~ "During",
                                (toupper(Period) %in% post_COVID) ~ "Post",
                                TRUE ~ "NA")) %>% 
  filter((Treatment.Function.Name %in% c("Trauma and Orthopaedic",
                                         "Gastroenterology",
                                         "Ophthalmology",
                                         "General Surgery",
                                         "Gynaecology",
                                         "Dermatology",
                                         "Urology",
                                         "Neurosurgery",
                                         "Oral Surgery",
                                         "Ear Nose and Throat",
                                         "Plastic Surgery",
                                         "Elderly Medicine",
                                         "Cardiology",
                                         "Neurology",
                                         "Cardiothoracic Surgery",
                                         "Rheumatology"))&
           RTT.Part.Description=="Completed Pathways For Admitted Patients"&
           (Timing %in% c("Pre","Post"))) %>%
  group_by(Timing,Treatment.Function.Name,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(IS_provider=ifelse(IS_provider=="1","IS","NHS")) %>%
  pivot_wider(
    names_from = IS_provider,
    names_sep = ".",
    values_from = c(Total.All)
  ) %>%
  left_join(.,mini_months_COVID,by=c("Timing"="COVID_timing")) %>%
  mutate(TotalPM=(NHS+IS)/n_months,
         Share=IS/(NHS+IS)*100) %>%
  select(-c("NHS","IS","n_months")) %>% 
  pivot_wider(
    names_from = Timing,
    names_sep = ".",
    values_from = c("TotalPM","Share")
  ) 

prepost_a <- prepost_raw %>%
  select(Treatment.Function.Name,TotalPM.Pre,TotalPM.Post) %>%
  mutate(Grid="Number of treatments per month") %>%
  rename(Specialty=Treatment.Function.Name,
         `April 2018 to February 2020`=TotalPM.Pre,
         `June 2021 to October 2022`=TotalPM.Post) %>%
  mutate(Specialty=fct_relevel(Specialty, c("Trauma and Orthopaedic",
                          "Gastroenterology",
                          "Ophthalmology",
                          "General Surgery",
                          "Gynaecology",
                          "Dermatology",
                          "Urology",
                          "Neurosurgery",
                          "Oral Surgery",
                          "Ear Nose and Throat",
                          "Plastic Surgery",
                          "Elderly Medicine",
                          "Cardiology",
                          "Neurology",
                          "Cardiothoracic Surgery",
                          "Rheumatology"))) %>%
  arrange(Specialty)

prepost_b <- prepost_raw %>%
  select(Treatment.Function.Name,Share.Pre,Share.Post) %>%
  mutate(Grid="Share of treatments delivered by the independent sector (%)") %>%
  rename(Specialty=Treatment.Function.Name,
         `April 2018 to February 2020`=Share.Pre,
         `June 2021 to October 2022`=Share.Post) %>%
  mutate(Specialty=fct_relevel(Specialty, c("Trauma and Orthopaedic",
                                            "Gastroenterology",
                                            "Ophthalmology",
                                            "General Surgery",
                                            "Gynaecology",
                                            "Dermatology",
                                            "Urology",
                                            "Neurosurgery",
                                            "Oral Surgery",
                                            "Ear Nose and Throat",
                                            "Plastic Surgery",
                                            "Elderly Medicine",
                                            "Cardiology",
                                            "Neurology",
                                            "Cardiothoracic Surgery",
                                            "Rheumatology"))) %>%
  arrange(Specialty)

flourish3 <- plyr::rbind.fill(prepost_a,prepost_b)
rm(prepost_a,prepost_b)

fwrite(flourish3,
       paste0(R_workbench,"/Charts/","prepost_all.csv"))

#####################################################
################### Map of providers ################
#####################################################

############ Volume

#Only for IS
RTT_providers_shapefile_IS <- subset(RTT_providers_shapefile,IS_status=="IS"&total_vol_2122>1000)

#Data for Flourish
provider_map_flourish <- RTT_providers_shapefile_IS@data %>%
  select(.,Provider.Org.Code,Provider.Postcode,lat,long,
         Provider.Org.Name,region,IS_status,number_specialties,has_opht,
         spec_mix_map,total_vol_2122) %>%
  mutate(log_volume=log(total_vol_2122),
         sqrt_volume=(total_vol_2122)^0.5,
         custom_volume=(total_vol_2122^0.35)/17,
         has_opht_shape=ifelse(has_opht==1,"circle","square"))
  
s3write_using(provider_map_flourish # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/Data for provider map 2122.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#############################################################
################### Timeline of IS providers ################
#############################################################

provider_counts_12m <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  mutate(.,year_clean=paste0("20",str_sub(monthyr, start= -2)),
         month_clean=substr(monthyr, 1, 3)) %>%
  mutate(.,date_clean=lubridate::ymd(paste(year_clean,month_clean,"01",sep="-"))) %>%
  filter(.,Treatment.Function.Name %in% c("Total","Ophthalmology","Gastroenterology"),
         RTT.Part.Description %in% c("Completed Pathways For Non-Admitted Patients",
                                     "Completed Pathways For Admitted Patients")) %>%
  mutate(.,independent=ifelse(IS_provider==1,"IS","NHS")) %>%
  group_by(Treatment.Function.Name,year_clean,independent) %>%
  summarise(nr_providers=n_distinct(Provider.Org.Code),
            total_patients=sum(Total.All, na.rm=TRUE),
            nr_months=n_distinct(month_clean)) %>% 
  ungroup() %>%
  mutate(.,patients_per_month=total_patients/nr_months) %>%
  pivot_longer(!c("Treatment.Function.Name","year_clean","independent","nr_months"),
               names_to = "Variable", values_to = "Count")

provider_counts_3m <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  mutate(.,year_clean=paste0("20",str_sub(monthyr, start= -2)),
         month_clean=substr(monthyr, 1, 3)) %>%
  mutate(.,date_clean=lubridate::ymd(paste(year_clean,month_clean,"01",sep="-"))) %>%
  filter(.,Treatment.Function.Name %in% c("Total","Ophthalmology","Gastroenterology"),
         RTT.Part.Description %in% c("Completed Pathways For Non-Admitted Patients",
                                     "Completed Pathways For Admitted Patients")) %>%
  filter(., month_clean %in% c("Jan","Feb","Mar")) %>% 
  mutate(.,independent=ifelse(IS_provider==1,"IS","NHS")) %>%
  group_by(Treatment.Function.Name,year_clean,independent) %>%
  summarise(nr_providers=n_distinct(Provider.Org.Code),
            total_patients=sum(Total.All, na.rm=TRUE),
            nr_months=n_distinct(month_clean)) %>% 
  ungroup() %>%
  mutate(.,patients_per_month=total_patients/nr_months) %>%
  pivot_longer(!c("Treatment.Function.Name","year_clean","independent","nr_months"),
               names_to = "Variable", values_to = "Count")

provider_counts_chart_bis <- provider_counts_3m %>%
  filter(.,Variable %in% c("nr_providers","patients_per_month"),
         independent=="IS",Treatment.Function.Name=="Ophthalmology") %>%
  mutate(Count_alt=ifelse(year_clean=="2022"&Variable=="nr_providers",NA,Count)) %>%
  arrange(Variable,year_clean) %>% 
  ggplot(.,aes(y=Count_alt, x=year_clean,group=1)) +
  geom_line(lwd=1.5) +
  geom_label_repel(aes(label = round(Count_alt,1)),
                                       nudge_x = 2,
                                       nudge_y = 1,
                                       na.rm = TRUE,size=3,alpha=0.7) +
  facet_wrap(~Variable, scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  xlab("Year") +
  labs(fill = "IMD quintile") +
  ggtitle("Complete pathways (admitted and non-admitted)") +
  theme_dark()  +
  theme(panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

provider_counts_chart_bis

ggsave(plot=provider_counts_chart_bis, paste0(R_workbench,"/Charts/","provider_counts_chart_ophth.png"), width = 20, height = 10, units = "cm")

########################################################################
################### Summarise by region and deprivation ################
########################################################################

#Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
region_pop_2020 <- data.frame(region=c("NORTH EAST","NORTH WEST",
                                        "YORKSHIRE AND THE HUMBER",
                                        "EAST MIDLANDS","WEST MIDLANDS",
                                        "EAST OF ENGLAND","LONDON",
                                        "SOUTH EAST","SOUTH WEST"),
                              pop20=c(2680763,7367456,
                                      5526350,4865583,
                                      5961929,6269161,
                                      9002488,9217265,
                                      5659143))

region_pop_2020 <- region_pop_2020 %>%
  mutate(region=str_to_title(region))

#Comparison by region

completed_region_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>% 
  filter(.,(Period %in% fy_202122),
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients"),
         Treatment.Function.Name=="Total") %>%
  group_by(region,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(region) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(region=str_to_title(region)) %>% 
  left_join(.,region_pop_2020,by="region") %>% 
  mutate(.,pct=Total.All/Total.All.Sectors,
         IS_provider=ifelse(IS_provider==1,"IS","NHS"),
         pathways_per_person=Total.All/pop20*100)

regions_casemix_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>% 
  filter(.,(Period %in% fy_202122),
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients"),
         Treatment.Function.Name!="Total") %>%
  mutate(IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>% 
  group_by(region,IS_provider,Treatment.Function.Name) %>%
  summarise(Total.All.Type.Treat=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(region,IS_provider) %>%
  mutate(Total.All.Sectors=sum(Total.All.Type.Treat,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct_sector=Total.All.Type.Treat/Total.All.Sectors*100,
         region=str_to_title(region)) %>% 
  group_by(region,IS_provider) %>%
  mutate(Treat.Small=ifelse(pct_sector<0,"Other",Treatment.Function.Name)) %>% 
  ungroup() %>%
  group_by(region,IS_provider,Treat.Small) %>%
  summarise(pct_sector=sum(pct_sector),
            Total.All.Type.Treat=sum(Total.All.Type.Treat),
            Total.All.Sectors=first(Total.All.Sectors)) %>% 
  ungroup() %>%
  left_join(.,region_pop_2020,by="region") %>%
  mutate(.,pathways_per_100_person=Total.All.Type.Treat/pop20*100)

case_mix_chart <- regions_casemix_table %>%
  ggplot(., aes(fill=Treat.Small, y=pathways_per_100_person, x=region, label=paste(round(pct_sector,0),"%"))) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 1, position = position_stack(vjust = 0.5)) +
  facet_wrap(~IS_provider,ncol=2) +
  theme_bw() +
  xlab("Sector") +
  ylab("Completed pathways per 100 people (2021/22)") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="bottom",
        panel.border = element_blank(),
        strip.text = element_text(size=5),
        text = element_text(size = 5),
        legend.title=element_text(size=5),
        legend.text=element_text(size=5),
        axis.text = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 5),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 5),
        axis.title.y = element_text(size = 5))

case_mix_chart

ggsave(plot=case_mix_chart, paste0(R_workbench,"/Charts/","region_chart_casemix.png"), width = 20, height = 9, units = "cm")

#Save data
s3write_using(completed_region_table # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/completed_region_table.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#Chart
region_chart <-  completed_region_table %>%
  select(.,region,IS_provider,Total.All,Total.All.Sectors,pathways_per_person,pct) %>%
  mutate(.,region=factor(region),
         pct_IS=ifelse(IS_provider=="IS",pct,NA)) %>%
  ggplot(.) +
  geom_bar(aes(fill=IS_provider, y=pathways_per_person, x=reorder(region,pathways_per_person)),
           position="stack", stat="identity") +
  geom_line(aes(x = reorder(region,Total.All.Sectors), y = 100*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  scale_y_continuous(labels = scales::comma, name="Total pathways per 100 people",
                     sec.axis = sec_axis(~./100, name = "Share of independent sector",labels = scales::percent)) +
  xlab("Region") +
  ggtitle("FY 2021/22, completed pathways (admitted)") +
  labs(fill = "Sector") +
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

region_chart

ggsave(plot=region_chart, paste0(R_workbench,"/Charts/","region_chart.png"), width = 20, height = 9, units = "cm")

#Heat map data

regions_casemix_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>% 
  filter(.,(Period %in% fy_202122),
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
                                     "Completed Pathways For Non-Admitted Patients")) %>%
  mutate(IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>% 
  group_by(region,RTT.Part.Description,IS_provider,Treatment.Function.Name) %>%
  summarise(Total.All.Type.Treat=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(region,IS_provider,RTT.Part.Description) %>%
  mutate(Total.All.Sectors=sum(Total.All.Type.Treat,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct_sector=Total.All.Type.Treat/Total.All.Sectors*100,
         region=str_to_title(region)) %>%
  select(.,region,RTT.Part.Description,IS_provider,Treatment.Function.Name,Total.All.Type.Treat) %>%
  pivot_wider(names_from = IS_provider,
              names_sep = ".",
              values_from = c(Total.All.Type.Treat)) %>%
  mutate(.,All=`IS`+`NHS`,
         pct_IS=`IS`/(`IS`+`NHS`)*100) %>%
  group_by( RTT.Part.Description,Treatment.Function.Name) %>%
  mutate(avg_IS=weighted.mean(pct_IS,All,na.rm=TRUE)) %>% 
  ungroup() %>%
  filter(Treatment.Function.Name %in% c("Total",
                                        "Dermatology",
                                        "Gastroenterology",
                                        "General Internal Medicine",
                                        "General Surgery",
                                        "Gynaecology",
                                        "Neurosurgery",
                                        "Ophthalmology",
                                        "Oral Surgery",
                                        "Trauma and Orthopaedic",
                                        "Urology")) %>% 
  arrange(.,RTT.Part.Description,region,desc(avg_IS))

england_casemix_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>% 
  filter(.,(Period %in% fy_202122),
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
                                     "Completed Pathways For Non-Admitted Patients")) %>%
  mutate(IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>% 
  group_by(RTT.Part.Description,IS_provider,Treatment.Function.Name) %>%
  summarise(Total.All.Type.Treat=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(IS_provider,RTT.Part.Description) %>%
  mutate(Total.All.Sectors=sum(Total.All.Type.Treat,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct_sector=Total.All.Type.Treat/Total.All.Sectors*100) %>%
  select(.,RTT.Part.Description,IS_provider,Treatment.Function.Name,Total.All.Type.Treat) %>%
  pivot_wider(names_from = IS_provider,
              names_sep = ".",
              values_from = c(Total.All.Type.Treat)) %>%
  mutate(.,All=`IS`+`NHS`,
         pct_IS=`IS`/(`IS`+`NHS`)*100) %>%
  group_by(RTT.Part.Description,Treatment.Function.Name) %>%
  mutate(avg_IS=weighted.mean(pct_IS,All,na.rm=TRUE),
         region="England") %>% 
  ungroup() %>%
  filter(Treatment.Function.Name %in% c("Total",
                                        "Dermatology",
                                        "Gastroenterology",
                                        "General Internal Medicine",
                                        "General Surgery",
                                        "Gynaecology",
                                        "Neurosurgery",
                                        "Ophthalmology",
                                        "Oral Surgery",
                                        "Trauma and Orthopaedic",
                                        "Urology")) %>% 
  arrange(.,RTT.Part.Description,region,desc(avg_IS))

casemix_table <- plyr::rbind.fill(regions_casemix_table,england_casemix_table) %>%
  mutate(.,region_order=case_when(region=="England" ~ "1",
                                   region=="North East" ~ "2",
                                   region=="North West" ~ "3",
                                   region=="Yorkshire And The Humber" ~ "4",
                                   region=="East Midlands" ~ "5",
                                   region=="West Midlands" ~ "6",
                                   region=="East Of England" ~ "7",
                                   region=="London" ~ "8",
                                   region=="South East" ~ "9",
                                   region=="South West" ~ "10",
                                   TRUE ~ "NA"),
         Pathway=ifelse(RTT.Part.Description=="Completed Pathways For Admitted Patients",
                        "Admitted","Non-admitted"),
         avg_IS=ifelse(Treatment.Function.Name=="Total",1000,avg_IS)) %>%
  arrange(Pathway,as.numeric(region_order),desc(avg_IS)) %>%
  rename(Specialty="Treatment.Function.Name",
         `Proportion of care delivered by the independent sector 2021/22`="pct_IS") %>% 
  select(.,region,Pathway,Specialty,`Proportion of care delivered by the independent sector 2021/22`)

#Save data
s3write_using(casemix_table # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/regions_casemix_table.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#Comparison by IMD quintile

#Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020
imd_pop_2020 <- data.frame(imd_quintile=c("1 (most deprived)",2:4,"5 (least deprived)"),
                              pop20=c(11301143,11629843,
                                      11485024,11177974,10956154))

completed_imd_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  filter(Treatment.Function.Name=="Total",
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients"),
         (Period %in% fy_202122)) %>% 
  #mutate(year_clean=word(Period,3,sep="-")) %>%
  mutate(year_clean="1") %>%
  group_by(year_clean,IMD19_quintile,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(year_clean,IMD19_quintile) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct=Total.All/Total.All.Sectors,
         IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="1","1 (most deprived)",IMD19_quintile)) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="5","5 (least deprived)",imd_quintile)) %>%
  left_join(.,imd_pop_2020,by="imd_quintile") %>%
  mutate(.,pathways_per_person=Total.All/pop20*100)

completed_imd_table_alt <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  filter(Treatment.Function.Name=="Total",
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients"),
         (Period %in% fy_202122)) %>% 
  #mutate(year_clean=word(Period,3,sep="-")) %>%
  mutate(year_clean="2020/21",
         IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>%
  group_by(year_clean,IMD19_quintile,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(year_clean,IS_provider) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct_quintile=Total.All/Total.All.Sectors) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="1","1 (most deprived)",IMD19_quintile)) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="5","5 (least deprived)",imd_quintile)) %>%
  arrange(.,IS_provider,IMD19_quintile)

imd_alt_flourish <- completed_imd_table_alt %>%
  select(.,IS_provider,imd_quintile,pct_quintile) %>%
  pivot_wider(names_from = imd_quintile,
              names_sep = ".",
              values_from = c(pct_quintile))

#Save data
s3write_using(completed_imd_table # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/completed_imd_table.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above

#Save data
s3write_using(completed_imd_table_alt # What R object we are saving
              , FUN = fwrite # Which R function we are using to save
              , object = paste0(RTT_subfolder,"/completed_imd_table.csv") # Name of the file to save to (include file type)
              , bucket = IHT_bucket) # Bucket name defined above


imd_chart <-  completed_imd_table %>%
  mutate(.,IMD19_quintile=factor(IMD19_quintile),
         pct_IS=ifelse(IS_provider=="IS",pct,NA)) %>%
  ggplot(.) +
  geom_bar(aes(fill=IS_provider, y=pathways_per_person, x=reorder(imd_quintile,IMD19_quintile)),
           position="stack", stat="identity") +
  geom_line(aes(x = reorder(imd_quintile,IMD19_quintile), y = 100*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  scale_y_continuous(labels = scales::comma, name="Total pathways per 100 people",
                     sec.axis = sec_axis(~./100, name = "Share of independent sector",labels = scales::percent))  +
  ggtitle("FY 2021-22, completed pathways (admitted)") +
  xlab("IMD") +
  labs(fill = "Sector") +
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

imd_chart

ggsave(plot=imd_chart, paste0(R_workbench,"/Charts/","imd_chart.png"), width = 20, height = 10, units = "cm")

##################################################
################### Specialty mix ################
##################################################

spec_mix_data_1 <- number_specialties_by_provider %>%
  group_by(IS_status,spec_mix_map) %>%
  summarize(Freq=n()) %>%
  ungroup() %>%
  group_by(IS_status) %>%
  mutate(.,total=sum(Freq)) %>% 
  ungroup() %>%
  mutate(.,pct=Freq/total)

spec_mix_chart_1 <-  spec_mix_data_1 %>%
  filter(.,spec_mix_map %in%
           c("Multi-specialty","Ophthalmology","Dermatology","Trauma and Orthopaedic","Other")) %>% 
  ggplot(aes(y=pct, x=spec_mix_map,fill=IS_status,label=scales::percent(pct))) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  xlab("Specialty mix") +
  scale_y_continuous(name="Percent of providers",labels = scales::percent,limits=c(0,1)) +
  scale_x_discrete(name="Number of providers",limits = c("Multi-specialty","Ophthalmology","Dermatology","Trauma and Orthopaedic","Other")) +
  ggtitle("Mix of specialties by type of provider (2018-2022)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.text = element_text(size=10),
        text = element_text(size = 10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
        axis.title.y = element_text(size = 10))

spec_mix_chart_1

ggsave(plot=spec_mix_chart_1, paste0(R_workbench,"/Charts/","spec_mix_chart_1.png"), width = 20, height = 10, units = "cm")
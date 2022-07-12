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

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")
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
  ungroup()

#Provider region
provider_to_IMD_region <- s3read_using(fread
                                       , object = paste0(RTT_subfolder,"/Custom RTT lookups/","provider_to_IMD_region.csv") # File to open
                                       , bucket = IHT_bucket) # Bucket name defined above

#Monthly data
RTT_allmonths <- s3read_using(fread
                              , object = paste0(RTT_subfolder,"/","RTT_allmonths.csv") # File to open
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
                                number_specialties>1 ~ "Multi-specialty")) %>%
  mutate(.,spec_mix_map2=ifelse(spec_mix_map=="Multi-specialty"&has_opht==1,"Multi-specialty (incl. opht.)",spec_mix_map)) %>% 
  mutate(.,spec_mix_map2=ifelse(spec_mix_map2=="Multi-specialty","Multi-specialty (no opht.)",spec_mix_map2))

#Merge names and volumes in
RTT_provider_locations <- RTT_provider_locations %>%
  left_join(.,provider_names,by="Provider.Org.Code") %>% #Add names
  left_join(.,select(provider_to_IMD_region,Provider.Org.Code,region),by="Provider.Org.Code") %>% #Add region
  left_join(.,select(number_specialties_by_provider,Provider.Org.Code,IS_status,
                     number_specialties,spec_mix_map,spec_mix_map2,has_opht,total_vol_2122),by="Provider.Org.Code")

#Turn into a shapefile
RTT_providers_shapefile <- SpatialPointsDataFrame(cbind(RTT_provider_locations$long,
                                                        RTT_provider_locations$lat),
                                                  data=RTT_provider_locations,
                                                  proj4string = CRS(latlong))

#####################################################
################### Map of providers ################
#####################################################

############ Volume

#Only for IS
RTT_providers_shapefile_IS <- subset(RTT_providers_shapefile,IS_status=="IS"&total_vol_2122>100)

#Data for Flourish
provider_map_flourish <- RTT_providers_shapefile_IS@data %>%
  select(.,Provider.Org.Code,Provider.Postcode,lat,long,
         Provider.Org.Name,region,IS_status,number_specialties,has_opht,
         spec_mix_map,spec_mix_map2,total_vol_2122) %>%
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

provider_counts <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  mutate(.,year_clean=paste0("20",str_sub(monthyr, start= -2)),
         month_clean=substr(monthyr, 1, 3)) %>%
  mutate(.,date_clean=lubridate::ymd(paste(year_clean,month_clean,"01",sep="-"))) %>%
  filter(.,Treatment.Function.Name %in% c("Total","Ophthalmology"),
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

provider_counts_chart_bis <- provider_counts %>%
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

#Comparison by region

region_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>% 
  filter(.,Period %in% fy_202122) %>%
  filter(Treatment.Function.Name=="Total") %>% 
  group_by(region,RTT.Part.Description,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(region,RTT.Part.Description) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct=Total.All/Total.All.Sectors,
         IS_provider=ifelse(IS_provider==1,"IS","NHS"))

region_chart <-  region_table %>%
  select(.,region,RTT.Part.Description,IS_provider,Total.All,Total.All.Sectors,pct) %>%
  filter(RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
                                     "Completed Pathways For Non-Admitted Patients")) %>% 
  mutate(.,region=factor(region),
         pct_IS=ifelse(IS_provider=="IS",pct,NA)) %>%
  ggplot(.) +
  geom_bar(aes(fill=IS_provider, y=Total.All, x=reorder(region,Total.All.Sectors)),
           position="stack", stat="identity") +
  geom_line(aes(x = reorder(region,Total.All.Sectors), y = 10000000*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  facet_wrap(~RTT.Part.Description, ncol=2) +
  scale_y_continuous(labels = scales::comma, name="Total pathways",
                     sec.axis = sec_axis(~./10000000, name = "Share of independent sector",labels = scales::percent)) +
  xlab("Region") +
  ggtitle("FY 2021/22") +
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

#Comparison by IMD quintile

imd_table <- RTT_allmonths %>%
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code") %>%
  filter(Treatment.Function.Name=="Total",
         RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
                                     "Completed Pathways For Non-Admitted Patients"),
         (Period %in% fy_202122)) %>% 
  #mutate(year_clean=word(Period,3,sep="-")) %>%
  mutate(year_clean="1") %>%
  group_by(year_clean,RTT.Part.Description,IMD19_quintile,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(year_clean,RTT.Part.Description,IMD19_quintile,) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct=Total.All/Total.All.Sectors,
         IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="1","1 (most deprived)",IMD19_quintile)) %>%
  mutate(imd_quintile=ifelse(IMD19_quintile=="5","5 (least deprived)",imd_quintile))

imd_chart <-  imd_table %>%
  mutate(.,IMD19_quintile=factor(IMD19_quintile),
         pct_IS=ifelse(IS_provider=="IS",pct,NA)) %>%
  ggplot(.) +
  geom_bar(aes(fill=IS_provider, y=Total.All, x=reorder(imd_quintile,IMD19_quintile)),
           position="stack", stat="identity") +
  geom_line(aes(x = reorder(imd_quintile,IMD19_quintile), y = 10000000*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  facet_wrap(~RTT.Part.Description, ncol=2) +
  scale_y_continuous(labels = scales::comma, name="Total pathways",
                     sec.axis = sec_axis(~./10000000, name = "Share of independent sector",labels = scales::percent))  +
  ggtitle("FY 2021-22") +
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
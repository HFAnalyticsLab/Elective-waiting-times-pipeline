##########################################
################### TO-DO ################
##########################################

#Which providers are multi-specialty? Map by number of specialties or by main specialty

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

#Clean up the global environment
rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
RTT_subfolder <- "RTT waiting times data"
R_workbench <- path.expand("~")
localgit <- dirname(rstudioapi::getSourceEditorContext()$path)

#Get raw data
#source('2. Produce descriptive statistics.R')

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

#Remove duplicates
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

#Volume and IS status by provider in 2021-22
# provider_volumes_IS_2122 <- RTT_allmonths %>%
#   filter(.,Period %in% fy_202122) %>%
#   filter(.,Treatment.Function.Name!="Total") %>% #Total is not a specialty
#   group_by(Provider.Org.Code) %>%
#   summarise(Provider.Org.Name=first(Provider.Org.Name),
#             Total.All=sum(Total.All,na.rm=TRUE),
#             IS=max(IS_provider)) %>%
#   ungroup()

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

#Ophthalmology
ophthalmology_volumes <- RTT_allmonths %>%
  filter(.,Treatment.Function.Name=="Ophthalmology",
         RTT.Part.Description=="Completed Pathways For Admitted Patients") %>% 
  mutate(.,vol_pre_COVID=ifelse(toupper(Period) %in% pre_COVID,Total.All,NA),
         vol_during_COVID=ifelse(toupper(Period) %in% during_COVID,Total.All,NA),
         vol_post_COVID=ifelse(toupper(Period) %in% post_COVID,Total.All,NA)) %>% 
  group_by(Provider.Org.Code) %>%
  summarise(total_vol_pre_COVID=sum(vol_pre_COVID,na.rm=TRUE),
            total_vol_during_COVID=sum(vol_during_COVID,na.rm=TRUE),
            total_vol_post_COVID=sum(vol_post_COVID,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,opht_monthly_vol_pre_COVID=total_vol_pre_COVID/filter(mini_months_COVID,COVID_timing=="Pre")$n_months, #Monhly volumes
         opht_monthly_vol_during_COVID=total_vol_during_COVID/filter(mini_months_COVID,COVID_timing=="During")$n_months,
         opht_monthly_vol_post_COVID=total_vol_post_COVID/filter(mini_months_COVID,COVID_timing=="Post")$n_months) %>%
  mutate(.,opht_prepost_COVID_pct_change=(opht_monthly_vol_post_COVID-opht_monthly_vol_pre_COVID)/opht_monthly_vol_pre_COVID*100,
         opht_prepost_COVID=(opht_monthly_vol_post_COVID-opht_monthly_vol_pre_COVID)) %>%
  select(.,Provider.Org.Code,starts_with("opht"))

# ophthalmology_volumes %>%
#   filter(.,opht_prepost_COVID<0) %>%
#   pull(.,opht_prepost_COVID) %>%
#   quantile(., probs = seq(0, 1, 1/3))
# 
# ophthalmology_volumes %>%
#   filter(.,opht_prepost_COVID>=0) %>%
#   pull(.,opht_prepost_COVID) %>%
#   quantile(., probs = seq(0, 1, 1/3))

# delta_data <- number_specialties_by_provider %>%
#   select(.,Provider.Org.Code,Provider.Org.Name,monthly_vol_pre_COVID,monthly_vol_post_COVID,
#          prepost_COVID_pct_change) %>%
#   arrange(desc(prepost_COVID_pct_change))

#Merge names and volumes in
RTT_provider_locations <- RTT_provider_locations %>%
  left_join(.,provider_names,by="Provider.Org.Code") %>% #Add names
  left_join(.,select(provider_to_IMD_region,Provider.Org.Code,region),by="Provider.Org.Code") %>% #Add region
  left_join(.,select(number_specialties_by_provider,Provider.Org.Code,IS_status,
                     number_specialties,spec_mix_map2,has_opht,total_vol_2122),by="Provider.Org.Code") %>%
  left_join(.,ophthalmology_volumes,by="Provider.Org.Code") 

#Turn into a shapefile
RTT_providers_shapefile <- SpatialPointsDataFrame(cbind(RTT_provider_locations$long,
                                                        RTT_provider_locations$lat),
                                                  data=RTT_provider_locations,
                                                  proj4string = CRS(latlong))

############################################################
################### Data for Flourish map 1 ################
############################################################

#Import Region shapefile

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Read shapefile
setwd(paste0(R_workbench,"/Shapefiles/Regions"))
Region_shapefile <- readOGR(dsn=".", layer="RGN_DEC_2021_EN_BUC") 
Region_shapefile <- spTransform(Region_shapefile, CRS(latlong))

#Get centroids
centroids <- rgeos::gCentroid(Region_shapefile, byid = TRUE)
centroids_data <- data.frame(region=Region_shapefile@data$RGN21NM,
                             lat=centroids@coords[,2],
                             long=centroids@coords[,1]) %>%
  mutate(.,lat=ifelse(region=="South East",50.8229,lat),
         long=ifelse(region=="South East",0.1363,long))
rm(centroids,Region_shapefile)

#Flourish data
flourish_region_data <- RTT_provider_locations %>%
  filter(.,has_opht==1) %>%
  group_by(region,IS_status) %>%
  summarise(opht_monthly_vol_pre_COVID=sum(opht_monthly_vol_pre_COVID,na.rm=TRUE),
            opht_monthly_vol_during_COVID=sum(opht_monthly_vol_during_COVID,na.rm=TRUE),
            opht_monthly_vol_post_COVID=sum(opht_monthly_vol_post_COVID,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(COVID_delta=round(opht_monthly_vol_post_COVID-opht_monthly_vol_pre_COVID,0),
         COVID_delta_abs=abs(opht_monthly_vol_post_COVID-opht_monthly_vol_pre_COVID)) %>%
  select(.,region,IS_status,COVID_delta,COVID_delta_abs) %>%
  left_join(.,centroids_data,by="region") %>%
  mutate(.,long.bis=ifelse(IS_status=="IS",long+0.2,long-0.2),
         symbol=ifelse(COVID_delta<=0,"arrow-down","arrow-up"),
         col=ifelse(IS_status=="IS","green","blue")) %>%
  arrange(.,desc(COVID_delta_abs)) %>%
  mutate(size_function=sqrt(COVID_delta_abs)*0.10) %>%
  select(.,region,IS_status,lat,long.bis,size_function,symbol,col,COVID_delta) %>%
  arrange(.,region,IS_status)

# RTT_provider_locations %>%
#   filter(.,has_opht==1) %>%
#   group_by(region) %>%
#   summarise(opht_monthly_vol_pre_COVID=sum(opht_monthly_vol_pre_COVID,na.rm=TRUE),
#             opht_monthly_vol_during_COVID=sum(opht_monthly_vol_during_COVID,na.rm=TRUE),
#             opht_monthly_vol_post_COVID=sum(opht_monthly_vol_post_COVID,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(COVID_delta=round(opht_monthly_vol_post_COVID-opht_monthly_vol_pre_COVID,0))

############################################################
################### Data for Flourish map 2 ################
############################################################

# #Projection codes
# ukgrid = "+init=epsg:27700"
# latlong="+init=epsg:4326"
# 
# #Flourish data
# flourish_animated_data <- RTT_allmonths %>%
#   filter(.,Treatment.Function.Name=="Ophthalmology",
#          RTT.Part.Description=="Completed Pathways For Admitted Patients") %>%
#   select(.,Period,Provider.Org.Code,Provider.Org.Name,Total.All,IS_provider,monthyr) %>%
#   left_join(.,select(RTT_provider_locations,Provider.Org.Code,lat,long),by="Provider.Org.Code") %>%
#   mutate(.,year=paste0("20",str_sub(monthyr, start= -2)),
#          month=tolower(word(Period,2,sep="-"))) %>%
#   mutate(date_start=lubridate::ymd(paste(year,month,"01",sep="-"))) %>%
#   mutate(date_end=lubridate::ceiling_date(date_start, "month") - lubridate::days(1)) %>% 
#   mutate(log_size=log(Total.All+1))
# 
# fwrite(flourish_animated_data,paste0(R_workbench,"/Charts/flourish_animated_data.csv"))

#####################################################
################### Map of providers ################
#####################################################

############ Volume

#Only for IS
RTT_providers_shapefile_IS <- subset(RTT_providers_shapefile,IS==1&(!is.na(total_vol_2122)))

#See values
table(RTT_providers_shapefile_IS$IS,useNA="always")
hist(RTT_providers_shapefile_IS$total_vol_2122)
quantile(RTT_providers_shapefile_IS$total_vol_2122, probs = seq(0, 1, 1/7))

# Create a color palette with handmade bins
quant_number <- 7

mypalette <- colorQuantile("RdYlBu", RTT_providers_shapefile_IS$total_vol_2122, reverse = TRUE, n = quant_number)
mypalette_cols <- brewer.pal(quant_number, "RdYlBu")[quant_number:1]
quantiles <- quantile(RTT_providers_shapefile_IS$total_vol_2122, probs = seq(0, 1, 1/quant_number))
quantiles_labels <- rep(NA,quant_number)
for (j in 1:quant_number){
  quantiles_labels[j] <- paste0(formatC(quantiles[j],format="f", big.mark=",", digits=0)," to ",
                                formatC(quantiles[(j+1)],format="f", big.mark=",", digits=0))
}

# Prepare the text for the tooltip:
mytext <- paste(
  "Code: ", RTT_providers_shapefile_IS$Provider.Org.Code, "<br/>", 
  "Name: ", RTT_providers_shapefile_IS$Provider.Org.Name, "<br/>",
  "Volume: ", formatC(RTT_providers_shapefile_IS$total_vol_2122, format="f", big.mark=",", digits=0), "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
#addLegend(pal=mypalette, values=~total_vol_2122, opacity=0.9, title = "Patient volume", position = "topright" )

map_IS_providers <- leaflet(RTT_providers_shapefile_IS, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(~long, ~lat, stroke = TRUE,
                   color = "black",weight = 1,opacity = 0.3,
                   fillColor = ~mypalette(total_vol_2122), fillOpacity = 0.7, radius=6,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend(colors=mypalette_cols, labels=quantiles_labels,
            opacity=0.9, title = "Patient volume in 2021/22", position = "topright" )

map_IS_providers
mapshot(map_IS_providers, file = paste0(R_workbench,"/Charts/","IS_providers_map_2122.png"))

############ Volume and specialty mix

#Only for IS and without small specialties
RTT_providers_shapefile_specmap <- subset(RTT_providers_shapefile,(!is.na(total_vol_2122)),IS==1&(!(spec_mix_map2 %in% c("Urology","Other"))))
RTT_providers_shapefile_specmap@data$sqrt.volume <- (RTT_providers_shapefile_specmap@data$total_vol_2122)^0.5

# Create a color palette with handmade bins

levels2 <- unique(RTT_providers_shapefile_specmap$spec_mix_map2)
mypalette2_cols <- brewer.pal(length(unique(RTT_providers_shapefile_specmap$spec_mix_map2)), "Set1")
#mypalette2_cols[1] <- "#B2DF8A"
mypalette2 <- colorFactor(palette=mypalette2_cols, levels=levels2,na.color = "#808080")

# Prepare the text for the tooltip:
mytext2 <- paste(
  "Code: ", RTT_providers_shapefile_specmap$Provider.Org.Code, "<br/>", 
  "Name: ", RTT_providers_shapefile_specmap$Provider.Org.Name, "<br/>",
  "Specialty: ", RTT_providers_shapefile_specmap$spec_mix_map2, "<br/>",
  "Volume: ", formatC(RTT_providers_shapefile_specmap$total_vol_2122, format="f", big.mark=",", digits=0), "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
#addLegend(pal=mypalette, values=~total_vol_2122, opacity=0.9, title = "Patient volume", position = "topright" )

map_IS_providers2 <- leaflet(RTT_providers_shapefile_specmap, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(~long, ~lat, stroke = TRUE,
                   color = "black",weight = 1,opacity = 0.3,
                   fillColor = ~mypalette2(spec_mix_map2), fillOpacity = 0.7, radius=~sqrt.volume/30,
                   label = mytext2,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend(colors=mypalette2_cols, labels=levels2,
            opacity=0.9, title = "Specialty", position = "topright" )
  
map_IS_providers2

mapshot(map_IS_providers2, file = paste0(R_workbench,"/Charts/","IS_providers_spec_map_2122.png"))

########################################################################
################### Summarise by region and deprivation ################
########################################################################

#Read in data and merge

RTT_allmonths <- RTT_allmonths %>% 
  left_join(.,provider_to_IMD_region,by="Provider.Org.Code")

#Comparison by region

region_table <- RTT_allmonths %>%
  filter(.,Period %in% fy_202122) %>%
  group_by(region,IS_provider) %>%
  filter(Treatment.Function.Name=="Total") %>% 
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(region) %>%
  mutate(Total.All.Sectors=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(.,pct=Total.All/Total.All.Sectors,
         IS_provider=ifelse(IS_provider==1,"IS","NHS"))

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

region_chart <-  region_table %>%
  select(.,region,IS_provider,Total.All,Total.All.Sectors,pct) %>%
  mutate(.,region=factor(region),
         pct_IS=ifelse(IS_provider=="IS",pct,NA)) %>%
  ggplot(.) +
  geom_bar(aes(fill=IS_provider, y=Total.All, x=reorder(region,Total.All.Sectors)),
           position="stack", stat="identity") +
  geom_line(aes(x = reorder(region,Total.All.Sectors), y = 100000000*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  scale_y_continuous(labels = scales::comma, name="Total pathways",
                     sec.axis = sec_axis(~./100000000, name = "Share of independent sector",labels = scales::percent)) +
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

ggsave(plot=region_chart, paste0(R_workbench,"/Charts/","region_chart.png"), width = 20, height = 10, units = "cm")

#Comparison by IMD quintile

imd_table <- RTT_allmonths %>%
  filter(.,Period %in% fy_202122) %>%
  group_by(IMD19_quintile,IS_provider) %>%
  summarise(Total.All=sum(Total.All,na.rm=TRUE)) %>% 
  ungroup() %>%
  group_by(IMD19_quintile) %>%
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
  geom_line(aes(x = reorder(imd_quintile,IMD19_quintile), y = 200000000*pct_IS,group = IS_provider),
            size = 1.5, color="red") +
  scale_y_continuous(labels = scales::comma, name="Total pathways",
                     sec.axis = sec_axis(~./200000000, name = "Share of independent sector",labels = scales::percent))  +
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
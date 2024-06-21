
## all specialties on 1 chart

library(arrow)
library(grid)

data <- read_csv_arrow('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/RTT%20waiting%20times%20data/RTT_processed.csv')

chart_data <- data %>%
              filter(type %in% c('completeadmitted', 'completenonadmitted')) %>%
              filter(!(specialty %in% c('Cardiology', 'Cardiothoracic Surgery', 'General Internal Medicine',
                                        'General Medicine', 'Neurology', 'Respiratory Medicine', 
                                        'Rheumatology', 'Thoracic Medicine', 'Elderly Medicine',
                                        'Total'))) %>%
              select(date, specialty, type, independent, total.patients)

totals <- chart_data %>%
          group_by(date, specialty, type) %>%
          summarise(volume.total = sum(total.patients))

chart_data <- merge(chart_data, totals, by = c('date', 'specialty', 'type')) %>%
              filter(independent == 'IS') %>%
              select(-independent)
chart_data$prop <- chart_data$total.patients / chart_data$volume.total

chart_data$type[chart_data$type == 'completeadmitted'] <- 'Complete admitted'
chart_data$type[chart_data$type == 'completenonadmitted'] <- 'Complete non-admitted'

chart_data$colour <- ifelse(chart_data$specialty %in% c('Trauma and Orthopaedic',
                                                        'Gastroenterology',
                                                        'Ophthalmology'),
                            '#EE2C2C', '#E0EEE0')
chart_data$line <- case_match(chart_data$specialty,
                              'Trauma and Orthopaedic' ~ 'dashed',
                              'Gastroenterology' ~ 'dotted',
                              'Ophthalmology' ~ 'longdash',
                              .default = 'solid')
## lockdown period for charts:
lockdown <- annotate('rect',
                     xmin = as.Date(c('2020-03-26')),
                     xmax = as.Date(c('2021-03-08')),
                     ymin = 0, ymax = Inf, alpha = 0.2) 
covid_note <- textGrob('covid lockdown periods',
                       gp = gpar(fontsize = 7))
ggplot(chart_data,
       aes(x = date, y = prop, group = specialty, color = colour)) +
       geom_line() +
#       scale_fill_manual(values=c("#999999", "#E69F00")) +
#       scale_color_manual(colour) +
       annotation_custom(covid_note, ymin = 0.4) +
       facet_wrap(~type) +
       ylab('Proportion of patients in IS care') + 
       theme_minimal() + 
       lockdown

#### Collect data for 2023/24 heatmap       
a <- read_csv_arrow('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/RTT%20waiting%20times%20data/IS_providers_allmonths.csv') %>%
  rbind(read_csv_arrow('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/RTT%20waiting%20times%20data/providers_allmonths.csv'))
full_Providers <- unique(a$codes)

fy_202324 <- c("RTT-APRIL-2023","RTT-MAY-2023","RTT-JUNE-2023","RTT-JULY-2023","RTT-AUGUST-2023",
               "RTT-SEPTEMBER-2023","RTT-OCTOBER-2023","RTT-NOVEMBER-2023","RTT-DECEMBER-2023",
               "RTT-JANUARY-2024","RTT-FEBRUARY-2024","RTT-MARCH-2024")

a <- a %>%
      filter(region %in% c('Y56','Y59','Y58','Y60','Y63','Y61','Y62') &
            monthyr %in% c("Apr23", "May23", "Jun23", "Jul23", "Aug23", "Sep23", "Oct23", "Nov23",
                           "Dec23", "Jan24", "Feb24", "Mar24")) %>%
      select(codes, region) %>% 
      unique() %>%
      filter(!(codes %in% c('NTX', 'T8R5I', 'Y8L9S', 'RPG'))) # these codes cause duplication issues, dealt with manually later

## population figures from here
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates
region_pop_2022 <- data.frame(name=c('London', 'South East', 'South West',
                                     'Midlands', 'North East and Yorkshire',
                                     'East of England', 'North West'),
                              region=c('Y56','Y59','Y58','Y60','Y63','Y61','Y62'),
                              pop20=c(8866180,9073701,5771573,10956592,8540584,6697937,7199831))

## get data from full RTT
reg_join <- RTT_allmonths %>%
  filter(toupper(Period) %in% fy_202324 &
         RTT.Part.Description == 'Completed Pathways For Admitted Patients') %>%
  select(-starts_with('Gt.')) %>%
  mutate(IS_provider=ifelse(IS_provider==1,"IS","NHS")) %>%
  left_join(.,a,by = c("Provider.Org.Code" = "codes"))

#### manually add conflicting region providers ####

reg_join$region[is.na(reg_join$region) & reg_join$Provider.Org.Name == 'SPAMEDICA BROMLEY'] <- 'Y56'
reg_join$region[is.na(reg_join$region) & reg_join$Provider.Org.Name == 'SPAMEDICA WOKINGHAM'] <- 'Y59'
reg_join$region[is.na(reg_join$region) & 
                  reg_join$Provider.Org.Name == 'THE ONE HEALTH GROUP LTD' & 
                  reg_join$Provider.Parent.Name == 'NHS DERBY AND DERBYSHIRE INTEGRATED CARE BOARD'] <- 'Y60'
reg_join$region[is.na(reg_join$region) & 
                  reg_join$Provider.Org.Name == 'THE ONE HEALTH GROUP LTD' & 
                  reg_join$Provider.Parent.Name == 'NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD'] <- 'Y63'
reg_join$region[is.na(reg_join$region) & reg_join$Provider.Org.Name == 'THE ONE HEALTH GROUP PLC'] <- 'Y56'
reg_join$region[is.na(reg_join$region) & 
                  reg_join$Provider.Org.Name == 'OXLEAS NHS FOUNDATION TRUST' & 
                  reg_join$Provider.Parent.Name == 'NHS KENT AND MEDWAY INTEGRATED CARE BOARD'] <- 'Y59'
reg_join$region[is.na(reg_join$region) & 
                  reg_join$Provider.Org.Name == 'OXLEAS NHS FOUNDATION TRUST' & 
                  reg_join$Provider.Parent.Name == 'NHS SOUTH EAST LONDON INTEGRATED CARE BOARD'] <- 'Y56'
#### end ####

## which specialties have highest volume over the year?
reg_join %>%
  select(Treatment.Function.Name, Total.All) %>%
  group_by(Treatment.Function.Name) %>%
  summarise(Total = sum(Total.All)) %>%
  arrange(-Total) %>%
  filter(Treatment.Function.Name != 'Other') %>%
  head(11)

# region casemix table ####
regions_casemix <- reg_join %>%
  filter(Treatment.Function.Name %in% c("Total",
                                        "Ophthalmology",
                                        "Trauma and Orthopaedic",
                                        "General Surgery",
                                        "Urology",
                                        "Gastroenterology",
                                        "Gynaecology",
                                        "Oral Surgery",
                                        "Plastic Surgery",
                                        "Ear Nose and Throat",
                                        "Dermatology")) %>% 
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
  arrange(.,RTT.Part.Description,region,desc(avg_IS))
# end ####

# England casemix table ####
england_casemix_table <- reg_join %>%
  filter(Treatment.Function.Name %in% c("Total",
                                        "Ophthalmology",
                                        "Trauma and Orthopaedic",
                                        "General Surgery",
                                        "Urology",
                                        "Gastroenterology",
                                        "Gynaecology",
                                        "Oral Surgery",
                                        "Plastic Surgery",
                                        "Ear Nose and Throat",
                                        "Dermatology")) %>% 
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
  arrange(.,RTT.Part.Description,region,desc(avg_IS))

# Combine
casemix_table <- plyr::rbind.fill(regions_casemix,england_casemix_table) %>%
  mutate(.avg_IS=ifelse(Treatment.Function.Name=="Total",1000,avg_IS)) %>%
  arrange(RTT.Part.Description,region,desc(avg_IS)) %>%
  rename(Specialty="Treatment.Function.Name",
         `Proportion of care delivered by the independent sector 2023/24`="pct_IS") %>% 
  select(.,region,RTT.Part.Description,Specialty,`Proportion of care delivered by the independent sector 2023/24`)



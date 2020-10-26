##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Scrape addresses of files

##############################################
################### SETUP ####################
##############################################

###### Libraries ######

if (!require("pacman")) install.packages("pacman")

pacman::p_load(RSelenium, seleniumPipes, tidyverse,dplyr,rvest,downloader,
               stringr,qdapRegex,tidyr,purrr,pbapply,data.table,readr,readxl)

rm(list = ls()) #Clear the working environment

rawdatadir <- "M:/Analytics/Elective waiting times data"

#####################################################
################### Web-scraping ####################
#####################################################

#2019-2020
months2021 <- c("Apr","May","Jun","Jul","Aug")
years2021 <- c(rep(20,length(months2021)))
series2021 <- rep(2021,length(months2021))
input2021 <- cbind.data.frame(month=paste0(months2021,years2021),series=series2021)

#2019-2020
months1920 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years1920 <- c(rep(19,9),rep(20,3))
series1920 <- rep(1920,12)
input1920 <- cbind.data.frame(month=paste0(months1920,years1920),series=series1920)

#2018-2019
months1819 <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
years1819 <- c(rep(18,9),rep(19,3))
series1819 <- rep(1819,12)
input1819 <- cbind.data.frame(month=paste0(months1819,years1819),series=series1819)

#All together

inputs <- plyr::rbind.fill(input2021,input1920,input1819)

#Function that reports links to 3 files for each month

return_links_rtt <- function(month,series){
  
  if (series=="2021"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/")
  } else if (series=="1920"){
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/")
  } else if (series=="1819") {
    read.first.page <- read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/")
  }
  
  xpath_month <- paste0("//a[contains(text(),'",month,"')]/@href")
  
  links <- read.first.page %>%
    html_nodes(xpath=xpath_month) %>%
    html_text()
  
  full.csv.link <- links[str_detect(links, "Full-CSV")][1]
  
  providers.link.incomp <- links[str_detect(links, "Incomplete-Provider")][1]
  
  providers.link.new <- links[str_detect(links, "New-Periods-Provider")][1]
  
  providers.link.adm <- links[str_detect(links, "Admitted-Provider")][1]
  
  providers.link.nonadm <- links[str_detect(links, "NonAdmitted-Provider")][1]
  
  out <- data.frame(month=month,full.csv.link,
                    providers.link.incomp,
                    providers.link.new,
                    providers.link.adm,
                    providers.link.nonadm)
  
  return(out)
  
}

#Apply for all months to get all links

links.out <- mapply(return_links_rtt,
                    month=inputs$month,
                    series = inputs$series)

links.out.df <- as.data.frame(t(links.out))

###########################################################
################### Download all files ####################
###########################################################

for (k in 1:nrow(links.out.df)){
  
  #Create directory
  setwd(rawdatadir)
  dir.create(as.character(links.out.df$month[k]))
  setwd(as.character(links.out.df$month[k]))
  
  #Download Full CSV
  download(as.character(links.out.df$full.csv.link[k]),
           dest=paste0(links.out.df$month[k],".zip"), mode="wb")
  
  #Unzip Full CSV
  unzip(paste0(links.out.df$month[k],".zip"), exdir = '.')
  
  #Download New Providers
  download(as.character(links.out.df$providers.link.new[k]),
           dest=paste0(links.out.df$month[k],"-newproviders.xls"), mode="wb")
  
  #Download Admitted Providers
  download(as.character(links.out.df$providers.link.adm[k]),
           dest=paste0(links.out.df$month[k],"-providers-admitted.xls"), mode="wb")
  
  #Download Non-Admitted Providers
  download(as.character(links.out.df$providers.link.nonadm[k]),
           dest=paste0(links.out.df$month[k],"-providers-nonadmitted.xls"), mode="wb")
  
  #Download Incomplete Providers
  download(as.character(links.out.df$providers.link.incomp[k]),
           dest=paste0(links.out.df$month[k],"-providers-incomplete.xls"), mode="wb")  
}

###########################################################################
################### Append all provider files into one ####################
###########################################################################

for (s in 1:nrow(links.out.df)){

  setwd(rawdatadir)
  setwd(as.character(links.out.df$month[s]))
  
  incomplete <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.xls"),
                           sheet = "IS Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links.out.df$month[s],"-providers-incomplete.xls"),
                           sheet = "IS Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links.out.df$month[s],"-newproviders.xls"),
                             sheet = "IS Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-admitted.xls"),
                             sheet = "IS Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links.out.df$month[s],"-providers-nonadmitted.xls"),
                                sheet = "IS Provider",skip=13)
  
  #Any new ones in 'incomplete' files?
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  
  summary_month <- data.frame(monthyr=rep(as.character(links.out.df$month[s]),length(codes)),codes,names)
  
  if (s==1) {
    storage <- summary_month
  } else {
    storage <- plyr::rbind.fill(storage,summary_month)
  }
  
}

#Remove duplicates
IS_providers_allmonths <- storage[!duplicated(storage), ]

#Save
setwd(rawdatadir)
fwrite(IS_providers_allmonths, file = paste0(rawdatadir,"/Clean/IS_providers_allmonths.csv"), sep = ",")

########################################################################################
################### Append all monthly waiting times files into one ####################
########################################################################################

#Re-load IS provider by month
IS_providers_allmonths <- fread(paste0(rawdatadir,"/Clean/IS_providers_allmonths.csv"), header=TRUE, sep=",", check.names=T)

for (j in 1:nrow(links.out.df)){
  
  setwd(rawdatadir)
  setwd(as.character(links.out.df$month[j]))
  
  file.name <- list.files()[str_detect(list.files(),"full-extract")]
  
  RTT_month <- fread(file.name, header=TRUE, sep=",", check.names=T)
  
  RTT_month$monthyr <- as.character(links.out.df$month[j])
  
  #New indicator variable to flag independent providers
  RTT_month$IS_provider <- ifelse(RTT_month$Provider.Org.Code %in% filter(IS_providers_allmonths,monthyr==links.out.df$month[j])$codes,1,0)
  
  if (j==1) {
    storage.rtt <- RTT_month
  } else {
    storage.rtt <- plyr::rbind.fill(storage.rtt,RTT_month)
  }
  
}

#Save
setwd(rawdatadir)
fwrite(storage.rtt, file = paste0(rawdatadir,"/Clean/RTT_allmonths.csv"), sep = ",")

##############################################################
################### Alternative: Selenium ####################
##############################################################

# driver$server$stop()
# 
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# 
# ### Open the page
# 
# binman::list_versions("chromedriver")
# 
# driver <- rsDriver(browser=c("chrome"), chromever="85.0.4183.83")
# 
# remote_driver <- driver[["client"]]
# 
# remote_driver$navigate("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/")
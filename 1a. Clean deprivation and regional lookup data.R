##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,DescTools,data.table,rgdal,
               tibble,leaflet,raster,plotly,
               pbapply,pbmcapply,here,readxl,varhandle)

#Clean up the global environment
rm(list = ls())

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Elective waiting times data"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#########################################################################
################### Mapping CCGs to deprivation and region ##############
#########################################################################

# Columns in this file are as follows: 
#   • CCG Code
# • CCG Name
# • RAvgRank = Rank of Average Rank
# • RAvgScor = Rank of Average Score
# • RPLMD10 = Rank of Proportion of LSOAs Most Deprived 10%

CCG_to_higher <- fread(paste0(rawdatadir,"/Lookups/Clinical_Commissioning_Group_to_NHS_England_(Region,_Local_Office)_and_NHS_England_(Region)_(April_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

CCG_to_IMD19 <- readOGR(dsn=paste0(rawdatadir,"/Shapefiles/Clinical_Commissioning_Group_(CCG)_IMD_2019_(OSGB1936)"), layer="c8aa66b8-d408-44b9-9641-ea45fb3344f02020315-1-68y7uz.trdmv")

CCG_to_IMD19 <- spTransform(CCG_to_IMD19, CRS(latlong)) #Set to the same projection

CCG_to_IMD19_data <- CCG_to_IMD19@data

rm(CCG_to_IMD19)

CCG_to_IMD19_data <- CCG_to_IMD19_data %>%
  mutate(.,IMD19_decile = ntile(RAvgScor, 10),IMD19_quintile=ntile(RAvgScor, 5)) %>%
  mutate(.,ccg19nm=toupper(ccg19nm))

CCG_to_IMD19_data <- left_join(CCG_to_IMD19_data,select(CCG_to_higher,CCG19CD,NHSER19CD,NHSER19NM),
                               by=c("ccg19cd"="CCG19CD"))

rm(CCG_to_higher)

#######################################
################### Save ##############
#######################################

fwrite(CCG_to_IMD19_data, file = paste0(rawdatadir,"/CCG_to_IMD19_data.csv"), sep = ",")
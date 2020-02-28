# 022720
# i will replace this with interpolated data processed through Owen's pipeline, that includes blwh predictions matched to the 5km grid using a nearest neighbor function

library(foreign)
library(lubridate)
library(tidyverse)
library(reshape2)
library(scales)
library(zoo)
library(ggrepel)
library(sf)
library(data.table)
library(wesanderson)
library(viridis)
library(here)
#library(ggerr)

# load RDS
dcrb_ca_vms_tix_analysis <- readRDS("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_2009-2018_fishtix_blue_whales_grids.RDS")
head(dcrb_ca_vms_tix_analysis)

# these data are summarized by year-month, too coarse
#load("~/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/Scenario_Analysis_Data_2009_2018.RData")

# a bit of pre-processing
# there are way fewer GRID5KM_ID in the DCRB VMS data than in the BIA data, and way less in both than overall. This means that not all whale BIAs will be represented when we jon the df's, and that the values for BIA_mn and BIA_mn can be NA, which is equivalent to 0

### Humpback sci name: Megaptera novaeangliae, hence the 'mn' abbreviation
### Blue sci name: Balaenoptera musculus, hence the 'bm' abbreviation

# check how many NAs
# length(which(is.na(dcrb_ca_vms_tix_analysis$BIA_mn)==TRUE)) # 187691
# length(which(dcrb_ca_vms_tix_analysis$BIA_mn==1)) # 165622
# length(which(dcrb_ca_vms_tix_analysis$BIA_mn==0)) # 553
# 
# length(which(is.na(dcrb_ca_vms_tix_analysis$BIA_bm)==TRUE)) # 187691
# length(which(dcrb_ca_vms_tix_analysis$BIA_bm==1)) # 94286
# length(which(dcrb_ca_vms_tix_analysis$BIA_bm==0)) # 78889

# change NAs to zeroes to indicate points outside the BIAs
### CA only
dcrb_ca_vms_tix_analysis$BIA_mn_noNAs <- ifelse(is.na(dcrb_ca_vms_tix_analysis$BIA_mn)==TRUE,0,dcrb_ca_vms_tix_analysis$BIA_mn)

dcrb_ca_vms_tix_analysis$BIA_bm_noNAs <- ifelse(is.na(dcrb_ca_vms_tix_analysis$BIA_bm)==TRUE,0,dcrb_ca_vms_tix_analysis$BIA_bm)

# truncate to Nov 2009 to Jul 2018
dcrb_ca_vms_tix_analysis <- dcrb_ca_vms_tix_analysis[-which(
  dcrb_ca_vms_tix_analysis$year_mo == "2009_01" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_02" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_03" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_04" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_05" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_06" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_07" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_08" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_09" |
    dcrb_ca_vms_tix_analysis$year_mo == "2009_10" |
    dcrb_ca_vms_tix_analysis$year_mo == "2018_08" |
    dcrb_ca_vms_tix_analysis$year_mo == "2018_09" |
    dcrb_ca_vms_tix_analysis$year_mo == "2018_10" |
    dcrb_ca_vms_tix_analysis$year_mo == "2018_11" |
    dcrb_ca_vms_tix_analysis$year_mo == "2018_12"
),]

# add week and vessel size
dcrb_ca_vms_tix_analysis <- dcrb_ca_vms_tix_analysis %>%
  mutate(
    week_of_year = week(westcoastdate_notime),
    month_as_numeric = month(westcoastdate_notime),
    crab.year = ifelse(
      month >= 11, paste0(year,"_",1+year), paste0(year - 1,"_",year)
      ),
    Vessel_Size = ifelse(FINAL_LENGTH >=40, ">=40 ft","<40 ft"),
    BIA_bm_or_mn = ifelse(BIA_bm_noNAs !=0 | BIA_mn_noNAs != 0, "Inside BIA","Outside BIA")
  )

# add column for crab year
dcrb_ca_vms_tix_analysis$crab.year <- ifelse(
  dcrb_ca_vms_tix_analysis$month >= 11, paste0(dcrb_ca_vms_tix_analysis$year,"-",1+dcrb_ca_vms_tix_analysis$year), paste0(dcrb_ca_vms_tix_analysis$year - 1,"-",dcrb_ca_vms_tix_analysis$year)
)

# make sure grid cell ID and Vessel_Size are factors
dcrb_ca_vms_tix_analysis$GRID5KM_ID <- as.factor(as.character(dcrb_ca_vms_tix_analysis$GRID5KM_ID))
dcrb_ca_vms_tix_analysis$Vessel_Size <- as.factor(as.character(dcrb_ca_vms_tix_analysis$Vessel_Size))
dcrb_ca_vms_tix_analysis$B_or_A_April1 <-
  as.factor(as.character(dcrb_ca_vms_tix_analysis$B_or_A_April1))

### create new columns that apportion to pings/lbs/$ from each fish ticket based on proportion of pings related to each ticket in each grid cell

# total records per trip (total number of VMS records associated with each fish ticket)
VMSrecords_per_trip <- dcrb_ca_vms_tix_analysis %>%
  group_by(Rec_ID) %>%
  summarise(trip_VMSrecords = n()) %>%
  filter(trip_VMSrecords > 1)
# add to fish ticket / vms data, make columns vessels, lbs, and $ per VMS location. note that columns with "...." in them, like ""TARGET...." reflect revenue 
dcrb_ca_vms_tix_analysis_TripInfo <- left_join(VMSrecords_per_trip, dcrb_ca_vms_tix_analysis, by="Rec_ID") %>%
  mutate(
    lbs_DCRB_per_location = DCRB..lbs./trip_VMSrecords,
    dollars_DCRB_per_location = DCRB..../trip_VMSrecords,
    Num_DCRB_Vessels_per_location = 1/trip_VMSrecords
  )
head(dcrb_ca_vms_tix_analysis_TripInfo)

### start by making a df for blwh occurrence that is grid cell and year_mo specific. otherwise we'd get slightly different monthly avg Blue_occurrence values likely because the days on which sm vs lg vessels were fishing differ within a month. so need to join blue_occurrence values after the fact

BLWH_5km_year_mo <- dcrb_ca_vms_tix_analysis_TripInfo %>%
  group_by(year, year_mo, B_or_A_April1, GRID5KM_ID, BIA_mn_noNAs, BIA_bm_noNAs, Region) %>% 
  summarise(
    Blue_occurrence = mean(BLWH_value, na.rm=TRUE)  ### here is where I need to add na.rm=TRUE, may want to add variability
  ) 
glimpse(BLWH_5km_year_mo)
length(which(is.na(BLWH_5km_year_mo$Blue_occurrence) == TRUE))/dim(BLWH_5km_year_mo)[1] # 0.03

write_rds(BLWH_5km_year_mo, 
          "~/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/BLWH_5km_year_mo_2009_2018.RDS")

# grab humpback data
humpback.sum.long <- read.csv("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/Humpback whale abundance monthly abundance predictions 2009-2018.csv")

# Join dcrb_ca_vms_tix and humpback data frames.
dcrb_ca_vms_tix_analysis$grid_year_mo <- as.character(paste0(dcrb_ca_vms_tix_analysis$GRID5KM_ID,"_",dcrb_ca_vms_tix_analysis$year_mo))
humpback.sum.long$grid_year_mo <- as.character(paste0(humpback.sum.long$GRID5KM_ID,"_",humpback.sum.long$Year_Month)) 

# dcrb_ca_vms_tix_analysis <- dcrb_ca_vms_tix_analysis %>%
#   left_join(humpback.sum.long, by = c("GRID5KM_ID"="GRID5KM_ID","grid_year_mo" = "grid_year_mo"))
# 
# dcrb_ca_vms_tix_analysis <- droplevels(dcrb_ca_vms_tix_analysis)


### at long last, make the df we want
glimpse(dcrb_ca_vms_tix_analysis_TripInfo)
names(dcrb_ca_vms_tix_analysis_TripInfo)

glimpse(BLWH_5km_year_mo)
names(BLWH_5km_year_mo)

start.time <- Sys.time()
con_df_weekly_years_5km_CA <- dcrb_ca_vms_tix_analysis_TripInfo %>%
  group_by(year, crab.year, year_mo, week_of_year, B_or_A_April1, GRID5KM_ID, BIA_bm_or_mn, BIA_mn_noNAs, BIA_bm_noNAs, Region, BAND_25KM, BAND_50KM, DEPTH_CATM) %>% 
  summarise(
    lbs_DCRB = sum(lbs_DCRB_per_location),
    dollars_DCRB =sum(dollars_DCRB_per_location),
    Num_DCRB_VMS_pings = n(),
    Num_DCRB_Vessels = sum(Num_DCRB_Vessels_per_location),
    Num_Unique_DCRB_Vessels = length(unique(as.factor(DOCNUM)))
  ) %>%
  ungroup() %>%
  left_join(BLWH_5km_year_mo) %>%
  left_join(humpback.sum.long, by = c("GRID5KM_ID"="GRID5KM_ID","grid_year_mo" = "grid_year_mo"))
Sys.time() - start.time
con_df_weekly_years_5km_CA <- droplevels(con_df_weekly_years_5km_CA)
head(as.data.frame(con_df_weekly_years_5km_CA))

write_rds(con_df_weekly_years_5km_CA, 
          "~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_2009-2018_fishtix_blue_humpback_whales_grids.RDS")

# query fishing data

library(tidyverse)

### read in data with tickets/VMS/vlengths matched, if ready

# this takes a few min to read in
dcrb_vms_tix_analysis <- read_rds(
  "~/Documents/RAIMBOW/Processed Data/VMS/vms_all_interpolated_w_grd_vlengths.RDS"
)
glimpse(dcrb_vms_tix_analysis)

# check years included
unique(dcrb_vms_tix_analysis$year)


#####################################################
# filter the data using the control variables below
# years <- seq(2009,2019,1)

state_agency_code <- "C"
#state <- "CA"
target_rev <- "DCRB"
target_lbs <- "DCRB"
region_north <- c(1040, 1041, 1042)
vessel_size_category_break <- 40
winter_months <- c("November", "December", "January", "February", "March")
crab_months <- c(1:7,11:12)
removal_types <- c("COMMERCIAL (NON-EFP)", "COMMERCIAL(DIRECT SALES)", "UNKNOWN")
max_speed <- 4.11556
min_speed <- 0

#ports <- c("CCA", "ERA", "BGA","BDA", "SFA", "MRA", "MNA")
#region_south <- c(1035, 1036, 1037, 1038)
#####################################################

#####################################################

# subset the data based on above queries. CA only
dcrb_ca_vms_tix_analysis <- dcrb_vms_tix_analysis %>%
  filter(agency_code == state_agency_code) %>%
  #filter(agency_code == state_agency_code & STATE == state) %>%
  filter(TARGET_rev == target_rev | TARGET_lbs == target_lbs) %>%
  filter(removal_type_name %in% removal_types) %>%
  filter(CA_OFFSHOR != -999) %>%
  filter(DEPTH_CATM == "0-100m" | DEPTH_CATM == "100-150m") %>%
  #filter(NGDC_M <= 0 & NGDC_M >= -12000) %>% # considered adding 052220. also see "evaluate fishing depths.R". probably unnecessary because NGDC_M refers to centroid depth and we can match depths by 5km grid cell later
  filter(avg_speed_recalc <= max_speed & avg_speed_recalc >= min_speed) %>%
  #filter(is.na(in_port) == TRUE) %>% # only removes ~4000 records
  #filter(port_group_code %in% ports) %>%
  mutate(#GRID5KM_ID = as.character(GRID5KM_ID),
    Region = ifelse(CA_OFFSHOR %in% region_north,
                    "NorCA","CenCA"),
    BIA_mn_noNAs = ifelse(is.na(BIA_mn)==TRUE,0,BIA_mn),
    BIA_bm_noNAs = ifelse(is.na(BIA_bm)==TRUE,0,BIA_bm),
    #year = lubridate::year(westcoastdate_notime), # removed because it is included with the vessel_lengths step
    year_month = paste0(lubridate::year(westcoastdate_notime),"_", substr(lubridate::ymd(westcoastdate_notime),6,7)), # substr() ensures month is a 2 digit value
    month = lubridate::month(westcoastdate_notime, label=TRUE, abbr = FALSE),
    month_as_numeric = month(westcoastdate_notime),
    week_of_year = week(westcoastdate_notime),
    day_of_year = yday(westcoastdate_notime),
    season = as.character(ifelse(month %in% winter_months, "Winter", "Spring-Summer")),
    crab_year = ifelse(
      month_as_numeric >= 11, paste0(year,"_",1+year), paste0(year - 1,"_",year)
    ),
    Vessel_Size = as.character(ifelse(FINAL_LENGTH >= vessel_size_category_break, paste0(">=",vessel_size_category_break, " ft"),paste0("<",vessel_size_category_break, " ft"))),
    BIA_bm_or_mn = ifelse(BIA_bm_noNAs !=0 | BIA_mn_noNAs != 0, "Inside BIA","Outside BIA")#,
  ) %>%
  filter(westcoastdate_notime >= as.Date("2009-11-01") & westcoastdate_notime <= as.Date("2019-08-01")) %>%
  filter(month_as_numeric %in% crab_months) %>%
  dplyr::select(
    Rec_ID, VMS_RECNO, drvid, Vessel_Size,
    westcoastdate_notime, year, crab_year, year_month, month, month_as_numeric, week_of_year, day_of_year, season,
    GRID5KM_ID, BAND_25KM, BAND_50KM, CA_OFFSHOR, Region, BIA_mn_noNAs, BIA_bm_noNAs, BIA_bm_or_mn, pacfin_port_code, port_group_code, DEPTH_CATM, NGDC_M,
    TARGET_lbs, TARGET_rev, DCRB_lbs, DCRB_revenue
  )

glimpse(dcrb_ca_vms_tix_analysis)

# number of unique vessels
length(unique(dcrb_ca_vms_tix_analysis$drvid)) #283

# number of unique tix
length(unique(dcrb_ca_vms_tix_analysis$Rec_ID)) #16182

# number of unique geolocations
length(unique(dcrb_ca_vms_tix_analysis$VMS_RECNO)) #369688

# summed revenue of VMS vessels 2009-14 vs 2014-18
unique(dcrb_ca_vms_tix_analysis$crab_year)

dcrb_ca_vms_tix_analysis %>%
  mutate(
    period = case_when(
      crab_year %in% c("2009_2010","2010_2011","2011_2012","2012_2013","2013_2014") ~ "pre",
      crab_year %in% c("2014_2015","2016_2017","2015_2016","2017_2018") ~ "MHW",
      TRUE ~ "post"
    ) 
  ) %>%
  group_by(period) %>%
  summarise(
    sum_DCRB_rev = sum(DCRB_revenue)
  )
3001185673/4
2939986917/5
750296418-587997383
162299035/587997383 # 27.6% higher annual revenue for DCRB VMS vessels during MHW

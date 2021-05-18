# Join fishing data to region and depth keys
library(tidyverse)

# all vessels
con_df_daily_years_5km_CA_no_regions <- readRDS("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels.RDS") #%>% # commented out 110920 because the columns below are no longer included
  #select(-contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
  #       -Region, -CA_OFFSHOR, -BAND_25KM , -BAND_50KM ) #, -BIA_mn_noNAs, -BIA_bm_noNAs, -BIA_bm_or_mn)

grid.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_key.rds") %>% 
  select(-region_ts)

con_df_daily_years_5km_CA_all_vessels_regions_depths <- con_df_daily_years_5km_CA_no_regions %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions. could choose just to exclude grid cells with centroids in Oregon
stopifnot(nrow(grid.key) == nrow(distinct(select(con_df_daily_years_5km_CA_all_vessels_regions_depths, GRID5KM_ID, Region, CA_OFFSHOR))))

write_rds(con_df_daily_years_5km_CA_all_vessels_regions_depths, "~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS")

# small vessels
con_df_daily_years_5km_CA_no_regions_sm <- readRDS("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_small_vessels.RDS") #%>% # commented out 110920 because the columns below are no longer included
  #select(-contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
  #       -Region, -CA_OFFSHOR, -BAND_25KM , -BAND_50KM ) #, -BIA_mn_noNAs, -BIA_bm_noNAs, -BIA_bm_or_mn)

grid.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_key.rds") %>% 
  select(-region_ts)

con_df_daily_years_5km_CA_small_vessels_regions_depths <- con_df_daily_years_5km_CA_no_regions_sm %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions
stopifnot(nrow(grid.key) >= nrow(distinct(select(con_df_daily_years_5km_CA_small_vessels_regions_depths, GRID5KM_ID, Region, CA_OFFSHOR)))) # changed to greater than or equal to because the number of grid cells small vessels operate in may be fewer than the number of grid cells all vessels operate in

write_rds(con_df_daily_years_5km_CA_small_vessels_regions_depths, "~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_small_vessels_regions_depths.RDS")

# large vessels
con_df_daily_years_5km_CA_no_regions_lg <- readRDS("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_large_vessels.RDS") #%>% # commented out 110920 because the columns below are no longer included
  #select(-contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
  #       -Region, -CA_OFFSHOR, -BAND_25KM , -BAND_50KM ) #, -BIA_mn_noNAs, -BIA_bm_noNAs, -BIA_bm_or_mn)

grid.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_key.rds") %>% 
  select(-region_ts)

con_df_daily_years_5km_CA_large_vessels_regions_depths <- con_df_daily_years_5km_CA_no_regions_lg %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions
stopifnot(nrow(grid.key) >= nrow(distinct(select(con_df_daily_years_5km_CA_large_vessels_regions_depths, GRID5KM_ID, Region, CA_OFFSHOR)))) # changed to greater than or equal to because the number of grid cells large vessels operate in may be fewer than the number of grid cells all vessels operate in

write_rds(con_df_daily_years_5km_CA_large_vessels_regions_depths, "~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_large_vessels_regions_depths.RDS")


#predictor variable: fault lines (amount in grid)

#-------------------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(viridis)
library(here)
library(rnaturalearth)
library(fasterize)
library(sp)
library(magrittr)
library(raster)
select <- dplyr::select
library(scales)
library(gridExtra)
library(nngeo)
library(scales)
library(stringr)
library(lubridate)

#-------------------------------------------------------------------------------------------------

#In QGIS, use quaternary_faults_folds_lines_OR_WA_geo.shp shapefile of fault lines (line features)
#Data from Blake, from https://www.usgs.gov/programs/earthquake-hazards/faults
#filtered data based on 'linetype' feature - Removed faults with 'inferred location'
#retained faults with 'moderately constrained' and 'well constrained' locations
#then using Vector --> Analysis Tools --> Sum line lengths tool 
#measure the amount (distance, in meters) of fault line in each study area grid
#tidy file and change unit to be km
#read in data:

path_km_fault_in_grid <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/data files/marine habitat geospatial data/USGS 2022 Quaternary Faults/length_km_of_faults_in_grids_FILT_mod_well_constrained_locs.csv"
km_fault_in_grid_raw <- read.csv(path_km_fault_in_grid)
#LENGTH_km provides length of fault lines in a grid (in km)
#COUNT provides count of unique faults which distance were summed (this is probably not useful)

#GridIDs repeat in cases where grid was in multiple pieces (overlaps with land etc)
#group-By GridID and sum fault length 
km_fault_in_grid <- km_fault_in_grid_raw %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(faults_km = sum(LENGTH_km))


#-------------------------------------------------------------------------------------------------

#join to existing df of predictors, use file that already has wind, SST and bathy

study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth.rds"))
#grids that were in pieces and had repeating gridID have been fixed in this file

#just join based on GRID5KM_ID as amount of faults is always the same (no effect by season on half_month time steps)
study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth %>% 
  left_join(km_fault_in_grid, by="GRID5KM_ID")

#save df
#write_rds(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults.rds"))



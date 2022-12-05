#predictor variable: distance from grid centroids to canyons/escarments (polygons) in km

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

#In QGIS, use study_area_grids_centroids.csv (C:\Users\lrie0\Documents\Projects\raimbow\DCRB_sdmTMB\data)
#and Canyon_polygons_west_coast_geo.shp and Escarpment_polygons_west_coast_geo.shp 
#from (C:\Users\lrie0\OneDrive\NOAA\Riekkola et al - predicting fishing effort\data files\marine habitat geospatial data\Harris et al 2014 Marine Geology) 
#and NNJoin plugin tool to measure distance between grid centroids (points) and canyons/escarpments (polygon)
#distance originally in meters,tidy file and change unit to be km
#read in data:

dist_grid_centroid_to_canyons_escarpments_km_raw <- read.csv(here::here('DCRB_sdmTMB', 'data','dist_grid_centroid_to_canyons_escarpments_km.csv'))
#distance to nearest canyon or escarpment feature provided in km
#no issues with repeating GridIDs

#-------------------------------------------------------------------------------------------------

#join to existing df of predictors, use file that already has wind, SST, bathy nd faults

study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults.rds"))
#grids that were in pieces and had repeating gridID have been fixed in this file

#just join based on GRID5KM_ID as dist to canyons/escarpment is always the same (no effect by season on half_month time steps)
study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults %>% 
  left_join(dist_grid_centroid_to_canyons_escarpments_km_raw, by="GRID5KM_ID")

#save df
#write_rds(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp.rds"))











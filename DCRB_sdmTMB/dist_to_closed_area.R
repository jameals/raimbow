#distance to nearest cloesd area/grid

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
library(nngeo)

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#this was originally in prep for sdmTMB script but moved here
#df full was df with those predcitors that were done at that point and response variable joined

closed_areas_df <- read_csv(here::here('DCRB_sdmTMB', 'data', 'study_area_grids_with_all_season_halfmonth_combos_and_closed_areas_df.csv'))

# df_full_with_closed_areas <- df_full %>% 
#   left_join(closed_areas_df, by=c('season', 'half_month','GRID5KM_ID'))
# glimpse(df_full_with_closed_areas)


study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2 <- read_rds(here::here('DCRB_sdmTMB', 'data', 'study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2.rds')) 

study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_with_closed_areas <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2 %>% 
  left_join(closed_areas_df, by=c('season', 'half_month','GRID5KM_ID'))
glimpse(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_with_closed_areas)


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# closed areas info was joined to the "full df"

# df_full_with_closed_areas <- read_rds(here::here('DCRB_sdmTMB', 'data', "df_full_not_final.rds")) %>% 
#   #but drop other predictor columns 
#   select(GRID5KM_ID:grd_y, open_closed)
# 
# #currently grid centroid location grd_x and grd_y are in lat and lon
# #keep them but also create coordinates in UTM 10 zone (grd_x_UTM10 and grd_y_UTM10)
# df_full_sf <- st_as_sf(df_full_with_closed_areas, 
#                        coords = c("grd_x", "grd_y"),
#                        crs = 4326,
#                        remove=F
# ) %>% 
#   # project to UTM zone 10
#   st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")
# #length units are in meters




df_with_closed_areas <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_with_closed_areas %>% 
  #but drop other predictor columns 
  select(GRID5KM_ID:grd_y, open_closed)

#currently grid centroid location grd_x and grd_y are in lat and lon
#keep them but also create coordinates in UTM 10 zone (grd_x_UTM10 and grd_y_UTM10)
df_with_closed_areas_sf <- st_as_sf(df_with_closed_areas, 
                       coords = c("grd_x", "grd_y"),
                       crs = 4326,
                       remove=F
) %>% 
  # project to UTM zone 10
  st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")
#length units are in meters

#-------------------------------------------------------------------------------------------------

# df_open_grids <- df_full_sf %>% 
#   filter(open_closed == "open")
# 
# df_closed_grids <- df_full_sf %>% 
#   filter(open_closed == "closed")


df_open_grids <- df_with_closed_areas_sf %>% 
  filter(open_closed == "open")

df_closed_grids <- df_with_closed_areas_sf %>% 
  filter(open_closed == "closed")


#-------------------------------------------------------------------------------------------------

#In a given time_step, for each open grid what is the distance to the nearest closed grid

df_open_grids_subset <- df_open_grids %>% 
  filter(season=="2009-2010", half_month=="September_1")
plot(df_open_grids_subset)

df_closed_grids_subset <- df_closed_grids %>% 
  filter(season=="2009-2010", half_month=="September_1")
plot(df_closed_grids_subset)


neares_closed_grid <- st_nn(df_open_grids_subset, df_closed_grids_subset, returnDist = T, k = 1) 



nn_to_df <- function(nn_out) {
  out <- map(names(nn_out), ~enframe(pluck(nn_out, .), name = "row_id", value = .) %>% 
               unnest(cols=all_of(.x))) 
  bind_cols(out[[1]], out[[2]] %>% select(-row_id))
}
myDF <- nn_to_df(nn_out=neares_closed_grid)

test_output <- df_open_grids_subset %>% 
  cbind(myDF) %>% 
  #we don't need the columns for row_id and nn
  select(-(row_id:nn)) %>% 
  #make dist column be in km
  mutate(dist = dist/1000) %>% 
  rename(dist_to_closed_km = dist)

plot(test_output)

test_output <- test_output %>% 
  st_set_geometry(NULL) %>% 
  select(GRID5KM_ID, season, half_month, dist_to_closed_km)


# columns <- c("GRID5KM_ID", "season", "half_month", "dist_to_closed_km")
# dummy_df <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
# colnames(dummy_df) = columns
# df_dist_to_closed <- dummy_df 

df_dist_to_closed <- df_dist_to_closed %>% 
  rbind(test_output)

unique(df_dist_to_closed$half_month)






#df_dist_to_closed_2009_2010 <-  df_dist_to_closed
#nrow(df_dist_to_closed_2009_2010)
#write_rds(df_dist_to_closed_2009_2010,here::here('DCRB_sdmTMB', 'data', 'closed areas', "df_dist_to_closed_2009_2010.rds"))

#no logbook data for WA for 2008-08 and 2008-09 - currently WA grids are jsut labelled clodes
#but that is not exactly accurate, so probably better to drop OR 2008-08 and 2008-09 data


#2018-2019 -- all grids are open between May_1 and August_1 so distance to closed area is 0 for all grids 
#fix this separately later



#------------------------------

dist_to_closed_all <- rbind(df_dist_to_closed_2009_2010,
                            df_dist_to_closed_2010_2011,
                            df_dist_to_closed_2011_2012,
                            df_dist_to_closed_2012_2013,
                            df_dist_to_closed_2013_2014,
                            df_dist_to_closed_2014_2015,
                            df_dist_to_closed_2015_2016,
                            df_dist_to_closed_2016_2017,
                            df_dist_to_closed_2017_2018,
                            df_dist_to_closed_2018_2019,
                            df_dist_to_closed_2019_2020
                            )

#write_rds(dist_to_closed_all,here::here('DCRB_sdmTMB', 'data', 'closed areas', "dist_to_closed_all.rds"))


#------------------------------


#df_full_with_closed_areas <- read_rds(here::here('DCRB_sdmTMB', 'data', "df_full_not_final.rds")) 
#this was created at the begining of script:
study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_with_closed_areas


dist_to_closed_all <- read_rds(here::here('DCRB_sdmTMB', 'data', 'closed areas', "dist_to_closed_all.rds")) %>% 
  #for some reason there are a small number of cases of pure duplication
  distinct(GRID5KM_ID, season, half_month, dist_to_closed_km) 





#when join all this to the previous df, any grids that are closed will have NA for distance to closed area (or we can make it 0)
#those grids get dropped out anyways from the actual analysis

#df_full_with_dist_to_closed_areas_dist <- df_full_with_closed_areas %>% 
#  left_join(dist_to_closed_all, by=c('season', 'half_month','GRID5KM_ID'))

df_full_with_dist_to_closed_areas_dist <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_with_closed_areas %>% 
  left_join(dist_to_closed_all, by=c('season', 'half_month','GRID5KM_ID'))





#2007-08 and 2008-09 are NAs as no logbooks for WA
#fix 2018-2019 -- all grids were open between May_1 and August_1 -- but distance to closed area 
#can't be NA, so instead use a large value (data max is 594km)

df_full_with_dist_to_closed_areas_dist <- df_full_with_dist_to_closed_areas_dist %>% 
  mutate(dist_to_closed_km = 
           ifelse(season=="2018-2019" & open_closed=="open" & is.na(dist_to_closed_km), 700, dist_to_closed_km))


#also here add extra column to denote OR/WA waters
#those grids at the border are more in WA waters so we will label them as such
df_full_with_dist_to_closed_areas_ORWA_waters <- df_full_with_dist_to_closed_areas_dist %>% 
  mutate(OR_WA_waters = ifelse(GRID5KM_ID <= 117319, 'OR', 'WA'))


#and also while we're at it, add column for times when WA had its supper pot reduction in place
df_full_with_dist_to_closed_areas_ORWA_waters_WA_summer_regs <- df_full_with_dist_to_closed_areas_ORWA_waters %>% 
  #first add a season_month column to make this easier
  mutate(half_month_dummy = half_month) %>% 
  separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
  select(-period) %>% 
  mutate(season_month = paste0(season,"_",month_name)) %>% 
  mutate(WA_pot_reduction = 
           ifelse(OR_WA_waters=="WA" & season_month %in% c('2018-2019_July',
                                                           '2018-2019_August',
                                                           '2018-2019_September',
                                                           '2019-2020_May',
                                                           '2019-2020_June',
                                                           '2019-2020_July',
                                                           '2019-2020_August',
                                                           '2019-2020_September')
                                                          , "Y", "N")) %>% 
  #drop season_month column as it is extra
  select(-season_month)


#write_rds(df_full_with_dist_to_closed_areas_ORWA_waters_WA_summer_regs,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_ClosedAreaDist.rds"))



###OLD:
#write_rds(df_full_with_dist_to_closed_areas_ORWA_waters_WA_summer_regs,here::here('DCRB_sdmTMB', 'data', "df_full_with_dist_to_closed_areas_not_final_20230120.rds"))
#after fixing duplicating rows
#write_rds(df_full_with_dist_to_closed_areas_ORWA_waters_WA_summer_regs,here::here('DCRB_sdmTMB', 'data', "df_full_with_dist_to_closed_areas_not_final_20230123.rds"))

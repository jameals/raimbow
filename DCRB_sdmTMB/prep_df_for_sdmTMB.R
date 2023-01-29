#prep df for sdmTMB

#-------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(sf)
library(ggcorrplot)

#-------------------------------------------------------------------------------------------------

#df with prepped response variable should be here

#this still includes grids that are closed in a given time step - so those 0s should be NAs 
#or those grid - time step combos need to be dropped
response_var_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_response_var.rds')) %>% 
  #add a presence/absence column - 0 pots in grid will be absence
  #note that atm there are still grids included in this df that were closed (so effort should be NA not 0)
  #once finish 'closed areas' df then these grids can just be dropped out
  mutate(present = ifelse(tottraps == 0, 0, 1))




#df with prepped predictor variables
#includes bottom)2, closed areas, dist to closed area, OR/WA waters, WA summer pot limit etc

predictor_vars_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_ClosedAreaDist.rds')) %>% 
  select(-grd_x, -grd_y )



# join predictor df and response df
df_full <- response_var_raw %>% 
  left_join(predictor_vars_raw, by=c('season', 'half_month','GRID5KM_ID')) %>% 
  #add a column denoting calendar month
  #mutate(half_month_dummy = half_month) %>% 
  #separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
  #select(-period) %>% 
  #add a column denoting winter vs summer fishery
  mutate(
    winter_summer = case_when(
      month_name == "December" | month_name == "January" | month_name == "February" | month_name == "March" | month_name == "April" ~ "Winter",
      month_name == "May" | month_name == "June" | month_name == "July" | month_name == "August" | month_name == "September" ~ "Summer"
    )
  )
glimpse(df_full)


#--------------------

##export df

#write_rds(df_full,here::here('DCRB_sdmTMB', 'data', "df_full_final_raw.rds"))

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#various tidying steps:

df_full_final_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_raw.rds')) 


#-------------------------------------------------------------------------------------------------
#restrict study area grids
#to improve sdmTMB runs restrict grids to be only those that have ever had effort
#we will keep 2007-08 and 2008-09 data for OR in this step 
#as there will ahve been various data loss during those years that had 30% data enty

restricted_study_area_grids <- df_full_final_raw %>% 
  filter(present==1) %>% 
  ungroup() %>% 
  distinct(GRID5KM_ID)

restricted_study_area_grids <- sort(unique(restricted_study_area_grids$GRID5KM_ID))

original_study_area <- read_sf(here::here('DCRB_sdmTMB','data', 'study_area.shp'))

restricted_study_area <- original_study_area %>% filter(GRID5KM_ID %in% restricted_study_area_grids) %>% 
  select(-(US_EEZ:path)) %>% 
  st_transform(4326)

#export shapefile in case want to use it for mapping later on
#st_write(restricted_study_area, "restricted_study_area.shp")

#filter df_full to new restricted study area
df_full_final_in_restricted_study_area <- df_full_final_raw %>% filter(GRID5KM_ID %in% restricted_study_area_grids)

#just double checking that no pots were in the grids dropped
#df_full_final_outside_restricted_study_area <- df_full_final_raw %>% filter(!GRID5KM_ID %in% restricted_study_area_grids)

#few of the grids that get dropped were in WA SMAs, but they never had any effort in them (not even in winter)

#-------------------------------------------------------------------------------------------------
#drop some predictors
#based on corrplots distance to escarpments and distance to canyons are highly correlated
#after checking which predictor performed better, decided to drop escarpments (but keep dist to canyons)
#through similar checking decided that best depth variables are depth_point_mean and depth_point_sd

df_full_final_chosen_predictors <- df_full_final_in_restricted_study_area %>% 
  select(-dist_escarpment_km, -depth_zonal_mean, -depth_zonal_median, -depth_zonal_sd, -depth_point_median) %>% 
  #we also decided not to do presence/absence modelling so drop that column
  select(-present) %>% 
  #we are also not expecting to use the percent itme or area a grid is open in the model
  select(-percent_grid_open, -percent_time_open, -`optional SMA name`)



## remove 2007-08 and 2008-09 from OR data - no data for WA for those seasons
df_full_final_chosen_predictors_years <- df_full_final_chosen_predictors %>% 
  filter(!season %in% c("2007-2008","2008-2009"))
  
#we will only model grids that were open to the fishery. Filter for those grids, after which can drop that column
df_full_final_open <- df_full_final_chosen_predictors_years %>% 
  filter(open_closed=="open") %>% 
  select(-open_closed)
#there are no NAs for predictors in the "open" df

#-------------------------------------------------------------------------------------------------
#z-scoring


#check how common 0s were in few of the variables
## just for our info, how many 0s were in those few variables that had bunch of 0s (which we don't want in the model)
#variables that have bunch of 0s: depth_point_sd, faults_km, and dist_canyon_km

#depth_point_sd: 1203 cases (unique grid & half_month step combos). 7 unique grid cells (122919  89913 103449 113352 118291 118952 128194)
##most of these 7 intersect with land, which explained 0s. Most had 0 traps, some had few (<57)

#faults_km: 124,297 cases (unique grid & half_month step combos). 701 unique grid cells 
## as there are a lot of grids that had no fault lines in them, some of them had a high number of pots. 
# This variable might be dropped during modelling as it doesn;t seem to have an effect

#dist_canyon_km: 3893 cases (unique grid & half_month step combos). 21 unique grid cells (90568 100470 116640 117301 117302 117303 117631 
#117632 117633 117634 118291 118952, 119283 119613 119941 120270 122248 122578 125219 125549 125879)
## as there are a quite a few grids that had 0 dist to canyon (i.e. grid centroid falls inside canyon polygon), 
#some of them had a high number of pots (<446) --  grid off Klipsan beach, about half of it is outside canyon. 

##none of these are excepted to be very problematic, so deal with these by z-scoring and fitting a quadratic term in the model



#separate z-scoring for winter data, summer data, and the full dataset
#save separate files for summer df (z-scored within itself), winter df (z-scored within itself) and all data (z-scored)


#df_all_scaled <- df_full_final_open 
#df_all_scaled$z_SST_avg <- (df_all_scaled$SST_avg-mean(df_all_scaled$SST_avg))/sd(df_all_scaled$SST_avg))


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


df_all_scaled <- df_full_final_open %>%
  ungroup() %>% 
  mutate(z_SST_avg = scale_this(SST_avg),
         z_wind_avg = scale_this(wind_avg),
         z_depth_point_mean = scale_this(depth_point_mean),
         z_depth_point_sd = scale_this(depth_point_sd),
         z_faults_km = scale_this(faults_km),
         z_dist_canyon_km = scale_this( dist_canyon_km),
         z_weighted_dist = scale_this(weighted_dist),
         z_weighted_fuel_pricegal = scale_this(weighted_fuel_pricegal),
         z_weighted_crab_ppp = scale_this(weighted_crab_ppp),
         z_bottom_O2_avg = scale_this(bottom_O2_avg),
         z_dist_to_closed_km = scale_this(dist_to_closed_km)
         )
         
         
df_all_scaled_corrplot <- df_all_scaled %>% 
  select(season, z_SST_avg:z_dist_to_closed_km, OR_WA_waters:winter_summer) 
  #different version of corrplot if year and month are numeric
  #select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

model.matrix(~0+., data=df_all_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

         
         
df_winter <- df_full_final_open %>% 
  filter(winter_summer=="Winter") %>%
  ungroup() %>% 
  mutate(z_SST_avg = scale_this(SST_avg),
         z_wind_avg = scale_this(wind_avg),
         z_depth_point_mean = scale_this(depth_point_mean),
         z_depth_point_sd = scale_this(depth_point_sd),
         z_faults_km = scale_this(faults_km),
         z_dist_canyon_km = scale_this( dist_canyon_km),
         z_weighted_dist = scale_this(weighted_dist),
         z_weighted_fuel_pricegal = scale_this(weighted_fuel_pricegal),
         z_weighted_crab_ppp = scale_this(weighted_crab_ppp),
         z_bottom_O2_avg = scale_this(bottom_O2_avg),
         z_dist_to_closed_km = scale_this(dist_to_closed_km)
  ) %>% 
  select(-winter_summer)


df_winter_scaled_corrplot <- df_winter %>% 
  select(season, z_SST_avg:z_dist_to_closed_km, OR_WA_waters:month_name) #drop WA_pot_reduction
#different version of corrplot if year and month are numeric
#select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

model.matrix(~0+., data=df_winter_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

         
         
df_summer <- df_full_final_open %>% 
  filter(winter_summer=="Summer") %>%
  ungroup() %>% 
  mutate(z_SST_avg = scale_this(SST_avg),
         z_wind_avg = scale_this(wind_avg),
         z_depth_point_mean = scale_this(depth_point_mean),
         z_depth_point_sd = scale_this(depth_point_sd),
         z_faults_km = scale_this(faults_km),
         z_dist_canyon_km = scale_this( dist_canyon_km),
         z_weighted_dist = scale_this(weighted_dist),
         z_weighted_fuel_pricegal = scale_this(weighted_fuel_pricegal),
         z_weighted_crab_ppp = scale_this(weighted_crab_ppp),
         z_bottom_O2_avg = scale_this(bottom_O2_avg),
         z_dist_to_closed_km = scale_this(dist_to_closed_km)
  ) %>% 
  select(-winter_summer)


df_summer_scaled_corrplot <- df_summer %>% 
  select(season, z_SST_avg:z_dist_to_closed_km, OR_WA_waters:WA_pot_reduction) #drop WA_pot_reduction
#different version of corrplot if year and month are numeric
#select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

model.matrix(~0+., data=df_summer_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


##export dfs

#write_rds(df_all_scaled,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_all_data.rds"))
#write_rds(df_winter,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_winter.rds"))
#write_rds(df_summer,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_summer.rds"))

         
         
         
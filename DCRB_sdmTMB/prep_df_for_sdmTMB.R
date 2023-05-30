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
#includes bottomO2, closed areas, dist to closed area, OR/WA waters, WA summer pot limit etc
#now also includes MOS = month of season, and half-month of season

predictor_vars_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice_bottomO2_ClosedAreaDist_MOS.rds')) %>% 
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

#fix couple bathymetry values

bathymetry_fixes <- read_csv(here::here('DCRB_sdmTMB', 'data','bathymetry_fixes.csv')) 

df_full_final_in_restricted_study_area_bathyFIX <- df_full_final_in_restricted_study_area %>% 
  left_join(bathymetry_fixes, by="GRID5KM_ID") %>% 
  #drop the old bathy columns
  select(-depth_point_mean.x, -depth_point_sd.x) %>% 
  #rename the new bathy columns
  rename(depth_point_mean = depth_point_mean.y,
         depth_point_sd = depth_point_sd.y)

df_full_final_in_restricted_study_area <- df_full_final_in_restricted_study_area_bathyFIX

## then do secondary bathymetry fixes on few wonky grids
bathymetry_fixes2 <- read_csv(here::here('DCRB_sdmTMB', 'data','bathymetry_fixes2.csv')) 

wonky_grids <- sort(unique(bathymetry_fixes2$GRID5KM_ID))

bathy_ok <- df_full_final_in_restricted_study_area %>% filter(!GRID5KM_ID %in% wonky_grids)
bathy_NOT_ok <- df_full_final_in_restricted_study_area %>% filter(GRID5KM_ID %in% wonky_grids) %>% 
  select(-depth_point_mean, -depth_point_sd) %>% 
  left_join(bathymetry_fixes2, by=c('GRID5KM_ID'))

df_full_final_in_restricted_study_area <- rbind(bathy_ok, bathy_NOT_ok)

#-------------------------------------------------------------------------------------------------

#fix fuel and crab price


weighted_fuel_price_fix <- read_rds(here::here('DCRB_sdmTMB', 'data','weighted_fuel_price_fix.rds')) 
weighted_crab_price_fix <- read_rds(here::here('DCRB_sdmTMB', 'data','weighted_crab_price_fix.rds')) 

df_full_final_in_restricted_study_area <- df_full_final_in_restricted_study_area %>% 
  left_join(weighted_fuel_price_fix, by=c('GRID5KM_ID', 'season','half_month')) %>% 
  left_join(weighted_crab_price_fix, by=c('GRID5KM_ID', 'season','half_month')) %>% 
  select(-weighted_fuel_pricegal, -weighted_crab_ppp) %>% 
  rename(weighted_fuel_pricegal = weighted_fuel_pricegal_v2,
         weighted_crab_ppp = weighted_crab_ppp_v2)

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
         z_dist_to_closed_km = scale_this(dist_to_closed_km),
         z_half_month_of_season = scale_this(half_month_of_season),
         z_month_of_season = scale_this(month_of_season)
         )
         
         
df_all_scaled_corrplot <- df_all_scaled %>% 
  select(season, z_SST_avg:z_half_month_of_season, OR_WA_waters:WA_pot_reduction) 
  #different version of corrplot if year and month are numeric
  #select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

df_all_scaled_corrplot$month_name <- factor(df_all_scaled_corrplot$month_name, levels = c("December", "January", "February","March", "April", "May", "June", "July", "August", "September"))

all_data_corrplot <- model.matrix(~0+., data=df_all_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3.5)

# #export for supplementary figure
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/all_data_corrplot.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(all_data_corrplot,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



##we might want to have month of season as a factorial option
df_all_scaled$month_of_seasonf <- as.factor(df_all_scaled$month_of_season)
df_all_scaled$half_month_of_seasonf <- as.factor(df_all_scaled$half_month_of_season)

 
##secondary scaling option -- if we center all variables and divide by 2 sd rather than 1, the relative influences will be similar
#The paper by Gelman describes this. Use standardize() function in the arm package.    

#first make season and month to be numerical
df_all_scaled$yearn <- as.numeric(substr(df_all_scaled$season,1,4))
df_all_scaled$yearf <- as.factor(df_all_scaled$yearn)

df_all_scaled$month_n <- 1
df_all_scaled$month_n[which(df_all_scaled$month_name=="January")] = 2
df_all_scaled$month_n[which(df_all_scaled$month_name=="February")] = 3
df_all_scaled$month_n[which(df_all_scaled$month_name=="March")] = 4
df_all_scaled$month_n[which(df_all_scaled$month_name=="April")] = 5
df_all_scaled$month_n[which(df_all_scaled$month_name=="May")] = 6
df_all_scaled$month_n[which(df_all_scaled$month_name=="June")] = 7
df_all_scaled$month_n[which(df_all_scaled$month_name=="July")] = 8
df_all_scaled$month_n[which(df_all_scaled$month_name=="August")] = 9
df_all_scaled$month_n[which(df_all_scaled$month_name=="September")] = 10

df_all_scaled <- df_all_scaled %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == "WA" ~ 1,
    OR_WA_waters == "OR" ~ 0
  )) %>% 
  mutate(WA_pot_reduction = case_when(
    WA_pot_reduction == "Y" ~ 1,
    WA_pot_reduction == "N" ~ 0
  ))

scale_this_2sd <- function(x){
  (x - mean(x, na.rm=TRUE)) / (2*sd(x, na.rm=TRUE))  
}

df_all_scaled_2sd <- df_all_scaled %>%
  mutate(z2sd_SST_avg = scale_this_2sd(SST_avg),
         z2sd_wind_avg = scale_this_2sd(wind_avg),
         z2sd_depth_point_mean = scale_this_2sd(depth_point_mean),
         z2sd_depth_point_sd = scale_this_2sd(depth_point_sd),
         z2sd_faults_km = scale_this_2sd(faults_km),
         z2sd_dist_canyon_km = scale_this_2sd( dist_canyon_km),
         z2sd_weighted_dist = scale_this_2sd(weighted_dist),
         z2sd_weighted_fuel_pricegal = scale_this_2sd(weighted_fuel_pricegal),
         z2sd_weighted_crab_ppp = scale_this_2sd(weighted_crab_ppp),
         z2sd_bottom_O2_avg = scale_this_2sd(bottom_O2_avg),
         z2sd_dist_to_closed_km = scale_this_2sd(dist_to_closed_km),
         z2sd_yearn = scale_this_2sd(yearn),
         z2sd_month_n = scale_this_2sd(month_n),
         z2sd_OR_WA_waters = scale_this_2sd(OR_WA_waters),
         z2sd_WA_pot_reduction = scale_this_2sd(WA_pot_reduction),
         z2sd_half_month_of_season = scale_this_2sd(half_month_of_season),
         z2sd_month_of_season = scale_this_2sd(month_of_season)
  )



        
#-----------------------------------------         
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
         z_dist_to_closed_km = scale_this(dist_to_closed_km),
         z_half_month_of_season = scale_this(half_month_of_season),
         z_month_of_season = scale_this(month_of_season)
  ) %>% 
  select(-winter_summer)


df_winter_scaled_corrplot <- df_winter %>% 
  select(season, z_SST_avg:z_half_month_of_season, OR_WA_waters) #drop WA_pot_reduction
#different version of corrplot if year and month are numeric
#select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

winter_corrplot <-  model.matrix(~0+., data=df_winter_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3.5)

# #export for supplementary figure
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/winter_corrplot.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(winter_corrplot,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##we might want to have month of season as a factorial option
df_winter$month_of_seasonf <- as.factor(df_winter$month_of_season)
df_winter$half_month_of_seasonf <- as.factor(df_winter$half_month_of_season)


##secondary scaling option -- if we center all variables and divide by 2 sd rather than 1, the relative influences will be similar
#The paper by Gelman describes this. Use standardize() function in the arm package.    

#first make season and month to be numerical
df_winter$yearn <- as.numeric(substr(df_winter$season,1,4))
df_winter$yearf <- as.factor(df_winter$yearn)

df_winter$month_n <- 1
df_winter$month_n[which(df_winter$month_name=="January")] = 2
df_winter$month_n[which(df_winter$month_name=="February")] = 3
df_winter$month_n[which(df_winter$month_name=="March")] = 4
df_winter$month_n[which(df_winter$month_name=="April")] = 5


df_winter <- df_winter %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == "WA" ~ 1,
    OR_WA_waters == "OR" ~ 0
  )) 

#scale_this_2sd <- function(x){
#  (x - mean(x, na.rm=TRUE)) / (2*sd(x, na.rm=TRUE))  
#}

df_winter_2sd <- df_winter %>%
  mutate(z2sd_SST_avg = scale_this_2sd(SST_avg),
         z2sd_wind_avg = scale_this_2sd(wind_avg),
         z2sd_depth_point_mean = scale_this_2sd(depth_point_mean),
         z2sd_depth_point_sd = scale_this_2sd(depth_point_sd),
         z2sd_faults_km = scale_this_2sd(faults_km),
         z2sd_dist_canyon_km = scale_this_2sd( dist_canyon_km),
         z2sd_weighted_dist = scale_this_2sd(weighted_dist),
         z2sd_weighted_fuel_pricegal = scale_this_2sd(weighted_fuel_pricegal),
         z2sd_weighted_crab_ppp = scale_this_2sd(weighted_crab_ppp),
         z2sd_bottom_O2_avg = scale_this_2sd(bottom_O2_avg),
         z2sd_dist_to_closed_km = scale_this_2sd(dist_to_closed_km),
         z2sd_yearn = scale_this_2sd(yearn),
         z2sd_month_n = scale_this_2sd(month_n),
         z2sd_OR_WA_waters = scale_this_2sd(OR_WA_waters),
         z2sd_half_month_of_season = scale_this_2sd(half_month_of_season),
         z2sd_month_of_season = scale_this_2sd(month_of_season)
  )


#-----------------------------------     
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
         z_dist_to_closed_km = scale_this(dist_to_closed_km),
         z_half_month_of_season = scale_this(half_month_of_season),
         z_month_of_season = scale_this(month_of_season)
  ) %>% 
  select(-winter_summer)


df_summer_scaled_corrplot <- df_summer %>% 
  select(season, z_SST_avg:z_half_month_of_season, OR_WA_waters,WA_pot_reduction) #drop WA_pot_reduction
#different version of corrplot if year and month are numeric
#select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)

summer_corrplot <-  model.matrix(~0+., data=df_summer_scaled_corrplot) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3.5)

# #export for supplementary figure
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/summer_corrplot.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(summer_corrplot,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##we might want to have month of season as a factorial option
df_summer$month_of_seasonf <- as.factor(df_summer$month_of_season)
df_summer$half_month_of_seasonf <- as.factor(df_summer$half_month_of_season)

##secondary scaling option -- if we center all variables and divide by 2 sd rather than 1, the relative influences will be similar
#The paper by Gelman describes this. Use standardize() function in the arm package.    

#first make season and month to be numerical
df_summer$yearn <- as.numeric(substr(df_summer$season,1,4))
df_summer$yearf <- as.factor(df_summer$yearn)

df_summer$month_n <- 6
df_summer$month_n[which(df_summer$month_name=="June")] = 7
df_summer$month_n[which(df_summer$month_name=="July")] = 8
df_summer$month_n[which(df_summer$month_name=="August")] = 9
df_summer$month_n[which(df_summer$month_name=="September")] = 10

df_summer <- df_summer %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == "WA" ~ 1,
    OR_WA_waters == "OR" ~ 0
  )) %>% 
  mutate(WA_pot_reduction = case_when(
    WA_pot_reduction == "Y" ~ 1,
    WA_pot_reduction == "N" ~ 0
  ))

#scale_this_2sd <- function(x){
#  (x - mean(x, na.rm=TRUE)) / (2*sd(x, na.rm=TRUE))  
#}

df_summer_2sd <- df_summer %>%
  mutate(z2sd_SST_avg = scale_this_2sd(SST_avg),
         z2sd_wind_avg = scale_this_2sd(wind_avg),
         z2sd_depth_point_mean = scale_this_2sd(depth_point_mean),
         z2sd_depth_point_sd = scale_this_2sd(depth_point_sd),
         z2sd_faults_km = scale_this_2sd(faults_km),
         z2sd_dist_canyon_km = scale_this_2sd( dist_canyon_km),
         z2sd_weighted_dist = scale_this_2sd(weighted_dist),
         z2sd_weighted_fuel_pricegal = scale_this_2sd(weighted_fuel_pricegal),
         z2sd_weighted_crab_ppp = scale_this_2sd(weighted_crab_ppp),
         z2sd_bottom_O2_avg = scale_this_2sd(bottom_O2_avg),
         z2sd_dist_to_closed_km = scale_this_2sd(dist_to_closed_km),
         z2sd_yearn = scale_this_2sd(yearn),
         z2sd_month_n = scale_this_2sd(month_n),
         z2sd_OR_WA_waters = scale_this_2sd(OR_WA_waters),
         z2sd_WA_pot_reduction = scale_this_2sd(WA_pot_reduction),
         z2sd_half_month_of_season = scale_this_2sd(half_month_of_season),
         z2sd_month_of_season = scale_this_2sd(month_of_season)
  )





df_all_scaled_2sd <- df_all_scaled_2sd %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == 1 ~ "WA",
    OR_WA_waters == 0 ~ "OR"
  )) %>% 
  mutate(WA_pot_reduction = case_when(
    WA_pot_reduction == 1 ~ "Y",
    WA_pot_reduction == 0 ~ "N"
  ))


df_winter_2sd <- df_winter_2sd %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == 1 ~ "WA",
    OR_WA_waters == 0 ~ "OR"
  )) 


df_summer_2sd <- df_summer_2sd %>% 
  mutate(OR_WA_waters = case_when(
    OR_WA_waters == 1 ~ "WA",
    OR_WA_waters == 0 ~ "OR"
  )) %>% 
  mutate(WA_pot_reduction = case_when(
    WA_pot_reduction == 1 ~ "Y",
    WA_pot_reduction == 0 ~ "N"
  ))



#----------------------------------- 
##export dfs

#write_rds(df_all_scaled_2sd,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_all_data_20230324.rds"))
#write_rds(df_winter_2sd,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_winter_20230324.rds"))
#write_rds(df_summer_2sd,here::here('DCRB_sdmTMB', 'data', "df_full_final_tidy_summer_20230324.rds"))

         
         
         
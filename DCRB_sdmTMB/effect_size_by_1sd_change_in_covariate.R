
#-------------------------------------------------------------------------------------------------

library(here)
library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)
library(plotmo)
library(viridis)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

###WINTER

#-------------------------------------------------------------------------------------------------

#read in winter data - the version where z-scoring is done across winter only
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230324.rds'))
glimpse(winter) 

winter$month_name_f <- factor(winter$month_name, levels = c("December", "January", "February", "March", "April"))

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))


mesh_winter <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh_winter$mesh$n


fit19b_winter <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','winter','after fixing fuel and crab price',"fit19b_winter.rds"))

#SST
dummy_SST <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_SST.csv"))
dummy_SST$half_month_of_seasonf <- as.factor(dummy_SST$half_month_of_seasonf)
predictions <- predict(fit19b_winter, newdata = dummy_SST)

#depth
dummy_depth <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_depth.csv"))
dummy_depth$half_month_of_seasonf <- as.factor(dummy_depth$half_month_of_seasonf)
predictions_depth <- predict(fit19b_winter, newdata = dummy_depth)

#fishing state
dummy_fishing_state <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_fishing_state.csv"))
dummy_fishing_state$half_month_of_seasonf <- as.factor(dummy_fishing_state$half_month_of_seasonf)
predictions_dummy_fishing_state <- predict(fit19b_winter, newdata = dummy_fishing_state)

#season
dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
predictions_dummy_season <- predict(fit19b_winter, newdata = dummy_season)

#half-month of season
dummy_HMOS <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_HMOS.csv"))
dummy_HMOS$half_month_of_seasonf <- as.factor(dummy_HMOS$half_month_of_seasonf)
predictions_dummy_HMOS <- predict(fit19b_winter, newdata = dummy_HMOS)

#wind
dummy_wind <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_wind.csv"))
dummy_wind$half_month_of_seasonf <- as.factor(dummy_wind$half_month_of_seasonf)
predictions_dummy_wind <- predict(fit19b_winter, newdata = dummy_wind)

#depth sd
dummy_depth_sd <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_depth_sd.csv"))
dummy_depth_sd$half_month_of_seasonf <- as.factor(dummy_depth_sd$half_month_of_seasonf)
predictions_dummy_depth_sd <- predict(fit19b_winter, newdata = dummy_depth_sd)

#fault lines
dummy_faults <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_faults.csv"))
dummy_faults$half_month_of_seasonf <- as.factor(dummy_faults$half_month_of_seasonf)
predictions_dummy_faults <- predict(fit19b_winter, newdata = dummy_faults)

#canyons
dummy_canyons <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_canyons.csv"))
dummy_canyons$half_month_of_seasonf <- as.factor(dummy_canyons$half_month_of_seasonf)
predictions_dummy_canyons <- predict(fit19b_winter, newdata = dummy_canyons)

#distance to port
dummy_port_dist <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_port_dist.csv"))
dummy_port_dist$half_month_of_seasonf <- as.factor(dummy_port_dist$half_month_of_seasonf)
predictions_dummy_port_dist <- predict(fit19b_winter, newdata = dummy_port_dist)

#fuel price
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_fuel.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_winter, newdata = dummy_fuel)

#crab price
dummy_crab <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_crab.csv"))
dummy_crab$half_month_of_seasonf <- as.factor(dummy_crab$half_month_of_seasonf)
predictions_dummy_crab <- predict(fit19b_winter, newdata = dummy_crab)

#bottom O2
dummy_O2 <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_O2.csv"))
dummy_O2$half_month_of_seasonf <- as.factor(dummy_O2$half_month_of_seasonf)
predictions_dummy_O2 <- predict(fit19b_winter, newdata = dummy_O2)

#distance to closed area
dummy_dist_to_closed <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs',"dummy_df_dist_to_closed.csv"))
dummy_dist_to_closed$half_month_of_seasonf <- as.factor(dummy_dist_to_closed$half_month_of_seasonf)
predictions_dummy_dist_to_closed <- predict(fit19b_winter, newdata = dummy_dist_to_closed)



#-------------------------------------------------------------------------------------------------

###SUMMER

#-------------------------------------------------------------------------------------------------

#read in summer data - the version where z-scoring is done across summer only
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230324.rds'))
glimpse(summer) 

summer$month_name_f <- factor(summer$month_name, levels = c("May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))


mesh_summer <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh_summer$mesh$n


fit19b_summer <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','summer','after fixing fuel and crab ppp',"fit19b_summer.rds"))

#SST
dummy_SST <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer' ,"dummy_df_SST.csv"))
dummy_SST$half_month_of_seasonf <- as.factor(dummy_SST$half_month_of_seasonf)
predictions <- predict(fit19b_summer, newdata = dummy_SST)

#depth
dummy_depth <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_depth.csv"))
dummy_depth$half_month_of_seasonf <- as.factor(dummy_depth$half_month_of_seasonf)
predictions_depth <- predict(fit19b_summer, newdata = dummy_depth)

#fishing state
dummy_fishing_state <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_fishing_state.csv"))
dummy_fishing_state$half_month_of_seasonf <- as.factor(dummy_fishing_state$half_month_of_seasonf)
predictions_dummy_fishing_state <- predict(fit19b_summer, newdata = dummy_fishing_state)

#WA pot reduction
dummy_WA_pot_reduction <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_WA_pot_reduction.csv"))
dummy_WA_pot_reduction$half_month_of_seasonf <- as.factor(dummy_WA_pot_reduction$half_month_of_seasonf)
predictions_dummy_WA_pot_reduction <- predict(fit19b_summer, newdata = dummy_WA_pot_reduction)

#season
dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
predictions_dummy_season <- predict(fit19b_summer, newdata = dummy_season)

#half-month of season
dummy_HMOS <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_HMOS.csv"))
dummy_HMOS$half_month_of_seasonf <- as.factor(dummy_HMOS$half_month_of_seasonf)
predictions_dummy_HMOS <- predict(fit19b_summer, newdata = dummy_HMOS)

#wind
dummy_wind <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_wind.csv"))
dummy_wind$half_month_of_seasonf <- as.factor(dummy_wind$half_month_of_seasonf)
predictions_dummy_wind <- predict(fit19b_summer, newdata = dummy_wind)

#depth sd
dummy_depth_sd <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_depth_sd.csv"))
dummy_depth_sd$half_month_of_seasonf <- as.factor(dummy_depth_sd$half_month_of_seasonf)
predictions_dummy_depth_sd <- predict(fit19b_summer, newdata = dummy_depth_sd)

#fault lines
dummy_faults <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_faults.csv"))
dummy_faults$half_month_of_seasonf <- as.factor(dummy_faults$half_month_of_seasonf)
predictions_dummy_faults <- predict(fit19b_summer, newdata = dummy_faults)

#canyons
dummy_canyons <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_canyons.csv"))
dummy_canyons$half_month_of_seasonf <- as.factor(dummy_canyons$half_month_of_seasonf)
predictions_dummy_canyons <- predict(fit19b_summer, newdata = dummy_canyons)

#distance to port
dummy_port_dist <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_port_dist.csv"))
dummy_port_dist$half_month_of_seasonf <- as.factor(dummy_port_dist$half_month_of_seasonf)
predictions_dummy_port_dist <- predict(fit19b_summer, newdata = dummy_port_dist)

#fuel price
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_fuel.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_summer, newdata = dummy_fuel)

#crab price
dummy_crab <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_crab.csv"))
dummy_crab$half_month_of_seasonf <- as.factor(dummy_crab$half_month_of_seasonf)
predictions_dummy_crab <- predict(fit19b_summer, newdata = dummy_crab)

#bottom O2
dummy_O2 <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_O2.csv"))
dummy_O2$half_month_of_seasonf <- as.factor(dummy_O2$half_month_of_seasonf)
predictions_dummy_O2 <- predict(fit19b_summer, newdata = dummy_O2)

#distance to closed area
dummy_dist_to_closed <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_dist_to_closed.csv"))
dummy_dist_to_closed$half_month_of_seasonf <- as.factor(dummy_dist_to_closed$half_month_of_seasonf)
predictions_dummy_dist_to_closed <- predict(fit19b_summer, newdata = dummy_dist_to_closed)



#prep df for sdmTMB

#-------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)

#-------------------------------------------------------------------------------------------------

#df with prepped response variable should be here

#this still includes grids that are closed in a given time step - so those 0s should be NAs 
#or those grid - time step combos need to be dropped
response_var_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_response_var.rds')) %>% 
  select(-tottraps_WA_data, -tottraps_OR_data) %>% 
  #add a presence/absence column - 0 pots in grid will be absence
  #note that atm there are still grids included in this df that were closed (so effort should be NA not 0)
  #once finish 'closed areas' df then these grids can just be dropped out
  mutate(present = ifelse(tottraps == 0, 0, 1))




#df with prepped predictor variables
#this might still change if get eg. bottom O2 data

predictor_vars_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice.rds')) %>% 
  select(-grd_x, -grd_y )


# join predictor df and response df
df_full <- response_var_raw %>% 
  left_join(predictor_vars_raw, by=c('season', 'half_month','GRID5KM_ID')) %>% 
  #add a column denoting calendar month
  mutate(half_month_dummy = half_month) %>% 
  separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
  select(-period) %>% 
  #add a column denoting winter vs summer fishery
  mutate(
    winter_summer = case_when(
      month_name == "December" | month_name == "January" | month_name == "February" | month_name == "March" | month_name == "April" ~ "Winter",
      month_name == "May" | month_name == "June" | month_name == "July" | month_name == "August" | month_name == "September" ~ "Summer"
    )
  )
glimpse(df_full)


#-------------------------------------------------------------------------------------------------
#add closed areas data

closed_areas_df <- read_csv(here::here('DCRB_sdmTMB', 'data', 'study_area_grids_with_all_season_halfmonth_combos_and_closed_areas_df.csv'))

df_full_with_closed_areas <- df_full %>% 
  left_join(closed_areas_df, by=c('season', 'half_month','GRID5KM_ID'))
glimpse(df_full_with_closed_areas)


#-------------------------------------------------------------------------------------------------


##here could also add couple things, like label for grids/time-steps in WA that have summer pot reduction 
#--  would need to separate grids in WA waters

#-------------------------------------------------------------------------------------------------


##export df

#this is just a working df for now - not a finished df of response and all predictors etc


#write_rds(df_full_with_closed_areas,here::here('DCRB_sdmTMB', 'data', "df_full_not_final.rds"))









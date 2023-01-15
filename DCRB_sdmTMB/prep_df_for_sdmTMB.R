#prep df for sdmTMB

#-------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)

#-------------------------------------------------------------------------------------------------

#df with prepped response variable should be here

response_var_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_response_var.rds')) %>% 
  select(-tottraps_WA_data, -tottraps_OR_data)
#this still includes grids that are closed in a given time step - so those 0s should be NAs 
#or those grid - time step combos need to be dropped
#but still need to finish closed areas df
#after that need to add column denoting presence and absence (1, 0) -- 0 pots in grid will be absence (once closed areas removed)



#df with prepped predictor variables
#this might still change if get eg. bottom O2 data

predictor_vars_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice.rds')) %>% 
  select(-grd_x, -grd_y )


# join predictor df and response df
df_full <- response_var_raw %>% 
  left_join(predictor_vars_raw, by=c('season', 'half_month','GRID5KM_ID'))
glimpse(df_full)


##here could add couple things, like variable that is calendar month, label for grids/tim-steps in WA that have summer pot reduction




##export df


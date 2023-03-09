##make maps


#---------------------------------------------


library(here)
library(tidyverse)
library(viridis)
library(ggeffects)
library(ggplot2)
library(sf)


#---------------------------------------------

#read in data from a cross validation model

cv_fits_5_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation', 'cv_winter_fix_test_2_iid','cv_fits_5_data.rds')) 

#if needed for coparison, could read in the data that was inout for cv
cv_input_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 


#---------------------------------------------

#if exported cv_fits[[5]]$data, then only 2019-2020 is part of fold 2
#fold 2 tottraps is identical to cv_input_data tottraps for May 2019-2020

#want to map May 2020
# so for cv_fits_5_data fold 2 we use tottraps as the actual and cv_predicted for the predictions

df_mapping <- cv_fits_5_data %>%
  filter(fold_id==2) %>% 
  select(GRID5KM_ID, season, half_month, tottraps, cv_predicted)

#read in restricted study area shapefile

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)


df_mapping_sf <- study_area %>% left_join(df_mapping, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA)

df_mapping_sf_May1 <- df_mapping_sf %>% filter(half_month == "May_1")
df_mapping_sf_May2 <- df_mapping_sf %>% filter(half_month == "May_2")

#export shapefile for QGIS
#st_write(df_mapping_sf, "df_mapping_sf_May_2020.shp")


#how to deal with predictions being in log scale?
# e.g. here, the exp version does not make sense as a pot count
df_mapping_sf_May2 <- df_mapping_sf_May2 %>% 
  mutate(cv_pred_exp = exp(cv_predicted),
         cv_pred_scaled = (cv_predicted - mean(cv_predicted))/sd(cv_predicted))
glimpse(df_mapping_sf_May2)

#should actuals and predictions be scaled or something.....?


#---------------------------------------------
#---------------------------------------------

##ALL DATA

#read in data from a cross validation model

all_data_cv_fits_5_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation', 'cv_all_data_fix_test_2_iid','all_data_cv_fits_5_data.rds')) 

#if needed for coparison, could read in the data that was inout for cv
cv_input_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 


#---------------------------------------------

#if exported cv_fits[[5]]$data, then only 2019-2020 is part of fold 2
#fold 2 tottraps is identical to cv_input_data tottraps for May 2019-2020

#want to map May 2020
# so for cv_fits_5_data fold 2 we use tottraps as the actual and cv_predicted for the predictions

df_mapping <- all_data_cv_fits_5_data %>%
  filter(fold_id==2) %>% 
  select(GRID5KM_ID, season, half_month, tottraps, cv_predicted)

#read in restricted study area shapefile

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)


df_mapping_sf <- study_area %>% left_join(df_mapping, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA)

df_mapping_sf_May1 <- df_mapping_sf %>% filter(half_month == "May_1")
df_mapping_sf_May2 <- df_mapping_sf %>% filter(half_month == "May_2")

#export shapefile for QGIS
#st_write(df_mapping_sf, "all_data_df_mapping_sf_May_2020.shp")


#how to deal with predictions being in log scale?
# e.g. here, the exp version does not make sense as a pot count
df_mapping_sf_May2 <- df_mapping_sf_May2 %>% 
  mutate(cv_pred_exp = exp(cv_predicted),
         cv_pred_scaled = (cv_predicted - mean(cv_predicted))/sd(cv_predicted))
glimpse(df_mapping_sf_May2)

#should actuals and predictions be scaled or something.....?



#---------------------------------------------
#---------------------------------------------

##SUMMER DATA

#read in data from a cross validation model

all_data_cv_fits_5_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation', 'cv_summer_test10b_ar1','summer_cv_fits_5_data.rds')) 

#if needed for coparison, could read in the data that was inout for cv
cv_input_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 


#---------------------------------------------

#if exported cv_fits[[5]]$data, then only 2019-2020 is part of fold 2
#fold 2 tottraps is identical to cv_input_data tottraps for May 2019-2020

#want to map May 2020
# so for cv_fits_5_data fold 2 we use tottraps as the actual and cv_predicted for the predictions

df_mapping <- all_data_cv_fits_5_data %>%
  filter(fold_id==2) %>% 
  select(GRID5KM_ID, season, half_month, tottraps, cv_predicted)

#read in restricted study area shapefile

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)


df_mapping_sf <- study_area %>% left_join(df_mapping, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA)

df_mapping_sf_May1 <- df_mapping_sf %>% filter(half_month == "May_1")
df_mapping_sf_May2 <- df_mapping_sf %>% filter(half_month == "May_2")

#export shapefile for QGIS
#st_write(df_mapping_sf, "summer_df_mapping_sf_May_2020.shp")


#how to deal with predictions being in log scale?
# e.g. here, the exp version does not make sense as a pot count
df_mapping_sf_May2 <- df_mapping_sf_May2 %>% 
  mutate(cv_pred_exp = exp(cv_predicted),
         cv_pred_scaled = (cv_predicted - mean(cv_predicted))/sd(cv_predicted))
glimpse(df_mapping_sf_May2)

#should actuals and predictions be scaled or something.....?



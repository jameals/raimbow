##make maps of best CV model - actuals vs predicted


#---------------------------------------------


library(here)
library(tidyverse)
library(viridis)
library(ggeffects)
library(ggplot2)
library(sf)


#---------------------------------------------



#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#for each restricted study area grid, assign a smaller area (management area & inshore/offshore)

restricted_study_area_management_areas <- read_csv(here::here('DCRB_sdmTMB',  'data', 'restricted_study_area_management_areas.csv')) 

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))

restricted_study_area_management_areas_sp <- study_area %>% left_join(restricted_study_area_management_areas)

# #st_write(restricted_study_area_management_areas_sp, "restricted_study_area_management_areas_sp.shp")


#------------------------------------------------------------------------------------------

#make map of actuals vs predicted for best CV model


#read in best CV model object
cv_test16_all_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV', 'cv_test16_all_data.rds')) 


#pick one May half_month to map
cv_test16_all_data_data_May1_2016 <- cv_test16_all_data[[1]]$data %>% 
  filter(season == "2015-2016") %>% 
  filter(half_month == "May_1") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*70900) #70900 was total estimated pots for May_1 2016 from landings

glimpse(cv_test16_all_data_data_May1_2016)


#read in restricted study area shapefile
study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)

df_mapping_sf_May1_2016 <- cv_test16_all_data_data_May1_2016 %>% left_join(study_area, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) %>%
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>%
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May1_2016 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May1_2016 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                            98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                            105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                            117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                            119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                            122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May1_2016 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                          98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                          105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                          117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                          119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                          122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May1_2016 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2016, "df_mapping_sf_May1_2016_20230414.shp")


#May_2 of 2016
cv_test16_all_data_data_May2_2016 <- cv_test16_all_data[[1]]$data %>% 
  filter(season == "2015-2016") %>% 
  filter(half_month == "May_2") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*55900) #55900 was total estimated pots for May_2 2016 from landings

glimpse(cv_test16_all_data_data_May2_2016)

df_mapping_sf_May2_2016 <- cv_test16_all_data_data_May2_2016 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May2_2016 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May2_2016 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May2_2016 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May2_2016 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2016, "df_mapping_sf_May2_2016_20230414.shp")


#---------------------------------------------

#May 2016-2017


#pick one May half_month to map
cv_test16_all_data_data_May1_2017 <- cv_test16_all_data[[2]]$data %>% 
  filter(season == "2016-2017") %>% 
  filter(half_month == "May_1") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*94800) #94800 was total estimated pots for May_1 2017 from landings

glimpse(cv_test16_all_data_data_May1_2017)


df_mapping_sf_May1_2017 <- cv_test16_all_data_data_May1_2017 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May1_2017 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May1_2017 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May1_2017 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May1_2017 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2017, "df_mapping_sf_May1_2017_20230414.shp")


#May_2 of 2017
cv_test16_all_data_data_May2_2017 <- cv_test16_all_data[[2]]$data %>% 
  filter(season == "2016-2017") %>% 
  filter(half_month == "May_2") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*67700) #67700 was total estimated pots for May_2 2017 from landings

glimpse(cv_test16_all_data_data_May2_2017)

df_mapping_sf_May2_2017 <- cv_test16_all_data_data_May2_2017 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May2_2017 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May2_2017 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May2_2017 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May2_2017 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2017, "df_mapping_sf_May2_2017_20230414.shp")



#---------------------------------------------

#May 2017-2018


#pick one May half_month to map
cv_test16_all_data_data_May1_2018 <- cv_test16_all_data[[3]]$data %>% 
  filter(season == "2017-2018") %>% 
  filter(half_month == "May_1") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*89800) #89800 was total estimated pots for May_1 2018 from landings

glimpse(cv_test16_all_data_data_May1_2018)


df_mapping_sf_May1_2018 <- cv_test16_all_data_data_May1_2018 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May1_2018 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May1_2018 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May1_2018 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May1_2018 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2018, "df_mapping_sf_May1_2018_20230414.shp")


#May_2 of 2018
cv_test16_all_data_data_May2_2018 <- cv_test16_all_data[[3]]$data %>% 
  filter(season == "2017-2018") %>% 
  filter(half_month == "May_2") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*73400) %>%  #73400 was total estimated pots for May_2 2018 from landings
  mutate(tottraps = percent_tottrap/100*73400)

glimpse(cv_test16_all_data_data_May2_2018)

df_mapping_sf_May2_2018 <- cv_test16_all_data_data_May2_2018 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May2_2018 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May2_2018 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May2_2018 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May2_2018 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2018, "df_mapping_sf_May2_2018_20230414.shp")


#---------------------------------------------

#May 2018-2019 -- note that WA pot reduction was not included in current model as it gave errors


#pick one May half_month to map
cv_test16_all_data_data_May1_2019 <- cv_test16_all_data[[4]]$data %>% 
  filter(season == "2018-2019") %>% 
  filter(half_month == "May_1") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*79400) #79400 was total estimated pots for May_1 2019 from landings

glimpse(cv_test16_all_data_data_May1_2019)


df_mapping_sf_May1_2019 <- cv_test16_all_data_data_May1_2019 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May1_2019 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May1_2019 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May1_2019 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May1_2019 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2019, "df_mapping_sf_May1_2019_20230414.shp")


#May_2 of 2019
cv_test16_all_data_data_May2_2019 <- cv_test16_all_data[[4]]$data %>% 
  filter(season == "2018-2019") %>% 
  filter(half_month == "May_2") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*61100) #61100 was total estimated pots for May_2 2019 from landings

glimpse(cv_test16_all_data_data_May2_2019)

df_mapping_sf_May2_2019 <- cv_test16_all_data_data_May2_2019 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May2_2019 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May2_2019 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May2_2019 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May2_2019 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2019, "df_mapping_sf_May2_2019_20230414.shp")


#---------------------------------------------

#May 2019-2020 -- note that WA pot reduction was not included in current model as it gave errors


#pick one May half_month to map
cv_test16_all_data_data_May1_2020 <- cv_test16_all_data[[5]]$data %>% 
  filter(season == "2019-2020") %>% 
  filter(half_month == "May_1") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*65400) %>%  #65400 was total estimated pots for May_1 2020 from landings
  mutate(tottraps = percent_tottrap/100*65400)

glimpse(cv_test16_all_data_data_May1_2020)


df_mapping_sf_May1_2020 <- cv_test16_all_data_data_May1_2020 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May1_2020 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May1_2020 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May1_2020 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May1_2020 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2020, "df_mapping_sf_May1_2020_20230414.shp")


#May_2 of 2020
cv_test16_all_data_data_May2_2020 <- cv_test16_all_data[[5]]$data %>% 
  filter(season == "2019-2020") %>% 
  filter(half_month == "May_2") %>% 
  #drop bunch of unnecessary columns
  select(GRID5KM_ID:month_n, month_name_f:cv_loglik) %>% 
  #backtransform predictions
  mutate(bck_trns_preds = exp(cv_predicted)) %>% 
  #each grid would be some % of the total pots
  #can't calc for backtransformed because of infinte values for backtransformed pred - get one grid with NaN, all others 0
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = cv_predicted/sum(cv_predicted)*100) %>% 
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*63700) %>%  #63700 was total estimated pots for May_2 2019 from landings
  mutate(tottraps = percent_tottrap/100*63700) %>% 
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May2_2020)

df_mapping_sf_May2_2020 <- cv_test16_all_data_data_May2_2020 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #main issue is bck_trns_preds as too many digits in the number. not needed for mapping anyway so drop it
  select(-bck_trns_preds)

## go back to the original methods, left join grid to df, and export, skip the below
## the repeating grid is not a problem if just mapping

# grid_86945 <- df_mapping_sf_May2_2020 %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3) %>% 
#   mutate(tottraps = tottraps/3)
# #grids that appear twice
# grids_twice <- df_mapping_sf_May2_2020 %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                     98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                     105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                     117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                     119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                     122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)%>% 
#   mutate(tottraps = tottraps/2)
# grids_ok <- df_mapping_sf_May2_2020 %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                   98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                   105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                   117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                   119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                   122588, 122589, 122919, 129512, 129842, 86945))
# df_mapping_sf_May2_2020 <- rbind(grids_ok, grids_twice, grid_86945)
# 
# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2020, "df_mapping_sf_May2_2020_20230502.shp")
# 


#average across mgmt areas
mgmt_averages_May1_2020 <- df_mapping_sf_May2_2020 %>% group_by(mgmt_area) %>% 
  summarise(median_diff = median(difference))

df_mapping_sf_May2_2020 <- df_mapping_sf_May2_2020 %>% left_join(mgmt_averages_May1_2020)
# #st_write(df_mapping_sf_May2_2020, "df_mapping_sf_May2_2020_20230502.shp")




p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
       geom_violin() +
       facet_wrap(~ mgmt_area) +
       coord_flip()+
       theme_classic()
 p

#inshore vs offshore
 p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
   geom_violin() +
   facet_wrap(~ inshore_offshore) +
   coord_flip()+
   theme_classic()
 p
 
#across all WC
 p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
   geom_violin() +
   coord_flip()+
   theme_classic()
 p
 
 
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#would e.g. avg. pots in grid in all May_1/May_2 across all years produce an equally good map?

df_full <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds')) 


df_full_filtered_May_1 <- df_full %>% 
  filter(half_month == "May_1") 

sum(df_full_filtered_May_1$tottraps)
#851793.6 ##across all seasons 2009-2010 to 2019-2020

df_full_summary_May_1 <- df_full_filtered_May_1 %>% 
  group_by(GRID5KM_ID, half_month, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps))

df_mapping_sf_summary_May_1 <- df_full_summary_May_1 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_May_1, "df_mapping_sf_summary_May_1.shp")




df_full_filtered_May_2 <- df_full %>% 
  filter(half_month == "May_2") 

sum(df_full_filtered_May_2$tottraps)
#680881.8 ##across all seasons 2009-2010 to 2019-2020

df_full_summary_May_2 <- df_full_filtered_May_2 %>% 
  group_by(GRID5KM_ID, half_month, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps)) #if wanted a % -- would need to look one season at a time, grid was % of that time step, then average those

df_mapping_sf_summary_May_2 <- df_full_summary_May_2 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_May_2, "df_mapping_sf_summary_May_2.shp")




#avg across all winters:

df_full_filtered_winter <- df_full %>% 
  filter(month_name %in% c('December', 'January', 'February', 'March', 'April')) 

df_full_summary_winter <- df_full_filtered_winter %>% 
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps))

df_mapping_sf_summary_winter <- df_full_summary_winter %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_winter, "df_mapping_sf_summary_winter.shp")


#avg across all summers:

df_full_filtered_summer <- df_full %>% 
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) 

df_full_summary_summer <- df_full_filtered_summer %>% 
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps))

df_mapping_sf_summary_summer <- df_full_summary_summer %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_summer, "df_mapping_sf_summary_summer.shp")

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------




















###OLD#####


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



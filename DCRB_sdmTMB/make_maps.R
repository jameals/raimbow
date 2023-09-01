##make maps of best CV model - actuals vs predicted


#---------------------------------------------


library(here)
library(tidyverse)
library(viridis)
library(ggeffects)
library(ggplot2)
library(sf)
library(lubridate)
library(ggpubr)


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
  mutate(weighted = percent_preds/100*70900) %>%  #70900 was total estimated pots for May_1 2016 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May1_2016)


#read in restricted study area shapefile
study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)

df_mapping_sf_May1_2016 <- cv_test16_all_data_data_May1_2016 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) %>%
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>%
  #select(-(month_name:weighted_crab_ppp)) %>%
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


#average across states
state_averages_May1_2016 <- df_mapping_sf_May1_2016 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May1_2016 <- df_mapping_sf_May1_2016 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May1_2016 <- df_mapping_sf_May1_2016 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May1_2016 <- df_mapping_sf_May1_2016 %>% left_join(state_averages_May1_2016) %>% 
  left_join(mgmt_averages_May1_2016) %>% 
  left_join(inshore_offshore_averages_May1_2016)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2016, "df_mapping_sf_May1_2016_20230621.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May1_2016, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May1_2016, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May1_2016, aes(x=mgmt_area, y=difference)) + #change x from '' to 'mgmt_area'
  geom_violin() +
  #facet_wrap(~ mgmt_area) + #don't do facet wrap
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May1_2016, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p





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
  mutate(weighted = percent_preds/100*55900) %>%  #55900 was total estimated pots for May_2 2016 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May2_2016)

df_mapping_sf_May2_2016 <- cv_test16_all_data_data_May2_2016 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:month_name_f)) %>% 
  #select(-(month_name:weighted_crab_ppp)) %>% 
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

#average across states
state_averages_May2_2016 <- df_mapping_sf_May2_2016 %>% group_by(OR_WA_waters ) %>% 
   summarise(median_diff_state = median(difference))
 
#average across mgmt areas
mgmt_averages_May2_2016 <- df_mapping_sf_May2_2016 %>% group_by(mgmt_area) %>% 
 summarise(median_diff_mgmt = median(difference))
 
##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May2_2016 <- df_mapping_sf_May2_2016 %>% group_by(inshore_offshore) %>% 
 summarise(median_diff_shore = median(difference))
 
df_mapping_sf_May2_2016 <- df_mapping_sf_May2_2016 %>% left_join(state_averages_May2_2016) %>% 
 left_join(mgmt_averages_May2_2016) %>% 
 left_join(inshore_offshore_averages_May2_2016)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2016, "df_mapping_sf_May2_2016_20230621.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May2_2016, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May2_2016, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May2_2016, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May2_2016, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p


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
  mutate(weighted = percent_preds/100*94800) %>%  #94800 was total estimated pots for May_1 2017 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May1_2017)


df_mapping_sf_May1_2017 <- cv_test16_all_data_data_May1_2017 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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

#average across states
state_averages_May1_2017 <- df_mapping_sf_May1_2017 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May1_2017 <- df_mapping_sf_May1_2017 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May1_2017 <- df_mapping_sf_May1_2017 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May1_2017 <- df_mapping_sf_May1_2017 %>% left_join(state_averages_May1_2017) %>% 
  left_join(mgmt_averages_May1_2017) %>% 
  left_join(inshore_offshore_averages_May1_2017)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2017, "df_mapping_sf_May1_2017_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May1_2017, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May1_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May1_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May1_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p




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
  mutate(weighted = percent_preds/100*67700) %>%  #67700 was total estimated pots for May_2 2017 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May2_2017)

df_mapping_sf_May2_2017 <- cv_test16_all_data_data_May2_2017 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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


#average across states
state_averages_May2_2017 <- df_mapping_sf_May2_2017 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May2_2017 <- df_mapping_sf_May2_2017 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May2_2017 <- df_mapping_sf_May2_2017 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May2_2017 <- df_mapping_sf_May2_2017 %>% left_join(state_averages_May2_2017) %>% 
  left_join(mgmt_averages_May2_2017) %>% 
  left_join(inshore_offshore_averages_May2_2017)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2017, "df_mapping_sf_May2_2017_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May2_2017, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May2_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May2_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May2_2017, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p


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
  mutate(weighted = percent_preds/100*89800) %>%  #89800 was total estimated pots for May_1 2018 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May1_2018)


df_mapping_sf_May1_2018 <- cv_test16_all_data_data_May1_2018 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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


#average across states
state_averages_May1_2018 <- df_mapping_sf_May1_2018 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May1_2018 <- df_mapping_sf_May1_2018 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May1_2018 <- df_mapping_sf_May1_2018 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May1_2018 <- df_mapping_sf_May1_2018 %>% left_join(state_averages_May1_2018) %>% 
  left_join(mgmt_averages_May1_2018) %>% 
  left_join(inshore_offshore_averages_May1_2018)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2018, "df_mapping_sf_May1_2018_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May1_2018, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May1_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May1_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May1_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p



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
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May2_2018)

df_mapping_sf_May2_2018 <- cv_test16_all_data_data_May2_2018 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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


#average across states
state_averages_May2_2018 <- df_mapping_sf_May2_2018 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May2_2018 <- df_mapping_sf_May2_2018 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May2_2018 <- df_mapping_sf_May2_2018 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May2_2018 <- df_mapping_sf_May2_2018 %>% left_join(state_averages_May2_2018) %>% 
  left_join(mgmt_averages_May2_2018) %>% 
  left_join(inshore_offshore_averages_May2_2018)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2018, "df_mapping_sf_May2_2018_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May2_2018, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May2_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May2_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May2_2018, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p


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
  mutate(weighted = percent_preds/100*79400) %>%  #79400 was total estimated pots for May_1 2019 from landings
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May1_2019)


df_mapping_sf_May1_2019 <- cv_test16_all_data_data_May1_2019 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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



#average across states
state_averages_May1_2019 <- df_mapping_sf_May1_2019 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May1_2019 <- df_mapping_sf_May1_2019 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May1_2019 <- df_mapping_sf_May1_2019 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May1_2019 <- df_mapping_sf_May1_2019 %>% left_join(state_averages_May1_2019) %>% 
  left_join(mgmt_averages_May1_2019) %>% 
  left_join(inshore_offshore_averages_May1_2019)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2019, "df_mapping_sf_May1_2019_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May1_2019, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May1_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May1_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May1_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p





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
  mutate(weighted = percent_preds/100*61100) %>%  #61100 was total estimated pots for May_2 2019 from landings
mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May2_2019)

df_mapping_sf_May2_2019 <- cv_test16_all_data_data_May2_2019 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>% 
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


#average across states
state_averages_May2_2019 <- df_mapping_sf_May2_2019 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May2_2019 <- df_mapping_sf_May2_2019 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May2_2019 <- df_mapping_sf_May2_2019 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May2_2019 <- df_mapping_sf_May2_2019 %>% left_join(state_averages_May2_2019) %>% 
  left_join(mgmt_averages_May2_2019) %>% 
  left_join(inshore_offshore_averages_May2_2019)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2019, "df_mapping_sf_May2_2019_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May2_2019, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May2_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May2_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May2_2019, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p


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
  mutate(difference = weighted - tottraps)

glimpse(cv_test16_all_data_data_May1_2020)


df_mapping_sf_May1_2020 <- cv_test16_all_data_data_May1_2020 %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) %>% 
  #writing shapefile has issues with column names, so drop most columns
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>%
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


#average across states
state_averages_May1_2020 <- df_mapping_sf_May1_2020 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May1_2020 <- df_mapping_sf_May1_2020 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May1_2020 <- df_mapping_sf_May1_2020 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May1_2020 <- df_mapping_sf_May1_2020 %>% left_join(state_averages_May1_2020) %>% 
  left_join(mgmt_averages_May1_2020) %>% 
  left_join(inshore_offshore_averages_May1_2020)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May1_2020, "df_mapping_sf_May1_2020_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May1_2020, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May1_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May1_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May1_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p



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
  select(-(SST_avg:dist_to_closed_km)) %>% 
  select(-(month_name:weighted_crab_ppp)) %>%
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
# 


#average across states
state_averages_May2_2020 <- df_mapping_sf_May2_2020 %>% group_by(OR_WA_waters ) %>% 
  summarise(median_diff_state = median(difference))

#average across mgmt areas
mgmt_averages_May2_2020 <- df_mapping_sf_May2_2020 %>% group_by(mgmt_area) %>% 
  summarise(median_diff_mgmt = median(difference))

##average across slightly larger mgmt areas in OR - need to decide what these are

#average across inshore/offshore
inshore_offshore_averages_May2_2020 <- df_mapping_sf_May2_2020 %>% group_by(inshore_offshore) %>% 
  summarise(median_diff_shore = median(difference))

df_mapping_sf_May2_2020 <- df_mapping_sf_May2_2020 %>% left_join(state_averages_May2_2020) %>% 
  left_join(mgmt_averages_May2_2020) %>% 
  left_join(inshore_offshore_averages_May2_2020)

# #export shapefile for QGIS
# #st_write(df_mapping_sf_May2_2020, "df_mapping_sf_May2_2020_20230504.shp")

#plot

#across all WC
p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
  geom_violin() +
  coord_flip()+
  theme_classic()
p

#across states
p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ OR_WA_waters) +
  coord_flip()+
  theme_classic()
p

#across mgmt areas
p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ mgmt_area) +
  coord_flip()+
  theme_classic()
p

#across larger mgmt areas (in OR)


#inshore vs offshore
p <- ggplot(df_mapping_sf_May2_2020, aes(x='', y=difference)) + 
  geom_violin() +
  facet_wrap(~ inshore_offshore) +
  coord_flip()+
  theme_classic()
p
 
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#would e.g. avg. actual pots in grid in all May_1/May_2 across all years produce an equally good map?

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


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
##FIGURE 2 - maps of avg pots in grids across all half-month time steps in winter vs summer


#avg across all winters:

df_full_filtered_winter <- df_full %>% 
  filter(month_name %in% c('December', 'January', 'February', 'March', 'April')) 

df_full_summary_winter <- df_full_filtered_winter %>% 
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps))

df_mapping_sf_summary_winter <- df_full_summary_winter %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS -- NOT ADJUSTED FOR CONFIDENTIALITY
#st_write(df_mapping_sf_summary_winter, "df_mapping_sf_summary_winter.shp")


#avg across all summers:

df_full_filtered_summer <- df_full %>% 
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) 

df_full_summary_summer <- df_full_filtered_summer %>% 
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(avg_pots = mean(tottraps))

df_mapping_sf_summary_summer <- df_full_summary_summer %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 

#export shapefile for QGIS -- NOT ADJUSTED FOR CONFIDENTIALITY
#st_write(df_mapping_sf_summary_summer, "df_mapping_sf_summary_summer.shp")



#check confidentiality for figure 2

#need df where pots are points
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))

#fix cases with pots with NA for GridID

#list cases where GridID = NA
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(is.na(GRID5KM_ID))
unique(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA$SetID2)

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA %>% 
  mutate(
    GRID5KM_ID = case_when(
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2017-2018_150255', 'OR_2017-2018_150487') ~ 108730,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_60805', 'OR_2009-2010_63542', 'OR_2009-2010_63844', 'OR_2013-2014_114062', 'OR_2013-2014_115031',
                                        'OR_2013-2014_116872', 'OR_2013-2014_118538', 'OR_2013-2014_118660', 'OR_2013-2014_118809', 'OR_2013-2014_120280',
                                        'OR_2013-2014_120830', 'OR_2013-2014_121068', 'OR_2013-2014_121501', 'OR_2013-2014_121662', 'OR_2013-2014_134735','OR_2015-2016_134735') ~ 91891,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_47313', 'OR_2018-2019_29393', 'OR_2018-2019_29397', 'OR_2018-2019_29400', 'OR_2018-2019_29404',
                                        'OR_2018-2019_29412', 'OR_2018-2019_29414', 'OR_2018-2019_29418', 'OR_2018-2019_29428', 'OR_2018-2019_29432',
                                        'OR_2008-2009_35331','OR_2008-2009_34625','OR_2008-2009_21606','OR_2008-2009_22112','OR_2008-2009_23531') ~ 117311,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_5402', 'WA_2009-2010_5407', 'WA_2010-2011_98515', 'WA_2010-2011_98521', 'WA_2010-2011_98528',
                                        'WA_2010-2011_98536', 'WA_2010-2011_98543', 'WA_2013-2014_17748', 'WA_2014-2015_4429', 'WA_2015-2016_11939',
                                        'WA_2017-2018_24912') ~ 117640,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2017-2018_7660', 'WA_2019-2020_11073', 'WA_2019-2020_11076', 'WA_2019-2020_11084', 'WA_2019-2020_11089',
                                        'WA_2019-2020_11092', 'WA_2019-2020_11098', 'WA_2019-2020_11134') ~ 122588,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_15317', 'WA_2009-2010_15318', 'WA_2009-2010_8387', 'WA_2009-2010_8389', 'WA_2009-2010_8391',
                                        'WA_2012-2013_21600', 'WA_2012-2013_21603', 'WA_2012-2013_21612', 'WA_2012-2013_21617', 'WA_2012-2013_21623',
                                        'WA_2012-2013_21625', 'WA_2012-2013_21628', 'WA_2012-2013_21633', 'WA_2012-2013_21639', 'WA_2017-2018_24617',
                                        'WA_2017-2018_24623', 'WA_2017-2018_24629', 'WA_2017-2018_24635', 'WA_2017-2018_24641', 'WA_2017-2018_24647',
                                        'WA_2017-2018_24653', 'WA_2017-2018_24659', 'WA_2017-2018_24665', 'WA_2017-2018_24671', 'WA_2017-2018_24677',
                                        'WA_2017-2018_24683', 'WA_2017-2018_24689', 'WA_2017-2018_24695', 'WA_2017-2018_24701', 'WA_2017-2018_24707',
                                        'WA_2017-2018_24713', 'WA_2017-2018_24719', 'WA_2017-2018_24725', 'WA_2017-2018_24731', 'WA_2017-2018_24737',
                                        'WA_2017-2018_24743', 'WA_2017-2018_24749', 'WA_2017-2018_24755', 'WA_2017-2018_24761', 'WA_2017-2018_24767',
                                        'WA_2017-2018_24773', 'WA_2017-2018_24779', 'WA_2017-2018_24785', 'WA_2017-2018_23332', 'WA_2017-2018_23335',
                                        'WA_2017-2018_23341', 'WA_2018-2019_28295', 'WA_2018-2019_28298', 'WA_2018-2019_28301', 'WA_2018-2019_28306',
                                        'WA_2018-2019_28309', 'WA_2018-2019_28312' ) ~ 122589,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2018-2019_2662', 'WA_2018-2019_28260', 'WA_2018-2019_28264', 'WA_2018-2019_28268', 'WA_2018-2019_28272',
                                        'WA_2018-2019_5671', 'WA_2018-2019_5674', 'WA_2018-2019_5677', 'WA_2018-2019_5680', 'WA_2018-2019_5683',
                                        'WA_2017-2018_23466', 'WA_2017-2018_23476', 'WA_2017-2018_23481', 'WA_2017-2018_23486', 'WA_2014-2015_28049',
                                        'WA_2014-2015_28051', 'WA_2014-2015_28053', 'WA_2014-2015_28055', 'WA_2014-2015_28057', 'WA_2014-2015_28059',
                                        'WA_2012-2013_15792', 'WA_2012-2013_15800', 'WA_2012-2013_15807', 'WA_2012-2013_15814', 'WA_2012-2013_15821',
                                        'WA_2012-2013_15829', 'WA_2012-2013_15837', 'WA_2012-2013_15844', 'WA_2012-2013_15852', 'WA_2012-2013_15860',
                                        'WA_2012-2013_15867', 'WA_2012-2013_15875', 'WA_2012-2013_15877', 'WA_2012-2013_15885', 'WA_2012-2013_15888',
                                        'WA_2012-2013_15892', 'WA_2012-2013_15900', 'WA_2012-2013_15909', 'WA_2012-2013_15916', 'WA_2012-2013_15922',
                                        'WA_2012-2013_15930', 'WA_2012-2013_15932', 'WA_2012-2013_15940', 'WA_2012-2013_15944', 'WA_2012-2013_15946',
                                        'WA_2012-2013_15954', 'WA_2012-2013_15965', 'WA_2012-2013_15970', 'WA_2017-2018_23471', 'WA_2018-2019_5686') ~ 122919,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_9919', 'WA_2013-2014_15078', 'WA_2013-2014_15080', 'WA_2013-2014_15081', 'WA_2013-2014_15083',
                                        'WA_2013-2014_15085', 'WA_2013-2014_15087', 'WA_2013-2014_15089', 'WA_2013-2014_15091', 'WA_2013-2014_15093',
                                        'WA_2013-2014_15095', 'WA_2013-2014_15097', 'WA_2013-2014_15099', 'WA_2013-2014_15101', 'WA_2013-2014_15103',
                                        'WA_2013-2014_15105', 'WA_2013-2014_15107', 'WA_2013-2014_15109', 'WA_2013-2014_15111', 'WA_2013-2014_15113',
                                        'WA_2013-2014_15115', 'WA_2013-2014_15117', 'WA_2013-2014_15119', 'WA_2013-2014_15121', 'WA_2013-2014_15123',
                                        'WA_2013-2014_15125', 'WA_2013-2014_15127', 'WA_2013-2014_15129', 'WA_2013-2014_15131', 'WA_2013-2014_15133',
                                        'WA_2013-2014_15135', 'WA_2013-2014_15137', 'WA_2013-2014_15139', 'WA_2013-2014_15141', 'WA_2013-2014_15143',
                                        'WA_2013-2014_15145', 'WA_2013-2014_15147', 'WA_2013-2014_15149', 'WA_2013-2014_15151', 'WA_2013-2014_15153',
                                        'WA_2013-2014_15155', 'WA_2013-2014_15157', 'WA_2013-2014_15159', 'WA_2015-2016_1078', 'WA_2015-2016_1088',
                                        'WA_2015-2016_10945', 'WA_2015-2016_10963', 'WA_2015-2016_1120', 'WA_2015-2016_1132', 'WA_2015-2016_1138',
                                        'WA_2015-2016_1163', 'WA_2015-2016_1179', 'WA_2015-2016_11795', 'WA_2015-2016_11809', 'WA_2015-2016_11820',
                                        'WA_2015-2016_11841', 'WA_2015-2016_11858', 'WA_2015-2016_1199', 'WA_2015-2016_1233', 'WA_2015-2016_1250',
                                        'WA_2015-2016_5428', 'WA_2015-2016_5444', 'WA_2015-2016_5462', 'WA_2015-2016_24635', 'WA_2015-2016_24638',
                                        'WA_2015-2016_24641', 'WA_2015-2016_24643', 'WA_2015-2016_24646', 'WA_2016-2017_26835', 'WA_2017-2018_11644',
                                        'WA_2019-2020_14099') ~ 120941,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_28525', 'WA_2017-2018_26745') ~ 120280,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2015-2016_3586', 'WA_2015-2016_3592', 'WA_2015-2016_3594', 'WA_2015-2016_3599', 'WA_2015-2016_3605',
                                        'WA_2015-2016_3610', 'WA_2015-2016_3614', 'WA_2015-2016_3630', 'WA_2015-2016_3632', 'WA_2015-2016_3634',
                                        'WA_2015-2016_3640', 'WA_2015-2016_3642', 'WA_2015-2016_3648', 'WA_2017-2018_7662', 'WA_2017-2018_7665',
                                        'WA_2017-2018_7668' ) ~ 122259
    )
  )

##############
# these points are in a grid that is not part of the 5x5km gridding because land areas are excluded by Blake in an earlier step
#NA -- WA_2012-2013_22421, WA_2012-2013_22423, WA_2012-2013_22425, WA_2012-2013_22427, WA_2012-2013_22429,
#      WA_2012-2013_22431, WA_2012-2013_22433, WA_2012-2013_22435, WA_2012-2013_22437, WA_2012-2013_22441,
#      WA_2012-2013_22443, WA_2012-2013_22445, WA_2012-2013_22447, WA_2012-2013_22449, WA_2012-2013_22451,
#      WA_2012-2013_22453, WA_2012-2013_22455, WA_2012-2013_22457, WA_2012-2013_22459, WA_2012-2013_22461,
#      WA_2012-2013_22463, WA_2012-2013_22465, WA_2012-2013_22467, WA_2012-2013_22469, WA_2012-2013_22471,
#      WA_2012-2013_22473, WA_2012-2013_22475, WA_2012-2013_22477, WA_2012-2013_22479, WA_2012-2013_22481,
#      WA_2012-2013_22483, WA_2012-2013_22485, WA_2012-2013_22486

#the part of the df that didn't have NAs for GridID
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(!is.na(GRID5KM_ID))

all_logs_points <- rbind(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA,traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED) %>% 
  #remove the final cases where GridID is NA
  filter(!is.na(GRID5KM_ID)) #33 pots removed, so 0%


#don't need the 2 first OR seasons
all_logs_points  <- all_logs_points %>% filter(!season %in% c('2007-2008', '2008-2009'))

#assign winter vs summer
all_logs_points_winter_summer <- all_logs_points %>% 
  mutate(winter_summer = ifelse(month_name %in% c('December', 'January', 'February', 'March', 'April'), 'winter','summer')) %>% 
  #the raw logs had few cases with no month name
  filter(!is.na(month_name))

#separate dfs for winter and summer
all_logs_points_winter <- all_logs_points_winter_summer %>% filter(winter_summer =="winter")
all_logs_points_summer <- all_logs_points_winter_summer %>% filter(winter_summer =="summer")

##check for confidentiality
#early WA data are the ones with no actual vessel ID, but still works
confidentiality_winter <- all_logs_points_winter %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(n_unique_vessels = n_distinct(Vessel)) %>% 
  mutate(confidential=ifelse(n_unique_vessels < 3, 'Y', 'N'))

confidentiality_summer <- all_logs_points_summer %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(n_unique_vessels = n_distinct(Vessel)) %>% 
  mutate(confidential=ifelse(n_unique_vessels < 3, 'Y', 'N'))


#join to shapefiles --  ADJUSTED FOR CONFIDENTIALITY

df_mapping_sf_summary_winter_conf <- df_mapping_sf_summary_winter %>% left_join(confidentiality_winter, by=c('GRID5KM_ID')) %>% 
  #all cases of NA for confidentiality label were grids with 0 effort ever, so label them as N
  mutate(confidential = ifelse(is.na(confidential),'N', confidential))

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_winter_conf, "df_mapping_sf_summary_winter_conf.shp")


df_mapping_sf_summary_summer_conf <- df_mapping_sf_summary_summer %>% left_join(confidentiality_summer, by=c('GRID5KM_ID')) %>% 
  #most cases of NA for confidentiality label were grids with 0 effort ever, so label them as N. the few others label as Y
  mutate(confidential = case_when(is.na(n_unique_vessels) & avg_pots != 0 ~ 'Y',
                                  !is.na(n_unique_vessels) ~ confidential)) %>% 
  mutate(confidential = ifelse(avg_pots == 0,'N', confidential)) 

#export shapefile for QGIS
#st_write(df_mapping_sf_summary_summer_conf, "df_mapping_sf_summary_summer_conf.shp")

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------


###THESE ARE USED FOR FIGURES IN MS###

#avg/median predicted integrated over a long time period 
# -- for May_1 across all years, and for May-2 across all years

#tidy dfs and join all May_1s
May_1_2016 <- cv_test16_all_data_data_May1_2016 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted) %>% 
  mutate(tottraps = tottraps + 0.000001) %>%  #do this as otherwise if divide by 0 just get infinity
  mutate(predicted = predicted + 0.000001) %>%  #so the difference stays the same
  mutate(percent_diff = (predicted-tottraps)/tottraps*100)

May_1_2017 <- cv_test16_all_data_data_May1_2017 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted) %>% 
  mutate(tottraps = tottraps + 0.000001) %>%  #do this as otherwise if divide by 0 just get infinity
  mutate(predicted = predicted + 0.000001) %>%  #so the difference stays the same
  mutate(percent_diff = (predicted-tottraps)/tottraps*100)

May_1_2018 <- cv_test16_all_data_data_May1_2018 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted) %>% 
  mutate(tottraps = tottraps + 0.000001) %>%  #do this as otherwise if divide by 0 just get infinity
  mutate(predicted = predicted + 0.000001) %>%  #so the difference stays the same
  mutate(percent_diff = (predicted-tottraps)/tottraps*100)

May_1_2019 <- cv_test16_all_data_data_May1_2019 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted) %>% 
  mutate(tottraps = tottraps + 0.000001) %>%  #do this as otherwise if divide by 0 just get infinity
  mutate(predicted = predicted + 0.000001) %>%  #so the difference stays the same
  mutate(percent_diff = (predicted-tottraps)/tottraps*100)

May_1_2020 <- cv_test16_all_data_data_May1_2020 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted) %>% 
  mutate(tottraps = tottraps + 0.000001) %>%  #do this as otherwise if divide by 0 just get infinity
  mutate(predicted = predicted + 0.000001) %>%  #so the difference stays the same
  mutate(percent_diff = (predicted-tottraps)/tottraps*100)


all_May_1_predicted <- rbind(May_1_2016,May_1_2017,May_1_2017,May_1_2019,May_1_2020)

all_May_1_summarised <- all_May_1_predicted %>%  
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(mean_tottraps = mean(tottraps),
            median_tottraps = median(tottraps),
            mean_predicted = mean(predicted),
            median_predicted = median(predicted),
            mean_difference = mean(difference),
            median_difference = median(difference))

all_May_1_summarised_sf <- all_May_1_summarised %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) 

# #export shapefile for QGIS
# #st_write(all_May_1_summarised_sf, "all_May_1_summarised_sf.shp")

##For plotting
all_May_1_predicted_sf <- all_May_1_predicted %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) 

# all_May_1_predicted_sf$mgmt_area <- factor(all_May_1_predicted_sf$mgmt_area, levels = c("59A-1", "59A-2", "60A-1",
#                                                                                         "60B", "60A-2", "60C",
#                                                                                         "50-A", "60D", "50-B",
#                                                                                         "50-C", "50-D", "50-E",
#                                                                                         "50-F", "50-G", "50-H",
#                                                                                         "50-I", "50-J", "50-K","50-L"))

all_May_1_predicted_sf$mgmt_area <- factor(all_May_1_predicted_sf$mgmt_area, levels = c("50-L", "50-K", "50-J", "50-I",
                                                                                        "50-H", "50-G", "50-F",
                                                                                        "50-E", "50-D", "50-C",
                                                                                        "50-B", "50-A", "60D",
                                                                                        "60C", "60A-2", "60B",
                                                                                        "60A-1", "59A-2", "59A-1"))



####density ridges instead of violin plot

###FIGURE 6b ######
p <- ggplot(all_May_1_predicted_sf, aes(x = difference, y = mgmt_area)) + 
  geom_density_ridges(rel_min_height = 0.005, alpha = 0.6) +
  xlim(-1000, 1000)+
  #coord_flip()+
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 13, colour = 'black'),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_line(colour = "grey90")
        )
p



#across  OR vs WA
###FIGURE 6c ######
all_May_1_predicted_sf <- all_May_1_predicted_sf %>% 
  mutate(Fishing_State = ifelse(mgmt_area %in% c('59A-1','59A-2','60A-1','60A-2','60B','60C'), 'WA','OR')) #we'll ignore 60D here as that is both OR and WA

p <- ggplot(all_May_1_predicted_sf, aes(x = difference, y = Fishing_State, fill = Fishing_State)) + 
  geom_density_ridges(rel_min_height = 0.005, alpha = 0.7) +
  xlim(-1000, 1000)+
  #coord_flip()+
  scale_fill_manual(values=c("navy", "#00843D")) +
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position='none'
  )
p



#inshore vs offshore
###FIGURE 6d ######
p <- ggplot(all_May_1_predicted_sf, aes(x = difference, y = inshore_offshore, fill = inshore_offshore)) + 
  geom_density_ridges(rel_min_height = 0.005) + #, alpha = 0.8
  xlim(-1000, 1000)+
  #coord_flip()+
  scale_fill_manual(values=c("skyblue1", "blue2")) +
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position='none'
  )
p




#tidy dfs and join all May_2s
May_2_2016 <- cv_test16_all_data_data_May2_2016 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted)

May_2_2017 <- cv_test16_all_data_data_May2_2017 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted)

May_2_2018 <- cv_test16_all_data_data_May2_2018 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted)

May_2_2019 <- cv_test16_all_data_data_May2_2019 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted)

May_2_2020 <- cv_test16_all_data_data_May2_2020 %>% 
  select(GRID5KM_ID, season, half_month, grd_x, grd_y, tottraps, weighted, difference) %>% 
  rename(predicted = weighted)


all_May_2_predicted <- rbind(May_2_2016,May_2_2017,May_2_2017,May_2_2019,May_2_2020)

all_May_2_summarised <- all_May_2_predicted %>%  
  group_by(GRID5KM_ID, grd_x, grd_y) %>% 
  summarise(mean_tottraps = mean(tottraps),
            median_tottraps = median(tottraps),
            mean_predicted = mean(predicted),
            median_predicted = median(predicted),
            mean_difference = mean(difference),
            median_difference = median(difference))

all_May_2_summarised_sf <- all_May_2_summarised %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) 

# #export shapefile for QGIS
# #st_write(all_May_2_summarised_sf, "all_May_2_summarised_sf.shp")

##For plotting
all_May_2_predicted_sf <- all_May_2_predicted %>% left_join(restricted_study_area_management_areas_sp, by=c('GRID5KM_ID')) %>%
  select(-NGDC_GRID, -ORIG_AREA) 

# all_May_2_predicted_sf$mgmt_area <- factor(all_May_2_predicted_sf$mgmt_area, levels = c("59A-1", "59A-2", "60A-1",
#                                                                                         "60B", "60A-2", "60C",
#                                                                                         "50-A", "60D", "50-B",
#                                                                                         "50-C", "50-D", "50-E",
#                                                                                         "50-F", "50-G", "50-H",
#                                                                                         "50-I", "50-J", "50-K","50-L"))
# 
all_May_2_predicted_sf$mgmt_area <- factor(all_May_2_predicted_sf$mgmt_area, levels = c("50-L", "50-K", "50-J", "50-I",
                                                                                        "50-H", "50-G", "50-F",
                                                                                        "50-E", "50-D", "50-C",
                                                                                        "50-B", "50-A", "60D",
                                                                                        "60C", "60A-2", "60B",
                                                                                        "60A-1", "59A-2", "59A-1"))


###SUPPLEMENTARY FIGURE S5b ######

p <- ggplot(all_May_2_predicted_sf, aes(x = difference, y = mgmt_area)) + 
  geom_density_ridges(rel_min_height = 0.005, alpha = 0.6) +
  xlim(-1000, 1000)+
  #coord_flip()+
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 13, colour = 'black'),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_line(colour = "grey90")
  )
p



#across  OR vs WA
###SUPPLEMENTARY FIGURE S5c ######
all_May_2_predicted_sf <- all_May_2_predicted_sf %>% 
  mutate(Fishing_State = ifelse(mgmt_area %in% c('59A-1','59A-2','60A-1','60A-2','60B','60C'), 'WA','OR')) #we'll ignore 60D here as that is both OR and WA

p <- ggplot(all_May_2_predicted_sf, aes(x = difference, y = Fishing_State, fill = Fishing_State)) + 
  geom_density_ridges(rel_min_height = 0.005, alpha = 0.7) +
  xlim(-1000, 1000)+
  #coord_flip()+
  scale_fill_manual(values=c("navy", "#00843D")) +
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position='none'
  )
p


#inshore vs offshore
###SUPPLEMENTARY FIGURE S5d ######
p <- ggplot(all_May_2_predicted_sf, aes(x = difference, y = inshore_offshore, fill = inshore_offshore)) + 
  geom_density_ridges(rel_min_height = 0.005) + #, alpha = 0.8
  xlim(-1000, 1000)+
  #coord_flip()+
  scale_fill_manual(values=c("skyblue1", "blue2")) +
  xlab("Difference (predicted-actual)") +
  ylab("") +
  geom_vline(xintercept = 0)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position='none'
  )
p






#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#check confidentiality of mgmt areas

#need df where pots are points
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))

#fix cases with pots with NA for GridID

#list cases where GridID = NA
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(is.na(GRID5KM_ID))
unique(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA$SetID2)

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA %>% 
  mutate(
    GRID5KM_ID = case_when(
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2017-2018_150255', 'OR_2017-2018_150487') ~ 108730,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_60805', 'OR_2009-2010_63542', 'OR_2009-2010_63844', 'OR_2013-2014_114062', 'OR_2013-2014_115031',
                                        'OR_2013-2014_116872', 'OR_2013-2014_118538', 'OR_2013-2014_118660', 'OR_2013-2014_118809', 'OR_2013-2014_120280',
                                        'OR_2013-2014_120830', 'OR_2013-2014_121068', 'OR_2013-2014_121501', 'OR_2013-2014_121662', 'OR_2013-2014_134735','OR_2015-2016_134735') ~ 91891,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_47313', 'OR_2018-2019_29393', 'OR_2018-2019_29397', 'OR_2018-2019_29400', 'OR_2018-2019_29404',
                                        'OR_2018-2019_29412', 'OR_2018-2019_29414', 'OR_2018-2019_29418', 'OR_2018-2019_29428', 'OR_2018-2019_29432',
                                        'OR_2008-2009_35331','OR_2008-2009_34625','OR_2008-2009_21606','OR_2008-2009_22112','OR_2008-2009_23531') ~ 117311,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_5402', 'WA_2009-2010_5407', 'WA_2010-2011_98515', 'WA_2010-2011_98521', 'WA_2010-2011_98528',
                                        'WA_2010-2011_98536', 'WA_2010-2011_98543', 'WA_2013-2014_17748', 'WA_2014-2015_4429', 'WA_2015-2016_11939',
                                        'WA_2017-2018_24912') ~ 117640,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2017-2018_7660', 'WA_2019-2020_11073', 'WA_2019-2020_11076', 'WA_2019-2020_11084', 'WA_2019-2020_11089',
                                        'WA_2019-2020_11092', 'WA_2019-2020_11098', 'WA_2019-2020_11134') ~ 122588,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_15317', 'WA_2009-2010_15318', 'WA_2009-2010_8387', 'WA_2009-2010_8389', 'WA_2009-2010_8391',
                                        'WA_2012-2013_21600', 'WA_2012-2013_21603', 'WA_2012-2013_21612', 'WA_2012-2013_21617', 'WA_2012-2013_21623',
                                        'WA_2012-2013_21625', 'WA_2012-2013_21628', 'WA_2012-2013_21633', 'WA_2012-2013_21639', 'WA_2017-2018_24617',
                                        'WA_2017-2018_24623', 'WA_2017-2018_24629', 'WA_2017-2018_24635', 'WA_2017-2018_24641', 'WA_2017-2018_24647',
                                        'WA_2017-2018_24653', 'WA_2017-2018_24659', 'WA_2017-2018_24665', 'WA_2017-2018_24671', 'WA_2017-2018_24677',
                                        'WA_2017-2018_24683', 'WA_2017-2018_24689', 'WA_2017-2018_24695', 'WA_2017-2018_24701', 'WA_2017-2018_24707',
                                        'WA_2017-2018_24713', 'WA_2017-2018_24719', 'WA_2017-2018_24725', 'WA_2017-2018_24731', 'WA_2017-2018_24737',
                                        'WA_2017-2018_24743', 'WA_2017-2018_24749', 'WA_2017-2018_24755', 'WA_2017-2018_24761', 'WA_2017-2018_24767',
                                        'WA_2017-2018_24773', 'WA_2017-2018_24779', 'WA_2017-2018_24785', 'WA_2017-2018_23332', 'WA_2017-2018_23335',
                                        'WA_2017-2018_23341', 'WA_2018-2019_28295', 'WA_2018-2019_28298', 'WA_2018-2019_28301', 'WA_2018-2019_28306',
                                        'WA_2018-2019_28309', 'WA_2018-2019_28312' ) ~ 122589,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2018-2019_2662', 'WA_2018-2019_28260', 'WA_2018-2019_28264', 'WA_2018-2019_28268', 'WA_2018-2019_28272',
                                        'WA_2018-2019_5671', 'WA_2018-2019_5674', 'WA_2018-2019_5677', 'WA_2018-2019_5680', 'WA_2018-2019_5683',
                                        'WA_2017-2018_23466', 'WA_2017-2018_23476', 'WA_2017-2018_23481', 'WA_2017-2018_23486', 'WA_2014-2015_28049',
                                        'WA_2014-2015_28051', 'WA_2014-2015_28053', 'WA_2014-2015_28055', 'WA_2014-2015_28057', 'WA_2014-2015_28059',
                                        'WA_2012-2013_15792', 'WA_2012-2013_15800', 'WA_2012-2013_15807', 'WA_2012-2013_15814', 'WA_2012-2013_15821',
                                        'WA_2012-2013_15829', 'WA_2012-2013_15837', 'WA_2012-2013_15844', 'WA_2012-2013_15852', 'WA_2012-2013_15860',
                                        'WA_2012-2013_15867', 'WA_2012-2013_15875', 'WA_2012-2013_15877', 'WA_2012-2013_15885', 'WA_2012-2013_15888',
                                        'WA_2012-2013_15892', 'WA_2012-2013_15900', 'WA_2012-2013_15909', 'WA_2012-2013_15916', 'WA_2012-2013_15922',
                                        'WA_2012-2013_15930', 'WA_2012-2013_15932', 'WA_2012-2013_15940', 'WA_2012-2013_15944', 'WA_2012-2013_15946',
                                        'WA_2012-2013_15954', 'WA_2012-2013_15965', 'WA_2012-2013_15970', 'WA_2017-2018_23471', 'WA_2018-2019_5686') ~ 122919,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_9919', 'WA_2013-2014_15078', 'WA_2013-2014_15080', 'WA_2013-2014_15081', 'WA_2013-2014_15083',
                                        'WA_2013-2014_15085', 'WA_2013-2014_15087', 'WA_2013-2014_15089', 'WA_2013-2014_15091', 'WA_2013-2014_15093',
                                        'WA_2013-2014_15095', 'WA_2013-2014_15097', 'WA_2013-2014_15099', 'WA_2013-2014_15101', 'WA_2013-2014_15103',
                                        'WA_2013-2014_15105', 'WA_2013-2014_15107', 'WA_2013-2014_15109', 'WA_2013-2014_15111', 'WA_2013-2014_15113',
                                        'WA_2013-2014_15115', 'WA_2013-2014_15117', 'WA_2013-2014_15119', 'WA_2013-2014_15121', 'WA_2013-2014_15123',
                                        'WA_2013-2014_15125', 'WA_2013-2014_15127', 'WA_2013-2014_15129', 'WA_2013-2014_15131', 'WA_2013-2014_15133',
                                        'WA_2013-2014_15135', 'WA_2013-2014_15137', 'WA_2013-2014_15139', 'WA_2013-2014_15141', 'WA_2013-2014_15143',
                                        'WA_2013-2014_15145', 'WA_2013-2014_15147', 'WA_2013-2014_15149', 'WA_2013-2014_15151', 'WA_2013-2014_15153',
                                        'WA_2013-2014_15155', 'WA_2013-2014_15157', 'WA_2013-2014_15159', 'WA_2015-2016_1078', 'WA_2015-2016_1088',
                                        'WA_2015-2016_10945', 'WA_2015-2016_10963', 'WA_2015-2016_1120', 'WA_2015-2016_1132', 'WA_2015-2016_1138',
                                        'WA_2015-2016_1163', 'WA_2015-2016_1179', 'WA_2015-2016_11795', 'WA_2015-2016_11809', 'WA_2015-2016_11820',
                                        'WA_2015-2016_11841', 'WA_2015-2016_11858', 'WA_2015-2016_1199', 'WA_2015-2016_1233', 'WA_2015-2016_1250',
                                        'WA_2015-2016_5428', 'WA_2015-2016_5444', 'WA_2015-2016_5462', 'WA_2015-2016_24635', 'WA_2015-2016_24638',
                                        'WA_2015-2016_24641', 'WA_2015-2016_24643', 'WA_2015-2016_24646', 'WA_2016-2017_26835', 'WA_2017-2018_11644',
                                        'WA_2019-2020_14099') ~ 120941,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_28525', 'WA_2017-2018_26745') ~ 120280,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2015-2016_3586', 'WA_2015-2016_3592', 'WA_2015-2016_3594', 'WA_2015-2016_3599', 'WA_2015-2016_3605',
                                        'WA_2015-2016_3610', 'WA_2015-2016_3614', 'WA_2015-2016_3630', 'WA_2015-2016_3632', 'WA_2015-2016_3634',
                                        'WA_2015-2016_3640', 'WA_2015-2016_3642', 'WA_2015-2016_3648', 'WA_2017-2018_7662', 'WA_2017-2018_7665',
                                        'WA_2017-2018_7668' ) ~ 122259
    )
  )

##############
# these points are in a grid that is not part of the 5x5km gridding because land areas are excluded by Blake in an earlier step
#NA -- WA_2012-2013_22421, WA_2012-2013_22423, WA_2012-2013_22425, WA_2012-2013_22427, WA_2012-2013_22429,
#      WA_2012-2013_22431, WA_2012-2013_22433, WA_2012-2013_22435, WA_2012-2013_22437, WA_2012-2013_22441,
#      WA_2012-2013_22443, WA_2012-2013_22445, WA_2012-2013_22447, WA_2012-2013_22449, WA_2012-2013_22451,
#      WA_2012-2013_22453, WA_2012-2013_22455, WA_2012-2013_22457, WA_2012-2013_22459, WA_2012-2013_22461,
#      WA_2012-2013_22463, WA_2012-2013_22465, WA_2012-2013_22467, WA_2012-2013_22469, WA_2012-2013_22471,
#      WA_2012-2013_22473, WA_2012-2013_22475, WA_2012-2013_22477, WA_2012-2013_22479, WA_2012-2013_22481,
#      WA_2012-2013_22483, WA_2012-2013_22485, WA_2012-2013_22486

#the part of the df that didn't have NAs for GridID
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(!is.na(GRID5KM_ID))

all_logs_points <- rbind(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA,traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED) %>% 
  #remove the final cases where GridID is NA
  filter(!is.na(GRID5KM_ID)) #33 pots removed, so 0%


#don't need the 2 first OR seasons
all_logs_points  <- all_logs_points %>% filter(!season %in% c('2007-2008', '2008-2009'))
#----------

restricted_study_area_management_areas <- read_csv(here::here('DCRB_sdmTMB',  'data', 'restricted_study_area_management_areas.csv')) 

all_logs_points_mgmt_areas <- all_logs_points %>% left_join(restricted_study_area_management_areas)


#assign half_month
all_logs_points_mgmt_areas <- all_logs_points_mgmt_areas %>% 
  mutate(d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) 

#only need May
all_logs_points_mgmt_areas_May <- all_logs_points_mgmt_areas %>% filter(month_name =="May")


##check for confidentiality
#early WA data are the ones with no actual vessel ID, but still works
confidentiality <- all_logs_points_mgmt_areas_May %>% 
  group_by(half_month, mgmt_area) %>% 
  summarise(n_unique_vessels = n_distinct(Vessel))




















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



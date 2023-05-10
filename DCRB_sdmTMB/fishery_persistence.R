#fishery persistence


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
library(ggridges)


#-------------------------------------------------------------------------------------------------

###WINTER

#-------------------------------------------------------------------------------------------------


winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230324.rds'))
glimpse(winter) 

winter_persistence <- winter %>% filter(tottraps>0) %>%  group_by(GRID5KM_ID) %>% summarise(n_seasons = n_distinct(season))

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))

winter_persistence_sf <- study_area %>%  left_join(winter_persistence, by=c('GRID5KM_ID'))

#Replace na values with 0 
winter_persistence_sf[is.na(winter_persistence_sf)] = 0

#n_seasons grouping
winter_persistence_sf <- winter_persistence_sf %>% 
  mutate(n_seasons_grouping = case_when(n_seasons == 0 ~ '0', 
                                        n_seasons %in% c(1,2) ~ '1-2', 
                                        n_seasons %in% c(3,4) ~ '3-4',                                        
                                        n_seasons %in% c(5,6) ~ '5-6',
                                        n_seasons %in% c(7,8) ~ '7-8', 
                                        n_seasons %in% c(9,10) ~ '9-10',
                                        n_seasons == 11 ~ '11'))


# #export shapefile for QGIS
# #st_write(winter_persistence_sf, "winter_persistence_sf.shp")


#-------------------------------------------------------------------------------------------------

###SUMMER

#-------------------------------------------------------------------------------------------------

summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230324.rds'))
glimpse(summer) 

summer_persistence <- summer %>% filter(tottraps>0) %>%  group_by(GRID5KM_ID) %>% summarise(n_seasons = n_distinct(season))

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))

summer_persistence_sf <- study_area %>%  left_join(summer_persistence, by=c('GRID5KM_ID'))

#Replace na values with 0 
summer_persistence_sf[is.na(summer_persistence_sf)] = 0

summer_persistence_sf <- summer_persistence_sf %>% 
  mutate(n_seasons_grouping = case_when(n_seasons == 0 ~ '0', 
                                        n_seasons %in% c(1,2) ~ '1-2', 
                                        n_seasons %in% c(3,4) ~ '3-4',                                        
                                        n_seasons %in% c(5,6) ~ '5-6',
                                        n_seasons %in% c(7,8) ~ '7-8', 
                                        n_seasons %in% c(9,10) ~ '9-10',
                                        n_seasons == 11 ~ '11'))

# #export shapefile for QGIS
# #st_write(summer_persistence_sf, "summer_persistence_sf.shp")


p <- ggplot(summer_persistence_sf, aes(factor(n_seasons_grouping))) + geom_bar() 
p










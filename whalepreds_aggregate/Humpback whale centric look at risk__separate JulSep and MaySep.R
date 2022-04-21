#'humpback whale centric' look at risk _ separate JulSep and MaySep

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(scales)

#-----------------------------------------------------------------------------------

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"


# Leena:
path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub

#-----------------------------------------------------------------------------------


# load the data
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)




# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)

grid.key_2 <- grid.key %>% 
  #into this join area_km_lno info from layer: grid.5km.lno
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

#-----------------------------------------------------------------------------------

x.hump_whale <- x.hump_2009_2020 %>% 
  inner_join(st_drop_geometry(grid.key_2), by = "GRID5KM_ID")
# for now join whale data to a larger grid, we will clip it to a specific domain later


#instead of working in calendar years, work in crab seasons -- also filter to 2013-2020
x.whale_crab_season <- x.hump_whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.whale_crab_season_v2 <- x.whale_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(season_month = paste0(season,"_",month)) %>% 
  select(-season_start, -season_end) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) 
glimpse(x.whale_crab_season_v2)


#----------------------------------------------------------------------------

# find 75th percentile value for hw - separately for Jul-Sep and May-Sep comparisons

#----------
##  Jul-Sep
#----------

# calculate MEAN whale values for grids across all of 2013-2020 - for Jul-Sep period only
# and then look at percentiles
x.whale.mean_all2013_2020_JulSep <- x.whale_crab_season_v2 %>% #this is already filtered for 2013-2020
  filter(month %in% c('07', '08', '09')) %>% 
  group_by(is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    #hw specific
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  left_join(grid.key_2, by = "GRID5KM_ID") # we will do spatial clip later
glimpse(x.whale.mean_all2013_2020_JulSep)

#find various percentile values from across 2013-2020
x.whale.all2013_2020_percentiles_JulSep <- x.whale.mean_all2013_2020_JulSep %>%
  filter(LATITUDE > 45) %>% #see email with Karin - clip data at ~45N
  #but we don't do any clipping in E-W of model
  ungroup() %>% 
  summarise(
    Humpback_dens_75th = quantile(Mean_Humpback_dens, probs=0.75, na.rm=TRUE),
    #for 'sensitivity testing', also get other percentiles
    Humpback_dens_50th = quantile(Mean_Humpback_dens, probs=0.50, na.rm=TRUE),
    Humpback_dens_60th = quantile(Mean_Humpback_dens, probs=0.60, na.rm=TRUE),
    Humpback_dens_70th = quantile(Mean_Humpback_dens, probs=0.70, na.rm=TRUE),
    Humpback_dens_80th = quantile(Mean_Humpback_dens, probs=0.80, na.rm=TRUE),
    Humpback_dens_90th = quantile(Mean_Humpback_dens, probs=0.90, na.rm=TRUE)
  ) 
glimpse(x.whale.all2013_2020_percentiles_JulSep)
# 0.02328934, is 75th percentile when clipped to 45N



#apply percentile value to each season
x.whale.mean_by_season_JulSep <- x.whale_crab_season_v2 %>%
  filter(month %in% c('07', '08', '09')) %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  inner_join(grid.5km)
glimpse(x.whale.mean_by_season_JulSep)

x.whale.all2013_2020_JulSep_quant_joined <- x.whale.mean_by_season_JulSep %>% 
  cbind(x.whale.all2013_2020_percentiles_JulSep)
glimpse(x.whale.all2013_2020_JulSep_quant_joined)

x.whale.all2013_2020_JulSep_good_habitats <- x.whale.all2013_2020_JulSep_quant_joined %>% 
  ungroup() %>% 
  mutate(HW_is_75th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_75th, 'Y', 'N'),
         #'sensitivity testing'
         HW_is_50th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_50th, 'Y', 'N'),
         HW_is_60th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_60th, 'Y', 'N'),
         HW_is_70th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_70th, 'Y', 'N'),
         HW_is_80th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_80th, 'Y', 'N'),
         HW_is_90th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_90th, 'Y', 'N')
  ) 
glimpse(x.whale.all2013_2020_JulSep_good_habitats)

#----------------
#Then try to look at what trap density was in the good hw habitat -- will need to check code
#bring in fishing data 
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

# average trap density (and count?) for each grid cell for May-Sep period
x.fish_WA_JulSep <- x.fish_WA %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
    #sum_M2_tottraps  = sum(M2_tottraps, na.rm=TRUE),
    #mean_tottraps = sum_M2_tottraps/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) #, mean_tottraps
glimpse(x.fish_WA_JulSep)


x.whale.2013_2020_JulSep_good_habitats_fishing <- x.whale.all2013_2020_JulSep_good_habitats %>% 
  left_join(x.fish_WA_JulSep, by=c('season', 'GRID5KM_ID'))
glimpse(x.whale.2013_2020_JulSep_good_habitats_fishing)


#in this situation doesn't matter if risk is 0 or NA
x.whale.2013_2020_JulSep_good_habitats_fishing_risk <- x.whale.2013_2020_JulSep_good_habitats_fishing %>% 
  mutate(
    hump_risk = Mean_Humpback_dens * mean_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_trapdens), 0, hump_risk)
  ) %>% 
  #remove 2019-20 as comparing pre-reg Jul-Sep to 2019
  filter(season != '2019-2020')



summary_75th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_75th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE),
  ) %>% 
  mutate(percentile = '75')
glimpse(summary_75th_HW_habitat_fishing_JulSep) 


summary_50th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_50th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '50')
glimpse(summary_50th_HW_habitat_fishing_JulSep) 

summary_60th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_60th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '60')
glimpse(summary_60th_HW_habitat_fishing_JulSep) 


summary_70th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_70th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '70')
glimpse(summary_70th_HW_habitat_fishing_JulSep) 

summary_80th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_80th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '80')
glimpse(summary_80th_HW_habitat_fishing_JulSep) 


summary_90th_HW_habitat_fishing_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_90th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '90')
glimpse(summary_90th_HW_habitat_fishing_JulSep)


summary_percentiles_JulSep <- rbind(
  summary_75th_HW_habitat_fishing_JulSep,
  summary_50th_HW_habitat_fishing_JulSep,
  summary_60th_HW_habitat_fishing_JulSep,
  summary_70th_HW_habitat_fishing_JulSep,
  summary_80th_HW_habitat_fishing_JulSep,
  summary_90th_HW_habitat_fishing_JulSep
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg'))



## overlap of fishery and good hw habitat per seasons -- JUL-SEP

#number of fishery grids that were also good hw habitat
test_summary_75_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_75th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '75')

#season     n_grids
#2013-2014    65
#2014-2015    52
#2015-2016    71
#2016-2017    71
#2017-2018    88
#2018-2019    5


test_summary_50_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_50th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '50')

test_summary_60_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_60th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '60')


test_summary_70_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_70th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '70')


test_summary_80_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_80th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '80')


test_summary_90_JulSep <- x.whale.2013_2020_JulSep_good_habitats_fishing_risk %>% 
  filter(HW_is_90th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  add_row(season = c("2018-2019"), n_grids = 0) %>% 
  mutate(percentile = '90')



summary_overlap_JulSep <- rbind(
  test_summary_75_JulSep,
  test_summary_50_JulSep,
  test_summary_60_JulSep,
  test_summary_70_JulSep,
  test_summary_80_JulSep,
  test_summary_90_JulSep
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg'))



#-----------------------------------------------------------------------------------
#----------
##  May-Sep
#----------

# calculate MEAN whale values for grids across all of 2013-2020 - for May-Sep period only
# and then look at percentiles
x.whale.mean_all2013_2020_MaySep <- x.whale_crab_season_v2 %>% #this is already filtered for 2013-2020
  filter(is_May_Sep == "Y") %>% 
  group_by(is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    #hw specific
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  left_join(grid.key_2, by = "GRID5KM_ID") # we will do spatial clip later
glimpse(x.whale.mean_all2013_2020_MaySep)

#find various percentile values from across 2013-2020
x.whale.all2013_2020_percentiles_MaySep <- x.whale.mean_all2013_2020_MaySep %>%
  filter(LATITUDE > 45) %>% #see email with Karin - clip data at ~45N
  #but we don't do any clipping in E-W of model
  ungroup() %>% 
  summarise(
    Humpback_dens_75th = quantile(Mean_Humpback_dens, probs=0.75, na.rm=TRUE),
    #for 'sensitivity testing', also get other percentiles
    Humpback_dens_50th = quantile(Mean_Humpback_dens, probs=0.50, na.rm=TRUE),
    Humpback_dens_60th = quantile(Mean_Humpback_dens, probs=0.60, na.rm=TRUE),
    Humpback_dens_70th = quantile(Mean_Humpback_dens, probs=0.70, na.rm=TRUE),
    Humpback_dens_80th = quantile(Mean_Humpback_dens, probs=0.80, na.rm=TRUE),
    Humpback_dens_90th = quantile(Mean_Humpback_dens, probs=0.90, na.rm=TRUE)
  ) 
glimpse(x.whale.all2013_2020_percentiles_MaySep)
# 0.02497305, is 75th percentile when clipped to 45N



#apply percentile value to each season
x.whale.mean_by_season_MaySep <- x.whale_crab_season_v2 %>%
  filter(is_May_Sep == "Y") %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  inner_join(grid.5km)
glimpse(x.whale.mean_by_season_MaySep)

x.whale.all2013_2020_MaySep_quant_joined <- x.whale.mean_by_season_MaySep %>% 
  cbind(x.whale.all2013_2020_percentiles_MaySep)
glimpse(x.whale.all2013_2020_MaySep_quant_joined)

x.whale.all2013_2020_MaySep_good_habitats <- x.whale.all2013_2020_MaySep_quant_joined %>% 
  ungroup() %>% 
  mutate(HW_is_75th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_75th, 'Y', 'N'),
         #'sensitivity testing'
         HW_is_50th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_50th, 'Y', 'N'),
         HW_is_60th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_60th, 'Y', 'N'),
         HW_is_70th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_70th, 'Y', 'N'),
         HW_is_80th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_80th, 'Y', 'N'),
         HW_is_90th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_90th, 'Y', 'N')
  ) 
glimpse(x.whale.all2013_2020_MaySep_good_habitats)

#----------------
#Then try to look at what trap density was in the good hw habitat -- will need to check code
#bring in fishing data 
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

# average trap density (and count?) for each grid cell for May-Sep period
x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
    #sum_M2_tottraps  = sum(M2_tottraps, na.rm=TRUE),
    #mean_tottraps = sum_M2_tottraps/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) #, mean_tottraps
glimpse(x.fish_WA_MaySep)


x.whale.2013_2020_MaySep_good_habitats_fishing <- x.whale.all2013_2020_MaySep_good_habitats %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID'))
glimpse(x.whale.2013_2020_MaySep_good_habitats_fishing)


#in this situation doesn't matter if risk is 0 or NA
x.whale.2013_2020_MaySep_good_habitats_fishing_risk <- x.whale.2013_2020_MaySep_good_habitats_fishing %>% 
  mutate(
    hump_risk = Mean_Humpback_dens * mean_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_trapdens), 0, hump_risk)
  ) %>% 
  #remove 2018-19 as comparing pre-reg May-Sep to 2020
  #keep 2018-2019 if want to do a map 
  filter(season != '2018-2019')



summary_75th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_75th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE),
    ) %>% 
  mutate(percentile = '75')
glimpse(summary_75th_HW_habitat_fishing_MaySep) 
# % change from pre-reg average
#pre-reg average
(29.958425+25.054179+23.829820+28.636783+28.783313)/5 ## 27.2525
(9.119614-27.2525)/27.2525*100 ##-66.5366


summary_50th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_50th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '50')
glimpse(summary_50th_HW_habitat_fishing_MaySep) 

summary_60th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_60th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '60')
glimpse(summary_60th_HW_habitat_fishing_MaySep) 



summary_70th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_70th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '70')
glimpse(summary_70th_HW_habitat_fishing_MaySep) 

summary_80th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_80th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '80')
glimpse(summary_80th_HW_habitat_fishing_MaySep) 



summary_90th_HW_habitat_fishing_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_90th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(hump_risk, na.rm=TRUE)
  ) %>% 
  mutate(percentile = '90')
glimpse(summary_90th_HW_habitat_fishing_MaySep)


summary_percentiles_MaySep <- rbind(
  summary_75th_HW_habitat_fishing_MaySep,
  summary_50th_HW_habitat_fishing_MaySep,
  summary_60th_HW_habitat_fishing_MaySep,
  summary_70th_HW_habitat_fishing_MaySep,
  summary_80th_HW_habitat_fishing_MaySep,
  summary_90th_HW_habitat_fishing_MaySep
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg'))





#----------------------------------------------------------------------------------------
#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2013_2014_MaySep <- read_rds(here::here('wdfw','data','dissolved_2013_2014_MaySep_WA_fishery_footprint_20220202.rds'))

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2014_2020_MaySep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only.shp')) %>% 
  st_transform(st_crs(dissolved_2014_2020_MaySep_non_conf)) #make it have same projection 

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49) 


hw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(HW_is_75th_or_higher)) %>% 
  filter(HW_is_75th_or_higher == 'Y')


map_hump_MaySep_75th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_subset_MaySep), 
          aes(fill=HW_is_75th_or_higher,
              col=HW_is_75th_or_higher
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  #geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_2014_2020_MaySep_non_conf, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black',linetype = "dotted",size=1, fill = NA) +  
  ggtitle("May-Sep 2019-2020 \ngood HW habitat (>75th) \nspatially clip at 45N \nwith non-conf. May-Sep fishery footprint (across 2014-2020)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_hump_MaySep_75th

png(paste0(path_figures, "/good_wh_habitat_MaySep_75th_across_20132020_applied to 2019_2020_spatially_clipped_45_with_all_NONCONF_summer_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep_75th,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#------------------------------------------------------------------------------------------------------------

## overlap of fishery and good hw habitat per seasons -- May-Sep


#number of fishery grids that were also good hw habitat
test_summary_75_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_75th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '75')

#season     n_grids
#2013-2014    124
#2014-2015    119
#2015-2016    106
#2016-2017    136
#2017-2018    167
#2018-2019    0
#2019-2020    87

# % change from pre-reg average
#pre-reg average
(124+119+106+136+167)/5 ## 130.4
(87-130.4)/130.4*100 ##-33.28221


test_summary_50_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_50th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '50')

test_summary_60_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_60th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '60')


test_summary_70_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_70th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '70')


test_summary_80_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_80th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '80')


test_summary_90_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_90th_or_higher == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  mutate(percentile = '90')


summary_overlap_MaySep <- rbind(
  test_summary_75_MaySep,
  test_summary_50_MaySep,
  test_summary_60_MaySep,
  test_summary_70_MaySep,
  test_summary_80_MaySep,
  test_summary_90_MaySep
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg'))








#----------------------------------------------------------------------------------------
#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2015_2016_JulSep <- read_rds(here::here('wdfw','data','dissolved_2015_2016_JulSep_WA_fishery_footprint_20220227.rds'))

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2014_2020_MaySep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only.shp')) %>% 
  st_transform(st_crs(dissolved_2014_2020_MaySep_non_conf)) #make it have same projection 

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49) 


hw_subset_JulSep <- x.whale.all2013_2020_JulSep_good_habitats %>%   #x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>%
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(HW_is_75th_or_higher)) %>% 
  filter(HW_is_75th_or_higher == 'Y')


map_hump_JulSep_75th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_subset_JulSep), 
          aes(fill=HW_is_75th_or_higher,
              col=HW_is_75th_or_higher
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  #geom_sf(data = dissolved_2015_2016_JulSep, color = 'black',size=1, fill = NA) +
  #geom_sf(data = dissolved_2014_2020_MaySep_non_conf, color = 'black',size=1, fill = NA) +
  #geom_sf(data = dissolved_study_area, color = 'black',linetype = "dotted",size=1, fill = NA) +  
  ggtitle("Jul-Sep 2013-2014, good HW habitat (>75th) \nspatially clip at 45N, with conf. Jul-Sep fishery footprint") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_hump_JulSep_75th

png(paste0(path_figures, "/good_wh_habitat_JulSep_80th_across_20142020_applied to 2018_2019_spatially_clipped_45_with_all_CONF_2019_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_JulSep_80th,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#------------------------------------------------------------------------------------------------------------




#-------------------
###    Plotting and GLM



###Jul-Sep

##sensitivity testing - risk in good HW habitat Jul-Sep
ts_fishing_in_good_hw_habitat_JulSep <- ggplot(summary_percentiles_JulSep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = percentile, color=percentile), size=1.8) +
  geom_point(aes(y = risk_sum, group = percentile, color=percentile), size=3.5) +
  ylab("Risk") +
  xlab("Season") +
  guides(color=guide_legend(nrow = 1, title="Percentile")) +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=50),
    #title = element_text(size = 15),
    legend.text = element_text(size=40),
    #legend.position = c(.93, .85),
    #legend.position = 'none',
    legend.position = 'bottom',
    axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=40),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_fishing_in_good_hw_habitat_JulSep

png(paste0(path_figures, "/ts_risk_sum_in_different_HW_habitat_JulSep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_fishing_in_good_hw_habitat_JulSep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



##GLM
# note that this is different to the fishery perspective, as that had one data point per month
# here good habtiat is defined across Jul-Sep and May-Sep, and there is only 1 data point per season

hist(summary_percentiles_JulSep$risk_sum)

summary_percentiles_JulSep$percentile <- as.factor(summary_percentiles_JulSep$percentile)

library(ggcorrplot)
model.matrix(~0+., data=summary_percentiles_JulSep) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod1_hump_JulSep <- glm(risk_sum ~ pre_post_reg + percentile,
                        family=gaussian, data=summary_percentiles_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_hump_JulSep)
hist(mod1_hump_JulSep$residuals)
plot(mod1_hump_JulSep)

library(multcomp)
summary(glht(mod1_hump_JulSep, mcp(percentile='Tukey')))


### OVERLAP JUL-SEP

#plot count of overlapping grids -- Jul-Sep
ts_overlapping_grids_count_all_JulSep <- ggplot(summary_overlap_JulSep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = percentile, color=percentile), size=1.8) +
  geom_point(aes(y = n_grids, group = percentile, color=percentile), size=3.5) +
  ylab("Overlap") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  theme_classic() +  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    #legend.position = c(.1, .3),
    legend.position = 'none',
    axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=40),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_overlapping_grids_count_all_JulSep



png(paste0(path_figures, "/count_overlap_fishery_and_good_HW_habitat_JulSep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_overlapping_grids_count_all_JulSep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


summary_overlap_JulSep$percentile <- as.numeric(summary_overlap_JulSep$percentile)
summary_overlap_JulSep$percentile <- as.factor(summary_overlap_JulSep$percentile)

hist(summary_overlap_JulSep$n_grids )

library(ggcorrplot)
model.matrix(~0+., data=summary_overlap_JulSep) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod2_hump_JulSep <- glm(n_grids ~ pre_post_reg + percentile,
                        family=gaussian, data=summary_overlap_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod2_hump_JulSep)
hist(mod2_hump_JulSep$residuals)
plot(mod2_hump_JulSep)

library(multcomp)
summary(glht(mod2_hump_JulSep, mcp(percentile='Tukey')))

summary_overlap_JulSep$percentilexprepost <- interaction(summary_overlap_JulSep$pre_post_reg,summary_overlap_JulSep$percentile)
glm.posthoc <- glm(n_grids ~  percentilexprepost, data=summary_overlap_JulSep,family="gaussian")
summary(glm.posthoc)


#---------------------------------------------------------------------

###    May-Sep    ###

ts_fishing_in_good_hw_habitat <- ggplot(summary_percentiles_MaySep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = percentile, color=percentile), size=1.8) +
  geom_point(aes(y = risk_sum, group = percentile, color=percentile), size=3.5) +
  ylab("Risk") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  guides(color=guide_legend(title="Percentile")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    #legend.position = c(.93, .85),
    legend.position = 'none',
    axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=40),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_fishing_in_good_hw_habitat


png(paste0(path_figures, "/ts_risk_sum_in_different_HW_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_fishing_in_good_hw_habitat,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


##GLM
# note that this is different to the fishery perspective, as that had one data point per month
# here good habtiat is defined across Jul-Sep and May-Sep, and there is only 1 data point per season

summary_percentiles_MaySep$percentile <- as.factor(summary_percentiles_MaySep$percentile)

hist(summary_percentiles_MaySep$risk_sum)

library(ggcorrplot)
model.matrix(~0+., data=summary_percentiles_MaySep) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod1_hump_MaySep <- glm(risk_sum ~ pre_post_reg + percentile,
                        family=gaussian, data=summary_percentiles_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_hump_MaySep)
hist(mod1_hump_MaySep$residuals)
plot(mod1_hump_MaySep)

library(multcomp)
summary(glht(mod1_hump_MaySep, mcp(percentile='Tukey')))


## OVERLAP MAY-SEP


#plot count of overlapping grids -- May-Sep
ts_overlapping_grids_count_all <- ggplot(summary_overlap_MaySep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = percentile, color=percentile), size=1.8) +
  geom_point(aes(y = n_grids, group = percentile, color=percentile), size=3.5) +
  ylab("Overlap") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    #legend.position = c(.1, .7),
    legend.position = 'none',
    axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=40),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_overlapping_grids_count_all



png(paste0(path_figures, "/count_overlap_fishery_and_good_HW_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_overlapping_grids_count_all,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




summary_overlap_MaySep$percentile <- as.factor(summary_overlap_MaySep$percentile)

hist(summary_overlap_MaySep$n_grids )

library(ggcorrplot)
model.matrix(~0+., data=summary_overlap_MaySep) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod1_hump_overlap_MaySep <- glm(n_grids  ~ pre_post_reg + percentile,
                                family=gaussian, data=summary_overlap_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_hump_overlap_MaySep)
hist(mod1_hump_overlap_MaySep$residuals)
plot(mod1_hump_overlap_MaySep)


library(multcomp)
summary(glht(mod1_hump_overlap_MaySep, mcp(percentile='Tukey')))


##########################################################################

## ts plot of 75th percentile risk and overlap 
## Jul-Sep and May-Sep on same plot

summary_75th_HW_habitat_fishing_JulSep 
summary_75th_HW_habitat_fishing_MaySep


ts_fishing_in_75th_hw_habitat_JulSep_MaySep <- ggplot(data=summary_75th_HW_habitat_fishing_JulSep, aes(x=season, y = risk_sum, group = 1, color='Jul-Sep')) +
  geom_line(size=2) +
  geom_point(size=5) +
  
  geom_line(data=summary_75th_HW_habitat_fishing_MaySep, aes(x=season, y = risk_sum, group = 1, color='May-Sep'), size=2) +
  geom_point(data=summary_75th_HW_habitat_fishing_MaySep, aes(x=season, y = risk_sum, group = 1, color='May-Sep'), size=5) +
  
  #ylab("Summed humpback whale risk") +
  ylab("Risk") +
  xlab("Season") +
  xlab("") +
  
  scale_color_manual(name="", values = c("#00bab5","#73377e")) +
  
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +

  theme_classic() +
  theme(legend.title = element_blank(),
        title = element_text(size = 15),
        legend.text = element_text(size = 40),
        legend.position = "none",
        #legend.position = c(.89, .92),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_fishing_in_75th_hw_habitat_JulSep_MaySep


png(paste0(path_figures, "/risk_75th_HW_habitat_JulSep_MaySep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_fishing_in_75th_hw_habitat_JulSep_MaySep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


## overlap
test_summary_75_JulSep 
test_summary_75_MaySep


ts_overlap_75th_hw_habitat_JulSep_MaySep <- ggplot(data=test_summary_75_JulSep, aes(x=season, y = n_grids, group = 1, color='Jul-Sep')) +
  geom_line(size=2) +
  geom_point(size=5) +
  
  geom_line(data=test_summary_75_MaySep, aes(x=season, y = n_grids, group = 1, color='May-Sep'), size=2) +
  geom_point(data=test_summary_75_MaySep, aes(x=season, y = n_grids, group = 1, color='May-Sep'), size=5) +
  
  #ylab("Number of overlapping grids") +
  ylab("Overlap") +
  xlab("Season") +
  
  scale_color_manual(name="", values = c("#00bab5","#73377e")) +
  
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  
  theme_classic() +
  theme(legend.title = element_blank(),
        title = element_text(size = 15),
        legend.text = element_text(size = 22),
        
        #legend.position = c(.9, .85),
        legend.position = "none",
        
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_overlap_75th_hw_habitat_JulSep_MaySep

png(paste0(path_figures, "/overlap_75th_HW_habitat_JulSep_MaySep.png"), width = 17, height = 10, units = "in", res = 360)
ggarrange(ts_overlap_75th_hw_habitat_JulSep_MaySep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

##############################################
## % change from pre-reg average to post-reg
summary_75th_HW_habitat_fishing_JulSep 
(27.151585+16.222773+15.964559+18.853179+20.220890)/5 ##19.6826
(0.081046-19.6826)/19.6826*100 ##-99.58824
test_summary_75_JulSep
(65+52+71+71+88)/5 ##69.4
(5-69.4)/69.4*100 ##-92.79539

summary_75th_HW_habitat_fishing_MaySep
(29.958425+25.054179+23.829820+28.636783+28.783313)/5 ##27.2525
(9.119614-27.2525)/27.2525*100 ##-66.5366
test_summary_75_MaySep
(124+119+106+136+167)/5 ##130.4
(87-130.4)/130.4*100 ##-33.28221


#percentiles
percent_change_risk_JulSep <- summary_percentiles_JulSep %>% 
  group_by(percentile, pre_post_reg) %>% 
  summarise(mean_risk_sum = mean(risk_sum))

percent_change_overlap_JulSep <- summary_overlap_JulSep %>% 
  group_by(percentile, pre_post_reg) %>% 
  summarise(mean_n_grids = mean(n_grids))


percent_change_risk_MaySep <- summary_percentiles_MaySep %>% 
  group_by(percentile, pre_post_reg) %>% 
  summarise(mean_risk_sum = mean(risk_sum))

percent_change_overlap_MaySep <- summary_overlap_MaySep %>% 
  group_by(percentile, pre_post_reg) %>% 
  summarise(mean_n_grids = mean(n_grids))


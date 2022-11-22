#' blue whale centric' look at risk -- changes in risk within the most likely blue whale habitat.
#' separate comparisons for Jul-Sep pre-regs vs 2019, and and May-Sep pre-regs vs 2020

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

library(ggridges)

#-----------------------------------------------------------------------------------
# set some paths

# Leena:
path.grid.5km <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

#whale data
# Leena:
#bw 2009-Jul 2019
path.blue <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"

# where to put outputs
# Leena:
path_figures <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/lrie0/Documents/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub

#-----------------------------------------------------------------------------------
# load the data

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


#bw output 2009-July 2019
x.blue <- readRDS(path.blue) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue)

#bw output Aug 2019-Sep 2021
x.blue_2019_2021 <- readRDS(path.blue_2019_2021) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue_2019_2021) #why does data end June 2021??

#join the 2 bw dfs
x.blue.all <- rbind(x.blue, x.blue_2019_2021) 


# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)

#-----------------------------------------------------------------------------------
# First determine cut-off value for defining most likely bw habitat
# based on the distribution of modelled bw values in study area
# this will be done separately for Jul-Sep pre-regs vs 2019, and and May-Sep pre-regs vs 2020

x.blue.all_crab_season <- x.blue.all %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

x.blue_2014_2020_crab_season_May_Sep <-  x.blue.all_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

#'study area' created in QGIS, to encompass all fished grids plus 'buffer' (grids that could be fished)
#read in 'study area' (grid)
study_area <- read_sf(here::here('wdfw','data', 'study_area.shp'))
glimpse(study_area)
#plot(study_area)

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 

study_area_df <- as.data.frame(study_area_grids_id) %>% 
  rename(GRID5KM_ID = study_area_grids_id)

#the study area grid needs to have all season-month combos for May-Sep
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
study_area_df_with_all_season_month_combos <- crossing(study_area_df, season_month_combos) %>%
  #and add to that the column to denote study area
  mutate(study_area = 'Y')

#join whale data to study area grid
study_area_bw <- full_join(study_area_df_with_all_season_month_combos, x.blue_2014_2020_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(study_area == 'Y')


#------------------------------------------------------------------------------------------

#----------------------------------
##  Jul-Sep pre-regulations vs 2019
#----------------------------------

# if want to visualise modeled bw prob. of occurrences using ggridges

study_area_bw_JulSep <- study_area_bw %>% 
  filter(month %in% c('07','08', '09'))

#density plot for pooled data across all 2014-2020 seasons
#NAs here are because bw model is restricted by ROMS< and the study area has more grids than the bw model output
bw_prob_occur_density_JulSep <- ggplot(study_area_bw_JulSep, aes(x = Blue_occurrence_mean, height = ..density..)) +
  geom_density(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("") +
  #coord_cartesian(clip = "off") +
  xlab("Blue whale probability of occurrence") +
  theme_ridges(grid = TRUE, 
               center_axis_labels = TRUE,
               font_size = 50
               )
bw_prob_occur_density_JulSep

#SAVE FIGURE -- Supplementary Figure S1.2
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/density_plot_BW_prob_occur_in_study_area_JulSep.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(bw_prob_occur_density_JulSep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


#find the mean +/- SD value from the distribution (in study area) for Jul-Sep (2014-2020)
summary_study_area_bw_JulSep <- study_area_bw_JulSep %>% 
  #data already limited to seasons of interest, and Jul-Sep  
  #get mean etc. values across full dataset
  summarise(Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE),
            Median_Blue_occurrence = median(Blue_occurrence_mean, na.rm=TRUE),
            sd_Blue_occurrence = sd(Blue_occurrence_mean, na.rm=TRUE))
# mean = 0.626
# sd = 0.0907
# mean+sd = 0.717  
# mean-sd = 0.535

#-------------

study_area_bw_v2 <-  study_area_bw %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID")

# calculate MEAN whale values for all grid cells in different seasons - across Jul-Sep period 
x.blue.mean_JulSep <- study_area_bw_v2 %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_JulSep)


#if the mean bw occurrence in a grid cell is more than the selected threshold value,
#label grid as good bw habitat in that season
JulSep_good_bw_hab <- x.blue.mean_JulSep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0535 = ifelse(Mean_Blue_occurrence > 0.535, 'Y', 'N'),
         good_bw_hab_0626 = ifelse(Mean_Blue_occurrence > 0.626, 'Y', 'N'),
         good_bw_hab_0717 = ifelse(Mean_Blue_occurrence > 0.717, 'Y', 'N')
  ) #%>%
  #inner_join(grid.5km.lno) #join to have geometry column
glimpse(JulSep_good_bw_hab)





#---------------------------

#Then look at what trap density was like in the good bw habitat 
#bring in fishing data 
#path.fish_WA <- "C:/Users/lrie0/Documents/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/lrie0/Documents/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

# average trap density for each grid cell for Jul-Sep period
x.fish_WA_JulSep <- x.fish_WA %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
    ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) 
glimpse(x.fish_WA_JulSep)


# join fishing data and bw data
JulSep_good_bw_hab_fishing <- JulSep_good_bw_hab %>% 
  left_join(x.fish_WA_JulSep, by=c('season', 'GRID5KM_ID')) %>% 
  left_join(st_drop_geometry(grid.key), by = "GRID5KM_ID") 
glimpse(JulSep_good_bw_hab_fishing)



# # calculate risk
# JulSep_good_bw_hab_fishing_risk <- JulSep_good_bw_hab_fishing %>% 
#   mutate(
#     blue_risk = Mean_Blue_occurrence * mean_trapdens
#   )%>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(blue_risk = 
#            ifelse(is.na(mean_trapdens), 0, blue_risk)
#   )

## normalize whale and fishing data before calculating risk
library("scales")
JulSep_good_bw_hab_fishing_risk <- JulSep_good_bw_hab_fishing %>%
  #while the rescale() function has worked with all other data, somehow it does not work correctly here
  #do the scaling 'manually'
  mutate(Mean_Blue_occurrence_norm = (Mean_Blue_occurrence - 0.3573179) / (0.8055918 - 0.3573179),
         mean_trapdens_norm = (mean_trapdens - 0.01785714) / (58.429451 - 0.01785714)) %>% 
  #in this case no need to alter normalized 0s
  #calculate risk  metric
  mutate(
    blue_risk = Mean_Blue_occurrence_norm * mean_trapdens_norm
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(blue_risk = 
           ifelse(is.na(mean_trapdens), 0, blue_risk)
  ) 



# summarise risk in each season based on threshold value used to define good bw habitat
# Jul-Sep comparisons are only for pre-regulations seasons vs 2019 (therefore we remove 2019-2020 season)
summary_good_bw_habitat_fishing_JulSep_0535 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0535 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.535')
glimpse(summary_good_bw_habitat_fishing_JulSep_0535)  

summary_good_bw_habitat_fishing_JulSep_0626 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0626 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.626')
glimpse(summary_good_bw_habitat_fishing_JulSep_0626)  

summary_good_bw_habitat_fishing_JulSep_0717 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0717 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.717')
glimpse(summary_good_bw_habitat_fishing_JulSep_0717)  



summary_probabilites_JulSep <- rbind(
  summary_good_bw_habitat_fishing_JulSep_0535,
  summary_good_bw_habitat_fishing_JulSep_0626,
  summary_good_bw_habitat_fishing_JulSep_0717
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg'))




#######
#overlap between good bw habitat and fishery in each season 
#(based on different threshold value used to define good bw habitat)

#Jul-Sep
test_summary_JulSep_0626 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0626 == 'Y') %>% #select grids that were defined as good bw habitat
  filter(!is.na(mean_trapdens)) %>% #select grids with fishing effort = fishery footprint
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.626')
#when use 0.626 threshold value
# season    n_grids
#  2013-2014      12
#  2014-2015      21
#  2015-2016      38
#  2016-2017      38
#  2017-2018      54
#  2018-2019      56

test_summary_JulSep_0535 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0535 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.535')
#when use 0.535 threshold value
# season    n_grids
#  2013-2014      48
#  2014-2015      33
#  2015-2016      61
#  2016-2017      60
#  2017-2018      76
#  2018-2019      67

test_summary_JulSep_0717 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0717 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  #no overlap in first 4 seasons, add a row so we can have these as 0 and not NA
  add_row(season = c("2013-2014", "2014-2015", "2015-2016", "2016-2017"), n_grids = 0) %>% 
  mutate(prob_of_occur = '0.717')
#when use 0.717 threshold value
# season    n_grids
#  2013-2014      0
#  2014-2015      0
#  2015-2016      0
#  2016-2017      0
#  2017-2018      10
#  2018-2019      21


summary_overlap_JulSep <- rbind(
  test_summary_JulSep_0535,
  test_summary_JulSep_0626,
  test_summary_JulSep_0717
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg'))




#------------------------------------------------------------------------------------------

#----------------------------------
##  May-Sep pre-regulations vs 2020
#----------------------------------

# if want to visualise modeled bw prob. of occurrences using ggridges

#density plot for pooled data across all 2014-2020 seasons
#NAs here are because bw model is restricted by ROMS, and the study area has more grids than the bw model output
bw_prob_occur_density_MaySep <- ggplot(study_area_bw, aes(x = Blue_occurrence_mean, height = ..density..)) +
  geom_density(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("") +
  #coord_cartesian(clip = "off") +
  xlab("Blue whale probability of occurrence") +
  theme_ridges(grid = TRUE, 
               center_axis_labels = TRUE,
               font_size = 50)
bw_prob_occur_density_MaySep

## SAVE FIGURE -- Supplementary Figure S1.2
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/density_plot_BW_prob_occur_in_study_area_MaySep.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(bw_prob_occur_density_MaySep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


#find the mean +/- SD value from the distribution (in study area) for May-Sep (2014-2020)
summary_study_area_bw_MaySep <- study_area_bw %>% 
  #data already limited to seasons of interest, and May-Sep  
  #get mean etc. values across full dataset
  summarise(Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE),
            sd_Blue_occurrence = sd(Blue_occurrence_mean, na.rm=TRUE))

# mean = 0.469
# sd = 0.229
# mean+sd = 0.698 #if use this as cutoff there is no overlap between good bw habitat and fishery
# mean-sd = 0.24


study_area_bw_v2 <-  study_area_bw %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID")

# calculate MEAN whale values for different grids in different seasons - across May-Sep period 
x.blue.mean_MaySep <- study_area_bw_v2 %>% 
  #data already restricted to be May-Sep
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_MaySep)


#if the mean bw occurrence in a grid cell is more than the selected threshold value,
#label grid as good bw habitat in that season
MaySep_good_bw_hab <- x.blue.mean_MaySep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0469 = ifelse(Mean_Blue_occurrence > 0.469, 'Y', 'N'),
         good_bw_hab_024 = ifelse(Mean_Blue_occurrence > 0.24, 'Y', 'N'),
         good_bw_hab_050 = ifelse(Mean_Blue_occurrence > 0.5, 'Y', 'N')
  ) #%>%
  #inner_join(grid.5km.lno) #join to have geometry column
glimpse(MaySep_good_bw_hab)



#---------------------------


#Then look at what trap density was like in the good bw habitat 

# fishing data has already been brought in

# average trap density (and count?) for each grid cell for May-Sep period
x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") %>% #filter data to be May-Sep only
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) 
glimpse(x.fish_WA_MaySep)


# join fishing data and bw data
MaySep_good_bw_hab_fishing <- MaySep_good_bw_hab %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID')) %>% 
  left_join(grid.key, by = "GRID5KM_ID") 
glimpse(MaySep_good_bw_hab_fishing)


# # calculate risk
# MaySep_good_bw_hab_fishing_risk <- MaySep_good_bw_hab_fishing %>% 
#   mutate(
#     blue_risk = Mean_Blue_occurrence * mean_trapdens
#   )%>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(blue_risk = 
#            ifelse(is.na(mean_trapdens), 0, blue_risk)
#   )

## normalize whale and fishing data before calculating risk
library("scales")
##for some reason rescale() funtion does not work here, do it manually
MaySep_good_bw_hab_fishing_risk <- MaySep_good_bw_hab_fishing %>% 
  mutate(Mean_Blue_occurrence_norm = (Mean_Blue_occurrence - 0.2841589) / (0.6509358 - 0.2841589),
         mean_trapdens_norm = (mean_trapdens - 0.01090909) / (54.546545 - 0.01090909)) %>% 
  #in this case no need to alter normalized 0s
  #calculate risk  metric
  mutate(
    blue_risk = Mean_Blue_occurrence_norm * mean_trapdens_norm
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(blue_risk = 
           ifelse(is.na(mean_trapdens), 0, blue_risk)
  ) 


# summarise risk in each season based on threshold value used to define good bw habitat
# May-Sep comparisons are only for pre-regulations seasons vs 2020 (therefore we remove 2018-2019 season)
summary_good_bw_habitat_fishing_MaySep_0469 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0469 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.469')
glimpse(summary_good_bw_habitat_fishing_MaySep_0469)

summary_good_bw_habitat_fishing_MaySep_024 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_024 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.24')
glimpse(summary_good_bw_habitat_fishing_MaySep_024)

summary_good_bw_habitat_fishing_MaySep_050 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_050 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.50')
glimpse(summary_good_bw_habitat_fishing_MaySep_050)


summary_probabilites_MaySep <- rbind(
  summary_good_bw_habitat_fishing_MaySep_024,
  summary_good_bw_habitat_fishing_MaySep_0469,
  summary_good_bw_habitat_fishing_MaySep_050
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg'))



#######
#overlap between good bw habitat and fishery in each season 
#(based on different threshold value used to define good bw habitat)

test_summary_MaySep_0469 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0469 == 'Y') %>% #select grids that were defined as good bw habitat
  filter(!is.na(mean_trapdens)) %>% #select grids with fishing effort = fishery footprint
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.469')
#when use 0.469 threshold value
# season    n_grids
#  2013-2014      10
#  2014-2015       9
#  2015-2016      55
#  2016-2017      62
#  2017-2018      94

#  2019-2020     101


test_summary_MaySep_024 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_024 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.24')
#when use 0.24 threshold value
#season       n_grids
#2013-2014      98
#2014-2015      87
#2015-2016      98
#2016-2017      122
#2017-2018      147

#2019-2020      104


test_summary_MaySep_050 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_050 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.50')
#when use 0.5 threshold value
#season       n_grids
#2013-2014      8
#2014-2015      2
#2015-2016      11
#2016-2017      13
#2017-2018      69

#2019-2020      75


summary_overlap_MaySep <- rbind(
  test_summary_MaySep_0469,
  test_summary_MaySep_024,
  test_summary_MaySep_050
) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg'))





#------------------------------------------------------------------------------------------------------------

#-----------------------
###    Plotting and GLM



### RISK ###
#summary_probabilites_JulSep
#summary_probabilites_MaySep
my_colors <- RColorBrewer::brewer.pal(4, "PRGn")[2:4]
ts_risk_in_good_bw_habitat_JulSep <- ggplot(summary_probabilites_JulSep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=2) +
  geom_point(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=4) +
  scale_color_manual(values = my_colors)+
  ylab("Risk") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  guides(color=guide_legend(title="Probability of occurrence")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=50),
    #title = element_text(size = 15),
    legend.text = element_text(size=50),
    #legend.position = c(.85, .15),
    legend.position = 'bottom',
    #legend.position = 'none',
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0, color='black'),
    axis.text.y = element_text(size = 50, color='black'),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=50),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_risk_in_good_bw_habitat_JulSep

# SAVE FIGURE -- Supplementary Figure S1.3
# png(paste0(path_figures, "/ts_risk_sum_in_different_BW_habitat_JulSep_NORM_LEGEND_UPDATED.png"), width = 17, height = 10, units = "in", res = 500)
# ggarrange(ts_risk_in_good_bw_habitat_JulSep,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


##GLM (RISK - Jul-Sep pre-reg vs 2019)
# note that this is different to the fishery perspective, as that had one data point per month
# here good habitat is defined across Jul-Sep and May-Sep, and there is only 1 data point per season

hist(summary_probabilites_JulSep$risk_sum)
summary_probabilites_JulSep$prob_of_occur <- as.factor(summary_probabilites_JulSep$prob_of_occur)



mod1_blue_JulSep <- glm(risk_sum ~ pre_post_reg + prob_of_occur,
                        family=gaussian, data=summary_probabilites_JulSep, na.action = na.omit) 
summary(mod1_blue_JulSep)
#hist(mod1_blue_JulSep$residuals)
#plot(mod1_blue_JulSep)

#library(multcomp)
#summary(glht(mod1_blue_JulSep, mcp(prob_of_occur='Tukey')))




ts_risk_in_good_bw_habitat_MaySep <- ggplot(summary_probabilites_MaySep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=2) +
  geom_point(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=4) +
  scale_color_manual(values = my_colors)+
  ylab("Risk") +
  xlab("Season") +
  guides(color=guide_legend(title="Prob. of occur.")) +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  guides(color=guide_legend(title="Probability of occurrence")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=50),
    #title = element_text(size = 15),
    legend.text = element_text(size=50),
    legend.position = 'bottom',
    #legend.position = 'none',
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0, color='black'),
    axis.text.y = element_text(size = 50, color='black'),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=50),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_risk_in_good_bw_habitat_MaySep

# SAVE FIGURE -- Supplementary Figure S1.3
# png(paste0(path_figures, "/ts_risk_sum_in_different_BW_habitat_MaySep_NORM_LEGEND_UPDATED.png"), width = 17, height = 10, units = "in", res = 500)
# ggarrange(ts_risk_in_good_bw_habitat_MaySep,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


##GLM (RISK - May-Sep pre-reg vs 2020)
# note that this is different to the fishery perspective, as that had one data point per month
# here good habitat is defined across Jul-Sep and May-Sep, and there is only 1 data point per season

hist(summary_probabilites_MaySep$risk_sum)
summary_probabilites_MaySep$prob_of_occur <- as.factor(summary_probabilites_MaySep$prob_of_occur)

mod2_blue_MaySep <- glm(risk_sum ~ pre_post_reg + prob_of_occur,
                        family=gaussian, data=summary_probabilites_MaySep, na.action = na.omit) 
summary(mod2_blue_MaySep)
hist(mod2_blue_MaySep$residuals)
plot(mod2_blue_MaySep)

#library(multcomp)
#summary(glht(mod2_blue_MaySep, mcp(prob_of_occur='Tukey')))



### OVERLAP ###
#summary_overlap_JulSep
#summary_overlap_MaySep

#plot count of overlapping grids -- Jul-Sep
ts_overlapping_grids_JulSep <- ggplot(summary_overlap_JulSep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=2) +
  geom_point(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=4) +
  scale_color_manual(values = my_colors)+
  ylab("Overlap") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=50),
    #title = element_text(size = 15),
    legend.text = element_text(size=50),
    legend.position = 'bottom',
    #legend.position = 'none',
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0, color='black'),
    axis.text.y = element_text(size = 50, color='black'),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=50),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_overlapping_grids_JulSep

# SAVE FIGURE -- Supplementary Figure S1.3
# png(paste0(path_figures, "/ts_overlap_good_BW_habitat_JulSep_LEGEND_UPDATED.png"), width = 17, height = 10, units = "in", res = 500)
# ggarrange(ts_overlapping_grids_JulSep,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



## GLM (OVERLAP - Jul-Sep pre-reg vs 2019)
summary_overlap_JulSep$prob_of_occur <- as.factor(summary_overlap_JulSep$prob_of_occur)

hist(summary_overlap_JulSep$n_grids )

mod1_blue_overlap_JulSep <- glm(n_grids  ~ pre_post_reg + prob_of_occur,
                                family=gaussian, data=summary_overlap_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_blue_overlap_JulSep)
#hist(mod1_blue_overlap_JulSep$residuals)
#plot(mod1_blue_overlap_JulSep)

#library(multcomp)
#summary(glht(mod1_blue_overlap_JulSep, mcp(prob_of_occur='Tukey')))



#plot count of overlapping grids -- May-Sep
ts_overlapping_grids <- ggplot(summary_overlap_MaySep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=2) +
  geom_point(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=4) +
  scale_color_manual(values = my_colors)+
  ylab("Overlap") +
  xlab("Season") +
  scale_x_discrete(labels=c("2013-2014" = "2014",
                            "2014-2015" = "2015",
                            "2015-2016" = "2016",
                            "2016-2017" = "2017",
                            "2017-2018" = "2018",
                            "2018-2019" = "2019",
                            "2019-2020" = "2020")) +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=50),
    #title = element_text(size = 15),
    legend.text = element_text(size=50),
    legend.position = 'none',
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0, color='black'),
    axis.text.y = element_text(size = 50, color='black'),
    axis.title = element_text(size = 50),
    strip.text = element_text(size=50),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_overlapping_grids

# SAVE FIGURE -- Supplementary Figure S1.3
# png(paste0(path_figures, "/ts_overlap_good_BW_habitat_MaySep_UPDATED.png"), width = 17, height = 10, units = "in", res = 500)
# ggarrange(ts_overlapping_grids,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


## GLM (OVERLAP - May-Sep pre-reg vs 2020)
summary_overlap_MaySep$prob_of_occur <- as.factor(summary_overlap_MaySep$prob_of_occur)

hist(summary_overlap_MaySep$n_grids )

mod1_blue_overlap_MaySep <- glm(n_grids  ~ pre_post_reg + prob_of_occur,
                                family=gaussian, data=summary_overlap_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_blue_overlap_MaySep)
hist(mod1_blue_overlap_MaySep$residuals)
plot(mod1_blue_overlap_MaySep)

#library(multcomp)
#summary(glht(mod1_blue_overlap_MaySep, mcp(prob_of_occur='Tukey')))



#------------------------------------
## ts plot of risk and overlap  - using the mean probability of occurrence cutoff 
## Jul-Sep and May-Sep on same plot

### RISK ### 
#summary_good_bw_habitat_fishing_JulSep_0626
#summary_good_bw_habitat_fishing_MaySep_0469

ts_risk_in_mean_bw_habitat_JulSep_MaySep <- ggplot(data=summary_good_bw_habitat_fishing_JulSep_0626, aes(x=season, y = risk_sum, group = 1, color='Jul-Sep')) +
  geom_line(size=2.5) +
  geom_point(size=7) +
  geom_line(data=summary_good_bw_habitat_fishing_MaySep_0469, aes(x=season, y = risk_sum, group = 1, color='May-Sep'), size=2.5) +
  geom_point(data=summary_good_bw_habitat_fishing_MaySep_0469, aes(x=season, y = risk_sum, group = 1, color='May-Sep'), size=7) +
  #ylab("Summed blue whale risk") +
  ylab("Risk") +
  #xlab("Season") +
  xlab("") +
  #scale_color_manual(name="", values = c("#00bab5","#73377e")) +
  scale_color_manual(name="", values = c("black","gray")) +
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
        #legend.position = "none",
        legend.position = 'none',
        axis.line = element_line(colour = 'black', size = 2),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks=element_line(size=2, colour = 'black'),
        axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0,color="black"),
        axis.text.y = element_text(size = 50,color="black"),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=50),
        strip.background = element_blank(),
        strip.placement = "left",
        plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_risk_in_mean_bw_habitat_JulSep_MaySep

# SAVE FIGURE -- Figure 5 -- use width = 20, height = 5
# png(paste0(path_figures, "/ts_risk_mean_good_BW_habitat_JulSep_MaySep_NORM_UPDATED.png"), width = 20, height = 5, units = "in", res = 500)
# ggarrange(ts_risk_in_mean_bw_habitat_JulSep_MaySep,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


### OVERLAP ### 
#test_summary_JulSep_0626
#test_summary_MaySep_0469

ts_overlap_in_mean_bw_habitat_JulSep_MaySep <- ggplot(data=test_summary_JulSep_0626, aes(x=season, y = n_grids, group = 1, color='Jul-Sep')) +
  geom_line(size=2.5) +
  geom_point(size=7) +
  geom_line(data=test_summary_MaySep_0469, aes(x=season, y = n_grids, group = 1, color='May-Sep'), size=2.5) +
  geom_point(data=test_summary_MaySep_0469, aes(x=season, y = n_grids, group = 1, color='May-Sep'), size=7) +
  #ylab("Number of overlapping grids") +
  ylab("Overlap") +
  xlab("") +
  #scale_color_manual(name="", values = c("#00bab5","#73377e")) +
  scale_color_manual(name="", values = c("black","gray")) +
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
        #legend.position = c(.85, .8),
        legend.position = "none",
        axis.line = element_line(colour = 'black', size = 2),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks=element_line(size=2, colour = 'black'),
        axis.text.x = element_text(hjust = 0.5,size = 50, angle = 0, color='black'),
        axis.text.y = element_text(size = 50, color='black'),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=50),
        strip.background = element_blank(),
        strip.placement = "left",
        plot.margin = unit(c(0,0,0,30), "pt")
  )
ts_overlap_in_mean_bw_habitat_JulSep_MaySep

# SAVE FIGURE -- Figure 5 -- width = 20, height = 5
# png(paste0(path_figures, "/ts_overlap_mean_good_BW_habitat_JulSep_MaySep_UPDATED.png"), width = 20, height = 5, units = "in", res = 500)
# ggarrange(ts_overlap_in_mean_bw_habitat_JulSep_MaySep,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())




#------------------------------------------------------------------------------------------

## % change from pre-reg average to post-reg

##pre-reg vs 2019 (Jul-Sep)##
#RISK
summary_good_bw_habitat_fishing_JulSep_0626
(58.56626+160.74911+194.61349+189.30851+312.90849)/5 ##183.2292
(241.15051-183.2292)/183.2292*100 ##31.6114
##NORMALIZED
(0.9739361+2.7787593+3.3551498+3.2458614+5.7945484)/5 ##3.229651
(4.4973293-3.229651)/3.229651*100 ##39.25125
#OVERLAP
test_summary_JulSep_0626
(12+21+38+38+54)/5 ##32.6
(56-32.6)/32.6*100 ##71.77914

##pre-reg vs 2020 (May-Sep)##
#RISK
summary_good_bw_habitat_fishing_MaySep_0469
(12.95299+42.48131+184.00885+200.70018+326.15871)/5 ##153.2604
(201.57120-153.2604)/153.2604*100 ##31.52204
##NORMALIZED
(0.3011995+0.8442963+3.8460518+4.1329046+7.3001116)/5 ##3.284913
(4.7419790-3.284913)/3.284913*100 ##44.3563
#OVERLAP
test_summary_MaySep_0469
(10+9+55+62+94)/5 ##46
(101-46)/46*100 ##119.5652


#sensitivity testing
percent_change_risk_JulSep <- summary_probabilites_JulSep %>% 
  group_by(prob_of_occur, pre_post_reg) %>% 
  summarise(mean_risk_sum = mean(risk_sum))

percent_change_overlap_JulSep <- summary_overlap_JulSep %>% 
  group_by(prob_of_occur, pre_post_reg) %>% 
  summarise(mean_n_grids = mean(n_grids))


percent_change_risk_MaySep <- summary_probabilites_MaySep %>% 
  group_by(prob_of_occur, pre_post_reg) %>% 
  summarise(mean_risk_sum = mean(risk_sum))

percent_change_overlap_MaySep <- summary_overlap_MaySep %>% 
  group_by(prob_of_occur, pre_post_reg) %>% 
  summarise(mean_n_grids = mean(n_grids))




#---------------------------------------------------------------------------------

# Mapping of example maps to go in supplementary (Supplementary Figure S1.4)

#re-do this step so that good bw habitat is not restricted to study area in the map:
# calculate MEAN whale values for different grid in different seasons - for May-Sep period (2014-2020)
x.blue.mean_MaySep <- x.blue_2014_2020_crab_season_May_Sep %>% 
  left_join(grid.5km.lno %>% st_drop_geometry()) %>% 
  #data already restricted to be May-Sep
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_MaySep)


MaySep_good_bw_hab <- x.blue.mean_MaySep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0469 = ifelse(Mean_Blue_occurrence > 0.469, 'Y', 'N'),
         good_bw_hab_024 = ifelse(Mean_Blue_occurrence > 0.24, 'Y', 'N'),
         good_bw_hab_050 = ifelse(Mean_Blue_occurrence > 0.5, 'Y', 'N')
  ) %>%
  inner_join(grid.5km.lno) #join to have geometry column
glimpse(MaySep_good_bw_hab)


## MAPPING ##
# map example of most likely bw habitat (of a chosen season) with NON-confidential summer fishery footprint (pooled 2014-2020)
dissolved_2014_2020_MaySep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds'))

# optional: add to map the outline of the 'study area'
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
bbox = c(-126.5,45.5,-122,49) 


# map 
bw_subset_MaySep <- MaySep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2018-2019") %>% 
  filter(!is.na(good_bw_hab_0469)) %>% 
  filter(good_bw_hab_0469 == 'Y')

map_blue_MaySep_good_hab <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=good_bw_hab_0469,
              col=good_bw_hab_0469
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2014_2020_MaySep_non_conf, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black',linetype = "dotted",size=1, fill = NA) +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=15,color="black"),
        axis.text=element_text(family="sans",size=15,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        #strip.text = element_text(size=14),
        title=element_text(size=20),
        legend.position = 'none'
  )
map_blue_MaySep_good_hab


# SAVE FIGURE -- Supplementary Figure S1.4
# png(paste0(path_figures, "/good_bw_habitat_0469_occur_MaySep_2018_2019_with_pooled_NONCONF_summer_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 400)
# ggarrange(map_blue_MaySep_good_hab,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())

#----------------

# Mapping Jul-Sep good habitat

#re-do this step so that good bw habitat is not restricted to study area in the map:
# calculate MEAN whale values for different grid in different seasons - for Jul-Sep period (2014-2020)
x.blue.mean_JulSep <- x.blue_2014_2020_crab_season_May_Sep %>% 
  left_join(grid.5km.lno %>% st_drop_geometry()) %>% 
  #restrict to be Jul-Sep
  filter(month %in% c('07','08', '09')) %>% 
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_JulSep)


JulSep_good_bw_hab <- x.blue.mean_JulSep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0626 = ifelse(Mean_Blue_occurrence > 0.626, 'Y', 'N')) %>%
  inner_join(grid.5km.lno) #join to have geometry column
glimpse(JulSep_good_bw_hab)


## MAPPING ##
# map example of most likely bw habitat (of a chosen season) with NON-confidential summer fishery footprint (pooled 2014-2020)
dissolved_2014_2020_JulSep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_JulSep_WA_fishery_footprint_NONCONF.rds'))

# optional: add to map the outline of the 'study area'
dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only.shp')) %>% 
  st_transform(st_crs(dissolved_2014_2020_JulSep_non_conf)) #make it have same projection 

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-126.5,45.5,-122,49) 


# map 
bw_subset_JulSep <- JulSep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(good_bw_hab_0626)) %>% 
  filter(good_bw_hab_0626 == 'Y')

map_blue_JulSep_good_hab <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_JulSep), 
          aes(fill=good_bw_hab_0626,
              col=good_bw_hab_0626
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2014_2020_JulSep_non_conf, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black',linetype = "dotted",size=1, fill = NA) +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=15,color="black"),
        axis.text=element_text(family="sans",size=15,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        #strip.text = element_text(size=14),
        title=element_text(size=20),
        legend.position = 'none'
  )
map_blue_JulSep_good_hab


# SAVE FIGURE -- Supplementary Figure 
# png(paste0(path_figures, "/good_bw_habitat_0626_occur_JulSep_2013_2014_with_pooled_NONCONF_JulSep_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 400)
# ggarrange(map_blue_JulSep_good_hab,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())
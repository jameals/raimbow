#Fishery centric look at risk

########## RISK IS SUMMED #################
#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(ggbeeswarm)

#-----------------------------------------------------------------------------------
#bring in some grids
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#risk to whales in STUDY AREA during May-Sep


#whale data

#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)


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


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

x.whale_crab_season_May_Sep <-  x.whale_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


#fishing effort

#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA <- x.fish_WA %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y) %>% #remove AREA as grouping factor here
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))





 #if no regs in place
 #path.fish_WA_no_regs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step_NO_REGS.rds"
 path.fish_WA_no_regs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step_NO_REGS.rds"

 x.fish_WA_no_regs <- readRDS(path.fish_WA_no_regs)
 #Grid ID 122919 end up having very high trap densities in few months 
 #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
 #this is because the grid is split across land, and few points happen to fall in a very tiny area
 #remove it
 x.fish_WA_no_regs <- x.fish_WA_no_regs %>% filter(GRID5KM_ID != 122919)
 # get avg traps dens per grid cell for each yr month to allow matching with whale data
 x.fish_WA2_no_regs <- x.fish_WA_no_regs %>%
   group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
   summarise( 
     number_obs = n(), #no. of grid cells in that season_month that had traps in them 
     mean_M2_trapdens = mean(M2_trapdens), 
   )
 
 # make column for year month for fishing data to allow matching with whale data
 x.fish_WA_MaySep_no_regs <- x.fish_WA2_no_regs %>%
   separate(season_month, into = c("season", "month_name"), sep = "_") %>%
   mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
   mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
   #restrict fishing data to May-Sep as was done to whale data
   filter(month %in% c('05', '06', '07', '08', '09'))

#-----------------------------------------------------------------------------------

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
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

#join fishing data to study area grid with whale data
study_area_whale_fishing <- left_join(study_area_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))



#calculate risk  metric
# risk_whales_WA_MaySep <- study_area_whale_fishing %>%
#   mutate(
#     hump_risk = Humpback_dens_mean * mean_M2_trapdens,
#     blue_risk = Blue_occurrence_mean * mean_M2_trapdens
#   ) %>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(hump_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
#          blue_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
#   ) %>%
#   #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
#   mutate(hump_risk = 
#            ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
#          blue_risk = 
#            ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
#   ) %>%
#   mutate(is_May_Sep = 
#            ifelse(month %in% c('05', '06', '07', '08', '09')
#                   ,'Y', 'N'))


## normalize whale and fishing data before calculating risk
library("scales")

risk_whales_WA_MaySep_normalized <- study_area_whale_fishing %>% 
  filter(study_area=='Y') %>%
  mutate(Humpback_dens_mean_norm = rescale(Humpback_dens_mean),
         Blue_occurrence_mean_norm = rescale(Blue_occurrence_mean),
         mean_M2_trapdens_norm = rescale(mean_M2_trapdens)) %>%
  #normalized 0-1: but 0 here is not a true 0 risk
  #--> change normalized 0 to a small non-zero value (using the smallest non-zero of the variable)
  mutate(Humpback_dens_mean_norm = ifelse(Humpback_dens_mean_norm == 0, (0.0001220106*10^-1), Humpback_dens_mean_norm),
         Blue_occurrence_mean_norm = ifelse(Blue_occurrence_mean_norm == 0, (0.001287732*10^-1), Blue_occurrence_mean_norm),
         mean_M2_trapdens_norm = ifelse(mean_M2_trapdens_norm == 0, (3.058840e-05*10^-1), mean_M2_trapdens_norm)) %>%
  #calculate risk  metric
  mutate(
    hump_risk_norm = Humpback_dens_mean_norm * mean_M2_trapdens_norm,
    blue_risk_norm = Blue_occurrence_mean_norm * mean_M2_trapdens_norm
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk_norm = 
           ifelse(is.na(mean_M2_trapdens_norm), 0, hump_risk_norm),
         blue_risk_norm = 
           ifelse(is.na(mean_M2_trapdens_norm), 0, blue_risk_norm)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk_norm = 
           ifelse(is.na(Humpback_dens_mean_norm), NA, hump_risk_norm),
         blue_risk_norm = 
           ifelse(is.na(Blue_occurrence_mean_norm), NA, blue_risk_norm)
  ) %>%
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))



# #if no regs
# #join fishing data to study area grid with whale data
# study_area_whale_fishing_no_regs <- left_join(study_area_whale, x.fish_WA_MaySep_no_regs, by=c("GRID5KM_ID", "season", "month")) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))
# 
# 
# #calculate risk  metric
# risk_whales_WA_MaySep_no_regs <- study_area_whale_fishing_no_regs %>%
#   mutate(
#     hump_risk = Humpback_dens_mean * mean_M2_trapdens,
#     blue_risk = Blue_occurrence_mean * mean_M2_trapdens
#   ) %>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(hump_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
#          blue_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
#   ) %>%
#   #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
#   mutate(hump_risk = 
#            ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
#          blue_risk = 
#            ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
#   ) %>%
#   mutate(is_May_Sep = 
#            ifelse(month %in% c('05', '06', '07', '08', '09')
#                   ,'Y', 'N'))
# 
# 
# 
# # sum_risk_whales_WA_MaySep_no_regs_2018_2019 <- risk_whales_WA_MaySep_no_regs %>%
# #   filter(season == '2018-2019') %>% 
# #   filter(study_area=='Y')%>% 
# #   group_by(season) %>% 
# #   summarise(
# #     Humpback_risk_sum_2018_2019 = sum(hump_risk, na.rm=TRUE),
# #     Blue_risk_sum_2018_2019 = sum(blue_risk, na.rm=TRUE)
# #   )
# 
# subset_2018_2019_NO_REGS <- risk_whales_WA_MaySep_no_regs %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season != '2019-2020') %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   group_by(season, month, pre_post_reg) %>% 
#   summarise(hump_risk = sum(hump_risk, na.rm=TRUE),
#             blue_risk = sum(blue_risk, na.rm=TRUE)) 
# 
# # 
# # sum_risk_whales_WA_MaySep_no_regs_2019_2020 <- risk_whales_WA_MaySep_no_regs %>%
# #   filter(season == '2019-2020') %>% 
# #   filter(study_area=='Y')%>% 
# #   group_by(season) %>% 
# #   summarise(
# #     Humpback_risk_sum_2019_2020 = sum(hump_risk, na.rm=TRUE),
# #     Blue_risk_sum_2019_2020 = sum(blue_risk, na.rm=TRUE)
# #   )
# 
# subset_2019_2020_NO_REGS <- risk_whales_WA_MaySep_no_regs %>% 
#   filter(month %in% c('05', '06', '07', '08', '09')) %>% 
#   filter(season != '2018-2019') %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   group_by(season, month, pre_post_reg) %>% 
#   summarise(hump_risk = sum(hump_risk, na.rm=TRUE),
#             blue_risk = sum(blue_risk, na.rm=TRUE)) 
# 
# 
# 
# ## NORMALIZED
# risk_whales_WA_MaySep_no_regs_normalized <- study_area_whale_fishing_no_regs %>% 
#   filter(study_area=='Y') %>%
#   mutate(Humpback_dens_mean_norm = rescale(Humpback_dens_mean),
#          Blue_occurrence_mean_norm = rescale(Blue_occurrence_mean),
#          mean_M2_trapdens_norm = rescale(mean_M2_trapdens)) %>% 
#   #calculate risk  metric
#   mutate(
#     hump_risk_norm = Humpback_dens_mean_norm * mean_M2_trapdens_norm,
#     blue_risk_norm = Blue_occurrence_mean_norm * mean_M2_trapdens_norm
#   ) %>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(hump_risk_norm = 
#            ifelse(is.na(mean_M2_trapdens_norm), 0, hump_risk_norm),
#          blue_risk_norm = 
#            ifelse(is.na(mean_M2_trapdens_norm), 0, blue_risk_norm)
#   ) %>%
#   #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
#   mutate(hump_risk_norm = 
#            ifelse(is.na(Humpback_dens_mean_norm), NA, hump_risk_norm),
#          blue_risk_norm = 
#            ifelse(is.na(Blue_occurrence_mean_norm), NA, blue_risk_norm)
#   ) %>%
#   mutate(is_May_Sep = 
#            ifelse(month %in% c('05', '06', '07', '08', '09')
#                   ,'Y', 'N'))
# 
# subset_2018_2019_NO_REGS_NORMALIZED <- risk_whales_WA_MaySep_no_regs_normalized %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season != '2019-2020') %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   group_by(season, month, pre_post_reg) %>% 
#   summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
#             blue_risk = sum(blue_risk_norm, na.rm=TRUE)) 
# 
# subset_2019_2020_NO_REGS_NORMALIZED <- risk_whales_WA_MaySep_no_regs_normalized %>% 
#   filter(month %in% c('05', '06', '07', '08', '09')) %>% 
#   filter(season != '2018-2019') %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   group_by(season, month, pre_post_reg) %>% 
#   summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
#             blue_risk = sum(blue_risk_norm, na.rm=TRUE)) 

#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,45,-120,49) 


subset_data <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=hump_risk,
              col=hump_risk
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_test

#-----------------------------------------------------------------------------------
# 
# # ts plot: summed May-Sep risk to whales in study area
# 
# plot_subset <- risk_whales_WA_MaySep %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Humpback_risk_sum = sum(hump_risk, na.rm=TRUE))
# 
# ts_sum_hump_risk_May_Sep_study_area <- ggplot() +
#   geom_point(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
#   geom_line(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1)) +
#   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Humpback_risk_sum_2018_2019, group = 1), size=4, colour = 'red') +
#   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Humpback_risk_sum_2019_2020, group = 1), size=4, colour = 'red') +
#   ylab("summed humpback Whale Risk May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_sum_hump_risk_May_Sep_study_area
# 
# #--------------
# 
# plot_subset <- risk_whales_WA_MaySep %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Blue_risk_sum = sum(blue_risk, na.rm=TRUE))
# 
# ts_sum_blue_risk_May_Sep_study_area <- ggplot() +
#   geom_point(data = plot_subset, aes(x = season, y = Blue_risk_sum,group = 1), size=4) +
#   geom_line(data = plot_subset, aes(x = season, y = Blue_risk_sum,group = 1)) +
#   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Blue_risk_sum_2018_2019, group = 1), size=4, colour = 'red') +
#   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Blue_risk_sum_2019_2020, group = 1), size=4, colour = 'red') +
# 
#   ylab("Summed blue Whale Risk May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_sum_blue_risk_May_Sep_study_area
# 
# 
# 
# #plot blues and humps together and save
# png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_NO_REGS_2018_2020.png"), width = 14, height = 10, units = "in", res = 300)
# png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_sum_hump_risk_May_Sep_study_area,
#           ts_sum_blue_risk_May_Sep_study_area,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())
# 
# #risk in study area on 2-weekly step
# #season     Humpback_risk_sum   if no regs   Blue_risk_sum      if no regs
# #2013-2014    126.48841                       1197.4357
# #2014-2015    64.40543                        649.9477
# #2015-2016    90.71798                        996.4560
# #2016-2017    104.04905                       975.0721
# #2017-2018    119.15737                       1268.0103
# #2018-2019    62.49217          64.78067      934.2833           1231.093
# #2019-2020    49.25905 	        69.16616      805.4855           1132.563
# 
# #note that this is taking 'average of averages'
# #hump risk: average across non-reg seasons
# (126.48841+64.40543+90.71798+104.04905+119.15737)/5
# #100.9636
# #% change 2018-19 from the average
# (62.49217-100.9636)/100.9636*100
# #-38.10426
# #% change 2019-20 from the average
# (49.25905-100.9636)/100.9636*100
# #-51.21108
# 
# #note that this is taking 'average of averages'
# #blue risk: average across non-reg seasons
# (1197.4357+649.9477+996.4560+975.0721+1268.0103)/5
# #1017.384
# #% change 2018-19 from the average
# (934.2833-1017.384)/1017.384*100
# #-8.168076
# #% change 2019-20 from the average
# (805.4855-1017.384)/1017.384*100
# #-20.82778
# 
# 
# 
# 
# 
# #COMPANION PLOT - ts plot of whale density/occurrence in study area
# 
# #start with study_area_whale df - data already filtered to May-Sep, but filter seasons, and separate species
# 
# study_area_hw <- study_area_whale %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   select(GRID5KM_ID:Humpback_dens_mean) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
#   group_by(season) %>% 
#   summarise(
#     Hump_dens_sum = sum(Humpback_dens_mean, na.rm=TRUE))
# 
# study_area_bw <- study_area_whale %>%
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#     select(-Humpback_dens_mean) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
#   group_by(season) %>%
#   summarise(
#     Blue_dens_sum = sum(Blue_occurrence_mean, na.rm=TRUE)) 
# 
# 
# ts_hump_dens_MaySep_study_area_sum <- ggplot() +
#   geom_point(data = study_area_hw, aes(x = season, y = Hump_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_hw, aes(x = season, y = Hump_dens_sum, group = 1)) +
#   ylab("Summed humpback Whale Density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_hump_dens_MaySep_study_area_sum
# 
# 
# 
# ts_blue_occur_MaySep_study_area_sum <- ggplot() +
#   geom_point(data = study_area_bw, aes(x = season, y = Blue_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_bw, aes(x = season, y = Blue_dens_sum, group = 1)) +
#   ylab("Summed blue Whale Density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_blue_occur_MaySep_study_area_sum
# 
# # plot blues and humps together
# png(paste0(path_figures, "/ts_sum_blue_occur_hump_dens_2014_2020_by crab season_MaySep only_in_study_area_CORRECTLY FILTERED.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_hump_dens_MaySep_study_area_sum,
#           ts_blue_occur_MaySep_study_area_sum,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())
# 
# 
# 
# 
# 
# #-------------------
# #####comparing pre-reg 2013-2018 seasons to post-reg (a) 2018-2019, and (b) 2019-2020 seasons separately. 
# #####For (a) focus on Jul-Sep of 2013-18 vs 2018-19. For (b) focus on May-Sep 2013-18 vs 2019-20
# 
# # ts plot: summed Jul-Sep risk to whales in study area
# # compare pre-reg and 2018-2019
# 
# plot_subset_pre_reg_vs_2018_2019 <- risk_whales_WA_MaySep %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Humpback_risk_sum = sum(hump_risk, na.rm=TRUE))
# 
# # ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
# #   geom_point(data = plot_subset_pre_reg_vs_2018_2019, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
# #   geom_line(data = plot_subset_pre_reg_vs_2018_2019, aes(x = season, y = Humpback_risk_sum,group = 1)) +
# #   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Humpback_risk_sum_2018_2019, group = 1), size=4, colour = 'red') +
# #   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Humpback_risk_sum_2019_2020, group = 1), size=4, colour = 'red') +
# #   ylab("summed humpback Whale Risk Jul-Sep") + 
# #   xlab("Season") +
# #   theme_classic() +
# #   theme(legend.title = element_blank(),
# #         #title = element_text(size = 26),
# #         legend.text = element_text(size = 20),
# #         legend.position = c(.15, .85),
# #         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
# #         axis.text.y = element_text(size = 20),
# #         axis.title = element_text(size = 20),
# #         strip.text = element_text(size=20),
# #         strip.background = element_blank(),
# #         strip.placement = "left"
# #   )
# # ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2018_2019
# 
# 
# # ts plot: summed May-Sep risk to whales in study area
# # compare pre-reg and 2019-2020
# 
# plot_subset_pre_reg_vs_2019_2020 <- risk_whales_WA_MaySep %>% 
#   #take out 2018-2019 seasons
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Humpback_risk_sum = sum(hump_risk, na.rm=TRUE))
# 
# # ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
# #   geom_point(data = plot_subset_pre_reg_vs_2019_2020, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
# #   geom_line(data = plot_subset_pre_reg_vs_2019_2020, aes(x = season, y = Humpback_risk_sum,group = 1)) +
# #   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Humpback_risk_sum_2018_2019, group = 1), size=4, colour = 'red') +
# #   #geom_point(data = sum_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Humpback_risk_sum_2019_2020, group = 1), size=4, colour = 'red') +
# #   ylab("summed humpback Whale Risk May-Sep") + 
# #   xlab("Season") +
# #   theme_classic() +
# #   theme(legend.title = element_blank(),
# #         #title = element_text(size = 26),
# #         legend.text = element_text(size = 20),
# #         legend.position = c(.15, .85),
# #         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
# #         axis.text.y = element_text(size = 20),
# #         axis.title = element_text(size = 20),
# #         strip.text = element_text(size=20),
# #         strip.background = element_blank(),
# #         strip.placement = "left"
# #   )
# # ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020
# 
# 
# 
# 
# #companion plot Jul-Sep
# study_area_hw_pre_reg_vs_2018_2019 <- study_area_whale %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   select(GRID5KM_ID:Humpback_dens_mean) %>% 
#   group_by(season) %>% 
#   summarise(
#     Hump_dens_sum = sum(Humpback_dens_mean, na.rm=TRUE))
# 
# ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_point(data = study_area_hw_pre_reg_vs_2018_2019, aes(x = season, y = Hump_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_hw_pre_reg_vs_2018_2019, aes(x = season, y = Hump_dens_sum, group = 1)) +
#   ylab("Summed humpback Whale Density Jul-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2018_2019
# 
# 
# #companion plot May-Sep
# # compare pre-reg and 2019-2020
# study_area_hw_pre_reg_vs_2019_2020 <- study_area_whale %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   select(GRID5KM_ID:Humpback_dens_mean) %>% 
#   group_by(season) %>% 
#   summarise(
#     Hump_dens_sum = sum(Humpback_dens_mean, na.rm=TRUE))
# 
# ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_point(data = study_area_hw_pre_reg_vs_2019_2020, aes(x = season, y = Hump_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_hw_pre_reg_vs_2019_2020, aes(x = season, y = Hump_dens_sum, group = 1)) +
#   ylab("Summed humpback Whale Density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2019_2020
# 
# 
# # plot HW risk and density, Jul-Sep and May-Sep
# png(paste0(path_figures, "/ts_sum_hump_risk_and_dens_JulSep_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2018_2019,
#           ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2018_2019,
#           ts_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020,
#           ts_hump_dens_MaySep_study_area_sum_pre_reg_vs_2019_2020,
#           ncol=2,
#           nrow=2,
#           legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())
# 
# 
# 
# 
# # ts plot: summed Jul-Sep risk to whales in study area
# # compare pre-reg and 2018-2019
# 
# bw_plot_subset_pre_reg_vs_2018_2019 <- risk_whales_WA_MaySep %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Blue_risk_sum = sum(blue_risk, na.rm=TRUE))
# 
# # ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
# #   geom_point(data = bw_plot_subset_pre_reg_vs_2018_2019, aes(x = season, y = Blue_risk_sum,group = 1), size=4) +
# #   geom_line(data = bw_plot_subset_pre_reg_vs_2018_2019, aes(x = season, y = Blue_risk_sum,group = 1)) +
# #   ylab("summed blue Whale Risk Jul-Sep") + 
# #   xlab("Season") +
# #   theme_classic() +
# #   theme(legend.title = element_blank(),
# #         #title = element_text(size = 26),
# #         legend.text = element_text(size = 20),
# #         legend.position = c(.15, .85),
# #         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
# #         axis.text.y = element_text(size = 20),
# #         axis.title = element_text(size = 20),
# #         strip.text = element_text(size=20),
# #         strip.background = element_blank(),
# #         strip.placement = "left"
# #   )
# # ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2018_2019
# 
# 
# # ts plot: summed May-Sep risk to whales in study area
# # compare pre-reg and 2019-2020
# 
# bw_plot_subset_pre_reg_vs_2019_2020 <- risk_whales_WA_MaySep %>% 
#   #take out 2018-2019 seasons
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Blue_risk_sum = sum(blue_risk, na.rm=TRUE))
# 
# # ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
# #   geom_point(data = bw_plot_subset_pre_reg_vs_2019_2020, aes(x = season, y = Blue_risk_sum,group = 1), size=4) +
# #   geom_line(data = bw_plot_subset_pre_reg_vs_2019_2020, aes(x = season, y = Blue_risk_sum,group = 1)) +
# #   ylab("Summed blue Whale Risk May-Sep") + 
# #   xlab("Season") +
# #   theme_classic() +
# #   theme(legend.title = element_blank(),
# #         #title = element_text(size = 26),
# #         legend.text = element_text(size = 20),
# #         legend.position = c(.15, .85),
# #         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
# #         axis.text.y = element_text(size = 20),
# #         axis.title = element_text(size = 20),
# #         strip.text = element_text(size=20),
# #         strip.background = element_blank(),
# #         strip.placement = "left"
# #   )
# # ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020
# 
# 
# 
# 
# #companion plot Jul-Sep
# study_area_bw_pre_reg_vs_2018_2019 <- study_area_whale %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   select(-Humpback_dens_mean) %>% 
#   group_by(season) %>% 
#   summarise(
#     Blue_dens_sum = sum(Blue_occurrence_mean, na.rm=TRUE))
# 
# ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_point(data = study_area_bw_pre_reg_vs_2018_2019, aes(x = season, y = Blue_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_bw_pre_reg_vs_2018_2019, aes(x = season, y = Blue_dens_sum, group = 1)) +
#   ylab("Summed blue Whale Density Jul-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2018_2019
# 
# 
# #companion plot May-Sep
# # compare pre-reg and 2019-2020
# study_area_bw_pre_reg_vs_2019_2020 <- study_area_whale %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   select(-Humpback_dens_mean) %>% 
#   group_by(season) %>% 
#   summarise(
#     Blue_dens_sum = sum(Blue_occurrence_mean, na.rm=TRUE))
# 
# ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_point(data = study_area_bw_pre_reg_vs_2019_2020, aes(x = season, y = Blue_dens_sum, group = 1), size=4) +
#   geom_line(data = study_area_bw_pre_reg_vs_2019_2020, aes(x = season, y = Blue_dens_sum, group = 1)) +
#   ylab("Summed blue Whale Density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2019_2020
# 
# 
# # plot BW risk and density, Jul-Sep and May-Sep
# png(paste0(path_figures, "/ts_sum_blue_risk_and_dens_JulSep_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2018_2019,
#           ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2018_2019,
#           ts_sum_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020,
#           ts_blue_occur_MaySep_study_area_sum_pre_reg_vs_2019_2020,
#           ncol=2,
#           nrow=2,
#           legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())
# 
# 
# 
# #BOXPLOTS
# #HW
# plot_subset_pre_reg_vs_2018_2019_box <- risk_whales_WA_MaySep %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered
# 
# box_sum_hump_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_boxplot(data = plot_subset_pre_reg_vs_2018_2019_box, aes(x = pre_post_reg, y = hump_risk)) +
#   ylab("humpback Whale Risk Jul-Sep") + 
#   xlab("Season") +
#   scale_x_discrete(limits = rev) +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_sum_hump_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019
# #warning: Removed 17 rows containing non-finite values (stat_boxplot).
# #--> this is because study area has few extra grids outside of HW model domain
# 
# 
# plot_subset_pre_reg_vs_2019_2020_box <- risk_whales_WA_MaySep %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered
# 
# box_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_boxplot(data = plot_subset_pre_reg_vs_2019_2020_box, aes(x = pre_post_reg, y = hump_risk)) +
#   ylab("humpback Whale Risk May-Sep") + 
#   scale_x_discrete(limits = rev) +
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020


#################################################
##making the boxplot how Jameal suggested:
#sum risk across months, each grid has 1 value for the post-reg season, use that for post-reg seasons boxplot
#for pre-reg seasons, sum across months and then average across pre-reg years and use that for boxplot
## --> actually, what we want to do here instead, is to sum across grids, so that each month has 1 value
##but note that this way Jul-Sep 2018-2019 only has 3 values, 1 for each month

#sum across grids

##Jul-Sep
plot_subset_2018_2019_box <- risk_whales_WA_MaySep %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season != '2019-2020') %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(hump_risk = sum(hump_risk, na.rm=TRUE),
            blue_risk = sum(blue_risk, na.rm=TRUE)) 
  

box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019 <- ggplot() +
  geom_violin(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk), lwd=2) +
  #geom_dotplot(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Humpback Whale Risk") + 
  ylab("Risk") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019


##NORMALIZED
plot_subset_2018_2019_box <- risk_whales_WA_MaySep_normalized %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season != '2019-2020') %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE)) 


box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019 <- ggplot() +
  geom_violin(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk), lwd=2) +
  stat_summary(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk),
               fun = "mean",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  #geom_jitter(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk), size=5, seed = 1, width = 0.025) + 
  geom_dotplot(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Humpback Whale Risk") + 
  ylab("Risk") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/HW_risk_prePreg_vs_2019_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019,
  ncol=1,
  nrow=1
  #legend="top",
  #labels="auto",
  #vjust=8,
  #hjust=-0.2
)
invisible(dev.off())


##May-Sep
MaySep_plot_subset_2019_2020_box <- risk_whales_WA_MaySep %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  filter(season != '2018-2019') %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(hump_risk = sum(hump_risk, na.rm=TRUE),
            blue_risk = sum(blue_risk, na.rm=TRUE)) 


box_hump_risk_MaySep_pre_reg_vs_2019_2020 <- ggplot() +
  geom_violin(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = hump_risk), lwd=2) +
  #geom_dotplot(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Humpback Whale Risk") + 
  ylab("") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_MaySep_pre_reg_vs_2019_2020


##NORMALIZED
MaySep_plot_subset_2019_2020_box <- risk_whales_WA_MaySep_normalized %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  filter(season != '2018-2019') %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE)) 


box_hump_risk_MaySep_pre_reg_vs_2019_2020 <- ggplot() +
  geom_violin(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = hump_risk), lwd=2) +
  stat_summary(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = hump_risk),
               fun = "mean",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Humpback Whale Risk") + 
  ylab("") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_MaySep_pre_reg_vs_2019_2020


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/HW_risk_prePreg_vs_2020_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_hump_risk_MaySep_pre_reg_vs_2019_2020,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())

################################




# #companion plot Jul-Sep
# 
# #all grid_year_month as rows in df
# study_area_hw_pre_reg_vs_2018_2019_box <- study_area_whale %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg))
# 
# box_hump_dens_Jul_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_violin(data = study_area_hw_pre_reg_vs_2018_2019_box, aes(x = pre_post_reg, y = Humpback_dens_mean)) +
#   scale_x_discrete(limits = rev) +
#   ylab("humpback Whale density Jul-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_hump_dens_Jul_Sep_study_area_pre_reg_vs_2018_2019  
# 
# #sum across grids as did for risk
# study_area_hw_pre_reg_vs_2018_2019_box <- study_area_whale %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   group_by(season, month, pre_post_reg) %>% 
#   summarise(hump_risk_sum = sum(Humpback_dens_mean, na.rm=TRUE))
# 
# box_hump_dens_Jul_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_violin(data = study_area_hw_pre_reg_vs_2018_2019_box, aes(x = pre_post_reg, y = hump_risk_sum)) +
#   scale_x_discrete(limits = rev) +
#   ylab("humpback Whale density sum Jul-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_hump_dens_Jul_Sep_study_area_pre_reg_vs_2018_2019  
# 
# 
# 
# 
# #companion plot May-Sep
# study_area_hw_pre_reg_vs_2019_2020_box <- study_area_whale %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg))
# 
# box_hump_dens_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_boxplot(data = study_area_hw_pre_reg_vs_2019_2020_box, aes(x = pre_post_reg, y = Humpback_dens_mean)) +
#   scale_x_discrete(limits = rev) +
#   ylab("humpback Whale density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_hump_dens_May_Sep_study_area_pre_reg_vs_2019_2020  
# 
# 
# # plot HW risk and density, Jul-Sep and May-Sep
# png(paste0(path_figures, "/box_hump_risk_and_dens_JulSep_MaySep.png"), width = 14, height = 14, units = "in", res = 300)
# ggarrange(box_sum_hump_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019,
#           box_hump_dens_Jul_Sep_study_area_pre_reg_vs_2018_2019,
#           box_sum_hump_risk_May_Sep_study_area_pre_reg_vs_2019_2020,
#           box_hump_dens_May_Sep_study_area_pre_reg_vs_2019_2020 ,
#           ncol=2,
#           nrow=2,
#           legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



#BW
# bw_plot_subset_pre_reg_vs_2018_2019_box <- risk_whales_WA_MaySep %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered


# box_blue_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_boxplot(data = bw_plot_subset_pre_reg_vs_2018_2019_box, aes(x = pre_post_reg, y = blue_risk )) +
#   ylab("blue Whale Risk Jul-Sep") + 
#   xlab("Season") +
#   scale_x_discrete(limits = rev) +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_blue_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019
#warning: Removed 141 rows containing non-finite values (stat_boxplot)
#--> this is because bw domain is smaller than study area

# box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_violin(data = plot_subset_pre_reg_box, aes(x = pre_post_reg, y = blue_risk)) +
#   geom_violin(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = blue_risk)) +
#   ylab("blue Whale Risk Jul-Sep") + 
#   xlab("") +
#   scale_x_discrete(limits = rev) +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019


# bw_plot_subset_pre_reg_vs_2019_2020_box <- risk_whales_WA_MaySep %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
#   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered
# 
# box_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_boxplot(data = bw_plot_subset_pre_reg_vs_2019_2020_box, aes(x = pre_post_reg, y = blue_risk)) +
#   ylab("blue Whale Risk May-Sep") + 
#   scale_x_discrete(limits = rev) +
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020


#Jul-Sep
plot_subset_2018_2019_box

box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019 <- ggplot() +
  geom_violin(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = blue_risk), lwd=2) +
  stat_summary(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = blue_risk),
               fun = "mean",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = blue_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Blue Whale Risk") + 
  ylab("Risk") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/BW_risk_prePreg_vs_2019_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())

#May-Sep
MaySep_plot_subset_2019_2020_box 

box_blue_risk_MaySep_pre_reg_vs_2019_2020 <- ggplot() +
  geom_violin(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = blue_risk), lwd=2) +
  stat_summary(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = blue_risk),
               fun = "mean",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = blue_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  #ylab("Summed Blue Whale Risk") + 
  ylab("") + 
  xlab("") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020"), expand = c(0,0)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_blue_risk_MaySep_pre_reg_vs_2019_2020

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/BW_risk_prePreg_vs_2020_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_blue_risk_MaySep_pre_reg_vs_2019_2020,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


# #companion plot Jul-Sep
# study_area_bw_pre_reg_vs_2018_2019_box <- study_area_whale %>% 
#   filter(month %in% c('07', '08', '09')) %>% 
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#   filter(study_area=='Y') %>% #need to filter to be only study area grids
#   mutate(pre_post_reg = 
#            ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg))
# 
# box_blue_occur_Jul_Sep_study_area_pre_reg_vs_2018_2019 <- ggplot() +
#   geom_boxplot(data = study_area_bw_pre_reg_vs_2018_2019_box, aes(x = pre_post_reg, y = Blue_occurrence_mean)) +
#   scale_x_discrete(limits = rev) +
#   ylab("blue Whale density Jul-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_blue_occur_Jul_Sep_study_area_pre_reg_vs_2018_2019  
# 
# 
# #companion plot May-Sep
# study_area_bw_pre_reg_vs_2019_2020_box <- study_area_whale %>% 
#   #take out 2018-2019 season
#   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
#   filter(study_area=='Y') %>% 
#   mutate(pre_post_reg = 
#            ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
#   mutate(pre_post_reg = as.factor(pre_post_reg))
# 
# box_blue_occur_May_Sep_study_area_pre_reg_vs_2019_2020 <- ggplot() +
#   geom_boxplot(data = study_area_bw_pre_reg_vs_2019_2020_box, aes(x = pre_post_reg, y = Blue_occurrence_mean)) +
#   scale_x_discrete(limits = rev) +
#   ylab("blue Whale density May-Sep") + 
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# box_blue_occur_May_Sep_study_area_pre_reg_vs_2019_2020  


# plot BW risk and density, Jul-Sep and May-Sep
png(paste0(path_figures, "/box_blue_risk_and_dens_JulSep_MaySep.png"), width = 14, height = 14, units = "in", res = 300)
ggarrange(box_blue_risk_Jul_Sep_study_area_pre_reg_vs_2018_2019,
          box_blue_occur_Jul_Sep_study_area_pre_reg_vs_2018_2019,
          box_blue_risk_May_Sep_study_area_pre_reg_vs_2019_2020,
          box_blue_occur_May_Sep_study_area_pre_reg_vs_2019_2020 ,
          ncol=2,
          nrow=2,
          legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



#when 2-week input file
percent_change_in_risk_MaySep <- MaySep_plot_subset_2019_2020_box %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
percent_change_in_risk_MaySep 
#pre_post_reg mean_hw_risk mean_bw_risk
#2019-2020            9.15         150.
#pre-reg             18.8          193.
#HW:
(9.150597-18.795108)/18.795108*100 #-51.31394
#BW:
(149.8488-192.7972)/192.7972*100 #-22.27646

percent_change_in_risk_JulSep <- plot_subset_2018_2019_box %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
percent_change_in_risk_JulSep
#pre_post_reg mean_hw_risk mean_bw_risk
#2018-2019            3.90         223.
#pre-reg             17.0          253.
#HW:
(3.897254-17.044269)/17.044269*100 #-77.13452
#BW:
(222.8370-253.3776)/253.3776*100 #-12.05339



#when 1-month input file
percent_change_in_risk_JulSep <- plot_subset_2018_2019_box %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
percent_change_in_risk_JulSep
#pre_post_reg mean_hw_risk mean_bw_risk
#2018-2019            4.00         232.
#pre-reg             17.9          264.
#HW:
(3.998742-17.870604)/17.870604*100 #-77.62391
#BW:
(232.3115-263.9242)/263.9242*100 #-11.97795
##NORMALIZED:
#pre_post_reg mean_hw_risk mean_bw_risk
#2018-2019            0.866         3.78
#pre-reg             4.00          4.29
#HW:
(0.866-4.00)/4.00*100 #-78.35
#BW:
(3.78-4.29)/4.29*100 #-11.88811
#--> no change after fixing normalization 0s

percent_change_in_risk_MaySep <- MaySep_plot_subset_2019_2020_box %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
percent_change_in_risk_MaySep 
#pre_post_reg mean_hw_risk mean_bw_risk
#2019-2020            9.85         161.
#pre-reg             20.2          203.
#HW:
(9.851811-20.192729)/20.192729*100 #-51.2111
#BW:
(161.0971-203.4769)/203.4769*100 #-20.82782
##NORMALIZED:
#pre_post_reg mean_hw_risk mean_bw_risk
#2019-2020            2.19         2.60
#pre-reg             4.51          3.27
#HW:
(2.19-4.51)/4.51*100 #-51.44124
#BW:
(2.60-3.27)/3.27*100 #-20.4893
#--> no change after fixing normalization 0s






##NO REGS, 1-month input file
subset_2018_2019_NO_REGS #subset_2018_2019_NO_REGS_NORMALIZED 
subset_2019_2020_NO_REGS #subset_2019_2020_NO_REGS_NORMALIZED 

summary_risk_JulSep_NO_REGS_NORMALIZED <- subset_2018_2019_NO_REGS_NORMALIZED %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
summary_risk_JulSep_NO_REGS_NORMALIZED
## % change in 2018-2019 if had or did not have regs
#HW:
(3.998742-6.041264)/6.041264*100 #-33.80951
#BW:
(232.3115-351.0894)/351.0894*100 #-33.83124
##NORMALIZED
#HW:2019 if no regs: 1.31 ;; 2019 with regs 0.866
(0.866-1.31)/1.31*100 ##-33.89
#BUT
(1.31-0.866)/0.866*100 ##51.27
#BW: 2019 if no regs: 5.71 ;; 2019 with regs 3.78
(3.78-5.71)/5.71*100 ##-33.80
#BUT
(5.71-3.78)/3.78*100 ##51.06

summary_risk_MaySep_NO_REGS_NORMALIZED <- subset_2019_2020_NO_REGS_NORMALIZED %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
summary_risk_MaySep_NO_REGS_NORMALIZED
## % change in 2019-2020 if had or did not have regs
#HW:
(9.851811-14.89231)/14.89231*100 #-33.84632
#BW:
(161.0971-243.4962)/243.4962*100 #-33.83999
##NORMALIZED
#HW:2020 if no regs: 3.32 ;; 2020 with regs 2.19
(2.19-3.32)/3.32*100 ##-34.03
#BUT
(3.32-2.19)/2.19*100 ##51.60
#BW: 2020 if no regs: 3.94 ;; 2020 with regs 2.60
(2.60-3.94)/3.94*100 ##-34.01
#BUT
(3.94-2.60)/2.60*100 ##51.54








###########################

#boxplot of last 2 seasons with or wihtout regs

#risk_whales_WA_MaySep / risk_whales_WA_MaySep_normalized
#risk_whales_WA_MaySep_no_regs / risk_whales_WA_MaySep_no_regs_normalized

#sum risk across grids so each month has one risk value, then plot

#HW
box_2018_2019_with_regs <- risk_whales_WA_MaySep_normalized %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season %in% c('2018-2019')) %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(regs = "with regulations") %>% 
  group_by(season, month, regs) %>% 
  summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE))
  
box_2018_2019_without_regs <- risk_whales_WA_MaySep_no_regs_normalized %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season %in% c('2018-2019')) %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>% 
  mutate(regs = "without regulations") %>% 
  group_by(season, month, regs) %>% 
  summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE))


box_hw_risk_2018_2019_with_and_without_regs <- ggplot() +
  geom_violin(data = box_2018_2019_with_regs, aes(x = regs, y = hump_risk), lwd=1) +
  geom_violin(data = box_2018_2019_without_regs, aes(x = regs, y = hump_risk), lwd=1, fill='gray88') +
  ylab("Summed Humpback Whale Risk Jul-Sep") + 
  xlab("2018-2019 season") +
  #scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 20, angle = 0),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hw_risk_2018_2019_with_and_without_regs



box_2019_2020_with_regs <- risk_whales_WA_MaySep_normalized %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  filter(season %in% c('2019-2020')) %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>%  #this will effectively mean that only fishing footprint is considered
  mutate(regs = "with regulations") %>% 
  group_by(season, month, regs) %>% 
  summarise(hump_risk = sum(hump_risk_norm , na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE))

box_2019_2020_without_regs <- risk_whales_WA_MaySep_no_regs_normalized %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  filter(season %in% c('2019-2020')) %>% 
  filter(study_area=='Y') %>% 
  #filter(!is.na(mean_M2_trapdens)) %>% 
  mutate(regs = "without regulations") %>% 
  group_by(season, month, regs) %>% 
  summarise(hump_risk = sum(hump_risk_norm, na.rm=TRUE),
            blue_risk = sum(blue_risk_norm, na.rm=TRUE))


box_hw_risk_2019_2020_with_and_without_regs <- ggplot() +
  geom_violin(data = box_2019_2020_with_regs, aes(x = regs, y = hump_risk), lwd=1) +
  geom_violin(data = box_2019_2020_without_regs, aes(x = regs, y = hump_risk), lwd=1, fill='gray88') +
  ylab("Summed Humpback Whale Risk May-Sep") + 
  xlab("2019-2020 season") +
  #scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 20, angle = 0),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hw_risk_2019_2020_with_and_without_regs




test <- rbind(box_2018_2019_with_regs, box_2018_2019_without_regs, box_2019_2020_with_regs, box_2019_2020_without_regs)

box_hw_risk_2018_2019_2020_with_and_without_regs <- ggplot() +
    geom_violin(data = test, aes(x = season, y = hump_risk, fill = regs), lwd=2) +
  scale_fill_manual(values = c("white", "gray80"))+
  ylab("Risk") + 
  scale_x_discrete(labels=c("2018-2019" = "2019", "2019-2020" = "2020")) +
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hw_risk_2018_2019_2020_with_and_without_regs


#plot things together and save
png(paste0(path_figures, "/hump_risk_2019_and_2020_with_and_without_regs_NORM.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_hw_risk_2018_2019_2020_with_and_without_regs,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


box_bw_risk_2018_2019_2020_with_and_without_regs <- ggplot() +
  geom_violin(data = test, aes(x = season, y = blue_risk, fill = regs), lwd=2) +
  scale_fill_manual(values = c("white", "gray80"))+
  ylab("Risk") + 
  scale_x_discrete(labels=c("2018-2019" = "2019", "2019-2020" = "2020")) +
    xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = "none",
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_bw_risk_2018_2019_2020_with_and_without_regs


#plot things together and save
png(paste0(path_figures, "/blue_risk_2019_and_2020_with_and_without_regs_NORM.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_bw_risk_2018_2019_2020_with_and_without_regs,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




# to calculate % change between 'with regulations' and 'without regulations' 
# summary_test <- test %>% 
#   group_by(season, regs) %>% 
#   summarise(sum_hump_risk = sum(hump_risk, na.rm = TRUE),
#             sum_blue_risk = sum(blue_risk, na.rm = TRUE),
#             n_row = n()
#             )
#season             regs        sum_hump_risk   sum_blue_risk
#2018-2019  with regulations      11.99622        696.9346
#2018-2019  without regulations   18.12379        1053.2682
#2019-2020  with regulations      49.25905        805.4855
#2019-2020  without regulations   74.46154        1217.4808









# point_bw_risk_2018_2019_2020_with_and_without_regs <- ggplot() +
#   
#   #geom_point(data = summary_test, aes(x = season, y = sum_hump_risk, color = regs), size=5) +
#   #ylab("sum hump Whale Risk") + 
#   
#   geom_point(data = summary_test, aes(x = season, y = sum_blue_risk, color = regs), size=5) +
#   ylab("sum blue Whale Risk") + 
#   
#   xlab("Season") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.2, .85),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# point_bw_risk_2018_2019_2020_with_and_without_regs
# 
# 
# #plot things together and save
# png(paste0(path_figures, "/point_blue_risk_2019_and_2020_with_and_without_regs_on_same_scale.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(point_bw_risk_2018_2019_2020_with_and_without_regs,
#           #box_hw_risk_2018_2019_with_and_without_regs,
#           #box_hw_risk_2019_2020_with_and_without_regs,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())






#######################################################################################
#######################################################################################















############## RISK IS AVERAGED #######################


#Fishery centric look at risk


#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

#-----------------------------------------------------------------------------------
#bring in some grids
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


#path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#risk to whales in STUDY AREA during May-Sep


#whale data

#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)


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


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

x.whale_crab_season_May_Sep <-  x.whale_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


#fishing effort

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA <- x.fish_WA %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))


#this is an older version when wanted to have a year_month column (2020_01) to join to whale data
# make column for year month for fishing data to allow matching with whale data
## step 1, make some columns that we can use
# x.fish_WA3 <- x.fish_WA2 %>%
#   separate(season_month, into = c("season", "month_name"), sep = "_") %>%
#   separate(season, into = c("yr_start", "yr_end"), sep = "-") %>%
#   mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
#   mutate(month = sprintf("%02d", as.numeric(month))) #change month to two digit number
# ## step 2, grab yr_start for December effort and make a year_month column in a new df
# df1 <- x.fish_WA3 %>%
#   filter(month_name=='December')
# df1 <- df1 %>%
#   mutate(year_month = paste0(yr_start,"_",month))
# ## step 3, grab yr_end for non-December effort and make a year_month column in a new df
# df2 <- x.fish_WA3 %>%
#   filter(month_name !='December')
# df2 <- df2 %>%
#   mutate(year_month = paste0(yr_end,"_",month))
# # squish the December and non-December df's together  
# x.fish_WA4 <- rbind(df1,df2)


#if no regs in place
path.fish_WA_no_regs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step_NO_REGS.rds"
x.fish_WA_no_regs <- readRDS(path.fish_WA_no_regs)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA_no_regs <- x.fish_WA_no_regs %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2_no_regs <- x.fish_WA_no_regs %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep_no_regs <- x.fish_WA2_no_regs %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))

#-----------------------------------------------------------------------------------

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
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

#join fishing data to study area grid with whale data
study_area_whale_fishing <- left_join(study_area_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep <- study_area_whale_fishing %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
   mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))

 

#if no regs
#join fishing data to study area grid with whale data
study_area_whale_fishing_no_regs <- left_join(study_area_whale, x.fish_WA_MaySep_no_regs, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep_no_regs <- study_area_whale_fishing_no_regs %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))



mean_risk_whales_WA_MaySep_no_regs_2018_2019 <- risk_whales_WA_MaySep_no_regs %>%
  filter(season == '2018-2019') %>% 
  filter(study_area=='Y')%>% 
  group_by(season) %>% 
  summarise(
    Humpback_risk_mean_2018_2019 = mean(hump_risk, na.rm=TRUE),
    sd_2018_2019_hw = sd(hump_risk, na.rm = TRUE),
    n_2018_2019_hw = n(),
    Blue_risk_mean_2018_2019 = mean(blue_risk, na.rm=TRUE),
    sd_2018_2019_bw = sd(blue_risk, na.rm = TRUE),
    n_2018_2019_bw = n()
  )%>% 
  mutate(se_2018_2019_hw = sd_2018_2019_hw / sqrt(n_2018_2019_hw),
         lower.ci_2018_2019_hw = Humpback_risk_mean_2018_2019 - qt(1 - (0.05 / 2), n_2018_2019_hw - 1) * se_2018_2019_hw,
         upper.ci_2018_2019_hw = Humpback_risk_mean_2018_2019 + qt(1 - (0.05 / 2), n_2018_2019_hw - 1) * se_2018_2019_hw) %>% 
  mutate(se_2018_2019_bw = sd_2018_2019_bw / sqrt(n_2018_2019_bw),
         lower.ci_2018_2019_bw = Blue_risk_mean_2018_2019 - qt(1 - (0.05 / 2), n_2018_2019_bw - 1) * se_2018_2019_bw,
         upper.ci_2018_2019_bw = Blue_risk_mean_2018_2019 + qt(1 - (0.05 / 2), n_2018_2019_bw - 1) * se_2018_2019_bw)

mean_risk_whales_WA_MaySep_no_regs_2019_2020 <- risk_whales_WA_MaySep_no_regs %>%
  filter(season == '2019-2020') %>% 
  filter(study_area=='Y')%>% 
  group_by(season) %>% 
  summarise(
    Humpback_risk_mean_2019_2020 = mean(hump_risk, na.rm=TRUE),
    sd_2019_2020_hw = sd(hump_risk, na.rm = TRUE),
    n_2019_2020_hw = n(),
    Blue_risk_mean_2019_2020 = mean(blue_risk, na.rm=TRUE),
    sd_2019_2020_bw = sd(blue_risk, na.rm = TRUE),
    n_2019_2020_bw = n()
  )%>% 
  mutate(se_2019_2020_hw = sd_2019_2020_hw / sqrt(n_2019_2020_hw),
         lower.ci_2019_2020_hw = Humpback_risk_mean_2019_2020 - qt(1 - (0.05 / 2), n_2019_2020_hw - 1) * se_2019_2020_hw,
         upper.ci_2019_2020_hw = Humpback_risk_mean_2019_2020 + qt(1 - (0.05 / 2), n_2019_2020_hw - 1) * se_2019_2020_hw) %>% 
  mutate(se_2019_2020_bw = sd_2019_2020_bw / sqrt(n_2019_2020_bw),
         lower.ci_2019_2020_bw = Blue_risk_mean_2019_2020 - qt(1 - (0.05 / 2), n_2019_2020_bw - 1) * se_2019_2020_bw,
         upper.ci_2019_2020_bw = Blue_risk_mean_2019_2020 + qt(1 - (0.05 / 2), n_2019_2020_bw - 1) * se_2019_2020_bw)

#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
# grid5km_bbox <- st_bbox(grid.5km.lno %>% 
#                           st_as_sf()
# )
bbox = c(-127,45,-120,49) 


subset_data <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=blue_risk,
              col=blue_risk
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_test

#-----------------------------------------------------------------------------------

# ts plot: May-Sep risk to whales in study area

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    Humpback_risk_mean = mean(hump_risk, na.rm=TRUE),
    sd = sd(hump_risk, na.rm = TRUE),
    n = n()
  )%>% 
  mutate(se = sd / sqrt(n),
         lower.ci = Humpback_risk_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = Humpback_risk_mean + qt(1 - (0.05 / 2), n - 1) * se)

ts_hump_risk_May_Sep_study_area <- ggplot() +
  geom_point(data = plot_subset, aes(x = season, y = Humpback_risk_mean,group = 1), size=4) +
  geom_line(data = plot_subset, aes(x = season, y = Humpback_risk_mean,group = 1)) +
  geom_errorbar(data = plot_subset,aes(x = season,ymin = lower.ci, ymax = upper.ci), colour="black", width=.2)+
  
  geom_point(data = mean_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Humpback_risk_mean_2018_2019, group = 1), size=4, colour = 'red') +
  geom_errorbar(data = mean_risk_whales_WA_MaySep_no_regs_2018_2019,aes(x = season, ymin = lower.ci_2018_2019_hw, ymax = upper.ci_2018_2019_hw), colour="red", width=.2)+
  geom_point(data = mean_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Humpback_risk_mean_2019_2020, group = 1), size=4, colour = 'red') +
  geom_errorbar(data = mean_risk_whales_WA_MaySep_no_regs_2019_2020,aes(x = season, ymin = lower.ci_2019_2020_hw, ymax = upper.ci_2019_2020_hw), colour="red", width=.2)+
  
  ylab("Humpback Whale Risk\nMay-Sep (mean +/- 95% CI)") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_risk_May_Sep_study_area

#--------------

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    Blue_risk_mean = mean(blue_risk, na.rm=TRUE),
    sd_bw = sd(blue_risk, na.rm = TRUE),
    n_bw = n()
  )%>% 
  mutate(se_bw = sd_bw / sqrt(n_bw),
         lower.ci_bw = Blue_risk_mean - qt(1 - (0.05 / 2), n_bw - 1) * se_bw,
         upper.ci_bw = Blue_risk_mean + qt(1 - (0.05 / 2), n_bw - 1) * se_bw)

ts_blue_risk_May_Sep_study_area <- ggplot() +
  geom_point(data = plot_subset, aes(x = season, y = Blue_risk_mean,group = 1), size=4) +
  geom_line(data = plot_subset, aes(x = season, y = Blue_risk_mean,group = 1)) +
  geom_errorbar(data = plot_subset,aes(x = season,ymin = lower.ci_bw, ymax = upper.ci_bw), colour="black", width=.2)+
  
  geom_point(data = mean_risk_whales_WA_MaySep_no_regs_2018_2019, aes(x = season, y = Blue_risk_mean_2018_2019, group = 1), size=4, colour = 'red') +
  geom_errorbar(data = mean_risk_whales_WA_MaySep_no_regs_2018_2019,aes(x = season, ymin = lower.ci_2018_2019_bw, ymax = upper.ci_2018_2019_bw), colour="red", width=.2)+
  geom_point(data = mean_risk_whales_WA_MaySep_no_regs_2019_2020, aes(x = season, y = Blue_risk_mean_2019_2020, group = 1), size=4, colour = 'red') +
  geom_errorbar(data = mean_risk_whales_WA_MaySep_no_regs_2019_2020,aes(x = season, ymin = lower.ci_2019_2020_bw, ymax = upper.ci_2019_2020_bw), colour="red", width=.2)+
  
  ylab("Blue Whale Risk\nMay-Sep (mean +/- 95% CI)") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_risk_May_Sep_study_area



#plot blues and humps together and save
png(paste0(path_figures, "/ts_mean_blue_hump_risk_with_CI_2014_2020_in_study_area_by crab season_MaySep_NO_REGS_2018_2020.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk_May_Sep_study_area,
          ts_blue_risk_May_Sep_study_area,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#risk in study area on 2-weekly step
#season     Humpback_risk_mean    Blue_risk_mean
#2013-2014    0.03605983            0.5607328
#2014-2015    0.01747583            0.2945987
#2015-2016    0.02588960            0.4626781
#2016-2017    0.02880740            0.4439852
#2017-2018    0.03432869            0.6248868
#2018-2019    0.01760779            0.4337910
#2019-2020    0.01369850            0.3654850

#note that this is taking 'average of averages'
#hump risk: average across non-reg seasons
(0.03605983+0.01747583+0.02588960+0.02880740+0.03432869)/5
#0.02851227
#% change 2018-19 from the average
(0.01760779-0.02851227)/0.02851227*100
#-38.24487
#% change 2019-20 from the average
(0.01369850-0.02851227)/0.02851227*100
#-51.95577

#note that this is taking 'average of averages'
#blue risk: average across non-reg seasons
(0.5607328+0.2945987+0.4626781+0.4439852+0.6248868)/5
#0.4773763
#% change 2018-19 from the average
(0.4337910-0.4773763)/0.4773763*100
#-9.130177
#% change 2019-20 from the average
(0.3654850-0.4773763)/0.4773763*100
#-23.4388



#risk in study area on 1-monthly step
#season     Humpback_risk_mean    Blue_risk_mean
#2013-2014    0.03787078            0.5841150
#2014-2015    0.01928306            0.3170477
#2015-2016    0.02716107            0.4860761
#2016-2017    0.03115241            0.4756449
#2017-2018    0.03567586            0.6185416
#2018-2019    0.01871023            0.4557479
#2019-2020    0.01474822            0.3929198

#note that this is taking 'average of averages'
#hump risk: average across non-reg seasons
(0.03787078+0.01928306+0.02716107+0.03115241+0.03567586)/5
#0.03022864
#% change 2018-19 from the average
(0.01871023-0.03022864)/0.03022864*100
#-38.10429
#% change 2019-20 from the average
(0.01474822-0.03022864)/0.03022864*100
#-51.2111


#note that this is taking 'average of averages'
#blue risk: average across non-reg seasons
(0.5841150+0.3170477+0.4860761+0.4756449+0.6185416)/5
#0.4962851
#% change 2018-19 from the average
(0.4557479-0.4962851)/0.4962851*100
#-8.168128
#% change 2019-20 from the average
(0.3929198-0.4962851)/0.4962851*100
#-20.82781




risk_whales_WA_MaySep_by_seaon_month <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% 
  group_by(season, month) %>%
  summarise(
    Humpback_risk_mean = mean(hump_risk, na.rm=TRUE),
    Blue_risk_mean = mean(blue_risk, na.rm=TRUE)
  )

test_ts <- ggplot(risk_whales_WA_MaySep_by_seaon_month, aes(x=as.factor(month), y=Blue_risk_mean, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  geom_point(size=2.5) + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean blue whale risk") +
  xlab("Month") + 
  scale_x_discrete("Month", labels = as.character(month), breaks = month)+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  #theme_classic()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
test_ts 

test_ts <- ggplot(risk_whales_WA_MaySep_by_seaon_month, aes(x=as.factor(month), y=Humpback_risk_mean, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  geom_point(size=2.5) + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean humpback whale risk") +
  xlab("Month") + 
  scale_x_discrete("Month", labels = as.character(month), breaks = month)+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
test_ts 





#COMPANION PLOT - ts plot of whale density/occurrence in study area

#start with study_area_whale df - data already filtered to May-Sep, but filter seasons, and separate species

study_area_hw <- study_area_whale %>% 
  select(GRID5KM_ID:Humpback_dens_mean) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  group_by(season) %>% 
  summarise(
    Hump_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE),
    sd = sd(Humpback_dens_mean, na.rm = TRUE),
    n = n()
    #Hump_dens_median = median(Humpback_dens_mean, na.rm=TRUE)
    ) %>% 
  mutate(se = sd / sqrt(n),
         lower.ci = Hump_dens_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = Hump_dens_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)

study_area_bw <- study_area_whale %>%
  select(-Humpback_dens_mean) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  group_by(season) %>%
  summarise(
    Blue_dens_mean = mean(Blue_occurrence_mean, na.rm=TRUE),
    sd_bw = sd(Blue_occurrence_mean, na.rm = TRUE),
    n_bw = n()
    #Blue_dens_median = median(Blue_occurrence_mean, na.rm=TRUE)
    ) %>% 
  mutate(se_bw = sd_bw / sqrt(n_bw),
         lower.ci_bw = Blue_dens_mean - qt(1 - (0.05 / 2), n_bw - 1) * se_bw,
         upper.ci_bw = Blue_dens_mean + qt(1 - (0.05 / 2), n_bw - 1) * se_bw) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)


ts_hump_dens_MaySep_study_area <- ggplot() +
  geom_point(data = study_area_hw, aes(x = season, y = Hump_dens_mean, group = 1), size=4) +
  geom_line(data = study_area_hw, aes(x = season, y = Hump_dens_mean, group = 1)) +
  geom_errorbar(data = study_area_hw,aes(x = season, ymin = lower.ci, ymax = upper.ci), colour="black", width=.2)+
  #geom_point(data = study_area_hw, aes(x = season, y = Hump_dens_median, group = 1), color = "darkred", size=4) +
  #geom_line(data = study_area_hw, aes(x = season, y = Hump_dens_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Density\nMay-Sep (mean +/- 95% CI)") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_dens_MaySep_study_area



ts_blue_occur_MaySep_study_area <- ggplot() +
  geom_point(data = study_area_bw, aes(x = season, y = Blue_dens_mean, group = 1), size=4) +
  geom_line(data = study_area_bw, aes(x = season, y = Blue_dens_mean, group = 1)) +
  geom_errorbar(data = study_area_bw,aes(x = season, ymin = lower.ci_bw, ymax = upper.ci_bw), colour="black", width=.2)+
  #geom_point(data = study_area_bw, aes(x = season, y = Blue_dens_median, group = 1), color = "darkred", size=4) +
  #geom_line(data = study_area_bw, aes(x = season, y = Blue_dens_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Blue Whale Density\nMay-Sep (mean +/- 95% CI)") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_occur_MaySep_study_area

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_occur_hump_dens_with_CI_2014_2020_by crab season_MaySep only_in_study_area.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_dens_MaySep_study_area,
          ts_blue_occur_MaySep_study_area,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#######################################################################################
#######################################################################################


# risk to whales in 'grids ever fished'



#find out grids that were ever fished
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
#x.fish_WA <- readRDS(path.fish_WA)
### Get grid cells with non-NA values for all, and save that of fishing data (grids ever fished)
grids_ever_fished_WA_waters <- sort(unique(x.fish_WA$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
# grids_ever_fished_WA <- grid.5km %>% filter(GRID5KM_ID %in% grids_ever_fished_WA_waters)
# glimpse(grids_ever_fished_WA)
# plot(grids_ever_fished_WA)


grids_ever_fished_df <- as.data.frame(grids_ever_fished_WA_waters) %>% 
  rename(GRID5KM_ID = grids_ever_fished_WA_waters)


#'grisd ever fished' needs to have all season-month combos for May-Sep
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
grids_ever_fished_df_with_all_season_month_combos <- crossing(grids_ever_fished_df, season_month_combos) %>%
  #and add to that the column to denote study area
  mutate(grids_ever_fished = 'Y')




#join whale data (same as above) to grids ever fished
grids_ever_fished_whale <- full_join(grids_ever_fished_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

#join fishing data (same as above) to grids ever fished with whale data
grids_ever_fished_whale_fishing <- left_join(grids_ever_fished_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep <- grids_ever_fished_whale_fishing %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))


#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
# grid5km_bbox <- st_bbox(grid.5km.lno %>% 
#                           st_as_sf()
# )
bbox = c(-127,45,-120,49) 


subset_data <- risk_whales_WA_MaySep %>% 
  filter(grids_ever_fished=='Y') %>% #restrict map to grids_ever_fished/check that all grids in grids_ever_fished show up
  filter(season == "2018-2019") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=grids_ever_fished,
              col=grids_ever_fished
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_test


#-----------------------------------------------------------------------------------

# ts plot: May-Sep risk to whales in study area

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(grids_ever_fished=='Y')  #restrict calculations to grids ever fished

ts_hump_risk_May_Sep_grids_ever_fished <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk, na.rm=TRUE)
    ),
  aes(
    x = season, 
    y = Humpback_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Humpback Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_risk_May_Sep_grids_ever_fished


ts_blue_risk_May_Sep_grids_ever_fished <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk, na.rm=TRUE)
    ), 
  aes(
    x = season, 
    y = Blue_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Blue Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_risk_May_Sep_grids_ever_fished


# # plot blues and humps together and save
# png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_grids_ever_fished_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_hump_risk_May_Sep_grids_ever_fished,
#           ts_blue_risk_May_Sep_grids_ever_fished,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


#######################################################################################
#######################################################################################



# risk to whales in 'grids ever fished in May-Sep'

#find out grids that were ever fished during May-Sep months
#subset data for May-Sep
subset_x.fish_WA_MaySep <- x.fish_WA %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")
grids_ever_fished_WA_waters_MaySep <- sort(unique(subset_x.fish_WA_MaySep$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
# grids_ever_fished_WA_MaySep <- grid.5km %>% filter(GRID5KM_ID %in% grids_ever_fished_WA_waters_MaySep)
# glimpse(grids_ever_fished_WA_MaySep)
# plot(grids_ever_fished_WA_MaySep)


grids_ever_fished_MaySep_df <- as.data.frame(grids_ever_fished_WA_waters_MaySep) %>% 
  rename(GRID5KM_ID = grids_ever_fished_WA_waters_MaySep)


#'grids ever fished in May-Sep' needs to have all season-month combos for May-Sep
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
grids_ever_fished_MaySep_df_with_all_season_month_combos <- crossing(grids_ever_fished_MaySep_df, season_month_combos) %>%
  #and add to that the column to denote study area
  mutate(grids_ever_fished_MaySep = 'Y')




#join whale data (same as above) to grids ever fished
grids_ever_fished_MaySep_whale <- full_join(grids_ever_fished_MaySep_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

#join fishing data (same as above) to grids ever fished with whale data
grids_ever_fished_MaySep_whale_fishing <- left_join(grids_ever_fished_MaySep_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep <- grids_ever_fished_MaySep_whale_fishing %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))


#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
# grid5km_bbox <- st_bbox(grid.5km.lno %>% 
#                           st_as_sf()
# )
bbox = c(-127,45,-120,49) 


subset_data <- risk_whales_WA_MaySep %>% 
  filter(grids_ever_fished_MaySep=='Y') %>% #restrict map to grids_ever_fished_MaySep/check that all grids in grids_ever_fished_MaySep show up
  filter(season == "2018-2019") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=blue_risk,
              col=blue_risk
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_test


#-----------------------------------------------------------------------------------

# ts plot: May-Sep risk to whales in study area

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(grids_ever_fished_MaySep=='Y')  #restrict calculations to grids ever fished in May-Sep

ts_hump_risk_May_Sep_grids_ever_fished_MaySep <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk, na.rm=TRUE)
    ),
  aes(
    x = season, 
    y = Humpback_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Humpback Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_risk_May_Sep_grids_ever_fished_MaySep


ts_blue_risk_May_Sep_grids_ever_fished_MaySep <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk, na.rm=TRUE)
    ), 
  aes(
    x = season, 
    y = Blue_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Blue Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_risk_May_Sep_grids_ever_fished_MaySep


# # plot blues and humps together and save
# png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_grids_ever_fished_MaySep_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_hump_risk_May_Sep_grids_ever_fished_MaySep,
#           ts_blue_risk_May_Sep_grids_ever_fished_MaySep,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


# #risk in grids ever fished in May-Sep
#  season    Humpback_risk_mean    % change from 2017-18     Blue_risk_mean  % change from 2017-18
#  2013-2014             0.0878                                1.20
#  2014-2015             0.0426                                0.632
#  2015-2016             0.0629                                0.990
#  2016-2017             0.0702                                0.952
#  2017-2018             0.0835                                1.34 
#  2018-2019             0.0429         -48.6                  0.931           -30.5
#  2019-2020             0.0334         -60                    0.785           -41.4

#note that this is taking 'average of averages'
#hump risk: average across non-reg seasons
(0.0878+0.0426+0.0629+0.0702+0.0835)/5
#0.0694
#% change from the average
(0.0429-0.0694)/0.0694*100
#-38.18444
(0.0334-0.0694)/0.0694*100
#-51.8732


#############################################################################################
#risk ts plot - only using grids that had fishing in a given season
#############################################################################################


#doing risk ts plot only for those grids that had fishing in each season
#i.e. the grids across which risk is calculated are not constant between seasons


path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA)
#don't think it is necessary to restrict whale data to fishing grids here, as the left join achieves that


# get avg traps dens per grid cell for each yr month
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data
## step 1, make some columns that we can use
x.fish_WA3 <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  separate(season, into = c("yr_start", "yr_end"), sep = "-") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) #change month to two digit number
## step 2, grab yr_start for December effort and make a year_month column in a new df
df1 <- x.fish_WA3 %>%
  filter(month_name=='December')
df1 <- df1 %>%
  mutate(year_month = paste0(yr_start,"_",month))
## step 3, grab yr_end for non-December effort and make a year_month column in a new df
df2 <- x.fish_WA3 %>%
  filter(month_name !='December')
df2 <- df2 %>%
  mutate(year_month = paste0(yr_end,"_",month))
# squish the December and non-December df's together  
x.fish_WA4 <- rbind(df1,df2)


# join the whale and fishing data by year_month
joined_df_hump <- x.fish_WA4 %>%
  left_join(x.hump_2009_2020,by=c("year_month","GRID5KM_ID"))

risk_hump <- joined_df_hump %>%
  mutate(
    hump_risk_M2 = Humpback_dens_mean * mean_M2_trapdens
  )

joined_df_blue <- x.fish_WA4 %>%
  left_join(x.blue.all,by=c("year_month","GRID5KM_ID"))

risk_blue <- joined_df_blue %>%
  mutate(
    blue_risk_M2 = Blue_occurrence_mean * mean_M2_trapdens
  )



#instead of working in calendar years, work in crab seasons
risk_hump_crab_season <- risk_hump %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_hump_crab_season_v2 <- risk_hump_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

risk_blue_crab_season <- risk_blue %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_blue_crab_season_v2 <- risk_blue_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))


#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
# grid5km_bbox <- st_bbox(grid.5km.lno %>%
#                           st_as_sf()
# )
bbox = c(-127,45,-120,49)


subset_data <- risk_hump_crab_season_v2 %>%
  filter(season == "2016-2017") %>%
  filter(month == "07") %>%
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() +
  geom_sf(data=sf::st_as_sf(subset_data),
          aes(fill=hump_risk_M2,
              col=hump_risk_M2
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) +
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
map_test


#-----------------------------------------------------------------------------------

risk_hump_crab_season_MaySep <- risk_hump_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_month
  summarise(
    hump_risk_M2_mean = mean(hump_risk_M2, na.rm=TRUE),
    hump_risk_M2_median = median(hump_risk_M2, na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(risk_hump_crab_season_MaySep)


ts_hump_risk_MaySep <- ggplot() +
  geom_point(data = risk_hump_crab_season_MaySep, aes(x = season, y = hump_risk_M2_mean, group = 1), size=4) +
  geom_line(data = risk_hump_crab_season_MaySep, aes(x = season, y = hump_risk_M2_mean, group = 1)) +
  #geom_point(data = risk_hump_crab_season_MaySep, aes(x = season, y = hump_risk_M2_median, group = 1), color = "darkred", size=4) +
  #geom_line(data = risk_hump_crab_season_MaySep, aes(x = season, y = hump_risk_M2_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_risk_MaySep



# plot May-Sep mean blue whale risk
risk_blue_crab_season_MaySep <- risk_blue_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_MaySep period
  summarise(
    blue_risk_M2_mean = mean(blue_risk_M2 , na.rm=TRUE),
    blue_risk_M2_median = median(blue_risk_M2 , na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(risk_blue_crab_season_MaySep)



ts_blue_risk_MaySep <- ggplot() +
  geom_point(data = risk_blue_crab_season_MaySep, aes(x = season, y = blue_risk_M2_mean, group = 1), size=4) +
  geom_line(data = risk_blue_crab_season_MaySep, aes(x = season, y = blue_risk_M2_mean, group = 1)) +
  #geom_point(data = risk_blue_crab_season_MaySep, aes(x = season, y = blue_risk_M2_median, group = 1), color = "darkred", size=4) +
  #geom_line(data = risk_blue_crab_season_MaySep, aes(x = season, y = blue_risk_M2_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_risk_MaySep


# # plot blues and humps together
# png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_by crab season_MaySep only_each seasons fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_hump_risk_MaySep,
#           ts_blue_risk_MaySep,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


































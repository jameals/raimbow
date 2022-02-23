## scenario test: all fishing years with summer reduction applied to 2019-2020 whale data
# tests robustness of regulations to changing fishing effort/distribution


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


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

# bring in whale data - use same code as 'Fishery centric look at risk'-script

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

#additional step - limit whale data to 2019-2020 season - this will be held constant in risk calc
#but only for May-Sep pre-regs vs 2020 comparison
x.whale_crab_season_May_Sep_2019_2020 <-  x.whale_crab_season_May_Sep %>% 
  filter(season == '2019-2020') %>% 
  #because whale data will be joined by month, remove season column
  select(-season)

#limit whale data to 2018-2019 season - this will be held constant in risk calc
#but only for Jul-Sep pre-regs vs 2019 comparison
x.whale_crab_season_Jul_Sep_2018_2019 <-  x.whale_crab_season_May_Sep %>% 
  filter(season == '2018-2019') %>% 
  #because whale data will be joined by month, remove season column
  select(-season)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


#fishing effort - all years with pot reduction
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step_REGS_IN_EVERY_SEASON.rds"

#don't impose regs to pre-seasons
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
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

#-----------------------------------------------------------------------------------
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
#because we will be holding whale data constant for all risk calc, only join with month and GRIDid (not by season)
study_area_whale_MaySep <- full_join(study_area_df_with_all_season_month_combos, 
                              x.whale_crab_season_May_Sep_2019_2020, 
                              by=c("GRID5KM_ID", "month")) 

study_area_whale_JulSep <- full_join(study_area_df_with_all_season_month_combos, 
                                     x.whale_crab_season_Jul_Sep_2018_2019, 
                                     by=c("GRID5KM_ID", "month")) 
#cases where season is NA are cases outside of study area -- all grid - season - month combos look to be there

#join fishing data to study area grid with whale data - fishing data will be joined by season and month
study_area_whale_fishing_MaySep <- left_join(study_area_whale_MaySep, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

study_area_whale_fishing_JulSep <- left_join(study_area_whale_JulSep, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))



#calculate risk  metric - whale data held constant, each season has regs
risk_whales_WA_MaySep <- study_area_whale_fishing_MaySep %>%
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

risk_whales_WA_JulSep <- study_area_whale_fishing_JulSep %>%
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
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


# ts plot: May-Sep risk to whales in study area -- if whale data was always the same, and if each season had regs

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE)
  )

ts_hump_risk_May_Sep_study_area <- ggplot() +
  geom_point(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
  geom_line(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1)) +
  ylab("Summed humpback Whale Risk May-Sep") + 
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
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE)
  )

ts_blue_risk_May_Sep_study_area <- ggplot() +
  geom_point(data = plot_subset, aes(x = season, y = Blue_risk_sum,group = 1), size=4) +
  geom_line(data = plot_subset, aes(x = season, y = Blue_risk_sum,group = 1)) +
  ylab("Summed blue Whale Risk May-Sep") + 
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


# plot blues and humps together and save
png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_constant_whale_data_every season has regs.png"), width = 14, height = 10, units = "in", res = 300)
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



### BOXPLOTS

#risk_whales_WA_MaySep 
#risk_whales_WA_JulSep

#HW
#Jul-Sep
box_fishing_actual_whale_2018_2019 <- risk_whales_WA_JulSep %>% 
  #filter to Jul-Sep
  filter(month %in% c('07','08','09')) %>% 
  #take out 2019-2020 season
  filter(season != '2019-2020') %>%
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

#Boxplots of risk - fishing actual, whale constant 2018-2019
box_hump_risk_Jul_Sep_constant_whale_2018_2019 <- ggplot() +
  geom_boxplot(data = box_fishing_actual_whale_2018_2019, aes(x = pre_post_reg, y = hump_risk)) +
  ylab("humpback Whale Risk Jul-Sep") + 
  scale_x_discrete(limits = rev) +
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
box_hump_risk_Jul_Sep_constant_whale_2018_2019


#May-Sep
box_fishing_actual_whale_2019_2020 <- risk_whales_WA_MaySep %>% 
  #already filtered to May-Sep
  #take out 2018-2019 season
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

#Boxplots of risk - fishing actual, whale constant 2018-2019
box_hump_risk_May_Sep_constant_whale_2019_2020 <- ggplot() +
  geom_boxplot(data = box_fishing_actual_whale_2019_2020, aes(x = pre_post_reg, y = hump_risk)) +
  ylab("humpback Whale Risk May-Sep") + 
  scale_x_discrete(limits = rev) +
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
box_hump_risk_May_Sep_constant_whale_2019_2020


#plot things together and save
png(paste0(path_figures, "/box_hump_risk_variable_fishing_constant_whale_JulSep.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(box_hump_risk_Jul_Sep_constant_whale_2018_2019,
          box_hump_risk_May_Sep_constant_whale_2019_2020,
          ncol=2,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


# just a quick test of how the ts plot would've looked if didn't epose 33% pot reduction in pre-seasons
# plot_subset <- box_fishing_actual_whale_2019_2020 %>% 
#   filter(study_area=='Y') %>% #restrict calculations to study area
#   group_by(season) %>%
#   summarise(
#     Humpback_risk_sum = sum(hump_risk, na.rm=TRUE)
#   )
# 
# ts_hump_risk_May_Sep_study_area <- ggplot() +
#   geom_point(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
#   geom_line(data = plot_subset, aes(x = season, y = Humpback_risk_sum,group = 1)) +
#   ylab("Summed humpback Whale Risk May-Sep") + 
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
# ts_hump_risk_May_Sep_study_area




#BW
#Jul-Sep
box_fishing_actual_whale_2018_2019 <- risk_whales_WA_JulSep %>% 
  #filter to Jul-Sep
  filter(month %in% c('07','08','09')) %>% 
  #take out 2019-2020 season
  filter(season != '2019-2020') %>%
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

#Boxplots of risk - fishing actual, whale constant 2018-2019
box_blue_risk_Jul_Sep_constant_whale_2018_2019 <- ggplot() +
  geom_boxplot(data = box_fishing_actual_whale_2018_2019, aes(x = pre_post_reg, y = blue_risk)) +
  ylab("blue Whale Risk Jul-Sep") + 
  scale_x_discrete(limits = rev) +
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
box_blue_risk_Jul_Sep_constant_whale_2018_2019


#May-Sep
box_fishing_actual_whale_2019_2020 <- risk_whales_WA_MaySep %>% 
  #already filtered to May-Sep
  #take out 2018-2019 season
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

#Boxplots of risk - fishing actual, whale constant 2018-2019
box_blue_risk_May_Sep_constant_whale_2019_2020 <- ggplot() +
  geom_boxplot(data = box_fishing_actual_whale_2019_2020, aes(x = pre_post_reg, y = blue_risk)) +
  ylab("blue Whale Risk May-Sep") + 
  scale_x_discrete(limits = rev) +
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
box_blue_risk_May_Sep_constant_whale_2019_2020


#plot things together and save
png(paste0(path_figures, "/box_blue_risk_variable_fishing_constant_whale_JulSep.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(box_blue_risk_Jul_Sep_constant_whale_2018_2019,
          box_blue_risk_May_Sep_constant_whale_2019_2020,
          ncol=2,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


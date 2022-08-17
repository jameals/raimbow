#Fishery centric look at risk (simple risk metric)

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
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area


# work in crabbing seasons instead of calendar years
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
bbox = c(-126,45.5,-121,49) 


subset_data <- risk_whales_WA_MaySep_normalized %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "06") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=blue_risk_norm,
              col=blue_risk_norm
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="",trans="sqrt",limits=c(0,0.79)) + # limits=c(0,0.71)) for HW
  scale_color_viridis(na.value=NA,option="D",name="",trans="sqrt",limits=c(0,0.79)) + # limits=c(0,0.71)) for HW
  ggtitle("BLue Whale Risk_2019-2020_06") +
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

# png(paste0(path_figures, "/BW_risk_2019-2020_06.png"), width = 14, height = 10, units = "in", res = 400)
# ggarrange(map_test,
#           ncol=1,
#           nrow=1,
#           #legend="top",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())

#-----------------------------------------------------------------------------------

##making plots:
# sum risk across grids, so that each month has 1 value
# but note that this way Jul-Sep 2018-2019 only has 3 values, 1 for each month

#sum across grids


##Humpback whales

##Jul-Sep
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

# SAVE FIGURE -- Supplementary Figure S5.1 
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/HW_risk_prePreg_vs_2019_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019,
#   ncol=1,
#   nrow=1
#   #legend="top",
#   #labels="auto",
#   #vjust=8,
#   #hjust=-0.2
# )
# invisible(dev.off())


##May-Sep
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


# SAVE FIGURE -- Supplementary Figure S5.1 
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/HW_risk_prePreg_vs_2020_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(box_hump_risk_MaySep_pre_reg_vs_2019_2020,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())

################################

##Blue whales


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

# SAVE FIGURE -- Supplementary Figure S5.1 
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/BW_risk_prePreg_vs_2019_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(box_blue_risk_Jul_Sep_pre_reg_vs_2018_2019,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


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

# SAVE FIGURE -- Supplementary Figure S5.1 
# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/BW_risk_prePreg_vs_2020_NORM_dots_and_mean_0s_fixed.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(box_blue_risk_MaySep_pre_reg_vs_2019_2020,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



###############################################

#percent change in risk -- Table 1, Figure 2

#when 1-month input file
percent_change_in_risk_JulSep <- plot_subset_2018_2019_box %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_hw_risk = mean(hump_risk), 
            mean_bw_risk = mean(blue_risk))
percent_change_in_risk_JulSep
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
##NORMALIZED:
#pre_post_reg mean_hw_risk mean_bw_risk
#2019-2020            2.19         2.60
#pre-reg             4.51          3.27
#HW:
(2.19-4.51)/4.51*100 #-51.44124
#BW:
(2.60-3.27)/3.27*100 #-20.4893
#--> no change after fixing normalization 0s



#######################################################################################

#GLM on simple overlap risk metric - Supplementary Table S5.1

#Jul-Sep pre-reg vs 2019
risk_whales_WA_MaySep_normalized <- risk_whales_WA_MaySep_normalized %>% 
  mutate(
  pre_post_reg = case_when(
    season == '2018-2019' & month %in% c('07', '08', '09') ~ "post-reg",  
    season == '2019-2020' & month %in% c('05', '06', '07', '08', '09') ~ "post-reg")) %>% 
  mutate(pre_post_reg = ifelse(is.na(pre_post_reg), 'pre-reg', pre_post_reg))


# Jul-Sep, all seasons, summed data across all grids in each month
risk_whales_WA_JulSep_summed <- risk_whales_WA_MaySep_normalized %>%  
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season != '2019-2020') %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk_norm, na.rm = T), 
            sum_blue_risk = sum(blue_risk_norm , na.rm = T)) 

hist(risk_whales_WA_JulSep_summed$sum_hump_risk)
hist(risk_whales_WA_JulSep_summed$sum_blue_risk)

library(ggcorrplot)
model.matrix(~0+., data=risk_whales_WA_JulSep_summed) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#retain month in glm even though it is not significant in Jul-Sep comparison
#in the Jul-Sep glm other family types don't improve results from basic gaussian model
#if use Gamma family, results don't make sense/match the plots
mod1_hump <- glm(sum_hump_risk ~ pre_post_reg + month,
                 family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit)
summary(mod1_hump)
hist(mod1_hump$residuals)

scatter.smooth(fitted(mod1_hump), residuals(mod1_hump, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

plot(mod1_hump)


#Blue whale
mod1_blue <- glm(sum_blue_risk ~ pre_post_reg + month,
                 family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit)
summary(mod1_blue)
hist(mod1_blue$residuals)

plot(mod1_blue)

scatter.smooth(fitted(mod1_hump), residuals(mod1_blue, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

library(car)
qqPlot(mod1_blue$residuals)



#May-Sep pre-reg vs 2020

#separate glm using may-sep data for 2014-18 and 2019-20 data only.
risk_whales_WA_MaySep_summed <- risk_whales_WA_MaySep_normalized %>% 
  filter(season != '2018-2019') %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk_norm, na.rm = T), 
            sum_blue_risk = sum(blue_risk_norm, na.rm = T)) 


hist(risk_whales_WA_MaySep_summed$sum_hump_risk)
hist(risk_whales_WA_MaySep_summed$sum_blue_risk)

model.matrix(~0+., data=risk_whales_WA_MaySep_summed) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod2_hump <- glm(sum_hump_risk ~ pre_post_reg + month,
                 family=gaussian, data=risk_whales_WA_MaySep_summed, na.action = na.omit) 
summary(mod2_hump) #interpreting significant intercept: It means you have enough evidence to say that the intercept isn't 0.
#We typically don't care if the intercept is significant or not. It's important to have in the model but unless there is a good reason to typically you don't interpret the significance test of the intercept
hist(mod2_hump$residuals)
plot(mod2_hump)

scatter.smooth(fitted(mod2_hump), residuals(mod2_hump, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

qqPlot(mod2_hump$residuals)


#Blue whales
mod2_blue <- glm(sum_blue_risk ~ pre_post_reg +month,
                 family=gaussian, data=risk_whales_WA_MaySep_summed, na.action = na.omit) 
summary(mod2_blue)
hist(mod2_blue$residuals)

scatter.smooth(fitted(mod2_blue), residuals(mod2_blue, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

plot(mod2_blue)
qqPlot(mod2_blue$residuals)


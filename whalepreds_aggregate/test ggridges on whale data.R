#test ggridges on whale data

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

#whale density/occurrence STUDY AREA during Jul-Sep or May-Sep


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

#----------------------------------------------------------------------------------------------------
#companion plots pre-reg vs reg years - companion plots for risk


#all grid_year_month as rows in df
study_area_hw_pre_reg_vs_2018_2019 <- study_area_whale %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
  filter(study_area=='Y') %>% #need to filter to be only study area grids
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))


#companion plot May-Sep
study_area_hw_pre_reg_vs_2019_2020 <- study_area_whale %>% 
  #take out 2018-2019 season
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
  filter(study_area=='Y') %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))





# #warning message: removed rows containing non-finite values is due to NAs
# ggplot(study_area_hw_pre_reg_vs_2018_2019, aes(x = Humpback_dens_mean, y = pre_post_reg, height = ..density..)) + 
#   geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   #coord_cartesian(clip = "off") +
#   xlab("Humpback whale density (Jul-Sep)") +
#   theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.8, .5),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 0),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left",
#         axis.title.y=element_blank()
#   )
# 
# 
# ggplot(study_area_hw_pre_reg_vs_2019_2020, aes(x = Humpback_dens_mean, y = pre_post_reg, height = ..density..)) + 
#   geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   #coord_cartesian(clip = "off") +
#   xlab("Humpback whale density (May-Sep)") +
#   theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.8, .5),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 0),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left",
#         axis.title.y=element_blank()
#   )
# 
# 
# ggplot(study_area_hw_pre_reg_vs_2018_2019, aes(x = Blue_occurrence_mean, y = pre_post_reg, height = ..density..)) + 
#   geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   #coord_cartesian(clip = "off") +
#   xlab("Blue whale occurrence (Jul-Sep)") +
#   theme_ridges(grid = TRUE, center_axis_labels = TRUE)+
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.8, .5),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 0),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left",
#         axis.title.y=element_blank()
#   )
# 
# ggplot(study_area_hw_pre_reg_vs_2019_2020, aes(x = Blue_occurrence_mean, y = pre_post_reg, height = ..density..)) + 
#   geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   #coord_cartesian(clip = "off") +
#   xlab("Blue whale occurrence  (May-Sep)") +
#   theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.8, .5),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 0),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=20),
#         strip.background = element_blank(),
#         strip.placement = "left",
#         axis.title.y=element_blank()
#   )






#ggridges plot with quantiles

hw_density_ridges_quantiles_JulSep <- ggplot(study_area_hw_pre_reg_vs_2018_2019, aes(x = Humpback_dens_mean, y = pre_post_reg, fill = stat(quantile))) +
  stat_density_ridges(#quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 0.95) +
  scale_fill_manual(name = "Quantile", values = c("#edf8e9", "#bae4b3", "#74c476", "#31a354"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #scale_x_continuous(limits = c(0, 0.065), expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0), labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  xlab("Humpback whale density") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = "none",
        #legend.position = c(.8, .5),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        axis.title.y=element_blank(),
  )
hw_density_ridges_quantiles_JulSep


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
png(paste0(path_figures, "/hump_whale_occurrence_in_study_area__JulSep_pre_reg_vs_2019.png"), width = 20, height = 15, units = "in", res = 400)
ggarrange(hw_density_ridges_quantiles_JulSep,
          ncol=1,
          nrow=1
          #legend="right",
          #labels="auto",
          #vjust=8,
          #hjust=0
)
invisible(dev.off())






hw_density_ridges_quantiles_MaySep <- ggplot(study_area_hw_pre_reg_vs_2019_2020, aes(x = Humpback_dens_mean, y = pre_post_reg, fill = stat(quantile))) +
  stat_density_ridges(#quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 0.95) +
  scale_fill_manual(name = "Quantile", values = c("#edf8e9", "#bae4b3", "#74c476", "#31a354"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #scale_x_continuous(limits = c(0, 0.065), expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0), labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  xlab("Humpback whale density") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.75, .9),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        axis.title.y=element_blank(),
  )
hw_density_ridges_quantiles_MaySep



path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
png(paste0(path_figures, "/hump_whale_occurrence_in_study_area__MaySep_pre_reg_vs_2020.png"), width = 20, height = 15, units = "in", res = 400)
ggarrange(hw_density_ridges_quantiles_MaySep,
          ncol=1,
          nrow=1
          #legend="right",
          #labels="auto",
          #vjust=8,
          #hjust=0
)
invisible(dev.off())








# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
# 
# png(paste0(path_figures, "/hump_whale_occurrence_in_study_area_pre_vs_post_regs.png"), width = 35, height = 15, units = "in", res = 400)
# ggarrange(hw_density_ridges_quantiles_JulSep,
#           hw_density_ridges_quantiles_MaySep,
#           ncol=2,
#           nrow=1,
#           #legend="right",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



















bw_density_ridges_quantiles_JulSep <- ggplot(study_area_hw_pre_reg_vs_2018_2019, aes(x = Blue_occurrence_mean, y = pre_post_reg, fill = stat(quantile))) +
  stat_density_ridges(#quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 0.95) +
  scale_fill_manual(name = "Quantile", values = c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #scale_x_continuous(limits = c(0.01, 0.86), expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0), labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  xlab("Blue whale probability of occurrence") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = "none",
        #legend.position = c(.8, .5),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        axis.title.y=element_blank(),
  )
bw_density_ridges_quantiles_JulSep

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
png(paste0(path_figures, "/blue_whale_occurrence_in_study_area__JulSep_pre_reg_vs_2019.png"), width = 20, height = 15, units = "in", res = 400)
ggarrange(bw_density_ridges_quantiles_JulSep,
          ncol=1,
          nrow=1
          #legend="right",
          #labels="auto",
          #vjust=8,
          #hjust=0
)
invisible(dev.off())



bw_density_ridges_quantiles_MaySep <- ggplot(study_area_hw_pre_reg_vs_2019_2020, aes(x = Blue_occurrence_mean, y = pre_post_reg, fill = stat(quantile))) +
  stat_density_ridges(#quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 0.95) +
  scale_fill_manual(name = "Quantile", values = c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #scale_x_continuous(limits = c(0.01, 0.86), expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0), labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  xlab("Blue whale probability of occurrence") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = c(.85, .9),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        axis.title.y=element_blank(),
  )
bw_density_ridges_quantiles_MaySep


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
png(paste0(path_figures, "/blue_whale_occurrence_in_study_area__MaySep_pre_reg_vs_2020.png"), width = 20, height = 15, units = "in", res = 400)
ggarrange(bw_density_ridges_quantiles_MaySep,
          ncol=1,
          nrow=1
          #legend="right",
          #labels="auto",
          #vjust=8,
          #hjust=0
)
invisible(dev.off())



path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub

png(paste0(path_figures, "/blue_whale_occurrence_in_study_area_pre_vs_post_regs.png"), width = 35, height = 15, units = "in", res = 400)
ggarrange(bw_density_ridges_quantiles_JulSep,
          bw_density_ridges_quantiles_MaySep,
          ncol=2,
          nrow=1,
          #legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




#############
##statistical tests
#study_area_hw_pre_reg_vs_2018_2019 
#study_area_hw_pre_reg_vs_2019_2020




##Jul-Sep
library(WRS2)
qcomhd_JulSep_HW <- qcomhd(Humpback_dens_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2018_2019, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
qcomhd_JulSep_BW <- qcomhd(Blue_occurrence_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2018_2019, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
#even BW is significant, 2019 higher in other quantiles except for 75-100%
#which fits as top 15 BW values are pre-reg
# 
# #need to separate df fro K-S test
# JulSep_pre_reg <- study_area_hw_pre_reg_vs_2018_2019 %>% 
#   filter(pre_post_reg =='pre-reg')
# JulSep_2018_2019 <- study_area_hw_pre_reg_vs_2018_2019 %>% 
#   filter(pre_post_reg =='2018-2019')
# 
# kstest_JulSep_HW <- ks.test(JulSep_pre_reg$Humpback_dens_mean, JulSep_2018_2019$Humpback_dens_mean)
# kstest_JulSep_BW <- ks.test(JulSep_pre_reg$Blue_occurrence_mean, JulSep_2018_2019$Blue_occurrence_mean)
# #even BW is significant
# 
# wilcox_test_JulSep_HW <- wilcox.test(Humpback_dens_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2018_2019, exact = FALSE)
# wilcox_test_JulSep_BW <- wilcox.test(Blue_occurrence_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2018_2019, exact = FALSE)
# #even BW is significant


##May-Sep
library(WRS2) #- This test provides a more detailed understanding of where and how distributions differ. 
qcomhd_MaySep_HW <- qcomhd(Humpback_dens_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2019_2020, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
qcomhd_MaySep_BW <- qcomhd(Blue_occurrence_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2019_2020, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
#significantly diff, overall higher in 2020, tho top values in pre-reg

# #need to separate df fro K-S test
# MaySep_pre_reg <- study_area_hw_pre_reg_vs_2019_2020 %>% 
#   filter(pre_post_reg =='pre-reg')
# MaySep_2019_2020 <- study_area_hw_pre_reg_vs_2019_2020 %>% 
#   filter(pre_post_reg =='2019-2020')
# 
# kstest_MaySep_HW <- ks.test(MaySep_pre_reg$Humpback_dens_mean, MaySep_2019_2020$Humpback_dens_mean)
# kstest_MaySep_BW <- ks.test(MaySep_pre_reg$Blue_occurrence_mean, MaySep_2019_2020$Blue_occurrence_mean)
# #both are significant
# 
# wilcox_test_MaySep_HW <- wilcox.test(Humpback_dens_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2019_2020, exact = FALSE)
# wilcox_test_MaySep_BW <- wilcox.test(Blue_occurrence_mean ~ pre_post_reg, data = study_area_hw_pre_reg_vs_2019_2020, exact = FALSE)
# #both are significant
























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


subset_data <- study_area_hw %>% 
  filter(study_area=='Y') %>% 
  filter(season == "2019-2020") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=Humpback_dens_mean,
              col=Humpback_dens_mean
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
## scenario test 2: apply all whale years to 2019-2020 fishing data
# tests robustness of regulations to changing whales
#hold fishing data constant as Jul-Sep 2019 and May-Sep 2020, vary whale data


#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(car)

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

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#fishing effort

# bring in fishing effort - only using 2018-19 and 2019-20 fishing effort data -hold fishing constant

path.fish_WA_regs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA_regs <- readRDS(path.fish_WA_regs)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA_regs <- x.fish_WA_regs %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2_regs <- x.fish_WA_regs %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep_regs <- x.fish_WA2_regs %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))

#additional step - limit fishing data to 2019-2020 season - this will be held constant in risk calc
x.fish_WA_MaySep_regs_2019_2020 <-  x.fish_WA_MaySep_regs %>% 
  filter(season == '2019-2020') #%>% 
#because whale data will be joined by month, remove season column
#select(-season)

x.fish_WA_MaySep_regs_2018_2019 <-  x.fish_WA_MaySep_regs %>% 
  filter(season == '2018-2019')



#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Calculate risk using study area and sum
#calculate risk separately for REGS and NO REGS

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
#because we will vary whale data  for all risk calc,  join with season, month and GRIDid 
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, 
                              x.whale_crab_season_May_Sep, 
                              by=c("season", "month", "GRID5KM_ID")) 



#join fishing data to study area grid with whale data - seasons with regs
#because we will hold fishing data constant, joined by month and grid (not by season)
study_area_whale_fishing_with_regs <- left_join(study_area_whale, x.fish_WA_MaySep_regs_2019_2020, by=c("GRID5KM_ID", "month")) %>% 
  select(-season.y) %>% 
  rename(season = season.x) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))
#make sure regs column doesn't have NAs
study_area_whale_fishing_with_regs$regs <- c("Y")

study_area_whale_fishing_with_regs_2018_2019 <- left_join(study_area_whale, x.fish_WA_MaySep_regs_2018_2019, by=c("GRID5KM_ID", "month")) %>% 
  select(-season.y) %>% 
  rename(season = season.x) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))





#calculate risk  metric - fishing data held constant - seasons with regs
risk_whales_WA_MaySep_study_area_with_regs <- study_area_whale_fishing_with_regs %>%
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
  ) 

risk_whales_WA_MaySep_study_area_with_regs_2018_2019 <- study_area_whale_fishing_with_regs_2018_2019 %>%
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
  ) 




#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
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


subset_data <- risk_whales_WA_MaySep_study_area_with_regs %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "07") %>% 
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

#--------------------------------------------------------------------------

# ts plot: May-Sep risk to whales in study area -- if fishing data was always the same
#with and without regulations


plot_subset_with_regs <- risk_whales_WA_MaySep_study_area_with_regs %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  filter(season != '2018-2019') %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(
    #use summed approach
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE),
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE))

plot_subset_with_regs_2018_2019 <- risk_whales_WA_MaySep_study_area_with_regs_2018_2019 %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season != '2019-2020') %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(
    #use summed approach
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE),
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE))


violin_hump_risk_MaySep_constant_fishing <- ggplot() +
  geom_violin(data = plot_subset_with_regs, aes(x = pre_post_reg, y = Humpback_risk_sum), lwd=1) +
  #geom_dotplot(data = plot_subset_with_regs, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  ylab("Summed Humpback Whale Risk May-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
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
violin_hump_risk_MaySep_constant_fishing

violin_hump_risk_JulSep_constant_fishing <- ggplot() +
  geom_violin(data = plot_subset_with_regs_2018_2019, aes(x = pre_post_reg, y = Humpback_risk_sum), lwd=1) +
  #geom_dotplot(data = plot_subset_with_regs_2018_2019, aes(x = pre_post_reg, y = hump_risk), binaxis='y', stackdir='center', dotsize=0.6) +
  ylab("Summed Humpback Whale Risk Jul-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
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
violin_hump_risk_JulSep_constant_fishing



#Jul-Sep
mod1_hump_JulSep <- glm(Humpback_risk_sum ~ month + pre_post_reg,
                 family=gaussian, data=plot_subset_with_regs_2018_2019, na.action = na.omit) 
summary(mod1_hump_JulSep)
plot(mod1_hump_JulSep)
qqPlot(mod1_hump_JulSep$residuals)
wilcox.test(Humpback_risk_sum ~ pre_post_reg, data = plot_subset_with_regs_2018_2019)


#May-Sep
mod1_hump <- glm(Humpback_risk_sum ~ month + pre_post_reg,
                 family=gaussian, data=plot_subset_with_regs, na.action = na.omit) 
summary(mod1_hump)
plot(mod1_hump)
qqPlot(mod1_hump$residuals)
wilcox.test(Humpback_risk_sum ~ pre_post_reg, data = plot_subset_with_regs)



violin_blue_risk_MaySep_constant_fishing <- ggplot() +
  geom_violin(data = plot_subset_with_regs, aes(x = pre_post_reg, y = Blue_risk_sum), lwd=1) +
  #geom_dotplot(data = plot_subset_with_regs, aes(x = pre_post_reg, y = Blue_risk_sum), binaxis='y', stackdir='center', dotsize=0.6) +
  ylab("Summed Blue Whale Risk May-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
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
violin_blue_risk_MaySep_constant_fishing

violin_blue_risk_JulSep_constant_fishing <- ggplot() +
  geom_violin(data = plot_subset_with_regs_2018_2019, aes(x = pre_post_reg, y = Blue_risk_sum), lwd=1) +
  #geom_dotplot(data = plot_subset_with_regs_2018_2019, aes(x = pre_post_reg, y = Blue_risk_sum), binaxis='y', stackdir='center', dotsize=0.6) +
  ylab("Summed Blue Whale Risk Jul-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
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
violin_blue_risk_JulSep_constant_fishing


#Jul-Sep
mod1_blue_JulSep <- glm(Blue_risk_sum ~ month + pre_post_reg,
                        family=gaussian, data=plot_subset_with_regs_2018_2019, na.action = na.omit) 
summary(mod1_blue_JulSep)
plot(mod1_blue_JulSep)
qqPlot(mod1_blue_JulSep$residuals)
wilcox.test(Blue_risk_sum ~ pre_post_reg, data = plot_subset_with_regs_2018_2019)


#May-Sep
mod1_blue_MaySep <- glm(Blue_risk_sum ~ month + pre_post_reg,
                 family=gaussian, data=plot_subset_with_regs, na.action = na.omit) 
summary(mod1_blue_MaySep)
plot(mod1_blue_MaySep)
qqPlot(mod1_blue_MaySep$residuals)
wilcox.test(Blue_risk_sum ~ pre_post_reg, data = plot_subset_with_regs)



#plot blues and humps together and save
png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_constant fishing data_with regs.png"), width = 14, height = 10, units = "in", res = 300)
png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_constant fishing data_with or without regs.png"), width = 14, height = 10, units = "in", res = 300)
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

#--------------


## get a % change from pre-reg average to post reg
## run GLM?







#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------






































































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

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#fishing effort

# bring in fishing effort WITH and WITHOUT regs - only focus on 2019-2020 season

#WITH REGULATIONS
path.fish_WA_regs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA_regs <- readRDS(path.fish_WA_regs)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA_regs <- x.fish_WA_regs %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2_regs <- x.fish_WA_regs %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep_regs <- x.fish_WA2_regs %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))

#additional step - limit fishing data to 2019-2020 season - this will be held constant in risk calc
x.fish_WA_MaySep_regs_2019_2020 <-  x.fish_WA_MaySep_regs %>% 
  filter(season == '2019-2020') #%>% 
  #because whale data will be joined by month, remove season column
  #select(-season)

x.fish_WA_MaySep_regs_2018_2019 <-  x.fish_WA_MaySep_regs %>% 
  filter(season == '2018-2019')


#WITHOUT REGULATIONS
path.fish_WA_NOregs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step_NO_REGS.rds"
x.fish_WA_NOregs <- readRDS(path.fish_WA_NOregs)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA_NOregs <- x.fish_WA_NOregs %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2_NOregs <- x.fish_WA_NOregs %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep_NOregs <- x.fish_WA2_NOregs %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))

#additional step - limit fishing data to 2019-2020 season - this will be held constant in risk calc
x.fish_WA2_MaySep_NOregs_2019_2020 <-  x.fish_WA_MaySep_NOregs %>% 
  filter(season == '2019-2020') #%>% 
#because whale data will be joined by month, remove season column
#select(-season)




##TURNS OUT WE FIRST HAVE TO CALC RISK BEFORE JOINING, OTHERWISE REGS COLUMN MIGHT BE NA
#add a column to denote regs or not
x.fish_WA_MaySep_regs_2019_2020$regs <- c("Y")
x.fish_WA2_MaySep_NOregs_2019_2020$regs <- c("N")


#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Calculate risk using study area and sum
#calculate risk separately for REGS and NO REGS

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
#because we will vary whale data  for all risk calc,  join with season, month and GRIDid 
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, 
                              x.whale_crab_season_May_Sep, 
                              by=c("season", "month", "GRID5KM_ID")) 



#join fishing data to study area grid with whale data - seasons with regs
#because we will hold fishing data constant, joined by month and grid (not by season)
study_area_whale_fishing_with_regs <- left_join(study_area_whale, x.fish_WA_MaySep_regs_2019_2020, by=c("GRID5KM_ID", "month")) %>% 
  select(-season.y) %>% 
  rename(season = season.x) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))
#make sure regs column doesn't have NAs
study_area_whale_fishing_with_regs$regs <- c("Y")

study_area_whale_fishing_with_regs_2018_2019 <- left_join(study_area_whale, x.fish_WA_MaySep_regs_2018_2019, by=c("GRID5KM_ID", "month")) %>% 
  select(-season.y) %>% 
  rename(season = season.x) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#join fishing data to study area grid with whale data - seasons without regs
#because we will hold fishing data constant, joined by month and grid (not by season)
study_area_whale_fishing_without_regs <- left_join(study_area_whale, x.fish_WA2_MaySep_NOregs_2019_2020, by=c("GRID5KM_ID", "month")) %>% 
  select(-season.y) %>% 
  rename(season = season.x) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))
#make sure regs column doesn't have NAs
study_area_whale_fishing_without_regs$regs <- c("N")



#calculate risk  metric - fishing data held constant - seasons with regs
risk_whales_WA_MaySep_study_area_with_regs <- study_area_whale_fishing_with_regs %>%
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
  ) 

risk_whales_WA_MaySep_study_area_with_regs_2018_2019 <- study_area_whale_fishing_with_regs_2018_2019 %>%
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
  ) 

#calculate risk  metric - fishing data held constant - seasons without regs
risk_whales_WA_MaySep_study_area_without_regs <- study_area_whale_fishing_without_regs %>%
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
  ) 


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
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


subset_data <- risk_whales_WA_MaySep_study_area_with_regs %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "07") %>% 
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

#--------------------------------------------------------------------------

# ts plot: May-Sep risk to whales in study area -- if fishing data was always the same
#with and without regulations

plot_subset_with_regs <- risk_whales_WA_MaySep_study_area_with_regs %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    #use summed approach
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE),
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE))

plot_subset_with_regs_2018_2019 <- risk_whales_WA_MaySep_study_area_with_regs_2018_2019 %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    #use summed approach
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE),
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE))

plot_subset_without_regs <- risk_whales_WA_MaySep_study_area_without_regs %>% 
  filter(study_area=='Y') %>% #restrict calculations to study area
  group_by(season) %>%
  summarise(
    #use summed approach
    Humpback_risk_sum = sum(hump_risk, na.rm=TRUE),
    Blue_risk_sum = sum(blue_risk, na.rm=TRUE))


ts_hump_risk_May_Sep_study_area <- ggplot() +
  #with regs
  geom_point(data = plot_subset_with_regs, aes(x = season, y = Humpback_risk_sum,group = 1), size=4) +
  geom_line(data = plot_subset_with_regs, aes(x = season, y = Humpback_risk_sum,group = 1)) +
  #2018-2019
  geom_point(data = plot_subset_with_regs_2018_2019, aes(x = season, y = Humpback_risk_sum,group = 1),colour="gray", size=4) +
  geom_line(data = plot_subset_with_regs_2018_2019, aes(x = season, y = Humpback_risk_sum,group = 1),colour="gray") +
  #without regs
  #geom_point(data = plot_subset_without_regs, aes(x = season, y = Humpback_risk_sum,group = 1), colour="red", size=4) +
  #geom_line(data = plot_subset_without_regs, aes(x = season, y = Humpback_risk_sum,group = 1), colour="red") +
  
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

ts_blue_risk_May_Sep_study_area <- ggplot() +
  #with regs
  geom_point(data = plot_subset_with_regs, aes(x = season, y = Blue_risk_sum,group = 1), size=4) +
  geom_line(data = plot_subset_with_regs, aes(x = season, y = Blue_risk_sum,group = 1)) +
  #2018-2019
  geom_point(data = plot_subset_with_regs_2018_2019, aes(x = season, y = Blue_risk_sum,group = 1),colour="gray", size=4) +
  geom_line(data = plot_subset_with_regs_2018_2019, aes(x = season, y = Blue_risk_sum,group = 1),colour="gray") +
  #without regs
  #geom_point(data = plot_subset_without_regs, aes(x = season, y = Blue_risk_sum,group = 1), colour="red", size=4) +
  #geom_line(data = plot_subset_without_regs, aes(x = season, y = Blue_risk_sum,group = 1), colour="red") +
  
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
ts_blue_risk_May_Sep_study_area


#plot blues and humps together and save
png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_constant fishing data_with regs.png"), width = 14, height = 10, units = "in", res = 300)
png(paste0(path_figures, "/ts_sum_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep_constant fishing data_with or without regs.png"), width = 14, height = 10, units = "in", res = 300)
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

#--------------


#HW
#season      WITH_regs          WITHOUT_regs      % change
# 2013-2014   51.19867            77.39991        51.2
# 2014-2015   44.97481            67.99507        51.2  
# 2015-2016   52.00878            78.62314        51.2
# 2016-2017   54.46291            82.33617        51.2
# 2017-2018   52.35124            79.14052        51.2
# 2018-2019   33.06973            49.98954        51.2
# 2019-2020   45.75299            69.16616        51.2



#BW
#season        WITH_regs        WITHOUT_regs     % change   
# 2013-2014      556.4575         841.2203        51.2
# 2014-2015      593.8966         897.8062        51.2
# 2015-2016      634.6979         959.4581        51.2  
# 2016-2017      623.9529         943.2148        51.2
# 2017-2018      673.4007         1017.9554       51.2  
# 2018-2019      655.3008         990.6801        51.2  
# 2019-2020      749.2442         1132.5630       

        

#fishing data held constant 2019-2020
#season      Humpback_risk_sum     Blue_risk_sum
# 2013-2014     51.19867            556.4575
# 2014-2015     44.97481            593.8966
# 2015-2016     52.00878            634.6979
# 2016-2017     54.46291            623.9529
# 2017-2018     52.35124            673.4007
# 2018-2019     33.06973            655.3008
# 2019-2020     45.75299            749.2442          

#fishing data held constant 2018-2019
#season     Humpback_risk_sum     Blue_risk_sum
# 2013-2014     84.14357            742.8685
# 2014-2015     71.82169            798.2301
# 2015-2016     84.51042            861.7217
# 2016-2017     87.19612            843.5239
# 2017-2018     86.64246            916.6605
# 2018-2019     58.81001            889.2715
# 2019-2020     81.37870            1052.7046




### BOXPOTS
#risk_whales_WA_MaySep_study_area_with_regs = 2019-2020 fishing data applied to all whale years
#risk_whales_WA_MaySep_study_area_with_regs_2018_2019 = 2018-2019 fishing data applied to all whale years

#read in data where risk is actual whale and actual fishing of each season
risk_whales_WA_MaySep_actual_fishing_actual_whale <- read_rds(here::here('wdfw','data','risk_whales_WA_MaySep_actual_fishing_actual_whale.rds'))

#this was just to check that the 'actual' fishing and whale df matched the fishing held constatnt df - it did
# risk_whales_WA_MaySep_actual_fishing_actual_whale_2019_2020 <- risk_whales_WA_MaySep_actual_fishing_actual_whale %>% 
#   filter(season == '2019-2020') %>% 
#   # add extra column to differentiate in boxplot
#   mutate(pre_post_reg = "2019-2020-actual") %>% 
#   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

 risk_whales_WA_MaySep_actual_fishing_actual_whale_pre_reg <- risk_whales_WA_MaySep_actual_fishing_actual_whale %>% 
   #select the pre-reg seasons from actual whale and actual fishing data
   filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
   # add extra column to differentiate in boxplot
   mutate(pre_post_reg = "pre_reg-actual_whale_actual_fishing") %>% 
   filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered



box_2019_2020_fishing_to_all_wh_years <- risk_whales_WA_MaySep_study_area_with_regs %>% 
  #already filtered to May-Sep
  #take out 2018-2019 season
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020_whale_and_fishing", "pre-reg_actual_whale_2020_fishing")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered
  
#Boxplots of risk based on pre-reg whale years and post-reg fishing years
box_hump_risk_May_Sep_constant_fishing_2019_2020 <- ggplot() +
  #pre-reg whale data and 2020 fishing
  geom_boxplot(data = box_2019_2020_fishing_to_all_wh_years, aes(x = pre_post_reg, y = hump_risk)) +
  
  #actual whale and actual fishing - filtered to pre-reg
  geom_boxplot(data = risk_whales_WA_MaySep_actual_fishing_actual_whale_pre_reg, aes(x = pre_post_reg, y = hump_risk)) +
  
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
box_hump_risk_May_Sep_constant_fishing_2019_2020


box_hump_risk_May_Sep_constant_fishing_2019_2020_v2 <- ggplot() +
  geom_boxplot(data = box_2019_2020_fishing_to_all_wh_years, aes(x = season, y = hump_risk)) +
  ylab("humpback Whale Risk May-Sep") + 
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
box_hump_risk_May_Sep_constant_fishing_2019_2020_v2


box_blue_risk_May_Sep_constant_fishing_2019_2020 <- ggplot() +
  geom_boxplot(data = box_2019_2020_fishing_to_all_wh_years, aes(x = pre_post_reg, y = blue_risk)) +
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
box_blue_risk_May_Sep_constant_fishing_2019_2020





##pre-reg vs 2018-2019 Jul-Sep
#risk_whales_WA_MaySep_study_area_with_regs_2018_2019 = 2018-2019 fishing data applied to all whale years

#read in data where risk is actual whale and actual fishing of each season
risk_whales_WA_MaySep_actual_fishing_actual_whale <- read_rds(here::here('wdfw','data','risk_whales_WA_MaySep_actual_fishing_actual_whale.rds'))

risk_whales_WA_MaySep_actual_fishing_actual_whale_pre_reg <- risk_whales_WA_MaySep_actual_fishing_actual_whale %>% 
  #select the pre-reg seasons from actual whale and actual fishing data
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  #for pre-reg vs 2019 comparison only looking at Jul-Sep
  filter(month %in% c('07','08','09')) %>% 
  # add extra column to differentiate in boxplot
  mutate(pre_post_reg = "pre_reg-actual_whale_actual_fishing") %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered


box_2018_2019_fishing_to_all_wh_years <- risk_whales_WA_MaySep_study_area_with_regs_2018_2019 %>% 
  #filter to Jul-Sep
  filter(month %in% c('07','08','09')) %>% 
  #take out 2019-2020 season
  filter(season != '2019-2020') %>%
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019_whale_and_fishing", "pre-reg_actual_whale_2019_fishing")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg)) %>% 
  filter(!is.na(mean_M2_trapdens)) #this will effectively mean that only fishing footprint is considered

#Boxplots of risk based on pre-reg whale years and post-reg fishing years
box_hump_risk_Jul_Sep_constant_fishing_2018_2019 <- ggplot() +
  #pre-reg whale data and 2019 fishing
  geom_boxplot(data = box_2018_2019_fishing_to_all_wh_years, aes(x = pre_post_reg, y = hump_risk)) +
  
  #actual whale and actual fishing - filtered to pre-reg
  geom_boxplot(data = risk_whales_WA_MaySep_actual_fishing_actual_whale_pre_reg, aes(x = pre_post_reg, y = hump_risk)) +
  
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
box_hump_risk_Jul_Sep_constant_fishing_2018_2019


box_blue_risk_Jul_Sep_constant_fishing_2018_2019 <- ggplot() +
  geom_boxplot(data = box_2018_2019_fishing_to_all_wh_years, aes(x = pre_post_reg, y = blue_risk)) +
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
box_blue_risk_Jul_Sep_constant_fishing_2018_2019







#plot things together and save
png(paste0(path_figures, "/box_blue_risk_variable_whale_constant fishing data.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(box_blue_risk_Jul_Sep_constant_fishing_2018_2019,
          box_blue_risk_May_Sep_constant_fishing_2019_2020,
          ncol=2,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

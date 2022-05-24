#Investigate depth distribution of whale and fishing data

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

# set some paths
# Jameal
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# whale outputs overlayed on 5km grid (i.e., not subset to DCRB fishing cells)
# Jameal
path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"

# Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


# where to put outputs
# Jameal
path_figures <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOWT/raimbow/whalepreds_aggregate/figures"

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

#hw output 2009-July 2019
# x.hump <- readRDS(path.hump) %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# glimpse(x.hump)

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


# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)

#-----------------------------------------------------------------------------------

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#-----------------------------------------------------------------------------------

#map depth data and grids with fishing effort - depth data not available for some grids in bays

#this is not restricted to WA waters:
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) 
### Get grid cells with non-NA values for all
grid.studyarea.id_WA <- sort(unique(x.fish_WA$GRID5KM_ID))
grid.5km.fish_WA <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)

path.fish_OR <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/OR/OR_adj_summtraps_SpatialFlag_filtered_2007_2018.rds"
x.fish_OR <- readRDS(path.fish_OR) #this works
### Get grid cells with non-NA values for all, and save that of fishing data
grid.studyarea.id_OR <- sort(unique(x.fish_OR$GRID5KM_ID))
grid.5km.fish_OR <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_OR)


bbox = c(-127,41,-120,49) 

map_depth <-  ggplot() + 
  geom_sf(data = grid.key, aes(fill = depth, col = depth)) +     
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  geom_sf(data=grid.5km.fish_WA,col='black',fill=NA,alpha=0.8) + 
  geom_sf(data=grid.5km.fish_OR,col='black',fill=NA,alpha=0.8) + 
  ggtitle("depth grid check") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
map_depth
#map looks bit different when redoing in Nov 2021 than it did in Sep 2021: the most northern extent of WA fishing data
#is missing some grids --> This is probably due to the step of removing too long stringlines

#png(paste0(path_figures, "/depth_grid_check_fishing data_20211110.png"), width = 14, height = 10, units = "in", res = 300)
#ggarrange(map_depth)
#invisible(dev.off())


#-----------------------------------------------------------

#look at the overlap between whale layers and depth data

grid.studyarea.id_hump <- sort(unique(x.hump_2009_2020$GRID5KM_ID))
grid.5km.hump <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_hump)

grid.studyarea.id_blue <- sort(unique(x.blue.all$GRID5KM_ID))
grid.5km.blue <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_blue)

map_depth_hw <-  ggplot() + 
  geom_sf(data = grid.key, aes(fill = depth, col = depth)) +     
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  geom_sf(data=grid.5km.hump,col='black',fill=NA,alpha=0.8) + 
  ggtitle("depth grid check_hw") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
map_depth_hw

map_depth_bw <-  ggplot() + 
  geom_sf(data = grid.key, aes(fill = depth, col = depth)) +     
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  geom_sf(data=grid.5km.blue,col='black',fill=NA,alpha=0.8) + 
  ggtitle("depth grid check_bw") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
map_depth_bw

# map blues and humps together (and save)
# png(paste0(path_figures, "/map_blue_hump_2009_2020_overlap with depth data_20211110.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_depth_hw,
          map_depth_bw,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
# invisible(dev.off())



#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#look at depth dist of whales (onshore vs offshore) across years

#clip things to exclude CA
grid.key_WA_OR <- grid.key %>%  filter(LATITUDE > 40.7)

#just mapping the depth grid data
test_map <-  ggplot() + 
  geom_sf(data = grid.key_WA_OR, aes(fill = depth, col = depth)) +     
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  ggtitle("depth grid check") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
test_map


# join blue and hump whale outputs
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

#instead of working in calendar years_months, work in crab seasons. But also retain the original year_month column
x.whale_crab_season <- x.whale %>% 
  mutate(year_month2 = year_month) %>% 
  separate(year_month2, into = c("year", "month"), sep = "_")  
x.whale_crab_season_v2 <- x.whale_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))


#join depth grid data and whale data -- the extent will be limited to that of the depth grid data
grid_depth_and_whale <- left_join(grid.key_WA_OR, x.whale_crab_season_v2, by = "GRID5KM_ID")



#HUMPBACK WHALE -- more likely in shallower waters
grid_depth_and_hump <-  grid_depth_and_whale %>% 
  st_drop_geometry() %>%
  select(-(Blue_occurrence_mean:Blue_occurrence_se)) %>% 
  filter(!is.na(depth)) %>% 
  filter(!is.na(year_month)) %>% 
  mutate(year = as.numeric(substr(year_month, 1,4)))

#------------------------
#quick map showing the area where grids have depth data and humpback whale data
grid.studyarea.id_hw <- sort(unique(grid_depth_and_hump$GRID5KM_ID))
grid.5km.hw <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_hw)
bbox = c(-127,41,-120,49) 

map_hw_study_area <-  ggplot() + 
  geom_sf(data = grid.key_WA_OR, aes(fill = depth, col = depth)) +  
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  geom_sf(data=grid.5km.hw,col='black',fill=NA,alpha=0.8) + 
  ggtitle("HW study area for inshore/offshore plots") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
map_hw_study_area
#------------------------


#plotting the depth dist of whale density - this will be limited to the extent of the depth grid data
# can either work in calendar years, or in crab seasons
grid_depth_and_hump_bins <-  grid_depth_and_hump %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) %>% 
  mutate(year = as.numeric(substr(year_month, 1,4))) %>% 
  mutate(month = as.numeric(substr(year_month, 6,7))) %>%
  #adjust grouping based on which of the below plots wanting to make
  #group_by(season, Bins) %>% 
  group_by(season, month, Bins) %>% 
  #group_by(year_month, Bins) %>% #is it correct to do the grouping at year_month level, or should it be at year level??
  summarise(Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE)) 


plot1 <- grid_depth_and_hump_bins %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_col(aes(x=Bins, y=Humpback_dens_median)) +
  #facet_wrap(~ year) +
  facet_wrap(~ season) +
  labs(x="depth bin (deeper <---> shallower)",y="Median humpback density") +
  ggtitle('Humpback whales') +
  theme_minimal()
plot1
#ggsave(here('whalepreds_aggregate','figures',paste0('median_hw_dens_by_depth_bin_and_crab_season_depth_grid_extent_in_WA_and_OR_20211110','.png')),plot1,w=12,h=10)


#this plot suggest that the shallowest depth bin (0-100m) has the highest hw dens
plot2 <- grid_depth_and_hump_bins %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_line(aes(x=factor(month,levels = c('12','1','2','3','4','5','6','7','8','9','10','11')), y=Humpback_dens_median, group=Bins, colour = Bins), size=1) +
  scale_colour_brewer(palette = "PRGn") +
  facet_wrap(~ season) +
  #facet_wrap(~ year) +
  labs(x="month",y="Median humpback density") +
  ggtitle('Humpback whales') +
  theme_minimal()
plot2
#ggsave(here('whalepreds_aggregate','figures',paste0('median_hw_dens_by_depth_bin_month_and_crab_season_depth_grid_extent_in_WA_and_OR_20211110','.png')),plot2,w=12,h=10)


#--------------------------------------------------------------


#BLUE WHALE -- less likely in the first 100m of water. OR restriction is 40 ftm = 73m
grid_depth_and_blue <-  grid_depth_and_whale %>% 
  st_drop_geometry() %>%
  #select(-(Humpback_dens_mean:Humpback_dens_se)) %>% #hw SE column doesn't exist in new data pull
  select(-(Humpback_dens_mean)) %>% 
  filter(!is.na(depth)) %>% 
  filter(!is.na(year_month)) %>% 
  mutate(year = as.numeric(substr(year_month, 1,4)))

#------------------------
#quick map showing the area where grids have depth data and humpback whale data
test <- grid_depth_and_blue %>% filter(!is.na(Blue_occurrence_mean)) #note that bw predictions actually end around 47.3
grid.studyarea.id_bw <- sort(unique(test$GRID5KM_ID))
grid.5km.bw <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_bw)
bbox = c(-127,41,-120,49) 


map_bw_study_area <-  ggplot() + 
  geom_sf(data = grid.key_WA_OR, aes(fill = depth, col = depth)) +  
  geom_sf(data=rmap.base,col=NA,fill='gray50') + 
  scale_fill_viridis(na.value=NA,option="D",name="depth") +      
  scale_color_viridis(na.value=NA,option="D",name="depth")  + 
  geom_sf(data=grid.5km.bw,col='black',fill=NA,alpha=0.8) + 
  ggtitle("BW study area for inshore/offshore plots") +  
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))
#coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) 
map_bw_study_area
#------------------------


#plotting the depth dist of whale density - this will be limited to the extent of the depth grid data
# can either work in calendar years, or in crab seasons
grid_depth_and_blue_bins <-  grid_depth_and_blue %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) %>% 
  mutate(year = as.numeric(substr(year_month, 1,4))) %>% 
  mutate(month = as.numeric(substr(year_month, 6,7))) %>%
  #adjust grouping based on which of the below plots wanting to make
  #group_by(season, Bins) %>% 
  group_by(season, month, Bins) %>%
  #group_by(year_month, Bins) %>% #is it correct to do the grouping at year_month level, or should it be at year level??
  summarise(Blue_occurrence_median = median(Blue_occurrence_mean, na.rm=TRUE))  


plot1b <- grid_depth_and_blue_bins %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_col(aes(x=Bins, y=Blue_occurrence_median)) +
  #facet_wrap(~ year) +
  facet_wrap(~ season) +
  labs(x="depth bin (deeper <---> shallower)",y="Median blue whale occurrence") +
  ggtitle('Blue whales') +
  theme_minimal()
plot1b
#ggsave(here('whalepreds_aggregate','figures',paste0('median_bw_dens_by_depth_bin_and_crab_season_depth_grid_extent_in_WA_and_OR_20211110','.png')),plot1b,w=12,h=10)


#this plot suggest that the shallowest depth bin (0-100m) has the least bw occurrence
plot2b <- grid_depth_and_blue_bins %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_line(aes(x=factor(month,levels = c('12','1','2','3','4','5','6','7','8','9','10','11')), y=Blue_occurrence_median, group=Bins, colour = Bins), size=1) +
  scale_colour_brewer(palette = "PRGn") +
  facet_wrap(~ season) +
  #facet_wrap(~ year) +
  labs(x="month",y="Median blue whale occurrence") +
  ggtitle('Blue whales') +
  theme_minimal()
plot2b
#ggsave(here('whalepreds_aggregate','figures',paste0('median_bw_dens_by_depth_bin_month_and_crab_season_depth_grid_extent_in_WA_and_OR_20211110','.png')),plot2b,w=12,h=10)



#----------------------------------------------------------------------
#what if first limit whale data to those grids that had fishing? -- try out with just WA

#this file is not clipped to WA waters:
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"

x.fish_WA <- readRDS(path.fish_WA) 
### Get grid cells with non-NA values for all, and save that of fishing data
grid.studyarea.id_WA <- sort(unique(x.fish_WA$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
grid.5km.fish_WA <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)
#filter whale data
grid_depth_and_hump_WA_fishing <-  grid_depth_and_hump %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)
#x.blue_WA <-  x.blue.all %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)


#plotting the depth dist of whale density - this will be limited to the extent of the depth grid data
# can either work in calendar years, or in crab seasons
grid_depth_and_hump_bins_WA_fishing <-  grid_depth_and_hump_WA_fishing %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) %>% 
  mutate(year = as.numeric(substr(year_month, 1,4))) %>% 
  mutate(month = as.numeric(substr(year_month, 6,7))) %>%
  #adjust grouping based on which of the below plots wanting to make
  #group_by(season, Bins) %>% 
  group_by(season, month, Bins) %>% 
  #group_by(year_month, Bins) %>% #is it correct to do the grouping at year_month level, or should it be at year level??
  summarise(Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE)) 


plot1_WA_fishing <- grid_depth_and_hump_bins_WA_fishing %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_col(aes(x=Bins, y=Humpback_dens_median)) +
  #facet_wrap(~ year) +
  facet_wrap(~ season) +
  labs(x="depth bin (deeper <---> shallower)",y="Median humpback density") +
  ggtitle('Humpback whales') +
  theme_minimal()
plot1_WA_fishing
#ggsave(here('whalepreds_aggregate','figures',paste0('median_hw_dens_by_depth_bin_and_crab_season_WA_fishing_extent_20211110','.png')),plot1_WA_fishing,w=12,h=10)


#this plot suggest that the shallowest depth bin (0-100m) has the highest hw dens
plot2_WA_fishing <- grid_depth_and_hump_bins_WA_fishing %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  ggplot() + 
  geom_line(aes(x=factor(month,levels = c('12','1','2','3','4','5','6','7','8','9','10','11')), y=Humpback_dens_median, group=Bins, colour = Bins), size=1) +
  scale_colour_brewer(palette = "PRGn") +
  facet_wrap(~ season) +
  #facet_wrap(~ year) +
  labs(x="month",y="Median humpback density") +
  ggtitle('Humpback whales') +
  theme_minimal()
plot2_WA_fishing
#ggsave(here('whalepreds_aggregate','figures',paste0('median_hw_dens_by_depth_bin_month_and_crab_season_WA_fishing_extent_20211110','.png')),plot2_WA_fishing,w=12,h=10)



#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------


#join depth grid data and fishing data
grid_depth_and_fish_WA <- left_join(grid.key_WA_OR, x.fish_WA, by = "GRID5KM_ID")
grid_depth_and_fish_OR <- left_join(grid.key_WA_OR, x.fish_OR, by = "GRID5KM_ID")

#WASHINGTON
grid_depth_and_fish_WA_v2 <-  grid_depth_and_fish_WA %>% 
  st_drop_geometry() %>%
  filter(!is.na(depth)) %>% 
  filter(!is.na(season_month))  

grid_depth_and_fish_WA_bins <-  grid_depth_and_fish_WA_v2 %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) %>%
  #adjust grouping based on which of the below plots wanting to make
  #group_by(season, Bins) %>%
  group_by(season_month, Bins) %>% #is it correct to do the grouping at year_month level, or should it be at year level??
  summarise(mean_M2_trapdens = mean(M2_trapdens, na.rm=TRUE)) %>% 
  #comment out last two lines if grouping by season (and not season_month)
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))  
  

plot3 <- grid_depth_and_fish_WA_bins %>%
  ggplot() + 
  geom_col(aes(x=Bins, y=mean_M2_trapdens)) +
  facet_wrap(~ season) +
  labs(x="depth",y="Mean trap density") +
  ggtitle('Fishing effort') +
  theme_minimal()
plot3
#ggsave(here('whalepreds_aggregate','figures',paste0('mean_trap_dens_by_depth_bin_and_season_WA_20211110','.png')),plot3,w=12,h=10)


#this plot suggest that the shallowest depth bin (0-100m) has the highest trap dens
plot3b <- grid_depth_and_fish_WA_bins %>%
  ggplot() + 
  geom_line(aes(x=month_name, y=mean_M2_trapdens, group=Bins, colour = Bins), size=1) +
  scale_colour_brewer(palette = "PRGn") +
  facet_wrap(~ season) +
  labs(x="month",y="Mean trap density") +
  ggtitle('Fishing effort') +
  theme_minimal()
plot3b
#ggsave(here('whalepreds_aggregate','figures',paste0('mean_trap_dens_by_depth_bin_month_and_season_WA_20211110','.png')),plot3b,w=12,h=10)



#OREGON -- note OR data not clipped to OR waters only
grid_depth_and_fish_OR_v2 <-  grid_depth_and_fish_OR %>% 
  st_drop_geometry() %>%
  filter(!is.na(depth)) %>% 
  filter(!is.na(season_month))  

grid_depth_and_fish_OR_bins <-  grid_depth_and_fish_OR_v2 %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) %>% 
  #adjust grouping based on which of the below plots wanting to make
  #group_by(season, Bins) %>%
  group_by(season_month, Bins) %>% #is it correct to do the grouping at year_month level, or should it be at year level??
  summarise(mean_M2_trapdens = mean(M2_trapdens, na.rm=TRUE)) %>% 
  #comment out last two lines if grouping by season (and not season_month)
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))

plot4 <- grid_depth_and_fish_OR_bins %>%
  ggplot() + 
  geom_col(aes(x=Bins, y=mean_M2_trapdens)) +
  facet_wrap(~ season) +
  labs(x="depth",y="Mean trap density") +
  ggtitle('Fishing effort_OR') +
  theme_minimal()
plot4
#ggsave(here('whalepreds_aggregate','figures',paste0('mean_trap_dens_by_depth_bin_and_season_OR_20211110','.png')),plot4,w=12,h=10)

#this plot suggest that the shallowest depth bin (0-100m) has the highest trap dens
plot4b <- grid_depth_and_fish_OR_bins %>%
  ggplot() + 
  geom_line(aes(x=month_name, y=mean_M2_trapdens, group=Bins, colour = Bins), size=1) +
  #scale_colour_brewer(palette = "PRGn") +
  facet_wrap(~ season) +
  labs(x="month",y="Mean trap density") +
  ggtitle('Fishing effort_OR') +
  theme_minimal()
plot4b
#ggsave(here('whalepreds_aggregate','figures',paste0('mean_trap_dens_by_depth_bin_month_and_season_OR_20211110','.png')),plot4b,w=12,h=10)

#--------------------------------------------------------------------------










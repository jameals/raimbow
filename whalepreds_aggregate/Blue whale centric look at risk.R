#' blue whale centric' look at risk

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
#path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
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
# x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
# glimpse(x.hump_2009_2020)


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

x.blue_crab_season <- x.blue.all %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") %>% # adds grid cell area  
  separate(year_month, into = c("year", "month"), sep = "_")  
x.blue_crab_season_v2 <- x.blue_crab_season %>% 
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
glimpse(x.blue_crab_season_v2)


# calculate MEAN whale values for different seasons - for May-Sep period (2013-2020)
x.blue.mean <- x.blue_crab_season_v2 %>%
  filter(is_May_Sep == "Y") %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  inner_join(grid.5km.lno) #if don't want ro clip at e.g. 44N
glimpse(x.blue.mean)

#-----------------------------------------------------------------------------------

#'good bw habitat', has very minimal overlap with fishery footprint (depends on definition of good habitat)
#this lack of overlap hinders doing a similar look with bw as with hw (where good habitat overlaps much more with fishery)
#due to the more coastal dist of hw, vs more offshore dist of bw

# try: if good bw habitat is defined as grids where average probability of occurrence is >0.50

# 0.75 probability of occurrence was too high cut-off, no good bw habitat in WA with that cut-off
# 0.70 probability of occurrence was also too high cut-off, no good bw habitat overlap with fishery
# 0.65 probability of occurrence was also too high cut-off, no good bw habitat overlap with fishery
# 0.60 probability of occurrence was also too high cut-off, almost no good bw habitat overlap with fishery
# 0.55 probability of occurrence had some good bw habitat overlapping with fishery in some years
# 0.50 probability of occurrence had some good bw habitat overlapping with fishery in most years

# start with df 'x.blue.mean' (x.whale.mean) which is mean value in a grid across May-Sep per seasons
MaySep_good_bw_hab_050_occur <- x.blue.mean %>% 
  #drop hw as this is bw specific
  #select(-Mean_Humpback_dens) %>% 
  group_by(season) %>% 
  mutate(BW_is_050_occur_or_higher = ifelse(Mean_Blue_occurrence > 0.50, 'Y', 'N')
  ) %>%
  #inner_join(grid.key_N44)
  inner_join(grid.5km.lno) #if don't want ro clip at e.g. 44N
glimpse(MaySep_good_bw_hab_050_occur)

#0.5 or 0.55 probability of occurrence seem the only values that would be useful, save a joint file
# MaySep_good_bw_hab_050_055_occur <- MaySep_good_bw_hab_050_occur %>% 
#   left_join(MaySep_good_bw_hab_055_occur, by = c("season", "is_May_Sep", "GRID5KM_ID", "area_km_lno", "Mean_Blue_occurrence", "geometry"))
# write_rds(MaySep_good_bw_hab_050_055_occur,here::here('wdfw','data',"MaySep_good_bw_hab_050_055_occur.rds"))



#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2019_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_MaySep_WA_fishery_footprint.rds'))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
#bbox = c(-127,30,-115,49) #extent inc CA
bbox = c(-127,43.5,-120,49) 

# plot blue whale
bw_subset_MaySep <- MaySep_good_bw_hab_050_occur %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(BW_is_050_occur_or_higher)) %>% 
  filter(BW_is_050_occur_or_higher == 'Y')

map_blue_MaySep_050_occur <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=BW_is_050_occur_or_higher,
              col=BW_is_050_occur_or_higher
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("May-Sep 2019-2020 \ngood BW habitat (>0.50 occurrence) \nwith 2019-2020 May-Sep fishery footprint") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_blue_MaySep_050_occur

png(paste0(path_figures, "/good_bw_habitat_050_occur_MaySep_2019_2020_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_blue_MaySep_050_occur,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())





#----------------------------------------------------------------

#Then try to look at what trap density was like in the good bw habitat -- will need to check code
#bring in fishing data 
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
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


MaySep_good_bw_hab_050_055_occur_fishing <- MaySep_good_bw_hab_050_055_occur %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID'))
glimpse(MaySep_good_bw_hab_050_055_occur_fishing)


summary_050_bw_habitat_fishing <- MaySep_good_bw_hab_050_055_occur_fishing %>% 
  filter(BW_is_050_occur_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE)
            #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            #tottraps_median = median(mean_tottraps, na.rm=TRUE)
  )
glimpse(summary_050_bw_habitat_fishing)  


ts_fishing_in_050_bw_habitat <- ggplot(summary_050_bw_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1)) + 
  geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (>0.5 prob of occur.) BW habitat") +
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
ts_fishing_in_050_bw_habitat




summary_055_bw_habitat_fishing <- MaySep_good_bw_hab_050_055_occur_fishing %>% 
  filter(BW_is_055_occur_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE)
            #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            #tottraps_median = median(mean_tottraps, na.rm=TRUE)
  )
glimpse(summary_050_bw_habitat_fishing)

ts_fishing_in_055_bw_habitat <- ggplot(summary_055_bw_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1)) + 
  geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (>0.55 prob of occur.) BW habitat") +
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
ts_fishing_in_055_bw_habitat




png(paste0(path_figures, "/ts_fishing_in_050_055_bw_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_050_bw_habitat,
          ts_fishing_in_055_bw_habitat,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


# change in mean trap dens between 2017-28 and 2019-20 seasons when prob occur >0.5
# season      mean trap dens    max trap dens
#2017-2018      5.0425274         49.504770
#2019-2020      3.8596105         29.494404
# change          -23%              -40%









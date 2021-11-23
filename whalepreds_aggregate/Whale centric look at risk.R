#'whale centric' look at risk

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
#WA fishery footprint is north of 44N during all of 2013-2020 - clip whale data to that

grid.key_N44 <- grid.key %>% 
  filter(LATITUDE > 44) %>% 
  #into this join area_km_lno info from layer: grid.5km.lno
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area



# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  #left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area
  inner_join(st_drop_geometry(grid.key_N44), by = "GRID5KM_ID")


#instead of working in calendar years, work in crab seasons -- also filter to 2013-2020
x.whale_crab_season <- x.whale %>% 
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

# calculate median whale values for different seasons - for May-Sep period (2013-2020)
x.whale.median <- x.whale_crab_season_v2 %>%
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.median)

##calculate median whale values across all of 2013-2020
x.whale.median.2013_2020 <- x.whale_crab_season_v2 %>%
  group_by(is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.median.2013_2020)

x.whale.median.2013_2020_MaySep <- x.whale.median.2013_2020 %>% 
  filter(is_May_Sep == 'Y')
#-----------------------------------------------------------------------------------

#map median density May-Sep for all of 2013-2020

#plus overlay WA May-Sep fishery footprint
dissolved_2013_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2013_2020_MaySep_WA_fishery_footprint.rds'))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 

map_hump <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median.2013_2020_MaySep), 
          aes(fill=Humpback_dens_median,
              col=Humpback_dens_median
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0.001,0.035,by=0.017),limits=c(0.001,0.035),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0.001,0.035,by=0.017),limits=c(0.001,0.035),oob=squish) + # 
  #if want to add fishery outline May-Sep 2013-2020
  geom_sf(data = dissolved_2013_2020_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("2013-2020 May-Sep \nMedian Humpback Whale Density") +
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
map_hump


map_blue <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median.2013_2020_MaySep), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.075,0.880,by=0.4),limits=c(0.075,0.880),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.075,0.880,by=0.4),limits=c(0.075,0.880),oob=squish) + 
  #if want to add fishery outline May-Sep 2013-2020
  geom_sf(data = dissolved_2013_2020_MaySep, color = 'black',size=1, fill = NA) +ggtitle("2013-2020 May-Sep \nMedianBlue Whale Occurrence") +
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
map_blue

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_2013_2020_clip at 44N_with fishery footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump,
          map_blue,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#-------------------------------------------------------------------------------

# map median density May-Sep by season
# input will be x.whale.median


# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 


subset_MaySep <- x.whale.median %>% 
  filter(is_May_Sep == "Y") %>% 
  #select season to map 
  filter(season == "2019-2020")


map_hump_MaySep <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_MaySep), 
          aes(fill=Humpback_dens_median,
              col=Humpback_dens_median
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  ggtitle("May-Sep 2019-2020 Median\nHumpback Whale Densities \nspatially clip at 44N") +
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
map_hump_MaySep


# plot median blue occurrence 
map_blue_MaySep <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_MaySep), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  ggtitle("May-Sep 2019-2020 Median\nBlue Whale Occurrence \nspatially clip at 44N") +
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
map_blue_MaySep

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_MaySep_2019-2020_spatially_clipped.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep,
          map_blue_MaySep,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#---------------------------------------------------------------
#looking for quantiles
#combined 2013-2020

##calculate quantiles across 2013-2020 by season
# the quantile function returns the cutpoints (i.e. 0%, 25%, 50%, 75%, and 100%) as well as the corresponding quantiles
#if we are just looking for 'hotspots' or the 'good habitats', only need the high percentiles
#'#BUT this provides info in monthly steps, and we might want to be working in May-Sep steps...
x.whale.2013_2020_quant <- x.whale_crab_season_v2 %>%
  filter(is_May_Sep == 'Y') %>% 
  select(season, Humpback_dens_mean, Blue_occurrence_mean) %>%
  group_by(season) %>% 
  summarise(
    #Humpback_dens_5th = quantile(Humpback_dens_mean, probs=0.05, na.rm=TRUE),
    #Humpback_dens_25th = quantile(Humpback_dens_mean, probs=0.25, na.rm=TRUE),
    #Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    #Humpback_dens_75th = quantile(Humpback_dens_mean, probs=0.75, na.rm=TRUE),
    Humpback_dens_90th = quantile(Humpback_dens_mean, probs=0.90, na.rm=TRUE),
    Humpback_dens_95th = quantile(Humpback_dens_mean, probs=0.95, na.rm=TRUE),

    #Blue_occur_5th = quantile(Blue_occurrence_mean, probs=0.05, na.rm=TRUE),
    #Blue_occur_25th = quantile(Blue_occurrence_mean, probs=0.25, na.rm=TRUE),
    #Blue_occur_median = median(Blue_occurrence_mean, na.rm=TRUE),
    #Blue_occur_75th = quantile(Blue_occurrence_mean, probs=0.75, na.rm=TRUE),
    Blue_occur_90th = quantile(Blue_occurrence_mean, probs=0.90, na.rm=TRUE),
    Blue_occur_95th = quantile(Blue_occurrence_mean, probs=0.95, na.rm=TRUE)
  ) 
glimpse(x.whale.2013_2020_quant)
#View(x.whale.2013_2020_quant)


x.whale.2013_2020_MaySep_quant_joined <- x.whale_crab_season_v2 %>% 
  filter(is_May_Sep == 'Y') %>% 
  left_join(x.whale.2013_2020_quant, by = "season")
glimpse(x.whale.2013_2020_MaySep_quant_joined)


#'good whale habitats' (90th or 95th percentiles) in each season
x.whale.2013_2020_MaySep_good_habitats <- x.whale.2013_2020_MaySep_quant_joined %>% 
  group_by(season) %>% 
  mutate(HW_is_90th_or_higher = ifelse(Humpback_dens_mean > Humpback_dens_90th, 'Y', 'N'),
         HW_is_95th_or_higher = ifelse(Humpback_dens_mean > Humpback_dens_95th, 'Y', 'N'),
         BW_is_90th_or_higher = ifelse(Blue_occurrence_mean > Blue_occur_90th, 'Y', 'N'),
         BW_is_95th_or_higher = ifelse(Blue_occurrence_mean > Blue_occur_95th, 'Y', 'N'),
         ) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.2013_2020_MaySep_good_habitats)
#note that output is still on a monthly time step....
View(x.whale.2013_2020_MaySep_good_habitats%>% filter(season=='2013-2014') %>% filter(is_May_Sep=='Y') %>% filter(GRID5KM_ID==113678))

#-------------------
# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 


hw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(HW_is_95th_or_higher))


map_hump_MaySep_95th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_subset_MaySep), 
          aes(fill=HW_is_95th_or_higher,
              col=HW_is_95th_or_higher
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  ggtitle("May-Sep 2013-2014 \ngood HW habitat (95th+) \nspatially clip at 44N") +
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
map_hump_MaySep_95th


# plot blue whale
bw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(BW_is_95th_or_higher))

map_blue_MaySep_95th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=BW_is_95th_or_higher,
              col=BW_is_95th_or_higher
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  ggtitle("May-Sep 2013-2014 \ngood BW habitat (95th+) \nspatially clip at 44N") +
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
map_blue_MaySep_95th

# plot blues and humps together
png(paste0(path_figures, "/test_good_wh_habitat_95th_MaySep_2013-2014_only_spatially_clipped.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep_95th,
          map_blue_MaySep_95th,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#-------------------

# calculate MEAN whale values for different seasons - for May-Sep period (2013-2020)
#and then look at percentiles
x.whale.mean <- x.whale_crab_season_v2 %>%
  filter(is_May_Sep == "Y") %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE),
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.mean)

x.whale.2013_2020_quant <- x.whale.mean %>%
  select(season, Mean_Humpback_dens, Mean_Blue_occurrence) %>%
  group_by(season) %>% 
  summarise(
    Humpback_dens_90th = quantile(Mean_Humpback_dens, probs=0.90, na.rm=TRUE),
    Humpback_dens_95th = quantile(Mean_Humpback_dens, probs=0.95, na.rm=TRUE),
    
    Blue_occur_90th = quantile(Mean_Blue_occurrence, probs=0.90, na.rm=TRUE),
    Blue_occur_95th = quantile(Mean_Blue_occurrence, probs=0.95, na.rm=TRUE)
  ) 
glimpse(x.whale.2013_2020_quant)



x.whale.2013_2020_MaySep_quant_joined <- x.whale.mean %>% 
  left_join(x.whale.2013_2020_quant, by = "season")
glimpse(x.whale.2013_2020_MaySep_quant_joined)


#'good whale habitats' (90th or 95th percentiles) in each season
x.whale.2013_2020_MaySep_good_habitats <- x.whale.2013_2020_MaySep_quant_joined %>% 
  group_by(season) %>% 
  mutate(HW_is_90th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_90th, 'Y', 'N'),
         HW_is_95th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_95th, 'Y', 'N'),
         BW_is_90th_or_higher = ifelse(Mean_Blue_occurrence > Blue_occur_90th, 'Y', 'N'),
         BW_is_95th_or_higher = ifelse(Mean_Blue_occurrence > Blue_occur_95th, 'Y', 'N'),
  ) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.2013_2020_MaySep_good_habitats)



# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 


hw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(HW_is_90th_or_higher))


map_hump_MaySep_90th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_subset_MaySep), 
          aes(fill=HW_is_90th_or_higher,
              col=HW_is_90th_or_higher
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  ggtitle("May-Sep 2013-2014 \ngood HW habitat (90th+) \nspatially clip at 44N") +
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
map_hump_MaySep_90th


# plot blue whale
bw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(BW_is_90th_or_higher))

map_blue_MaySep_90th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=BW_is_90th_or_higher,
              col=BW_is_90th_or_higher
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  ggtitle("May-Sep 2013-2014 \ngood BW habitat (90th+) \nspatially clip at 44N") +
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
map_blue_MaySep_90th

# plot blues and humps together
png(paste0(path_figures, "/test_good_wh_habitat_MaySep_averaged_bf_estimating_90th__2013-2014_only_spatially_clipped.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep_90th,
          map_blue_MaySep_90th,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#-------------------
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
    mean_trapdens = sum_M2_trapdens/number_obs,
    sum_M2_tottraps  = sum(M2_tottraps, na.rm=TRUE),
    mean_tottraps = sum_M2_tottraps/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens, mean_tottraps)
glimpse(x.fish_WA_MaySep)


x.whale.2013_2020_MaySep_good_habitats_fishing <- x.whale.2013_2020_MaySep_good_habitats %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID'))
glimpse(x.whale.2013_2020_MaySep_good_habitats_fishing)


summary_95th_HW_habitat_fishing <- x.whale.2013_2020_MaySep_good_habitats_fishing %>% 
  filter(HW_is_95th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE),
            tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            tottraps_median = median(mean_tottraps, na.rm=TRUE)
            )
glimpse(summary_95th_HW_habitat_fishing)  


#PLOT
ts_fishing_in_95th_hw_habitat_a <- ggplot(summary_95th_HW_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1), color = "darkred") + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (95th) HW habitat \n(defined one season at a time)") +
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
ts_fishing_in_95th_hw_habitat_a

ts_fishing_in_95th_hw_habitat_b <- ggplot(summary_95th_HW_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = tottraps_mean, group = 1), color="steelblue") +
  geom_line(aes(y = tottraps_median, group = 1), color="steelblue", linetype="twodash") +
  ylab("Total traps") + 
  xlab("Season") +
  ggtitle("May-Sep total traps \nmean = solid line, median = dashed line \nin good (95th) HW habitat \n(defined one season at a time)") +
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
ts_fishing_in_95th_hw_habitat_b

png(paste0(path_figures, "/test_ts_fishing_in_95th_hw_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_95th_hw_habitat_a,
          ts_fishing_in_95th_hw_habitat_b,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#For BW -- did bw 95th habitat completely miss fishing effort in 2015-16??
summary_90th_BW_habitat_fishing <- x.whale.2013_2020_MaySep_good_habitats_fishing %>% 
  filter(BW_is_90th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE),
            tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            tottraps_median = median(mean_tottraps, na.rm=TRUE)
  )
glimpse(summary_90th_BW_habitat_fishing)  


#PLOT
ts_fishing_in_90th_bw_habitat_a <- ggplot(summary_90th_BW_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1), color = "darkred") + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (90th) BW habitat \n(defined one season at a time)") +
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
ts_fishing_in_90th_bw_habitat_a

ts_fishing_in_90th_bw_habitat_b <- ggplot(summary_90th_BW_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = tottraps_mean, group = 1), color="steelblue") +
  geom_line(aes(y = tottraps_median, group = 1), color="steelblue", linetype="twodash") +
  ylab("Total traps") + 
  xlab("Season") +
  ggtitle("May-Sep total traps \nmean = solid line, median = dashed line \nin good (90th) BW habitat \n(defined one season at a time)") +
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
ts_fishing_in_90th_bw_habitat_b

png(paste0(path_figures, "/test_ts_fishing_in_90th_bw_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_90th_bw_habitat_a,
          ts_fishing_in_90th_bw_habitat_b,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#----------------------------------------------------------------------------
#testing couple diff ways for calcualting quantiles for HW across all of 2013-2020
test_quantiles_hw_all_of_2013_2020 <-  x.whale_crab_season_v2 %>%
  filter(is_May_Sep == 'Y') %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  select(Humpback_dens_mean) %>% 
  quantile(probs = c(0.9, 0.95), na.rm=TRUE)
# 90%        95% 
#  0.03304709 0.03717593 

x.whale.2013_2020_quant <- x.whale_crab_season_v2 %>%
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  filter(is_May_Sep == 'Y') %>% 
  select(Humpback_dens_mean, Blue_occurrence_mean) %>%
  summarise(
    Humpback_dens_90th = quantile(Humpback_dens_mean, probs=0.90, na.rm=TRUE),
    Humpback_dens_95th = quantile(Humpback_dens_mean, probs=0.95, na.rm=TRUE)
  ) 
glimpse(x.whale.2013_2020_quant)
## Humpback_dens_90th <dbl> 0.03304709
## Humpback_dens_95th <dbl> 0.03717593

#or #note that this below doesn't filter for May-Sep or 2013-2020...
x.whale_crab_season_v2_filtered <- x.whale_crab_season_v2 %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  filter(is_May_Sep == 'Y') 
x.whale_crab_season_v2_filtered$hw_quantile_all_2013_2020 <- ecdf(x.whale_crab_season_v2_filtered$Humpback_dens_mean)(x.whale_crab_season_v2_filtered$Humpback_dens_mean)
#and then could prob go:
hw_quantiles_95th_plus <- x.whale_crab_season_v2_filtered %>% 
  mutate(is_95th_percentile = ifelse(hw_quantile_all_2013_2020 > 0.95, 'Y', 'N')) %>%
  inner_join(grid.key_N44)
#----------------------------------------------------------------------------

x.whale.2014_2015_quantiles <- x.whale_crab_season_v2 %>%
  filter(season =='2014-2015') %>% 
  mutate(HW_95th_percentile = ifelse(Humpback_dens_mean > 0.04042436, 'Y', 'N'),
         BW_95th_percentile = ifelse(Blue_occurrence_mean > 0.6816083, 'Y', 'N')) %>%
  inner_join(grid.key_N44)
glimpse(x.whale.2014_2015_quantiles)


test_map_quantile_20142015 <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.2014_2015_quantiles), 
          aes(fill=HW_95th_percentile,
              col=HW_95th_percentile
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + 
  ggtitle("May-Sep 2014-2015 \nHumpback Whale \n95th percentile") +
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
test_map_quantile_20142015


test_map_hw_all_of_2013_2020 <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_quantiles_95th_plus), 
          aes(fill=is_95th_percentile,
              col=is_95th_percentile
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + 
  ggtitle("HW 2013-2020 \nHumpback Whale \n95th percentile") +
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
test_map_hw_all_of_2013_2020
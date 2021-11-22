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


#instead of working in calendar years, work in crab seasons
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
                  ,'Y', 'N'))
glimpse(x.whale_crab_season_v2)

# calculate median whale values for seasons - for May-Sep period
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
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0.001,0.033,by=0.032),limits=c(0.001,0.033),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0.001,0.033,by=0.032),limits=c(0.001,0.033),oob=squish) + # 
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
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.08,0.88,by=0.4),limits=c(0.08,0.88),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.08,0.88,by=0.4),limits=c(0.08,0.88),oob=squish) + 
  ggtitle("2013-2020 May-Sep \nMedianBlue Whale Occurrence") +
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
png(paste0(path_figures, "/map_median_blue_hump_2013_2020_clip at 44N.png"), width = 14, height = 10, units = "in", res = 300)
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













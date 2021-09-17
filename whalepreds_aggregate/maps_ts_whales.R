library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

# set some paths
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"
#Leena:
#path.grid.5km <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
#I keep having issues trying to load Grid_5km_landerased.rds. the readRDS() command later just gives error: Error in readRDS(file) : unknown input format
#I had this issue with some of the other whale coding files
#The only way around I've found is this:
#path.save2 <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.RDATA"
#load(path.save2)
#I don't have: weighted_mean_NGDC_depths_for_5km_gridcells.csv

# should be all outputs through july 2019 overlayed on 5km grid (i.e., not subset to DCRB fishing cells)
path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"
#Leena:
#path.hump <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#path.blue <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"


# where to put outputs
path_figures <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOWT/raimbow/whalepreds_aggregate/figures"

# load the data
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)

x.hump <- readRDS(path.hump) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
glimpse(x.hump)

x.blue <- readRDS(path.blue) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue)

# take a quick peek at 2019 to be sure nothing is weird
View(x.blue %>% group_by(year_month) %>% summarise(
  'Mean Occurrence' = mean(Blue_occurrence_mean, na.rm=TRUE),
  'Median Occurrence' = median(Blue_occurrence_mean,na.rm=TRUE),
  '75th Percentile' = quantile(Blue_occurrence_mean, probs=0.75, na.rm=TRUE),
  '25th Percentile' = quantile(Blue_occurrence_mean, probs=0.25, na.rm=TRUE),
  n=n()
)
)

# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#glimpse(grid.key)


# note here is where you could insert some code to filter out whale predictions so that they only include 5km cells where DCRB fishing occurred previously

# join blue and hump whale outputs
x.whale <- full_join(x.hump, x.blue, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

# calculate median whale values for full time period

x.whale.median <- x.whale %>%
  group_by(GRID5KM_ID, area_km_lno) %>%
  summarise(
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  left_join(grid.5km.lno)

# make maps based on whale outputs
## i did not convert humpback densities to abundance
## i did not normalize whale predictions

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
grid5km_bbox <- st_bbox(grid.5km.lno %>% 
                     st_as_sf()
)

map_hump <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median), 
          aes(fill=Humpback_dens_median,
              col=Humpback_dens_median
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2019 Median\nHumpback Whale Densities") +
  coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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

# png(paste0(path_maps, "/map_median_hump_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
# map_hump
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# plot median blue occurrence 

map_blue <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2019 Median\nBlue Whale Occurrence") +
  coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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

# png(paste0(path_maps, "/map_blue_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
# map_blue
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_2009_2019.png"), width = 14, height = 10, units = "in", res = 300)
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

# make time series based on whale outputs

# plot annual mean humpback densities
ts_hump <- ggplot(
  data = x.whale %>% 
    mutate(
      year = as.numeric(substr(year_month, 1,4))
    ) %>%
    group_by(year) %>%
    summarise(
      Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
    ), 
  aes(
    x = year, 
    y = Humpback_dens_mean
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2010, 2019, 1),
                     limits = c(2009.5,2019.5)) +
  ylab("Humpback Whale Density\n(mean)") + 
  xlab("Year") +
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
ts_hump

# plot annual mean blue whale densities
ts_blue <- ggplot(
  data = x.whale %>% 
    mutate(
      year = as.numeric(substr(year_month, 1,4))
    ) %>%
    group_by(year) %>%
    summarise(
      Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
    ), 
  aes(
    x = year, 
    y = Blue_occurrence_mean
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2010, 2019, 1),
                     limits = c(2009.5,2019.5)) +
  ylab("Blue Whale Occurrence\n(mean)") + 
  xlab("Year") +
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
ts_blue

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_2009_2019.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump,
          ts_blue,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#--------------------------------------------
ts_hump2 <- ggplot(
  data = x.whale %>% 
    group_by(year_month) %>%
    summarise(
      Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
    ), 
  aes(
    x = year_month, 
    y = Humpback_dens_mean
  )
) +
  geom_point(size=4) +
  geom_line(aes(group=1)) +
  
  ylab("Humpback Whale Density\n(mean)") + 
  xlab("Year_month") +
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
ts_hump2


ts_blue2 <- ggplot(
  data = x.whale %>% 
    group_by(year_month) %>%
    summarise(
      Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
    ), 
  aes(
    x = year_month, 
    y = Blue_occurrence_mean
  )
) +
  geom_point(size=4) +
  geom_line(aes(group=1)) +
  
  ylab("Blue Whale Occurrence\n(mean)") + 
  xlab("Year_month") +
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
ts_blue2
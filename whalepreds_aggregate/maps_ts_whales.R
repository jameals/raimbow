library(dplyr)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)

# set some paths
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"


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
  group_by()
  summarise

# make maps based on whale outputs
## i did not convert humpback densities to abundace
## i did not normalize whale predictions

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

map_hump <- ggplot() + 
  geom_sf(data=x.whale, 
          aes(fill=Humpback_dens_mean,
              col=Humpback_dens_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Density") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Humpback Density") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2019 Median humpback densities\non Dungeness crab fishing grounds\nduring the fishing season") +
  coord_sf(xlim=c(sq_bbox[1],sq_bbox[3]),ylim=c(sq_bbox[2],sq_bbox[4])) + 
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

png(paste0(path_maps, "/map_hump_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
map_hump
invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# plot median blue occurrence 

map_blue <- ggplot() + 
  geom_sf(data=sq_maps_grid_noNA, 
          aes(fill=Blue_occurrence,
              col=Blue_occurrence
          )
  ) +
  facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue whale occurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Blue whale occurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2019 Median blue whale occurrence\non Dungeness crab fishing grounds\nduring the fishing season") +
  coord_sf(xlim=c(sq_bbox[1],sq_bbox[3]),ylim=c(sq_bbox[2],sq_bbox[4])) + 
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

png(paste0(path_maps, "/map_blue_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
map_blue
invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# make time series based on whale outputs
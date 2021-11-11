#Some mapping of whale data

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


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area



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
  left_join(grid.5km.lno)
glimpse(x.whale.median)

#-----------------------------------------------------------------------------------


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
#grid5km_bbox <- st_bbox(grid.5km.lno %>% 
#                          st_as_sf()
#)
bbox = c(-127,41,-120,49) 


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
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("May-Sep 2019-2020 Median\nHumpback Whale Densities") +
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

# png(paste0(path_maps, "/map_median_hump_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# map_hump_MaySep
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_median_hump_MaySep.png'),map_hump,h=8,w=6)


# plot median blue occurrence 


map_blue_MaySep <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_MaySep), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("May-Sep 2019-2020 Median\nBlue Whale Occurrence") +
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

# png(paste0(path_maps, "/map_median_blue_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# map_blue_MaySep
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_median_blue_MaySep.png'),map_hump,h=8,w=6)

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_MaySep_2019-2020.png"), width = 14, height = 10, units = "in", res = 300)
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



#------------------------------------------------------------------

#grab grid cells that had fishing in May-Sep in that season
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") %>% 
  #and select season
  filter(season == "2019-2020")

grid.studyarea.id_WA_MaySep <- sort(unique(x.fish_WA_MaySep$GRID5KM_ID))
grid.5km.fish_WA_MaySep <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA_MaySep)
#----------------

#filter whale data to those grids and redo maps

subset_MaySep_fish_grids_only <- x.whale.median %>% 
  filter(is_May_Sep == "Y") %>% 
  #select season to map 
  filter(season == "2019-2020") %>%
  filter(GRID5KM_ID %in% grid.studyarea.id_WA_MaySep)


map_hump_MaySep_fish_grids_only <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_MaySep_fish_grids_only), 
          aes(fill=Humpback_dens_median,
              col=Humpback_dens_median
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("May-Sep 2019-2020 Median\nHumpback Whale Densities") +
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
map_hump_MaySep_fish_grids_only

# png(paste0(path_maps, "/map_median_hump_MaySep_fish_grids_only.png"), width = 14, height = 10, units = "in", res = 300)
# map_hump_MaySep_fish_grids_only
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_median_hump_MaySep_fish_grids_only.png'),map_hump,h=8,w=6)


# plot median blue occurrence 


map_blue_MaySep_fish_grids_only <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_MaySep_fish_grids_only), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("May-Sep 2019-2020 Median\nBlue Whale Occurrence") +
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
map_blue_MaySep_fish_grids_only

# png(paste0(path_maps, "/map_median_blue_MaySep_fish_grids_only.png"), width = 14, height = 10, units = "in", res = 300)
# map_blue_MaySep_fish_grids_only
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_median_blue_MaySep_fish_grids_only.png'),map_hump,h=8,w=6)

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_MaySep_2019-2020_fish_grids_only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep_fish_grids_only,
          map_blue_MaySep_fish_grids_only,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#How often was each grid cell used between 2013-2020 seasons?

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) 

#How many unique grids were ever usedn between 2013-2020?
grid.studyarea.id_WA <- sort(unique(x.fish_WA$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
# 635
#other way to do the same
#test <- x.fish_WA$GRID5KM_ID %>% unique() %>% sort()

grid.5km.fish_WA <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)



#unique grid IDs used in 2013-2014
x.fish_WA_2013_2014 <- x.fish_WA %>% filter(season =="2013-2014")
n_grids_2013_2014 <- sort(unique(x.fish_WA_2013_2014$GRID5KM_ID))
length(n_grids_2013_2014) #409

#unique grid IDs used in 2014-2015
x.fish_WA_2014_2015 <- x.fish_WA %>% filter(season =="2014-2015")
n_grids_2014_2015 <- sort(unique(x.fish_WA_2014_2015$GRID5KM_ID))
length(n_grids_2014_2015) #413

#unique grid IDs used in 2015-2016
x.fish_WA_2015_2016 <- x.fish_WA %>% filter(season =="2015-2016")
n_grids_2015_2016 <- sort(unique(x.fish_WA_2015_2016$GRID5KM_ID))
length(n_grids_2015_2016) #370

#unique grid IDs used in 2016-2017
x.fish_WA_2016_2017 <- x.fish_WA %>% filter(season =="2016-2017")
n_grids_2016_2017 <- sort(unique(x.fish_WA_2016_2017$GRID5KM_ID))
length(n_grids_2016_2017) #495

#unique grid IDs used in 2017-2018
x.fish_WA_2017_2018 <- x.fish_WA %>% filter(season =="2017-2018")
n_grids_2017_2018 <- sort(unique(x.fish_WA_2017_2018$GRID5KM_ID))
length(n_grids_2017_2018) #511

#unique grid IDs used in 2018-2019
x.fish_WA_2018_2019 <- x.fish_WA %>% filter(season =="2018-2019")
n_grids_2018_2019 <- sort(unique(x.fish_WA_2018_2019$GRID5KM_ID))
length(n_grids_2018_2019) #490

#unique grid IDs used in 2019-2020
x.fish_WA_2019_2020 <- x.fish_WA %>% filter(season =="2019-2020")
n_grids_2019_2020 <- sort(unique(x.fish_WA_2019_2020$GRID5KM_ID))
length(n_grids_2019_2020) #397



#How often was each grid cell used between 2013-2020 seasons?
grid.5km.fish_WA_grids <- grid.5km.fish_WA %>% 
  mutate(season_2013_2014 = ifelse(GRID5KM_ID %in% n_grids_2013_2014, '1', '0')) %>% 
  mutate(season_2014_2015 = ifelse(GRID5KM_ID %in% n_grids_2014_2015, '1', '0')) %>% 
  mutate(season_2015_2016 = ifelse(GRID5KM_ID %in% n_grids_2015_2016, '1', '0')) %>% 
  mutate(season_2016_2017 = ifelse(GRID5KM_ID %in% n_grids_2016_2017, '1', '0')) %>% 
  mutate(season_2017_2018 = ifelse(GRID5KM_ID %in% n_grids_2017_2018, '1', '0')) %>% 
  mutate(season_2018_2019 = ifelse(GRID5KM_ID %in% n_grids_2018_2019, '1', '0')) %>% 
  mutate(season_2019_2020 = ifelse(GRID5KM_ID %in% n_grids_2019_2020, '1', '0')) %>% 
  mutate_if(is.character,as.numeric)
  

grid.5km.fish_WA_grids_sum <- grid.5km.fish_WA_grids %>% 
  rowwise() %>%
  mutate(sum = sum(season_2013_2014,
                   season_2014_2015,
                   season_2015_2016,
                   season_2016_2017,
                   season_2017_2018,
                   season_2018_2019,
                   season_2019_2020,
                   na.rm = T))

nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 7)) #268
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 6)) #68
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 5)) #55
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 4)) #35
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 3)) #54
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 2)) #69
nrow(grid.5km.fish_WA_grids_sum %>% filter(sum == 1)) #86




#map it
# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)


bbox = c(-127,45,-120,49) 


map_grids_used <- ggplot() + 
  geom_sf(data=sf::st_as_sf(grid.5km.fish_WA_grids_sum), 
          aes(fill=sum,
              col=sum
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="plasma",name="No. seasons\ngrid used") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="plasma",name="No. seasons\ngrid used") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("Grids used by \nfishery (2013-2020)") +
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
map_grids_used

png(paste0(path_figures, "/map_Number of seasons when grid cell used by fishery_2013-2020.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_grids_used,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())






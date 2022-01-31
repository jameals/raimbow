# WA fishery footprint area by season

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

path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#-----------------------------------------------------------------------------------

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week period
# look at May_Sep only
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

#x.fish_WA_MaySep <- x.fish_WA %>% 
#  filter(is_May_Sep == "Y") 

MaySep_area_fished <- x.fish_WA %>% 
  filter(is_May_Sep == 'Y') %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6)

#  season    total_area (km2)
# 2013-2014      2963
# 2014-2015      2775
# 2015-2016      2303
# 2016-2017      3509
# 2017-2018      4527
# 2018-2019      3679
# 2019-2020      3008

#average area across pre-reg seasons
mean_area_pre_reg <- MaySep_area_fished %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  summarise(mean_area_pre_reg = mean(total_area_km2))
#3215.4  
#% change from pre-reg average to 2018-19
(3679-3215.4)/3215.4*100
#14.41811
#% change from pre-reg average to 2019-20
(3008-3215.4)/3215.4*100
#-6.450208

#--------------------------------------------------------------------------------

x.fish_WA_2013_2014_winter <- x.fish_WA %>% filter(season == '2013-2014' & is_May_Sep == 'N')
grids_2013_2014_winter <- sort(unique(x.fish_WA_2013_2014_winter$GRID5KM_ID))
grids_5km_2013_2014_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2013_2014_winter)
dissolved_2013_2014_winter <- st_union(grids_5km_2013_2014_winter)
#plot(dissolved_2013_2014_winter)

x.fish_WA_2013_2014_summer <- x.fish_WA %>% filter(season == '2013-2014' & is_May_Sep == 'Y')
grids_2013_2014_summer <- sort(unique(x.fish_WA_2013_2014_summer$GRID5KM_ID))
grids_5km_2013_2014_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2013_2014_summer)
dissolved_2013_2014_summer <- st_union(grids_5km_2013_2014_summer)
#plot(dissolved_2013_2014_summer)


rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)


bbox = c(-126,46,-122,49) 

map_outline_2013_2014 <- ggplot() + 
  geom_sf(data = dissolved_2013_2014_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2013_2014_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2013-2014\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2013-2014") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2013_2014




x.fish_WA_2014_2015_winter <- x.fish_WA %>% filter(season == '2014-2015' & is_May_Sep == 'N')
grids_2014_2015_winter <- sort(unique(x.fish_WA_2014_2015_winter$GRID5KM_ID))
grids_5km_2014_2015_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2014_2015_winter)
dissolved_2014_2015_winter <- st_union(grids_5km_2014_2015_winter)
#plot(dissolved_2014_2015_winter)

x.fish_WA_2014_2015_summer <- x.fish_WA %>% filter(season == '2014-2015' & is_May_Sep == 'Y')
grids_2014_2015_summer <- sort(unique(x.fish_WA_2014_2015_summer$GRID5KM_ID))
grids_5km_2014_2015_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2014_2015_summer)
dissolved_2014_2015_summer <- st_union(grids_5km_2014_2015_summer)
#plot(dissolved_2014_2015_summer)

map_outline_2014_2015 <- ggplot() + 
  geom_sf(data = dissolved_2014_2015_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2014_2015_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2014-2015\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2014-2015") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2014_2015




x.fish_WA_2015_2016_winter <- x.fish_WA %>% filter(season == '2015-2016' & is_May_Sep == 'N')
grids_2015_2016_winter <- sort(unique(x.fish_WA_2015_2016_winter$GRID5KM_ID))
grids_5km_2015_2016_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2015_2016_winter)
dissolved_2015_2016_winter <- st_union(grids_5km_2015_2016_winter)
#plot(dissolved_2015_2016_winter)

x.fish_WA_2015_2016_summer <- x.fish_WA %>% filter(season == '2015-2016' & is_May_Sep == 'Y')
grids_2015_2016_summer <- sort(unique(x.fish_WA_2015_2016_summer$GRID5KM_ID))
grids_5km_2015_2016_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2015_2016_summer)
dissolved_2015_2016_summer <- st_union(grids_5km_2015_2016_summer)
#plot(dissolved_2015_2016_summer)

map_outline_2015_2016 <- ggplot() + 
  geom_sf(data = dissolved_2015_2016_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2015_2016_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2015-2016\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2015-2016") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2015_2016




x.fish_WA_2016_2017_winter <- x.fish_WA %>% filter(season == '2016-2017' & is_May_Sep == 'N')
grids_2016_2017_winter <- sort(unique(x.fish_WA_2016_2017_winter$GRID5KM_ID))
grids_5km_2016_2017_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2016_2017_winter)
dissolved_2016_2017_winter <- st_union(grids_5km_2016_2017_winter)
#plot(dissolved_2016_2017_winter)

x.fish_WA_2016_2017_summer <- x.fish_WA %>% filter(season == '2016-2017' & is_May_Sep == 'Y')
grids_2016_2017_summer <- sort(unique(x.fish_WA_2016_2017_summer$GRID5KM_ID))
grids_5km_2016_2017_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2016_2017_summer)
dissolved_2016_2017_summer <- st_union(grids_5km_2016_2017_summer)
#plot(dissolved_2016_2017_summer)

map_outline_2016_2017 <- ggplot() + 
  geom_sf(data = dissolved_2016_2017_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2016_2017_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2016-2017\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2016-2017") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2016_2017




x.fish_WA_2017_2018_winter <- x.fish_WA %>% filter(season == '2017-2018' & is_May_Sep == 'N')
grids_2017_2018_winter <- sort(unique(x.fish_WA_2017_2018_winter$GRID5KM_ID))
grids_5km_2017_2018_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2017_2018_winter)
dissolved_2017_2018_winter <- st_union(grids_5km_2017_2018_winter)
#plot(dissolved_2017_2018_winter)

x.fish_WA_2017_2018_summer <- x.fish_WA %>% filter(season == '2017-2018' & is_May_Sep == 'Y')
grids_2017_2018_summer <- sort(unique(x.fish_WA_2017_2018_summer$GRID5KM_ID))
grids_5km_2017_2018_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2017_2018_summer)
dissolved_2017_2018_summer <- st_union(grids_5km_2017_2018_summer)
#plot(dissolved_2017_2018_summer)

map_outline_2017_2018 <- ggplot() + 
  geom_sf(data = dissolved_2017_2018_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2017_2018_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2017-2018\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2017-2018") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2017_2018




x.fish_WA_2018_2019_winter <- x.fish_WA %>% filter(season == '2018-2019' & is_May_Sep == 'N')
grids_2018_2019_winter <- sort(unique(x.fish_WA_2018_2019_winter$GRID5KM_ID))
grids_5km_2018_2019_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2018_2019_winter)
dissolved_2018_2019_winter <- st_union(grids_5km_2018_2019_winter)
#plot(dissolved_2018_2019_winter)

x.fish_WA_2018_2019_summer <- x.fish_WA %>% filter(season == '2018-2019' & is_May_Sep == 'Y')
grids_2018_2019_summer <- sort(unique(x.fish_WA_2018_2019_summer$GRID5KM_ID))
grids_5km_2018_2019_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2018_2019_summer)
dissolved_2018_2019_summer <- st_union(grids_5km_2018_2019_summer)
#plot(dissolved_2018_2019_summer)

map_outline_2018_2019 <- ggplot() + 
  geom_sf(data = dissolved_2018_2019_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2018_2019_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2018-2019\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2018-2019") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2018_2019




x.fish_WA_2019_2020_winter <- x.fish_WA %>% filter(season == '2019-2020' & is_May_Sep == 'N')
grids_2019_2020_winter <- sort(unique(x.fish_WA_2019_2020_winter$GRID5KM_ID))
grids_5km_2019_2020_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2019_2020_winter)
dissolved_2019_2020_winter <- st_union(grids_5km_2019_2020_winter)
#plot(dissolved_2019_2020_winter)

x.fish_WA_2019_2020_summer <- x.fish_WA %>% filter(season == '2019-2020' & is_May_Sep == 'Y')
grids_2019_2020_summer <- sort(unique(x.fish_WA_2019_2020_summer$GRID5KM_ID))
grids_5km_2019_2020_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_2019_2020_summer)
dissolved_2019_2020_summer <- st_union(grids_5km_2019_2020_summer)
#plot(dissolved_2019_2020_summer)

map_outline_2019_2020 <- ggplot() + 
  geom_sf(data = dissolved_2019_2020_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_2019_2020_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2019-2020\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("2019-2020") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2019_2020


path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"
png(paste0(path_figures, "/Fishery_footprint_outline_winter_summer.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(
  map_outline_2013_2014, 
  map_outline_2014_2015,
  map_outline_2015_2016,
  map_outline_2016_2017,
  map_outline_2017_2018,
  map_outline_2018_2019,
  map_outline_2019_2020,
  ncol=4,
  nrow=2,
  #legend="top",
  #common.legend = TRUE,
  #legend="right",
  #labels="auto",
  vjust=8,
  hjust=0
)
invisible(dev.off())



#------------------
x.fish_WA_all_winter <- x.fish_WA %>% filter(is_May_Sep == 'N')
grids_all_winter <- sort(unique(x.fish_WA_all_winter$GRID5KM_ID))
grids_5km_all_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_all_winter)
dissolved_all_winter <- st_union(grids_5km_all_winter)
#plot(dissolved_all_winter)

x.fish_WA_all_summer <- x.fish_WA %>% filter(is_May_Sep == 'Y')
grids_all_summer <- sort(unique(x.fish_WA_all_summer$GRID5KM_ID))
grids_5km_all_summer <- grid.5km %>% filter(GRID5KM_ID %in% grids_all_summer)
dissolved_all_summer <- st_union(grids_5km_all_summer)
#plot(dissolved_all_summer)


map_outline_2014_2020 <- ggplot() + 
  geom_sf(data = dissolved_all_winter, color = 'blue', fill = 'blue',alpha=0.1) +
  geom_sf(data = dissolved_all_summer, color = 'red', fill = 'red',alpha=0.1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #ggtitle("Fishery outline 2019-2020\nblue = Dec-Apr, red = May-Sep") +
  ggtitle("All seasons") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=10)
  )
map_outline_2014_2020







#####################################################################################
#####################################################################################
#OLD
x.fish_WA_MaySep_footprints <- x.fish_WA_MaySep %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6)

#  season    total_area (km2)
# 2013-2014      4048
# 2014-2015      3012
# 2015-2016      2867
# 2016-2017      5533
# 2017-2018      6322
# 2018-2019      4747
# 2019-2020      3231

mean_area <- mean(x.fish_WA_MaySep_footprints$total_area_km2)

x.fish_WA_MaySep_footprints$season <- factor(x.fish_WA_MaySep_footprints$season, 
                                             levels = c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'))



WA_fishery_footprint_area <- ggplot(x.fish_WA_MaySep_footprints, aes(x=season)) + 
  geom_line(aes(y = total_area_km2, group = 1), color = "black") + 
  geom_point(aes(y = total_area_km2, group = 1), color = "black", size=2) + 
  geom_hline(yintercept= mean_area, linetype="dashed", color = "red") +
  ylab("Area (km2)") + 
  xlab("Season") +
  ggtitle("May-Sep fishery footprint area\nred line = mean area across 2013-2020") +
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
WA_fishery_footprint_area










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

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week ste or 1-month step
# look at May_Sep only
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
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

#only one season differes if use 2w vs 1m data
#  season    total_area (km2) using 1-mon data
# 2013-2014      2963
# 2014-2015      2775
# 2015-2016      2303
# 2016-2017      3495
# 2017-2018      4527            4550
# 2018-2019      3679
# 2019-2020      2994

#average area across pre-reg seasons
mean_area_pre_reg <- MaySep_area_fished %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  summarise(mean_area_pre_reg = mean(total_area_km2))
#	3217.07
#% change from pre-reg average to 2018-19
(3679-3217.07)/3217.07*100
#14.35872
#% change from pre-reg average to 2019-20
(2994-3217.07)/3217.07*100
#-6.933949

#using 1-month gridded data
# mean_area_pre_reg = 3217.07 
#% change from pre-reg average to 2018-19
(3679-3217.07)/3217.07*100
#14.35872
#% change from pre-reg average to 2019-20
(2994-3217.07)/3217.07*100
#-6.933949


##boxplot
MaySep_area_fished_box <- MaySep_area_fished %>% 
  filter(season != '2018-2019') %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))

box_May_Sep_footprint_area_pre_reg_vs_2019_2020 <- ggplot() +
  geom_dotplot(data = MaySep_area_fished_box %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = total_area_km2),binaxis='y', stackdir='center', dotsize=1) +
  geom_point(data = MaySep_area_fished_box %>% filter(pre_post_reg=='2019-2020'), aes(x = pre_post_reg, y = total_area_km2), size=3, color='red') +
  ylab("Fishery footprint area (km2) May-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_May_Sep_footprint_area_pre_reg_vs_2019_2020


JulSep_area_fished <- x.fish_WA %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6)

#season       total_area_km2
#2013-2014      1433
#2014-2015      1147
#2015-2016      1551
#2016-2017      1583
#2017-2018      1973
#2018-2019      1856
#2019-2020      1476

##boxplot
JulSep_area_fished_box <- JulSep_area_fished %>% 
  filter(season != '2019-2020') %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))

boxJul_Sep_footprint_area_pre_reg_vs_2018_2019 <- ggplot() +
  geom_dotplot(data = JulSep_area_fished_box %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = total_area_km2),binaxis='y', stackdir='center', dotsize=1) +
  geom_point(data = JulSep_area_fished_box %>% filter(pre_post_reg=='2018-2019'), aes(x = pre_post_reg, y = total_area_km2), size=3, color='red') +
  ylab("Fishery footprint area (km2) Jul-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
boxJul_Sep_footprint_area_pre_reg_vs_2018_2019
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
png(paste0(path_figures, "/Fishery_footprint_outline_winter_summer_using 1monthly gridded data.png"), width = 14, height = 10, units = "in", res = 300)
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
#write_rds(dissolved_all_summer,here::here('wdfw','data',"dissolved_2014_2020_MaySep_WA_fishery_footprint.rds"))

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

path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"
png(paste0(path_figures, "/Fishery_footprint_outline_winter_summer_all_seasons_using 1monthly gridded data.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(
  map_outline_2014_2020, 
  ncol=1,
  nrow=1,
  #legend="top",
  #common.legend = TRUE,
  #legend="right",
  #labels="auto",
  vjust=8,
  hjust=0
)
invisible(dev.off())






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




#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#non-confidental versions of maps
#start from scratch, bring data in

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week ste or 1-month step
# look at May_Sep only
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

#-----------------------------------------------------------------------
#this is the  way of figuring if more than 3 unique vessels were in a grid cell in a given period
#bring in data as points, not summarised by grid cell
traps_g_all_logs_WA_waters <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))

#count number of unique vessels that used a given grid cell within a given time step
logs_all_nvessels <- traps_g_all_logs_WA_waters %>% 
  group_by(season, is_May_Sep, GRID5KM_ID,grd_x,grd_y) %>% 
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 


#join the new, correct, number of unique vessels in a grid in an interval into gridded df
x.fish_WA_v2 <- x.fish_WA %>% 
  select(-nvessels) %>% 
  left_join(logs_all_nvessels,by=c("season", "is_May_Sep","GRID5KM_ID", "grd_x", "grd_y"))


#If fewer than 3 unique vessels in a grid, that should be removed
non_conf_x.fish_WA <- x.fish_WA_v2 %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F)) %>%
  filter(is_confidential == FALSE)


#-----------------------------------------------------------------------


MaySep_area_fished_non_conf <- non_conf_dat2 %>% 
  filter(is_May_Sep == 'Y') %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6)



#--------------------------------------------------------------------------------

x.fish_WA_2013_2014_winter <- non_conf_x.fish_WA %>% filter(season == '2013-2014' & is_May_Sep == 'N')
grids_2013_2014_winter <- sort(unique(x.fish_WA_2013_2014_winter$GRID5KM_ID))
grids_5km_2013_2014_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2013_2014_winter)
dissolved_2013_2014_winter <- st_union(grids_5km_2013_2014_winter)
#plot(dissolved_2013_2014_winter)

x.fish_WA_2013_2014_summer <- non_conf_x.fish_WA %>% filter(season == '2013-2014' & is_May_Sep == 'Y')
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




x.fish_WA_2014_2015_winter <- non_conf_x.fish_WA %>% filter(season == '2014-2015' & is_May_Sep == 'N')
grids_2014_2015_winter <- sort(unique(x.fish_WA_2014_2015_winter$GRID5KM_ID))
grids_5km_2014_2015_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2014_2015_winter)
dissolved_2014_2015_winter <- st_union(grids_5km_2014_2015_winter)
#plot(dissolved_2014_2015_winter)

x.fish_WA_2014_2015_summer <- non_conf_x.fish_WA %>% filter(season == '2014-2015' & is_May_Sep == 'Y')
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




x.fish_WA_2015_2016_winter <- non_conf_x.fish_WA %>% filter(season == '2015-2016' & is_May_Sep == 'N')
grids_2015_2016_winter <- sort(unique(x.fish_WA_2015_2016_winter$GRID5KM_ID))
grids_5km_2015_2016_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2015_2016_winter)
dissolved_2015_2016_winter <- st_union(grids_5km_2015_2016_winter)
#plot(dissolved_2015_2016_winter)

x.fish_WA_2015_2016_summer <- non_conf_x.fish_WA %>% filter(season == '2015-2016' & is_May_Sep == 'Y')
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




x.fish_WA_2016_2017_winter <- non_conf_x.fish_WA %>% filter(season == '2016-2017' & is_May_Sep == 'N')
grids_2016_2017_winter <- sort(unique(x.fish_WA_2016_2017_winter$GRID5KM_ID))
grids_5km_2016_2017_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2016_2017_winter)
dissolved_2016_2017_winter <- st_union(grids_5km_2016_2017_winter)
#plot(dissolved_2016_2017_winter)

x.fish_WA_2016_2017_summer <- non_conf_x.fish_WA %>% filter(season == '2016-2017' & is_May_Sep == 'Y')
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




x.fish_WA_2017_2018_winter <- non_conf_x.fish_WA %>% filter(season == '2017-2018' & is_May_Sep == 'N')
grids_2017_2018_winter <- sort(unique(x.fish_WA_2017_2018_winter$GRID5KM_ID))
grids_5km_2017_2018_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2017_2018_winter)
dissolved_2017_2018_winter <- st_union(grids_5km_2017_2018_winter)
#plot(dissolved_2017_2018_winter)

x.fish_WA_2017_2018_summer <- non_conf_x.fish_WA %>% filter(season == '2017-2018' & is_May_Sep == 'Y')
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




x.fish_WA_2018_2019_winter <- non_conf_x.fish_WA %>% filter(season == '2018-2019' & is_May_Sep == 'N')
grids_2018_2019_winter <- sort(unique(x.fish_WA_2018_2019_winter$GRID5KM_ID))
grids_5km_2018_2019_winter <- grid.5km %>% filter(GRID5KM_ID %in% grids_2018_2019_winter)
dissolved_2018_2019_winter <- st_union(grids_5km_2018_2019_winter)
#plot(dissolved_2018_2019_winter)

x.fish_WA_2018_2019_summer <- non_conf_x.fish_WA %>% filter(season == '2018-2019' & is_May_Sep == 'Y')
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




non_conf_x.fish_WA_2019_2020_winter <- non_conf_x.fish_WA %>% filter(season == '2019-2020' & is_May_Sep == 'N')
non_conf_grids_2019_2020_winter <- sort(unique(non_conf_x.fish_WA_2019_2020_winter$GRID5KM_ID))
non_conf_grids_5km_2019_2020_winter <- grid.5km %>% filter(GRID5KM_ID %in% non_conf_grids_2019_2020_winter)
non_conf_dissolved_2019_2020_winter <- st_union(non_conf_grids_5km_2019_2020_winter)
#plot(dissolved_2019_2020_winter)

x.fish_WA_2019_2020_summer <- non_conf_x.fish_WA %>% filter(season == '2019-2020' & is_May_Sep == 'Y')
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
png(paste0(path_figures, "/Fishery_footprint_outline_winter_summer_using 2w gridded data_NON_CONF.png"), width = 14, height = 10, units = "in", res = 300)
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



#this is the  way of figuring if more than 3 unique vessels were in a grid cell in a given period
#bring in data as points, not summarised by grid cell
traps_g_all_logs_WA_waters <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))

#count number of unique vessels that used a given grid cell within a given time step
logs_all_nvessels <- traps_g_all_logs_WA_waters %>% 
  group_by(is_May_Sep, GRID5KM_ID,grd_x,grd_y) %>% #here do not group by seasons
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 


#join the new, correct, number of unique vessels in a grid in an interval into gridded df
#that is filtered to May-Sep but all seasons grouped
x.fish_WA_v2 <- x.fish_WA %>% 
  select(-nvessels) %>% 
  left_join(logs_all_nvessels,by=c("is_May_Sep","GRID5KM_ID", "grd_x", "grd_y"))


#If fewer than 3 unique vessels in a grid, that should be removed
non_conf_x.fish_WA <- x.fish_WA_v2 %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F)) %>%
  filter(is_confidential == FALSE)


non_conf_x.fish_WA_all_winter <- non_conf_x.fish_WA %>% filter(is_May_Sep == 'N')
non_conf_grids_all_winter <- sort(unique(non_conf_x.fish_WA_all_winter$GRID5KM_ID))
non_conf_grids_5km_all_winter <- grid.5km %>% filter(GRID5KM_ID %in% non_conf_grids_all_winter)
non_conf_dissolved_all_winter <- st_union(non_conf_grids_5km_all_winter)
#plot(non_conf_dissolved_all_winter)

non_conf_x.fish_WA_all_summer <- non_conf_x.fish_WA %>% filter(is_May_Sep == 'Y')
non_conf_grids_all_summer <- sort(unique(non_conf_x.fish_WA_all_summer$GRID5KM_ID))
non_conf_grids_5km_all_summer <- grid.5km %>% filter(GRID5KM_ID %in% non_conf_grids_all_summer)
non_conf_dissolved_all_summer <- st_union(non_conf_grids_5km_all_summer)
#plot(non_conf_dissolved_all_summer)
#write_rds(non_conf_dissolved_all_summer,here::here('wdfw','data',"dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds"))


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

path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"
png(paste0(path_figures, "/Fishery_footprint_outline_winter_summer_all_seasons_using 2w gridded data_NON_CONF.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(
  map_outline_2014_2020, 
  ncol=1,
  nrow=1,
  #legend="top",
  #common.legend = TRUE,
  #legend="right",
  #labels="auto",
  vjust=8,
  hjust=0
)
invisible(dev.off())



#% overlap between confidential and non-confidential data
#grids_5km_all_winter
#non_conf_grids_5km_all_winter

grids_5km_all_winter_v2 <- grids_5km_all_winter %>% 
  st_drop_geometry() %>% 
  mutate(conf_nonconf = "confidential")

non_conf_grids_5km_all_winter_v2 <- non_conf_grids_5km_all_winter %>% 
  st_drop_geometry() %>% 
  mutate(conf_nonconf = "non-confidential")

full_join_grids_winter_all_seasons <- 
  full_join(grids_5km_all_winter_v2, non_conf_grids_5km_all_winter_v2, by=c('GRID5KM_ID'))

nrow(full_join_grids_winter_all_seasons %>%  filter(!is.na(conf_nonconf.y)))
#every grid in the non-confidential data set also exists in the confidential data

nrow(full_join_grids_winter_all_seasons %>%  filter(conf_nonconf.x == "confidential")) #414
nrow(full_join_grids_winter_all_seasons %>%  filter(!is.na(conf_nonconf.y))) #352
352/414*100
#85% of of confidential grids overlap/exist in the non-confidential version




#summer
#grids_5km_all_summer
#non_conf_grids_5km_all_summer


grids_5km_all_summer_v2 <- grids_5km_all_summer %>% 
  st_drop_geometry() %>% 
  mutate(conf_nonconf = "confidential")

non_conf_grids_5km_all_summer_v2 <- non_conf_grids_5km_all_summer %>% 
  st_drop_geometry() %>% 
  mutate(conf_nonconf = "non-confidential")

full_join_grids_summer_all_seasons <- 
  full_join(grids_5km_all_summer_v2, non_conf_grids_5km_all_summer_v2, by=c('GRID5KM_ID'))

nrow(full_join_grids_summer_all_seasons %>%  filter(!is.na(conf_nonconf.y)))
#every grid in the non-confidential data set also exists in the confidential data

nrow(full_join_grids_summer_all_seasons %>%  filter(conf_nonconf.x == "confidential")) #281
nrow(full_join_grids_summer_all_seasons %>%  filter(!is.na(conf_nonconf.y))) #173
173/281*100
#62% of of confidential grids overlap/exist in the non-confidential version



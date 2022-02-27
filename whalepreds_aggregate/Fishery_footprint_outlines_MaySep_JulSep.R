## FISHERY FOOTPRINT OUTLINES
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

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# load the data
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)

#-----------------------------------------------------------------------------------

#May-Sep


path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))


x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") 


#May-Sep 2014-2020 fishery footprint
grid.studyarea.id_WA_MaySep <- sort(unique(x.fish_WA_MaySep$GRID5KM_ID))
grid.5km.fish_WA_MaySep <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA_MaySep)


#unique grid IDs used in 2013-2014 May_Sep
x.fish_WA_MaySep_2013_2014 <- x.fish_WA_MaySep %>% filter(season =="2013-2014")
n_grids_2013_2014 <- sort(unique(x.fish_WA_MaySep_2013_2014$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2013_2014 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2013_2014)

#unique grid IDs used in 2014-2015 May_Sep
x.fish_WA_MaySep_2014_2015 <- x.fish_WA_MaySep %>% filter(season =="2014-2015")
n_grids_2014_2015 <- sort(unique(x.fish_WA_MaySep_2014_2015$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2014_2015 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2014_2015)

#unique grid IDs used in 2015-2016 May_Sep
x.fish_WA_MaySep_2015_2016 <- x.fish_WA_MaySep %>% filter(season =="2015-2016")
n_grids_2015_2016 <- sort(unique(x.fish_WA_MaySep_2015_2016$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2015_2016 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2015_2016)

#unique grid IDs used in 2016-2017 May_Sep
x.fish_WA_MaySep_2016_2017 <- x.fish_WA_MaySep %>% filter(season =="2016-2017")
n_grids_2016_2017 <- sort(unique(x.fish_WA_MaySep_2016_2017$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2016_2017 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2016_2017)

#unique grid IDs used in 2017-2018 May_Sep
x.fish_WA_MaySep_2017_2018 <- x.fish_WA_MaySep %>% filter(season =="2017-2018")
n_grids_2017_2018 <- sort(unique(x.fish_WA_MaySep_2017_2018$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2017_2018 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2017_2018)

#unique grid IDs used in 2018-2019 May_Sep
x.fish_WA_MaySep_2018_2019 <- x.fish_WA_MaySep %>% filter(season =="2018-2019")
n_grids_2018_2019 <- sort(unique(x.fish_WA_MaySep_2018_2019$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2018_2019 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2018_2019)

#unique grid IDs used in 2019-2020 May_Sep
x.fish_WA_MaySep_2019_2020 <- x.fish_WA_MaySep %>% filter(season =="2019-2020")
n_grids_2019_2020 <- sort(unique(x.fish_WA_MaySep_2019_2020$GRID5KM_ID))
grid.5km.fish_WA_MaySep_2019_2020 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2019_2020)



# OUTLINES OF FISHERY FOOTPRINTS
# May-Sep


#outline of fishery footprint 2014-2020 May-Sep
dissolved_2014_2020_MaySep <- st_union(grid.5km.fish_WA_MaySep)
plot(dissolved_2014_2020_MaySep)
#write_rds(dissolved_2014_2020_MaySep,here::here('wdfw','data',"dissolved_2014_2020_MaySep_WA_fishery_footprint_20220202.rds"))


dissolved_2013_2014_MaySep <- st_union(grid.5km.fish_WA_MaySep_2013_2014)
plot(dissolved_2013_2014_MaySep)

dissolved_2014_2015_MaySep <- st_union(grid.5km.fish_WA_MaySep_2014_2015)
plot(dissolved_2014_2015_MaySep)

dissolved_2015_2016_MaySep <- st_union(grid.5km.fish_WA_MaySep_2015_2016)
plot(dissolved_2015_2016_MaySep)

dissolved_2016_2017_MaySep <- st_union(grid.5km.fish_WA_MaySep_2016_2017)
plot(dissolved_2016_2017_MaySep)

dissolved_2017_2018_MaySep <- st_union(grid.5km.fish_WA_MaySep_2017_2018)
plot(dissolved_2017_2018_MaySep)

dissolved_2018_2019_MaySep <- st_union(grid.5km.fish_WA_MaySep_2018_2019)
plot(dissolved_2018_2019_MaySep)

dissolved_2019_2020_MaySep <- st_union(grid.5km.fish_WA_MaySep_2019_2020)
plot(dissolved_2019_2020_MaySep)


write_rds(dissolved_2013_2014_MaySep,here::here('wdfw','data',"dissolved_2013_2014_MaySep_WA_fishery_footprint_20220202.rds"))



#---------------------------


#Jul-Sep


path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))


x.fish_WA_JulSep <- x.fish_WA %>% 
  filter(month_name %in% c('July', 'August', 'September')) 


#Jul-Sep 2014-2020 fishery footprint
grid.studyarea.id_WA_JulSep <- sort(unique(x.fish_WA_JulSep$GRID5KM_ID))
grid.5km.fish_WA_JulSep <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA_JulSep)


#unique grid IDs used in 2013-2014 Jul_Sep
x.fish_WA_JulSep_2013_2014 <- x.fish_WA_JulSep %>% filter(season =="2013-2014")
n_grids_2013_2014 <- sort(unique(x.fish_WA_JulSep_2013_2014$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2013_2014 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2013_2014)

#unique grid IDs used in 2014-2015 Jul_Sep
x.fish_WA_JulSep_2014_2015 <- x.fish_WA_JulSep %>% filter(season =="2014-2015")
n_grids_2014_2015 <- sort(unique(x.fish_WA_JulSep_2014_2015$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2014_2015 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2014_2015)

#unique grid IDs used in 2015-2016 Jul_Sep
x.fish_WA_JulSep_2015_2016 <- x.fish_WA_JulSep %>% filter(season =="2015-2016")
n_grids_2015_2016 <- sort(unique(x.fish_WA_JulSep_2015_2016$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2015_2016 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2015_2016)

#unique grid IDs used in 2016-2017 Jul_Sep
x.fish_WA_JulSep_2016_2017 <- x.fish_WA_JulSep %>% filter(season =="2016-2017")
n_grids_2016_2017 <- sort(unique(x.fish_WA_JulSep_2016_2017$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2016_2017 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2016_2017)

#unique grid IDs used in 2017-2018 Jul_Sep
x.fish_WA_JulSep_2017_2018 <- x.fish_WA_JulSep %>% filter(season =="2017-2018")
n_grids_2017_2018 <- sort(unique(x.fish_WA_JulSep_2017_2018$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2017_2018 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2017_2018)

#unique grid IDs used in 2018-2019 Jul_Sep
x.fish_WA_JulSep_2018_2019 <- x.fish_WA_JulSep %>% filter(season =="2018-2019")
n_grids_2018_2019 <- sort(unique(x.fish_WA_JulSep_2018_2019$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2018_2019 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2018_2019)

#unique grid IDs used in 2019-2020 Jul_Sep
x.fish_WA_JulSep_2019_2020 <- x.fish_WA_JulSep %>% filter(season =="2019-2020")
n_grids_2019_2020 <- sort(unique(x.fish_WA_JulSep_2019_2020$GRID5KM_ID))
grid.5km.fish_WA_JulSep_2019_2020 <- grid.5km %>% filter(GRID5KM_ID %in% n_grids_2019_2020)



# OUTLINES OF FISHERY FOOTPRINTS
# Jul-Sep


#outline of fishery footprint 2014-2020 Jul-Sep
dissolved_2014_2020_JulSep <- st_union(grid.5km.fish_WA_JulSep)
plot(dissolved_2014_2020_JulSep)
#write_rds(dissolved_2014_2020_JulSep,here::here('wdfw','data',"dissolved_2014_2020_JulSep_WA_fishery_footprint_20220227.rds"))


dissolved_2013_2014_JulSep <- st_union(grid.5km.fish_WA_JulSep_2013_2014)
plot(dissolved_2013_2014_JulSep)

dissolved_2014_2015_JulSep <- st_union(grid.5km.fish_WA_JulSep_2014_2015)
plot(dissolved_2014_2015_JulSep)

dissolved_2015_2016_JulSep <- st_union(grid.5km.fish_WA_JulSep_2015_2016)
plot(dissolved_2015_2016_JulSep)

dissolved_2016_2017_JulSep <- st_union(grid.5km.fish_WA_JulSep_2016_2017)
plot(dissolved_2016_2017_JulSep)

dissolved_2017_2018_JulSep <- st_union(grid.5km.fish_WA_JulSep_2017_2018)
plot(dissolved_2017_2018_JulSep)

dissolved_2018_2019_JulSep <- st_union(grid.5km.fish_WA_JulSep_2018_2019)
plot(dissolved_2018_2019_JulSep)

dissolved_2019_2020_JulSep <- st_union(grid.5km.fish_WA_JulSep_2019_2020)
plot(dissolved_2019_2020_JulSep)


write_rds(dissolved_2019_2020_JulSep,here::here('wdfw','data',"dissolved_2019_2020_JulSep_WA_fishery_footprint_20220227.rds"))




















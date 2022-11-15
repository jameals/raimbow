#create all study area and time step combos

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(magrittr)

#-----------------------------------------------------------------------------------

# 'study area' created in QGIS, to encompass all grids inside 200m depth contour 
# plus 1-grid 'buffer' (grids that could be fished, provides some absences for presence-absence modelling)

#read in 'study area' (grid)
study_area <- read_sf(here::here('DCRB_sdmTMB','data', 'study_area.shp'))
glimpse(study_area)
#plot(study_area)

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 
#1532 unique grid IDs, but some grids that overlap with land have been split into smaller pieces
#1570 polygons

#### OWEN EDITS 11/15/2022
# with merged polys
study_area_unique_grid_id <- study_area %>% 
  group_by(GRID5KM_ID) %>% 
  summarise() %>% 
  ungroup()
# lets put the attributes back in: sum of area and mean NGDC_GRID
## NOTE: not sure if this is how you want to define depth (mean value across sliced up grid pieces). 
# It seems like a fine decision but there are other options
extra_atts <- study_area %>% 
  st_set_geometry(NULL) %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(AREA=sum(AREA),
            NGDC_GRID=mean(NGDC_GRID)) %>% 
  ungroup()
study_area_unique_grid_id %<>%
  left_join(extra_atts,by=c("GRID5KM_ID"))
glimpse(study_area_unique_grid_id)
# get grid centroids for newly summarized grid
study_area_centroids <- study_area_unique_grid_id %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(GRID5KM_ID=unique(study_area_unique_grid_id$GRID5KM_ID))
study_area_centroids %>% 
  ggplot(aes(X,Y,color=GRID5KM_ID))+
  geom_point()
write_rds(study_area_centroids,here('DCRB_sdmTMB','data','study_grid_centroids.rds'))

#### END OWEN EDITS ####


study_area_df <- as.data.frame(study_area_grids_id) %>% 
  rename(GRID5KM_ID = study_area_grids_id)

#the study area grid needs to have all 2-week combos for all years
#first half of month will be days 1-15, second half of month will be days 16 and up
season <- c("2007-2008","2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014", 
            "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
half_month <- as.factor(c("December_1", "December_2", "January_1", "January_2", 
                          "February_1", "February_2", "March_1", "March_2", 
                          "April_1", "April_2", "May_1", "May_2",
                          "June_1","June_2","July_1","July_2",
                          "August_1","August_2","September_1"))
half_month <- factor(half_month, levels = c("December_1", "December_2", "January_1", "January_2", 
                          "February_1", "February_2", "March_1", "March_2", 
                          "April_1", "April_2", "May_1", "May_2",
                          "June_1","June_2","July_1","July_2",
                          "August_1","August_2","September_1"))

season_halfmonth_combos <- crossing(season, half_month)

study_area_df_with_all_season_halfmonth_combos <- crossing(study_area_df, season_halfmonth_combos) 


#-----------------------------------------------------------------------------------

#bring in original grid (these steps are same as what is done before simulating pots as points)
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
names(grd)

# spatial area matching key of each grid cell (because the grid has been trimmed to the coastline)
# also matches to areas with specific port and embayment codes (NGDC_GRID) based on the bathymetry grid
grd_area_key <- grd %>% 
  select(GRID5KM_ID,NGDC_GRID,AREA) 


#select only those grid cells in study area
grids_study_area <- grd_area_key %>% 
  filter(GRID5KM_ID %in% study_area_grids_id)

study_area_grids_with_all_season_halfmonth_combos <- grids_study_area %>% 
  left_join(study_area_df_with_all_season_halfmonth_combos, by="GRID5KM_ID")
#save the sf version
#write_rds(study_area_grids_with_all_season_halfmonth_combos,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_sf.rds"))



#### this part not working
##trying to find grid centroid for all grids, including those that are not regular shapes (being cut off by land)
study_area_grids_with_all_season_halfmonth_combos_v2 <- study_area_grids_with_all_season_halfmonth_combos %>% 
  mutate(fasterize_field = paste0(GRID5KM_ID,"_",AREA))

# rasterized grid, for extracting evenly spaced centroid coordinates for later plotting
grd_r <- fasterize(study_area_grids_with_all_season_halfmonth_combos_v2,
                   raster = raster(study_area_grids_with_all_season_halfmonth_combos_v2,
                                   res=5000,
                                   crs=crs(study_area_grids_with_all_season_halfmonth_combos_v2)),
                   field="fasterize_field")
grd_xy <- rasterToPoints(grd_r) %>% as_tibble() %>% set_colnames(c("x","y","GRID5KM_ID")) %>%
  st_as_sf(coords=c('x','y'),crs=st_crs(study_area_grids_with_all_season_halfmonth_combos_v2))
grd_xy <- grd_xy %>% st_coordinates() %>% as_tibble() %>% mutate(GRID5KM_ID=grd_xy$GRID5KM_ID) %>%
  set_colnames(c("grd_x","grd_y","GRID5KM_ID"))









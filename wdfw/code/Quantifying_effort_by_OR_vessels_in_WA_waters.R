# Quantifying the effort/percent of traps by OR vessels in WA waters
# / what percent of pots in WA waters are from WA and OR vessels


library(tidyverse)
library(sf)
library(viridis)
library(cowplot)
library(here)
library(rnaturalearth)
library(fasterize)
library(sp)
library(magrittr)
library(raster)
select <- dplyr::select
library(scales)
library(lubridate)
library(gridExtra)
library(nngeo)
library(scales)

#------------------------------------------------------------------------------

# bring in logs for both states in point format (not gridded yet)
# add a column denoting from which state the logbook is from/in which state landed

traps_g_WA <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2020.rds')) %>% 
  mutate(Landing_logbook_state = 'WA')
traps_g_WA <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2009_2020.rds')) %>% 
  mutate(Landing_logbook_state = 'WA')

# create columns for season, month etc
traps_g_WA <- traps_g_WA %>% 
  #st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )


traps_g_OR <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds')) %>% 
  mutate(Landing_logbook_state = 'OR')

# create columns for season, month etc
traps_g_OR <- traps_g_OR %>% 
  #st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )


# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
names(grd)

#a grid to help label WA waters and OR waters
# note that this is not perfect, as WA-OR border goes across a grid, but that has been labelled as WA
WA_grids_test <- read_sf(here::here('wdfw','data','WA_grids_test.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid


#------------------------------------------------------------------------------

# add a column to logbooks to denote whether a simulated point is in WA or OR waters
# this is not very accurate as grids overlap with WA-OR border

grids_WA_waters <- sort(unique(WA_grids_test$GRID5KM_ID)) 

traps_g_WA <- traps_g_WA %>% 
  mutate(pot_loc_state = ifelse(GRID5KM_ID %in% grids_WA_waters, "WA", "OR"))

# # test to see if labeling worked
# test <- traps_g_WA %>%
#   filter(season == "2018-2019") %>%
#   select(geometry,pot_loc_state)
# 
# # grab a base map
# rmap.base <- c(
#   st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
#     filter(admin %in% c("Canada", "Mexico")) %>%
#     st_geometry() %>%
#     st_transform(st_crs(WA_grids_test))
# )
# 
# #bbox
# bbox = c(-126,45,-123,47)
# 
# map_test <- ggplot() +
#   geom_sf(data=sf::st_as_sf(test),
#           aes(fill=pot_loc_state,
#               col=pot_loc_state
#           )
#   ) +
#   geom_sf(data=rmap.base,col=NA,fill='gray50') +
#   ggtitle("test") +
#   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
#   theme_minimal() + #theme_classic() +
#   theme(text=element_text(family="sans",size=10,color="black"),
#         legend.text = element_text(size=10),
#         axis.title=element_text(family="sans",size=14,color="black"),
#         axis.text=element_text(family="sans",size=8,color="black"),
#         panel.grid.major = element_line(color="gray50",linetype=3),
#         axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
#         strip.text = element_text(size=14),
#         title=element_text(size=16)
#   )
# map_test

#st_write(test, "test1.shp") #not restricted to MaySep


traps_g_OR <- traps_g_OR %>% 
  mutate(pot_loc_state = ifelse(GRID5KM_ID %in% grids_WA_waters, "WA", "OR"))

#------------------------------------------------------------------------------
#focus on May-Sep

traps_g_WA_MaySep <- traps_g_WA %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")
  
traps_g_OR_MaySep <- traps_g_OR %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")

#------------------------------------------------------------------------------  
#remove some columns to allow joining of datasets

traps_g_WA_MaySep_tidy <- traps_g_WA_MaySep %>% 
  select(Vessel, License, SetID, geometry, GRID5KM_ID, Landing_logbook_state, season, month_name, pot_loc_state)

traps_g_OR_MaySep_tidy <- traps_g_OR_MaySep %>% 
  select(Vessel, PermitNumber, SetID, geometry, GRID5KM_ID, Landing_logbook_state, season, month_name, pot_loc_state) %>% 
  rename(License = PermitNumber)

#join logs
traps_g_WA_OR_MaySep <- rbind(traps_g_WA_MaySep_tidy, traps_g_OR_MaySep_tidy)


#filter to be seasons that appear in both logs
traps_g_WA_OR_MaySep_2013_2018 <- traps_g_WA_OR_MaySep %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) 
#alternatively, look at those seasons that match and OR was 100% entered
#traps_g_WA_OR_MaySep_2009_2011 <- traps_g_WA_OR_MaySep %>% 
#  filter(season %in% c('2009-2010','2010-2011')) 

#st_write(traps_g_WA_OR_MaySep_2013_2018, "test2.shp")
#st_write(traps_g_WA_MaySep, "traps_g_WA_MaySep.shp")
#st_write(traps_g_WA_MaySep, "traps_g_WA_MaySep_labels.shp") #MaySep labelled but not filtered for #--> too large, takes too long to try to export as shp
#------------------------------------------------------------------------------  

#out of the pots that are in WA waters, what % was landed/reported in OR logbooks?

summary_traps_g_WA_OR_MaySep_2013_2018 <- traps_g_WA_OR_MaySep_2013_2018 %>% 
  st_set_geometry(NULL) %>% 
  filter(pot_loc_state == "WA") %>%  #pots that were located in WA waters
  group_by(season) %>% 
  summarise(n_records = n(), #total number of pots in a given season in WA waters
            n_landed_in_WA = length(Landing_logbook_state[Landing_logbook_state == "WA"]),
            n_landed_in_OR = length(Landing_logbook_state[Landing_logbook_state == "OR"])
            ) %>% 
  mutate(percent_landed_in_WA = (n_landed_in_WA/n_records)*100,
         percent_landed_in_OR = (n_landed_in_OR/n_records)*100
         )


#season    n_records   n_landed_in_WA    n_landed_in_OR    percent_landed_in_WA  percent_landed_in_OR
# 2013-2014   242184        242067              117               99.95169            0.04831038
# 2014-2015   94431         94431               0                 100.00000           0.00000000
# 2015-2016   134300        133981              319               99.76247            0.23752792
# 2016-2017   164453        164353              100               99.93919            0.06080765
# 2017-2018   200181        200081              100               99.95005            0.04995479

#2009-10 and 2010-11, OR logs were 100% entered
summary_traps_g_WA_OR_MaySep_2009_2011 <- traps_g_WA_OR_MaySep_2009_2011 %>% 
  st_set_geometry(NULL) %>% 
  filter(pot_loc_state == "WA") %>%  #pots that were located in WA waters
  group_by(season) %>% 
  summarise(n_records = n(), #total number of pots in a given season in WA waters
            n_landed_in_WA = length(Landing_logbook_state[Landing_logbook_state == "WA"]),
            n_landed_in_OR = length(Landing_logbook_state[Landing_logbook_state == "OR"])
  ) %>% 
  mutate(percent_landed_in_WA = (n_landed_in_WA/n_records)*100,
         percent_landed_in_OR = (n_landed_in_OR/n_records)*100
  )

#season     n_records    n_landed_in_WA    n_landed_in_OR  percent_landed_in_WA   percent_landed_in_OR
#2009-2010    129180        128480              700               99.45812            0.5418795
#2010-2011    129344        128911              433               99.66523            0.3347662
#------------------------------------------------------------------------------  
#------------------------------------------------------------------------------
#label but don't filter out May-Sep

traps_g_WA_MaySep_labelled <- traps_g_WA %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

traps_g_OR_MaySep_labelled <- traps_g_OR %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) 

#is it possible that same SetID could appear both in WA and OR?
traps_g_WA_MaySep_labelled <- traps_g_WA_MaySep_labelled %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))

traps_g_OR_MaySep_labelled <- traps_g_OR_MaySep_labelled %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))



#remove some columns to allow joining of datasets
#assigning state using grid is not good, so don't include that column #pot_loc_state,
#pot limit not joined to WA data yet
traps_g_WA_MaySep_tidy <- traps_g_WA_MaySep_labelled %>% 
  select(Vessel, License, SetID, SetDate, PotsFished, geometry, depth, GRID5KM_ID, grd_x, grd_y, Landing_logbook_state, season, month_name,  is_May_Sep, SetID2)

traps_g_OR_MaySep_tidy <- traps_g_OR_MaySep_labelled %>% 
  select(Vessel, PermitNumber, SetID, SetDate, PotsFished, geometry, depth, GRID5KM_ID, grd_x, grd_y, Landing_logbook_state, season, month_name,  is_May_Sep, SetID2) %>% #pot_loc_state,
  rename(License = PermitNumber)

#join logs
traps_g_WA_OR_MaySep_labelled <- rbind(traps_g_WA_MaySep_tidy, traps_g_OR_MaySep_tidy)


#dataset too large to export as a whole
#export one season at a time --> then can do clipping of point data in QGIS
#note that currently don't have 2018-2020 for OR
traps_g_WA_OR_MaySep_2013_2014_labelled <- traps_g_WA_OR_MaySep_labelled %>% 
  filter(season %in% c('2013-2014')) 
st_write(traps_g_WA_OR_MaySep_2013_2014_labelled, "traps_g_WA_OR_MaySep_2013_2014_labelled_SetID2.shp") #MaySep labelled but not filtered out


#------------------------------------------------------------------------------  
#------------------------------------------------------------------------------  


#could we clip the point data in R using this? --> yes, but still keeps some points in WA waters that would need to be removed 
#- somehow matching to grid line rather than the exact parallel border between OR and WA?
WA_waters_clip_layer <- read_sf(here::here('wdfw','data', 'WA_waters_clip_layer.shp')) %>% 
  st_transform(st_crs(grd))

traps_g_WA_OR_MaySep_2013_2014_labelled <- traps_g_WA_OR_MaySep_2013_2014_labelled %>% 
  st_transform(st_crs(grd))

st_crs(WA_waters_clip_layer)==st_crs(traps_g_WA_OR_MaySep_2013_2014_labelled)

cropped<-st_crop( traps_g_WA_OR_MaySep_2013_2014_labelled$geometry, WA_waters_clip_layer$geometry) #only keeping geometry
plot(cropped)

cropped<-st_crop( traps_g_WA_OR_MaySep_2013_2014_labelled, WA_waters_clip_layer) #keep all columns
plot(cropped)

st_write(cropped, "traps_g_WA_OR_MaySep_2013_2014_labelled_cropped_in_R.shp") #MaySep labelled but not filtered out

st_crs(grd)


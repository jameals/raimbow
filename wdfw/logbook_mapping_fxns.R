## Mapping functions for WDFW logbook data
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(viridis)
library(magrittr)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)

# if you have to load the logs you can do it here
# using the 2017-2018 slice provided by Leena
#Note that PrimaryLogbookPage can be of format e.g. "1009-1", so input as character not double
logs <- read_csv(here('wdfw','data','WDFW-Dcrab-logbooks-compiled_stackcoords_season20172018.csv'),
                 col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')

# QC: FishTicket1 of format Q999999 are landings into OR and have been entered into WA database because the vessel sent logbook copies. 
# These tickets should not be used in the data set because they would be part of the OR Dungeness crab fishery.
logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") #use this to retain NAs until the next step

# QC: for each variable/column in the logs, count how many NA values there are
# numNA <- logs %>% summarise(across(everything(),~sum(is.na(.x))))

# bathymetry
bathy <- raster(here::here('wdfw','data','vms_composite_bath.txt'))

# example spatial grid
# 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
names(grd)

# spatial area matching key of each grid cell (because the grid has been trimmed to the coastline)
# also matches to areas with specific port and embayment codes (NGDC_GRID) based on the bathymetry grid

grd_area_key <- grd %>% 
  select(GRID5KM_ID,NGDC_GRID,AREA) %>%
  mutate(is_port_or_bay=ifelse(NGDC_GRID==-999999,F,T))

# background map (coastline)
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%  
  st_transform(st_crs(grd))

# Main steps for these functions
# 1. make strings into individual pots by segmentizing lines (sf::st_line_sample())
# 2. remove points on land by using a bathymetry layer
# 3. filter to desired year, month, and period (period is 1 or 2 for the first or second half of the month)
# 4. map

# this function makes the traps and filters for bathymetry
# df is the logbooks dataframe
# bathy is a raster representation of bathymetry

# df<- logs;year_choice=2018;month_choice=2;period_choice=2

place_traps <- function(df,bathy,year_choice,month_choice,period_choice){
  df %<>%
    dplyr::select(Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
    # filter for the desired dates
    mutate(yr=year(SetDate),m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
    filter(yr%in%year_choice,m%in%month_choice,period%in%period_choice) %>% 
    distinct() %>% 
    # make sure each set has a beginning and end
    group_by(SetID) %>% 
    mutate(n=n()) %>% 
    filter(n==2,!is.na(PotsFished)) %>%
    # convert to sf points
    st_as_sf(coords=c('lon','lat'),crs=4326) %>% 
    st_transform(32610) %>% 
    # create linestrings
    group_by(Vessel,SetID,PotsFished,SetDate) %>% 
    summarise(do_union = FALSE) %>% 
    st_cast("LINESTRING")
  
  traps <- df %>% 
    ungroup() %>% 
    # now use those linestrings to place pots using sf::st_line_sample
    mutate(traplocs=purrr::pmap(list(PotsFished,geometry),
                             .f=function(pots,line) st_line_sample(line,n=pots))) %>% 
    # pull out the x/y coordinates of the traps
    mutate(trapcoords=purrr::map(traplocs,
                                 function(x)st_coordinates(x) %>% set_colnames(c('x','y','id')) %>% as_tibble())) %>% 
    select(Vessel,SetID,PotsFished,SetDate,trapcoords) %>% 
    # reorganize and unlist (i.e., make a dataframe where each row is an individual trap location)
    st_set_geometry(NULL) %>% 
    unnest(cols=c(trapcoords))
  
  # find depth of traps
  traps_sf <- traps %>%
    st_as_sf(coords=c('x','y'),crs=32610) %>% 
    st_transform(4326) %>% 
    select(-id)
  
  # do the raster extract with the bathymetry grid
  # Note- I think this would be faster with a vector representation (points) of the bathy grid
  bathy.points <- raster::extract(bathy,traps_sf)
  
  # add depth as a column variable
  # divide by 10 to go from decimeters to meters
  traps_sf %<>%
    mutate(depth=bathy.points/10)
  
  # find points on land and collect their SetIDs to a list
  traps_on_land <- traps_sf %>% filter(depth > 0) 
  # if want to also filter out pots at unreasonable depth, while retaining very low values for ports/embayments use something like
  # traps_sf %>% filter(depth < -500 & depth > -1000 | depth > 0)
  unique_SetIDs_on_land <- unique(traps_on_land$SetID)
  
  # Remove ALL points whose Set_ID appears on that list
  traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)
  
  return(traps_sf)  
}

# This next function spatially joins Blake's 5km grid (or some other grid) to the data
# It also joins the grid id matching key to identify ports and bays and assign correct grid cell areas
# trapssf is the spatial (sf) dataframe produced in the previous function,gkey is the grid with matching key

join_grid <- function(traps_sf,gkey){

  traps_sf %<>%
    # convert to planar projection to match the grid
    st_transform(st_crs(gkey))
  
  # rasterized grid, for extracting evenly spaced centroid coordinates for later plotting
   grd_r <- fasterize(gkey,raster = raster(grd,res=5000,crs=crs(gkey)),field="GRID5KM_ID")

   grd_xy <- rasterToPoints(grd_r) %>% as_tibble() %>% set_colnames(c("x","y","GRID5KM_ID")) %>%
     st_as_sf(coords=c('x','y'),crs=st_crs(gkey))
   grd_xy <- grd_xy %>% st_coordinates() %>% as_tibble() %>% mutate(GRID5KM_ID=grd_xy$GRID5KM_ID) %>%
     set_colnames(c("grd_x","grd_y","GRID5KM_ID"))
  
  # Spatially join traps to 5k grid, with grid/area matching key
  traps_g <- traps_sf %>%
    st_join(gkey) %>% 
    left_join(grd_xy,by="GRID5KM_ID")
  
  return(traps_g)
}

# Finally, a function to draw a map
# Function takes the output of the previous function, applies a correction for double counting
map_traps <- function(gridded_traps){
  summtraps <- gridded_traps %>% 
    st_set_geometry(NULL) %>%
    filter(!is.na(GRID5KM_ID)) %>% 
    # count the total number of traps in each grid cell in each set
    group_by(Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>% 
    summarise(ntraps_vessel_set_cell=n()) %>% 
    # average the number of pots per vessel per grid cell
    ungroup() %>% 
    group_by(Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
    summarise(ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
    # finally, sum the total traps per grid cell, across vessels
    ungroup() %>% 
    group_by(GRID5KM_ID,grd_x,grd_y,AREA) %>% 
    summarise(tottraps=sum(ntraps_vessel_cell)) %>% 
    # trap density is total traps divided by area (in sq. km) of each cell
    mutate(trapdens=tottraps/(AREA/1e6)) %>% 
    ungroup() %>% 
    filter(!is.na(tottraps))
  
  # CONFIDENTIALITY CHECK: RULE OF 3
  # "grey out" any cell for which there are less than 3 unique vessels contributing to that cell's data
  confidential_cells <- gridded_traps %>%
    st_set_geometry(NULL) %>% 
    group_by(GRID5KM_ID) %>% 
    summarise(nvessels=n_distinct(Vessel,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(is_confidential=ifelse(nvessels<3,T,F))
  
  summtraps %<>%
    left_join(confidential_cells,by="GRID5KM_ID") %>% 
    mutate(tottraps=ifelse(is_confidential,NA,tottraps),
           trapdens=ifelse(is_confidential,NA,trapdens))
  
  # Make a map
  # bbox=grd %>% filter(STATE %in% c("WA","OR")) %>% st_bbox()
  bbox = c(800000,1650000,1013103,1970000)
  
  map_out <- summtraps %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    scale_fill_viridis(na.value='grey70',option="C")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    labs(x='',y='',fill='Traps per\nsq. km',title='')+
    theme(axis.text.x.bottom = element_text(angle=45),
          legend.position = c(0.8,0.3),
          title=element_text(size=16),
          legend.title = element_text(size=10))
  return(map_out)
    
}

## USE BELOW TO TEST OUT THE ABOVE FUNCTIONS ###

testtraps <- place_traps(df=logs,bathy=bathy,year_choice = 2018,month_choice = 5,period_choice = 2)
test_traps_grid <- testtraps%>% join_grid(gkey=grd_area_key)
test_map<- test_traps_grid %>% map_traps()
test_map

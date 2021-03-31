## Mapping functions for WDFW logbook data
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(marmap)
library(viridis)
library(magrittr)
library(SpatialKDE)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)

# bathymetry is in decimeters

# if you have to load the logs you can do it here
# using the 2017-2018 slice provided by Leena
#Note that PrimaryLogbookPage can be of format e.g. "1009-1", so input as character not double
logs <- read_csv(here('wdfw','data','WDFW-Dcrab-logbooks-compiled_stackcoords_season20172018.csv'),
                 col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')

# QC: FishTicket1 of format Q999999 are landings into OR and have been entered into WA database because the vessel sent logbook copies. 
# These tickets should not be used in the data set because they would be part of the OR Dungeness crab fishery.
logs %<>% filter(FishTicket1 != "Q999999")


# QC: for each variable/column in the logs, count how many NA values there are
numNA <- logs %>% summarise(across(everything(),~sum(is.na(.x))))

# example bathymetry
bathy <- readGEBCO.bathy(file=here::here('wdfw','data','GEBCO_2019_-132.3021_50.6549_-116.6354_31.2799.nc'))
# example spatial grid
# 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','regions_master_final_lamb.shp'))
names(grd)

# background map (coastline)
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%  
  st_transform(st_crs(grd))

# Main steps for these functions
# 1. make strings into individual pots by segmentizing lines (sf::st_line_sample())
# 2. remove points on land by using a bathymetry layer
# 3. filter to desired year, month, and period (period is 1 or 2 for the first or second half of the month)
# 4. map, using kernel density estimation from SpatialKDE::kde()

# this function makes the traps and filters for bathymetry
# df is the logbooks dataframe
# bathy is a raster representation of bathymetry

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
  traps_depths <- traps %>%
    st_as_sf(coords=c('x','y'),crs=32610) %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    # use marmap to extract depth
    get.depth(bathy,.,locator=FALSE)
  
  traps %<>%
    bind_cols(traps_depths) %>% 
    # destroy traps placed on land or in greater than 100m of water
    # KEEP TRAPS IN CUSTOM LOOKUP TABLE
    filter(depth<0,depth>=-100) %>% 
    # add a unique trap identifier
    select(-id) %>% 
    mutate(trap_number=row_number())
  
  return(traps)  
}

# This next function spatially joins Blake's 5km grid (or some other grid) to the data
# trapsdf is the dataframe produced in the previous function, g is the desired grid, g_res is resolution in meters
# note: we lose a lot of observations here using Blake's grid (i.e., grid cells not assigned); not sure why
join_grid <- function(trapsdf,g,g_res=5000){
  # convert vms to spatial object (longitude/latitude)
  traps_sf <- trapsdf %>%
    st_as_sf(coords=c('lon','lat'),crs=4326) %>% 
    # then, convert to planar projection to match the grid
    st_transform(st_crs(g))
  
  # rasterize the grid
  # transform to a projection that uses meters
  grd_rast <- fasterize(g,raster = raster(g,res=g_res,crs=crs(g)),field="GRID5KM_ID")
  # set -99999 to NA
  grd_rast[grd_rast==-99999] <- NA
  
  grd_xy <- rasterToPoints(grd_rast) %>% as_tibble() %>% set_colnames(c("x","y","GRID5KM_ID")) %>%
    st_as_sf(coords=c('x','y'),crs=st_crs(grd))
  grd_xy <- grd_xy %>% st_coordinates() %>% as_tibble() %>% mutate(GRID5KM_ID=grd_xy$GRID5KM_ID) %>% 
    set_colnames(c("grd_x","grd_y","GRID5KM_ID"))
  
  # do the join
  traps_g <- traps_sf %>%
    st_join(g) %>% 
    left_join(grd_xy) %>% 
    mutate(cellarea_sqkm=(g_res/1000)^2)
}

# Finally, a function to draw a map
# Function takes the output of the previous function, applies a correction for double counting, then performs KDE
map_traps <- function(gridded_traps){
  summtraps <- gridded_traps %>% 
    st_set_geometry(NULL) %>% 
    filter(!is.na(GRID5KM_ID)) %>% 
    # count the total number of traps in each grid cell in each set
    group_by(Vessel,GRID5KM_ID,grd_x,grd_y,SetID,cellarea_sqkm) %>% 
    summarise(ntraps_vessel_set_cell=n()) %>% 
    # average the number of pots per vessel per grid cell
    ungroup() %>% 
    group_by(Vessel,GRID5KM_ID,grd_x,grd_y,cellarea_sqkm) %>% 
    summarise(ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
    # finally, sum the total traps per grid cell, across vessels
    ungroup() %>% 
    group_by(GRID5KM_ID,grd_x,grd_y,cellarea_sqkm) %>% 
    summarise(tottraps=sum(ntraps_vessel_cell)) %>% 
    # TRAP DENSITY IS TOTAL TRAPS DIVIDED BY THE AREA OF EA
    mutate(trapdens=tottraps/cellarea_sqkm) %>% 
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
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Traps per\nsq. km',title='')+
    theme(axis.text.x.bottom = element_text(angle=45),
          legend.position = c(0.8,0.3),
          title=element_text(size=16),
          legend.title = element_text(size=10))
  return(map_out)
    
}

## USE BELOW TO TEST OUT THE ABOVE FUNCTIONS ###

testtraps <- place_traps(df=logs,bathy=bathy,year_choice = 2018,month_choice = 2,period_choice = 2)
test_traps_grid <- testtraps%>% join_grid(g=grd)
test_map <- test_traps_grid %>% map_traps()
test_map

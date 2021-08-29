## This script has been modified from the 'original' mapping functions for WDFW logbook data to fit OR data:
#place_traps, join_grid() and original mapping function, looping through 2-week periods based on crab-year/month/period choices
#as well as code adjusted from the above mentioned functions to be run on 'full' data set (all logs 2013-2019)

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
library(gridExtra)
library(nngeo)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.text.x.bottom = element_text(angle=45),
        legend.position = c(0.8,0.3),
        title=element_text(size=12),
        legend.title = element_text(size=10),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)


#---------------------------------- 
#### READ IN LOGBOOK DATA ####
#Note that PrimaryLogbookPage can be of format e.g. "1009-1", so input as character not double
logs <- read_csv(here('wdfw', 'data','OR', 'ODFW-Dcrab-logbooks-compiled_stackcoords_license_2013-2018_2021-08-17.csv')) 
# fine to let R set col_types automatically
#dataset with all seasons - includes early seasons that were 100% entered
logs <- read_csv(here('wdfw', 'data','OR', 'ODFW-Dcrab-logbooks-compiled_stackcoords_license_2007-2018_20210830.csv')) 

# jameal
#logs <- read_csv("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Logbook-VMS/WA logbooks - mapping for CP/WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2019.csv",col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')


##FORMAT SETDATE COLUMN
# logs  <-  logs_raw %>% 
#   mutate(SetDate=as.Date(SetDate,"%d/%m/%Y"))


## IS THERE AN OR EQUIVALENT FOR THIS:
# QC: FishTicket1 of format Q999999 are landings into OR and have been entered into WA database because the vessel sent logbook copies. 
# These tickets should not be used in the data set because they would be part of the OR Dungeness crab fishery.
#logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") #use this to retain NAs until the next step

# QC: for each variable/column in the logs, count how many NA values there are
# numNA <- logs %>% summarise(across(everything(),~sum(is.na(.x))))


#### READ IN BATHYMETRY DATA ####
# bathymetry
bathy <- raster(here::here('wdfw','data','vms_composite_bath.txt'))
# jameal
#bathy <- raster("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Logbook-VMS/WA logbooks - mapping for CP/vms_composite_bath.txt")

# crop bathymetry to extent of logbook data
ex <- logs %>% select(lat,lon) %>% st_as_sf(coords=c('lon','lat'),crs=4326) %>% extent()
bathy <- bathy %>% crop(ex)


#### READ IN SPATIAL GRID DATA ####
# example spatial grid
# 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
names(grd)
# jameal
#grd <- read_sf("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid Apr 2021/fivekm_grid_polys_shore_lamb.shp")


# spatial area matching key of each grid cell (because the grid has been trimmed to the coastline)
# also matches to areas with specific port and embayment codes (NGDC_GRID) based on the bathymetry grid
grd_area_key <- grd %>% 
  select(GRID5KM_ID,NGDC_GRID,AREA) %>%
  mutate(is_port_or_bay=ifelse(NGDC_GRID==-999999,F,T))

# rasterized grid, for extracting evenly spaced centroid coordinates for later plotting
grd_r <- fasterize(grd_area_key,raster = raster(grd_area_key,res=5000,crs=crs(grd_area_key)),field="GRID5KM_ID")
grd_xy <- rasterToPoints(grd_r) %>% as_tibble() %>% set_colnames(c("x","y","GRID5KM_ID")) %>%
  st_as_sf(coords=c('x','y'),crs=st_crs(grd_area_key))
grd_xy <- grd_xy %>% st_coordinates() %>% as_tibble() %>% mutate(GRID5KM_ID=grd_xy$GRID5KM_ID) %>%
  set_colnames(c("grd_x","grd_y","GRID5KM_ID"))

# background map (coastline)
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%  
  st_transform(st_crs(grd))


## ARE THERE OR EQUIVALENTS FOR THESE:
#borders for 'static' WA management areas (MA), shapefile available on Kiteworks folder
#MA_shp <- read_sf(here::here('wdfw','data','WA_static_MA_borders.shp')) %>% 
#  st_transform(st_crs(grd)) #set same projection as the grid
#Note that Quinault SMA borders can move within seasons 
#borders for a 'default' borders, from:https://wdfw.wa.gov/fishing/commercial/crab/coastal/maps#quinault, + extra line for Joe Creek 
#shapefile available on Kiteworks folder
#QSMA_shp <- read_sf(here::here('wdfw','data','Quinault_SMA_border_default_LINE.shp')) %>% 
#  st_transform(st_crs(grd)) #set same projection as the grid
#---------------------------------- 

#####################
#Here is where user can decide whether they want 'confidential' maps or not
#When make_confidential_maps is TRUE, grid cells with fewer than 3 unique vessels contributing to that cell's data will be greyed out and won't show trap density value
#When make_confidential_maps is FALSE, all grid cells will be coloured based on trap density, regardless of the number of unique vessels in the cell
make_confidential_maps <- FALSE
#####################


#######################################
# Main steps for mapping functions
# 1. make strings into individual pots by segmentizing lines (sf::st_line_sample())
# 2. remove points on land by using a bathymetry layer
# 3. filter to desired year, month, and period (period is 1 or 2 for the first or second half of the month)
# 4. map

# this function makes the traps and filters for bathymetry
# df is the logbooks dataframe
# bathy is a raster representation of bathymetry



place_traps <- function(df,bathy,crab_year_choice,month_choice,period_choice){
  
  # labels for season, month, and period of choice
  mnth <- month.name[month_choice]
  p <- ifelse(period_choice==1,"first half","second half")
  
  df %<>%
    dplyr::select(season, Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
    # filter for the desired dates 
    mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
    filter(season%in%crab_year_choice,m%in%month_choice,period%in%period_choice) %>% 
    distinct() 
  
  # IF THE ABOVE FILTER COMES UP EMPTY, JUST RETURN A STRING VECTOR OF MONTH,SEASON, PERIOD
  if(nrow(df)==0) return(c(mnth,p,crab_year_choice))
  
  df %<>% 
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
  bathy.points <- raster::extract(bathy,traps_sf)
  
  # add depth as a column variable
  # divide by 10 to go from decimeters to meters
  traps_sf %<>%
    mutate(depth=bathy.points/10)
  
  # DEPTH FILTER
  # current code:
  # only remove those simulated pots that are on land or deeper than 200m, but also need to still retain the embayment data (highly negative values)
  traps_sf %<>% 
    filter(depth <= 0) %>% #keep points in water
    filter(depth > -200 | depth < -5000) #keep points shallower than 200m, but also those deeper than 5000m (ports/embayments)
  # ALTERNATIVE DEPTH FILTER OPTION
  # find points on land and deeper than 200m (while retaining very low values for ports/embayments) and collect their SetIDs to a list
  #traps_on_land <- traps_sf %>% filter(depth < -200 & depth > -5000 | depth > 0)
  #unique_SetIDs_on_land <- unique(traps_on_land$SetID)
  # Remove ALL points whose Set_ID appears on that list - assumption here is that if some points are on land/too deep the data is not trustworthy
  #traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)
  
  traps_sf %<>% mutate(month_name=mnth,period=p,season=crab_year_choice)
  
  return(traps_sf)  
}



# This next function spatially joins Blake's 5km grid (or some other grid) to the data
# It also joins the grid id matching key to identify ports and bays and assign correct grid cell areas
# traps_sf is the spatial (sf) dataframe produced in the previous function,gkey is the grid with matching key
join_grid <- function(traps_sf,gkey){
  
  # if the data are empty (i.e., no observations matching choice of month, period, season)
  # just return the grid with month, season, period labels (so we can still label the map later)
  
  if(is.character(traps_sf)) {
    
    empty_grd <- gkey %>% left_join(grd_xy,by="GRID5KM_ID") %>% 
      mutate(month_name=traps_sf[1],period=traps_sf[2],season=traps_sf[3])
    warning(paste0("Traps data frame is empty. Returning empty map for ",
                   traps_sf[2], " of ",traps_sf[1]," for season ",traps_sf[3]))
    
    return(empty_grd)
  }
  
  traps_sf %<>%
    # convert to planar projection to match the grid
    st_transform(st_crs(gkey))
  
  # Spatially join traps to 5k grid, with grid/area matching key
  traps_g <- traps_sf %>%
    st_join(gkey) %>% 
    left_join(grd_xy,by="GRID5KM_ID")
  
  return(traps_g)
}



# Finally, a function to draw a map
# Function takes the output of the previous function, applies a correction for double counting
map_traps <- function(gridded_traps){
  
  # labels for plot titles
  month_label=unique(gridded_traps$month_name)
  period_label=unique(gridded_traps$period)
  season_label=paste("Season:",unique(gridded_traps$season))
  t <- paste0(season_label,"\n",month_label,", ",period_label)
  
  # if the traps dataframe is empty, return a map with zeroes everywhere
  if(!('SetID' %in% names(gridded_traps))){
    bbox = c(800000,1650000,1013103,1970000)
    map_out <- gridded_traps %>% 
      st_set_geometry(NULL) %>% 
      mutate(trapdens=0) %>% 
      ggplot()+
      geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
      geom_sf(data=coaststates,col=NA,fill='gray50')+
      geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
      geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
      scale_fill_viridis(na.value='grey70',option="C")+
      coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
      labs(x='',y='',fill='Traps per\nsq. km',title=t)
    
    return(map_out)
  }
  
  summtraps <- gridded_traps %>% #This is assumption/adjustment method 1 (M1) for double counting of traps
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
  
  #If user wants maps where grids with <3 vessels are confidential, replace totrap and trapdens values with NAs
  #else just join dataframe with the confidential_cells, just so that info is retained in the working dataframe
  if(make_confidential_maps == TRUE){
    summtraps %<>%
      left_join(confidential_cells,by="GRID5KM_ID") %>% 
      mutate(tottraps=ifelse(is_confidential,NA,tottraps),
             trapdens=ifelse(is_confidential,NA,trapdens))
  } else {
    summtraps %<>%
      left_join(confidential_cells,by="GRID5KM_ID")
  }
  

  #change color scaling for all NA traps
  if(all(is.na(summtraps$trapdens))){
    bbox = c(800000,1650000,1013103,1970000)
    map_out <- gridded_traps  %>% 
      st_set_geometry(NULL) %>% 
      mutate(trapdens=NA) %>%
      ggplot()+
      geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=F,fill='gray70',alpha=0.8)+
      geom_sf(data=coaststates,col=NA,fill='gray50')+
      geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
      geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
      coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
      labs(x='',y='',fill='Traps per\nsq. km',title=t)
    return(map_out)
  } 
  
  # Make a map
  # bbox=grd %>% filter(STATE %in% c("WA","OR")) %>% st_bbox()
  bbox = c(800000,1650000,1013103,1970000)
  
  #max_trapdens <- 62.019  #something to still be adjusted, scaling of colours and legend
  
  map_out <- summtraps %>%
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C")+
    #scale_fill_continuous(low="blue", high="yellow",limits=c(0,55), breaks=c(0,55),labels=c("low (0)","high(55)"))+
    #scale_fill_viridis(limits=c(0,max_trapdens), breaks=c(0,max_trapdens),labels=c("low (0)","high(62)"),na.value='grey70',option="C")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    labs(x='',y='',fill='Traps per\nsq. km',title=t)

  return(map_out)
}


# if you want to use all of the functions together
make_effort_map <- function(df,bathy,crab_year_choice,month_choice,period_choice, gkey){
  
  traps <- place_traps(df,bathy,crab_year_choice,month_choice,period_choice) 
  
  traps_map <- traps %>% 
    join_grid(gkey) %>% 
    map_traps()
  
  return(traps_map)
}


# for a loop across multiple months or periods
# to order plots so that they start from December, re-order the scenarios tibble after it has been created:
scenarios <- crossing(crab_year_choice='2013-2014',month_choice=c(1:12),period_choice=1:2)
s1 <- scenarios[1:22,]
s2 <- scenarios[23:24,]
scenarios <- rbind(s2,s1)
#then run the functions
tm <- proc.time()
plts <- scenarios %>% pmap(.f=make_effort_map,df=logs,bathy=bathy,gkey=grd_area_key)
proc.time()-tm


#---------------------------------------------------------------
#Running above functions on 'full' logbook data sets 
#(i.e. not actually specifying the functions, or crab-year/month/period choices)

#Note that currently OR data only up to 2017-2018 season
#run adjusted version of place_traps() to retain 'License' (original place_traps() did not retain this column), but only on 2013-2018 data due to memory limits
logs2013_2018 <- logs %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) 

df <- logs2013_2018
df <- logs #no filtering, has early seasons when 100% logs where entered

# For now retain SpatialFlag column - can filter for that later 
# Note that OR logs had permit data joined in pre-processing stage
df %<>%
  dplyr::select(season,Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type, PermitNumber, Potlimit, SpatialFlag) %>%  
  distinct() 

df %<>% 
  # make sure each set has a beginning and end
  group_by(SetID) %>% 
  mutate(n=n()) %>% 
  filter(n==2,!is.na(PotsFished)) %>%
  # convert to sf points
  st_as_sf(coords=c('lon','lat'),crs=4326) %>% 
  st_transform(32610) %>% 
  # create linestrings
  group_by(Vessel,SetID,PotsFished,SetDate, PermitNumber, Potlimit, SpatialFlag) %>% #OR permit and potlimit done in pre-processing
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
  select(Vessel, SetID,PotsFished,SetDate, PermitNumber, Potlimit, SpatialFlag, trapcoords) %>% #Note that OR logs don't have License column
  # reorganize and unlist (i.e., make a dataframe where each row is an individual trap location)
  st_set_geometry(NULL) %>% 
  unnest(cols=c(trapcoords))

# find depth of traps
traps_sf <- traps %>%
  st_as_sf(coords=c('x','y'),crs=32610) %>% 
  st_transform(4326) %>%
  select(-id)

# do the raster extract with the bathymetry grid
bathy.points <- raster::extract(bathy,traps_sf)

# add depth as a column variable
# divide by 10 to go from decimeters to meters
traps_sf %<>%
  mutate(depth=bathy.points/10)

# DEPTH FILTER
# current code:
# only remove those simulated pots that are on land or deeper than 200m, but also need to still retain the embayment data (highly negative values)
traps_sf %<>% 
  filter(depth <= 0) %>% #keep points in water
  filter(depth > -200 | depth < -5000) #keep points shallower than 200m, but also those deeper than 5000m (ports/embayments)
# ALTERNATIVE DEPTH FILTER OPTION
# find points on land and deeper than 200m (while retaining very low values for ports/embayments) and collect their SetIDs to a list
#traps_on_land <- traps_sf %>% filter(depth < -200 & depth > -5000 | depth > 0)
#unique_SetIDs_on_land <- unique(traps_on_land$SetID)
# Remove ALL points whose Set_ID appears on that list - assumption here is that if some points are on land/too deep the data is not trustworthy
#traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)

#write_rds(traps_sf,here::here('wdfw', 'data','OR', "OR_traps_sf_license_all_logs_2013_2018.rds"))


gkey <- grd_area_key

traps_sf %<>%
  # convert to planar projection to match the grid
  st_transform(st_crs(gkey))

# Spatially join traps to 5k grid, with grid/area matching key
traps_g <- traps_sf %>%
  st_join(gkey) %>% 
  left_join(grd_xy,by="GRID5KM_ID")

#running everything on OR 2013-2018 logs subset took about 8min
#running everything on OR 2007-2010 + 2013-2018 logs subset took about 35min
#running everything on OR 2007-2018 logs  took about 40min
#write_rds(traps_g,here::here('wdfw', 'data','OR', "OR_traps_g_all_logs_2013_2018.rds"))
#write_rds(traps_g,here::here('wdfw', 'data','OR', "OR_traps_g_all_logs_2007_2018.rds"))

traps_g_SpatialFlag_filtered <- traps_g %>% 
  filter(SpatialFlag == FALSE)
#write_rds(traps_g_SpatialFlag_filtered,here::here('wdfw', 'data','OR', "OR_traps_g_all_logs_2013_2018_SpatialFlag_filtered.rds"))
#write_rds(traps_g_SpatialFlag_filtered,here::here('wdfw', 'data','OR', "OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds"))

#--------------------------------------------------------------------------------
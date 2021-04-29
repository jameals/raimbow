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


#### READ IN LOGBOOK DATA ####
#Note that PrimaryLogbookPage can be of format e.g. "1009-1", so input as character not double
# logs <- read_csv(here('wdfw','data','WDFW-Dcrab-logbooks-compiled_stackcoords_season20172018.csv'),
                 # col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2019.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
# jameal
logs <- read_csv("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Logbook-VMS/WA logbooks - mapping for CP/WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2019.csv",col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')

# QC: FishTicket1 of format Q999999 are landings into OR and have been entered into WA database because the vessel sent logbook copies. 
# These tickets should not be used in the data set because they would be part of the OR Dungeness crab fishery.
logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") #use this to retain NAs until the next step

#Filter for a crab_year
#logs20142015 <- logs %>% filter(season=='2014-2015')


# QC: for each variable/column in the logs, count how many NA values there are
# numNA <- logs %>% summarise(across(everything(),~sum(is.na(.x))))


#### READ IN BATHYMETRY DATA ####
# bathymetry
bathy <- raster(here::here('wdfw','data','vms_composite_bath.txt'))
# jameal
bathy <- raster("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Logbook-VMS/WA logbooks - mapping for CP/vms_composite_bath.txt")

# crop bathymetry to extent of logbook data
ex <- logs %>% select(lat,lon) %>% st_as_sf(coords=c('lon','lat'),crs=4326) %>% extent()
bathy <- bathy %>% crop(ex)


#### READ IN SPATIAL GRID DATA ####
# example spatial grid
# 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
names(grd)

# jameal
grd <- read_sf("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid Apr 2021/fivekm_grid_polys_shore_lamb.shp")
names(grd)

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



# use SetDate and season to define new columns designating month and month_interval 
logs %<>%
  mutate(
    m = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",m),
    month_interval = paste0(season_month, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
                            )
    )
# tail(data.frame(logs))



# make a summary df that represents the summed number of traps in WA during each interval as reported by PotsFished column in logbooks
#There are some cases where SetDate was NA, and therefore m ends up being NA too
dat <- logs %>% filter(!is.na(SetDate))

# interval<- season_month; regions<- NULL
# sum_traps <- function(dat,interval,regions){
  
  dat2 <-  dat %>% #dat[1:100,]
    distinct(SetID, .keep_all = TRUE) %>%
    #group_by(interval, regions) %>%
    group_by(season_month) %>%
    mutate(
      sum_traps=sum(PotsFished, na.rm=TRUE),
      sum_lost=sum(PotsLost, na.rm=TRUE)
    ) %>%
    distinct(season_month, .keep_all = TRUE) %>%
    select(
      season, season_month, m, month_interval, 
      sum_traps, sum_lost
    )
#   return(dat2)
# }
###PLOTTING TIME SERIES OF SUM TRAPS####
  dat3 <- dat2 %>%   
    filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
    mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
    mutate(sum_traps_1000 = sum_traps/1000)

 
  logs_ts <- ggplot(dat3, aes(x=m, y=sum_traps_1000, colour=season,  group=season))+
    geom_line(size=1.2) +
    scale_colour_brewer(palette = "PRGn") +
    #scale_colour_viridis_d(option = "plasma") + 
    ylab("Summed no. of pots\nfished (thousands)") +
    xlab("Month") +
    scale_y_continuous(breaks=seq(0, 600, 200),limits=c(0,600))+
    theme(legend.title = element_blank(),
          #title = element_text(size = 32),
          legend.text = element_text(size=12),
          axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = c(0.9, 0.8)
          )
  logs_ts
  
  png(here::here(
    "wdfw",
    "DRAFT_ts plot_sum no. of traps in WA_by season.png"), 
    width = 7.5, height = 5, units = "in", res = 300
    )
  logs_ts
  invisible(dev.off())

# head(logs$season)
# head(logs$SetDate)
# logs[1:5,] %>%
#   mutate(
#     m = month(SetDate, label=TRUE, abbr = FALSE),
#     season_month = paste0(season,"_",m),
#     month_interval = paste0(season_month,
#                             "_",
#                             ifelse(day(SetDate)<=15,1,2)
#                             )
#   ) %>%
#   select(
#     season_month, m, month_interval
#   )

# Main steps for these functions
# 1. make strings into individual pots by segmentizing lines (sf::st_line_sample())
# 2. remove points on land by using a bathymetry layer
# 3. filter to desired year, month, and period (period is 1 or 2 for the first or second half of the month)
# 4. map

# this function makes the traps and filters for bathymetry
# df is the logbooks dataframe
# bathy is a raster representation of bathymetry

df<- logs;crab_year_choice='2013-2014';month_choice=1;period_choice=1

# jameal makes a new place_traps_ts function for making the data frame used for making time series.
  # period_choice can take values of 1, 2, or 0 where 0 indicates the full month
place_traps_ts <- function(df,bathy,crab_year_choice,month_choice,period_choice){
  
  # labels for season, month, and period of choice
  mnth <- month.name[month_choice]
  p <- ifelse(period_choice==1,
              "first half",
              ifelse(period_choice == 2, "second half", "full month")
  )
  
  # if we want a month-level summary
  if(p == "full month"){
    df %<>%
    dplyr::select(season, Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
      mutate(m=month(SetDate), d=day(SetDate), period=0) %>%
      filter(season%in%crab_year_choice,m%in%month_choice) %>% 
    distinct() 
  } else {
  # if we want a period-level summary
    df %<>%
      #dplyr::select(Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
      dplyr::select(season, Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
      # filter for the desired dates 
      mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
      filter(season%in%crab_year_choice,m%in%month_choice,period%in%period_choice) %>% 
      distinct() 
  }
  
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
  # Note- I think this would be faster with a vector representation (points) of the bathy grid
  bathy.points <- raster::extract(bathy,traps_sf)
  
  # add depth as a column variable
  # divide by 10 to go from decimeters to meters
  traps_sf %<>%
    mutate(depth=bathy.points/10)
  
  # find points on land and collect their SetIDs to a list
  #traps_on_land <- traps_sf %>% filter(depth > 0) 
  # if want to also filter out pots at unreasonable depth, while retaining very low values for ports/embayments use something like
  traps_on_land <- traps_sf %>% filter(depth < -200 & depth > -5000 | depth > 0)
  unique_SetIDs_on_land <- unique(traps_on_land$SetID)
  
  # Remove ALL points whose Set_ID appears on that list
  traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)
  
  
  traps_sf %<>% mutate(month_name=mnth,period=p,season=crab_year_choice)
  
  return(traps_sf)  
}

place_traps <- function(df,bathy,crab_year_choice,month_choice,period_choice){
  
  # labels for season, month, and period of choice
  mnth <- month.name[month_choice]
  #mnth <- month.name[as.numeric(month_choice)]
  p <- ifelse(period_choice==1,"first half","second half")
  
  df %<>%
   #dplyr::select(Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
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
  # Note- I think this would be faster with a vector representation (points) of the bathy grid
  bathy.points <- raster::extract(bathy,traps_sf)
  
  # add depth as a column variable
  # divide by 10 to go from decimeters to meters
  traps_sf %<>%
    mutate(depth=bathy.points/10)
  
  # find points on land and collect their SetIDs to a list
  #traps_on_land <- traps_sf %>% filter(depth > 0) 
  # if want to also filter out pots at unreasonable depth, while retaining very low values for ports/embayments use something like
  traps_on_land <- traps_sf %>% filter(depth < -200 & depth > -5000 | depth > 0)
  unique_SetIDs_on_land <- unique(traps_on_land$SetID)
  
  # Remove ALL points whose Set_ID appears on that list
  traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)
  
  
  traps_sf %<>% mutate(month_name=mnth,period=p,season=crab_year_choice)
  
  return(traps_sf)  
}

# This next function spatially joins Blake's 5km grid (or some other grid) to the data
# It also joins the grid id matching key to identify ports and bays and assign correct grid cell areas
# trapssf is the spatial (sf) dataframe produced in the previous function,gkey is the grid with matching key

gkey <- grd_area_key

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



# make a summary df that represents the summed number of traps in each 5km grid cell during each monthly interval

traps_g2 <-  traps_g %>% #traps_g[1:100,]
  mutate(
    season_month = paste0(season,"_",month_name), 
    month_interval = paste0(season_month,"_",period)
  ) %>% 
  group_by(season_month, GRID5KM_ID) %>% 
  mutate(
    sum_traps = n(),
    num_vessels = length(unique(Vessel)) #add count of unique vessels
  ) %>%
  mutate(
    GRID5KM_ID_season_month = paste0(GRID5KM_ID,"_",season_month)
  ) %>%
  st_drop_geometry() %>%
  distinct(GRID5KM_ID_season_month, .keep_all = TRUE) %>%
  select(
    GRID5KM_ID, AREA, depth, NGDC_GRID, is_port_or_bay,
    season_month, month_name, 
    sum_traps, num_vessels
  )


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
      scale_fill_viridis(na.value='grey70',option="C")+
      coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
      labs(x='',y='',fill='Traps per\nsq. km',title=t)
    
    return(map_out)
  }
  
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

  ############################################################################# 
   # if the summtraps$trapdens is only NAs, return an empty map
  #Test 1
  # if(all(is.na(summtraps$trapdens))){
  #   bbox = c(800000,1650000,1013103,1970000)
  #   map_out <- gridded_traps  %>% 
  #    st_set_geometry(NULL) %>% 
  #    mutate(trapdens=0) %>% 
  #    ggplot()+
  #    geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
  #    geom_sf(data=coaststates,col=NA,fill='gray50')+
  #    scale_fill_viridis(na.value='grey70',option="C")+
  #    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  #    labs(x='',y='',fill='Traps per\nsq. km',title=t)
  # return(map_out)
  # }
  # 
  #Test 2
  # if(all(is.na(summtraps$trapdens))){
  #   bbox = c(800000,1650000,1013103,1970000)
  #   map_out <- gridded_traps  %>% 
  #     st_set_geometry(NULL) %>% 
  #     # mutate(trapdens=0) %>% 
  #     ggplot()+
  #     # geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
  #     geom_sf(data=coaststates,col=NA,fill='gray50')+
  #     scale_fill_viridis(na.value='grey70',option="C")+
  #     coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  #     labs(x='',y='',fill='Traps per\nsq. km',title=t)
  # return(map_out)
  # } 
  
  # Test 3: change color scaling for all NA traps
  if(all(is.na(summtraps$trapdens))){
    bbox = c(800000,1650000,1013103,1970000)
    map_out <- gridded_traps  %>% 
      st_set_geometry(NULL) %>% 
      mutate(trapdens=NA) %>%
      ggplot()+
      geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=F,fill='gray70',alpha=0.8)+
      geom_sf(data=coaststates,col=NA,fill='gray50')+
      coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
      labs(x='',y='',fill='Traps per\nsq. km',title=t)
    return(map_out)
  } 
  
  # Make a map
  # bbox=grd %>% filter(STATE %in% c("WA","OR")) %>% st_bbox()
  bbox = c(800000,1650000,1013103,1970000)
  
  map_out <- summtraps %>%
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    #scale_fill_continuous(low="blue", high="yellow",limits=c(0,55), breaks=c(0,55),labels=c("low (0)","high(55)"))+
    scale_fill_viridis(na.value='grey70',option="C")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    labs(x='',y='',fill='Traps per\nsq. km',title=t)
  
  # map_out <- summtraps %>% 
  #   ggplot()+
  #   geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
  #   geom_sf(data=coaststates,col=NA,fill='gray50')+
  #   scale_fill_continuous(low="blue", high="yellow", breaks = quantile(summtraps$trapdens, probs = c(0, 0.5, 1), na.rm=TRUE),na.value='grey70')+
  #   #scale_fill_viridis(na.value='grey70',option="C")+
  #   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  #   labs(x='',y='',fill='Traps per\nsq. km',title='')
  # map_out
  
  return(map_out)
    
}

## USE BELOW TO TEST OUT THE ABOVE FUNCTIONS ###
# if you want to use all of the functions together
make_effort_map <- function(df,bathy,crab_year_choice,month_choice,period_choice, gkey){
  
  traps <- place_traps(df,bathy,crab_year_choice,month_choice,period_choice) 
  
  traps_map <- traps %>% 
    join_grid(gkey) %>% 
    map_traps()
  
  return(traps_map)
}

#testtraps <- place_traps(df=logs,bathy=bathy,year_choice = 2018,month_choice = 5,period_choice = 2)
testtraps <- place_traps(df=logs,bathy=bathy,crab_year_choice = '2014-2015',month_choice = 9,period_choice = 2)
test_traps_grid <- testtraps%>% join_grid(gkey=grd_area_key)
test_map<- test_traps_grid %>% map_traps()
test_map

######
##A start for looping through different crab_year, month and period combinations

# make_effort_map <- function(df,bathy,crab_year_choice,month_choice,period_choice, gkey){
#   place_traps(df,bathy,crab_year_choice,month_choice,period_choice) %>% 
#     join_grid(gkey) %>% 
#     map_traps()
# }

# all together
t <- proc.time()
test_map <- make_effort_map(df=logs,bathy=bathy,crab_year_choice = '2014-2015',month_choice=9,period_choice=2,gkey = grd_area_key)
test_map
proc.time()-t

# for a loop across multiple months or periods
scenarios <- crossing(crab_year_choice=unique(logs$season),month_choice=1:4,period_choice=1:2)
plts <- scenarios %>% pmap(.f=make_effort_map,df=logs,bathy=bathy,gkey=grd_area_key)

# for a loop across multiple months and periods  
#scenarios <- crossing(crab_year_choice=unique(logs$season),month_choice=1:8,period_choice=1:2)

scenarios <- crossing(crab_year_choice='2018-2019',month_choice=c(1:12),period_choice=1:2)
#month_list <- c('12','1','2','3','4','5','6','7','8','9','10','11')
#month_list <- factor(month_list, levels = c('12','1','2','3','4','5','6','7','8','9','10','11'))
#scenarios <- crossing(crab_year_choice='2014-2015',month_choice=month_list,period_choice=1:2)
tm <- proc.time()
plts <- scenarios %>% pmap(.f=make_effort_map,df=logs,bathy=bathy,gkey=grd_area_key)
proc.time()-tm



###EXPORT IN PDF
ggsave(
  filename = "2017-2018_Jan-Aug.pdf", 
  plot = marrangeGrob(plts, nrow=1, ncol=1), 
  width = 15, height = 9
)


######Manually setting legend limits
#Use summary(summtraps$trapdens) to get the max value for the selected season (e.g.2017-2018) (because there are NAs max() just returns NA)
#Replace the final part of the map_traps function with this, where you use the max trapdens to set the limits and labels
#I couldn't get the limits and labels to work within the scale_fill_viridis()
map_out <- summtraps %>% 
  ggplot()+
  geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
  geom_sf(data=coaststates,col=NA,fill='gray50')+
  scale_fill_continuous(low="blue", high="yellow",limits=c(0,115), breaks=c(0,115),labels=c("low","high"))+
  #scale_fill_viridis(na.value='grey70',option="C")+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  labs(x='',y='',fill='Traps per\nsq. km',title='')+
  theme(axis.text.x.bottom = element_text(angle=45),
        legend.position = c(0.8,0.3),
        title=element_text(size=16),
        legend.title = element_text(size=10))
return(map_out)




####################################
#Max trap density for full seasons
#season       max(trapdens)
# 2013-2014     62.019
# 2014-2015     55.2899
# 2015-2016     46.904
# 2016-2017     40.304
# 2017-2018     37.8061 -- note that actually was 113.3558 but it was a clear outlier
# 2018-2019     54.5422
max_trapdens <- 62.019

#Running functions on full seasons (no month or period choices) -- no need to dela with empty df etc as full season won't have that problem
#subset a season
logs20182019 <- logs %>% filter(season=='2018-2019')
df <- logs20182019

#place_traps_full <- function(df,bathy,crab_year_choice){
  
  # labels for season, month, and period of choice
  season_label1 <- unique(df$season)
  #season_label1 <- crab_year_choice
  
  df %<>%
    dplyr::select(season, Vessel,SetID,lat,lon,PotsFished,SetDate,coord_type) %>% 
    #filter(season%in%crab_year_choice) %>% 
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
  #traps_on_land <- traps_sf %>% filter(depth > 0) 
  # if want to also filter out pots at unreasonable depth, while retaining very low values for ports/embayments use something like
  traps_on_land <- traps_sf %>% filter(depth < -200 & depth > -5000 | depth > 0)
  unique_SetIDs_on_land <- unique(traps_on_land$SetID)
  
  # Remove ALL points whose Set_ID appears on that list
  traps_sf %<>% dplyr::filter(!SetID %in% unique_SetIDs_on_land)
  
  traps_sf %<>% mutate(season=season_label1)
  
#  return(traps_sf)  
#}


  gkey <- grd_area_key
#  join_grid_full <- function(traps_sf,gkey){
    
    traps_sf %<>%
      # convert to planar projection to match the grid
      st_transform(st_crs(gkey))
    
    # Spatially join traps to 5k grid, with grid/area matching key
    traps_g <- traps_sf %>%
      st_join(gkey) %>% 
      left_join(grd_xy,by="GRID5KM_ID")
    
#    return(traps_g)
#  }

  gridded_traps <- traps_g
#    map_traps_full <- function(gridded_traps){
      
      # labels for plot titles
      season_label=paste("Full season:",unique(gridded_traps$season))
      t <- paste0(season_label)
      
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
        #max_trapdens across all seasons from 2013-2019 is 62.019
        #scale_fill_continuous(low="darkblue", high="yellow",limits=c(0,max_trapdens), breaks=c(0,max_trapdens),labels=c("low (0)","high(62)"), na.value='grey70')+
        scale_fill_viridis(limits=c(0,max_trapdens), breaks=c(0,max_trapdens),labels=c("low (0)","high(62)"),na.value='grey70',option="C")+
        coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
        labs(x='',y='',fill='Traps per\nsq. km',title=t)
    
     
#      return(map_out)
# }  

      
# make_effort_map_full <- function(df,bathy,crab_year_choice, gkey){
#         
#  traps <- place_traps_full(df,bathy,crab_year_choice) 
#         
#  traps_map <- traps %>% 
#   join_grid_full(gkey) %>% 
#   map_traps_full()
#         
#  return(traps_map)
# }



#test_map <- make_effort_map_full(df=logs,bathy=bathy,crab_year_choice = '2013-2014',gkey = grd_area_key)
#test_map

# scenarios <- crossing(crab_year_choice=c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))
# tm <- proc.time()
# plts <- scenarios %>% pmap(.f=make_effort_map_full,df=logs,bathy=bathy,gkey=grd_area_key)
# proc.time()-tm
      
      
      
###EXPORT IN PDF
 ggsave(
  filename = "DRAFT_constant legend across seasons_full season_2013-2014.pdf", 
  plot = map_out, 
  width = 15, height = 9
  )      

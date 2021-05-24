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

#borders for 'static' WA management areas (MA), shapefile available on Kiteworks folder
MA_shp <- read_sf(here::here('wdfw','data','WA_static_MA_borders.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid

#Note that Quinault SMA borders have moved a lot, including within seasons 
#borders for a 'default' borders, from:https://wdfw.wa.gov/fishing/commercial/crab/coastal/maps#quinault, shapefile available on Kiteworks folder
QSMA_shp <- read_sf(here::here('wdfw','data','Quinault_SMA_border_default_LINE.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid


#####################
#Here is where user can decide whether they want 'confidential' maps or not
#When make_confidential_maps is TRUE, grid cells with fewer than 3 unique vessels contributing to that cell's data will be greyed out and won't show trap density value
#When make_confidential_maps is FALSE, all grid cells will be coloured based on trap density, regardless of the number of unique vessels in the cell
make_confidential_maps <- FALSE
#####################


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


#######################################
# make a summary df that represents the summed number of traps in WA during each interval as reported by PotsFished column in logbooks. 
#This is not very good as it overcounts traps!!
#There are some cases where SetDate was NA, and therefore m ends up being NA too
# dat <- logs %>% filter(!is.na(SetDate))

# interval<- season_month; regions<- NULL
# sum_traps <- function(dat,interval,regions){
  
#   dat2 <-  dat %>% #dat[1:100,]
#     distinct(SetID, .keep_all = TRUE) %>%
#     #group_by(interval, regions) %>%
#     group_by(season_month) %>%
#     mutate(
#       sum_traps=sum(PotsFished, na.rm=TRUE),
#       sum_lost=sum(PotsLost, na.rm=TRUE)
#     ) %>%
#     distinct(season_month, .keep_all = TRUE) %>%
#     select(
#       season, season_month, m, month_interval, 
#       sum_traps, sum_lost
#     )
# #   return(dat2)
# # }
# ###PLOTTING TIME SERIES OF SUM TRAPS####
#   dat3 <- dat2 %>%   
#     filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
#     mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
#     mutate(sum_traps_1000 = sum_traps/1000)
# 
#  
#   logs_ts <- ggplot(dat3, aes(x=m, y=sum_traps_1000, colour=season,  group=season))+
#     geom_line(size=1.2) +
#     scale_colour_brewer(palette = "PRGn") +
#     #scale_colour_viridis_d(option = "plasma") + 
#     ylab("Summed no. of pots\nfished (thousands)") +
#     xlab("Month") +
#     scale_y_continuous(breaks=seq(0, 600, 200),limits=c(0,600))+
#     theme(legend.title = element_blank(),
#           #title = element_text(size = 32),
#           legend.text = element_text(size=12),
#           axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
#           axis.text.y = element_text(size = 12),
#           axis.title = element_text(size = 12),
#           legend.position = c(0.9, 0.8)
#           )
#   logs_ts
#   
#   png(here::here(
#     "wdfw",
#     "DRAFT_ts plot_sum no. of traps in WA_by season.png"), 
#     width = 7.5, height = 5, units = "in", res = 300
#     )
#   logs_ts
#   invisible(dev.off())







#######################################
# Main steps for mapping functions
# 1. make strings into individual pots by segmentizing lines (sf::st_line_sample())
# 2. remove points on land by using a bathymetry layer
# 3. filter to desired year, month, and period (period is 1 or 2 for the first or second half of the month)
# 4. map

# this function makes the traps and filters for bathymetry
# df is the logbooks dataframe
# bathy is a raster representation of bathymetry

df<- logs;crab_year_choice='2013-2014';month_choice=1;period_choice=0


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
  #mnth <- month.name[as.numeric(month_choice)] #trying to reorder plots
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


###########################################
# make a summary df that represents the summed number of traps in each 5km grid cell during each monthly interval

# traps_g2 <-  traps_g[1:100,] %>% #traps_g[1:100,]
#   mutate(
#     season_month = paste0(season,"_",month_name), 
#     month_interval = paste0(season_month,"_",period)
#   ) %>% 
#   group_by(season_month, GRID5KM_ID) %>% 
#   mutate(
#     sum_traps = n(),
#     num_vessels = length(unique(Vessel)) #add count of unique vessels
#   ) %>%
#   mutate(
#     GRID5KM_ID_season_month = paste0(GRID5KM_ID,"_",season_month)
#   ) %>%
#   st_drop_geometry() %>%
#   distinct(GRID5KM_ID_season_month, .keep_all = TRUE) %>%
#   select(
#     GRID5KM_ID, AREA, depth, NGDC_GRID, is_port_or_bay,
#     season_month, month_name, 
#     sum_traps, num_vessels
#   )


#getting traps_g for full logs takes a long time to run, so saved it as RDS, which can be found in Kiteworks folder
traps_g_for_all_logs_full_seasons <- read_rds(here::here('wdfw', 'data','traps_g_for all logs full seasons.rds'))
traps_g <- traps_g_for_all_logs_full_seasons

traps_g <- traps_g %>% 
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

#For now look at 2013-2019
traps_g <- traps_g %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))


#OPTION 1: group by season_month 
summtraps5km <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month, Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>% 
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month, Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    tottraps=sum(ntraps_vessel_cell),
    num_vessels = length(unique(Vessel)) #add count of unique vessels
  ) %>% 
  # trap density is total traps divided by area (in sq. km) of each cell
  mutate(
    trapdens=tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(tottraps))
glimpse(summtraps5km)

# now we want a summary for each season_month based on the above for all of WA
summtrapsWA <- summtraps5km %>%
  group_by(season_month) %>%  
  summarise(
    tottraps = sum(tottraps),
    number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    meantrapdens = mean(trapdens),
    sdtrapdens = sd(trapdens),
    mediantrapdens = median(trapdens),
    percentile_975th = quantile(trapdens, probs=0.975, na.rm=TRUE),
    percentile_75th = quantile(trapdens, probs=0.75, na.rm=TRUE),
    percentile_25th = quantile(trapdens, probs=0.25, na.rm=TRUE),
    percentile_025th = quantile(trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(summtrapsWA)

summtrapsWA <- summtrapsWA %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) 

#could look into using bins (categorical variable) to specify line width in plot (curently continuous variable)
#summtrapsWA <- summtrapsWA %>% 
#mutate(number_obs_bins = cut(number_obs, breaks = c(0,50,100,150,200,250,300,350,400,450)),
#       number_obs_bins = as.factor(number_obs_bins))
#and then in plotting code change geom_line call to this:
#geom_line(aes(size=factor(number_obs_bins)))
#the problem is that with lots of bins it's hard to tell the width difference between them - unless can manually edit the widths...

#PLOT for Option 1
logs_ts <- ggplot(summtrapsWA, aes(x= month_name, y= meantrapdens, colour=season,  group=season))+
  #make line width reflect the area/no. of grid cells used
  geom_line(aes(size=totarea),lineend = "round") + #size=number_obs; size=totarea
  scale_colour_brewer(palette = "PRGn") +
  #scale_colour_viridis_d(option = "plasma") + 
  ylab("Mean of trapdens across \ngrid cells for entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
logs_ts


#OPTION 2: group by season_month_interval -- not as good as option 1
summtraps5km <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month_interval, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    tottraps=sum(ntraps_vessel_cell),
    num_vessels = length(unique(Vessel)) #add count of unique vessels
  ) %>% 
  # trap density is total traps divided by area (in sq. km) of each cell
  mutate(
    trapdens=tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(tottraps))
glimpse(summtraps5km)

# now we want a summary for each season_month_interval based on the above for all of WA
summtrapsWA <- summtraps5km %>%
  group_by(season_month_interval) %>%  
  summarise(
    tottraps = sum(tottraps),
    number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    meantrapdens = mean(trapdens),
    sdtrapdens = sd(trapdens),
    mediantrapdens = median(trapdens),
    percentile_975th = quantile(trapdens, probs=0.975, na.rm=TRUE),
    percentile_75th = quantile(trapdens, probs=0.75, na.rm=TRUE),
    percentile_25th = quantile(trapdens, probs=0.25, na.rm=TRUE),
    percentile_025th = quantile(trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(summtrapsWA)

summtrapsWA <- summtrapsWA %>%
  separate(season_month_interval, into = c("season", "month_name", "period"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) %>% 
  mutate(season_month_interval = paste0(season_month,"_",period)) %>% 
  mutate(month_interval = paste0(month_name,"_",period)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2')))

#PLOT for Option 2 - not very good looking, lines overlap too much
logs_ts <- ggplot(summtrapsWA, aes(x= month_interval, y= mediantrapdens, colour=season,  group=season))+
  #make line width reflect the area/no. of grid cells used
  geom_line(aes(size=number_obs),lineend = "round") + #size=number_obs; size=totarea
  scale_colour_brewer(palette = "PRGn") +
  #scale_colour_viridis_d(option = "plasma") + 
  ylab("Median of trapdens across \ngrid cells for entire WA") +
  xlab("Month_1st or 2nd half") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
logs_ts

#if want to export as png
png(here::here(
  "wdfw",
  "DRAFT_mean of trapdens across grid cells for entire WA.png"), 
  width = 7.5, height = 5, units = "in", res = 300
)
logs_ts
invisible(dev.off())



#subset a season
#logs20182019 <- logs %>% filter(season=='2018-2019')
#df <- logs20182019
#Run first 2 functions adjusted for full seasons (no month or period choices) at the bottom of script to get traps_g for the subset season
# traps_g3 <-  traps_g %>% #traps_g[1:100,]
#   mutate(
#     month_name = month(SetDate, label=TRUE, abbr = FALSE),
#     season_month = paste0(season,"_",month_name), 
#     #month_interval = paste0(season_month,"_",period)
#     ) %>% 
#   st_set_geometry(NULL) %>%
#   filter(!is.na(GRID5KM_ID)) %>% 
#   # count the total number of traps in each grid cell in each set
#   group_by(Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA, season_month) %>% #,month_interval
#   summarise(ntraps_vessel_set_cell=n()) %>% 
#   # average the number of pots per vessel per grid cell
#   ungroup() %>% 
#   group_by(Vessel,GRID5KM_ID,grd_x,grd_y,AREA, season_month) %>% #,month_interval
#   summarise(ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
#   # finally, sum the total traps per grid cell, across vessels
#   ungroup() %>% 
#   group_by(GRID5KM_ID,grd_x,grd_y,AREA, season_month) %>% #,month_interval
#   summarise(tottraps=sum(ntraps_vessel_cell)) %>% 
#   # trap density is total traps divided by area (in sq. km) of each cell
#   mutate(trapdens=tottraps/(AREA/1e6)) %>% 
#   ungroup() %>% 
#   mutate(GRID5KM_ID_season_month = paste0(GRID5KM_ID,"_",season_month)) %>% 
#   filter(!is.na(tottraps)) %>% 
#   separate(season_month, into = c("season", "month_name"), sep = "_") %>%
#   mutate(season_month = paste0(season,"_",month_name))
# 
# traps_g4 <-  traps_g3 %>%
#   group_by(season_month) %>% 
#   summarise(trap_sum_month = sum(tottraps)) %>% 
#   #mutate(trap_sum_month = sum(tottraps))
#   mutate(season_month = factor(season_month,
#                                   levels = c('2018-2019_December',
#                                              '2018-2019_January',
#                                              '2018-2019_February',
#                                              '2018-2019_March',
#                                              '2018-2019_April',
#                                              '2018-2019_May',
#                                              '2018-2019_June',
#                                              '2018-2019_July',
#                                              '2018-2019_August',
#                                              '2018-2019_September',
#                                              '2018-2019_October',
#                                              '2018-2019_November')))
# 
# ts_plots <- ggplot(traps_g4, aes(x=season_month, y=trap_sum_month))+ #aim for x=month and group_by season_month...?
#   geom_point(size=1.5) +
#   ylab("Total no. of traps") +
#   xlab("Season_Month") 
#   
# ts_plots
# #this seems to produce somethin g much closer to wdfw CP plot of lines in water







######################################
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
    # summtraps %<>%
  #   left_join(confidential_cells,by="GRID5KM_ID") %>% 
  #   mutate(tottraps=ifelse(is_confidential,NA,tottraps),
  #          trapdens=ifelse(is_confidential,NA,trapdens))

  
  
  # Test 3: change color scaling for all NA traps
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
  
  max_trapdens <- 62.019
  
  map_out <- summtraps %>%
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
    #scale_fill_continuous(low="blue", high="yellow",limits=c(0,55), breaks=c(0,55),labels=c("low (0)","high(55)"))+
    #scale_fill_viridis(na.value='grey70',option="C")+
    scale_fill_viridis(limits=c(0,max_trapdens), breaks=c(0,max_trapdens),labels=c("low (0)","high(62)"),na.value='grey70',option="C")+
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
#testtraps <- place_traps(df=logs,bathy=bathy,crab_year_choice = '2014-2015',month_choice = 9,period_choice = 2)
#test_traps_grid <- testtraps%>% join_grid(gkey=grd_area_key)
#test_map<- test_traps_grid %>% map_traps()
#test_map




##################
##A start for looping through different crab_year, month and period combinations

# all together
#t <- proc.time()
#test_map <- make_effort_map(df=logs,bathy=bathy,crab_year_choice = '2014-2015',month_choice=9,period_choice=2,gkey = grd_area_key)
#test_map
#proc.time()-t

# for a loop across multiple months or periods
#scenarios <- crossing(crab_year_choice=unique(logs$season),month_choice=1:4,period_choice=1:2)
#plts <- scenarios %>% pmap(.f=make_effort_map,df=logs,bathy=bathy,gkey=grd_area_key)
#scenarios <- crossing(crab_year_choice='2018-2019',month_choice=c(1:12),period_choice=1:2)

#testing re-ordering of plots. If make month numbers as factors, scenario list is accurate (starts from 12, then 1,2...)
#BUT in plts, first map IS first half of Dec BUT the label is first half of Jan
#month_list <- c('12','1','2','3','4','5','6','7','8','9','10','11')
#month_list <- factor(month_list, levels = c('12','1','2','3','4','5','6','7','8','9','10','11'))
#scenarios <- crossing(crab_year_choice='2013-2014',month_choice=month_list,period_choice=1:2)

##What seems to work for re-ordering plots is to re-order the scenarios tibble after it has been created:
scenarios <- crossing(crab_year_choice='2013-2014',month_choice=c(1:12),period_choice=1:2)
s1 <- scenarios[1:22,]
s2 <- scenarios[23:24,]
scenarios <- rbind(s2,s1)
#then run the functions
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
  geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
  geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
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
        geom_sf(data=MA_shp,col="black", size=1, fill=NA)+
        geom_sf(data=QSMA_shp,col="black", linetype = "11", size=1.1, fill=NA)+
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

 
 
#########################################
#Plotting (simulated) pots by depth bin
 
#use the full set of logbook data for this (no breakdown by particular year or period etc) 
#from raw logs, need to run place_traps function to create the traps along each line and get their depth
#running place_traps on the full logs dataframe takes really long, so ran it once and saved it as traps_sf_for_all_logs_and_seasons_2009-2019.rds
#and uploaded it to Kiteworks

traps_sf_all_logs <- read_rds(here::here('wdfw', 'data','traps_sf_for_all_logs_and_seasons_2009-2019.rds'))
 
logs_all <- traps_sf_all_logs %>% 
 st_set_geometry(NULL) %>% 
 mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
 mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
 mutate(season = str_sub(SetID,1,9)) %>% 
 mutate(season_month = paste0(season,"_",m))
#dataset has higly negative values (~ -30000) to denote port and bay areas - remove those. 
#Also note that place_traps function already removes depths <200m as crab fishing at deeper depths is not likely
logs_all %<>% filter(depth > -1000)
 
logs_all %<>% mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) 

#Testing some plotting

ggplot(logs_all, aes(depth)) +
  geom_histogram()

#simple histogram by season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth)) +
    geom_histogram(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list


#simple bar chart by season, by depth bin
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth)) +
    geom_bar() +
    scale_x_binned() +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list 


#stacked histogram of depth by month in season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth, fill = m)) +
    geom_histogram(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i]))) +
    theme(legend.position = c(0.3,0.6))
  plot_list[[i]] = p
}
plot_list
 

#frequency polygon of depth by month in season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth, colour = m)) +
    geom_freqpoly(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i]))) +
    theme(legend.position = c(0.3,0.6))
  plot_list[[i]] = p
}
plot_list

# bar chart of proportions instead of counts by season, by depth bin
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i])) +
    geom_bar(aes(x=depth, y=stat(prop))) +
    scale_x_binned() +
    scale_y_continuous(breaks=seq(0.0, 0.5, 0.1),limits=c(0.0,0.5))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
  }
plot_list 

#to save to a pdf.
pdf("NAME.pdf")
for (i in 1:length(ids)) {
  print(plot_list[[i]])
}
dev.off()


#########################################
#Investigate the average pots per vessel per 2wk interval

#Start with traps_g df for all seasons (traps are simulated and joined to grid)
#getting traps_g for full logs takes a long time to run, so saved it as RDS, which can be found in Kiteworks folder

#the next lines, up to filtering for years, are same as the first steps in making a ts plot
traps_g_for_all_logs_full_seasons <- read_rds(here::here('wdfw', 'data','traps_g_for all logs full seasons.rds'))
traps_g <- traps_g_for_all_logs_full_seasons

traps_g <- traps_g %>% 
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

#For now look at 2013-2019
traps_g <- traps_g %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))

#modifying the summtraps code that adjust for double counting
testdf <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
  # finally, sum the total traps per Vessel, across all grid cells in the 2-week period in question
  ungroup() %>% 
  group_by(season_month_interval, Vessel) %>% 
  summarise(
    adjusted_tottraps=sum(ntraps_vessel_cell))
glimpse(testdf)


#bring in raw logs - code at the beginning of script
#sum raw PotsFished, and get info on how many landings a vessel did in a 2-week period
logsdf <- logs %>% 
  mutate(
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

#For now look at 2013-2019
logsdf <- logsdf %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) 

#Sum the raw/unadjusted PotsFished for vessel by 2-week period
testdf2  <- logsdf %>% 
  distinct(SetID, .keep_all= TRUE) %>% #note in raw logs data are repeated - has a row for start and end of stringline
  group_by(season_month_interval, Vessel) %>%  
  summarise(
    sum_PotsFished=sum(PotsFished,na.rm=T) #need to include na.rm=T statement
  )
glimpse(testdf2)


logsdf <- logsdf %>% 
  # count the total number of fishtickets landed by vessel in 2-week period
  group_by(season_month_interval, Vessel) %>%  
  summarise(
    count_FishTicket=n_distinct(FishTicket1)) 
glimpse(logsdf)


#join the datasets
testdf %<>% 
  left_join(testdf2,by=c("season_month_interval","Vessel")) %>% 
  left_join(logsdf, by=c("season_month_interval","Vessel")) %>% 
  separate(season_month_interval, into = c("season", "month_name", "interval"), sep = "_") %>%
  filter(!is.na(month_name)) %>% 
  mutate(month_interval = paste0(month_name,"_",interval)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2'))) %>% 
  arrange(Vessel,season, month_interval)
glimpse(testdf)



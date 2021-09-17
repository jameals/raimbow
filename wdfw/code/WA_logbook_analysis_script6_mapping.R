## WA logbook analysis 
# making maps on 2-weekly step
# making maps on 1-monthly step
# making summary maps for May 1-Sep 15 period

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

#------------------------------------------------------------------------------

## Read in all data and shapefiles for mapping

# Cleaned and summarized, simulated crab trap data
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2013_2020.rds'))

# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
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
grd_xy <- grd_xy %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(GRID5KM_ID=grd_xy$GRID5KM_ID) %>%
  set_colnames(c("grd_x","grd_y","GRID5KM_ID"))

# background map (coastline)
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%  
  st_transform(st_crs(grd))

# borders for 'static' WA management areas (MA)
MA_shp <- read_sf(here::here('wdfw','data','WA_static_MA_borders.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid

# Note that Quinault SMA borders can move within seasons, but that is not incorporated here
# static or 'default' borders for Quinault SMA, from:https://wdfw.wa.gov/fishing/commercial/crab/coastal/maps#quinault
QSMA_shp <- read_sf(here::here('wdfw','data','Quinault_SMA_border_default_LINE.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid

#------------------------------------------------------------------------------

# # we want to compare M1 (old summary) vs. M2 (new weighted density)
# adj_summtraps %>%
#   #sample_n(1000) %>%
#   ggplot(aes(M1_trapdens,M2_trapdens))+
#   geom_point()+
#   geom_abline(slope=1,intercept=0)+
#   annotate("text",x=20,y=10,label="1:1 Line")+
#   labs(x="Old Summary",y="New Summary")

#------------------------------------------------------------------------------
# If want to create non-confidential maps (do not show data if < 3 vessels in grid)
### NOTE THAT THIS IS NOT ACTUALLY CORRECT - NEED TO BRING IN POINTS DATA AND COUNT UNIQUE VESSELS THAT WAY
### SEE SCRIPT FOR LOG_VMS_COMPARISONS_FEIST_ET_AL
adj_summtraps <- adj_summtraps %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F))

# use conf_dat as input in mapping loop, if want cells with < 3 vessels to be gray
conf_dat <-  adj_summtraps %>% 
  mutate(M1_tottraps=ifelse(is_confidential,NA,M1_tottraps),
         M1_trapdens=ifelse(is_confidential,NA,M1_trapdens),
         M2_tottraps=ifelse(is_confidential,NA,M2_tottraps),
         M2_trapdens=ifelse(is_confidential,NA,M2_trapdens)
  )

# or use conf_dat2 as input in the above mapping loop, if want cells with < 3 vessels to be fully removed
conf_dat2 <-  conf_dat %>%
  filter(is_confidential == FALSE)
#------------------------------------------------------------------------------

# Making maps on a 2-weekly time step

# currently mapping all data, including grid cells with < 3 vessels (i.e. confidential data)
# adjust the max value in colour scale depending on whether you want to focus on one season, or compare different seasons 
# change input file if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)


# Figure out good trap density scale
adj_summtraps %>% 
  ggplot()+
  geom_density(aes(M2_trapdens))
# adj_summtraps %>% 
#   ggplot()+
#   geom_density(aes(M1_trapdens))


map_traps <- function(gridded_traps,saveplot=TRUE){
  
  # labels for plot titles
  month_label=unique(gridded_traps$month_name)
  period_label <- ifelse(gridded_traps$period==1,"first half", "second half")  
  season_label=paste("Season:",unique(gridded_traps$season))
  t1 <- paste0(season_label,"\n",month_label,", ",period_label, " Method 1")
  t2 <- paste0(season_label,"\n",month_label,", ",period_label, " Method 2")

  bbox = c(800000,1650000,1013103,1970000)

  # include this if want comparison maps of M1 and M2 methods  
  # M1_map_out <- gridded_traps %>% 
  #   ggplot()+
  #   geom_tile(aes(grd_x,grd_y,fill=M1_trapdens),na.rm=T,alpha=0.8)+
  #   geom_sf(data=coaststates,col=NA,fill='gray50')+
  #   geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
  #   geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
  #   scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80),oob=squish)+
  #   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
  #   labs(x='',y='',fill='Traps per\nsq. km',title=t1)
  
  M2_map_out <- gridded_traps %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,120),breaks=c(0,30,60,90,120),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    #if you do NOT want to show lat/lon lines on the map, use the below line instead:
    #coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Traps per\nsq. km',title=t2)

  # comparison maps of M1 and M2 methods   
  #map_out <- plot_grid(M1_map_out,M2_map_out,nrow=1)
  # M2 only
  map_out <- M2_map_out
  # saving
  if(saveplot){
    pt <- unique(gridded_traps$season_month_interval)
    ggsave(here('wdfw','maps',paste0(pt,'.png')),map_out,w=6,h=5)
  }
  return(map_out)
}

# Loop and save maps
# change input file here if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)
tm <- proc.time()
all_maps <- purrr::map(unique(adj_summtraps$season_month_interval),function(x){
  adj_summtraps %>% 
    filter(season_month_interval==x) %>% 
    map_traps()
})
proc.time()-tm


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Making maps on a 1-monthly time step

# this will require using the df on a 2-weekly step (adj_summtraps) and
# taking the average trap density for each grid cell for the desired time period
M2_summtrapsWA_month <- adj_summtraps %>% 
  group_by(season_month,GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M1_trapdens = mean(M1_trapdens), 
    mean_M2_trapdens = mean(M2_trapdens), 
    #M1_sdtrapdens = sd(M1_trapdens), 
    #M2_sdtrapdens = sd(M2_trapdens)
  ) 
glimpse(M2_summtrapsWA_month)


M2_summtrapsWA_month %>% 
  ggplot()+
  geom_density(aes(mean_M2_trapdens))


#making a loop of maps on monthly step
map_log_monthly <- function(M2_summtrapsWA_month,saveplot=TRUE){
  
  # labels for plot titles
  season_month_label=unique(M2_summtrapsWA_month$season_month)
  
  bbox = c(800000,1650000,1013103,1970000)
  
  log_monthly_map_out <- M2_summtrapsWA_month %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=mean_M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,120),breaks=c(0,30,60,90,120),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    #if you do NOT want to show lat/lon lines on the map, use the below line instead:
    #coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='mean trap density\nper sq. km',title=season_month_label)
  
  # saving
  if(saveplot){
    pt <- unique(M2_summtrapsWA_month$season_month)
    ggsave(here('wdfw','maps',paste0(pt,'.png')),log_monthly_map_out,w=6,h=5)
  }
  return(log_monthly_map_out)
}

# Loop and save comparison maps
tm <- proc.time()
all_maps <- purrr::map(unique(M2_summtrapsWA_month$season_month),function(x){
  M2_summtrapsWA_month %>% 
    filter(season_month==x) %>% 
    map_log_monthly()
})
proc.time()-tm

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Making summary maps for May - Sep 15 period

## Cleaned and summarized, simulated crab trap data
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2013_2020.rds'))

# create a column in df to indicate whether data fall between May1 and Sep15
# the 'periods' included between May1 and Sep15 are:
# May_1, May-2, June_1, June_2, July_1, July_2, AUgust_1, August_2, September_1

adj_summtraps_MaySep <- adj_summtraps %>% 
  mutate(is_May1_Sep15 = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1')
                  ,'Y', 'N')) %>% 
  filter(is_May1_Sep15 == 'Y')

# average M1 and M2 trap density for each grid cell for May-Sep period
MaySep_summtrapsWA <- adj_summtraps_MaySep %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    sum_nvessels = sum(nvessels), # include this for creating non-confidential maps 
    number_obs = n(), #no. of grid cells being used for averaging
    mean_M1_trapdens = sum_M1_trapdens/number_obs,
    mean_M2_trapdens = sum_M2_trapdens/number_obs
    #here can include some measure of variance or CV as well
    #M2_sdtrapdens = sd(M2_trapdens),
    #M2_mediantrapdens = median(M2_trapdens),
    #M2_percentile_975th = quantile(M2_trapdens, probs=0.975, na.rm=TRUE),
    #M2_percentile_75th = quantile(M2_trapdens, probs=0.75, na.rm=TRUE),
    #M2_percentile_25th = quantile(M2_trapdens, probs=0.25, na.rm=TRUE),
    #M2_percentile_025th = quantile(M2_trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(MaySep_summtrapsWA)

#--------------------------
# If want to create non-confidential maps (do not show data if < 3 vessels in grid)
### NOTE THAT THIS IS NOT ACTUALLY CORRECT - NEED TO BRING IN POINTS DATA AND COUNT UNIQUE VESSELS THAT WAY
### SEE SCRIPT FOR LOG_VMS_COMPARISONS_FEIST_ET_AL
conf_MaySep_summtrapsWA <- MaySep_summtrapsWA %>%
  mutate(is_confidential=ifelse(sum_nvessels<3,T,F))

# use conf_MaySep_summtrapsWA as input in mapping loop, if want cells with < 3 vessels to be gray
conf_MaySep_summtrapsWA <- conf_MaySep_summtrapsWA %>% 
  mutate(mean_M1_trapdens = ifelse(is_confidential,NA,mean_M1_trapdens),
         mean_M2_trapdens = ifelse(is_confidential,NA,mean_M2_trapdens)
  )

# or use conf_MaySep_summtrapsWA2 as input in mapping loop, if want cells with < 3 vessels to be fully removed
conf_MaySep_summtrapsWA2 <-  conf_MaySep_summtrapsWA %>%
  filter(is_confidential == FALSE)
#----------------------------

# map May 1- Sep 15
# currently for M2 only, but can be edited to map M1 as well

# Figure out good trap density scale
MaySep_summtrapsWA %>%
  ggplot()+
  geom_density(aes(mean_M2_trapdens))


# change input file if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)

map_maysep <- function(MaySep_summtrapsWA,saveplot=TRUE){
  
  # labels for plot titles
  season_label=unique(MaySep_summtrapsWA$season)
  
  bbox = c(800000,1650000,1013103,1970000)
  
  MaySep_map_out <- MaySep_summtrapsWA %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=mean_M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,50),breaks=c(0, 25,50),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    #if you do NOT want to show lat/lon lines on the map, use the below line instead:
    #coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Avg. trap density\nper sq. km',title=paste0('May 1 - Sep 15\n',season_label))
  
  # saving
  if(saveplot){
    pt <- unique(MaySep_summtrapsWA$season)
    ggsave(here('wdfw','maps',paste0('May 1 - Sep 15 ',pt,'.png')),MaySep_map_out,w=6,h=5)
  }
  return(MaySep_map_out)
}

# Loop and save maps
# change input file here if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)
tm <- proc.time()
all_maps <- purrr::map(unique(MaySep_summtrapsWA$season),function(x){
  MaySep_summtrapsWA %>% 
    filter(season==x) %>% 
    map_maysep()
})
proc.time()-tm




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




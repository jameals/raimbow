# Map outputs of logbook crab trap simulation exercise
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

## Cleaned and summarized, simulated crab trap data
dat <- read_rds(here::here('wdfw','data','adj_summtraps.rds'))
#### READ IN SPATIAL GRID DATA ####
# example spatial grid
# 5x5 grid shapefile
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

#borders for 'static' WA management areas (MA), shapefile available on Kiteworks folder
MA_shp <- read_sf(here::here('wdfw','data','WA_static_MA_borders.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid

#Note that Quinault SMA borders have moved a lot, including within seasons 
#borders for a 'default' borders, from:https://wdfw.wa.gov/fishing/commercial/crab/coastal/maps#quinault, shapefile available on Kiteworks folder
QSMA_shp <- read_sf(here::here('wdfw','data','Quinault_SMA_border_default_LINE.shp')) %>% 
  st_transform(st_crs(grd)) #make it have same projection as the grid

# we want to compare M1 (old summary) vs. M2 (new weighted density)
dat %>% 
  sample_n(1000) %>% 
  ggplot(aes(M1_trapdens,M2_trapdens))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  annotate("text",x=20,y=10,label="1:1 Line")+
  labs(x="Old Summary",y="New Summary")

## Figure out good trap density scale
dat %>% 
  ggplot()+
  geom_density(aes(M2_trapdens))
dat %>% 
  ggplot()+
  geom_density(aes(M1_trapdens))

# Function takes the output of the previous function, applies a correction for double counting
map_traps <- function(gridded_traps,saveplot=TRUE){
  
  # labels for plot titles
  month_label=unique(gridded_traps$month_name)
  period_label=unique(gridded_traps$period)
  season_label=paste("Season:",unique(gridded_traps$season))
  t1 <- paste0(season_label,"\n",month_label,", ",period_label, " Method 1")
  t2 <- paste0(season_label,"\n",month_label,", ",period_label, " Method 2")

  bbox = c(800000,1650000,1013103,1970000)
  
  M1_map_out <- gridded_traps %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=M1_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Traps per\nsq. km',title=t1)
  
  M2_map_out <- gridded_traps %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Traps per\nsq. km',title=t2)
  
  map_out <- plot_grid(M1_map_out,M2_map_out,nrow=1)
  # saving
  if(saveplot){
    pt <- unique(gridded_traps$season_month_interval)
    ggsave(here('wdfw','maps',paste0(pt,'.png')),map_out,w=6,h=5)
  }
  return(map_out)
}

# Loop and save comparison maps
tm <- proc.time()
all_maps <- purrr::map(unique(dat$season_month_interval),function(x){
  dat %>% 
    filter(season_month_interval==x) %>% 
    map_traps()
})
proc.time()-tm



####################################################################
#VMS comparison maps
#note that you have to read in MA borders at the top of script
#speed and depth filtered vms is output from pipeline step 5, naming convention yearmatched_fitered.rds
vms20132014_raw <- read_rds(here::here('wdfw','data','2014matched_filtered.rds'))

##There are few filtering options we could do with VMS:
#vms agency_code == W but no target sp filter
#vms agency_code == W AND DCRB target (lbs OR rev) filter
#no agency-code filter but DCRB target (lbs OR rev) filter
#For now going to focus on vms agency_code == W AND DCRB target (lbs OR rev) filter

vms20132014_W <- vms20132014_raw %>% filter(agency_code=='W') 

#filter to be only vms trips that targeted DCRB
vms20132014_W_DCRB <- vms20132014_W %>% 
  mutate(Target_lbs_match = ifelse(TARGET_lbs=="DCRB", 'keep', 'remove')) %>% 
  mutate(Target_rev_match = ifelse(TARGET_rev=="DCRB", 'keep', 'remove'))
vms20132014_W_DCRB <- vms20132014_W_DCRB %>% 
  mutate(Targetmatch = ifelse(Target_lbs_match=="keep"|Target_rev_match=="keep", 'keep', 'remove'))
vms20132014_W_DCRB <- vms20132014_W_DCRB %>% filter(Targetmatch=='keep') 

#join vms to grid -- vms needs to be sf
gkey <- grd_area_key

#W vms only, filtered by target sp
vms20132014_sf_DCRB <- vms20132014_W_DCRB %>%
  st_as_sf(coords=c('X_COORD','Y_COORD'),crs=32610) %>% 
  st_transform(4326) 

vms20132014_sf_DCRB %<>%
  # convert to planar projection to match the grid
  st_transform(st_crs(gkey))

# Spatially join traps to 5k grid, with grid/area matching key
vms20132014_g_DCRB <- vms20132014_sf_DCRB %>%
  st_join(gkey) %>% 
  left_join(grd_xy,by="GRID5KM_ID")

vms20132014_g_DCRB <- vms20132014_g_DCRB %>% 
  mutate(
    season = str_sub(westcoastdate_notime,1,4),
    month_name = month(westcoastdate_notime, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(westcoastdate_notime)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )


vms20132014_g_mapping_DCRB <- vms20132014_g_DCRB %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of VMS points in each grid cell 
  group_by(season_month, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(n_vms_points_cell=n()) %>% 
  # vms point density is total vms points divided by area (in sq. km) of each cell
  mutate(vms_dens=n_vms_points_cell/(AREA/1e6)) %>% 
  ungroup() %>% 
  filter(!is.na(n_vms_points_cell))
glimpse(vms20132014_g_mapping_DCRB)


# #making individual maps
# d_DCRB <- vms20132014_g_mapping_DCRB %>% 
#   filter(season_month == '2014_January')
# 
# bbox = c(800000,1650000,1013103,1970000)
# 
# vms_map_DCRB <- d_DCRB %>% 
#   ggplot()+
#   geom_tile(aes(grd_x,grd_y,fill=log(vms_dens)),na.rm=T,alpha=0.8)+
#   geom_sf(data=coaststates,col=NA,fill='gray50')+
#   geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
#   geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
#   scale_fill_viridis(na.value='grey70',option="D")+
#   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
#   labs(x='',y='',fill='log VMS points\nper sq. km',title='W only, DCRB target sp trips only')
# vms_map_DCRB


#making a loop of maps
map_vms <- function(vms_g_mapping,saveplot=TRUE){
  
  # labels for plot titles
  season_month_label=unique(vms_g_mapping$season_month)
  
  bbox = c(800000,1650000,1013103,1970000)
  
  vms_map_out <- vms_g_mapping %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=log(vms_dens)),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="D")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='log VMS points\nper sq. km',title=season_month_label)
  
  # saving
  if(saveplot){
    pt <- unique(vms_g_mapping$season_month)
    ggsave(here('wdfw','vms_maps',paste0('vms',pt,'.png')),vms_map_out,w=6,h=5)
  }
  return(vms_map_out)
}

# Loop and save comparison maps
tm <- proc.time()
all_maps <- purrr::map(unique(vms20132014_g_mapping_DCRB$season_month),function(x){
  vms20132014_g_mapping_DCRB %>% 
    filter(season_month==x) %>% 
    map_vms()
})
proc.time()-tm




#logbook maps on monthly step - average trap density in grid cell for the time period

adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps.rds'))

M2_summtrapsWA_test <- adj_summtraps %>% 
  group_by(season_month,GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    sum_M1_tottraps = sum(M1_tottraps), 
    sum_M2_tottraps = sum(weighted_traps), 
    mean_M1_trapdens = mean(M1_trapdens), 
    mean_M2_trapdens = mean(M2_trapdens), 
    M1_sdtrapdens = sd(M1_trapdens), 
    M2_sdtrapdens = sd(M2_trapdens)
  ) 
glimpse(M2_summtrapsWA_test)


# #making individual maps
# d_test <- M2_summtrapsWA_test %>% 
#   filter(season_month == '2013-2014_January')
# 
# bbox = c(800000,1650000,1013103,1970000)
# 
# test_map <- d_test %>% 
#   ggplot()+
#   geom_tile(aes(grd_x,grd_y,fill=M2_meantrapdens),na.rm=T,alpha=0.8)+
#   geom_sf(data=coaststates,col=NA,fill='gray50')+
#   geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
#   geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
#   scale_fill_viridis(na.value='grey70',option="C")+
#   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
#   labs(x='',y='',fill='mean trap density\nper sq. km',title='')
# test_map


#making a loop of maps
map_log_monthly <- function(M2_summtrapsWA_test,saveplot=TRUE){
  
  # labels for plot titles
  season_month_label=unique(M2_summtrapsWA_test$season_month)
  
  bbox = c(800000,1650000,1013103,1970000)
  
  log_monthly_map_out <- M2_summtrapsWA_test %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=M2_meantrapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='mean trap density\nper sq. km',title=season_month_label)
  
  # saving
  if(saveplot){
    pt <- unique(M2_summtrapsWA_test$season_month)
    ggsave(here('wdfw','vms_maps',paste0(pt,'.png')),log_monthly_map_out,w=6,h=5)
  }
  return(log_monthly_map_out)
}

# Loop and save comparison maps
tm <- proc.time()
all_maps <- purrr::map(unique(M2_summtrapsWA_test$season_month),function(x){
  M2_summtrapsWA_test %>% 
    filter(season_month==x) %>% 
    map_log_monthly()
})
proc.time()-tm

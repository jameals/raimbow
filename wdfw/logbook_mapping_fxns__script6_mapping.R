# Map outputs of logbook crab trap simulation exercise
library(tidyverse)
library(sf)
library(viridis)
library(cowplot)
library(here)
library(rnaturalearth)


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
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80))+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Traps per\nsq. km',title=t1)
  
  M2_map_out <- gridded_traps %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80))+
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
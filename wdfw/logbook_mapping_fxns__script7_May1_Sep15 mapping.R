## Mapping functions for WDFW logbook data 
# making summary maps for May 1- Sep 15 period

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


## Cleaned and summarized, simulated crab trap data
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps.rds'))

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

#create a column in df to indicate whether data fall between May1 and Sep15
#the 'periods' included between May1 and Sep15 are:
#May_1, May-2, June_1, June_2, July_1, July_2, AUgust_1, August_2, September_1
adj_summtraps_MaySep <- adj_summtraps %>% 
  mutate(is_May1_Sep15 = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1')
         ,'Y', 'N')) %>% 
  filter(is_May1_Sep15 == 'Y')


#average M1 and M2 trap density for each grid cell
MaySep_summtrapsWA <- adj_summtraps_MaySep %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_M1_trapdens = sum_M1_trapdens/number_obs,
    mean_M2_trapdens = sum_M2_trapdens/number_obs
    #include some measure of variance or CV as well
    #M2_sdtrapdens = sd(M2_trapdens),
    #M2_mediantrapdens = median(M2_trapdens),
    #M2_percentile_975th = quantile(M2_trapdens, probs=0.975, na.rm=TRUE),
    #M2_percentile_75th = quantile(M2_trapdens, probs=0.75, na.rm=TRUE),
    #M2_percentile_25th = quantile(M2_trapdens, probs=0.25, na.rm=TRUE),
    #M2_percentile_025th = quantile(M2_trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(MaySep_summtrapsWA)


#then map May 1- Sep 15
map_maysep <- function(MaySep_summtrapsWA,saveplot=TRUE){
  
  # labels for plot titles
  season_label=unique(MaySep_summtrapsWA$season)
  
  bbox = c(800000,1650000,1013103,1970000)
 
  MaySep_map_out <- MaySep_summtrapsWA %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=mean_M1_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0, 20, 40,60,80),oob=squish)+
    #scale_fill_viridis(na.value='grey70',option="C")+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    labs(x='',y='',fill='average trap density\nper sq. km',title=paste0('May 1 - Sep 15\n',season_label))

  # saving
  if(saveplot){
    pt <- unique(MaySep_summtrapsWA$season)
    ggsave(here('wdfw','may_sep_maps',paste0('May 1 - Sep 15 ',pt,'.png')),MaySep_map_out,w=6,h=5)
  }
  return(MaySep_map_out)
}

# Loop and save comparison maps
tm <- proc.time()
all_maps <- purrr::map(unique(MaySep_summtrapsWA$season),function(x){
  MaySep_summtrapsWA %>% 
    filter(season==x) %>% 
    map_maysep()
})
proc.time()-tm






#crab_year_choice = '2018-2019'
#dat_test <- adj_summtraps_MaySep %>% 
#  filter(season == crab_year_choice)

## Figure out good trap density scale
#MaySep_summtrapsWA %>% 
#  ggplot()+
#  geom_density(aes(mean_M1_trapdens))
#MaySep_summtrapsWA %>% 
#  ggplot()+
#  geom_density(aes(mean_M2_trapdens))
#max avg trap density by season
#   13-14 14-15 15-16 16-17 17-18 18-19
#M1 13.2  39.7  14.0  10.6  83.1  22.5
#M2 37.9  26.7  25.9  44.0  94.5  85.7

bbox = c(800000,1650000,1013103,1970000)

MaySep_map_out <- MaySep_summtrapsWA %>% 
  ggplot()+
  geom_tile(aes(grd_x,grd_y,fill=mean_M2_trapdens),na.rm=T,alpha=0.8)+
  geom_sf(data=coaststates,col=NA,fill='gray50')+
  geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
  geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
  scale_fill_viridis(na.value='grey70',option="C",limits=c(0,80),breaks=c(0,20,40,60,80),oob=squish)+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  labs(x='',y='',fill='average trap density\nper sq. km',title=paste0('May 1 - Sep 15\n',crab_year_choice))
MaySep_map_out


ggsave(
  filename = "2017-2018_Jan-Aug.pdf", 
  plot = marrangeGrob(plts, nrow=1, ncol=1), 
  width = 15, height = 9
)
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
#adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2013_2020.rds'))
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds'))

#Grid ID 122919 has very high trap density (244pots/km2) in May 2013-2014 season
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
adj_summtraps <- adj_summtraps %>% filter(M2_trapdens < 244)


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
# adj_summtraps <- adj_summtraps %>%
#   mutate(is_confidential=ifelse(nvessels<3,T,F))
# 
# # use conf_dat as input in mapping loop, if want cells with < 3 vessels to be gray
# conf_dat <-  adj_summtraps %>% 
#   mutate(M1_tottraps=ifelse(is_confidential,NA,M1_tottraps),
#          M1_trapdens=ifelse(is_confidential,NA,M1_trapdens),
#          M2_tottraps=ifelse(is_confidential,NA,M2_tottraps),
#          M2_trapdens=ifelse(is_confidential,NA,M2_trapdens)
#   )
# 
# # or use conf_dat2 as input in the above mapping loop, if want cells with < 3 vessels to be fully removed
# conf_dat2 <-  conf_dat %>%
#   filter(is_confidential == FALSE)


##### THIS IS THE CORRECT WAY ##### 
# This section is copied and edited from SCRIPT FOR LOG_VMS_COMPARISONS_FEIST_ET_AL
# If want to create non-confidential maps (do not show data if < 3 vessels in grid)

#I think this is the correct way of figuring if more than 3 unique vessels were in a grid cell in a given period
#bring in data as points, not summarised by grid cell
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2020.rds'))

#for each point record assign the month_interval as per Feist et al Fig 2
logs_all_x <- traps_g_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(period2=ifelse(d<=15,1,2)) %>%
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m)) %>% 
  mutate(month_interval = paste0(m,"_",period2))

#count number of unique vessels that used a given grid cell within a given time step
logs_all_nvessels <- logs_all_x %>% 
  group_by(season, month_interval, GRID5KM_ID,grd_x,grd_y) %>% 
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 

#join the new, correct, number of unique vessels in a grid in an interval into gridded df
adj_summtraps %<>%
  left_join(logs_all_nvessels,by=c("season", "month_interval","GRID5KM_ID", "grd_x", "grd_y"))

#If fewer than 3 unique vessels in a grid, that should be removed
adj_summtraps <- adj_summtraps %>%
  mutate(is_confidential=ifelse(nvessels.y<3,T,F))

# use conf_dat as input in mapping loop, if want cells with < 3 vessels to be gray
conf_dat <-  adj_summtraps %>% 
  mutate(M1_tottraps=ifelse(is_confidential,NA,M1_tottraps),
         M1_trapdens=ifelse(is_confidential,NA,M1_trapdens),
         M2_tottraps=ifelse(is_confidential,NA,M2_tottraps),
         M2_trapdens=ifelse(is_confidential,NA,M2_trapdens)
  )

# or use conf_dat2 as input in the mapping loop, if want cells with < 3 vessels to be fully removed
conf_dat2 <-  conf_dat %>%
  filter(is_confidential == FALSE)



#----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Making maps on a 2-weekly time step

# currently mapping all data, including grid cells with < 3 vessels (i.e. confidential data)
# adjust the max value in colour scale depending on whether you want to focus on one season, or compare different seasons 
# change input file if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)


# Figure out good trap density scale
adj_summtraps %>% 
#conf_dat2 %>% 
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

# test calculating number of grid cells in use in a given month
test <- M2_summtrapsWA_month %>% 
  filter(season_month == '2017-2018_January')
test_2 <- test %>% distinct(GRID5KM_ID, .keep_all = TRUE)
nrow(test_2) #172 for 2017-2018_January, 80 for 2017-2018_July

#--------------------------
#MAKING NON_CONFIDENTIAL MONTHLY MAPS
# THIS IS THE CORRECT WAY
# This section is copied and edited from SCRIPT FOR LOG_VMS_COMPARISONS_FEIST_ET_AL
# If want to create non-confidential maps (do not show data if < 3 vessels in grid)

#I think this is the correct way of figuring if more than 3 unique vessels were in a grid cell in a given period
#bring in data as points, not summarised by grid cell
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2020.rds'))

#for each point record assign the month_interval as per Feist et al Fig 2
logs_all_x <- traps_g_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(period2=ifelse(d<=15,1,2)) %>%
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m)) %>% 
  mutate(month_interval = paste0(m,"_",period2))

#count number of unique vessels that used a given grid cell within a given time step
logs_all_nvessels <- logs_all_x %>% 
  group_by(season_month, GRID5KM_ID,grd_x,grd_y) %>% 
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 

#join the new, correct, number of unique vessels in a grid in an interval into gridded df
adj_summtraps %<>%
  left_join(logs_all_nvessels,by=c("season_month", "GRID5KM_ID", "grd_x", "grd_y"))

#If fewer than 3 unique vessels in a grid, that should be removed
adj_summtraps <- adj_summtraps %>%
  mutate(is_confidential=ifelse(nvessels.y<3,T,F))  %>%
  filter(is_confidential == FALSE)


# this will require using the df on a 2-weekly step (adj_summtraps) and
# taking the average trap density for each grid cell for the desired time period
conf_M2_summtrapsWA_month <- adj_summtraps %>% 
  group_by(season_month,GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M1_trapdens = mean(M1_trapdens), 
    mean_M2_trapdens = mean(M2_trapdens), 
    #M1_sdtrapdens = sd(M1_trapdens), 
    #M2_sdtrapdens = sd(M2_trapdens)
  ) 
glimpse(conf_M2_summtrapsWA_month)

conf_M2_summtrapsWA_month %>% 
  ggplot()+
  geom_density(aes(mean_M2_trapdens))


# test calculating number of grid cells in use in a given month
test <- conf_M2_summtrapsWA_month %>% 
  filter(season_month == '2017-2018_July')
test_2 <- test %>% distinct(GRID5KM_ID, .keep_all = TRUE)
nrow(test_2) #117 for 2017-2018_January, 26 for 2017-2018_July


#if try to look at % of pots lost, instead of grid cells ---- unsure if this is correct, might be double counting things...
logs_all_x %<>%
  left_join(logs_all_nvessels,by=c("season_month", "GRID5KM_ID", "grd_x", "grd_y"))

logs_all_x2 <- logs_all_x %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F))

test_no_of_pots <- logs_all_x2 %>% 
  group_by(season_month) %>% 
  summarise(n_pots = n())

test_no_pots_confidential <- logs_all_x2 %>% 
  filter(is_confidential==T) %>% 
  group_by(season_month) %>% 
  summarise(n_pots_confidential = n())

test_no_pots_non_confidential <- logs_all_x2 %>% 
  filter(is_confidential==F) %>% 
  group_by(season_month) %>% 
  summarise(n_pots_non_confidential = n())

joined <- left_join(test_no_of_pots,test_no_pots_confidential, by="season_month")
joined2 <- left_join(joined,test_no_pots_non_confidential, by="season_month")

joined3 <- joined2 %>% mutate(ratio = n_pots_non_confidential/n_pots)

#--------------------------

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

# Loop and save comparison maps - here replace input with non-confidential verison to make non-confidential maps
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
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds'))

# create a column in df to indicate whether data fall between May1 and Sep15
# the 'periods' included between May1 and Sep15 are:
# May_1, May-2, June_1, June_2, July_1, July_2, AUgust_1, August_2, September_1

adj_summtraps_MaySep <- adj_summtraps %>% 
  mutate(is_May1_Sep15 = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1')
                  ,'Y', 'N')) %>% 
  filter(is_May1_Sep15 == 'Y') %>% 
  select(-nvessels)

#here filter with N to make dec-apr (winter) maps
adj_summtraps_DecApr <- adj_summtraps %>% 
  mutate(is_May1_Sep15 = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1')
                  ,'Y', 'N')) %>% 
  #here filter with Y to make May-Sep (summer) maps, or with N to make dec-apr (winter) maps
  filter(is_May1_Sep15 == 'N') %>% 
  select(-nvessels)




# average M1 and M2 trap density for each grid cell for May-Sep period
MaySep_summtrapsWA <- adj_summtraps_MaySep %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    #sum_nvessels = sum(nvessels), # include this for creating non-confidential maps - old, this is wrong
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

# average M1 and M2 trap density for each grid cell for Dec-Apr period
DecApr_summtrapsWA <- adj_summtraps_DecApr %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    #sum_nvessels = sum(nvessels), # include this for creating non-confidential maps - old, this is wrong
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
glimpse(DecApr_summtrapsWA)




#--------------------------

#THIS IS THE CORRECT WAY

#bring in data as points, not summarised by grid cell
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2020.rds'))

#for each point record assign the correct month_interval time step required (e.g. May1-Sep15)
logs_all_x <- traps_g_all_logs %>% 
  st_set_geometry(NULL) %>% 
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
  ) %>% 
  mutate(is_May1_Sep15 = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1')
                  ,'Y', 'N')) %>% 
  #here filter with Y to make May-Sep (summer) maps, or with N to make dec-apr (winter) maps
  #filter(is_May1_Sep15 == 'Y')
  filter(is_May1_Sep15 == 'N')

#count number of unique vessels that used a given grid cell within a given time step
logs_all_nvessels <- logs_all_x %>% 
  group_by(season, month_interval, GRID5KM_ID,grd_x,grd_y) %>% 
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 


#join the new, correct, number of unique vessels in a grid in an interval into gridded df
adj_summtraps_MaySep %<>%
  left_join(logs_all_nvessels,by=c("season", "month_interval","GRID5KM_ID", "grd_x", "grd_y"))

adj_summtraps_DecApr %<>%
  left_join(logs_all_nvessels,by=c("season", "month_interval","GRID5KM_ID", "grd_x", "grd_y"))



conf_adj_summtraps_MaySep <- adj_summtraps_MaySep %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F)) %>%
  filter(is_confidential == FALSE)

conf_adj_summtraps_DecApr <- adj_summtraps_DecApr %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F)) %>%
  filter(is_confidential == FALSE)



# average M1 and M2 trap density for each grid cell for May-Sep period
cof_MaySep_summtrapsWA <- conf_adj_summtraps_MaySep %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    #sum_nvessels = sum(nvessels), # include this for creating non-confidential maps - old, this is wrong
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
glimpse(cof_MaySep_summtrapsWA)

# average M1 and M2 trap density for each grid cell for Dec-Apr period
cof_DecApr_summtrapsWA <- conf_adj_summtraps_DecApr %>%
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    #sum_nvessels = sum(nvessels), # include this for creating non-confidential maps - old, this is wrong
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
glimpse(cof_DecApr_summtrapsWA)

# use cof_MaySep_summtrapsWA as input in mapping loop, if want cells with < 3 vessels to be fully removed
# use cof_DecApr_summtrapsWA for winter version

#----------------------------

# map May 1- Sep 15
# currently for M2 only, but can be edited to map M1 as well

# Figure out good trap density scale
MaySep_summtrapsWA %>%
#cof_MaySep_summtrapsWA %>%
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



# map Dec - Apr
# currently for M2 only, but can be edited to map M1 as well

# Figure out good trap density scale
DecApr_summtrapsWA %>%
  #cof_DecApr_summtrapsWA %>%
  ggplot()+
  geom_density(aes(mean_M2_trapdens))


# change input file if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)

map_decapr <- function(DecApr_summtrapsWA,saveplot=TRUE){
  
  # labels for plot titles
  season_label=unique(DecApr_summtrapsWA$season)
  
  bbox = c(800000,1650000,1013103,1970000)
  
  DecApr_map_out <- DecApr_summtrapsWA %>% 
    ggplot()+
    geom_tile(aes(grd_x,grd_y,fill=mean_M2_trapdens),na.rm=T,alpha=0.8)+
    geom_sf(data=coaststates,col=NA,fill='gray50')+
    geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
    geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
    scale_fill_viridis(na.value='grey70',option="C",limits=c(0,50),breaks=c(0, 25,50),oob=squish)+
    coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
    #if you do NOT want to show lat/lon lines on the map, use the below line instead:
    #coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
    labs(x='',y='',fill='Avg. trap density\nper sq. km',title=paste0('Dec - Apr\n',season_label))
  
  # saving
  if(saveplot){
    pt <- unique(DecApr_summtrapsWA$season)
    ggsave(here('wdfw','maps',paste0('Dec - Apr ',pt,'.png')),DecApr_map_out,w=6,h=5)
  }
  return(DecApr_map_out)
}

# Loop and save maps
# change input file here (cof_DecApr_summtrapsWA) if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)
tm <- proc.time()
#all_maps <- purrr::map(unique(DecApr_summtrapsWA$season),function(x){
all_maps <- purrr::map(unique(cof_DecApr_summtrapsWA$season),function(x){
    #DecApr_summtrapsWA %>% 
    cof_DecApr_summtrapsWA %>% 
    filter(season==x) %>% 
    map_decapr()
})
proc.time()-tm



#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

# test calculating number of grid cells in use in a given dec-Apr and May-Sep season
test <- DecApr_summtrapsWA %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(confidental_number_grids = n())

test <- MaySep_summtrapsWA %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(confidental_number_grids = n())

# test calculating number of grid cells in use in a given month
test <- cof_DecApr_summtrapsWA %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(non_confidental_number_grids = n())

test <- cof_MaySep_summtrapsWA %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(non_confidental_number_grids = n())



#monthly time step
test <- conf_M2_summtrapsWA_month %>% 
     group_by(season_month) %>% distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
     summarise(non_confidental_number_grids = n())

test_conf <- M2_summtrapsWA_month %>% 
     group_by(season_month) %>% distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
     summarise(confidental_number_grids = n())

joined <- left_join(test_conf, test, by="season_month")

joined <- joined %>% mutate(ratio = non_confidental_number_grids/confidental_number_grids)
joined <- joined %>% mutate(prop_grids_lost = 1 - ratio)

joined_v2 <-  joined %>% 
  separate(season_month, into = c("season", "month"), sep = "_") %>%  
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))


plot_ratio_nonconf_vs_conf <- ggplot(joined_v2, aes(x=month, y=ratio, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("grid cells visible\nratio non-conf. vs conf. data") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
plot_ratio_nonconf_vs_conf


#what is average % or proportion grids lost per month during the entire period of the 4-monthly steps?
# full study period is November 2013 to June 2016
xx <- joined %>% 
  mutate(
    month_interval = case_when(
      season_month == "2013-2014_December" | season_month == "2013-2014_January" | season_month == "2013-2014_February" ~ "13_14 11-02",
      season_month == "2013-2014_March" | season_month == "2013-2014_April" | season_month == "2013-2014_May" | season_month == "2013-2014_June" ~ "2014 03-06",
      season_month == "2013-2014_July" | season_month == "2013-2014_August" | season_month == "2013-2014_September" ~ "2014 07-10",
      season_month == "2014-2015_December" | season_month == "2014-2015_January" | season_month == "2014-2015_February" ~ "14_15 11-02",
      season_month == "2014-2015_March" | season_month == "2014-2015_April" | season_month == "2014-2015_May" | season_month == "2014-2015_June" ~ "2015 03-06",
      season_month == "2014-2015_July" | season_month == "2014-2015_August" | season_month == "2014-2015_September" ~ "2015 07-10",
      #note that 2015-16 season does not include December, fishery opened in Jan
      season_month == "2015-2016_December" | season_month == "2015-2016_January" | season_month == "2015-2016_February" ~ "15_16 11-02",
      season_month == "2015-2016_March" | season_month == "2015-2016_April" | season_month == "2015-2016_May" | season_month == "2015-2016_June" ~ "2016 03-06"
    )
  ) %>% 
  filter(!is.na(month_interval))

mean(xx$prop_grids_lost) #0.566697, i.e. 57% grids lost on average on a monthly step 

xx_v2 <-  xx %>% 
  separate(season_month, into = c("season", "month"), sep = "_") %>%  
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))

plot_prop_grids_lost <- ggplot(xx_v2, aes(x=month, y=prop_grids_lost, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  #scale_colour_brewer(palette = "PRGn") +
  ylab("prop grid cells lost\nnon-conf. vs conf. data") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
plot_prop_grids_lost

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

plotting_adj_summtraps_MaySep <- adj_summtraps_MaySep %>% 
  group_by(season) %>%
  #summarise across all grid cells in given season
  summarise(
    mean_M2_trapdens = mean(M2_trapdens, na.rm=TRUE),
    median_M2_trapdens = median(M2_trapdens, na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(plotting_adj_summtraps_MaySep)

ts_trapdens_MaySep <- ggplot() +
  geom_point(data = plotting_adj_summtraps_MaySep, aes(x = season, y = mean_M2_trapdens , group = 1), size=4) +
  geom_line(data = plotting_adj_summtraps_MaySep, aes(x = season, y = mean_M2_trapdens , group = 1)) +
  geom_point(data = plotting_adj_summtraps_MaySep, aes(x = season, y = median_M2_trapdens, group = 1), color = "darkred", size=4) +
  geom_line(data = plotting_adj_summtraps_MaySep, aes(x = season, y = median_M2_trapdens, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Trap density May-Sep") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_trapdens_MaySep
#ggsave(here('wdfw','plots',paste0('ts_meanANDmedian_trapdens_MaySep_WA_waters_only_2wk_input_file_20220120','.png')),ts_trapdens_MaySep,w=12,h=10)


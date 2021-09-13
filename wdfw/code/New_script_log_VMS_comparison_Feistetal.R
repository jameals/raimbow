#Summarise logbook data in order to compare with VMS maps in Feist et al 2021 Fig 2

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


# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
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
#------------------------------------------------------------------------------

# Read in trap density dataframe - note that currently this has not been fixed for the
# issue of 'too short and too long' stringlines
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps.rds'))

# the df has trap density in a grid cell in a 2-week period
# so step 1 is to summarise data on the same time steps as Feist et al 2021 Fig 2
# note that logbook df currently covers 2013-2019 period, so not a perfect overlap with Feist et al 2021 Fig 2

# summarising trap densities on same time step as Feist et al 2021 Fig 2 --> average trap density
# in each grid cell across the chose time period
# code is modified from script 6 - Making summary maps for May - Sep 15 period

# create a column in df to indicate when data fall in the time steps used in Feist et al 2021 Fig 2
# Note that October and November data may exist in VMS but not in logbooks
adj_summtraps_intervals <- adj_summtraps %>% 
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
glimpse(adj_summtraps_intervals)


# average M2 trap density for each grid cell for May-Sep period
avg_trap_dens_adj_summtraps_intervals <- adj_summtraps_intervals %>%
  group_by(month_interval, GRID5KM_ID, grd_x, grd_y) %>%  #do not group by AREA as some grid cells that overlap land appear twice
  summarise(
    #sum_M1_trapdens = sum(M1_trapdens),
    sum_M2_trapdens = sum(M2_trapdens),
    
    #these are not the correct way to determine confidentiality. see below
    #sum_nvessels = sum(nvessels),
    #avg_nvessels = mean(nvessels),# include this for creating non-confidential maps -- should this be avg instead of sum?
    
    number_obs = n(), #no. of grid cells being used for averaging
    #mean_M1_trapdens = sum_M1_trapdens/number_obs,
    mean_M2_trapdens = sum_M2_trapdens/number_obs
  )
glimpse(avg_trap_dens_adj_summtraps_intervals)



#----------------------------------------------------------------------------------------------
# # If want to create non-confidential maps (do not show data if < 3 vessels in grid)

#I think this is the correct way of figuring if more than 3 unique vessels were in a grid cell in a given period
#bring in data as points, not summarised by grid cell
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))

#for each point record assign the month_interval as per Feist et al Fig 2
logs_all_x <- traps_g_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m)) %>% 
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

#count number of unique vessels that used a given grid cell within a given month_interval
logs_all_nvessels <- logs_all_x %>% 
  group_by(month_interval, GRID5KM_ID,grd_x,grd_y) %>% 
  summarise(
    nvessels=n_distinct(Vessel,na.rm=T)) 

#join the new, correct, number of unique vessels in a grid in an interval into gridded df
avg_trap_dens_adj_summtraps_intervals %<>%
  left_join(logs_all_nvessels,by=c("month_interval","GRID5KM_ID", "grd_x", "grd_y"))

#If fewer than 3 unique vessels in a grid, that should be removed
conf_avg_trap_dens_adj_summtraps_intervals4 <- avg_trap_dens_adj_summtraps_intervals %>%
  mutate(is_confidential=ifelse(nvessels<3,T,F)) 
conf_avg_trap_dens_adj_summtraps_intervals5 <-  conf_avg_trap_dens_adj_summtraps_intervals4 %>%
  filter(is_confidential == FALSE)



#----------------------------------------------------------------------------------------------
# # map 
# # Figure out good trap density scale
# conf_avg_trap_dens_adj_summtraps_intervals5 %>%
#   ggplot()+
#   geom_density(aes(mean_M2_trapdens))
# 
# 
# # change input file if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)
# 
# map_log_vms <- function(avg_trap_dens_adj_summtraps_intervals_normalised,saveplot=TRUE){
#   
#   # labels for plot titles
#   season_label=unique(avg_trap_dens_adj_summtraps_intervals_normalised$month_interval)
#   
#   bbox = c(800000,1650000,1013103,1970000)
#   
#   log_vms_map_out <- avg_trap_dens_adj_summtraps_intervals_normalised %>% 
#     ggplot()+
#     geom_tile(aes(grd_x,grd_y,fill=trapdens_norm_within_interval ),na.rm=T,alpha=0.8)+
#     geom_sf(data=coaststates,col=NA,fill='gray50')+
#     #geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
#     #geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
#     #scale_fill_viridis(na.value='grey70',option="C",limits=c(0,60),breaks=c(0, 20, 40, 60),oob=squish)+
#     scale_fill_viridis(na.value='grey70',option="C",limits=c(0,1),breaks=c(0, 0.5, 1),oob=squish)+
#     coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
#     #if you do NOT want to show lat/lon lines on the map, use the below line instead:
#     #coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
#     labs(x='',y='',fill='Avg. trap density\nper sq. km',title=season_label)
#   
#   # saving
#   if(saveplot){
#     pt <- unique(avg_trap_dens_adj_summtraps_intervals_normalised$month_interval)
#     ggsave(here('wdfw','maps',paste0('log_vms',pt,'.png')),log_vms_map_out,w=6,h=5)
#   }
#   return(log_vms_map_out)
# }
# 
# # Loop and save maps
# # change input file here if want to make non-confidential maps (currently showing confidential data for grids with < 3 vessels)
# tm <- proc.time()
# all_maps <- purrr::map(unique(avg_trap_dens_adj_summtraps_intervals_normalised$month_interval),function(x){
#   avg_trap_dens_adj_summtraps_intervals_normalised %>% 
#     filter(month_interval==x) %>% 
#     map_log_vms()
# })
# proc.time()-tm


#----------------------------------------

#read in Blakes VMS data and normalise
Dungeness_4mon_5km_attribute_table_CONFIDENTIAL <- read_csv(here::here('wdfw','data','Dungeness 4mon 5km attribute table CONFIDENTIAL.csv'))

#If need to make VMS into long format:
#pivot_longer(df, starts_with("_")) -- so don't have to type all column names
longdata <- pivot_longer(Dungeness_4mon_5km_attribute_table_CONFIDENTIAL, starts_with("DUN"))
#longdata <- pivot_longer(Dungeness_4mon_5km_attribute_table_CONFIDENTIAL, c(DUN_NF1314, DUN_MJ2014, DUN_JO2014, DUN_NF1415, DUN_MJ2015, DUN_JO2015, DUN_NF1516, DUN_MJ2016))

# Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised <- Dungeness_4mon_5km_attribute_table_CONFIDENTIAL %>% 
#    mutate(DUN_NF1314_norm = scales::rescale(DUN_NF1314 , to=c(0,1)),
#           DUN_MJ2014_norm = scales::rescale(DUN_MJ2014 , to=c(0,1)),
#           DUN_JO2014_norm = scales::rescale(DUN_JO2014 , to=c(0,1)),
#           DUN_NF1415_norm = scales::rescale(DUN_NF1415 , to=c(0,1)),
#           DUN_MJ2015_norm = scales::rescale(DUN_MJ2015 , to=c(0,1)),
#           DUN_JO2015_norm = scales::rescale(DUN_JO2015 , to=c(0,1)),
#           DUN_NF1516_norm = scales::rescale(DUN_NF1516 , to=c(0,1)),
#           DUN_MJ2016_norm = scales::rescale(DUN_MJ2016 , to=c(0,1))
#           )

#Blake requires the resulting df in a 'wide' format
#but I also saved a long verion before running pivot_wider()
#write_csv(Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised,here::here('wdfw', 'data', "Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised_within_between_long.csv"))

Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised <- longdata %>% 
  mutate(VMS_norm_between_intervals = scales::rescale(value, to=c(0,1))) %>% 
  group_by(name) %>% 
  mutate(VMS_norm_within_interval = scales::rescale(value, to=c(0,1))) %>%
  pivot_wider(names_from = name,
              names_sep = "_",
              values_from = c(value, VMS_norm_within_interval, VMS_norm_between_intervals)
  ) 

Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised_wide <-  Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised %>% 
  rename(
    DUN_NF1314_norm_within = VMS_norm_within_interval_DUN_NF1314,
    DUN_MJ2014_norm_within = VMS_norm_within_interval_DUN_MJ2014,
    DUN_JO2014_norm_within = VMS_norm_within_interval_DUN_JO2014,
    DUN_NF1415_norm_within = VMS_norm_within_interval_DUN_NF1415,
    DUN_MJ2015_norm_within = VMS_norm_within_interval_DUN_MJ2015,
    DUN_JO2015_norm_within = VMS_norm_within_interval_DUN_JO2015,
    DUN_NF1516_norm_within = VMS_norm_within_interval_DUN_NF1516,
    DUN_MJ2016_norm_within = VMS_norm_within_interval_DUN_MJ2016,
    DUN_NF1314_norm_between = VMS_norm_between_intervals_DUN_NF1314,
    DUN_MJ2014_norm_between = VMS_norm_between_intervals_DUN_MJ2014,
    DUN_JO2014_norm_between = VMS_norm_between_intervals_DUN_JO2014,
    DUN_NF1415_norm_between = VMS_norm_between_intervals_DUN_NF1415,
    DUN_MJ2015_norm_between = VMS_norm_between_intervals_DUN_MJ2015,
    DUN_JO2015_norm_between = VMS_norm_between_intervals_DUN_JO2015,
    DUN_NF1516_norm_between = VMS_norm_between_intervals_DUN_NF1516,
    DUN_MJ2016_norm_between = VMS_norm_between_intervals_DUN_MJ2016
  )

#write_csv(Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised_wide,here::here('wdfw', 'data', "Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised_within_between.csv"))


#----------------------------------------
# Normalise trap density within intervals, but also within 13/14 11-02 and 2016 03-06 period
#for now use 'confidential' data, no need to remove grids with < 3 vessels
avg_trap_dens_adj_summtraps_intervals_normalised <- avg_trap_dens_adj_summtraps_intervals %>% 
  group_by(month_interval) %>% 
  mutate(trapdens_norm_within_interval = scales::rescale(mean_M2_trapdens , to=c(0,1)))

#need to ungroup for rescale() to work correctly
avg_trap_dens_adj_summtraps_normalised_within_between <- avg_trap_dens_adj_summtraps_intervals_normalised %>%
  ungroup() %>% 
  mutate(trapdens_norm_between_intervals = scales::rescale(mean_M2_trapdens , to=c(0,1)))

#write_csv(avg_trap_dens_adj_summtraps_normalised_within_between,here::here('wdfw', 'data', "avg_trap_dens_normalised_within_between_long.csv"))


#get logbook data to wider format for Blake
#remove some excess columns and rename some to make things tidier
avg_trap_dens_adj_summtraps_normalised_within_between_tidy <-  avg_trap_dens_adj_summtraps_normalised_within_between %>% 
  select(c(GRID5KM_ID,mean_M2_trapdens, trapdens_norm_within_interval, trapdens_norm_between_intervals, month_interval)) %>% 
  rename(mean_trapdens=mean_M2_trapdens, trapdens_norm_within = trapdens_norm_within_interval, trapdens_norm_between = trapdens_norm_between_intervals)

avg_trap_dens_normalised_within_between_wide <- avg_trap_dens_adj_summtraps_normalised_within_between_tidy %>%
  pivot_wider(names_from = month_interval,
              names_sep = "_",
              values_from = c(mean_trapdens, trapdens_norm_within, trapdens_norm_between)
              )

#write_csv(avg_trap_dens_normalised_within_between_wide,here::here('wdfw', 'data', "avg_trap_dens_normalised_within_between_wide.csv"))



#------------------------------------------
#make time series plot
#read in the long dataframes of VMS pings and logs with normalised data

logs_norm <- read_csv(here::here('wdfw','data','avg_trap_dens_normalised_within_between_long.csv'))
VMS_norm <- read_csv(here::here('wdfw','data','Dungeness_4mon_5km_attribute_table_CONFIDENTIAL_normalised_within_between_long.csv'))

logs_norm <- logs_norm %>% 
  mutate(month_interval = factor(month_interval, 
                                 levels = c('13_14 11-02','2014 03-06','2014 07-10','14_15 11-02','2015 03-06','2015 07-10','15_16 11-02','2016 03-06'))) 

#change some column names etc in VMS df to make it similar to log df
VMS_norm <-  VMS_norm %>% 
  rename(VMS_pings = value,  month_interval= name) %>% 
  mutate(
  month_interval = case_when(
    month_interval == "DUN_NF1314" ~ "13_14 11-02",
    month_interval == "DUN_MJ2014" ~ "2014 03-06",
    month_interval == "DUN_JO2014" ~ "2014 07-10",
    month_interval == "DUN_NF1415" ~ "14_15 11-02",
    month_interval == "DUN_MJ2015" ~ "2015 03-06",
    month_interval == "DUN_JO2015" ~ "2015 07-10",
    month_interval == "DUN_NF1516" ~ "15_16 11-02",
    month_interval == "DUN_MJ2016" ~ "2016 03-06"
    )
  ) %>% 
  mutate(month_interval = factor(month_interval, 
        levels = c('13_14 11-02','2014 03-06','2014 07-10','14_15 11-02','2015 03-06','2015 07-10','15_16 11-02','2016 03-06'))) 
  

VMS_norm_max <- VMS_norm %>% 
  group_by(month_interval) %>% 
  summarise(max = max(VMS_norm_between_intervals)) 

tsplot1 <-  
  ggplot()+
  geom_point(data= VMS_norm, aes(x=month_interval,y=VMS_norm_between_intervals, group=1), alpha = 0.2, show.legend = F)+
  geom_line(data=VMS_norm_max, aes(x=month_interval,y=max, group=1))+
  scale_y_continuous(breaks=seq(0, 1, 0.1),limits=c(0,1))+
  labs(x="4-Month intervals",y="Normalised pings") +
  ggtitle("VMS pings") + 
  theme(legend.position = ("top"),legend.title=element_blank())
tsplot1

logs_norm_max <- logs_norm %>% 
  group_by(month_interval) %>% 
  summarise(max = max(trapdens_norm_between_intervals)) 

tsplot2 <-  
  ggplot()+
  geom_point(data= logs_norm, aes(x=month_interval,y=trapdens_norm_between_intervals, group=1, colour='red'), alpha = 0.2, show.legend = F)+
  geom_line(data=logs_norm_max, aes(x=month_interval,y=max, group=1, colour='red'), show.legend = F)+
  scale_y_continuous(breaks=seq(0, 1, 0.1),limits=c(0,1))+
  labs(x="4-Month intervals",y="Normalised trap density") +
  ggtitle("Trap density") + 
  theme(legend.position = ("top"),legend.title=element_blank())
tsplot2

map_out <- plot_grid(tsplot1,tsplot2,nrow=1)

#ggsave(here('wdfw','plots',paste0('ts_normalised VMS pings and trap densities','.png')),map_out,w=12,h=10)


joined <- inner_join(VMS_norm,logs_norm, by=c("month_interval", "GRID5KM_ID"))

tsplot1 <-  
  ggplot()+
  geom_point(data= joined, aes(x=month_interval,y=VMS_norm_between_intervals, group=1))+
  scale_y_continuous(breaks=seq(0, 1, 0.1),limits=c(0,1))+
  labs(x="4-Month intervals",y="Normalised pings") +
  ggtitle("VMS pings") + 
  theme(legend.position = ("top"),legend.title=element_blank())
tsplot1

tsplot2 <-  
  ggplot()+
  geom_point(data= joined, aes(x=month_interval,y=trapdens_norm_between_intervals, group=1, colour='red'), show.legend = F)+
  scale_y_continuous(breaks=seq(0, 1, 0.1),limits=c(0,1))+
  labs(x="4-Month intervals",y="Normalised trap density") +
  ggtitle("Trap density") + 
  theme(legend.position = ("top"),legend.title=element_blank())
tsplot2

map_out <- plot_grid(tsplot1,tsplot2,nrow=1)

## WA logbook analysis 
# BW model domains cuts short around 47.3N
# Investigate proportion of WA DCRB traps that are north of where BW domain ends

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
library(cowplot)
library(ggpubr)
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

# Bring in logbook data clipped to WA waters as points (not summarised on grid cell level yet)
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))

logs_all <- traps_g_all_logs %>% 
  #st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m))

logs_all %<>% mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))

glimpse(logs_all)



# Bring in bw model output, this may be in Projects\NOAA data\maps_ts_whales\data
bw_5m_long_monthly <- read_rds(here::here('wdfw', 'data','BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds'))
# bw output has all grid cells appearing multiple times (each year_month combo)
# but only need list of grid cells
distinct_bw_grids <- bw_5m_long_monthly %>% distinct(GRID5KM_ID) 
  #and add an extra column denoting that this is bw domain
distinct_bw_grids$bw_domain <- "bw_domain"


# join the dfs based on GRID5KM_ID -- the bw extent is large, and the only time logbook data would not be 
# inside the bw domain, is at the N end
bw_log_joined <- left_join(logs_all, distinct_bw_grids, by = c("GRID5KM_ID"))


#If 'bw_domain' is NA, it means the simulated pot/logbook records is outside bw domain
bw_log_joined_v2 <- bw_log_joined %>% 
  mutate(bw_domain=ifelse(is.na(bw_domain), "outside_bw_domain", "bw_domain")) %>% 
  #instances where labelled as outside_bw_domain: grids along klipsan beach (not full grid cells), 
  #inside ports/bays, AND outside of N boundary of bw domain <-- this is the only one we want to keep as 'outside'
  mutate(bw_domain=ifelse(is.na(grd_x), "bw_domain", bw_domain)) %>% 
  mutate(bw_domain=ifelse(bw_domain=='outside_bw_domain' & is_port_or_bay == T, "bw_domain", bw_domain))


#-------------------------------------------------------------------------------------------------
#visualization

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


sub_set <- bw_log_joined_v2 %>% 
  filter(season== '2019-2020')

bbox = c(800000,1650000,1013103,1970000)

test_map_out <- sub_set %>% 
  ggplot()+
  geom_tile(aes(grd_x,grd_y,fill=as.factor(bw_domain)),na.rm=T,alpha=0.8)+
  geom_sf(data=coaststates,col=NA,fill='gray50')+
  #scale_fill_viridis(na.value='grey70',option="C",limits=c(0,120),breaks=c(0,30,60,90,120),oob=squish)+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  labs(x='',y='',fill='2019-2020')
test_map_out

#-------------------------------------------------------------------------------------------------

#summarise the proportion of data outside of bw domain

#if want to work on pot level, use bw_log_joined_v2
percent_pots_outisde_bw_domain <-  bw_log_joined_v2 %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_outise_domain = length(bw_domain[bw_domain == "outside_bw_domain"])
  ) %>% 
  mutate(percent_pots_outisde = (n_outise_domain/n_records)*100)

#% outside by season and May-Sep
percent_pots_outisde_bw_domain <-  bw_log_joined_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(m %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == 'Y') %>% 
  group_by(season, is_May_Sep) %>% 
  summarise(n_records = n(),
            n_outise_domain = length(bw_domain[bw_domain == "outside_bw_domain"])
  ) %>% 
  mutate(percent_pots_outisde = (n_outise_domain/n_records)*100)


#if want to work on 'stringline'/logbook entry level --- UNSURE IF THIS IS CORRECT
bw_log_joined_strings <-  bw_log_joined_v2 %>% distinct(SetID, .keep_all = TRUE)

percent_strings_outisde_bw_domain <-  bw_log_joined_strings %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_outise_domain = length(bw_domain[bw_domain == "outside_bw_domain"])
  ) %>% 
  mutate(percent_strings_outisde = (n_outise_domain/n_records)*100)


#ALSO by month

#if want to work on pot level, use bw_log_joined_v2
percent_pots_outisde_bw_domain <-  bw_log_joined_v2 %>% 
  group_by(season, m) %>% 
  summarise(n_records = n(),
            n_outise_domain = length(bw_domain[bw_domain == "outside_bw_domain"])
  ) %>% 
  mutate(percent_pots_outisde = (n_outise_domain/n_records)*100)


#if want to work on 'stringline'/logbook entry level
bw_log_joined_strings <-  bw_log_joined_v2 %>% distinct(SetID, .keep_all = TRUE)

percent_strings_outisde_bw_domain <-  bw_log_joined_strings %>% 
  group_by(season, m) %>% 
  summarise(n_records = n(),
            n_outise_domain = length(bw_domain[bw_domain == "outside_bw_domain"])
  ) %>% 
  mutate(percent_strings_outisde = (n_outise_domain/n_records)*100)









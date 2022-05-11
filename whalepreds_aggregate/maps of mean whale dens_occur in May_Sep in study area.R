#maps of mean whale dens/occur in May-Sep in study area



#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(scales)

#-----------------------------------------------------------------------------------
#bring in some grids
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#maps of mean whale data in May-Sep of each season

#whale data

#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)


#bw output 2009-July 2019
x.blue <- readRDS(path.blue) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue)

#bw output Aug 2019-Sep 2021
x.blue_2019_2021 <- readRDS(path.blue_2019_2021) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue_2019_2021) #why does data end June 2021??

#join the 2 bw dfs
x.blue.all <- rbind(x.blue, x.blue_2019_2021)


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

x.whale_crab_season_May_Sep <-  x.whale_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


#'study area' created in QGIS, to encompass all fished grids plus 'buffer' (grids that could be fished)
#read in 'study area' (grid)
study_area <- read_sf(here::here('wdfw','data', 'study_area.shp'))
glimpse(study_area)
#plot(study_area)

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 

study_area_df <- as.data.frame(study_area_grids_id) %>% 
  rename(GRID5KM_ID = study_area_grids_id)


#the study area grid needs to have all season-month combos for May-Sep
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
study_area_df_with_all_season_month_combos <- crossing(study_area_df, season_month_combos) %>%
  #and add to that the column to denote study area
  mutate(study_area = 'Y')


#join whale data to study area grid
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

summary_study_area_whale <- study_area_whale %>% 
  filter(study_area == 'Y') %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  group_by(season, GRID5KM_ID) %>% 
  summarise(
    mean_hump_dens = mean(Humpback_dens_mean, na.rm=TRUE),
    sum_hump_dens = sum(Humpback_dens_mean),
    mean_blue_occur = mean(Blue_occurrence_mean, na.rm=TRUE),
    sum_blue_occur = sum(Blue_occurrence_mean)
    )
#group by season and grid, get an average, map



# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49) 


#dissolved_2019_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_MaySep_WA_fishery_footprint_20220202.rds'))


subset_data <- summary_study_area_whale %>% 
  filter(season == "2019-2020") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_hump <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=sum_hump_dens,
              #fill=mean_hump_dens,
              col=sum_hump_dens
              #col=mean_hump_dens
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  #max mean hump dens 0.0427
  #scale_fill_viridis(na.value=NA,option="D",name="Mean humpback\ndensity",breaks=seq(0,0.042,by=0.014),limits=c(0,0.042),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Mean humpback\ndensity",breaks=seq(0,0.042,by=0.014),limits=c(0,0.042),oob=squish) + 
  
  #max sum hump dens 0.2136640 in study area
  scale_fill_viridis(na.value=NA,option="D",name="Sum humpback\ndensity",breaks=seq(0, 0.21,by=0.07),limits=c(0, 0.21),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Sum humpback\ndensity",breaks=seq(0, 0.21,by=0.07),limits=c(0, 0.21),oob=squish) + 
  
  #geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  
  ggtitle("2019-2020 May-Sep") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_hump



subset_data <- summary_study_area_whale %>% 
  filter(season == "2019-2020") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_blue <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=sum_blue_occur,
              #fill=mean_blue_occur,
              col=sum_blue_occur
              #col=mean_blue_occur
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  
  #max mean blue occur 0.6509358
  #scale_fill_viridis(na.value=NA,option="D",name="Mean blue whale\noccurrence",breaks=seq(0,0.65,by=0.325),limits=c(0,0.65),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Mean blue whale\noccurrence",breaks=seq(0,0.65,by=0.325),limits=c(0,0.65),oob=squish) + 
 
  #max sum blue occur 3.254679 in study area
  scale_fill_viridis(na.value=NA,option="D",name="Sum blue whale\noccurrence",breaks=seq(0,3.25,by=1.625),limits=c(0,3.25),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="Sum blue whale\noccurrence",breaks=seq(0,3.25,by=1.625),limits=c(0,3.25),oob=squish) + 
  
  #geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("2019-2020 May-Sep") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_blue



# plot blues and humps together
png(paste0(path_figures, "/map_sum_blue_hump_2019_2020_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump,
          map_blue,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




#--------------------------------------------------------
#test some monthly maps

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49) 


#dissolved_2019_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_MaySep_WA_fishery_footprint_20220202.rds'))


subset_data <- x.whale_crab_season_May_Sep %>% 
  filter(season == "2019-2020") %>%
  filter(month == "09") %>%
  filter(!is.na(Humpback_dens_mean)) %>%
  left_join(grid.5km, by = "GRID5KM_ID")

map_hump <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=Humpback_dens_mean,
              #fill=Humpback_dens_mean,
              col=Humpback_dens_mean
              #col=mean_hump_dens
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  
  scale_fill_viridis(na.value=NA,option="D",name="humpback\ndensity",breaks=seq(0, 0.06,by=0.02),limits=c(0, 0.06),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="humpback\ndensity",breaks=seq(0, 0.06,by=0.02),limits=c(0, 0.06),oob=squish) + 
  
  #geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  
  ggtitle("2019-2020 September") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_hump



subset_data <- x.whale_crab_season_May_Sep %>% 
  filter(season == "2019-2020") %>%
  filter(month == "09") %>%
  filter(!is.na(Blue_occurrence_mean)) %>%
  left_join(grid.5km, by = "GRID5KM_ID")

map_blue <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=Blue_occurrence_mean,
              #fill=Blue_occurrence_mean,
              col=Blue_occurrence_mean
              #col=Blue_occurrence_mean
          )
  ) +
  geom_sf(data=rmap.base,col='black',fill='gray50') +
  
  scale_fill_viridis(na.value=NA,option="D",name="blue whale\noccurrence",breaks=seq(0, 0.86,by=0.215),limits=c(0, 0.86),oob=squish) + 
  scale_color_viridis(na.value=NA,option="D",name="blue whale\noccurrence",breaks=seq(0, 0.86,by=0.215),limits=c(0, 0.86),oob=squish) + 
  
  #geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("2019-2020 September") +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_blue



# plot blues and humps together
png(paste0(path_figures, "/map_blue_hump_2019_2020_09.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump,
          map_blue,
          ncol=2,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())







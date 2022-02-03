#3-panel map figure of fishign and whales


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

path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased

path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid

#-----------------------------------------------------------------------------------

#maps of fishing/trap density

#fishing trap density gridded data
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds'))

#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
adj_summtraps <- adj_summtraps %>% filter(GRID5KM_ID != 122919)

#only focus on May-Sep data
adj_summtraps_MaySep <- adj_summtraps %>% 
    mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>% 
  select(-nvessels) #this isn't the correct count of vessels to make non-confidential maps. see script 6 to make non-conf maps

# average trap density for each grid cell for May-Sep period
# first group those seasons that are pre-reg (don't want to take averages of averages)
adj_summtraps_MaySep_groups <- adj_summtraps_MaySep %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg"
                  )
         )

#to get mean trap dens in a grid pre-reg, select and group by pre-reg grouping
MaySep_summtraps_pre_reg <- adj_summtraps_MaySep_groups %>%
  filter(seasons_with_regs == "pre-reg") %>% 
  group_by(GRID5KM_ID, grd_x, grd_y, AREA) %>%  #here don't group_by season, as want to keep all pre-reg seasons together
  summarise(
    sum_trapdens = sum(M2_trapdens),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_trapdens/number_obs
  )
glimpse(MaySep_summtraps_pre_reg)

MaySep_summtraps_post_reg <- adj_summtraps_MaySep_groups %>%
  filter(seasons_with_regs == "post-reg") %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  #here group_by season, as want to keep 2018-19 and 2019-20 data separate
  summarise(
    sum_trapdens = sum(M2_trapdens),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_trapdens/number_obs
  )
glimpse(MaySep_summtraps_post_reg)






#----------------------------------------------------------

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-126.5,45.5,-121,49) 


pre_regs_data <- MaySep_summtraps_pre_reg %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_pre_regs <- ggplot() + 
  geom_sf(data=sf::st_as_sf(pre_regs_data), 
          aes(fill=mean_trapdens,
              col=mean_trapdens
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  scale_color_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  ggtitle("Pre-regulations \n(2013-14 to 2017-18)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_pre_regs



post_regs_data_2018_2019 <- MaySep_summtraps_post_reg %>% 
  filter(season == '2018-2019') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2018_2019 <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2018_2019), 
          aes(fill=mean_trapdens,
              col=mean_trapdens
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  scale_color_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  ggtitle("Post-regulations \n(2018-19)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_post_regs_2018_2019



post_regs_data_2019_2020 <- MaySep_summtraps_post_reg %>% 
  filter(season == '2019-2020') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2019_2020 <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2019_2020), 
          aes(fill=mean_trapdens,
              col=mean_trapdens
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  scale_color_viridis(na.value=NA,option="D",name="Trap density",breaks=seq(0,30,by=5),limits=c(0,30),oob=squish) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  ggtitle("Post-regulations \n(2019-20)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_post_regs_2019_2020


#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#maps of whale data

#bring in whale data -- data is on a year_month level

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

#at this point data is not spatially restricted
#------------------------------------------------------------------------------------------

#Humpback whales
#restrict data to May-Sep, and to pre and post reg seasons

x.hump_2014_2020_crab_season <- x.hump_2009_2020 %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))  %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(is_May_Sep = 
         ifelse(month %in% c('05', '06', '07', '08', '09')
                ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")  

# average hw density for each grid cell for May-Sep period
# first group those seasons that are pre-reg (don't want to take averages of averages)
x.hump_2014_2020_crab_season_MaySep_groups <- x.hump_2014_2020_crab_season %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg"
           )
  )


#to get mean hw dens in a grid pre-reg, select and group by pre-reg grouping
MaySep_hw_pre_reg <- x.hump_2014_2020_crab_season_MaySep_groups %>%
  filter(seasons_with_regs == "pre-reg") %>% 
  group_by(GRID5KM_ID) %>%  #here don't group_by season, as want to keep all pre-reg seasons together
  summarise(
    Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
  )
glimpse(MaySep_hw_pre_reg)

MaySep_hw_post_reg <- x.hump_2014_2020_crab_season_MaySep_groups %>%
  filter(seasons_with_regs == "post-reg") %>% 
  group_by(season, GRID5KM_ID) %>%  #here group_by season, as want to keep 2018-19 and 2019-20 data separate
  summarise(
    Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
  )
glimpse(MaySep_hw_post_reg)

#-------------

pre_regs_data_hw <- MaySep_hw_pre_reg %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_pre_regs_hw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(pre_regs_data_hw), 
          aes(fill=Humpback_dens_mean,
              col=Humpback_dens_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  scale_color_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  #ggtitle("Pre-regulations (2013-14 to 2017-18)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_pre_regs_hw



post_regs_data_2018_2019_hw <- MaySep_hw_post_reg %>% 
  filter(season == '2018-2019') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2018_2019_hw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2018_2019_hw), 
          aes(fill=Humpback_dens_mean,
              col=Humpback_dens_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  scale_color_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  #ggtitle("Post-regulations (2018-19)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_post_regs_2018_2019_hw



post_regs_data_2019_2020_hw <- MaySep_hw_post_reg %>% 
  filter(season == '2019-2020') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2019_2020_hw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2019_2020_hw), 
          aes(fill=Humpback_dens_mean,
              col=Humpback_dens_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  scale_color_viridis(na.value=NA,option="A",name="Whale density",breaks=seq(0,0.043,by=0.01),limits=c(0,0.043),oob=squish) + 
  #ggtitle("Post-regulations (2019-20)") +
  #geom_sf(data = study_area, color = 'black', fill = NA) +
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
map_post_regs_2019_2020_hw



#------------------------------------------------------------------------------------------

#Blue whales
#restrict data to May-Sep, and to pre and post reg seasons

x.blue_2014_2020_crab_season <- x.blue.all %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))  %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")  

# average bw density for each grid cell for May-Sep period
# first group those seasons that are pre-reg (don't want to take averages of averages)
x.blue_2014_2020_crab_season_MaySep_groups <- x.blue_2014_2020_crab_season %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg"
           )
  )


#to get mean bw dens in a grid pre-reg, select and group by pre-reg grouping
MaySep_bw_pre_reg <- x.blue_2014_2020_crab_season_MaySep_groups %>%
  filter(seasons_with_regs == "pre-reg") %>% 
  group_by(GRID5KM_ID) %>%  #here don't group_by season, as want to keep all pre-reg seasons together
  summarise(
    Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
  )
glimpse(MaySep_bw_pre_reg)

MaySep_bw_post_reg <- x.blue_2014_2020_crab_season_MaySep_groups %>%
  filter(seasons_with_regs == "post-reg") %>% 
  group_by(season, GRID5KM_ID) %>%  #here group_by season, as want to keep 2018-19 and 2019-20 data separate
  summarise(
    Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
  )
glimpse(MaySep_bw_post_reg)

#-------------


pre_regs_data_bw <- MaySep_bw_pre_reg %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_pre_regs_bw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(pre_regs_data_bw), 
          aes(fill=Blue_occurrence_mean,
              col=Blue_occurrence_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  scale_color_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  #ggtitle("Pre-regulations (2013-14 to 2017-18)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_pre_regs_bw



post_regs_data_2018_2019_bw <- MaySep_bw_post_reg %>% 
  filter(season == '2018-2019') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2018_2019_bw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2018_2019_bw), 
          aes(fill=Blue_occurrence_mean,
              col=Blue_occurrence_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  scale_color_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  #ggtitle("Post-regulations (2018-19)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16), 
        legend.position = 'none'
  )
map_post_regs_2018_2019_bw



post_regs_data_2019_2020_bw <- MaySep_bw_post_reg %>% 
  filter(season == '2019-2020') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_post_regs_2019_2020_bw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(post_regs_data_2019_2020_bw), 
          aes(fill=Blue_occurrence_mean,
              col=Blue_occurrence_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  scale_color_viridis(na.value=NA,option="G",name="Probability of \noccurrence",breaks=seq(0,0.9,by=0.2),limits=c(0,0.9),oob=squish) + 
  ggtitle("Post-regulations (2019-20)") +
  #geom_sf(data = study_area, color = 'black', fill = NA) +
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
map_post_regs_2019_2020_bw


#-----------------------------------------------------------------------------------
#join and save all plots into one figure
 # map_pre_regs
 # map_post_regs_2018_2019
 # map_post_regs_2019_2020
 # map_pre_regs_hw
 # map_post_regs_2018_2019_hw
 # map_post_regs_2019_2020_hw
 # map_pre_regs_bw
 # map_post_regs_2018_2019_bw
 # map_post_regs_2019_2020_bw

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"

png(paste0(path_figures, "/map_mean_traps_MaySep_pre_and_post_reg.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_pre_regs,
          map_post_regs_2018_2019,
          map_post_regs_2019_2020,
          ncol=3,
          nrow=1,
          #legend="top",
          common.legend = TRUE,
          legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




test1 <- ggarrange(
          map_pre_regs,
          map_post_regs_2018_2019,
          map_post_regs_2019_2020,
          ncol=3,
          nrow=1,
          #legend="top",
          common.legend = TRUE,
          legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)

test2 <- ggarrange(
         map_pre_regs_hw,
         map_post_regs_2018_2019_hw,
         map_post_regs_2019_2020_hw,
          ncol=3,
          nrow=1,
          #legend="top",
          common.legend = TRUE,
          legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)

test3 <- ggarrange(
  map_pre_regs_bw,
  map_post_regs_2018_2019_bw,
  map_post_regs_2019_2020_bw,
  ncol=3,
  nrow=1,
  #legend="top",
  common.legend = TRUE,
  legend="right",
  #labels="auto",
  vjust=8,
  hjust=0
)

png(paste0(path_figures, "/map_mean_bw_MaySep_pre_and_post_reg_test1.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(test1,
          ncol=1,
          nrow=1,
          #legend="top",
          #common.legend = TRUE,
          #legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


# png(paste0(path_figures, "/map_mean_traps_and_whales_MaySep_pre_and_post_reg.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(map_pre_regs,
#           map_post_regs_2018_2019,
#           map_post_regs_2019_2020,
#           map_pre_regs_hw,
#           map_post_regs_2018_2019_hw,
#           map_post_regs_2019_2020_hw,
#           map_pre_regs_bw,
#           map_post_regs_2018_2019_bw,
#           map_post_regs_2019_2020_bw,
#           ncol=3,
#           nrow=3,
#           #legend="top",
#           legend="right",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



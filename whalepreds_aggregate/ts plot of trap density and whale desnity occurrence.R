# ts plot of trap density and whale density/occurrence

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
library(magrittr)

path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased

path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid

#-----------------------------------------------------------------------------------

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

#at this point data i s not spatially restricted
#------------------------------------------------------------------------------------------

#'study area' created in QGIS, to encompass all fished grids plus 'buffer' (grids that could be fished)
#read in 'study area' (grid)
study_area <- read_sf(here::here('wdfw','data', 'study_area.shp'))
glimpse(study_area)
#plot(study_area)

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 
x.hump_2009_2020_WA <-  x.hump_2009_2020 %>% filter(GRID5KM_ID %in% study_area_grids_id)


x.hump_2014_2020_crab_season <- x.hump_2009_2020_WA %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))  %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

x.hump_2014_2020_crab_season_v2 <- x.hump_2014_2020_crab_season %>% 
  mutate(season_month = factor(paste0(season,"_",month))) %>%  
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    #Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE),
    #Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    Humpback_dens_sum = sum(Humpback_dens_mean, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('12','01','02','03','04','05','06','07','08','09','10','11'))) %>% 
  arrange(season, month)
glimpse(x.hump_2014_2020_crab_season_v2)

ordered.ids <- factor(x.hump_2014_2020_crab_season_v2$season_month, levels=x.hump_2014_2020_crab_season_v2$season_month)

ts_hump_dens <- ggplot(
  data = x.hump_2014_2020_crab_season_v2, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = Humpback_dens_sum,
    #y = Humpback_dens_mean,
    #y = Humpback_dens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=3)])+
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2013-2014_12,2019-2020_09)) +
  ylab("Sum Humpback Whale Density\nin study area") + 
  #ylab("Mean Humpback Whale Density\nin study area") + 
  xlab("Season_month") +
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
ts_hump_dens
#ts of mean (when 0s included) and sum looks the same (shape is same, y-axis is different)


#ts for Jul-Sep only: how steady was HW in Jul-Sep of pre-reg seasons?
x.hump_2014_2020_crab_season_v3 <- x.hump_2014_2020_crab_season_v2 %>% 
  filter(month %in% c('07', '08', '09'))

ordered.ids <- factor(x.hump_2014_2020_crab_season_v3$season_month, levels=x.hump_2014_2020_crab_season_v3$season_month)


ts_hump_dens <- ggplot(
  data = x.hump_2014_2020_crab_season_v3, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = Humpback_dens_sum,
    #y = Humpback_dens_mean,
    #y = Humpback_dens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=1)])+
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2013-2014_12,2019-2020_09)) +
  ylab("Sum Humpback Whale Density\nin study area") + 
  #ylab("Mean Humpback Whale Density\nin study area") + 
  xlab("Season_month") +
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
ts_hump_dens

#-----------------------------------------------------------------------------------
# quick visual check with a map

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
# grid5km_bbox <- st_bbox(grid.5km.lno %>% 
#                           st_as_sf()
# )
bbox = c(-127,45,-120,49) 


subset_data <- x.blue_2014_2020_crab_season %>% 
  #filter(study_area=='Y') %>% 
  filter(season == "2019-2020") %>% 
  filter(month == "09") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=Blue_occurrence_mean,
              col=Blue_occurrence_mean
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="",breaks=seq(0,0.8,by=0.1),limits=c(0,0.8)) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  scale_color_viridis(na.value=NA,option="D",name="",breaks=seq(0,0.8,by=0.1),limits=c(0,0.8)) + # ,breaks=seq(0,0.05,by=0.01),limits=c(0,0.05) -- HW
  #ggtitle("2009-2020 Median\nHumpback Whale Densities") +
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
map_test


#-----------------------------------------------------------------------------------

x.blue.all_WA <-  x.blue.all %>% filter(GRID5KM_ID %in% study_area_grids_id)


x.blue_2014_2020_crab_season <- x.blue.all_WA %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))  %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

x.blue_2014_2020_crab_season_v2 <- x.blue_2014_2020_crab_season %>% 
  mutate(season_month = factor(paste0(season,"_",month))) %>%  
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    #Blue_dens_mean = mean(Blue_occurrence_mean, na.rm=TRUE),
    #Blue_dens_median = median(Blue_occurrence_mean, na.rm=TRUE),
    Blue_dens_sum = sum(Blue_occurrence_mean, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('12','01','02','03','04','05','06','07','08','09','10','11'))) %>% 
  arrange(season, month) %>% 
  filter(season_month != '2019-2020_11') %>% 
  filter(season_month != '2019-2020_10')
glimpse(x.blue_2014_2020_crab_season_v2)


ordered.ids <- factor(x.blue_2014_2020_crab_season_v2$season_month, levels=x.blue_2014_2020_crab_season_v2$season_month)

ts_blue_dens <- ggplot(
  data = x.blue_2014_2020_crab_season_v2, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = Blue_dens_sum,
    #y = Blue_dens_mean,
    #y = Blue_dens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=3)])+
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Sum Blue Whale Occurrence\nin study area") + 
  #ylab("Mean Blue Whale Occurrence\nin study area") + 
  xlab("Season_month") +
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
ts_blue_dens

#--------------------------------------------------------------------------
#bar chart for whales SUM Dec-Apr vs May-Sep
#x.blue_2014_2020_crab_season
#x.hump_2014_2020_crab_season


x.blue_2014_2020_crab_season_bar <- x.blue_2014_2020_crab_season %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'May-Sep', 'Dec-Apr')) %>%  
  group_by(is_May_Sep) %>%
  summarise(
    occur_sum = sum(Blue_occurrence_mean, na.rm=TRUE) #,
    #dens_mean = mean(Blue_occurrence_mean, na.rm=TRUE),
    #sd = sd(Blue_occurrence_mean, na.rm = TRUE),
    #n = n()
  )%>% 
  #mutate(se = sd / sqrt(n)
  #) %>% 
  mutate(species = 'bw')
glimpse(x.blue_2014_2020_crab_season_bar)


x.hump_2014_2020_crab_season_bar <- x.hump_2014_2020_crab_season %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'May-Sep', 'Dec-Apr')) %>%  
  group_by(is_May_Sep) %>%
  summarise(
    dens_sum = sum(Humpback_dens_mean, na.rm=TRUE) #,
    #dens_mean = mean(Humpback_dens_mean, na.rm=TRUE),
    #sd = sd(Humpback_dens_mean, na.rm = TRUE),
    #n = n()
  )%>% 
  #mutate(se = sd / sqrt(n)
  #)%>% 
  mutate(species = 'hw')
glimpse(x.hump_2014_2020_crab_season_bar)



p1 <- ggplot(data=x.blue_2014_2020_crab_season_bar, aes(x = species, y = occur_sum, fill = is_May_Sep))+
  geom_bar(position="dodge", stat = "identity")+
  #geom_errorbar(aes(ymin=dens_mean-se, ymax=dens_mean+se), 
   #             width=.25, position=position_dodge(0.9)) +
  scale_fill_manual(values = c("deepskyblue3", "indianred1"))+
  scale_y_continuous(position = "right")+
  labs(y = "Sum blue whale\nprobability of occurrence",
       x = "Dec-Apr   May-Sep")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p1

p2 <- ggplot(data=x.hump_2014_2020_crab_season_bar, aes(x = species, y = dens_sum, fill = is_May_Sep))+
  geom_bar(position="dodge", stat = "identity")+
  #geom_errorbar(aes(ymin=dens_mean-se, ymax=dens_mean+se), 
   #             width=.25, position=position_dodge(0.9)) +
  scale_fill_manual(values = c("deepskyblue3", "indianred1"))+
  scale_y_continuous(position = "right")+
  labs(y = "Sum humpback whale density",
       x = "Dec-Apr   May-Sep")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p2


ts_plots <- ggarrange(ts_hump_dens,
                      ts_blue_dens,
                      ncol=1,
                      nrow=2,
                      #legend="top",
                      #labels="auto",
                      vjust=8,
                      hjust=0
)

bar_plots <- ggarrange(p2,
                       p1,
                      ncol=1,
                      nrow=2,
                      #legend="top",
                      #labels="auto",
                      vjust=8,
                      hjust=0
)


#https://stackoverflow.com/questions/18427455/multiple-ggplots-of-different-sizes
lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

lay_out(list(ts_plots, 1:2, 1:3),
        list(p2, 1, 4),
        list(p1, 2, 4))
#how to save this?? other than using cnipping tool...?




#--------------------------------------------------------------------------












#--------------------------------------------------------------------------
#if wanted to have third layer to ts to show fishing data


path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA <- x.fish_WA %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens),
    mean_M2_tottraps = mean(M2_tottraps)
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_crab_season <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) #change month to two digit number

x.fish_2014_2020_crab_season_v2 <- x.fish_WA_crab_season %>% 
  mutate(season_month = factor(paste0(season,"_",month))) %>%  
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    Trap_dens_sum = sum(mean_M2_trapdens, na.rm=TRUE),
    tottraps_sum = sum(mean_M2_tottraps, na.rm=TRUE),
    Trap_dens_mean = mean(mean_M2_trapdens, na.rm=TRUE),
    Trap_dens_median = median(mean_M2_trapdens, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") 

#want to make sure that fishing data has all year - month combos so that the plot matches with whale plots
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("12","01","02","03","04", "05", "06", "07", "08", "09", "10", "11"))
season_month_combos <- crossing(season, month)

fish_with_all_season_month_combos <- left_join(season_month_combos, x.fish_2014_2020_crab_season_v2 , by=c("season", "month")) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('12','01','02','03','04','05','06','07','08','09','10','11'))) %>% 
  arrange(season, month) %>% 
  mutate(season_month = factor(paste0(season,"_",month))) %>% 
  filter(season_month != '2019-2020_11') %>% 
  filter(season_month != '2019-2020_10') %>% 
  mutate(Trap_dens_mean = ifelse(is.na(Trap_dens_mean), 0, Trap_dens_mean))


ordered.ids <- factor(fish_with_all_season_month_combos$season_month, levels=fish_with_all_season_month_combos$season_month)

ts_trap_dens <- ggplot(
  data = fish_with_all_season_month_combos, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    #y = Trap_dens_sum,
    y = tottraps_sum,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=3)])+
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  #ylab("Sum Trap Density\nin grids being used") + 
  ylab("Lines in the water") + 
  xlab("Season_month") +
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
ts_trap_dens



#--------------
#do fishery plot as number of active unique licenses multiplied by their pot limit

traps_g_license_logs_2013_2020 <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))

#traps_g <- traps_g_for_all_logs_full_seasons
traps_g <- traps_g_license_logs_2013_2020

# For now look at 2013-2019, traps_g_license_logs_2013_2019.rds is already filtered for these years
#traps_g <- traps_g %>% 
# filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))

# modifying the summtraps code from script 2 that adjusts for double counting
testdf <- traps_g %>% 
  #st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License,GRID5KM_ID,grd_x,grd_y,SetID2,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
  # finally, sum the total traps per Vessel, across all grid cells in the 2-week period in question
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License) %>% 
  summarise(
    M1_tottraps=sum(ntraps_vessel_cell))
glimpse(testdf)


#bring in 'raw' logs 
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#want to keep effort in WA waters by OR vessels
#logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") 

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

#For now look at 2013-2020
logsdf <- logsdf %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019', '2019-2020')) 

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


#Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

#join Pot_Limit info. 
testdf %<>%
  left_join(WA_pot_limit_info,by=c("License"))
glimpse(testdf)

# apply 2019 summer pot limit reduction, which took effect July 1 and was in effect through the end of the season (Sept. 15)
# apply 2020 summer pot limit reduction (May-Sep)
## create season_month column
testdf %<>%
  mutate(season_month = paste0(season,"_",month_name))
## make a new column for summer pot limit reduction
testdf %<>% 
  mutate(Pot_Limit_SummerReduction = Pot_Limit)
## split df to pre and post reduction periods
df1 <- testdf %>%
  filter(!season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                              '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))
df2 <- testdf %>%
  filter(season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                             '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))
## adjust pot limit post 1 July 2019
df2 %<>% 
  mutate(Pot_Limit_SummerReduction = ifelse(Pot_Limit_SummerReduction==500, 330, 200))
## join dfs back together  
testdf <- rbind(df1,df2)

testdf %<>% 
  select(season, month_name, season_month, interval, month_interval, Vessel, License, M1_tottraps, sum_PotsFished, Pot_Limit_SummerReduction, count_FishTicket) %>% 
  #Vessels pot limit in a 2-week interval is same as what is assumed to be its fished pot count using M2 method
  rename(Pot_Limit_or_M2 = Pot_Limit_SummerReduction)
glimpse(testdf)
# this summary df showcases the difference between pot counts via M1 or M2, an between summing raw pot count from logbooks



# calculating an estimate for lines in water as the sum of pot limits for those vessels that were active in a given time period 

check_lines_in_water <- testdf %>% 
  group_by(season, month_name, Pot_Limit_or_M2) %>% 
  na.omit() %>% 
  summarise(numberoflicenses=n_distinct(License), na.rm=TRUE) %>% 
  mutate(check_PotsFished=sum(numberoflicenses * Pot_Limit_or_M2)) %>% 
  select(season, month_name, check_PotsFished) %>% 
  distinct() %>% 
  collect()

#want to make sure that fishing data has all year - month combos so that the plot matches with whale plots
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("12","01","02","03","04", "05", "06", "07", "08", "09", "10", "11"))
season_month_combos <- crossing(season, month)

check_lines_in_water <-  check_lines_in_water %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  mutate(month = factor(month, levels = c('12','01','02','03','04','05','06','07','08','09','10','11'))) %>% 
  left_join(season_month_combos, check_lines_in_water , by=c("season", "month")) %>% 
  arrange(season, month) %>% 
  mutate(season_month = factor(paste0(season,"_",month))) %>% 
  filter(season_month != '2019-2020_11') %>% 
  filter(season_month != '2019-2020_10') %>% 
  mutate(check_PotsFished = ifelse(is.na(check_PotsFished), 0, check_PotsFished))

left_join(season_month_combos, check_lines_in_water , by=c("season", "month"))

#use ordered.ids as defiend from other data, as the logs won't have some months
#ordered.ids <- factor(check_lines_in_water$season_month, levels=check_lines_in_water$season_month)

ts_lines_in_water <- ggplot(
  data = check_lines_in_water, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    #y = Trap_dens_sum,
    y = check_PotsFished,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=3)])+
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  #ylab("Sum Trap Density\nin grids being used") + 
  ylab("Lines in water") + 
  xlab("Season_month") +
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
ts_lines_in_water



















#--------
#join and save all plots into one figure
#ts_trap_dens or ts_lines_in_water
#ts_hump_dens
#ts_blue_dens

path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"
path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"

# ts_sum_lines_in_water_v2 calcualted as number of unique license multiplied by their pot limit

png(paste0(path_figures, "/ts_sum_lines_in_water_v2_and_whales_2014_2020_by crab season_study_area_or_fishing_grids_using_1month_gridded_data.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(#ts_trap_dens,
          ts_lines_in_water,
          ts_hump_dens,
          ts_blue_dens,
          ncol=1,
          nrow=3,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())









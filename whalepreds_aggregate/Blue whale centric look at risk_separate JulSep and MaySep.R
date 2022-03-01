#' blue whale centric' look at risk - separate for Jul-Sep and May-Sep

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

# set some paths
# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

#whale data
# Leena:
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"

# where to put outputs
# Leena:
path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub

#-----------------------------------------------------------------------------------

# load the data
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)



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


# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)

#-----------------------------------------------------------------------------------

#First determine cut-off value based on the distribution of modelled bw values in study area
x.blue.all_crab_season <- x.blue.all %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

x.blue_2014_2020_crab_season_May_Sep <-  x.blue.all_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

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
study_area_bw <- full_join(study_area_df_with_all_season_month_combos, x.blue_2014_2020_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(study_area == 'Y')


#----------
##  May-Sep
#----------

# #if want to visualise using ggridges
library(ggridges)

study_area_bw$season <- factor(study_area_bw$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

ggplot(study_area_bw, aes(x = Blue_occurrence_mean, y = season, height = ..density..)) +
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Blue whale occurrence (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)




#find the mean +/- SD / median value from the distribution (in study area) for May-Sep

#data filtered to study area (bw model reaches outside of study area, giving NAs)
summary_study_area_bw_MaySep <- study_area_bw %>% 
  #data already limited to seasons of interest, and May-Sep  
  #get mean etc. values across full dataset
  summarise(Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE),
            Median_Blue_occurrence = median(Blue_occurrence_mean, na.rm=TRUE),
            sd_Blue_occurrence = sd(Blue_occurrence_mean, na.rm=TRUE))

# mean = 0.469
# median = 0.544
# sd = 0.229
# mean+sd = 0.698 #if use this as cutoff there is no overlap 
# mean-sd = 0.24


study_area_bw_v2 <-  study_area_bw %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID")

# calculate MEAN whale values for different grid in different seasons - for May-Sep period (2013-2020)
x.blue.mean_MaySep <- study_area_bw_v2 %>% 
  #data already restricted to be May-Sep
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_MaySep)


# start with df 'x.blue.mean' which is mean value in a grid across May-Sep per seasons
MaySep_good_bw_hab <- x.blue.mean_MaySep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0469 = ifelse(Mean_Blue_occurrence > 0.469, 'Y', 'N'),
         good_bw_hab_024 = ifelse(Mean_Blue_occurrence > 0.24, 'Y', 'N'),
         good_bw_hab_050 = ifelse(Mean_Blue_occurrence > 0.5, 'Y', 'N')
  ) %>%
  inner_join(grid.5km.lno) #join to have geometry column
glimpse(MaySep_good_bw_hab)


#---------------------------

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2019_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_MaySep_WA_fishery_footprint_20220202.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only_line.shp')) %>% 
  st_transform(st_crs(dissolved_2019_2020_MaySep))


# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49)

# plot blue whale
bw_subset_MaySep <- MaySep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(good_bw_hab_050)) %>% 
  filter(good_bw_hab_050 == 'Y')

map_blue_MaySep_good_hab <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=good_bw_hab_050,
              col=good_bw_hab_050
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black', linetype='dashed', size=1, fill = NA) +
  ggtitle("May-Sep 2019-2020 \ngood BW habitat (>0.50 occurrence) \nwith 2019-2020 May-Sep fishery footprint") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_blue_MaySep_good_hab


png(paste0(path_figures, "/good_bw_habitat_050_occur_MaySep_2019_2020_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_blue_MaySep_good_hab,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#---------------------------


#Then try to look at what trap density was like in the good bw habitat -- will need to check code
#bring in fishing data 
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

# average trap density (and count?) for each grid cell for May-Sep period
x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
    #sum_M2_tottraps  = sum(M2_tottraps, na.rm=TRUE),
    #mean_tottraps = sum_M2_tottraps/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) #, mean_tottraps
glimpse(x.fish_WA_MaySep)


MaySep_good_bw_hab_fishing <- MaySep_good_bw_hab %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID')) %>% 
  left_join(st_drop_geometry(grid.key), by = "GRID5KM_ID") 
glimpse(MaySep_good_bw_hab_fishing)


MaySep_good_bw_hab_fishing_risk <- MaySep_good_bw_hab_fishing %>% 
  mutate(
    blue_risk = Mean_Blue_occurrence * mean_trapdens
  )%>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(blue_risk = 
           ifelse(is.na(mean_trapdens), 0, blue_risk)
  )



summary_good_bw_habitat_fishing_MaySep_0469 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0469 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.469')
glimpse(summary_good_bw_habitat_fishing_MaySep_0469)

summary_good_bw_habitat_fishing_MaySep_024 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_024 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.24')
glimpse(summary_good_bw_habitat_fishing_MaySep_024)

summary_good_bw_habitat_fishing_MaySep_050 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_050 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2018-2019 season from May-Sep comparison
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.50')
glimpse(summary_good_bw_habitat_fishing_MaySep_050)


summary_probabilites_MaySep <- rbind(
  summary_good_bw_habitat_fishing_MaySep_024,
  summary_good_bw_habitat_fishing_MaySep_0469,
  summary_good_bw_habitat_fishing_MaySep_050
)



ts_risk_in_good_bw_habitat_MaySep <- ggplot(summary_probabilites_MaySep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=1.8) +
  geom_point(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=3.5) +
  ylab("Blue whale risk (sum)") +
  xlab("Season") +
  #ggtitle("May-Sep risk (sum)\nin good (>0.5 prob of occur.) BW habitat") +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    legend.position = c(.85, .25),
    axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=20),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_risk_in_good_bw_habitat_MaySep

ts_risk_in_good_bw_habitat_MaySep <- ggplot(summary_good_bw_habitat_fishing_MaySep_0469, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = 1), size=1.8) +
  geom_point(aes(y = risk_sum, group = 1), size=3.5) +
  ylab("Blue whale risk (sum)") +
  xlab("Season") +
  #ggtitle("May-Sep risk (sum)\nin good (>0.469 prob of occur.) BW habitat") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_risk_in_good_bw_habitat_MaySep


png(paste0(path_figures, "/ts_risk_in_0469_bw_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_risk_in_good_bw_habitat_MaySep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



##overlap
test_summary_0469 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0469 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.469')
#when use 0.469
# season    n_grids
#  2013-2014      10
#  2014-2015       9
#  2015-2016      55
#  2016-2017      63
#  2017-2018      95

#  2019-2020     101


test_summary_024 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_024 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.24')
#when use 0.24
#season       n_grids
#2013-2014      99
#2014-2015      87
#2015-2016      98
#2016-2017      123
#2017-2018      148

#2019-2020      104


test_summary_050 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_050 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2018-2019') %>% 
  mutate(prob_of_occur = '0.50')
#when use 0.5
#season       n_grids
#2013-2014      8
#2014-2015      2
#2015-2016      11
#2016-2017      14
#2017-2018      70

#2019-2020      75


summary_overlap_MaySep <- rbind(
  test_summary_0469,
  test_summary_024,
  test_summary_050
)

ts_overlapping_grids <- ggplot(summary_overlap_MaySep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=1.8) +
  geom_point(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=3.5) +
  ylab("Number of overlapping grids") +
  xlab("Season") +
  #ggtitle("May-Sep overlapping grids\nin good (>0.5 prob of occur.) BW habitat") +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    legend.position = c(.85, .2),
    axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=20),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_overlapping_grids

ts_overlapping_grids <- ggplot(test_summary_0469, aes(x=season)) +
  geom_line(aes(y = n_grids, group = 1), size=1.8) +
  geom_point(aes(y = n_grids, group = 1), size=3.5) +
  ylab("Number of overlapping grids") +
  xlab("Season") +
  #ggtitle("May-Sep overlapping grids\nin good (>0.469 prob of occur.) BW habitat") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_overlapping_grids


png(paste0(path_figures, "/ts_overlap_in_0469_bw_habitat_MaySep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_overlapping_grids,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



#----------
##  Jul-Sep
#----------

# #if want to visualise using ggridges
library(ggridges)

study_area_bw_JulSep <- study_area_bw %>% 
  filter(month %in% c('07','08', '09'))

study_area_bw_JulSep$season <- factor(study_area_bw_JulSep$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

ggplot(study_area_bw_JulSep, aes(x = Blue_occurrence_mean, y = season, height = ..density..)) +
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Blue whale occurrence (Jul-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)



#what if want to look for value for Jul-Sep
summary_study_area_bw_JulSep <- study_area_bw %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  #data already limited to seasons of interest, and May-Sep  
  #get mean etc. values across full dataset
  summarise(Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE),
            Median_Blue_occurrence = median(Blue_occurrence_mean, na.rm=TRUE),
            sd_Blue_occurrence = sd(Blue_occurrence_mean, na.rm=TRUE))
# mean = 0.626
# median = 0.629
# sd = 0.0907
# mean+sd = 0.717  
# mean-sd = 0.535

#-------------

study_area_bw_v2 <-  study_area_bw %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID")

# calculate MEAN whale values for different grid in different seasons - for JUL-Sep period (2013-2020)
x.blue.mean_JulSep <- study_area_bw_v2 %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean_JulSep)





JulSep_good_bw_hab <- x.blue.mean_JulSep %>% 
  group_by(season) %>% 
  mutate(good_bw_hab_0535 = ifelse(Mean_Blue_occurrence > 0.535, 'Y', 'N'),
         good_bw_hab_0626 = ifelse(Mean_Blue_occurrence > 0.626, 'Y', 'N'),
         good_bw_hab_0717 = ifelse(Mean_Blue_occurrence > 0.717, 'Y', 'N')
  ) %>%
  inner_join(grid.5km.lno) #join to have geometry column
glimpse(JulSep_good_bw_hab)


#---------------------------
## will probably need to get Jul-Sep footprints
#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2019_2020_JulSep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_JulSep_WA_fishery_footprint_20220227.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only_line.shp')) %>% 
  st_transform(st_crs(dissolved_2019_2020_JulSep))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-122,49) 

# plot blue whale
bw_subset_JulSep <- JulSep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(good_bw_hab_0717)) %>% 
  filter(good_bw_hab_0717 == 'Y')

map_blue_JulSep_good_hab <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_JulSep), 
          aes(fill=good_bw_hab_0717,
              col=good_bw_hab_0717
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2019_2020_JulSep, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black', linetype='dashed', size=1, fill = NA) +
  ggtitle("Jul-Sep 2019-2020 \ngood BW habitat (>0.717 occurrence) \nwith 2019-2020 Jul-Sep fishery footprint") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_blue_JulSep_good_hab



png(paste0(path_figures, "/good_bw_habitat_0717_occur_MaySep_2019_2020_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_blue_JulSep_good_hab,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#---------------------------

#Then try to look at what trap density was like in the good bw habitat -- will need to check code
#bring in fishing data 
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

# average trap density (and count?) for each grid cell for May-Sep period
x.fish_WA_JulSep <- x.fish_WA %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  summarise(
    sum_M2_trapdens = sum(M2_trapdens, na.rm=TRUE),
    number_obs = n(), #no. of grid cells being used for averaging
    mean_trapdens = sum_M2_trapdens/number_obs
    #sum_M2_tottraps  = sum(M2_tottraps, na.rm=TRUE),
    #mean_tottraps = sum_M2_tottraps/number_obs
  ) %>% 
  #and drop some columns so that after join things are little tidier
  select(season, GRID5KM_ID, mean_trapdens) #, mean_tottraps
glimpse(x.fish_WA_JulSep)



JulSep_good_bw_hab_fishing <- JulSep_good_bw_hab %>% 
  left_join(x.fish_WA_JulSep, by=c('season', 'GRID5KM_ID')) %>% 
  left_join(st_drop_geometry(grid.key), by = "GRID5KM_ID") 
glimpse(JulSep_good_bw_hab_fishing)




JulSep_good_bw_hab_fishing_risk <- JulSep_good_bw_hab_fishing %>% 
  mutate(
    blue_risk = Mean_Blue_occurrence * mean_trapdens
  )%>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(blue_risk = 
           ifelse(is.na(mean_trapdens), 0, blue_risk)
  )


  

summary_good_bw_habitat_fishing_JulSep_0535 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0535 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.535')
glimpse(summary_good_bw_habitat_fishing_JulSep_0535)  

summary_good_bw_habitat_fishing_JulSep_0626 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0626 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.626')
glimpse(summary_good_bw_habitat_fishing_JulSep_0626)  

summary_good_bw_habitat_fishing_JulSep_0717 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0717 == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    risk_sum = sum(blue_risk, na.rm=TRUE)) %>% 
  #remove 2019-2020 season
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.717')
glimpse(summary_good_bw_habitat_fishing_JulSep_0717)  



summary_probabilites_JulSep <- rbind(
  summary_good_bw_habitat_fishing_JulSep_0535,
  summary_good_bw_habitat_fishing_JulSep_0626,
  summary_good_bw_habitat_fishing_JulSep_0717
)


ts_risk_in_good_bw_habitat_JulSep <- ggplot(summary_probabilites_JulSep, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=1.8) +
  geom_point(aes(y = risk_sum, group = prob_of_occur, color = prob_of_occur), size=3.5) +
  ylab("Blue whale risk (sum)") +
  xlab("Season") +
  #ggtitle("Jul-Sep risk (sum)\nin good (>0.5 prob of occur.) BW habitat") +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    legend.position = c(.85, .15),
    axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=20),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_risk_in_good_bw_habitat_JulSep

ts_risk_in_good_bw_habitat_JulSep <- ggplot(summary_good_bw_habitat_fishing_JulSep_0626, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = 1), size=1.8) +
  geom_point(aes(y = risk_sum, group = 1), size=3.5) +
  ylab("Blue whale risk (sum)\nJul-Sep") +
  xlab("Season") +
  #ggtitle("Jul-Sep risk (sum)\nin good (>0.626 prob of occur.) BW habitat") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_risk_in_good_bw_habitat_JulSep


png(paste0(path_figures, "/ts_risk_in_0626_bw_habitat_JulSep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_risk_in_good_bw_habitat_JulSep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())




#overlap

#Jul-Sep
test_summary_0626 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0626 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.626')
#when use 0.626
# season    n_grids
#  2013-2014      12
#  2014-2015      21
#  2015-2016      38
#  2016-2017      38
#  2017-2018      55
#  2018-2019      56

test_summary_0535 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0535 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  mutate(prob_of_occur = '0.535')
#when use 0.535
# season    n_grids
#  2013-2014      49
#  2014-2015      33
#  2015-2016      61
#  2016-2017      60
#  2017-2018      77
#  2018-2019      67

test_summary_0717 <- JulSep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab_0717 == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n()) %>% 
  filter(season != '2019-2020') %>% 
  #no overlap in 2015-2016, add a row
  add_row(season = c("2013-2014", "2014-2015", "2015-2016", "2016-2017"), n_grids = 0) %>% 
  mutate(prob_of_occur = '0.717')
#when use 0.717
# season    n_grids
#  2013-2014      0
#  2014-2015      0
#  2015-2016      0
#  2016-2017      0
#  2017-2018      11
#  2018-2019      21


summary_overlap_JulSep <- rbind(
  test_summary_0535,
  test_summary_0626,
  test_summary_0717
)



ts_overlapping_grids_JulSep <- ggplot(summary_overlap_JulSep, aes(x=season)) +
  geom_line(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=1.8) +
  geom_point(aes(y = n_grids, group = prob_of_occur, color=prob_of_occur), size=3.5) +
  ylab("Number of overlapping grids") +
  xlab("Season") +
  #ggtitle("Jul-Sep overlapping grids\nin good (>0.5 prob of occur.) BW habitat") +
  guides(color=guide_legend(title="Prob. of occur.")) +
  theme_classic() +
  theme(#legend.title = element_blank(),
    legend.title = element_text(size=20),
    #title = element_text(size = 15),
    legend.text = element_text(size=20),
    legend.position = c(.15, .8),
    axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=20),
    strip.background = element_blank(),
    strip.placement = "left"
  )
ts_overlapping_grids_JulSep

ts_overlapping_grids_JulSep <- ggplot(test_summary_0626, aes(x=season)) +
  geom_line(aes(y = n_grids, group = 1), size=1.8) +
  geom_point(aes(y = n_grids, group = 1), size=3.5) +
  ylab("Number of overlapping grids") +
  xlab("Season") +
  #ggtitle("Jul-Sep overlapping grids\nin good (>0.626 prob of occur.) BW habitat") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_overlapping_grids_JulSep



png(paste0(path_figures, "/ts_overlap_in_0626_bw_habitat_JulSep.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_overlapping_grids_JulSep,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())










#---------------------------
# This mapping hasn't been updated for separate Jul-Sep and May-Sep comparisons
#---------------------------

# map example of most likely bw habitat with NON-confidential summer fishery footprint

# would need to get Jul-Sep specific footprint

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
#dissolved_2014_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint.rds'))
dissolved_2014_2020_MaySep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only.shp')) %>% 
  st_transform(st_crs(dissolved_2014_2020_MaySep_non_conf)) #make it have same projection 


# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,46,-120,49) 

# plot blue whale
bw_subset_MaySep <- JulSep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(good_bw_hab)) %>% 
  filter(good_bw_hab == 'Y')

map_blue_MaySep_good_hab <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=good_bw_hab,
              col=good_bw_hab
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2013_2014_MaySep_non_conf, color = 'black',size=1, fill = NA) +
  geom_sf(data = dissolved_study_area, color = 'black',linetype = "dotted",size=1, fill = NA) +
  ggtitle("May-Sep 2019-2020 \ngood BW habitat (>0.469 occurrence) \nnon-conf. May-Sep fishery footprint (across 2014-2020)") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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
map_blue_MaySep_good_hab



png(paste0(path_figures, "/good_bw_habitat_0469_occur_MaySep_2019_2020_with_all_NONCONF_summer_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_blue_MaySep_good_hab,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())











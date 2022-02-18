#' blue whale centric' look at risk

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
# Jameal
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# whale outputs overlayed on 5km grid (i.e., not subset to DCRB fishing cells)
# Jameal
path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"

# Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
#path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


# where to put outputs
# Jameal
path_figures <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOWT/raimbow/whalepreds_aggregate/figures"

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

grid.key_N44 <- grid.key %>% 
  filter(LATITUDE > 44) %>% 
  #into this join area_km_lno info from layer: grid.5km.lno
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

#hw output 2009-July 2019
# x.hump <- readRDS(path.hump) %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# glimpse(x.hump)

#hw output 2009-2020
# x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
# glimpse(x.hump_2009_2020)


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

x.blue_crab_season <- x.blue.all %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") %>% # adds grid cell area  
  separate(year_month, into = c("year", "month"), sep = "_")  
x.blue_crab_season_v2 <- x.blue_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(season_month = paste0(season,"_",month)) %>% 
  select(-season_start, -season_end) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) 
glimpse(x.blue_crab_season_v2)


# calculate MEAN whale values for different seasons - for May-Sep period (2013-2020)
x.blue.mean <- x.blue_crab_season_v2 %>%
  filter(is_May_Sep == "Y") %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  inner_join(grid.key_N44)
  #inner_join(grid.5km.lno) #if don't want ro clip at e.g. 44N
glimpse(x.blue.mean)

#-----------------------------------------------------------------------------------

#'good bw habitat', has very minimal overlap with fishery footprint (depends on definition of good habitat)
#this lack of overlap hinders doing a similar look with bw as with hw (where good habitat overlaps much more with fishery)
#due to the more coastal dist of hw, vs more offshore dist of bw

# try: if good bw habitat is defined as grids where average probability of occurrence is >0.50

#these were done with the old fishing data (not clipped to WA waters, didn't include OR effort)
#after updating fishing data, only tier 0.5 and 0.55, and 0.55 didn't have enough overlap
# 0.75 probability of occurrence was too high cut-off, no good bw habitat in WA with that cut-off
# 0.70 probability of occurrence was also too high cut-off, no good bw habitat overlap with fishery
# 0.65 probability of occurrence was also too high cut-off, no good bw habitat overlap with fishery
# 0.60 probability of occurrence was also too high cut-off, almost no good bw habitat overlap with fishery
# 0.55 probability of occurrence had some good bw habitat overlapping with fishery in some years
# 0.50 probability of occurrence had some good bw habitat overlapping with fishery in most years

# start with df 'x.blue.mean' (x.whale.mean) which is mean value in a grid across May-Sep per seasons
MaySep_good_bw_hab_050_occur <- x.blue.mean %>% 
  #drop hw as this is bw specific
  #select(-Mean_Humpback_dens) %>% 
  group_by(season) %>% 
  mutate(BW_is_050_occur_or_higher = ifelse(Mean_Blue_occurrence > 0.50, 'Y', 'N')
  ) %>%
  inner_join(grid.key_N44)
  #inner_join(grid.5km.lno) #if don't want ro clip at e.g. 44N
glimpse(MaySep_good_bw_hab_050_occur)

#0.5 or 0.55 probability of occurrence seem the only values that would be useful, save a joint file
# MaySep_good_bw_hab_050_055_occur <- MaySep_good_bw_hab_050_occur %>% 
#   left_join(MaySep_good_bw_hab_055_occur, by = c("season", "is_May_Sep", "GRID5KM_ID", "area_km_lno", "Mean_Blue_occurrence", "geometry"))
# write_rds(MaySep_good_bw_hab_050_055_occur,here::here('wdfw','data',"MaySep_good_bw_hab_050_055_occur.rds"))



#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2013_2014_MaySep <- read_rds(here::here('wdfw','data','dissolved_2013_2014_MaySep_WA_fishery_footprint_20220202.rds'))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
#bbox = c(-127,30,-115,49) #extent inc CA
bbox = c(-127,43.5,-120,49) 

# plot blue whale
bw_subset_MaySep <- MaySep_good_bw_hab_050_055_occur %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
  filter(!is.na(BW_is_050_occur_or_higher)) %>% 
  filter(BW_is_050_occur_or_higher == 'Y')

map_blue_MaySep_050_occur <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
          aes(fill=BW_is_050_occur_or_higher,
              col=BW_is_050_occur_or_higher
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2013_2014_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("May-Sep 2013-2014 \ngood BW habitat (>0.50 occurrence) \nwith 2013-2014 May-Sep fishery footprint") +
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
map_blue_MaySep_050_occur

png(paste0(path_figures, "/good_bw_habitat_050_occur_MaySep_2013_2014_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_blue_MaySep_050_occur,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())





#----------------------------------------------------------------

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


MaySep_good_bw_hab_050_055_occur <- read_rds(here::here('wdfw', 'data','MaySep_good_bw_hab_050_055_occur.rds'))
MaySep_good_bw_hab_050_055_occur_fishing <- MaySep_good_bw_hab_050_055_occur %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID')) %>% 
  left_join(st_drop_geometry(grid.key), by = "GRID5KM_ID") %>% 
  filter(LATITUDE > 44)
glimpse(MaySep_good_bw_hab_050_055_occur_fishing)

MaySep_good_bw_hab_050_055_occur_fishing_risk <- MaySep_good_bw_hab_050_055_occur_fishing %>% 
  mutate(
    blue_risk = Mean_Blue_occurrence * mean_trapdens
  )%>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(blue_risk = 
           ifelse(is.na(mean_trapdens), 0, blue_risk)
  )

summary_050_bw_habitat_fishing <- MaySep_good_bw_hab_050_055_occur_fishing_risk %>% 
  filter(BW_is_050_occur_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE),
            risk_mean = mean(blue_risk, na.rm=TRUE),
            risk_sum = sum(blue_risk, na.rm=TRUE),
            sd = sd(blue_risk, na.rm = TRUE),
            n = n()
            #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            #tottraps_median = median(mean_tottraps, na.rm=TRUE)
  )%>% 
  mutate(se = sd / sqrt(n),
         lower.ci = risk_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = risk_mean + qt(1 - (0.05 / 2), n - 1) * se)
glimpse(summary_050_bw_habitat_fishing)  


ts_fishing_in_050_bw_habitat <- ggplot(summary_050_bw_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1)) + 
  geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (>0.5 prob of occur.) BW habitat") +
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
ts_fishing_in_050_bw_habitat

ts_fishing_in_050_bw_habitat <- ggplot(summary_050_bw_habitat_fishing, aes(x=season)) +
  geom_line(aes(y = risk_mean, group = 1)) +
  geom_point(aes(y = risk_mean, group = 1), size=2) +
  geom_errorbar(aes(x = season,ymin = lower.ci, ymax = upper.ci), colour="black", width=.2)+
  ylab("Blue whale risk (mean +/- 95% CI)") +
  xlab("Season") +
  ggtitle("May-Sep risk (mean +/- 95% CI)\nin good (>0.5 prob of occur.) BW habitat") +
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
ts_fishing_in_050_bw_habitat

png(paste0(path_figures, "/ts_risk_mean_CI_in_050_bw_habitat_MaySep_risk_NA_if_no_overlap_clipped_at_44N.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_050_bw_habitat,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#THERE IS NOT ENOUGH OVERLAP BETWEEN FISHERY AND >0.55 BW HABITAT
# summary_055_bw_habitat_fishing <- MaySep_good_bw_hab_050_055_occur_fishing %>% 
#   filter(BW_is_055_occur_or_higher == 'Y') %>% 
#   group_by(season) %>% 
#   summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
#             trapdens_median = median(mean_trapdens, na.rm=TRUE)
#             #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
#             #tottraps_median = median(mean_tottraps, na.rm=TRUE)
#   )
# glimpse(summary_055_bw_habitat_fishing)
# 
# ts_fishing_in_055_bw_habitat <- ggplot(summary_055_bw_habitat_fishing, aes(x=season)) + 
#   geom_line(aes(y = trapdens_mean, group = 1)) + 
#   geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
#   geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
#   geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
#   ylab("Trap density") + 
#   xlab("Season") +
#   ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (>0.55 prob of occur.) BW habitat") +
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 26),
#         legend.text = element_text(size = 20),
#         legend.position = c(.15, .85),
#         axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         strip.text = element_text(size=12),
#         strip.background = element_blank(),
#         strip.placement = "left"
#   )
# ts_fishing_in_055_bw_habitat
# 
# 
# 
# 
# png(paste0(path_figures, "/ts_fishing_in_050_055_bw_habitat_MaySep_20220202.png"), width = 17, height = 10, units = "in", res = 300)
# ggarrange(ts_fishing_in_050_bw_habitat,
#           ts_fishing_in_055_bw_habitat,
#           ncol=2,
#           nrow=1,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())


#THIS HASN'T BEEN UPDATED FRO WA WATERS + OR DATA
# change in mean trap dens between 2017-28 and 2019-20 seasons when prob occur >0.5
# season      mean trap dens    max trap dens
#2017-2018      5.0425274         49.504770
#2019-2020      3.8596105         29.494404
# change          -23%              -40%



#######################################################################################################
########################################################################################################
#start fully new test here

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
# Jameal
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# whale outputs overlayed on 5km grid (i.e., not subset to DCRB fishing cells)
# Jameal
path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"

# Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
#path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


# where to put outputs
# Jameal
path_figures <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOWT/raimbow/whalepreds_aggregate/figures"

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
# #if want to visualise using ggridges
library(ggridges)
study_area_bw$season <- factor(study_area_bw$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))
#warning message: removed 1400 rows containing non-finite values is due to NAs
ggplot(study_area_bw, aes(x = Blue_occurrence_mean, y = season, height = ..density..)) +
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Blue whale occurrence (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)

#the other ggridges plot that peaked around 0.3 was actually wrong, it was not clipped to study area,
#due to using full_join it had bw grids out to the west of study area
#----------

#find the mean +/- SD / median value from the distribution (in study area)

#filter to study area (bw model reaches outside of study areagiving NAs)
summary_study_area_bw <- study_area_bw %>% 
  #filter(study_area == 'Y') %>% 
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

#----------------------------------------------------------------------------
  
  
study_area_bw_v2 <-  study_area_bw %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID")

# calculate MEAN whale values for different grid in different seasons - for May-Sep period (2013-2020)
x.blue.mean <- study_area_bw_v2 %>% 
  #data already restricted to be May-Sep
  group_by(season, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Blue_occurrence = mean(Blue_occurrence_mean, na.rm=TRUE)
  ) 
glimpse(x.blue.mean)


# start with df 'x.blue.mean' which is mean value in a grid across May-Sep per seasons
MaySep_good_bw_hab <- x.blue.mean %>% 
  group_by(season) %>% 
  mutate(good_bw_hab = ifelse(Mean_Blue_occurrence > 0.469, 'Y', 'N')
  ) %>%
  inner_join(grid.5km.lno) #join to have geometry column
glimpse(MaySep_good_bw_hab)


#---------------------------

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2013_2014_MaySep <- read_rds(here::here('wdfw','data','dissolved_2013_2014_MaySep_WA_fishery_footprint_20220202.rds'))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 

# plot blue whale
bw_subset_MaySep <- MaySep_good_bw_hab %>% 
  #select season to map 
  filter(season == "2013-2014") %>% 
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
  geom_sf(data = dissolved_2013_2014_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("May-Sep 2013-2014 \ngood BW habitat (>0.24 occurrence) \nwith 2013-2014 May-Sep fishery footprint") +
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



png(paste0(path_figures, "/good_bw_habitat_024_occur_MaySep_2013_2014_with_fishery_footprint.png"), width = 14, height = 10, units = "in", res = 300)
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

summary_good_bw_habitat_fishing <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab == 'Y') %>% 
  group_by(season) %>% 
  summarise(
    #trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            #trapdens_median = median(mean_trapdens, na.rm=TRUE),
            #risk_mean = mean(blue_risk, na.rm=TRUE),
            risk_sum = sum(blue_risk, na.rm=TRUE)#,
            #sd = sd(blue_risk, na.rm = TRUE),
            #n = n()
            #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            #tottraps_median = median(mean_tottraps, na.rm=TRUE)
  )#%>% 
  #mutate(se = sd / sqrt(n),
   #      lower.ci = risk_mean - qt(1 - (0.05 / 2), n - 1) * se,
    #     upper.ci = risk_mean + qt(1 - (0.05 / 2), n - 1) * se)
glimpse(summary_good_bw_habitat_fishing)  


ts_fishing_in_good_bw_habitat <- ggplot(summary_good_bw_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1)) + 
  geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (>0.469 prob of occur.) BW habitat") +
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
ts_fishing_in_good_bw_habitat

ts_risk_in_good_bw_habitat <- ggplot(summary_good_bw_habitat_fishing, aes(x=season)) +
  geom_line(aes(y = risk_sum, group = 1)) +
  geom_point(aes(y = risk_sum, group = 1), size=2) +
  #geom_errorbar(aes(x = season,ymin = lower.ci, ymax = upper.ci), colour="black", width=.2)+
  #ylab("Blue whale risk (mean +/- 95% CI)") +
  ylab("Blue whale risk (sum)") +
  xlab("Season") +
  #ggtitle("May-Sep risk (mean +/- 95% CI)\nin good (>0.469 prob of occur.) BW habitat") +
  ggtitle("May-Sep risk (sum)\nin good (>0.469 prob of occur.) BW habitat") +
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
ts_risk_in_good_bw_habitat

png(paste0(path_figures, "/ts_trap_dens_risk_in_0469_bw_habitat_MaySep_risk_0_if_no_overlap_in_study_area.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_good_bw_habitat,
          ts_risk_in_good_bw_habitat,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#---------------------------

# map example of most likely bw habitat with NON-confidential summer fishery footprint

#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
#dissolved_2014_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint.rds'))
dissolved_2013_2014_MaySep_non_conf <- read_rds(here::here('wdfw','data','dissolved_2014_2020_MaySep_WA_fishery_footprint_NONCONF.rds'))

dissolved_study_area <- read_sf(here::here('wdfw','data','study_area_dissolved_boundary_only.shp')) %>% 
  st_transform(st_crs(dissolved_2013_2014_MaySep_non_conf)) #make it have same projection 


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
bw_subset_MaySep <- MaySep_good_bw_hab %>% 
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







#ggridge of MaySep_good_bw_hab_fishing_risk - risk calculated but not aceraged for ts
MaySep_good_bw_hab_fishing_risk$season <- factor(MaySep_good_bw_hab_fishing_risk$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

MaySep_good_bw_hab_fishing_risk_v2 <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab == 'Y') %>% 
  filter(!is.na(mean_trapdens))

ggplot(MaySep_good_bw_hab_fishing_risk_v2, aes(x = blue_risk, y = season, height = ..density..)) +
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Risk (May-Sep) in 'most likely bw habitat' (>0.469") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)



# # grab a base map
# rmap.base <- c(
#   st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
#     filter(admin %in% c("Canada", "Mexico")) %>%
#     st_geometry() %>%
#     st_transform(st_crs(grid.5km.lno))
# )
# 
# #bbox
# bbox = c(-127,46,-120,49) 
# 
# # plot blue whale
# bw_subset_MaySep <- MaySep_good_bw_hab_fishing_risk_v2 %>% 
#   #select season to map 
#   filter(season == "2015-2016") 
# 
# map_blue_MaySep_good_hab <- ggplot() + 
#   geom_sf(data=sf::st_as_sf(bw_subset_MaySep), 
#           aes(fill=blue_risk,
#               col=blue_risk
#           )
#   ) +
#   # facet_wrap(~time_period, nrow=1) +
#   geom_sf(data=rmap.base,col=NA,fill='gray50') +
#   #scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
#   #scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence",breaks=seq(0.06,0.91,by=0.25),limits=c(0.06,0.91),oob=squish) + 
#   #scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
#   #scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
#   ggtitle("May-Sep 2015-20164 \ngood BW habitat (>0.469 occurrence) \nwith May-Sep fishery footprint (across all 2014-2020") +
#   coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
#   #coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
#   theme_minimal() + #theme_classic() +
#   theme(text=element_text(family="sans",size=10,color="black"),
#         legend.text = element_text(size=10),
#         axis.title=element_text(family="sans",size=14,color="black"),
#         axis.text=element_text(family="sans",size=8,color="black"),
#         panel.grid.major = element_line(color="gray50",linetype=3),
#         axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
#         strip.text = element_text(size=14),
#         title=element_text(size=16)
#   )
# map_blue_MaySep_good_hab



test_summary <- MaySep_good_bw_hab_fishing_risk %>% 
  filter(good_bw_hab == 'Y') %>% 
  filter(!is.na(mean_trapdens)) %>% 
  group_by(season) %>% 
  summarise(n_grids = n())
#when use 0.469
# season    n_grids
#  2013-2014      10
#  2014-2015       9
#  2015-2016      55
#  2016-2017      63
#  2017-2018      95
#  2018-2019      77
#  2019-2020     101

#when use 0.24
#season       n_grids
#2013-2014      99
#2014-2015      87
#2015-2016      98
#2016-2017      123
#2017-2018      148
#2018-2019      115
#2019-2020      104

#when use 0.5
#season       n_grids
#2013-2014      8
#2014-2015      2
#2015-2016      11
#2016-2017      14
#2017-2018      70
#2018-2019      43
#2019-2020      75


ts_overlapping_grids <- ggplot(test_summary, aes(x=season)) +
  geom_line(aes(y = n_grids, group = 1)) +
  geom_point(aes(y = n_grids, group = 1), size=2) +
  ylab("Number of overlapping grids") +
  xlab("Season") +
  ggtitle("May-Sep overlapping grids\nin good (>0.5 prob of occur.) BW habitat") +
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
ts_overlapping_grids



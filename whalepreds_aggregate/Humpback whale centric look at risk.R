#'humpback whale centric' look at risk

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

# Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
#path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
#path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


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

#hw output 2009-July 2019
# x.hump <- readRDS(path.hump) %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# glimpse(x.hump)

#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)




# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)



#-----------------------------------------------------------------------------------
#WA fishery footprint is north of 44N during all of 2013-2020 - clip whale data to that

grid.key_N44 <- grid.key %>% 
  filter(LATITUDE > 44) %>% 
  #into this join area_km_lno info from layer: grid.5km.lno
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

grid.key_N46 <- grid.key %>% 
  filter(LATITUDE > 46.26) %>% 
  #into this join area_km_lno info from layer: grid.5km.lno
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area


x.hump_whale <- x.hump_2009_2020 %>% 
  inner_join(st_drop_geometry(grid.key_N44), by = "GRID5KM_ID")
  #inner_join(st_drop_geometry(grid.key_N46), by = "GRID5KM_ID")


#instead of working in calendar years, work in crab seasons -- also filter to 2013-2020
x.whale_crab_season <- x.hump_whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.whale_crab_season_v2 <- x.whale_crab_season %>% 
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
glimpse(x.whale_crab_season_v2)

#----------------------------------------------------------------------------
# find 75th percentile value for hw across 2013-2020, in clipped (44N/46N) extent

# calculate MEAN whale values for grids across all of 2013-2020 - for May-Sep period only
# and then look at percentiles
x.whale.mean_all2013_2020 <- x.whale_crab_season_v2 %>% #this is already filtered for 2013-2020
  filter(is_May_Sep == "Y") %>% 
  group_by(is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    #hw specific
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  inner_join(grid.key_N44)
  #inner_join(grid.key_N46)
glimpse(x.whale.mean_all2013_2020)

#find the 75th percentile value from across 2013-2020
x.whale.all2013_2020_75th_quant <- x.whale.mean_all2013_2020 %>%
  #select(Mean_Humpback_dens, Mean_Blue_occurrence) %>%
  ungroup() %>% 
  summarise(
    Humpback_dens_75th = quantile(Mean_Humpback_dens, probs=0.75, na.rm=TRUE)
  ) 
glimpse(x.whale.all2013_2020_75th_quant)
# 0.02466905, when clipped to 44N
# if in the first clump of code above DON't get a mean for each grid across 
# May-Sep 2013-2020, then 75th percentile value is 0.02408221
# also the ts plot is slightly different, but overall same story

#apply percentile value to each season
x.whale.mean_by_season <- x.whale_crab_season_v2 %>%
  filter(is_May_Sep == "Y") %>% 
  group_by(season, is_May_Sep, GRID5KM_ID, area_km_lno) %>%
  summarise(
    Mean_Humpback_dens = mean(Humpback_dens_mean, na.rm=TRUE)) %>%
  inner_join(grid.key_N44)
  #inner_join(grid.key_N46)
glimpse(x.whale.mean_by_season)

x.whale.all2013_2020_MaySep_quant_joined <- x.whale.mean_by_season %>% 
  cbind(x.whale.all2013_2020_75th_quant)
glimpse(x.whale.all2013_2020_MaySep_quant_joined)

x.whale.all2013_2020_MaySep_good_habitats <- x.whale.all2013_2020_MaySep_quant_joined %>% 
  ungroup() %>% 
  mutate(HW_is_75th_or_higher = ifelse(Mean_Humpback_dens > Humpback_dens_75th, 'Y', 'N')) %>%
  inner_join(grid.key_N44)
  #inner_join(grid.key_N46)
glimpse(x.whale.all2013_2020_MaySep_good_habitats)

#----------------
#Then try to look at what trap density was in the good (75th) hw habitat -- will need to check code
#bring in fishing data 
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
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


x.whale.2013_2020_MaySep_good_habitats_fishing <- x.whale.all2013_2020_MaySep_good_habitats %>% 
  left_join(x.fish_WA_MaySep, by=c('season', 'GRID5KM_ID'))
glimpse(x.whale.2013_2020_MaySep_good_habitats_fishing)


#in this situation doesn't matter if risk is 0 or NA
x.whale.2013_2020_MaySep_good_habitats_fishing_risk <- x.whale.2013_2020_MaySep_good_habitats_fishing %>% 
  mutate(
    hump_risk = Mean_Humpback_dens * mean_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
         ifelse(is.na(mean_trapdens), 0, hump_risk)
  )

summary_75th_HW_habitat_fishing <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  filter(HW_is_75th_or_higher == 'Y') %>% 
  group_by(season) %>% 
  summarise(trapdens_mean = mean(mean_trapdens, na.rm=TRUE),
            trapdens_median = median(mean_trapdens, na.rm=TRUE),
            risk_mean = mean(hump_risk, na.rm=TRUE),
            risk_sum = sum(hump_risk, na.rm=TRUE),
            sd = sd(hump_risk, na.rm = TRUE),
            n = n()
            #tottraps_mean = mean(mean_tottraps, na.rm=TRUE),
            #tottraps_median = median(mean_tottraps, na.rm=TRUE)
            
  ) %>% 
  mutate(se = sd / sqrt(n),
         lower.ci = risk_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = risk_mean + qt(1 - (0.05 / 2), n - 1) * se)
glimpse(summary_75th_HW_habitat_fishing)  


ts_fishing_in_75th_hw_habitat <- ggplot(summary_75th_HW_habitat_fishing, aes(x=season)) + 
  geom_line(aes(y = trapdens_mean, group = 1)) + 
  geom_point(aes(y = trapdens_mean, group = 1), size=2) + 
  geom_line(aes(y = trapdens_median, group = 1), color = "darkred", linetype="twodash") + 
  geom_point(aes(y = trapdens_median, group = 1), color = "darkred", size=2) + 
  ylab("Trap density") + 
  xlab("Season") +
  ggtitle("May-Sep trap density \nmean = solid line, median = dashed line \nin good (75th) HW habitat \n(defined across 2013-2020)") +
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
ts_fishing_in_75th_hw_habitat

#plot follows same curve if use risk (instead of trap)
ts_fishing_in_75th_hw_habitat <- ggplot(summary_75th_HW_habitat_fishing, aes(x=season)) +
  geom_line(aes(y = risk_mean, group = 1)) +
  geom_point(aes(y = risk_mean, group = 1), size=2) +
  geom_errorbar(aes(x = season,ymin = lower.ci, ymax = upper.ci), colour="black", width=.2)+
  ylab("Humpback whale risk (mean +/- 95% CI)") +
  xlab("Season") +
  ggtitle("May-Sep risk (mean +/- 95% CI)\nin good (75th) HW habitat \n(defined across 2013-2020, clipped to 44N)") +
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
ts_fishing_in_75th_hw_habitat


png(paste0(path_figures, "/ts_risk_mean_CI_in_75th_hw_habitat_MaySep_clipped_to44N_risk_0_if_no_overlap.png"), width = 17, height = 10, units = "in", res = 300)
ggarrange(ts_fishing_in_75th_hw_habitat,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



#----------------------------------------------------------------------------------------
#map all seasons May_Sep good whale habitats with fishery footprint for that season's May-Sep
dissolved_2019_2020_MaySep <- read_rds(here::here('wdfw','data','dissolved_2019_2020_MaySep_WA_fishery_footprint_20220202.rds'))

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-127,43.5,-120,49) 
#bbox = c(-127,45.5,-120,49) 


hw_subset_MaySep <- x.whale.2013_2020_MaySep_good_habitats_fishing_risk %>% 
  #select season to map 
  filter(season == "2019-2020") %>% 
  filter(!is.na(HW_is_75th_or_higher)) %>% 
  filter(HW_is_75th_or_higher == 'Y')


map_hump_MaySep_75th <- ggplot() + 
  geom_sf(data=sf::st_as_sf(hw_subset_MaySep), 
          aes(fill=HW_is_75th_or_higher,
              col=HW_is_75th_or_higher
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity",breaks=seq(0,0.04,by=0.01),limits=c(0.0,0.04),oob=squish) + 
  scale_fill_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  scale_color_manual(values = c("mediumspringgreen"), name = "Good whale habitat", labels = c("Yes")) +
  geom_sf(data = dissolved_2019_2020_MaySep, color = 'black',size=1, fill = NA) +
  ggtitle("May-Sep 2019-2020 \ngood HW habitat (75th+) \nspatially clip at 44N \nwith 2019-2020 May-Sep fishery footprint") +
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
map_hump_MaySep_75th

png(paste0(path_figures, "/good_wh_habitat_MaySep_75th_across_20132020_applied to each season_2019-2020_spatially_clipped_44.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(map_hump_MaySep_75th,
          ncol=1,
          nrow=1,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

#------------------------------------------------------------------------------------------------------------











 
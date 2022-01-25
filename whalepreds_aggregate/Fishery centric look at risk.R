#Fishery centric look at risk


#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

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


#path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#risk to whales in STUDY AREA during May-Sep


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


#fishing effort

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #restrict fishing data to May-Sep as was done to whale data
  filter(month %in% c('05', '06', '07', '08', '09'))


#this is an older version when wanted to have a year_month column (2020_01) to join to whale data
# make column for year month for fishing data to allow matching with whale data
## step 1, make some columns that we can use
# x.fish_WA3 <- x.fish_WA2 %>%
#   separate(season_month, into = c("season", "month_name"), sep = "_") %>%
#   separate(season, into = c("yr_start", "yr_end"), sep = "-") %>%
#   mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
#   mutate(month = sprintf("%02d", as.numeric(month))) #change month to two digit number
# ## step 2, grab yr_start for December effort and make a year_month column in a new df
# df1 <- x.fish_WA3 %>%
#   filter(month_name=='December')
# df1 <- df1 %>%
#   mutate(year_month = paste0(yr_start,"_",month))
# ## step 3, grab yr_end for non-December effort and make a year_month column in a new df
# df2 <- x.fish_WA3 %>%
#   filter(month_name !='December')
# df2 <- df2 %>%
#   mutate(year_month = paste0(yr_end,"_",month))
# # squish the December and non-December df's together  
# x.fish_WA4 <- rbind(df1,df2)


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

#join fishing data to study area grid with whale data
study_area_whale_fishing <- left_join(study_area_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep <- study_area_whale_fishing %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
   mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))

 

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


subset_data <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y') %>% #restrict map to study area/check that all grids in study area show up
  filter(season == "2019-2020") %>% 
  filter(month == "05") %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=blue_risk,
              col=blue_risk
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
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

# ts plot: May-Sep risk to whales in study area

plot_subset <- risk_whales_WA_MaySep %>% 
  filter(study_area=='Y')  #restrict calculations to study area

ts_hump_risk_May_Sep_study_area <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk, na.rm=TRUE)
    ),
  aes(
    x = season, 
    y = Humpback_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Humpback Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
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
ts_hump_risk_May_Sep_study_area


ts_blue_risk_May_Sep_study_area <- ggplot(
  data = plot_subset %>% 
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk, na.rm=TRUE)
    ), 
  aes(
    x = season, 
    y = Blue_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  ylab("Blue Whale Risk\n(mean) May-Sep") + 
  xlab("Season") +
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
ts_blue_risk_May_Sep_study_area


# # plot blues and humps together and save
# png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(ts_hump_risk_May_Sep_study_area,
#           ts_blue_risk_May_Sep_study_area,
#           ncol=1,
#           nrow=2,
#           legend="top",
#           labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



risk_whales_WA_MaySep_by_seaon_month <- risk_whales_WA_MaySep %>% 
  group_by(season, month) %>%
  summarise(
    Humpback_risk_mean = mean(hump_risk, na.rm=TRUE),
    Blue_risk_mean = mean(blue_risk, na.rm=TRUE)
  )

test_ts <- ggplot(risk_whales_WA_MaySep_by_seaon_month, aes(x=as.factor(month), y=Blue_risk_mean, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  geom_point(size=2.5) + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean blue whale risk") +
  xlab("Month") + 
  scale_x_discrete("Month", labels = as.character(month), breaks = month)+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  #theme_classic()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
test_ts 

test_ts <- ggplot(risk_whales_WA_MaySep_by_seaon_month, aes(x=as.factor(month), y=Humpback_risk_mean, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  geom_point(size=2.5) + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean humpback whale risk") +
  xlab("Month") + 
  scale_x_discrete("Month", labels = as.character(month), breaks = month)+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
test_ts 
#######################################################################################




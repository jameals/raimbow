

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

# set some paths
# Jameal
path.grid.5km <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/Grid_5km_landerased.rds"
path.grid.depth <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/5x5 Grid/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

#Leena:
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"


# should be all outputs through july 2019 overlayed on 5km grid (i.e., not subset to DCRB fishing cells)
# Jameal
path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"

#Leena:
#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
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


# take a quick peek at 2019 to be sure nothing is weird
#View(x.blue %>% group_by(year_month) %>% summarise(
View(x.blue.all %>% group_by(year_month) %>% summarise(
  'Mean Occurrence' = mean(Blue_occurrence_mean, na.rm=TRUE),
  'Median Occurrence' = median(Blue_occurrence_mean,na.rm=TRUE),
  '75th Percentile' = quantile(Blue_occurrence_mean, probs=0.75, na.rm=TRUE),
  '25th Percentile' = quantile(Blue_occurrence_mean, probs=0.25, na.rm=TRUE),
  n=n()
)
)

# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)
###see further down for a mapping check on this

#----------------------------------------------------------------------------------------------------------------------

# note here is where you could insert some code to filter out whale predictions so that they only include 5km cells where DCRB fishing occurred previously


path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) #this works
### Get grid cells with non-NA values for all, and save that of fishing data
grid.studyarea.id_WA <- sort(unique(x.fish_WA$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
grid.5km.fish_WA <- grid.5km %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)
#filter whale data
x.hump_WA <-  x.hump_2009_2020 %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)
x.blue_WA <-  x.blue.all %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)

#----------------------------------------------------------------------------------------------------------------------


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area


# calculate median whale values for full time period
# note the hw data goes to Sep 2020 while bw data goes up to June 2021
x.whale.median <- x.whale %>%
  group_by(GRID5KM_ID, area_km_lno) %>%
  summarise(
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean, na.rm=TRUE)
  ) %>%
  left_join(grid.5km.lno)



# make maps based on whale outputs
## i did not convert humpback densities to abundance
## i did not normalize whale predictions

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
grid5km_bbox <- st_bbox(grid.5km.lno %>% 
                     st_as_sf()
)

map_hump <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median), 
          aes(fill=Humpback_dens_median,
              col=Humpback_dens_median
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2020 Median\nHumpback Whale Densities") +
  coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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

# png(paste0(path_maps, "/map_median_hump_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
# map_hump
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# plot median blue occurrence 

map_blue <- ggplot() + 
  geom_sf(data=sf::st_as_sf(x.whale.median), 
          aes(fill=Blue_occurrence_median,
              col=Blue_occurrence_median
          )
  ) +
  # facet_wrap(~time_period, nrow=1) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  scale_fill_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  scale_color_viridis(na.value=NA,option="D",name="Blue Whale\noccurrence") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  ggtitle("2009-2021 Median\nBlue Whale Occurrence") +
  coord_sf(xlim=c(grid5km_bbox[1],grid5km_bbox[3]),ylim=c(grid5km_bbox[2],grid5km_bbox[4])) + 
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

# png(paste0(path_maps, "/map_blue_by_time_period.png"), width = 14, height = 10, units = "in", res = 300)
# map_blue
# invisible(dev.off())
#ggsave(here::here('tradeoffs','map_hump_by_time_period.png'),map_hump,h=8,w=6)

# plot blues and humps together
png(paste0(path_figures, "/map_median_blue_hump_2009_2019.png"), width = 14, height = 10, units = "in", res = 300)
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





#--------------------------------------------------------------
# make time series based on whale outputs


#instead of working in calendar years, work in crab seasons
x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.whale_crab_season_v2 <- x.whale_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

#also, filter whale grid cells to be WA study area only
x.whale_crab_season_v2 <-  x.whale_crab_season_v2 %>% filter(GRID5KM_ID %in% grid.studyarea.id_WA)
x.whale_crab_season_v2 <- x.whale_crab_season_v2 %>% filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))

# plot annual mean humpback densities across all grid cells
ts_hump <- ggplot(
  #data = x.whale %>% 
    #or if want to work in crab_season rather than calendar year, read in the following instead
  data = x.whale_crab_season_v2 %>% 
    #mutate(
    #  year = as.numeric(substr(year_month, 1,4))
    #) %>%
    #group_by(year) %>%
    #or if want to work in crab_season rather than calendar year, skip the previous couple lines, and group by season instead
    group_by(season) %>%
    summarise(
      Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
    ), 
  aes(
    #x = year, 
    x = season, 
    y = Humpback_dens_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Density\n(mean)") + 
  #xlab("Year") +
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
ts_hump

# plot annual mean blue whale densities
ts_blue <- ggplot(
  #data = x.whale %>% 
  #or if want to work in crab_season rather than calendar year, read in the following instead
  data = x.whale_crab_season_v2 %>% 
    #mutate(
    #  year = as.numeric(substr(year_month, 1,4))
    #) %>%
    #group_by(year) %>%
    #or if want to work in crab_season rather than calendar year, skip the previous couple lines, and group by season instead
    group_by(season) %>%
    summarise(
      Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
    ), 
  aes(
    #x = year, 
    x = season,
    y = Blue_occurrence_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
                     #limits = c(2009.5,2019.5)) +
                     #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Occurrence\n(mean)") + 
  #xlab("Year") +
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
ts_blue

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_2009_2019_BY CRAB SEASON_only WA grids with fishing_across_all_season.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump,
          ts_blue,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#--------------------------------------------

#x.hump_WA -- mean hump dens by year_month

xlabels <- sort(unique(x.whale$year_month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ts_hump2 <- ggplot(
  data = x.whale %>% 
    group_by(year_month) %>%
    summarise(
      Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE)
    )
  , 
  aes(
    x = year_month, 
    y = Humpback_dens_mean
  )
) +
  geom_point(size=4) +
  geom_line(aes(group=1)) +
  scale_x_discrete(labels = xlabels) +
  ylab("Humpback Whale Density\n(mean)") + 
  xlab("Year_month") +
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
ts_hump2




ts_blue2 <- ggplot(
  data = x.whale %>% 
    group_by(year_month) %>%
    summarise(
      Blue_occurrence_mean = mean(Blue_occurrence_mean, na.rm=TRUE)
    ), 
  aes(
    x = year_month, 
    y = Blue_occurrence_mean
  )
) +
  geom_point(size=4) +
  geom_line(aes(group=1)) +
  scale_x_discrete(labels = xlabels) +
  ylab("Blue Whale Occurrence\n(mean)") + 
  xlab("Year_month") +
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
ts_blue2


#----------------------------------------------------

#--------------------------------------------------------------------------
#test simple risk calc
#logbook data: x.fish_WA 
#grids that had fishing in 2013-2020: grid.studyarea.id_WA 
#whale data filtered, only in those grids that had fishing 2013-2020
#x.hump_WA 
#x.blue_WA 


# get avg traps dens per grid cell for each yr month
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data
## step 1, make some columns that we can use
x.fish_WA3 <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  separate(season, into = c("yr_start", "yr_end"), sep = "-") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) #change month to two digit number
## step 2, grab yr_start for December effort and make a year_month column in a new df
df1 <- x.fish_WA3 %>%
  filter(month_name=='December')
df1 <- df1 %>%
  mutate(year_month = paste0(yr_start,"_",month))
## step 3, grab yr_end for non-December effort and make a year_month column in a new df
df2 <- x.fish_WA3 %>%
  filter(month_name !='December')
df2 <- df2 %>%
  mutate(year_month = paste0(yr_end,"_",month))
# squish the December and non-December df's together  
x.fish_WA4 <- rbind(df1,df2)


# join the whale and fishing data by year_month
joined_df_hump <- x.fish_WA4 %>%
  left_join(x.hump_WA,by=c("year_month","GRID5KM_ID"))

risk_hump <- joined_df_hump %>%
  mutate(
    hump_risk_M2 = Humpback_dens_mean * mean_M2_trapdens
  )

joined_df_blue <- x.fish_WA4 %>%
  left_join(x.blue_WA,by=c("year_month","GRID5KM_ID"))

risk_blue <- joined_df_blue %>%
  mutate(
    blue_risk_M2 = Blue_occurrence_mean * mean_M2_trapdens
  )



#instead of working in calendar years, work in crab seasons
risk_hump_crab_season <- risk_hump %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_hump_crab_season_v2 <- risk_hump_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

risk_blue_crab_season <- risk_blue %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_blue_crab_season_v2 <- risk_blue_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))



# plot annual mean humpback risk
ts_hump_risk <- ggplot(
  #data = x.whale %>% 
  #or if want to work in crab_season rather than calendar year, read in the following instead
  data = risk_hump_crab_season_v2 %>% 
    #mutate(
    #  year = as.numeric(substr(year_month, 1,4))
    #) %>%
    #group_by(year) %>%
    #or if want to work in crab_season rather than calendar year, skip the previous couple lines, and group by season instead
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk_M2, na.rm=TRUE)
    ), 
  aes(
    #x = year, 
    x = season, 
    y = Humpback_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Risk\n(mean)") + 
  #xlab("Year") +
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
ts_hump_risk


# plot annual mean blue whale risk
ts_blue_risk <- ggplot(
  #data = x.whale %>% 
  #or if want to work in crab_season rather than calendar year, read in the following instead
  data = risk_blue_crab_season_v2 %>% 
    #mutate(
    #  year = as.numeric(substr(year_month, 1,4))
    #) %>%
    #group_by(year) %>%
    #or if want to work in crab_season rather than calendar year, skip the previous couple lines, and group by season instead
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk_M2, na.rm=TRUE)
    ), 
  aes(
    #x = year, 
    x = season,
    y = Blue_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Risk\n(mean)") + 
  #xlab("Year") +
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
ts_blue_risk

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_RISK_2013_2020_BY CRAB SEASON_across_all_season.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk,
          ts_blue_risk,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020


#-------------------------------------------------------------------------------------

#if want to look at RISK at season_month level

risk_hump_crab_season_month <- risk_hump_crab_season_v2 %>% 
mutate(season_month = factor(paste0(season,"_",month_name))) %>%  
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    Humpback_risk_mean = mean(hump_risk_M2, na.rm=TRUE),
    Humpback_risk_median = median(hump_risk_M2, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  arrange(season, month)
glimpse(risk_hump_crab_season_month)

ordered.ids <- factor(risk_hump_crab_season_month$season_month, levels=risk_hump_crab_season_month$season_month)

# NOW DO SAME PLOTS BUT WITH SEASON_MONTH STEPS 
ts_hump_risk <- ggplot(
  data = risk_hump_crab_season_month, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = Humpback_risk_mean,
    #y = Humpback_risk_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Risk\n(mean)") + 
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
ts_hump_risk


# plot annual mean blue whale risk
risk_blue_crab_season_month <- risk_blue_crab_season_v2 %>% 
  mutate(season_month = factor(paste0(season,"_",month_name))) %>%  
  group_by(season_month) %>%
  summarise(
    Blue_risk_mean = mean(blue_risk_M2 , na.rm=TRUE),
    Blue_risk_median = median(blue_risk_M2 , na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  arrange(season, month)
glimpse(risk_blue_crab_season_month)

ordered.ids <- factor(risk_blue_crab_season_month$season_month, levels=risk_blue_crab_season_month$season_month)


ts_blue_risk <- ggplot(
  data = risk_blue_crab_season_month, 
  aes(
    x = factor(season_month, levels=ordered.ids),
    y = Blue_risk_mean,
   #y = Blue_risk_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Risk\n(mean)") + 
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
ts_blue_risk


# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_RISK_2013_2020_by crab season_month_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk,
          ts_blue_risk,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020


#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#if want to look at DENSITY/OCCURRENCE at season_month level


dens_hump_crab_season_month <- risk_hump_crab_season_v2 %>% 
  mutate(season_month = factor(paste0(season,"_",month_name))) %>%  
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE),
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  arrange(season, month)
glimpse(dens_hump_crab_season_month)

ordered.ids <- factor(dens_hump_crab_season_month$season_month, levels=dens_hump_crab_season_month$season_month)

# NOW DO SAME PLOTS BUT WITH SEASON_MONTH STEPS 
ts_hump_dens <- ggplot(
  data = dens_hump_crab_season_month, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = Humpback_dens_mean,
    #y = Humpback_dens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Density\n(mean)") + 
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


# plot monthly mean blue whale risk
occur_blue_crab_season_month <- risk_blue_crab_season_v2 %>% 
  mutate(season_month = factor(paste0(season,"_",month_name))) %>%  
  group_by(season_month) %>%
  summarise(
    Blue_occurrence_mean = mean(Blue_occurrence_mean , na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean , na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  arrange(season, month)
glimpse(occur_blue_crab_season_month)

ordered.ids <- factor(occur_blue_crab_season_month$season_month, levels=occur_blue_crab_season_month$season_month)


ts_blue_occur <- ggplot(
  data = occur_blue_crab_season_month, 
  aes(
    x = factor(season_month, levels=ordered.ids),
    y = Blue_occurrence_mean,
    #y = Blue_occurrence_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Occurrence\n(mean)") + 
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
ts_blue_occur


# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_occur_hump_dens_2013_2020_by crab season_month_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_dens,
          ts_blue_occur,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020



#-------------------------------------------------------------------------------------------

#plot mean trap density on a similar plot
# x.fish_WA


x.fish_WA_season_month <- x.fish_WA %>% 
  group_by(season_month) %>%
  #summarise across all grid cells in given season_month
  summarise(
    M2_trapdens_mean = mean(M2_trapdens, na.rm=TRUE),
    M2_trapdens_median = median(M2_trapdens, na.rm=TRUE)
  ) %>% 
  mutate(season_month2 = season_month) %>% 
  separate(season_month2, into = c("season", "month"), sep = "_") %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  arrange(season, month)
glimpse(x.fish_WA_season_month)

ordered.ids <- factor(x.fish_WA_season_month$season_month, levels=x.fish_WA_season_month$season_month)

# NOW DO SAME PLOTS BUT WITH SEASON_MONTH STEPS 
ts_trap_dens <- ggplot(
  data = x.fish_WA_season_month, 
  aes(
    x = factor(season_month, levels=ordered.ids), 
    y = M2_trapdens_mean,
    #y = M2_trapdens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("WA Trap  Density\n(mean)") + 
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


# plot blues and humps together
png(paste0(path_figures, "/ts_mean_trap_dens_2013_2020_by crab season_month_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_trap_dens,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# doing plots to show means for May-Sep period for each season
## I THINK THIS IS WRONG, SEE LATER FOR A DIFFERENT ATTEMPT THAT IS PROBABLY CORRECT
dens_hump_crab_season_MaySep <- risk_hump_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_month
  summarise(
    Humpback_dens_mean = mean(Humpback_dens_mean, na.rm=TRUE),
    Humpback_dens_median = median(Humpback_dens_mean, na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(dens_hump_crab_season_MaySep)

#ordered.ids <- factor(dens_hump_crab_season_MaySep$season, levels=dens_hump_crab_season_MaySep$season)


ts_hump_dens_MaySep <- ggplot(
  data = dens_hump_crab_season_MaySep, 
  aes(
    #x = factor(season_month, levels=ordered.ids), 
    x = season, 
    y = Humpback_dens_mean,
    #y = Humpback_dens_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Density\n(mean) May-Sep") + 
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
ts_hump_dens_MaySep



# plot May-Sep mean blue whale occurrence
occur_blue_crab_season_MaySep <- risk_blue_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_MaySep period
  summarise(
    Blue_occurrence_mean = mean(Blue_occurrence_mean , na.rm=TRUE),
    Blue_occurrence_median = median(Blue_occurrence_mean , na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(occur_blue_crab_season_MaySep)

#ordered.ids <- factor(occur_blue_crab_season_MaySep$season, levels=occur_blue_crab_season_MaySep)


ts_blue_occur_MaySep <- ggplot(
  data = occur_blue_crab_season_MaySep, 
  aes(
    x = season,
    y = Blue_occurrence_mean,
    #y = Blue_occurrence_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
  ylab("Blue Whale Occurrence\n(mean) May-Sep") + 
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
ts_blue_occur_MaySep



# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_occur_hump_dens_2013_2020_by crab season_MaySep only_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_dens_MaySep,
          ts_blue_occur_MaySep,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020



#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# doing plots to show mean RISK for May-Sep period for each season

risk_hump_crab_season_MaySep <- risk_hump_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_month
  summarise(
    hump_risk_M2_mean = mean(hump_risk_M2, na.rm=TRUE),
    hump_risk_M2_median = median(hump_risk_M2, na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(risk_hump_crab_season_MaySep)

#ordered.ids <- factor(dens_hump_crab_season_MaySep$season, levels=dens_hump_crab_season_MaySep$season)


ts_hump_risk_MaySep <- ggplot(
  data = risk_hump_crab_season_MaySep, 
  aes(
    #x = factor(season_month, levels=ordered.ids), 
    x = season, 
    y = hump_risk_M2_mean,
    #y = hump_risk_M2_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
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
ts_hump_risk_MaySep



# plot May-Sep mean blue whale risk
risk_blue_crab_season_MaySep <- risk_blue_crab_season_v2 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>%
  group_by(season) %>%
  #summarise across all grid cells in given season_MaySep period
  summarise(
    blue_risk_M2_mean = mean(blue_risk_M2 , na.rm=TRUE),
    blue_risk_M2_median = median(blue_risk_M2 , na.rm=TRUE)
  ) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)
glimpse(risk_blue_crab_season_MaySep)

#ordered.ids <- factor(occur_blue_crab_season_MaySep$season, levels=occur_blue_crab_season_MaySep)


ts_blue_risk_MaySep <- ggplot(
  data = risk_blue_crab_season_MaySep, 
  aes(
    x = season,
    y = blue_risk_M2_mean,
    #y = blue_risk_M2_median,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
  #scale_x_continuous(breaks = seq(2010, 2019, 1),
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #limits = c(2009.5,2019.5)) +
  #limits = c(2009.5,2020.5)) +
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
ts_blue_risk_MaySep



# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_risk_2013_2020_by crab season_MaySep only_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk_MaySep,
          ts_blue_risk_MaySep,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# I think the correct way of doing plots to show means for May-Sep period for each season
# is to not use the risk df, but the more original whale df filtered to fishing grids
# x.hump_WA and x.blue_WA --  otherwise there has already been various averaging etc...

x.hump_WA_crab_season <- x.hump_WA %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.hump_WA_crab_season_v2 <- x.hump_WA_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))

x.blue_WA_crab_season <- x.blue_WA %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.blue_WA_crab_season_v2 <- x.blue_WA_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))


#for some reason median() command wasn't working properly at one point...
x.hump.median <- x.hump_WA_crab_season_v2 %>% 
  filter(is_May_Sep == 'Y') %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  group_by(season) %>% 
  summarise(
    Hump_dens_mean = mean(Humpback_dens_mean),
    Hump_dens_median = median(Humpback_dens_mean, na.rm=TRUE)) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)

x.blue.median <- x.blue_WA_crab_season_v2 %>%
  filter(is_May_Sep == 'Y') %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  group_by(season) %>%
  summarise(
    Blue_dens_mean = mean(Blue_occurrence_mean, na.rm=TRUE),
    Blue_dens_median = median(Blue_occurrence_mean, na.rm=TRUE)) %>% 
  mutate(season = factor(season, levels = c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))) %>% 
  arrange(season)


ts_hump_dens_MaySep <- ggplot() +
  geom_point(data = x.hump.median, aes(x = season, y = Hump_dens_mean, group = 1), size=4) +
  geom_line(data = x.hump.median, aes(x = season, y = Hump_dens_mean, group = 1)) +
  geom_point(data = x.hump.median, aes(x = season, y = Hump_dens_median, group = 1), color = "darkred", size=4) +
  geom_line(data = x.hump.median, aes(x = season, y = Hump_dens_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Humpback Whale Density\n(mean and median) May-Sep") + 
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
ts_hump_dens_MaySep

ts_blue_occur_MaySep <- ggplot() +
  geom_point(data = x.blue.median, aes(x = season, y = Blue_dens_mean, group = 1), size=4) +
  geom_line(data = x.blue.median, aes(x = season, y = Blue_dens_mean, group = 1)) +
  geom_point(data = x.blue.median, aes(x = season, y = Blue_dens_median, group = 1), color = "darkred", size=4) +
  geom_line(data = x.blue.median, aes(x = season, y = Blue_dens_median, group = 1), color = "darkred", linetype="twodash") +
  #scale_x_continuous(breaks = seq(2010, 2021, 1),
  #                   limits = c(2009.5,2021.5)) +
  ylab("Blue Whale Density\n(mean) May-Sep") + 
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
ts_blue_occur_MaySep

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_and_median_blue_occur_hump_dens_2013_2020_by crab season_MaySep only_WA fishing grids only.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_dens_MaySep,
          ts_blue_occur_MaySep,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#these plots are filtered to be only in grid cells that had some fishing between 2013-2020








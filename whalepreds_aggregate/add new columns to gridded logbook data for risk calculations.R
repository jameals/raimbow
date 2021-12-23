# add new columns to gridded logbook data for risk calculations

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

# logbooks
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"

# grids
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

# whale data
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"




# load the data
grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)


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
  #%>% 
  #filter to be only up to 2020_09
  #filter(year_month %in% c('2019_08', '2019_09', '2019_010', '2019_11', '2019_12',
  #                         '2020_01', '2020_02', '2020_03', '2020_04', '2020_05', '2020_06', '2020_07', '2020_08', '2020_09'))

# bring in gridded logbook data (on 2-weekly step)
x.fish_WA <- readRDS(path.fish_WA) 



# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

#-----------------------------------------------------------------------------------


# get avg traps dens per grid cell for each yr month
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y) %>% #, AREA -- here should incl area in grouping
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


# join the whale and fishing data by year_month -- keeping those grids that are NA fishing
joined_df_whales <- x.whale %>%
  left_join(x.fish_WA4,by=c("year_month","GRID5KM_ID"))


risk_whales <- joined_df_whales %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  )


#instead of working in calendar years, work in crab seasons, and MaySep/DecApr column
risk_whales_crab_season <- risk_whales %>% 
  select(-month, -yr_start, -yr_end) %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>%   
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'MaySep', 'DecApr'))


# join with a grid to get lat and long info - refers to centroid of grid
risk_whales_crab_season_latlon <-  risk_whales_crab_season %>% 
  left_join(st_drop_geometry(grid.5km), by = "GRID5KM_ID") 


# assign state based on grid centroid - note that this is not exactly the best method
risk_whales_crab_season_latlon_state <- risk_whales_crab_season_latlon %>% 
  mutate(state = case_when(
    LATITUDE > 46.26  ~ 'WA',
    LATITUDE < 46.26 & LATITUDE > 42.00 ~ 'OR',
    LATITUDE < 42.00  ~ 'CA'
  ))

# create a column for 'study area'
risk_whales_crab_season_latlon_state_studyarea <-  risk_whales_crab_season_latlon_state %>% 
  mutate(study_area = ifelse(LONGITUDE>-125.03 & state == 'WA', 'Y', 'N'))
# find hw grids inside 'study area'
hw_grids_in_study_area <- risk_whales_crab_season_latlon_state_studyarea %>% 
  filter(study_area == 'Y') %>% 
  filter(!is.na(Humpback_dens_mean))
grid_studyarea <- sort(unique(hw_grids_in_study_area$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
#grid_studyarea_v2 <- grid.5km %>% filter(GRID5KM_ID %in% grid_studyarea)
risk_whales_crab_season_latlon_state_studyarea_v2 <- risk_whales_crab_season_latlon_state_studyarea %>% 
  mutate(study_area2 = GRID5KM_ID %in% grid_studyarea)
risk_whales_crab_season_latlon_state_studyarea_v3 <- risk_whales_crab_season_latlon_state_studyarea_v2 %>% 
  select(-study_area) %>% 
  rename(study_area = study_area2)


#specify 2013-2020
risk_whales_crab_season_latlon_state_studyarea_2013_2020 <- risk_whales_crab_season_latlon_state_studyarea_v3 %>% 
  mutate(is_2013_2020 = 
           ifelse(season %in% c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020')
                  ,'Y', 'N'))


#write_rds(risk_whales_crab_season_latlon_state_studyarea_2013_2020,here::here('wdfw','data',"whale_risk_df.rds"))


risk_whales_crab_season_latlon_state_studyarea_2013_2020_filtered <-  risk_whales_crab_season_latlon_state_studyarea_2013_2020 %>% 
  filter(is_2013_2020 == 'Y') %>% 
  filter(state == 'WA' | state == 'OR') %>% 
  #also remove 2020_10 and 2020_11 which occur in bw data
  mutate(remove = case_when(
    year == 2020 & month == 10  ~ 'remove',
    year == 2020 & month == 11  ~ 'remove'
  )) %>% 
  filter(is.na(remove)) %>% 
  filter(study_area == TRUE)
  
#write_csv(risk_whales_crab_season_latlon_state_studyarea_2013_2020_filtered,here::here('wdfw','data',"whale_risk_df_filt_2013_2020_WA_OR.csv"))


rows <- risk_whales_crab_season_latlon_state_studyarea_2013_2020_filtered %>% group_by(GRID5KM_ID) %>% summarise(n_rows = n())



bbox = c(-130,42,-120,49) 
 
   
   subset_MaySep <- risk_whales_crab_season_latlon_state_studyarea_2013_2020_filtered %>% 
     filter(season == "2019-2020") %>% 
     left_join(grid.5km, by = "GRID5KM_ID")

   
   map_hump_MaySep <- ggplot() + 
     geom_sf(data=sf::st_as_sf(subset_MaySep), 
                         aes(fill=GRID5KM_ID,
                                             col=GRID5KM_ID
                                         )
                 ) +
     geom_sf(data=rmap.base,col=NA,fill='gray50') +
     ggtitle("") +
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
 map_hump_MaySep









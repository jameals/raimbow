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

#################################################
#'study area' created in QGIS, to encompass all fished grids plus 'buffer' (grids that could be fished)
#read in 'study area' (grid)
study_area <- read_sf(here::here('wdfw','data', 'study_area.shp'))
glimpse(study_area)
plot(study_area)
################################################


#find out grids that were ever fished
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
x.fish_WA <- readRDS(path.fish_WA)
### Get grid cells with non-NA values for all, and save that of fishing data (grids ever fished)
grids_ever_fished_WA_waters <- sort(unique(x.fish_WA$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
grids_ever_fished_WA <- grid.5km %>% filter(GRID5KM_ID %in% grids_ever_fished_WA_waters)
glimpse(grids_ever_fished_WA)
plot(grids_ever_fished_WA)


#find out grids that were ever fished during May-Sep months
#subset data for May-Sep
subset_x.fish_WA_MaySep <- x.fish_WA %>% 
  mutate(is_May_Sep = 
         ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")
grids_ever_fished_WA_waters_MaySep <- sort(unique(subset_x.fish_WA_MaySep$GRID5KM_ID)) #find those unique grid cells that had data at some point in 2013-2020
grids_ever_fished_WA_MaySep <- grid.5km %>% filter(GRID5KM_ID %in% grids_ever_fished_WA_waters_MaySep)
glimpse(grids_ever_fished_WA_MaySep)
plot(grids_ever_fished_WA_MaySep)

#-----------------------------------------------------------------------------------
###############################################################################
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


#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#risk to whales in STUDY area during May-Sep

# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area
#####################################################################################

#whale data in study area

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID))
#filter whale data
x.whale_in_study_area <- x.whale %>% filter(GRID5KM_ID %in% study_area_grids_id)


###########################################################
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
#######################################################


# join the whale and fishing data by year_month (join into hw data, that way restricted to study area)
joined_df_wh <- x.whale_in_study_area  %>%
  left_join(x.fish_WA4,by=c("year_month","GRID5KM_ID"))

#calculate risk on a year_month and GRID level
risk_df <- joined_df_wh %>%
  mutate(
    hump_risk_M2 = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk_M2 = Blue_occurrence_mean * mean_M2_trapdens
  )



#instead of working in calendar years, work in crab seasons
risk_df_crab_season <- risk_df %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_df_crab_season_v2 <- risk_df_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(year_month = paste0(year,"-",month))

risk_df_crab_season_2014_2020_MaySep <- risk_df_crab_season_v2 %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  #I think it is somehow wrong to filter for May-Sep here
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")



summary_of_data <- risk_df_crab_season_2014_2020_MaySep %>% 
  group_by(season, is_May_Sep) %>% 
  






risk_df_crab_season_2014_2020_MaySep <- risk_df_crab_season_2014_2020_MaySep %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk_M2 = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk_M2),
         blue_risk_M2 = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk_M2)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk_M2 = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk_M2),
         blue_risk_M2 = 
           ifelse(is.na(Humpback_dens_mean), NA, blue_risk_M2)
  )



ts_hump_risk_May_Sep_study_area <- ggplot(
  data = risk_df_crab_season_2014_2020_MaySep %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk_M2, na.rm=TRUE)
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
ts_hump_risk_May_Sep_study_area


ts_blue_risk_May_Sep_study_area <- ggplot(
  data = risk_df_crab_season_2014_2020_MaySep %>% 
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk_M2, na.rm=TRUE)
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
ts_blue_risk_May_Sep_study_area

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk_May_Sep_study_area,
          ts_blue_risk_May_Sep_study_area,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#risk to whales in May-Sep, in grids that have EVER been fished

# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area


#whale data in grids that have ever been fished
grids_ever_fished_WA_waters <- sort(unique(x.fish_WA$GRID5KM_ID)) 
#filter whale data
x.whale_in_grids_ever_fished <- x.whale %>% filter(GRID5KM_ID %in% grids_ever_fished_WA_waters)



#fishing effort
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
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



# join the whale and fishing data by year_month (join into hw data, that way restricted to study area)
joined_df_wh <- x.whale_in_grids_ever_fished  %>%
  left_join(x.fish_WA4,by=c("year_month","GRID5KM_ID"))

#calculate risk on a year_month and GRID level
risk_df <- joined_df_wh %>%
  mutate(
    hump_risk_M2 = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk_M2 = Blue_occurrence_mean * mean_M2_trapdens
  )



#instead of working in calendar years, work in crab seasons
risk_df_crab_season <- risk_df %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_df_crab_season_v2 <- risk_df_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  mutate(year_month = paste0(year,"-",month))

risk_df_crab_season_2014_2020_MaySep <- risk_df_crab_season_v2 %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")


risk_df_crab_season_2014_2020_MaySep <- risk_df_crab_season_2014_2020_MaySep %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk_M2 = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk_M2),
         blue_risk_M2 = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk_M2)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk_M2 = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk_M2),
         blue_risk_M2 = 
           ifelse(is.na(Humpback_dens_mean), NA, blue_risk_M2)
  )



#WHY DO THE PLOTS LOOK EXACTLY THE SAME AS WITH STUDY AREA?? ISSUE WITH NAs??
ts_hump_risk_May_Sep_grids_ever_fished <- ggplot(
  data = risk_df_crab_season_2014_2020_MaySep %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk_M2, na.rm=TRUE)
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
ts_hump_risk_May_Sep_grids_ever_fished


ts_blue_risk_May_Sep_grids_ever_fished <- ggplot(
  data = risk_df_crab_season_2014_2020_MaySep %>% 
    group_by(season) %>%
    summarise(
      Blue_risk_mean = mean(blue_risk_M2, na.rm=TRUE)
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
ts_blue_risk_May_Sep_grids_ever_fished

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_grids_ever_fished_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk_May_Sep_grids_ever_fished,
          ts_blue_risk_May_Sep_grids_ever_fished,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())
#-----------------------------------------------------------------------------------

######################################################################################
#maybe need to first calc all stuff for all whale grids and then clip to study area??
#script add new columns... line 88 onwards


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
#here all grids still exist


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
#here all grids still exist

risk_whales_crab_season_2014_2020 <- risk_whales_crab_season %>%  
  filter(season %in% c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'))
#here all grids still exist


risk_whales_crab_season_2014_2020_v2 <- risk_whales_crab_season_2014_2020 %>% 
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
           ifelse(is.na(Humpback_dens_mean), NA, blue_risk)
  )
#here all grids still exist


#so loss of all study area grids happens somewhere here
#then clip to study area
study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) #this maps correctly

#these lines don't do the job correctly
# risk_whales_crab_season_2014_2020_WA <-  risk_whales_crab_season_2014_2020 %>% filter(GRID5KM_ID %in% study_area_grids_id) #with this lose some of the edge grids that don't have whale data
# 
# risk_whales_crab_season_2014_2020_WA <-  risk_whales_crab_season_2014_2020 %>% 
#   mutate(study_area = ifelse(GRID5KM_ID %in% study_area_grids_id, 'Y', 'N'))
# 
# 
# test <- risk_whales_crab_season_2014_2020_WA %>% filter(study_area=='Y') %>%  group_by(GRID5KM_ID) %>% summarise(n_rows=n())




############################################
study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) #this maps correctly

#these lines do the job correctly
study_area_df <- as.data.frame(study_area_grids_id) %>% 
  rename(GRID5KM_ID = study_area_grids_id) #%>% 
#mutate(study_area = 'Y')

#this should be the correct stuff for 'study area'
#risk_whales_crab_season_2014_2020_WA <- full_join(study_area_df, risk_whales_crab_season_2014_2020_v2, by=c("GRID5KM_ID"))
#only works on month level, not in May-Sep, -- the study area gridding needs to have all season-month combos
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
study_area_df_with_all_season_month_combos <- crossing(study_area_df, season_month_combos)
#and add to that the column to denote study area
study_area_df_with_all_season_month_combos <-  study_area_df_with_all_season_month_combos %>% 
  mutate(study_area = 'Y')

x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
x.whale_crab_season_v2 <- x.whale_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))
x.whale_crab_season_v2_May_Sep <-  x.whale_crab_season_v2 %>% 
  filter(month %in% c('05', '06', '07', '08', '09'))

test_xx <- full_join(study_area_df_with_all_season_month_combos, x.whale_crab_season_v2_May_Sep, by=c("GRID5KM_ID", "season", "month"))

x.fish_WA4_v2 <- x.fish_WA4%>% 
  select(-month, -yr_start, -yr_end) %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>%   
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'MaySep', 'DecApr')) %>% 
  filter(is_May_Sep=='MaySep')

test_xx_v2 <- left_join(test_xx, x.fish_WA4_v2, by=c("GRID5KM_ID", "season", "month")) 

test_xx_risk_whales <- test_xx_v2 %>%
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
           ifelse(is.na(Humpback_dens_mean), NA, blue_risk)
  ) %>% 
  select(-is_May_Sep) %>% 
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))



# risk_whales_crab_season_2014_2020_WA_xx <- full_join(study_area_df_with_all_season_month_combos, risk_whales_crab_season_2014_2020_v2, by=c("GRID5KM_ID", "season", "month"))
# risk_whales_crab_season_2014_2020_WA_xx_May_Sep <-  risk_whales_crab_season_2014_2020_WA_xx %>% 
#   filter(is_May_Sep=='MaySep') #the May-Sep filter causes loss of study area grids.
# 
# 
# risk_whales_crab_season_2014_2020_v2_May_Sep <-  risk_whales_crab_season_2014_2020_v2 %>% 
#   filter(is_May_Sep=='MaySep')
# 
# #the study area df doesn't have is_May_Sep info
# risk_whales_crab_season_2014_2020_WA_May_Sep <- full_join(study_area_df_with_all_season_month_combos, risk_whales_crab_season_2014_2020_v2_May_Sep, by=c("GRID5KM_ID", "season", "month"))
# 
# risk_whales_crab_season_2014_2020_WA_May_Sep_v2 <- risk_whales_crab_season_2014_2020_WA_May_Sep %>% 
#   #if there is no fishing data in grid, then risk is 0, as there is no fishing
#   mutate(hump_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
#          blue_risk = 
#            ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
#   ) %>%
#   #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
#   mutate(hump_risk = 
#            ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
#          blue_risk = 
#            ifelse(is.na(Humpback_dens_mean), NA, blue_risk)
#   ) 


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


subset_data <- plot_subset %>% 
  #filter(study_area=='Y') %>% 
  #filter(is_May_Sep=='MaySep') %>% #the May-Sep filter causes loss of some study area grids
  
  #select season to map 
  #filter(year_month=='2020_01') %>% 
  filter(season == "2017-2018") %>% 
  filter(month == "06") %>% 
  #filter(is_May_Sep == 'MaySep') %>% 
  left_join(grid.5km, by = "GRID5KM_ID")

map_test <- ggplot() + 
  geom_sf(data=sf::st_as_sf(subset_data), 
          aes(fill=study_area,
              col=study_area
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  #scale_fill_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
  #scale_color_viridis(na.value=NA,option="D",name="Humpback Whale\nDensity") + # ,breaks=seq(0,1,by=0.25),limits=c(0,1)
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



#this causes a loss of som eof the study area grids
plot_subset <- test_xx_risk_whales %>% 
  filter(study_area=='Y') %>% 
  filter(is_May_Sep=='Y') #it is the May-Sep filter that causes loss of some study area grids



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
ts_blue_risk_May_Sep_study_area

# plot blues and humps together
png(paste0(path_figures, "/ts_mean_blue_hump_risk_2014_2020_in_study_area_by crab season_MaySep.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(ts_hump_risk_May_Sep_study_area,
          ts_blue_risk_May_Sep_study_area,
          ncol=1,
          nrow=2,
          legend="top",
          labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


#######################################################################################






#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


#Filter hw model data to study area
study_area_grids_id <- sort(unique(study_area$GRID5KM_ID))
x.hump_WA <-  x.hump_2009_2020 %>% filter(GRID5KM_ID %in% study_area_grids_id)
x.hump_WA <- study_area %>% 
  left_join(x.hump_2009_2020,by=c("GRID5KM_ID"))
#WHY DOES THE NUMBER OF ROWS CHANGE? ALL GRIDS SHOULD HAVE EQUAL AMOUNT OF ENTRIES??
x.hump_WA <- full_join(study_area, x.hump_2009_2020, by=c("GRID5KM_ID"))
#test <- x.hump_WA %>% st_set_geometry(NULL) %>% group_by(GRID5KM_ID) %>% summarise(n_rows=n())




#fishing effort
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
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


# join the whale and fishing data by year_month (join into hw data, that way restricted to study area)
joined_df_hump <- x.hump_WA  %>%
  left_join(x.fish_WA4,by=c("year_month","GRID5KM_ID"))

risk_hump <- joined_df_hump %>%
  mutate(
    hump_risk_M2 = Humpback_dens_mean * mean_M2_trapdens
  )

#instead of working in calendar years, work in crab seasons
risk_hump_crab_season <- risk_hump %>% 
  separate(year_month, into = c("year", "month"), sep = "_")  
risk_hump_crab_season_v2 <- risk_hump_crab_season %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))

risk_hump_crab_season_v3 <- risk_hump_crab_season_v2 %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y")

risk_hump_crab_season_v4 <- risk_hump_crab_season_v3 %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk_M2 = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk_M2)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk_M2 = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk_M2)
  )


ts_hump_risk <- ggplot(
  data = risk_hump_crab_season_v4 %>% 
    group_by(season) %>%
    summarise(
      Humpback_risk_mean = mean(hump_risk_M2, na.rm=TRUE)
    ), 
  aes(
    x = season, 
    y = Humpback_risk_mean,
    group = 1
  )
) +
  geom_point(size=4) +
  geom_line() +
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















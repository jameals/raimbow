#testing a glm on risk

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


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#fishing effort

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"

x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA <- x.fish_WA %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y) %>% #remove AREA as grouping factor here
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #create column for summer season (May-Sep) in case want to filter to that at some point
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))

x.fish_WA_MaySep$month_name <- factor(x.fish_WA_MaySep$month_name, 
                                  levels = c('December', 'January', 'February', 'March', 'April', 'May', 
                                             'June', 'July', 'August', 'September'))


#-----------------------------------------------------------------------------------


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



#-----------------------------------------------------------------------------------


#join whale data to fishing data
fishing_whale_joined <- left_join(x.fish_WA_MaySep, x.whale_crab_season, by=c("GRID5KM_ID", "season", "month")) %>% 
  select(-season_start, -season_end, -area_km_lno)

#calculate risk  metric
risk_fishing_whale_joined <- fishing_whale_joined %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) 

#add column to denote pre-post regulation
risk_fishing_whale_joined <- risk_fishing_whale_joined %>% 
  mutate(
  pre_post_reg = case_when(
    season == '2018-2019' & month %in% c('07', '08', '09') ~ "xpost-reg",  #and 'x' to name so that pre-reg comes first
    season == '2019-2020' & month %in% c('05', '06', '07', '08', '09') ~ "xpost-reg")) %>% 
  mutate(pre_post_reg = ifelse(is.na(pre_post_reg), 'pre-reg', pre_post_reg))
    

risk_fishing_whale_joined$month <- as.numeric(risk_fishing_whale_joined$month)

#-----------------------------------------------------------------------------------

#glm


#choosing family for glm:
#https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
#"If you are dealing with continuous non-negative outcome, then you could consider the Gamma distribution, 
#or Inverse Gaussian distribution."

#https://stats.stackexchange.com/questions/67547/when-to-use-gamma-glms
#"Given skewed positive data I will often find myself trying gamma and lognormal models 
#(in GLM context log link, normal or Gaussian family) and choosing which works better."


#probably want to focus on summer season only

risk_fishing_whale_joined_MaySep <- risk_fishing_whale_joined %>% 
  filter(is_May_Sep=='Y') 

hist(risk_fishing_whale_joined_MaySep$hump_risk)


m1_hump <- glm(hump_risk ~ month_name + season + pre_post_reg,
               family=Gamma, data=risk_fishing_whale_joined_MaySep, na.action = na.omit)
summary(m1_hump)




risk_fishing_whale_joined_MaySep_sum_month <- risk_fishing_whale_joined_MaySep %>% 
  group_by(season, month_name, pre_post_reg) %>% 
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))

hist(risk_fishing_whale_joined_MaySep_sum_month$sum_hump_risk)


m2_hump <- glm(sum_hump_risk ~ month_name + season + pre_post_reg,
               family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2_hump)







hist(risk_fishing_whale_joined_MaySep$blue_risk)

m1_blue <- glm(blue_risk ~ month_name + season + pre_post_reg,
               family=Gamma, data=risk_fishing_whale_joined_MaySep)
summary(m1_blue)
  

hist(risk_fishing_whale_joined_MaySep_sum_month$sum_blue_risk)

m2_blue <- glm(sum_blue_risk ~ month_name + season + pre_post_reg,
               family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2_blue)

#################################################################################




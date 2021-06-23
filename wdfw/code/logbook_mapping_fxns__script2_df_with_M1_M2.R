## Mapping functions for WDFW logbook data 
# creating df with both adjustment methods (M1 and M2) for double counting of traps

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(viridis)
library(magrittr)
library(gridExtra)
library(nngeo)


#-------------------------------------------------------------

# Start with traps_g df (traps are simulated and joined to grid, script 1)  
# RDS can be found in Kiteworks folder
traps_g <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))


# create columns for season, month etc
traps_g %<>%
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
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


# Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g %<>%
  left_join(WA_pot_limit_info,by=c("License")) %>% 
  drop_na(Pot_Limit) #2 NAs for cases with no license info unless correct it with drop_na(Pot_Limit)


# apply weighting based on permitted max pot number (this is Method 2 or M2)
adj_traps_g <- traps_g %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count up traps for vessel in 2-week period
  group_by(season_month_interval, Vessel, License, Pot_Limit) %>%  
  summarise(
    M2_n_traps_vessel=n(), na.rm=TRUE 
  ) %>% 
  # create a column with weighting - proportion of max allowed traps
  # divide pot limit by number of simulated traps
  # because you want to up-weight traps < pot_limit, and downweight traps > pot_limit
  mutate(trap_limit_weight = Pot_Limit/M2_n_traps_vessel) %>% 
  ungroup()

# join the "weighting key" back to the simulated pots data
traps_g %<>%
  left_join(adj_traps_g,by=c('season_month_interval','Vessel','License','Pot_Limit'))

# do Method 1/M1 calculations/adjustment, group by season_month_interval 
M1_summtraps <- traps_g %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    M1_ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    M1_ntraps_vessel_cell=mean(M1_ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month_interval, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    M1_tottraps=sum(M1_ntraps_vessel_cell),
  # add count of unique vessels for confidentiality check, this could be done here or in mapping phase
    nvessels=n_distinct(Vessel,na.rm=T) 
  ) %>% 
  # trap density (in sq. km) is total traps divided by area (which is in sq. m) of each cell
  mutate(
    M1_trapdens=M1_tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(M1_tottraps))
glimpse(M1_summtraps)


# Now sum pots for M2 (just like for M1), use 'trap_limit_weight' to sum, instead of n().
traps_summ <- traps_g %>% 
  group_by(season_month_interval,GRID5KM_ID,NGDC_GRID,grd_x,grd_y, AREA) %>%  
  # this is the new/key step -- weighted_traps is the M2 version of 'tottraps' column of M1 method
  summarise(M2_tottraps=sum(trap_limit_weight)) %>%  
  mutate(
    M2_trapdens=M2_tottraps/(AREA/1e6)
  ) %>% ungroup() %>% 
  filter(!is.na(M2_tottraps))
glimpse(traps_summ) 


# join results from M1 and M2 
adj_summtraps <- left_join(M1_summtraps,traps_summ, by=c("season_month_interval", "GRID5KM_ID", "grd_x", "grd_y", "AREA"))
glimpse(adj_summtraps) 


# fivekm_grid_polys_shore_lamb.shp shapefile: 5km grid cells that fall partially within any of the bays or estuaries 
# will have a separate polygon within said bay or estuary that will have the same Grid5km_ID value as the adjacent 
# portion of the 5km grid cell that does not fall within the bay or estuary. 
# Therefore, if you want to calculate total area of each grid cell that falls in water, 
# sum the total area for that grid cell by its Grid5km_ID value.

# joining data for portions of grids with same grid ID
adj_summtraps %<>%
  group_by(season_month_interval,GRID5KM_ID, grd_x,grd_y) %>% #remove NGDC_GRID as a grouping factor
  summarise(
    AREA = sum(AREA),
    M1_tottraps = sum(M1_tottraps),
    nvessels = sum(nvessels),
    M1_trapdens = M1_tottraps/(AREA/1e6),
    M2_tottraps = sum(M2_tottraps),
    M2_trapdens = M2_tottraps/(AREA/1e6)
  )
glimpse(adj_summtraps)


adj_summtraps %<>%
  separate(season_month_interval, into = c("season", "month_name", "period"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) %>% 
  mutate(season_month_interval = paste0(season_month,"_",period)) %>% 
  mutate(month_interval = paste0(month_name,"_",period)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2')))
glimpse(adj_summtraps)

#write_rds(adj_summtraps,here::here('wdfw','data',"adj_summtraps.rds"))


#----------------------------------------------------------------------------

# Few visuals comapring the M1 and M2 methods
pairs(~ M1_trapdens + M2_trapdens, data = adj_summtraps)
pairs(~ M1_tottraps + M2_tottraps, data = adj_summtraps)

library(ggplot2)                    
library(GGally)

ggpairs(adj_summtraps[, c(10, 12)])
ggpairs(adj_summtraps, columns = c(10, 12), ggplot2::aes(colour=season))

ggpairs(adj_summtraps[, c(8, 11)])
ggpairs(adj_summtraps, columns = c(8, 11), ggplot2::aes(colour=season))

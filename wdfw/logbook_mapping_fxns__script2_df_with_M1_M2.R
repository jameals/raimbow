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

################################################################


#link here to script with the original functions?


################################################################

#Start with traps_g df (traps are simulated and joined to grid)  
#RDS can be found in Kiteworks folder
traps_g_license_logs_2013_2019 <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))
traps_g <- traps_g_license_logs_2013_2019

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


#Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

#join Pot_Limit to traps_g -- this will take ~2min to run
traps_g %<>%
  left_join(WA_pot_limit_info,by=c("License")) %>% 
  drop_na(Pot_Limit)


#apply weighting based on permitted max pot number (this is Method 2 or M2)
adj_traps_g <- traps_g %>% 
  #st_set_geometry(NULL) %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count up traps for vessel in 2-week period
  group_by(season_month_interval, Vessel, License, Pot_Limit) %>%  
  summarise(
    M2_n_traps_vessel=n(), na.rm=TRUE 
  ) %>% 
  # create a column with weighting - proportion of max allowed traps
  # divide pot limit by number of traps
  # because you want to up-weight traps < pot_limit, and downweight traps < pot_limit
  mutate(trap_limit_weight = Pot_Limit/M2_n_traps_vessel) %>% #trap_limit_weight ends up having 2 NAs for cases with no license info unless correct it with drop_na(Pot_Limit)
  ungroup()

# join the "weighting key" back to the simulated pots data
traps_g %<>%
  left_join(adj_traps_g,by=c('season_month_interval','Vessel','License','Pot_Limit'))

# do Method 1/M1 calculations/adjustment, group by season_month_interval 
M1_summtraps <- traps_g %>% 
  #st_set_geometry(NULL) %>%
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
    nvessels=n_distinct(Vessel,na.rm=T) #add count of unique vessels for confidentiality check, this could be done here or in mapping phase
  ) %>% 
  # trap density is total traps divided by area (in sq. km) of each cell
  mutate(
    M1_trapdens=M1_tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(M1_tottraps))
glimpse(M1_summtraps)


# Now sum pots for M2 (just like for M1), use 'trap_limit_weight' to sum, instead of n().
traps_summ <- traps_g %>% 
  #st_set_geometry(NULL) %>% 
  group_by(season_month_interval,GRID5KM_ID,NGDC_GRID,grd_x,grd_y, AREA) %>%  # or whatever grouping variables are applicable
  summarise(weighted_traps=sum(trap_limit_weight)) %>%  # this is the new/key step -- weighted_traps is the M2 version of 'tottraps' column of M1 method
  mutate(
    M2_trapdens=weighted_traps/(AREA/1e6)
  ) %>% ungroup() %>% 
  filter(!is.na(weighted_traps))
glimpse(traps_summ) #no NAs for weighted_traps or M2_trapdens here


#join results from M1 and M2 
adj_summtraps <- left_join(M1_summtraps,traps_summ, by=c("season_month_interval", "GRID5KM_ID", "grd_x", "grd_y", "AREA"))
glimpse(adj_summtraps) #20 NAs for weighted_traps and M2_trapdens here, also NAs for NGDC_GRID, no NA's once used drop_na(Pot_Limit) earlier

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

#CAN WE NOW USE THIS DF FOR EVERYTHING? e.g., for making maps (while still also having some type of loop/function to pump out lots of maps?)
#and also for time series plots etc...?
#but can't use this df for plotting depth distribution of pots, as this df is already on grid scale
#and depth dist pots require pots to be indiv data points




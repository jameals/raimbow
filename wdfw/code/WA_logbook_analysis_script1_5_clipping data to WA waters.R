# clip logbook data to WA waters
# clipping not fully accurate in R, so export shapefile and clip in 

library(tidyverse)
library(sf)
library(viridis)
library(cowplot)
library(here)
library(rnaturalearth)
library(fasterize)
library(sp)
library(magrittr)
library(raster)
select <- dplyr::select
library(scales)
library(lubridate)
library(gridExtra)
library(nngeo)
library(scales)

#------------------------------------------------------------------------------

# bring in logs in point format - one for WA landed WA logs, and one for OR landed WA logs (Q999999)

traps_g_WA_landed_WA_logs_raw <- read_rds(here::here('wdfw', 'data','traps_g_WA_logs_2014_2020_20220119.rds')) %>% 
  mutate(Landing_logbook_state = 'WA')

# create columns for season, month etc
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs_raw %>% 
  #st_set_geometry(NULL) %>% 
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


traps_g_OR_landed_WA_logs_Q999999_raw <- read_rds(here::here('wdfw', 'data', 'traps_g_WA_Q999999_logs_2014_2020_20220119.rds')) %>% 
  mutate(Landing_logbook_state = 'OR')

# create columns for season, month etc
traps_g_OR_landed_WA_logs_Q999999 <- traps_g_OR_landed_WA_logs_Q999999_raw %>% 
  #st_set_geometry(NULL) %>% 
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


#------------------------------------------------------------------------------


#label but don't filter out May-Sep

traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

traps_g_OR_landed_WA_logs_Q999999 <- traps_g_OR_landed_WA_logs_Q999999 %>% 
  mutate(is_May_Sep = 
           ifelse(month_name  %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) 

#create another SetID column just to be sure that same SetID wouldn't appear in both datasets
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))

traps_g_OR_landed_WA_logs_Q999999 <- traps_g_OR_landed_WA_logs_Q999999 %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))

#------------------------------------------------------------------------------

#export as shapefile
st_write(traps_g_OR_landed_WA_logs_Q999999, "traps_g_OR_landed_WA_logs_Q999999_2014_2020_20220119.shp") 

#the WA landed logs are too big to export as one shapefile. 
#split and export one season at a time
traps_g_WA_landed_WA_logs_2019_2020 <- traps_g_WA_landed_WA_logs %>% 
  filter(season == '2019-2020') 
st_write(traps_g_WA_landed_WA_logs_2019_2020, "traps_g_WA_landed_WA_logs_2019_2020_20220119.shp") #MaySep labelled but not filtered out



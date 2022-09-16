##logbook prepping

#getting DCRB fishing effort data ready for sdmTMB work


#------------------------------------------------------------------------------
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

#Both WA and OR landed logs have been run through logbook processing, to the point that
#each pot is a point (line in df), and they have been linked to 5km grid

#do we need to split the WA landed and OR landed logbooks based on whether the pot
#occurred in WA or OR waters, and link to that states license? Will we be able to link to
#other States license, will the vessel number be the same?

#------------------------------------------------------------------------------

# bring in logs in point format - one for WA landed logs (Q999999 are removed),
#and one for OR landed logs (no similar case to the Q999999 situation)

#WA - use the data run of 2009/10 to 2019/20 season done for Blake's wind energy work
traps_g_WA_landed_WA_logs_raw <- read_rds(here::here('wdfw', 'data','traps_g_WA_logs_2010_2020_20220906.rds')) %>% 
  mutate(Landing_logbook_state = 'WA')
#note that this is an sf object
#st_crs(traps_g_WA_landed_WA_logs_raw) #CA_Curr_Lamb_Azi_Equal_Area 

# create columns for season, month etc
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs_raw %>% 
  #st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)
  ) %>% 
  #also rename license column to denote that info came from WA
  rename(WA_License = License)

glimpse(traps_g_WA_landed_WA_logs)


# Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  left_join(WA_pot_limit_info,by=c("WA_License" = "License")) #%>% 
  #drop_na(Pot_Limit) 

#there are some instances of missing License info 
#traps_g_WA_landed_WA_logs %>% st_set_geometry(NULL) %>%  filter(is.na(License)) %>% select(Vessel) %>% distinct()
#vessel RAVEN DANCER, or NA for vessel name

#there are also instances for NA in Pot_Limit, either because License was NA,
#or License number was present, but there was no link to the WA_pot_limit_info

summary_missing_license_info <- traps_g_WA_landed_WA_logs %>% 
  st_set_geometry(NULL) %>% 
  filter(is.na(Pot_Limit)) %>% 
  group_by(season) %>% 
  summarise(no_vessels_with_missing_License = n_distinct(Vessel))
#season    no_vessels_with_missing_License
# 2009-2010                             123 --> 100% of vessels
# 2010-2011                             137 --> 100% of vessels
# 2011-2012                             144 --> 100% of vessels
# 2015-2016                               1
# 2016-2017                               1
# --> we have a lot of missing pot limit info for the early seasons
#the License number for all is a small value
#When did WA introduce the 300 and 500 licecnse categories? --> 2000

# View(traps_g_WA_landed_WA_logs %>% 
#        st_set_geometry(NULL) %>% 
#        filter(is.na(Pot_Limit)) %>% 
#        filter(season %in% c('2009-2010', '2010-2011', '2011-2012')) %>% 
#        distinct(Vessel,License))



#-----

#OR - use the data run of 2001/08 to 2017/18 data
traps_g_OR_landed_OR_logs_raw <- read_rds(here::here('wdfw', 'data','OR','OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered_20220915.rds')) %>% 
  mutate(Landing_logbook_state = 'OR')
#note that this is an sf object
#st_crs(traps_g_OR_landed_OR_logs_raw) #CA_Curr_Lamb_Azi_Equal_Area 

# create columns for season, month etc
traps_g_OR_landed_OR_logs <- traps_g_OR_landed_OR_logs_raw %>% 
  #st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)
  ) %>% 
  #also rename license column to denote that info came from WA
  rename(OR_License = PermitNumber)
#note that there are some cases of NAs for OR_License, but no NAs in PotLimit info, only PotLimit needed if weighting pots

glimpse(traps_g_OR_landed_OR_logs)




#-------------------------
##NEED TO EDIT THIS TO MATCH THE ABOVE DF NAMES ETC

#create another SetID column just to be sure that same SetID wouldn't appear in both datasets
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))

traps_g_OR_landed_OR_logs <- traps_g_OR_landed_OR_logs %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))


#------------------------------------------------------------------------------

#export as shapefile
#the logs are too big to export as one shapefile. 
#split and export one season at a time
traps_g_WA_landed_WA_logs_2019_2020 <- traps_g_WA_landed_WA_logs %>% 
  filter(season == '2019-2020') 
st_write(traps_g_WA_landed_WA_logs_2019_2020, "traps_g_WA_logs_2019_2020_20220915.shp") 






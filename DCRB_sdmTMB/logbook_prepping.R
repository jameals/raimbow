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
library(stringr)


#------------------------------------------------------------------------------

#Both WA and OR landed logs have been run through logbook processing, to the point that
#each pot is a point (row in df), and they have been linked to 5km grid (column dentoes 5km grid ID)

#next we split the WA landed and OR landed logbooks based on whether the pot
#occurred in WA or OR waters, and link to that states license and pot limit info 
#howver the matching to other state's license data is imperfect

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


# Read in and join WA license & pot limit info
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
#the License number for all is a small value (as in  1, 2 or 3-digit value)
#whereas all license numbers in WA_pot_limit_info_May2021.csv are 5-digit values
#got updated logbooks from WDFW for the early years, will find actual license info from that later on

# View(traps_g_WA_landed_WA_logs %>% 
#        st_set_geometry(NULL) %>% 
#        filter(is.na(Pot_Limit)) %>% 
#        filter(season %in% c('2009-2010', '2010-2011', '2011-2012')) %>% 
#        distinct(Vessel,License))



#-----

#OR - use the data run of 2007/08 to 2017/18 data
#traps_g_OR_landed_OR_logs_raw <- read_rds(here::here('wdfw', 'data','OR','OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered_20220915.rds')) %>% 
#  mutate(Landing_logbook_state = 'OR')
#note that this is an sf object
#st_crs(traps_g_OR_landed_OR_logs_raw) #CA_Curr_Lamb_Azi_Equal_Area 

#OR - ran data to include 2018/19 and 2019/20 seasons as well
#comparison of overlapping years with summary() indicates that it matches with the previous version
traps_g_OR_landed_OR_logs_raw <- read_rds(here::here('wdfw', 'data','OR','OR_traps_g_all_logs_2007_2020_SpatialFlag_filtered_20221027.rds')) %>% 
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
  #also rename license column to denote that info came from OR
  rename(OR_License = PermitNumber)
#note that there are some cases of NAs for OR_License, but no NAs in PotLimit info, only PotLimit needed if weighting pots
glimpse(traps_g_OR_landed_OR_logs)




#-------------------------

#create another SetID column just to be sure that same SetID wouldn't appear in both datasets
traps_g_WA_landed_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))

traps_g_OR_landed_OR_logs <- traps_g_OR_landed_OR_logs %>% 
  mutate(SetID2 = paste0(Landing_logbook_state,"_",SetID))


#-------------------------
#-------------------------
#quick check on the number of grid cells
grids_WA_logs <- traps_g_WA_landed_WA_logs %>% 
  st_set_geometry(NULL) %>% 
  distinct(GRID5KM_ID)

grids_OR_logs <- traps_g_OR_landed_OR_logs %>% 
  st_set_geometry(NULL) %>% 
  distinct(GRID5KM_ID)

grids_WA_OR_logs <- rbind(grids_WA_logs, grids_OR_logs) %>% 
  distinct(GRID5KM_ID)
nrow(grids_WA_OR_logs) #1243

#------------------------------------------------------------------------------

#export as shapefiles
#the logs for both states are too big to export as one shapefile. 
#split and export one season per state at a time
#traps_g_OR_landed_OR_logs_2019_2020 <- traps_g_OR_landed_OR_logs %>% 
#  filter(season == '2019-2020') 
#st_write(traps_g_OR_landed_OR_logs_2019_2020, "traps_g_OR_logs_2019_2020_20221028.shp") 


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
### work in QGIS to clip data correctly to WA and OR waters
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#read in clipped data, see if can match license info across states and logs
#back up option might be to try to link via fishtickets -- i.e. first link to pacfin tix,
#from that get vessel info, and then join to license info?? might not work for WA as need license number....?



# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data', 'fivekm_grid_polys_shore_lamb.shp'))
names(grd)



############### WA #################
###### all this has been run once, now just read in the final product ######
# # read in version of traps_g, point data, that is clipped to OR waters - WA logs landed in OR waters
# # reading this shapefile in takes a long time
# # traps_g_WA_logs_in_OR_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','WA logs shapefiles' ,'traps_g_WA_logs_2010_2020_20220915_clipped to OR waters_updated.shp')) %>% 
# #   st_transform(st_crs(grd)) %>%  #make it have same projection as the grid
# #    dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
# #              point_y = sf::st_coordinates(.)[,2])
# #x and y are point locations, northing and easting
# #crs was CA_Curr_Lamb_Azi_Equal_Area
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_WA_logs_in_OR_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_2010_2020_20220915_clipped to OR waters_updated.rds"))
# traps_g_WA_logs_in_OR_waters_2010_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_WA_logs_2010_2020_20220915_clipped to OR waters_updated.rds')) %>% 
#   select(-path, -layer) %>%  #columns that have been added in QGIS step, when joining files
#   mutate(Pot_State = 'OR') %>%  #Pot_State = in what state did the pot occur, according to logbook
# #updated file is multipoint, but should be point
#   st_cast("POINT")
# 
# # read in WA logs in WA waters
# # reading this shapefile in takes a long time
# # traps_g_WA_logs_in_WA_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','WA logs shapefiles' ,'traps_g_WA_logs_2010_2020_20220915_clipped to WA waters_updated.shp')) %>% 
# #   st_transform(st_crs(grd))  #make it have same projection as the grid
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_WA_logs_in_WA_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_2010_2020_20220915_clipped to WA waters_updated.rds"))
# traps_g_WA_logs_in_WA_waters_2010_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_WA_logs_2010_2020_20220915_clipped to WA waters_updated.rds')) %>% 
#   select(-path, -layer) %>% #columns that have been added in QGIS step, when joining files 
#   #Issue when trying to separate coordinates as file was multipoint
#   st_cast("POINT") %>% 
#   #x and y are point locations, northing and easting
#   #crs was CA_Curr_Lamb_Azi_Equal_Area
#   dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
#                 point_y = sf::st_coordinates(.)[,2]) %>% 
#   mutate(Pot_State = 'WA') #Pot_State = in what state did the pot occur, according to logbook
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_WA_logs_in_WA_waters_2010_2020raw,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_in_WA_waters_2010_2020raw.rds"))
# 
# traps_g_WA_logs_in_WA_waters_2010_2020raw_no_geometry <- traps_g_WA_logs_in_WA_waters_2010_2020raw %>% st_set_geometry(NULL)
# traps_g_WA_logs_in_OR_waters_2010_2020raw_no_geometry <- traps_g_WA_logs_in_OR_waters_2010_2020raw %>% st_set_geometry(NULL)
# traps_g_WA_logs_all <- rbind(traps_g_WA_logs_in_WA_waters_2010_2020raw_no_geometry,traps_g_WA_logs_in_OR_waters_2010_2020raw_no_geometry)
# # write_rds(traps_g_WA_logs_all,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_ALL_2010_2020.rds"))


traps_g_WA_logs_ALL_2010_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_WA_logs_ALL_2010_2020.rds')) 
#at this point one row is one pot

traps_g_WA_logs_ALL_2010_2020 <- traps_g_WA_logs_ALL_2010_2020raw %>% 
  select(-NGDC_GR,-is_pr__) %>% 
  #rename some columns, plus those that got shortened by QGIS
  rename(
    WA_License = WA_Lcns,
    PotsFished = PtsFshd,
    line_length_m = ln_lng_,
    GRID5KM_ID = GRID5KM,
    Landing_logbook_state = Lndng__,
    month_name = mnth_nm,
    season_month = ssn_mnt,
    WA_Pot_Limit = Pot_Lmt
  ) #%>% 
  #add column
  #mutate( OR_License = NA)  #as these are WA landed data, and pots were in WA waters, there is no OR license info that is relevant


############### Fix first 3 years of Washington licenses ##############
#split data

WA_license_OK <- traps_g_WA_logs_ALL_2010_2020 %>% 
  filter(!season %in% c('2009-2010', '2010-2011', '2011-2012'))

WA_license_NOT_OK <- traps_g_WA_logs_ALL_2010_2020 %>% 
  filter(season %in% c('2009-2010', '2010-2011', '2011-2012'))



#bring in raw logbooks, and using SetID, join in info t=from 'Fishticket1' column
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>%
  select(SetID, FishTicket1) %>%
  distinct()
#each set ID matches to 1 FIshticket1

#join Fisticket1 into data
WA_license_NOT_OK_fishticket1 <- WA_license_NOT_OK %>%
  left_join(raw_WA_logs_selected_columns, by="SetID")
#length(unique(WA_license_NOT_OK$SetID)) #41255
#WA_license_NOT_OK_fishticket1 %>% filter(is.na(FishTicket1)) %>% select(SetID) %>% n_distinct() #215
# 215/41255*100 = 0.5% of cases didn't have Fishticket1 column filled to be able to join
#note that if these are cases that are in OR waters, WA license won't even matter
# 
# #read in file from WDFW that has real license info for first 3 seasons
# WA_license_fix_data <- read_csv(here('wdfw', 'data','2009-2012 logbook data_fixed_license.csv'),col_types = 'cdcccccddddddddddddddd')
# WA_license_fix_data_selected_columns <- WA_license_fix_data %>% 
#   select(License, `Fish Ticket 1`) %>% 
#   filter(`Fish Ticket 1` != 'Q999999') %>% 
#   rename(FishTicket1 = `Fish Ticket 1`) %>% 
#   distinct()
#there are some (27) fishticket1 values that link to multiple vessel license
#manually went through these and created a lookup table with unique combinations of FishticketID and License
#read in manually created look up table

lookup_table <- read_csv(here('DCRB_sdmTMB', 'data','lookup_table_for_Fishticket1_and_correct_wa_license_early_seasons.csv'),col_types = 'cdd')

#join correct WA license info to data, for early seasons
WA_license_NOT_OK_fixed <- WA_license_NOT_OK_fishticket1 %>% 
  left_join(lookup_table, by="FishTicket1")
#all instances where FIshticket1 existed found a correct WA license

#bring in WA license and pot limit table
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

# join Pot_Limit to WA_license_NOT_OK_fixed 
WA_license_NOT_OK_fixed_pot_limit <- WA_license_NOT_OK_fixed %>% 
  left_join(WA_pot_limit_info,by="License") 
WA_license_NOT_OK_fixed_pot_limit$WA_License <- WA_license_NOT_OK_fixed_pot_limit$License
WA_license_NOT_OK_fixed_pot_limit$WA_Pot_Limit <- WA_license_NOT_OK_fixed_pot_limit$Pot_Limit
WA_license_NOT_OK_fixed_pot_limit <- WA_license_NOT_OK_fixed_pot_limit %>% 
  select(-FishTicket1, -License, -Pot_Limit, -n_distinct_License)

##join WA data back together

traps_g_WA_logs_ALL_2010_2020 <- rbind( WA_license_NOT_OK_fixed_pot_limit, WA_license_OK)
#each row is a pot/point. about 99.9% of stringlines have a correct WA license number and pot limit





############### OR #######################
###### all this has been run once, now just read in the final product ######
# # read in version of traps_g, point data, that is clipped to WA waters - OR logs landed in WA waters
# # reading this shapefile in takes a long time
# # traps_g_OR_logs_in_WA_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','OR logs shapefiles' ,'traps_g_OR_logs_2008_2020_20221028_clipped to WA waters_updated.shp')) %>% 
# #   st_transform(st_crs(grd)) %>%  #make it have same projection as the grid
# #    dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
# #              point_y = sf::st_coordinates(.)[,2])
# #x and y are point locations, northing and easting
# #crs was CA_Curr_Lamb_Azi_Equal_Area
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_OR_logs_in_WA_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_2008_2020_202201028_clipped to WA waters_updated.rds"))
# traps_g_OR_logs_in_WA_waters_2008_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_OR_logs_2008_2020_202201028_clipped to WA waters_updated.rds')) %>% 
#   select(-path, -layer) %>%  #columns that have been added in QGIS step, when joining files
#   mutate(Pot_State = 'WA') %>%  #Pot_State = in what state did the pot occur, according to logbook
# #updated file is multipoint, but should be point
#   st_cast("POINT")


# # read in OR logs in OR waters
# # reading this shapefile in takes a long time
# # traps_g_OR_logs_in_OR_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','OR logs shapefiles' ,'traps_g_OR_logs_2008_2020_20221028_clipped to OR waters_noCA_updated.shp')) %>% 
# #   st_transform(st_crs(grd))  #make it have same projection as the grid
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_OR_logs_in_OR_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_2008_2020_20221028_clipped to OR waters_updated.rds"))
# traps_g_OR_logs_in_OR_waters_2008_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_OR_logs_2008_2020_20221028_clipped to OR waters_updated.rds')) %>% 
#   select(-path, -layer) %>% #columns that have been added in QGIS step, when joining files 
#   #Issue when trying to separate coordinates as file was multipoint
#   st_cast("POINT") %>% 
#   #x and y are point locations, northing and easting
#   #crs was CA_Curr_Lamb_Azi_Equal_Area
#   dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
#                 point_y = sf::st_coordinates(.)[,2]) %>% 
#   mutate(Pot_State = 'OR') #Pot_State = in what state did the pot occur, according to logbook
# # #save a rds version and read that in in the future:
# # write_rds(traps_g_OR_logs_in_OR_waters_2008_2020raw,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_in_OR_waters_2008_2020raw.rds"))
# 
# traps_g_OR_logs_in_OR_waters_2008_2020raw_no_geometry <- traps_g_OR_logs_in_OR_waters_2008_2020raw %>% st_set_geometry(NULL)
# traps_g_OR_logs_in_WA_waters_20108_2020raw_no_geometry <- traps_g_OR_logs_in_WA_waters_2008_2020raw %>% st_set_geometry(NULL)
# traps_g_OR_logs_all <- rbind(traps_g_OR_logs_in_OR_waters_2008_2020raw_no_geometry, traps_g_OR_logs_in_WA_waters_20108_2020raw_no_geometry)
# # write_rds(traps_g_OR_logs_all,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_ALL_2008_2020.rds"))


traps_g_OR_logs_ALL_2008_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_OR_logs_ALL_2008_2020.rds')) 
#at this point one row is one pot


traps_g_OR_logs_ALL_2008_2020 <- traps_g_OR_logs_ALL_2008_2020raw %>% 
  select(-NGDC_GR,-is_pr__) %>% 
  #rename some columns, plus those that got shortened by QGIS
  rename(
    OR_License = OR_Lcns,
    PotsFished = PtsFshd,
    line_length_m = ln_lng_,
    GRID5KM_ID = GRID5KM,
    Landing_logbook_state = Lndng__,
    month_name = mnth_nm,
    season_month = ssn_mnt,
    OR_Pot_Limit = Potlimt
  ) 


#####################################################################################################
#####################################################################################################
##next bring in license files and see if can match across states

#work on WA logbook df: traps_g_WA_logs_ALL_2010_2020
#split out pots in OR waters
traps_g_WA_logs_ALL_2010_2020_WA_waters <- traps_g_WA_logs_ALL_2010_2020 %>% 
  filter(Pot_State == 'WA')
traps_g_WA_logs_ALL_2010_2020_OR_waters <- traps_g_WA_logs_ALL_2010_2020 %>% 
  filter(Pot_State == 'OR')


#if we bring back federal ID to WA logs, that should match docnum in OR license data --> not every time
#especially the early seasons have the wrong vessel and license, and Federal ID info, WDFW sent a new file to fix this

#bring back federal ID to WA logs
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files. Keep FIshticket1 as some WA early years need to be matched to new file from WDFW
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(SetID, FederalID, FishTicket1) %>% 
  distinct()
#each SetID has one FederalID

#note that Federal ID in raw logs has a gap between the first 3 and last 3 digits
raw_WA_logs_selected_columns <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 

traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID <- traps_g_WA_logs_ALL_2010_2020_OR_waters %>% 
  left_join(raw_WA_logs_selected_columns, by="SetID")
#all SetIDs have a Federal ID
#correct number of rows in df
#but for the 3 early seasons the FederalID may be wrong (only a 3 digit number)



#read in file from WDFW that has real license info for first 3 seasons
WA_license_fix_data <- read_csv(here('wdfw', 'data','2009-2012 logbook data_fixed_license.csv'),col_types = 'cdcccccddddddddddddddd')
WA_license_fix_data_selected_columns <- WA_license_fix_data %>% 
  select(License,`Federal ID`) %>% #`Fish Ticket 1`
  rename(FederalID_v2 = `Federal ID`) %>% #,  FishTicket1 = `Fish Ticket 1`
  distinct()
#sometimes License is linked to multiple FederalID
WA_license_fix_data_selected_columns$License <- as.numeric(WA_license_fix_data_selected_columns$License)

#note that Federal ID in raw logs has a gap between the first 3 and last 3 digits
WA_license_fix_data_selected_columns <-as.data.frame(apply(WA_license_fix_data_selected_columns,2, str_remove_all, " ")) 



#want to use FederalID_v2 for only those that have a FederalID of 3 characters or less
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID$FederalID_length = str_length(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID$FederalID)

#only those with short Federal ID need to be fixed
#nrow(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID) #1,027,858
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_OK <- traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID %>% filter(FederalID_length > 4)
#nrow(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_OK) #905135
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK <- traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID %>% filter(FederalID_length < 4)
#nrow(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK) #122723 --> all from first 3 seasons


#if just try to join with WA_license_fix_data_selected_columns, somehow data/rows get repeated, the length of the df changes 
#Because of cases where fishticket is NA (if joining via FIshticket1), also not all Fishticket IDs are unique, and they are linked to different vessels
#and if working with 'License' and Federal ID - those combinations aren't always unique either
#again easier to just manually create a lookup table

summary_test <- WA_license_fix_data_selected_columns %>% group_by(FederalID_v2) %>% summarise(n_distinct_License = n_distinct(License))
summary_test_v2 <- WA_license_fix_data_selected_columns %>% group_by(FederalID_v2,License) %>% summarise(n_row = n())

lookup_table_federalID <- read_csv(here('DCRB_sdmTMB', 'data','lookup_table_for_FederalID_and_correct_wa_license_early_seasons.csv'),col_types = 'ddc')


# join Pot_Limit to WA_license_NOT_OK_fixed 
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed <- traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK %>% 
  left_join(lookup_table_federalID,by=c("WA_License" = "License"))
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed$FederalID <- traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed$FederalID_v2
traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed <- traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed %>% 
  select(-FederalID_v2, -n_distinct_FederalID)

#glimpse(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_OK)
#glimpse(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed)

traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed <- rbind(traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_notOK_fixed, traps_g_WA_logs_ALL_2010_2020_OR_waters_FederalID_OK)



#unique vessels in WA logs that had pots in OR waters
unique_vessels <- as.list(unique(traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed$FederalID)) #96




# Read in and join license & pot limit info
OR_pot_limit_info_raw <- read_csv(here::here('wdfw', 'data', 'OR', 'OregonCrabPermitData2007-2020.csv'))

OR_pot_limit_info <- OR_pot_limit_info_raw %>% 
  rename(Vessel = Docnum,
         PermitNumber = Number)

OR_pot_limit_info %<>%
  mutate(Begindate=as.Date(Begindate,"%d/%m/%Y"),#these are different in the 2007-2019 license data file
         Enddate=as.Date(Enddate,"%d/%m/%Y"))


OR_pot_limit_info %>% distinct(Potlimit) # 500, 300, 200
# OR permits - a permit can change from vessel to vessel sometimes 
# but does the pot limit for a given permit number stay the same?
test <- OR_pot_limit_info %>%                              
  group_by(PermitNumber) %>%
  summarise(count = n_distinct(Potlimit))
# Yes, except for 2 instances: Permit Numbers 96125 and 96262 have 2 unique pot limit values
cases <- OR_pot_limit_info %>% 
  filter(PermitNumber == 96125 | PermitNumber == 96262)
# 96125: for 12 years pot limit is 300, but in 2014 it is 500 - assume mistake for now
# 96262: for 12 years pot limit is 300, but in 2008 it is 200 - assume mistake for now, also possibly outside years of interest anyway
OR_pot_limit_info %<>%
  mutate(Potlimit = ifelse(PermitNumber == 96125 | PermitNumber == 96262, 300, Potlimit))


OR_pot_limit_info_v2 <- OR_pot_limit_info %>% 
  select(PermitNumber, Vessel, Begindate, Enddate, Potlimit) %>% 
  filter(Vessel %in% unique_vessels) 


library(fuzzyjoin)

#2009-2010 season - 10 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20092010 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2009-2010')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20092010_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20092010, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

#not all find a match
length(unique(subset_traps_g_WA_logs_in_OR_waters_20092010_joined$Vessel.x)) #28 unique vessels in 2009/10 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20092010_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #6 = 21% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20092010_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20092010_joined_license.csv"))



#2010-2011 season - 11 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20102011 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2010-2011')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20102011_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20102011, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

#not all find a match
length(unique(subset_traps_g_WA_logs_in_OR_waters_20102011_joined$Vessel.x)) #24 unique vessels in 2010/11 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20102011_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #6 = 25% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20102011_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20102011_joined_license.csv"))



#2011-2012 season - 11 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20112012 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2011-2012')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20112012_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20112012, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

#not all find a match
length(unique(subset_traps_g_WA_logs_in_OR_waters_20112012_joined$Vessel.x)) #27 unique vessels in 2011/12 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20112012_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #9 = 33% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20112012_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20112012_joined_license.csv"))



#2012-2013 season - 12 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20122013 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2012-2013')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20122013_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20122013, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

#not all find a match
length(unique(subset_traps_g_WA_logs_in_OR_waters_20122013_joined$Vessel.x)) #21 unique vessels in 2012/13 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20122013_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #4 = 19% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20122013_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20122013_joined_license.csv"))



#2013-2014 season -  20 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20132014 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2013-2014')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20132014_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20132014, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20132014_joined$Vessel.x)) #23 unique vessels in 2013/14 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20132014_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #4 = 17% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20132014_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20132014_joined_license.csv"))



#2014-2015 season - 22 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20142015 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2014-2015')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20142015_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20142015, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20142015_joined$Vessel.x)) #23 unique vessels in 2014/15 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20142015_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #0 = 0% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20142015_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20142015_joined_license.csv"))



#2015-2016 season - 21 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20152016 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2015-2016')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20152016_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20152016, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20152016_joined$Vessel.x)) #29 unique vessels in 2015/16 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20152016_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #1 = 3% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20152016_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20152016_joined_license.csv"))



#2016-2017 season - 30 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20162017 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2016-2017')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20162017_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20162017, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20162017_joined$Vessel.x)) #37 unique vessels in 2016/17 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20162017_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #3 = 8% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20162017_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20162017_joined_license.csv"))



#2017-2018 season - 19 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20172018 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2017-2018')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20172018_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20172018, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20172018_joined$Vessel.x)) #33 unique vessels in 2017/18 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20172018_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #2 = 6% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20172018_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20172018_joined_license.csv"))


#2018-2019 season - 25 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20182019 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2018-2019')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20182019_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20182019, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20182019_joined$Vessel.x)) #29 unique vessels in 2018/19 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20182019_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #2 = 7% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20182019_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20182019_joined_license.csv"))



### we currently don't have OR license data for 2019-2020 season (to match to WA logs that cover 2019-20)
#2019-2020 season - 24 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20192020 <- traps_g_WA_logs_ALL_2010_2020_OR_waters_fixed %>% 
  filter(season=='2019-2020')
tm <- proc.time()
subset_traps_g_WA_logs_in_OR_waters_20192020_joined <- fuzzy_left_join(
  subset_traps_g_WA_logs_in_OR_waters_20192020, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(subset_traps_g_WA_logs_in_OR_waters_20192020_joined$Vessel.x)) #31 unique vessels in 2019/20 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20192020_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #2 = 6% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20192020_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20192020_joined_license.csv"))



#summary of WA landed logs that were from OR waters
#that didn't match to OR license and pot limit info
#season       n_no_match  n_unique_vessel prop_no_match                 n_unique_vessels_in_all_WA_landed_logs (WA license cap is 223)
# 2009-2010       6             28            21%
# 2010-2011       6             24            25%
# 2011-2012       9             27            33%
# 2012-2013       4             21            19%                                        140   
# 2013-2014       4             23            17%                                        157   
# 2014-2015       0             23            0%                                         159   
# 2015-2016       1             29            3%                                         155   
# 2016-2017       3             37            8%                                         162       
# 2017-2018       2             33            6%                                         152   
# 2018-2019       2             29            7%                                         158         
# 2019-2020       2             31            6%                                         138       



traps_g_WA_logs_in_OR_waters_all_joined <- list.files(path = "C:/Users/lrie0/Documents/Projects/raimbow/DCRB_sdmTMB/data/WA logs in OR waters joined to OR license info", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv,col_types = 'cccddcdddddccccdcddcccdccccd') %>% 
  # Combine data sets into one
  bind_rows                                                        
#glimpse(traps_g_WA_logs_in_OR_waters_all_joined)

traps_g_WA_logs_in_OR_waters_all_joined_v2 <- traps_g_WA_logs_in_OR_waters_all_joined %>% 
  select( -FederalID, -FishTicket1, -FederalID_length, -Vessel.y, -Begindate, -Enddate) %>% 
  rename(Vessel = Vessel.x, OR_PermitNumber = PermitNumber, OR_Potlimit = Potlimit)

#--> then join back with WA logs in WA waters
glimpse(traps_g_WA_logs_ALL_2010_2020_WA_waters)
traps_g_WA_logs_ALL_2010_2020_WA_waters_v2 <- traps_g_WA_logs_ALL_2010_2020_WA_waters %>% 
  mutate(OR_PermitNumber = 'NA',
         OR_Potlimit = 'NA')

###ALL WA LOGS, WHETHER IN WA OR OREGON WATERS - THOSE IN OR WATERS BUT NO OR POT LIMIT ARE STILL IN THE DF
traps_g_WA_logs_ALL_2010_2020_fixed <- rbind(traps_g_WA_logs_ALL_2010_2020_WA_waters_v2, traps_g_WA_logs_in_OR_waters_all_joined_v2)
# # write_rds(traps_g_WA_logs_ALL_2010_2020_fixed,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_ALL_2010_2020_fixed.rds"))
# note that those pots in OR waters without OR license have not yet been deleter

#how many pots get deleted if remove pots in OR waters if no OR license?
removed_pots <- traps_g_WA_logs_ALL_2010_2020_fixed %>% 
  filter(Pot_State == 'OR' & is.na(OR_Potlimit))
summary <- removed_pots %>% group_by(season) %>% summarise(n_pots =)




#######################################################################################
#-----------------------
#OR logs: traps_g_OR_logs_ALL_2008_2020


#WA license data only has license number, not vessel name.
#the vessel name in OR logs in WA waters, might match docnum in OR license, which matches Federal ID in WA logs, 
#from logs and Federal ID we can get WA license, from which can get WA Pot Limit
#but this won't always match

#split out pots in WA waters
traps_g_OR_logs_ALL_2008_2020_WA_waters <- traps_g_OR_logs_ALL_2008_2020 %>% 
  filter(Pot_State == 'WA')
traps_g_OR_logs_ALL_2008_2020_OR_waters <- traps_g_OR_logs_ALL_2008_2020 %>% 
  filter(Pot_State == 'OR')


#unique vessels in OR logs that had pots in WA waters
unique_vessels_OR_logs_in_WA_waters <- traps_g_OR_logs_ALL_2008_2020_WA_waters %>% 
  select(Vessel) %>% 
  distinct()
#55

##but note here the early WA seasons in the original file have wrong info 
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(License, FederalID, FishTicket1) %>% #use FishTicket1 to fix wrong Federal IDs
  distinct() %>% 
  rename(WA_License = License)

#select from new WDFW file FIshticket1, Federal ID etc for the first 3 seasons, and join to the above from original logs





#FederalID column in WA logs has a space between the first 3 and last 3 digits, while pacfin ticket don't have this gap
raw_WA_logs_selected_columns_new <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 
#then can match Federal ID from WA logs to OR logs
unique_vessels_OR_logs_in_WA_waters_joined_WA_license <- unique_vessels_OR_logs_in_WA_waters %>% 
  left_join(raw_WA_logs_selected_columns_new,by=c("Vessel" = "FederalID")) 

length(unique(unique_vessels_OR_logs_in_WA_waters_joined_WA_license$Vessel)) #45
#but sometimes OR vessel name, therefore OR docnum, so WA Federal ID is linked to different WA license numbers in different years
#some of the lo numbers in Federal ID might be fixed with new data for the first few years in WA

#will just manually create a df using OR Vessel, and if multiple WA licenses, use the ones that actually exists
#e.g. vessel 528154 is linked (through WA logs) to WA license 160, 410, 59 and 59935, but only 59935 exists in WA pot limit - license data
#vessel 615728 has two proper WA licenses: 59945 and 58039, both were 500 pots
#vessel 695550 has two proper WA licenses: 58106 and 58119, both were 500 pots
#vessel 1198334 has two proper WA licenses: 60180 and 58209, both were 500 pots
#vessel OR908AEZ has three proper WA licenses: 60180, 59945 and 58169, all were 500 pots

Vessel <- c("528154", "591368", "612155", "OR921ABG", "544609", "555388", "610349", "615728", "695550", "537773", "590537", 
            "519132", "941807", "1198334", "1230071", "240319", "261974", "694038", "1075750", "594790", "OR908AEZ", "522674", "697860", 
            "1193066", "512179", "515580", "557686")

WA_License <- c("59935", "59974", "60000", "58048", "58078", "58176", "58038", "58039", "58119", "58089", "61522", 
                "58174", "58181", "58209", "58051", "58208", "58099", "59994", "58167", "58110", "59945", "58198", "58183", 
                "58094", "58115", "58039", "58036")

lookup_table_OR_vessel_WA_license <- data.frame(Vessel, WA_License) #27 cases where OR vessel name linked to >1 WA license

OR_logs_in_WA_waters_joined_WA_license <- traps_g_OR_logs_in_WA_waters_2008_2018 %>% 
  left_join(lookup_table_OR_vessel_WA_license,by=c("Vessel" = "Vessel")) 
(unique(OR_logs_in_WA_waters_joined_WA_license$Vessel)) #45 vessels

test_df_NA <- OR_logs_in_WA_waters_joined_WA_license %>% filter(is.na(WA_License))
length(unique(test_df_NA$Vessel)) #18 --> 40% of vessels of OR logs in WA waters don't find a WA license number (to provide WA PotLimit)
summary_by_season <- test_df_NA %>% 
  distinct(Vessel,season) %>% 
  group_by(season) %>% 
  summarise(n_row = n())

unique_vessels <- OR_logs_in_WA_waters_joined_WA_license %>% 
  distinct(Vessel,season) %>% 
  group_by(season) %>% 
  summarise(n_row_all = n())

summary_by_season <- summary_by_season %>% 
  left_join(unique_vessels, by="season") %>% 
  mutate(prop_no_match = n_row/n_row_all)

#season      n_row  n_row_all     prop_no_match
# 2007-2008     1         3         0.333 33%
# 2008-2009     3         5         0.6   60%
# 2009-2010     4        11         0.364 36%
# 2010-2011     7        11         0.636 64%
# 2011-2012     1         5         0.2   20%
# 2012-2013               5         0     0% --> 100% match
# 2013-2014     2         8         0.25  25%
# 2014-2015     1         7         0.143 14%
# 2015-2016     1        10         0.1   10%
# 2016-2017     3        12         0.25  25%
# 2017-2018     3        11         0.273 27%

#these are cases where no matching license, now join with WDFW license-potlimit table





WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)
WA_pot_limit_info$License <- as.character(WA_pot_limit_info$License)

# join Pot_Limit to traps_g 
OR_logs_in_WA_waters_joined_WA_license_WA_PotLim <- OR_logs_in_WA_waters_joined_WA_license %>% 
  left_join(WA_pot_limit_info,by=c("WA_License" = "License")) %>% 
  rename(WA_Pot_Limit = Pot_Limit)

#the summary will still be the same as above 


#---------------------------------------------------------------------------------------------------

#WA landed and logged pots that were in OR water
traps_g_WA_logs_in_OR_waters_all_joined



#OR landed and logged pots that were in WA water
OR_logs_in_WA_waters_joined_WA_license



#read in WA logs in WA waters
#read in OR logs in OR waters

# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
#grd <- read_sf(here::here('wdfw','data', 'fivekm_grid_polys_shore_lamb.shp'))
#names(grd)



#read in OR logs in OR waters
# traps_g_OR_logs_in_OR_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','OR logs shapefiles' ,'traps_g_OR_logs_2008_2018_20220915_clipped to OR waters_noCA.shp')) %>% 
#   st_transform(st_crs(grd)) #make it have same projection as the grid
# #save a rds version and read that in in the future:
# write_rds(traps_g_OR_logs_in_OR_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_2008_2018_20220915_clipped to OR waters.rds"))
traps_g_OR_logs_in_OR_waters_2008_2018raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_OR_logs_2008_2018_20220915_clipped to OR waters.rds')) %>% 
  select(-path, -layer) %>% #columns that have been added in QGIS step, when joining files
  #x and y are point locations, northing and easting
  #crs was CA_Curr_Lamb_Azi_Equal_Area
  dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
                point_y = sf::st_coordinates(.)[,2]) 
#these are OR logs (pots) in OR waters, i.e. exclude any pots that were in CA waters but landed in OR


traps_g_OR_logs_in_OR_waters_2008_2018 <- traps_g_OR_logs_in_OR_waters_2008_2018raw %>% 
  st_set_geometry(NULL) %>% #remove geometry as it is slowing everything down
  #drop couple useless columns
  select(-NGDC_GR,-is_pr__,-SptlFlg) %>% #remove spatial flag column, doesn't exist in WA data, already filtered
  #rename some columns, plus those that got shortened by QGIS
  rename(
    PotsFished = PtsFshd,
    OR_License = OR_Lcns,
    OR_Pot_Limit = Potlimt,
    line_length_m = ln_lng_,
    GRID5KM_ID = GRID5KM,
    Landing_logbook_state = Lndng__,
    month_name = mnth_nm,
    season_month = ssn_mnt
  ) %>% 
  #add column
  mutate(Pot_State = 'OR', #Pot_State = in what state did the pot occur, according to logbook
         WA_License = NA)  #as these are OR landed data, and pots were in OR waters, there is no WA license info that is relevant



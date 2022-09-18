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

#export as shapefile
#the logs are too big to export as one shapefile. 
#split and export one season at a time
traps_g_OR_landed_OR_logs_2017_2018 <- traps_g_OR_landed_OR_logs %>% 
  filter(season == '2017-2018') 
st_write(traps_g_OR_landed_OR_logs_2017_2018, "traps_g_OR_logs_2017_2018_20220915.shp") 


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
### work in QGIS to clip data correctly to WA and OR waters
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#read in clipped data, see if can match license info across states and logs
#back up option might be to try to link via fishtickets -- first link to pacfin tix,
#from that get vessel info, and then join to license info?? might not work for WA as need license number....?



# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
grd <- read_sf(here::here('wdfw','data', 'fivekm_grid_polys_shore_lamb.shp'))
names(grd)


# read in version of traps_g, point data, that is clipped to OR waters - WA logs landed in OR waters
# reading this shapefile in takes a long time
# traps_g_WA_logs_in_OR_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','WA logs shapefiles' ,'traps_g_WA_logs_2010_2020_20220915_clipped to OR waters.shp')) %>% 
#   st_transform(st_crs(grd)) #make it have same projection as the grid
# #save a rds version and read that in in the future:
# write_rds(traps_g_WA_logs_in_OR_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_2010_2020_20220915_clipped to OR waters.rds"))
traps_g_WA_logs_in_OR_waters_2010_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_WA_logs_2010_2020_20220915_clipped to OR waters.rds')) %>% 
  select(-path, -layer) #columns that have been added in QGIS step, when joining files

# traps_g_OR_logs_in_WA_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','OR logs shapefiles' ,'traps_g_OR_logs_2008_2018_20220915_clipped to WA waters.shp')) %>% 
#   st_transform(st_crs(grd)) #make it have same projection as the grid
# #save a rds version and read that in in the future:
# write_rds(traps_g_OR_logs_in_WA_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_OR_logs_2008_2018_20220915_clipped to WA waters.rds"))
traps_g_OR_logs_in_WA_waters_2008_2018raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_OR_logs_2008_2018_20220915_clipped to WA waters.rds')) %>% 
  select(-path, -layer) #columns that have been added in QGIS step, when joining files



##next bring in license files and see if can match across states


#OR license data file has column for 'docnum' which could match with something in WA logs
# (also 'number' which should match OR license number) 
#if we bring back federal ID to WA logs, that should match docnum in OR license data --> not every time
traps_g_WA_logs_in_OR_waters_2010_2020 <- traps_g_WA_logs_in_OR_waters_2010_2020raw %>% 
  st_set_geometry(NULL)

#if we bring back federal ID to WA logs, that should match docnum in OR license data --> not every time
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(SetID, FederalID) %>% 
  distinct()

raw_WA_logs_selected_columns <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 

traps_g_WA_logs_in_OR_waters_2010_2020_FederalID <- traps_g_WA_logs_in_OR_waters_2010_2020 %>% 
  left_join(raw_WA_logs_selected_columns, by="SetID")
#all SetIDs have a Federal ID

#unique vessels in WA logs that had pots in OR waters
unique_vessels <- as.list(unique(traps_g_WA_logs_in_OR_waters_2010_2020_FederalID$FederalID))




# Read in and join license & pot limit info
OR_pot_limit_info_raw <- read_csv(here::here('wdfw', 'data', 'OR', 'OregonCrabPermitData2007-2019.csv'))

OR_pot_limit_info <- OR_pot_limit_info_raw %>% 
  rename(Vessel = Docnum,
         PermitNumber = Number)

OR_pot_limit_info %<>%
  mutate(Begindate=as.Date(Begindate,"%m/%d/%Y"),
         Enddate=as.Date(Enddate,"%m/%d/%Y"))


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

#2009-2010 season - 7 mins to run with permit data filtered to unique_vessels
ODFW_Dcrab_logbooks_20092010 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
  filter(season=='2009-2010')
tm <- proc.time()
ODFW_Dcrab_logbooks_20092010_joined <- fuzzy_left_join(
  ODFW_Dcrab_logbooks_20092010, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

#not all find a match, likely to do with early years of WA data
length(unique(ODFW_Dcrab_logbooks_20092010_joined$Vessel.x)) #28 unique vessels in 2009/10 that fished in OR but landed in WA

no_license_info <- ODFW_Dcrab_logbooks_20092010_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #19 = 68% didn't find OR PotLimit info




#2013-2014 season -  12mins to run with permit data filtered to unique_vessels
ODFW_Dcrab_logbooks_20132014 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
  filter(season=='2013-2014')
tm <- proc.time()
ODFW_Dcrab_logbooks_20132014_joined <- fuzzy_left_join(
  ODFW_Dcrab_logbooks_20132014, OR_pot_limit_info_v2,
  by = c(
    "FederalID" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm

length(unique(ODFW_Dcrab_logbooks_20132014_joined$Vessel.x)) #23 unique vessels in 2013/14 that fished in OR but landed in WA

no_license_info <- ODFW_Dcrab_logbooks_20132014_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #4 = 17% didn't find OR PotLimit info









#WA license data only has license number, not vessel name.
#the vessel name in OR logs in WA waters, might match docnum in OR license, which matches Federal ID in WA logs, from which can get WA license
#but this won't always match
traps_g_OR_logs_in_WA_waters_2008_2018 <- traps_g_OR_logs_in_WA_waters_2008_2018raw %>% 
  st_set_geometry(NULL) #%>% 
  #select(Vessel, OR_Lcns,SetID, SetID2,SetDate)
  
  
OR_pot_limit_info_raw <- read_csv(here::here('wdfw', 'data', 'OR', 'OregonCrabPermitData2007-2019.csv'))

OR_pot_limit_info_selected_columns <- OR_pot_limit_info_raw %>% 
  select(Number, Docnum)

OR_logs_in_WA_waters_plus_docnum <- traps_g_OR_logs_in_WA_waters_2008_2018 %>% 
  left_join(OR_pot_limit_info_selected_columns, by=c("Vessel" = "Docnum")) 

raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(License, FederalID) %>% 
  distinct()

#FederalID column in logs has a space between the first 3 and last 3 digits, while pacfin ticket don't have this gap
raw_WA_logs_selected_columns_new <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 
test_df_2 <- OR_logs_in_WA_waters_plus_docnum %>% 
  left_join(raw_WA_logs_selected_columns_new,by=c("Vessel" = "FederalID")) 

length(unique(test_df_2$Vessel)) #45

test_df_NA <- test_df_2 %>% filter(is.na(License))
length(unique(test_df_NA$Vessel)) #16 --> 36% of vessels of OR logs in WA waters don't find a WA license number (to provide WA PotLimit)
summary_by_season <- test_df_NA %>% 
  distinct(Vessel,season) %>% 
  group_by(season) %>% 
  summarise(n_row = n())

unique_vessels <- test_df_2 %>% 
  distinct(Vessel,season) %>% 
  group_by(season) %>% 
  summarise(n_row_all = n())

summary_by_season <- summary_by_season %>% 
  left_join(unique_vessels, by="season") %>% 
  mutate(prop_no_match = n_row/n_row_all)

#season    n_row n_row_all prop_no_match
# 2007-2008     1         3         0.333 33%
# 2008-2009     3         5         0.6   60%
# 2009-2010     2        11         0.182 18%
# 2010-2011     6        11         0.545 55%
# 2011-2012     1         5         0.2   20%
# 2013-2014     2         8         0.25  25%
# 2014-2015     1         7         0.143 14%
# 2015-2016     1        10         0.1   10%
# 2016-2017     3        12         0.25  25%
# 2017-2018     3        11         0.273 27%

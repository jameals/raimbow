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
#will iether need to get some other license data from WA, or ignore the first 3 seasons

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
#traps_g_OR_landed_OR_logs_2017_2018 <- traps_g_OR_landed_OR_logs %>% 
#  filter(season == '2017-2018') 
#st_write(traps_g_OR_landed_OR_logs_2017_2018, "traps_g_OR_logs_2017_2018_20220915.shp") 


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


# read in version of traps_g, point data, that is clipped to OR waters - WA logs landed in OR waters
# reading this shapefile in takes a long time
# traps_g_WA_logs_in_OR_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','WA logs shapefiles' ,'traps_g_WA_logs_2010_2020_20220915_clipped to OR waters.shp')) %>% 
#   st_transform(st_crs(grd)) %>%  #make it have same projection as the grid
#    dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
#              point_y = sf::st_coordinates(.)[,2])
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



#if we bring back federal ID to WA logs, that should match docnum in OR license data --> not every time
traps_g_WA_logs_in_OR_waters_2010_2020 <- traps_g_WA_logs_in_OR_waters_2010_2020raw %>%
  st_set_geometry(NULL)

#bring back federal ID to WA logs
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(SetID, FederalID) %>% 
  distinct()

#note that Federal ID in raw logs has a gap between the first 3 and last 3 digits
raw_WA_logs_selected_columns <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 

traps_g_WA_logs_in_OR_waters_2010_2020_FederalID <- traps_g_WA_logs_in_OR_waters_2010_2020 %>% 
  left_join(raw_WA_logs_selected_columns, by="SetID")
#all SetIDs have a Federal ID

#unique vessels in WA logs that had pots in OR waters
unique_vessels <- as.list(unique(traps_g_WA_logs_in_OR_waters_2010_2020_FederalID$FederalID)) #127




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

#2009-2010 season - 9 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20092010 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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

#not all find a match, likely to do with early years of WA data
length(unique(subset_traps_g_WA_logs_in_OR_waters_20092010_joined$Vessel.x)) #28 unique vessels in 2009/10 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20092010_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #19 = 68% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20092010_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20092010_joined_license.csv"))



#2010-2011 season - 10 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20102011 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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

#not all find a match, likely to do with early years of WA data
length(unique(subset_traps_g_WA_logs_in_OR_waters_20102011_joined$Vessel.x)) #24 unique vessels in 2010/11 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20102011_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #13 = 54% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20102011_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20102011_joined_license.csv"))



#2011-2012 season - 8 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20112012 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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

#not all find a match, likely to do with early years of WA data
length(unique(subset_traps_g_WA_logs_in_OR_waters_20112012_joined$Vessel.x)) #27 unique vessels in 2011/12 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20112012_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #22 = 81% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20112012_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20112012_joined_license.csv"))



#2012-2013 season - 10 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20122013 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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

#not all find a match, likely to do with early years of WA data
length(unique(subset_traps_g_WA_logs_in_OR_waters_20122013_joined$Vessel.x)) #21 unique vessels in 2012/13 that fished in OR but landed in WA

no_license_info <- subset_traps_g_WA_logs_in_OR_waters_20122013_joined %>% filter(is.na(Potlimit))
length(unique(no_license_info$Vessel.x)) #4 = 19% didn't find OR PotLimit info

#write_csv(subset_traps_g_WA_logs_in_OR_waters_20122013_joined,here::here('DCRB_sdmTMB', 'data', "subset_traps_g_WA_logs_in_OR_waters_20122013_joined_license.csv"))



#2013-2014 season -  20 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20132014 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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



#2014-2015 season - 12 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20142015 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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



#2015-2016 season - 13 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20152016 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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



#2016-2017 season - 18 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20162017 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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



#2017-2018 season - 13 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20172018 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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


#2018-2019 season - 16 mins to run with permit data filtered to unique_vessels
subset_traps_g_WA_logs_in_OR_waters_20182019 <- traps_g_WA_logs_in_OR_waters_2010_2020_FederalID %>% 
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




#summary of WA landed logs that were from OR waters
#that didn't match to OR license and pot limit info
#season       prop_no_match
# 2009-2010     68%
# 2010-2011     54%
# 2011-2012     81%
# 2012-2013     19%
# 2013-2014     17%
# 2014-2015     0%
# 2015-2016     3%
# 2016-2017     8%
# 2017-2018     6%
# 2018-2019     7%




traps_g_WA_logs_in_OR_waters_all_joined <- list.files(path = "C:/Users/Leena.Riekkola/Projects/raimbow/DCRB_sdmTMB/data/WA logs in OR waters joined to OR license info", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv,col_types = 'cccddcdddddddcccccccccccd') %>% 
  # Combine data sets into one
  bind_rows                                                        
#glimpse(traps_g_WA_logs_in_OR_waters_all_joined)









#-----------------------

#WA license data only has license number, not vessel name.
#the vessel name in OR logs in WA waters, might match docnum in OR license, which matches Federal ID in WA logs, 
#from logs and Federal ID we can get WA license, from which can get WA Pot Limit
#but this won't always match
traps_g_OR_logs_in_WA_waters_2008_2018 <- traps_g_OR_logs_in_WA_waters_2008_2018raw %>% 
  st_set_geometry(NULL) #%>% 
  #select(Vessel, OR_Lcns,SetID, SetID2,SetDate)

#unique vessels in OR logs that had pots in WA waters
unique_vessels_OR_logs_in_WA_waters <- traps_g_OR_logs_in_WA_waters_2008_2018 %>% 
  select(Vessel) %>% 
  distinct()


raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>% 
  select(License, FederalID) %>% 
  distinct() %>% 
  rename(WA_License = License)

#FederalID column in WA logs has a space between the first 3 and last 3 digits, while pacfin ticket don't have this gap
raw_WA_logs_selected_columns_new <-as.data.frame(apply(raw_WA_logs_selected_columns,2, str_remove_all, " ")) 
#then can match Federal ID from WA logs to OR logs
unique_vessels_OR_logs_in_WA_waters_joined_WA_license <- unique_vessels_OR_logs_in_WA_waters %>% 
  left_join(raw_WA_logs_selected_columns_new,by=c("Vessel" = "FederalID")) 

length(unique(unique_vessels_OR_logs_in_WA_waters_joined_WA_license$Vessel)) #45
#but sometimes OR vessel name, therefore OR docnum, so WA Federal ID is linked to different WA license numbers in different years

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

#season    n_row n_row_all prop_no_match
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

# read in WA logs in WA waters
# reading this shapefile in takes a long time
# traps_g_WA_logs_in_WA_waters_raw <- read_sf(here::here('DCRB_sdmTMB','data','WA logs shapefiles' ,'traps_g_WA_logs_2010_2020_20220915_clipped to WA waters.shp')) %>% 
#   st_transform(st_crs(grd)) #make it have same projection as the grid
# #save a rds version and read that in in the future:
# write_rds(traps_g_WA_logs_in_WA_waters_raw,here::here('DCRB_sdmTMB','data',"traps_g_WA_logs_2010_2020_20220915_clipped to WA waters.rds"))
traps_g_WA_logs_in_WA_waters_2010_2020raw <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_WA_logs_2010_2020_20220915_clipped to WA waters.rds')) %>% 
  select(-path, -layer) %>% #columns that have been added in QGIS step, when joining files 
  #x and y are point locations, northing and easting
  #crs was CA_Curr_Lamb_Azi_Equal_Area
  dplyr::mutate(point_x = sf::st_coordinates(.)[,1],
              point_y = sf::st_coordinates(.)[,2]) 

traps_g_WA_logs_in_WA_waters_2010_2020 <- traps_g_WA_logs_in_WA_waters_2010_2020raw %>% 
  st_set_geometry(NULL) %>% #remove geometry as it is slowing everything down
  #drop couple useless columns
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
  ) %>% 
  #add column
  mutate(Pot_State = 'WA', #Pot_State = in what state did the pot occur, according to logbook
         OR_License = NA)  #as these are WA landed data, and pots were in WA waters, there is no OR license info that is relevant


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



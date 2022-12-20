## distance to port

# associate each pot with a port of landing
# for each grid, make a list of 'valid landing ports'
# measure distance e.g. from grid centroid to all valid landing ports (average distance to landing port)


#-------------------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(viridis)
library(here)
library(rnaturalearth)
library(fasterize)
library(sp)
library(magrittr)
library(raster)
select <- dplyr::select
library(scales)
library(gridExtra)
library(nngeo)
library(scales)
library(stringr)
#-------------------------------------------------------------------------------------------------

# read in df with all OR and WA pots as point

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))


#we need to join Fishticket1 from WA raw logbooks back to data
#we need to join TicketNum/FishTicket from OR raw logbooks back to data
#join using each states SetID
#they both match with FTID in pacfin data
#note that in OR logs, the FishTicket column sometimes has multiple entries

WA_logs <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% filter(Landing_logbook_state == "WA")
OR_logs <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% filter(Landing_logbook_state == "OR")


## WA
#bring in raw logbooks, and using SetID, join in info from 'Fishticket1' column
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>%
  select(SetID, FishTicket1) %>%
  distinct()
#each set ID matches to 1 FIshticket1
## WHEN WANT TO JOIN TO LBS AND $ NEED TO JOIN WITH ALL FISTICKETS LINKED TO THAT STRING

#join Fisticket1 into data
WA_logs_fishticket1 <- WA_logs %>%
  left_join(raw_WA_logs_selected_columns, by="SetID")
#each SetID associated with only 1 FishTicket1

#not all data find a Fishticket1
has_NAs <- WA_logs_fishticket1 %>% filter(is.na(FishTicket1))
unique(has_NAs$SetID) #1450 SetIDs
summary_table <- has_NAs %>% group_by(Pot_State) %>% summarise(n_distinct_SetID = n_distinct(SetID))
#Pot_State    n_distinct_SetID
#OR                 35
#WA               1436
nrow(has_NAs)/nrow(WA_logs)*100 ##0.9% of pots/of WA logs didn't have a Fishticket number recorded in logbooks to be matched to PacFin tickets



## OR
#bring in raw logbooks, and using SetID, join in info from 'FishTicket' column
raw_OR_logs <- read_csv(here('wdfw', 'data','OR','ODFW-Dcrab-logbooks-compiled_stackcoords_license_2007-2020_20221025.csv'),col_types = 'cdcTddddddddddcccccddccc')
#SetID is the same between the two files
raw_OR_logs_selected_columns <- raw_OR_logs %>%
  select(SetID, FishTicket) %>% #add in DEP as rpobably need it later anyways to adjust for 30% vs 100% entry years
  distinct() %>% 
  rename(FishTicket1 = FishTicket) #to match WA
#each set ID matches to 1 FIshTicket

#but note that some FishTickets have multiple entries in the grid cell
#also some have 2 fishtix but the numbers are not separated by a space or a character
## THIS IS ALSO RELEVANT WHEN WANT TO JOIN WITH LBS OR $. FOR LANDING PORT JUST NEED ONE FISTIX TO LINK TO
raw_OR_logs_selected_columns_v2 <- raw_OR_logs_selected_columns %>%
  separate(col=FishTicket1, into=c('FishTicket1', 'FishTicket2'), sep=';')

raw_OR_logs_selected_columns_v3 <- raw_OR_logs_selected_columns_v2 %>%
  mutate(n_char_tix1 = nchar(FishTicket1),
         n_char_tix2 = nchar(FishTicket2))

raw_OR_logs_selected_columns_v4 <- raw_OR_logs_selected_columns_v3 %>%
  separate(col=FishTicket1, into=c('FishTicket1', 'FishTicket3', 'FishTicket4', 'FishTicket5', 'FishTicket6'), sep=',')

nchar_test <- raw_OR_logs_selected_columns_v4 %>%
  mutate(n_char_tix1 = nchar(FishTicket1))
#now Fishicket1 column has only 1 value

#for now one fishtix is enough to find landing port
#join Fisticket1 into data
OR_logs_fishticket1 <- OR_logs %>%
  left_join(raw_OR_logs_selected_columns_v4, by="SetID") %>% 
  #drop columns not needed
  select(-(FishTicket3:n_char_tix2))
#ALL data find a Fishticket1 - noNAs
#each SetID associated with only 1 FishTicket1


#-------------------------------------------------------------------------

##NEW WAY
# read in separate df for OR and WA pots as points
#these have been weighted to correct for OR 30% data entry, and compliance
#also missing GridIDs have been fixed

OR_weighted_pots <- read_rds(here::here('DCRB_sdmTMB', 'data','OR_pots_points_but_weighted.rds'))

#for WA forgot to delete some extra columns before saving
WA_weighted_pots <- read_rds(here::here('DCRB_sdmTMB', 'data','WA_pots_points_but_weighted.rds')) %>% 
  select(-(n_traps_all:ratio))

WA_OR_weighted_pots <- rbind(WA_weighted_pots, OR_weighted_pots) %>% 
  #the second half-month identifier is based on landing date
  rename(half_month_landing_date = half_month)

#-------------------------------------------------------------------------

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

# df is large so subset to years of interest
fishtix_2007_2020 <- fishtix_raw %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
  VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
  PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME, PACFIN_GROUP_PORT_CODE,
  PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)")

fishtix_landing_port_only <- fishtix_2007_2020 %>% 
  select(FTID, PACFIN_PORT_NAME, PACFIN_PORT_CODE, PACFIN_GROUP_PORT_CODE) %>% 
  distinct() #requires distinct call here, otherwise some rows get repeated
#First several instance of Fishticket being associated with multiple ports
#because one FTID might be linked to multiple PacFIn FISH_TICKET_ID.
#Many of these cases seemed not to be DCRB catch but other species - maybe mistake in FTID 
#if filter for DCRB in earlier step, then only 7 FTID linked to more than 1 port
#View(fishtix_landing_port_only %>% group_by(FTID) %>% summarise(n_ports = n_distinct(PACFIN_PORT_NAME)))
#Looks like FTID is not fully unique and may be reused after some years
#but these FTIDs don't aeem to appear in the logbooks, so they are not a problem
list <- c("D459453","D533128","D577685","Z635276","Z635278","Z635279","Z635288")


glimpse(WA_logs_fishticket1)
glimpse(OR_logs_fishticket1)

ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- rbind(WA_logs_fishticket1, OR_logs_fishticket1) 
#each SetID2 associated with only 1 Fishticket




#join landing port via PacFIn FishTIx

ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port <- ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  left_join(fishtix_landing_port_only, by = c("FishTicket1" = "FTID"))
#number of unique SetIDs and data rows are still the same

##NEW 
ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port <- WA_OR_weighted_pots %>% 
  left_join(fishtix_landing_port_only, by = c("FishTicket1" = "FTID"))
#number of unique SetIDs and data rows are still the same



#what % of pots did not find Port
no_port_match <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% filter(is.na(PACFIN_PORT_NAME))
nrow(no_port_match) / nrow(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port) *100 #0.79% -- 0% in the new way as issues have already been ropped out
#some of this was due to small % of WA raw logs not having a Fishticket1 recorded. -- these are already dropped in new run
#some (majority?) fishtix numbers from logs don't appear as FTID in PacFIn data
unique(no_port_match$FishTicket1)


ports_used <- unique(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$PACFIN_PORT_NAME) 
port_groups_used <- unique(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$PACFIN_GROUP_PORT_CODE) 


##probably need to correct for 30%vs100% data entry in OR, and also for compliance as well (?) before can calc % of pots to ports:
#this has now been done, pots are weighted
summary_pots_to_ports <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% 
  filter(!is.na(PACFIN_PORT_NAME)) %>% 
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  group_by(PACFIN_PORT_NAME, PACFIN_PORT_CODE) %>% 
  #summarise(n_pots = n()) %>% 
  summarise(n_pots = sum(tottraps_FINAL)) %>% #now sum tottraps_FINAL column as that is weighted
  #mutate(percent_pots = n_pots/nrow(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port)*100)
  mutate(percent_pots = n_pots/sum(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$tottraps_FINAL)*100)

summary_pots_to_port_groups <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% 
  filter(!is.na(PACFIN_GROUP_PORT_CODE)) %>% 
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  group_by(PACFIN_GROUP_PORT_CODE) %>% 
  #summarise(n_pots = n()) %>% 
  summarise(n_pots = sum(tottraps_FINAL)) %>% #now sum tottraps_FINAL column as that is weighted
  #mutate(percent_pots = n_pots/nrow(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port)*100)
  mutate(percent_pots = n_pots/sum(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$tottraps_FINAL)*100)


#those pots that don't have grid IDs might be in grids that were split into pieces due to land etc
#or points that are on land in terms of grid, but not in terms of the depth rater used

#will need to fix those
#    OR_2009-2010_60805, OR_2009-2010_63542, OR_2009-2010_63844, OR_2013-2014_114062, OR_2013-2014_115031, ...
# ...OR_2013-2014_116872, OR_2013-2014_118538, OR_2013-2014_118660, OR_2013-2014_118809, OR_2013-2014_120280, 
# ...OR_2013-2014_120830, OR_2013-2014_121068, OR_2013-2014_121501, OR_2013-2014_121662, OR_2013-2014_134735 --> 5km grid id 91891
#OR_2017-2018_150255, OR_2017-2018_150487 --> 5km grid id 108730
#OR_2008-2009_35331, OR_2008-2009_34625, OR_2008-2009_21606, OR_2008-2009_22112, OR_2008-2009_23531,
# ...OR_2009-2010_47313, OR_2018-2019_29393, OR_2018-2019_29397, OR_2018-2019_29400, OR_2018-2019_29404,
# ...OR_2018-2019_29412, OR_2018-2019_29414, OR_2018-2019_29418, OR_2018-2019_29428, OR_2018-2019_29432 --> 5km grid id 117311
#WA logs with na grid ID:243 unique SetIDs
#WA_2018-2019_2662, WA_2018-2019_28260, WA_2018-2019_28264, WA_2018-2019_28268, WA_2018-2019_28272,
# ...WA_2018-2019_5671, WA_2018-2019_5674, WA_2018-2019_5677, WA_2018-2019_5680, WA_2018-2019_5683,
# ...WA_2018-2019_2686, WA_2017-2018_23466, WA_2017-2018_223471, WA_2017-2018_23476, WA_2017-2018_23481,
#... WA_2017-2018_23486, WA_2014-2015_28049, WA_2014-2015_28051, WA_2014-2015_28053, WA_2014-2015_28055,
# ...WA_2014-2015_28057, WA_2014-2015_28059, WA_2012-2013_15792, WA_2012-2013_15800, WA_2012-2013_15807,
# ...WA_2012-2013_15814, WA_2012-2013_15821, WA_2012-2013_15829, WA_2012-2013_15837, WA_2012-2013_15844,
# ...WA_2012-2013_15852, WA_2012-2013_15860, WA_2012-2013_15867, WA_2012-2013_15875, WA_2012-2013_15877,
# ...WA_2012-2013_15885, WA_2012-2013_15888, WA_2012-2013_15892, WA_2012-2013_15900, WA_2012-2013_15909,
# ...WA_2012-2013_15916, WA_2012-2013_15922, WA_2012-2013_15930, WA_2012-2013_15932, WA_2012-2013_15940,
# ...WA_2012-2013_15944, WA_2012-2013_15946, WA_2012-2013_15954, WA_2012-2013_15965, WA_2012-2013_15970 --> grid 122919
##NOT FINISHED WITH THIS

summary_ports_to_grid <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>%
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  group_by(GRID5KM_ID) %>%
  #summarise(N = n(), type = toString(unique(PACFIN_PORT_NAME)), .groups = 'drop') 
  summarise(N = sum(tottraps_FINAL), type = toString(unique(PACFIN_PORT_NAME)), .groups = 'drop') #do this now that each row (or pot) is actually weighted

summary_port_groups_to_grid <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>%
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  group_by(GRID5KM_ID) %>%
  #summarise(N = n(), type = toString(unique(PACFIN_PORT_NAME)), .groups = 'drop') 
  summarise(N = sum(tottraps_FINAL), type = toString(unique(PACFIN_GROUP_PORT_CODE)), .groups = 'drop') #do this now that each row (or pot) is actually weighted





summary_ports_to_grid_v2 <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% 
  filter(!is.na(PACFIN_PORT_NAME)) %>% 
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  group_by(GRID5KM_ID,PACFIN_GROUP_PORT_CODE, PACFIN_PORT_NAME) %>% 
  summarise(n_pots = sum(tottraps_FINAL)) %>% #now sum tottraps_FINAL column as that is weighted
  left_join(summary_ports_to_grid %>% dplyr::select(-type), by = c("GRID5KM_ID")) %>% 
  mutate(percent_pots_from_this_grid = n_pots/N*100,
         percent_all_pots = n_pots/sum(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$tottraps_FINAL)*100)

##note that ports: O WA COAST, O COL WA, UNKN WA, O S PUGET and A N PUGET
#arent ports with coordinates -- would need to be dropped, or use some proxy coordinates

n_pots_in_grids <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% 
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  #also for simplicity, drop those were port not clearly defined
  filter(!PACFIN_PORT_NAME %in% c("O WA COAST", "O COL WA", "UNKN WASH",  "O S PUGET",  "O N PUGET" )) %>% 
  group_by(GRID5KM_ID) %>%
  summarise(no_pots_in_this_grid = sum(tottraps_FINAL))  
  
n_pots_from_grid_to_port <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>%
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  #also for simplicity, drop those were port not clearly defined
  filter(!PACFIN_PORT_NAME %in% c("O WA COAST", "O COL WA", "UNKN WASH",  "O S PUGET",  "O N PUGET" )) %>% 
  group_by(GRID5KM_ID, PACFIN_PORT_NAME) %>%
  summarise(no_pots_from_this_grid_to_port = sum(tottraps_FINAL))

n_pots_from_grid_to_port_group <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>%
  #might need to drop WA5 as it is unknwon port
  filter(PACFIN_GROUP_PORT_CODE != "WA5") %>% 
  #also for simplicity, drop those were port not clearly defined
  filter(!PACFIN_PORT_NAME %in% c("O WA COAST", "O COL WA", "UNKN WASH",  "O S PUGET",  "O N PUGET" )) %>% 
  group_by(GRID5KM_ID, PACFIN_GROUP_PORT_CODE) %>%
  summarise(no_pots_from_this_grid_to_port_group = sum(tottraps_FINAL))

summary_table_port_group <- n_pots_in_grids %>% 
  left_join(n_pots_from_grid_to_port_group, by = c("GRID5KM_ID")) %>% 
  mutate(percent_pots_to_port_group = no_pots_from_this_grid_to_port_group/no_pots_in_this_grid*100) %>% 
  select(-no_pots_in_this_grid, -no_pots_from_this_grid_to_port_group)
  
summary_table_port <- n_pots_in_grids %>% 
  left_join(n_pots_from_grid_to_port, by = c("GRID5KM_ID")) %>% 
  mutate(percent_pots_to_port = no_pots_from_this_grid_to_port/no_pots_in_this_grid*100) %>% 
  select(-no_pots_in_this_grid, -no_pots_from_this_grid_to_port)



##find lat and lon of all ports



###BEFORE join to grid, maybe calc the weighted port distance, so that only have to find one valeu per empty grid
#now it would have a lsit of possible ports

study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_sf.rds"))
#grids that were in pieces and had repeating gridID have been fixed
#drop the exisitng predictors, so have one df with just the response variable
study_area_grids <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed %>% 
  select(-NGDC_GRID, -AREA, -season, -half_month) %>% 
  distinct()

study_area_grids_ports <- study_area_grids %>% 
  left_join(summary_table_port)


study_area <- read_sf(here::here('DCRB_sdmTMB','data','study_area.shp')) %>% 
  select(GRID5KM_ID,geometry)

study_area_grids_ports <- study_area %>% 
  left_join(summary_table_port)

#st_write(study_area_grids_ports, "study_area_grids_ports.shp") 




#join to grid to see if all study area grids have list of ports






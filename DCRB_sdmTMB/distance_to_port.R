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

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

# df is large so subset to years of interest
fishtix_2007_2020 <- fishtix_raw %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
  VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
  PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME,
  PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)")

fishtix_landing_port_only <- fishtix_2007_2020 %>% 
  select(FTID, PACFIN_PORT_NAME, PACFIN_PORT_CODE) %>% 
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




#what % of pots did not find Port
no_port_match <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% filter(is.na(PACFIN_PORT_NAME))
nrow(no_port_match) / nrow(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port) *100 #0.79%
#some of this was due to small % of WA raw logs not having a Fishticket1 recorded. 
#some (majority?) fishtix numbers from logs don't appear as FTID in PacFIn data
unique(no_port_match$FishTicket1)


ports_used <- unique(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port$PACFIN_PORT_NAME) 

##probably need to correct for 30%vs100% data entry in OR, and also for compliance as well (?) before can calc % of pots to ports:
summary_pots_to_ports <- ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port %>% 
  filter(!is.na(PACFIN_PORT_NAME)) %>% 
  group_by(PACFIN_PORT_NAME, PACFIN_PORT_CODE) %>% 
  summarise(n_pots = n()) %>% 
  mutate(percent_pots = n_pots/nrow(ALL_WA_2010_2020_and_ALL_OR_2008_2020_landing_port)*100)

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
  group_by(GRID5KM_ID) %>%
  summarise(N = n(), type = toString(unique(PACFIN_PORT_NAME)), .groups = 'drop') 
  


#will need to check that fish tickets aren't repeating when join to logs, e.g. due to different catch area codes
##find lat and lon of all ports


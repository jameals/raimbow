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
#they both match with FTID in pacfin data
#join using each states SetID
#note that in OR logs, the FishTicket column sometimes has multiple entries

WA_logs <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% filter(Landing_logbook_state == "WA")
OR_logs <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% filter(Landing_logbook_state == "OR")


## WA
#bring in raw logbooks, and using SetID, join in info t=from 'Fishticket1' column
raw_WA_logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
raw_WA_logs_selected_columns <- raw_WA_logs %>%
  select(SetID, FishTicket1) %>%
  distinct()
#each set ID matches to 1 FIshticket1

#join Fisticket1 into data
WA_logs_fishticket1 <- WA_logs %>%
  left_join(raw_WA_logs_selected_columns, by="SetID")

#not all data find a Fishticket1
has_NAs <- WA_logs_fishticket1 %>% filter(is.na(FishTicket1))
unique(has_NAs$SetID) #1450 SetIDs
summary_table <- has_NAs %>% group_by(Pot_State) %>% summarise(n_distinct_SetID = n_distinct(SetID))
#Pot_State    n_distinct_SetID
#OR                 35
#WA               1436
nrow(has_NAs)/nrow(WA_logs)*100 ##0.9% of pots/of WA logs didn't have a Fishticket number recorded in logbooks to be matched to PacFin tickets


#-------------------------------------------------------------------------

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

# df is large so subset to years of interest
fishtix_2007_2020 <- fishtix_raw %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE, NUM_OF_DAYS_FISHED, AGENCY_CODE,
  VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
  PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PORT_CODE, PACFIN_PORT_NAME,
  PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  #filter(PACFIN_SPECIES_CODE == "DCRB") %>% #maybe keep this if want to look at if vessels fish for multile species simultaneously
  #we will also filter out personal catch and research catch here, which has negligible impact to the results
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)")



#will need to check that fish tickets aren't repeating when join to logs, e.g. due to different catch area codes




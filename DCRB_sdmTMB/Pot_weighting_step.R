# Pot weighting step

# a thought: if we use PacFIn landing date to create half month steps,
#and look at who were active then (and try to find Pot Limits)
#the landing date might not match the date in logbook (when pots pulled)

#were there any logbook data that were landed in CA ports?
# --> no looks like all logbooked landings were to OR or WA ports
#but there could be landings to OR ports that were in CA waters?
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
library(lubridate)

#-------------------------------------------------------------------------------------------------


#Need to at least adjust for OR 30% data entry, and WA 33% pot reduction
#but also try to adjust for compliance? like ODFW said, using vessel IDs in FishTIx as the total that was out there?

#Bring in Fishtix file

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

# df is large so subset to years of interest
fishtix_2007_2020 <- fishtix_raw %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_CATCH_AREA_DESCRIPTION, CDFW_AREA_BLOCK, PACFIN_PORT_CODE, PACFIN_PORT_NAME,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)")


distinct_area_blocks_and_agency <- fishtix_2007_2020 %>% distinct(CDFW_AREA_BLOCK, AGENCY_CODE)
#maybe we need to only keep cases where CDFW_AREA_BLOCK is NA, or 0 (those are the only cases where agency is O or W)
#although also couple cases where CDFW_AREA_BLOCK is NA, or 0 but agency is C - keep those?
#looks like all logbooked landings were to OR or WA ports
#but there could be landings to OR ports that were in CA waters?

#so maybe fine to filter to W and O data, and just make sure that CDFW_AREA_BLOCK is NA 
#- meaning that hopefully catch was caught in OR waters

block_is_na_agency_is_C <- fishtix_2007_2020 %>% filter(is.na(CDFW_AREA_BLOCK) & AGENCY_CODE == 'C')
#only 5 cases
unique(block_is_na_agency_is_C$PACFIN_CATCH_AREA_DESCRIPTION)

block_is_0_agency_is_C <- fishtix_2007_2020 %>% filter(CDFW_AREA_BLOCK == 0 & AGENCY_CODE == 'C')
#only 5 cases
unique(block_is_0_agency_is_C$PACFIN_CATCH_AREA_DESCRIPTION)

fishtix_2007_2020_O_W <- fishtix_2007_2020 %>% filter(AGENCY_CODE != 'C')
unique(fishtix_2007_2020_O_W$CDFW_AREA_BLOCK)
#0 or NA
unique(fishtix_2007_2020_O_W$PACFIN_CATCH_AREA_DESCRIPTION)
#all these catches seem to have come from OR and WA waters (though also option like 'multiple areas')
#so could look at unique vessel IDs in fishtix_2007_2020_O_W and try to find Pot Limits (note WA data doesn't cover all that)






# read in df with all OR and WA pots as point

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))

test_OR_2017_2018 <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>%  
  filter(Landing_logbook_state == 'OR') %>% 
  filter(season == '2017-2018') %>% 
  mutate(d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>% 
  select(-d, -period)
glimpse(test_OR_2017_2018)

#how many unique vessels and pot limits were in each half-month time step -- I think this option 1
#is the only one that doesn't work
test_OR_2017_2018_max_pots <- test_OR_2017_2018 %>% 
  group_by(season, half_month) %>% 
  distinct(Vessel, OR_Pot_Limit)
test_OR_2017_2018_max_pots_v2 <- test_OR_2017_2018_max_pots %>% 
  group_by(season, half_month) %>%
  summarise(pots_30_percent = sum(OR_Pot_Limit))
#these half-monthly pot counts represent 30% of the pots due to 30% data entry
#in which case 100% of pots in each time step would have been
test_OR_2017_2018_max_pots_v3 <- test_OR_2017_2018_max_pots_v2 %>% 
  mutate(pots_100_percent = pots_30_percent/0.3,
         pots_70_percent = pots_100_percent*0.7,  #these are the missing 70% of pots
         pot_weightx = pots_30_percent/pots_100_percent) #what each of 30% entered pots need to be worth to adjust for missing 70%
  
xproportional_dist_of_pots_in_grids <- test_OR_2017_2018 %>% 
  group_by(season, half_month, GRID5KM_ID, grd_x, grd_y, AREA) %>%
  summarise(tottraps=n()) %>% 
  left_join(test_OR_2017_2018_max_pots_v3, by=c('season', 'half_month')) %>% 
  mutate(prop_of_pots = tottraps/pots_100_percent) %>% 
  mutate(tottraps_expanded = tottraps + (prop_of_pots*pots_70_percent))
#then adjust by pot limit? but how as already on grid level?
#or this is already adjusted for pot limit due to the calculation of the max possible pots based on vessels and their licenses?


##option 3
test_OR_2017_2018_weighted_pots_new <- test_OR_2017_2018 %>% 
  mutate(original_pot_weight = 1) %>% 
  mutate(pot_weight_expand_30percent = 0.3) %>%  #each point is worth 0.3 pots as we are missing 70% (0.7) pots
  filter(!is.na(GRID5KM_ID)) %>% 
  # count up traps for vessel in 2-week period
  group_by(season, half_month, Vessel, OR_PermitNumber, OR_Pot_Limit) %>%  
  summarise(
    n_traps_vessel=sum(pot_weight_expand_30percent)
  ) %>% 
  # create a column with weighting - proportion of max allowed traps
  # divide pot limit by number of simulated traps
  # because you want to up-weight traps < pot_limit, and downweight traps > pot_limit
  mutate(trap_limit_weight = OR_Pot_Limit/n_traps_vessel) %>% #now weight for pot limit
  ungroup()

# join the "weighting key" back to the simulated pots data
test_OR_2017_2018_with_weighting_new <- test_OR_2017_2018 %>% 
  left_join(test_OR_2017_2018_weighted_pots_new,by=c('season', 'half_month','Vessel','OR_PermitNumber','OR_Pot_Limit'))

traps_summ_new <- test_OR_2017_2018_with_weighting_new %>% 
  group_by(season, half_month, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  # this is the new/key step -- weighted_traps 
  summarise(tottraps=sum(trap_limit_weight)) #%>%  
#mutate(
#  M2_trapdens=M2_tottraps/(AREA/1e6)
#) %>% ungroup() %>% 
#filter(!is.na(M2_tottraps))
glimpse(traps_summ_new) 




##option 2 - first weight logged pots, then calc missing 70%
test_OR_2017_2018_weighted_pots <- test_OR_2017_2018 %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count up traps for vessel in 2-week period
  group_by(season, half_month, Vessel, OR_PermitNumber, OR_Pot_Limit) %>%  
  summarise(
    n_traps_vessel=n(), na.rm=TRUE 
  ) %>% 
  # create a column with weighting - proportion of max allowed traps
  # divide pot limit by number of simulated traps
  # because you want to up-weight traps < pot_limit, and downweight traps > pot_limit
  mutate(trap_limit_weight = OR_Pot_Limit/n_traps_vessel) %>% 
  ungroup()

# join the "weighting key" back to the simulated pots data
test_OR_2017_2018_with_weighting <- test_OR_2017_2018 %>% 
  left_join(test_OR_2017_2018_weighted_pots,by=c('season', 'half_month','Vessel','OR_PermitNumber','OR_Pot_Limit'))

traps_summ <- test_OR_2017_2018_with_weighting %>% 
  group_by(season, half_month, GRID5KM_ID, grd_x, grd_y, AREA) %>%  
  # this is the new/key step -- weighted_traps 
  summarise(tottraps=sum(trap_limit_weight)) #%>%  
  #mutate(
  #  M2_trapdens=M2_tottraps/(AREA/1e6)
  #) %>% ungroup() %>% 
  #filter(!is.na(M2_tottraps))
glimpse(traps_summ) 

weighted_pots_total_all_grids <- traps_summ %>% 
  group_by(season, half_month) %>% 
  summarise(total_weighted_pots = sum(tottraps))

proportional_dist_of_pots_in_grids <- traps_summ %>% 
  left_join(weighted_pots_total_all_grids, by=c('season', 'half_month')) %>% 
  mutate(prop_of_pots = tottraps/total_weighted_pots)

#then find the number for the missing 70%, times by propotion, summed to tottraps

#total_weighted_pots column represents the 30% of data entered. 
missing_70_percent <- weighted_pots_total_all_grids %>% 
  mutate(pots_100_percent = total_weighted_pots/0.3,
         pots_70_percent = pots_100_percent*0.7) %>% 
  select(-total_weighted_pots, -pots_100_percent)

#distribute the missing 70% of pots to grids
traps_summ_expanded <- proportional_dist_of_pots_in_grids %>% 
  left_join(missing_70_percent,by=c('season', 'half_month')) %>% 
  mutate(tottraps_expanded = tottraps + (prop_of_pots*pots_70_percent))
#this should now be the new pot count per grid per hal-month step, fixed for 30% data entry
#but now I think that weighting by pot limt already account for some of the missing 70% data entry...




test_OR_fishtix_2018 <- fishtix_raw %>% 
  filter(AGENCY_CODE == 'O') %>% 
  filter(LANDING_YEAR %in% c(2018)) %>% #2017-2018 season in OR did not include December 
  filter(LANDING_MONTH %in% c(1,2,3,4,5,6,7,8)) %>% #fishery opened in Jan, closed in Aug
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, LANDING_MONTH, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_CATCH_AREA_DESCRIPTION, CDFW_AREA_BLOCK, PACFIN_PORT_CODE, PACFIN_PORT_NAME,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)") %>% 
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month = paste0(LANDING_MONTH,"_",period)) %>% 
  select(-d, -period)

test_OR_fishtix_2018_April_1 <- test_OR_fishtix_2018 %>% 
  filter(half_month == '4_1')
OR_2018_unique_vessels <-  unique(test_OR_fishtix_2018_April_1$VESSEL_NUM) #156 unique vessels
#and vessel_num seems to match Docnum in OR license data
#maybe jsut do it this way, we might lose a little something for those effrot that are in WA waters, but perhaps who cares...
#pot limit might be diferent if have dual license, but we can't really do much about that

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
  select(PermitNumber, Vessel, Begindate, Enddate, Potlimit)# %>% 
  #filter(Vessel %in% OR_2018_unique_vessels) 

library(fuzzyjoin)

tm <- proc.time()
test_OR_fishtix_2018_joined <- fuzzy_left_join(
  test_OR_fishtix_2018, OR_pot_limit_info_v2,
  by = c(
    "VESSEL_NUM" = "Vessel",
    "LANDING_DATE" = "Begindate",
    "LANDING_DATE" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
proc.time()-tm


#maybe a simple left join is enough
OR_pot_limit_info_v2 <- OR_pot_limit_info %>% 
  select(PermitNumber, Vessel, Begindate, Enddate, Potlimit) %>%
  filter(Begindate > '2018-01-01' & Enddate < '2018-12-31')

test_OR_fishtix_2018_joined_test2 <- test_OR_fishtix_2018 %>% 
  distinct(VESSEL_NUM) %>% 
  left_join(OR_pot_limit_info_v2, by=c("VESSEL_NUM" = "Vessel"))


#what if make atable
#filter fishtix for OR
#and selected years, DCRB etc
#create column for season based on landing date - remove months 9, 10, 11
#get distinct season and vessel_num combos
#then join OR license info - will have repeating rows 
#look for cases where pot limit for vessel number varies, and fix those

fishtix_OR_2007_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE == 'O') %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  filter(LANDING_MONTH %in% c(12,1,2,3,4,5,6,7,8)) %>% #OR fishery always closes in August
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, LANDING_MONTH, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_CATCH_AREA_DESCRIPTION, CDFW_AREA_BLOCK, PACFIN_PORT_CODE, PACFIN_PORT_NAME,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)") %>% 
  mutate(season_start = ifelse(LANDING_MONTH == 12, LANDING_YEAR, LANDING_YEAR-1)) %>% 
  mutate(season_end = ifelse(LANDING_MONTH == 12, LANDING_YEAR+1, LANDING_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  filter(season != '2006-2007') %>% #don't need this one, no logs anyways
  filter(season != '2020-2021')

#maybe a simple left join is enough
OR_pot_limit_info_v2 <- OR_pot_limit_info %>% 
  select(PermitNumber, Vessel, Potlimit)

test_OR_fishtix_2008_2020_joined_test <- fishtix_OR_2007_2020 %>% 
  distinct(season, VESSEL_NUM) %>% 
  left_join(OR_pot_limit_info_v2, by=c("VESSEL_NUM" = "Vessel"))

summary_of_Pot_Limits <- test_OR_fishtix_2008_2020_joined_test %>% 
  group_by(VESSEL_NUM,season) %>% 
  summarise(distint_pot_lims = n_distinct(Potlimit))



  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month = paste0(LANDING_MONTH,"_",period)) %>% 
  select(-d, -period)


#take one year of OR fishtix at a time
#use fuzzyjoin to get pot limit for each unique vessel num in a year
#join pot limit to fishtix
#then break into half_months in each year
#




#need to find and fix Grids with ID = NA. also if PotLim is NA



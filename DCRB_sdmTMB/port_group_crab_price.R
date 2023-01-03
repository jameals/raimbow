#port group specific price for crab

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

#port cpecific crab price data downloaded form here: https://reports.psmfc.org/pacfin/f?p=501:1000:

crab_price_by_port_raw <- read_csv(here('DCRB_sdmTMB', 'data', 'port group crab price','PacFin_CRAB002-W-O-2007_2020_price_by_port.csv'))


crab_price_by_port_selected_columns <- crab_price_by_port_raw %>% 
  select(CRAB_YEAR, AGENCY_CODE, PACFIN_GROUP_PORT_CODE, ends_with('PPP')) %>% 
  select(-TOTAL_ROUND_WEIGHT_PPP)

crab_price_by_port_long <- crab_price_by_port_selected_columns %>% 
  pivot_longer(
    cols = NOV_ROUND_WEIGHT_PPP:OCT_ROUND_WEIGHT_PPP,
    names_to = "month",
    values_to = "ppp"
  ) %>% 
  separate(col=month, into=c('month', 'extra'), sep='_') %>% 
  select(-extra) %>% 
  mutate(month_name = case_when(
    month == 'NOV' ~ 'November',
    month == 'DEC' ~ 'December',
    month == 'JAN' ~ 'January',
    month == 'FEB' ~ 'February',
    month == 'MAR' ~ 'March',
    month == 'APR' ~ 'April',
    month == 'MAY' ~ 'May',
    month == 'JUN' ~ 'June',
    month == 'JUL' ~ 'July',
    month == 'AUG' ~ 'August',
    month == 'SEP' ~ 'September',
    month == 'OCT' ~ 'October'
  )) %>% 
  select(-month) %>% 
  mutate(season_start = ifelse(month_name == 'December', CRAB_YEAR, CRAB_YEAR-1)) %>% 
  mutate(season_end = ifelse(month_name == 'December', CRAB_YEAR+1, CRAB_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end, -CRAB_YEAR, -AGENCY_CODE)



#read in proportion of pots to port groups by half month
proportion_pots_to_port_group_by_halfmonth <- read_rds(here::here('DCRB_sdmTMB', 'data', "proportion_pots_to_port_group_by_halfmonth.rds")) %>% 
  #this needs a column for month as fuel price is by month not half-month
  mutate(half_month_dummy = half_month_SetID) %>% 
  separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
  select(-period)


#join crab price to df with proportion of pots from grid to port group
proportion_pots_to_port_group_by_halfmonth_crab_price <- proportion_pots_to_port_group_by_halfmonth %>% 
  left_join(crab_price_by_port_long, by=c('season', 'month_name','PACFIN_GROUP_PORT_CODE'))
#some NAs - same as with fuel price, if NA, use price of that month from closest port group


proportion_pots_to_port_group_by_halfmonth_crab_price_noNAs <- proportion_pots_to_port_group_by_halfmonth_crab_price %>% 
  filter(!is.na(ppp)) 

proportion_pots_to_port_group_by_halfmonth_crab_price_NAs <- proportion_pots_to_port_group_by_halfmonth_crab_price %>% 
  filter(is.na(ppp)) %>% 
  #drop couple columns to avoid repeating columns in left_join
  select(-ppp) %>% 
  mutate(PACFIN_GROUP_PORT_CODE2 = case_when(
    PACFIN_GROUP_PORT_CODE == 'BRA' ~ 'CBA',
    PACFIN_GROUP_PORT_CODE == 'CBA' ~ 'NPA',
    PACFIN_GROUP_PORT_CODE == 'CLO' ~ 'CLW',
    PACFIN_GROUP_PORT_CODE == 'CLW' ~ 'CLO',
    PACFIN_GROUP_PORT_CODE == 'CWA' ~ 'CLW',
    PACFIN_GROUP_PORT_CODE == 'NPA' ~ 'TLA',
    PACFIN_GROUP_PORT_CODE == 'NPS' ~ 'SPS',
    PACFIN_GROUP_PORT_CODE == 'SPS' ~ 'NPA',
    PACFIN_GROUP_PORT_CODE == 'TLA' ~ 'CLO'
  )) %>% 
  inner_join(crab_price_by_port_long, by=c("PACFIN_GROUP_PORT_CODE2"= "PACFIN_GROUP_PORT_CODE", "season", "month_name")) %>% 
  select(-PACFIN_GROUP_PORT_CODE2)


proportion_pots_to_port_group_by_halfmonth_crab_price_fixed <- rbind(proportion_pots_to_port_group_by_halfmonth_crab_price_noNAs,proportion_pots_to_port_group_by_halfmonth_crab_price_NAs)
#but still some NAs




##so maybe better to work on compiled pacfin file??



#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

# df is large so subset to years of interest
fishtix_2007_2020 <- fishtix_raw %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  filter(AGENCY_CODE != 'C') %>% #just want O and W landing
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)") %>% 
  #filter out catch that is from Puget Sound as not part of WA coastal catches
  #and as those vessels won't appear in wA coastal fishery permit data
  filter(PACFIN_CATCH_AREA_DESCRIPTION != 'PUGET SOUND') %>% 
  #if gear name/code is trawl, then likely crab caught in non commercial DCRB fishery (i.e. was bycatch but was sold)
  #this was chatted with Troy (ODFW) re: OR data
  filter(GEAR_NAME %in% c("CRAB POT", "CRAB RING", "SHELLFISH POT (CRAB)", "SHELLFISH POT (NON-CRAB)", "BOTTOMFISH POT", "FISH POT")) %>% 
  #also take out catch labelled as treaty Indian fisher
  filter(PARTICIPATION_GROUP_NAME != "TREATY INDIAN COMMERCIAL FISHER") %>% 
  #Troy ODFW: Bay Dungeness crab is 825 and ocean/Columbia River Dungeness crab is 824. same data in 'species code' as well
  filter(MARKET_CATEGORY != 825) %>% 
  filter(SPECIES_CODE_NAME != "CRAB, DUNGENESS (BAY)") %>% 
  filter(PRODUCT_USE_NAME != "DISCARD") %>% 
  filter(DISPOSITION_NAME != "SEIZED (ILLEGAL)") %>% 
  #once add some extra filters, NAs (for ppp or vessel revenue) are gone, but there are still some 0s
  #but only 15 so drop those
  filter(EXVESSEL_REVENUE > 0) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME, PACFIN_GROUP_PORT_CODE,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME,
         PACFIN_CATCH_AREA_DESCRIPTION, GEAR_NAME, PARTICIPATION_GROUP_NAME) 
  
















#fuel prices

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

#fuel price data is form here: https://www.psmfc.org/efin/data/fuel.html#Data

#fuel prices only checked once month, so can't get separate values for each half month step
#need to apply each month's price to both half month steps

#for now downloaded OR and WA, but may also need CA? (Maybe jsut the northern ports of Crescent City and Eureka?)
fuel_OR <- read_csv(here('DCRB_sdmTMB', 'data', 'fuel','fuelor.csv'))
fuel_WA <- read_csv(here('DCRB_sdmTMB', 'data', 'fuel','fuelwa.csv'))

fuel_or_wa_raw <- rbind(fuel_OR, fuel_WA)

fuel_or_wa <- fuel_or_wa_raw %>% 
  #drop out some useless years
  filter(YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

fuel_or_wa_v2 <- transform(fuel_or_wa, month_name = month.name[MONTH])

fuel_or_wa_v3 <- fuel_or_wa_v2 %>% 
  mutate(season_start = ifelse(MONTH == 12, YEAR, YEAR-1)) %>% 
  mutate(season_end = ifelse(MONTH == 12, YEAR+1, YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end, -notes) 

# if pricettl and pricegal are 0, they are actually NA, no info was available
fuel_or_wa_v4 <- fuel_or_wa_v3 %>% 
  mutate_at(c('pricettl','pricegal'), ~na_if(., 0))
  
#for each ports where fuel price info collected, assign PacFIn port group code (see e.g. distance to port script)
unique(fuel_or_wa_v4$portname)
#port name                port group
# "Astoria"               CLO
# "Brookings"             BRA
# "Florence"              CBA
# "Gold Beach"            BRA
# "Newport"               NPA
# "Winchester Bay"        CBA
# "Tillamook/Garabaldi"   TLA
# "Anacortes"             NPS
# "Bellingham Bay"        NPS
# "Blaine"                NPS
# "Everett"               SPS
# "Ilwaco/Chinook"        CLW
# "Neah Bay"              NPS
# "Olympia"               SPS
# "Port Angeles"          NPS
# "Seattle"               SPS
# "Shelton"               SPS
# "Tacoma"                SPS
# "Port Townsend"         NPS
# "West Port"             CWA

fuel_or_wa_v5 <- fuel_or_wa_v4 %>% 
  mutate(PACFIN_GROUP_PORT_CODE = case_when(
    portname %in% c('Brookings','Gold Beach') ~ 'BRA',
    portname %in% c('Florence','Winchester Bay') ~ 'CBA',
    portname %in% c('Astoria') ~ 'CLO',
    portname %in% c('Ilwaco/Chinook') ~ 'CLW',
    portname %in% c('West Port') ~ 'CWA',
    portname %in% c('Newport') ~ 'NPA',
    portname %in% c('Anacortes','Bellingham Bay','Blaine', 'Neah Bay','Port Angeles','Port Townsend') ~ 'NPS',
    portname %in% c('Everett','Olympia','Seattle','Shelton', 'Tacoma') ~ 'SPS',
    portname %in% c('Tillamook/Garabaldi') ~ 'TLA'
  ))



#find average fuel price within each month as can't do half month step)
#for each port group (see port grouping e.g. herehttps://www.psmfc.org/efin/docs/2020FuelPriceReport.pdf -- did it based on PacFin see dist to port code) 
#that data is available
fuel_price_month_step_portgroup <- fuel_or_wa_v5 %>% 
  group_by(season, month_name, PACFIN_GROUP_PORT_CODE) %>% #don't include dock code here ##STATE, port, portname
  summarise(
            #avg_pricettl = mean(pricettl, na.rm = TRUE),
            avg_pricegal = mean(pricegal, na.rm = TRUE)
            )

#25 cases (month and port combos)  where no fuel price available
#TLA port group the one that mostly has NA - did they stop checking these...?
#use state average? or nearby port group average? --> nearby portgroup as state can be very variables
#there are more NAs in the avg_pricettl variable, so probably better to just stick to avg_pricegal


fuel_price_month_step_portgroup_noNAs <- fuel_price_month_step_portgroup %>% 
  filter(!is.na(avg_pricegal)) 

fuel_price_month_step_portgroup_NAs <- fuel_price_month_step_portgroup %>% 
  filter(is.na(avg_pricegal)) %>% 
  select(-avg_pricegal) %>% 
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
  )) %>% inner_join(fuel_price_month_step_portgroup, by=c("PACFIN_GROUP_PORT_CODE2"= "PACFIN_GROUP_PORT_CODE", "season", "month_name")) %>% 
  #one more case of NA as in the same month (May 2020) both CLO and CLW don't have fuel price
  mutate(avg_pricegal = case_when(
    is.na(avg_pricegal) & PACFIN_GROUP_PORT_CODE == 'CLO' ~ 1.49,
    is.na(avg_pricegal) & PACFIN_GROUP_PORT_CODE == 'CLW' ~ 1.19,
    !is.na(avg_pricegal) ~ avg_pricegal 
  )) %>% 
  select(-PACFIN_GROUP_PORT_CODE2)

fuel_price_month_step_portgroup_fixed <- rbind(fuel_price_month_step_portgroup_noNAs,fuel_price_month_step_portgroup_NAs)




#read in proportion of pots to port groups by half month
proportion_pots_to_port_group_by_halfmonth <- read_rds(here::here('DCRB_sdmTMB', 'data', "proportion_pots_to_port_group_by_halfmonth.rds")) %>% 
  #this needs a column for month as fuel price is by month not half-month
  mutate(half_month_dummy = half_month_SetID) %>% 
  separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
  select(-period)


#join fuel price to df with proportion of pots from grid to port group






#at some point need to adjust for inflation, all $ in dollars of that specific year etc
























#can also download and use monthly state prices
fuel_state_averages_raw <- read_csv(here('DCRB_sdmTMB', 'data', 'fuel','state_averages.csv'))

fuel_state_averages <- fuel_state_averages_raw %>% 
  #drop out alaska
  filter(STATE != 'AK') %>% 
  #drop out some useless years
  filter(YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

fuel_state_averages_v2 <- transform(fuel_state_averages, month_name = month.name[MONTH])

fuel_state_averages_v3 <- fuel_state_averages_v2 %>% 
  mutate(season_start = ifelse(MONTH == 12, YEAR, YEAR-1)) %>% 
  mutate(season_end = ifelse(MONTH == 12, YEAR+1, YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) 








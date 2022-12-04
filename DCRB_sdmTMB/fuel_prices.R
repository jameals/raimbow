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
  

#find average fuel price within each month (step)as can't do half month step)
#for each port (or could do port group? see por grouping e.g. herehttps://www.psmfc.org/efin/docs/2020FuelPriceReport.pdf) 
#that data is available
fuel_price_month_step_port <- fuel_or_wa_v4 %>% 
  group_by(season, month_name, STATE, port, portname) %>% #don't include dock code here
  summarise(avg_pricettl = mean(pricettl, na.rm = TRUE),
            avg_pricegal = mean(pricegal, na.rm = TRUE)
            )

#if wanted to group by PSMFC/EFIN port regions before getting monthly avg
#Northern Washington: Blaine, Bellingham Bay, Anacortes, Port Townsend
#Puget Sound: Everett, Tacoma, Olympia, Shelton, Seattle
#Washington Coast: Neah Bay, Port Angeles, Westport, Ilwaco/Chinook
#Oregon: Astoria, Newport, Florence, Winchester Bay
#California: Crescent City, Eureka, Sausalito, San Francisco, Moss Landing, Morro Bay, Santa Barbara, Port Hueneme, San Pedro



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








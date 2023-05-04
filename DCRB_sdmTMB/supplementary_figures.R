##make supplementary pltos of vessels landing, max pots, and fishery footprint area


#---------------------------------------------


library(here)
library(tidyverse)
library(viridis)
library(ggeffects)
library(ggplot2)
library(sf)
library(lubridate)

#---------------------------------------------
#---------------------------------------------

#number of unique vessels landing throught season, across seasons


#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 

#initally use same filtering as for crab ppp
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
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_MONTH, LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME, PACFIN_GROUP_PORT_CODE,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME,
         PACFIN_CATCH_AREA_DESCRIPTION, GEAR_NAME, PARTICIPATION_GROUP_NAME) 


#filter a bit more, don't need 2007 and 2008, add season, month
fishtix_2009_2020 <- fishtix_2007_2020 %>% 
  mutate(season_start = ifelse(LANDING_MONTH == 12, LANDING_YEAR, LANDING_YEAR-1)) %>% 
  mutate(season_end = ifelse(LANDING_MONTH == 12, LANDING_YEAR+1, LANDING_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  #don't need seasons we don't have logs for
  filter(season %in% c('2009-2010','2010-2011','2011-2012','2012-2013', '2013-2014',
                       '2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_MONTH, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>%
  select(-d, -period)

#summarise number of unique vessels landing by month and season

unique_vessels_landing <- fishtix_2009_2020 %>% 
  group_by(season, month_name) %>% 
  summarise(n_unique_vessel = n_distinct(VESSEL_NUM)) %>% 
  #drop october and november as no actual crabbing season then
  filter(!month_name %in% c('October', 'November')) %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March',
                                                    'April','May','June','July','August','September')))



plot_number_of_vessels <- ggplot(unique_vessels_landing, aes(x= month_name, y= n_unique_vessel, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Number of unique vessels landing") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.text.y = element_text(size = 13, colour = 'black'),
        axis.title = element_text(size = 12, colour = 'black'),
        axis.line = element_line(colour = 'black', size = 0.7),
        axis.ticks.length=unit(.1, "cm"),
        axis.ticks=element_line(size=0.7, colour = 'black'),
        legend.position="bottom"
  )
plot_number_of_vessels



#--------------------------------------------------------------------------------------------

#plot of maximum number of pots out in water by month and season

#run code in 'pot weighting step' to join fishtix with license data (as well as possible)
#then for OR, and for WA, grouped data by season and month, and took distinct vessel & licence combos
#and summed to get the 'max' possible pots - separate for the 2 state still


max_pots_per_month_OR <- read_csv(here('DCRB_sdmTMB', 'data','max_pots_per_month_OR.csv')) 
max_pots_per_month_WA <- read_csv(here('DCRB_sdmTMB', 'data','max_pots_per_month_WA.csv')) 

max_pots_by_month <- max_pots_per_month_WA %>%  left_join(max_pots_per_month_OR, by=c('season', 
                                                                                   'month_name')) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(max_pots = total_pots.x+total_pots.y) %>% 
  select(-total_pots.x, -total_pots.y) %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March',
                                                    'April','May','June','July','August','September')))



plot_max_pots <- ggplot(max_pots_by_month, aes(x= month_name, y= max_pots, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Maximum number of pots") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text.x = element_text(size = 12, colour = 'black'),
    axis.text.y = element_text(size = 13, colour = 'black'),
    axis.title = element_text(size = 12, colour = 'black'),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    legend.position="bottom"
  )
plot_max_pots


#--------------------------------------------------------------------------------------------

#fishery footprint area by month and by season

#we'll use logbook data that is in point for, and linked to grids with an area
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds')) 

fisery_footprint <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  #drop the first 2 seasons (OR data only)
  filter(!season %in% c("2007-2008","2008-2009")) %>% 
  filter(!is.na(month_name)) %>% 
  filter(!is.na(AREA)) %>% 
  group_by(season, month_name) %>% 
  distinct(GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6, na.rm=TRUE) %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March',
                                                    'April','May','June','July','August','September')))


plot_fishery_footprint <- ggplot(fisery_footprint, aes(x= month_name, y= total_area_km2, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Fishery footprint area (km2)") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text.x = element_text(size = 12, colour = 'black'),
    axis.text.y = element_text(size = 13, colour = 'black'),
    axis.title = element_text(size = 12, colour = 'black'),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    legend.position="bottom"
  )
plot_fishery_footprint















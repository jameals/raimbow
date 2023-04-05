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
## bunch of missing crab prices if do this way

# crab_price_by_port_raw <- read_csv(here('DCRB_sdmTMB', 'data', 'port group crab price','PacFin_CRAB002-W-O-2007_2020_price_by_port.csv'))
# 
# 
# crab_price_by_port_selected_columns <- crab_price_by_port_raw %>% 
#   select(CRAB_YEAR, AGENCY_CODE, PACFIN_GROUP_PORT_CODE, ends_with('PPP')) %>% 
#   select(-TOTAL_ROUND_WEIGHT_PPP)
# 
# crab_price_by_port_long <- crab_price_by_port_selected_columns %>% 
#   pivot_longer(
#     cols = NOV_ROUND_WEIGHT_PPP:OCT_ROUND_WEIGHT_PPP,
#     names_to = "month",
#     values_to = "ppp"
#   ) %>% 
#   separate(col=month, into=c('month', 'extra'), sep='_') %>% 
#   select(-extra) %>% 
#   mutate(month_name = case_when(
#     month == 'NOV' ~ 'November',
#     month == 'DEC' ~ 'December',
#     month == 'JAN' ~ 'January',
#     month == 'FEB' ~ 'February',
#     month == 'MAR' ~ 'March',
#     month == 'APR' ~ 'April',
#     month == 'MAY' ~ 'May',
#     month == 'JUN' ~ 'June',
#     month == 'JUL' ~ 'July',
#     month == 'AUG' ~ 'August',
#     month == 'SEP' ~ 'September',
#     month == 'OCT' ~ 'October'
#   )) %>% 
#   select(-month) %>% 
#   mutate(season_start = ifelse(month_name == 'December', CRAB_YEAR, CRAB_YEAR-1)) %>% 
#   mutate(season_end = ifelse(month_name == 'December', CRAB_YEAR+1, CRAB_YEAR)) %>% 
#   mutate(season = paste0(season_start,"-",season_end)) %>% 
#   select(-season_start, -season_end, -CRAB_YEAR, -AGENCY_CODE)
# 
# 
# 
# #read in proportion of pots to port groups by half month
# proportion_pots_to_port_group_by_halfmonth <- read_rds(here::here('DCRB_sdmTMB', 'data', "proportion_pots_to_port_group_by_halfmonth.rds")) %>% 
#   #this needs a column for month as fuel price is by month not half-month
#   mutate(half_month_dummy = half_month_SetID) %>% 
#   separate(col=half_month_dummy, into=c('month_name', 'period'), sep='_') %>% 
#   select(-period)
# 
# 
# #join crab price to df with proportion of pots from grid to port group
# proportion_pots_to_port_group_by_halfmonth_crab_price <- proportion_pots_to_port_group_by_halfmonth %>% 
#   left_join(crab_price_by_port_long, by=c('season', 'month_name','PACFIN_GROUP_PORT_CODE'))
# #some NAs - same as with fuel price, if NA, use price of that month from closest port group
# 
# 
# proportion_pots_to_port_group_by_halfmonth_crab_price_noNAs <- proportion_pots_to_port_group_by_halfmonth_crab_price %>% 
#   filter(!is.na(ppp)) 
# 
# proportion_pots_to_port_group_by_halfmonth_crab_price_NAs <- proportion_pots_to_port_group_by_halfmonth_crab_price %>% 
#   filter(is.na(ppp)) %>% 
#   #drop couple columns to avoid repeating columns in left_join
#   select(-ppp) %>% 
#   mutate(PACFIN_GROUP_PORT_CODE2 = case_when(
#     PACFIN_GROUP_PORT_CODE == 'BRA' ~ 'CBA',
#     PACFIN_GROUP_PORT_CODE == 'CBA' ~ 'NPA',
#     PACFIN_GROUP_PORT_CODE == 'CLO' ~ 'CLW',
#     PACFIN_GROUP_PORT_CODE == 'CLW' ~ 'CLO',
#     PACFIN_GROUP_PORT_CODE == 'CWA' ~ 'CLW',
#     PACFIN_GROUP_PORT_CODE == 'NPA' ~ 'TLA',
#     PACFIN_GROUP_PORT_CODE == 'NPS' ~ 'SPS',
#     PACFIN_GROUP_PORT_CODE == 'SPS' ~ 'NPA',
#     PACFIN_GROUP_PORT_CODE == 'TLA' ~ 'CLO'
#   )) %>% 
#   inner_join(crab_price_by_port_long, by=c("PACFIN_GROUP_PORT_CODE2"= "PACFIN_GROUP_PORT_CODE", "season", "month_name")) %>% 
#   select(-PACFIN_GROUP_PORT_CODE2)
# 
# 
# proportion_pots_to_port_group_by_halfmonth_crab_price_fixed <- rbind(proportion_pots_to_port_group_by_halfmonth_crab_price_noNAs,proportion_pots_to_port_group_by_halfmonth_crab_price_NAs)
# #but still some NAs




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
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_MONTH, LANDING_YEAR, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME, PACFIN_GROUP_PORT_CODE,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME,
         PACFIN_CATCH_AREA_DESCRIPTION, GEAR_NAME, PARTICIPATION_GROUP_NAME) 
  

#what was (avg) ppp in each port group in each half-month step?
ppp_halfmonth_portgroup <- fishtix_2007_2020 %>% 
  mutate(season_start = ifelse(LANDING_MONTH == 12, LANDING_YEAR, LANDING_YEAR-1)) %>% 
  mutate(season_end = ifelse(LANDING_MONTH == 12, LANDING_YEAR+1, LANDING_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  #don't need seasons we don't have logs for
  filter(season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013',
                       '2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) %>% 
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_MONTH, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>%
  select(-d, -period, -month_name) %>% 
  #sometimes fishticket_ID repeats  
  group_by(season, half_month, FISH_TICKET_ID, PACFIN_GROUP_PORT_CODE) %>% 
  summarise(EXVESSEL_REVENUE = sum(EXVESSEL_REVENUE),
            LANDED_WEIGHT_LBS = sum(LANDED_WEIGHT_LBS)) %>% 
#nrow(ppp_halfmonth_portgroup)
#length(unique(ppp_halfmonth_portgroup$FISH_TICKET_ID))
#now there is only one record per unique fishticket ID
  group_by(season, half_month, PACFIN_GROUP_PORT_CODE) %>% 
  summarise(total_rev = sum(EXVESSEL_REVENUE),
            total_lbs = sum(LANDED_WEIGHT_LBS)) %>% 
  mutate(ppp = total_rev/total_lbs)


#--------------------------------------------
#adjust for inflation, all $ in dollars of that specific year etc

cpi_raw <- read_csv(here('wdfw', 'data', 'cpi_2021.csv'),col_types='idc')

# add a conversion factor to 2020 $$
cpi <- cpi_raw %>% 
  mutate(convert2020=1/(annual_average/258.8)) %>% 
  filter(year>2006) %>% 
  filter(year<2021) %>% 
  dplyr::select(year,convert2020) 


#this df needs a 'year' column so can join with cpi
ppp_halfmonth_portgroup_v2 <- ppp_halfmonth_portgroup %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_start", "season_end"), sep = "-") %>% 
  mutate(half_month2 = half_month) %>% 
  separate(half_month2, into = c("month_name", "period"), sep = "_") %>% 
  mutate(year = ifelse(month_name == "December", season_start, season_end)) %>% 
  select(-season_start, -season_end, -period) 

ppp_halfmonth_portgroup_v2$year <- as.numeric(ppp_halfmonth_portgroup_v2$year)


ppp_halfmonth_portgroup_adj_inf <- ppp_halfmonth_portgroup_v2 %>% 
  left_join(cpi, by = c('year')) %>% 
  mutate(ppp_adj = ppp * convert2020) %>% 
  #drop columns no longer needed
  select(-(total_rev:convert2020))



### go to bottom to do fuel pricing with the same proportion of pots to port groups as with dist to port



#--------------------------------------------------------
#are we going to run into lot of NA cases if work on half-month instead of month?

#read in proportion of pots to port groups by half month
#re-did this using landing date based half month - outputs are in folder 'v2'
proportion_pots_to_port_group_by_halfmonth <- read_rds(here::here('DCRB_sdmTMB', 'data', "proportion_pots_to_port_group_by_halfmonth_based_on_landing_date.rds")) %>% 
  rename(half_month = half_month_landing_date) #%>% 
  #because some logs don't have a set_date, in earlier steps NAs created for half-month steps (in lhalf_month_SetID only)
  #filter(half_month != "NA_NA")


#join crab price to df with proportion of pots from grid to port group
proportion_pots_to_port_group_by_halfmonth_grab_price <- proportion_pots_to_port_group_by_halfmonth %>% 
  left_join(ppp_halfmonth_portgroup_adj_inf, by=c('season', 'half_month','PACFIN_GROUP_PORT_CODE'))  
#136 NAs - in some half months no landing to a given port group so no ppp 
# - could be because area was closed, these will get fixed when deal with closed area
#76 when remove half_month = NA_NA
#all NAs are December or January so likely to do with area openings
#drop these and let them be covered by interpolation?
#or do what did with fuel, use price from closest port group?


proportion_pots_to_port_group_by_halfmonth_grab_price_noNAs <- proportion_pots_to_port_group_by_halfmonth_grab_price %>% 
  filter(!is.na(ppp_adj)) 

proportion_pots_to_port_group_by_halfmonth_grab_price_NAs <- proportion_pots_to_port_group_by_halfmonth_grab_price %>% 
  filter(is.na(ppp_adj)) %>% 
  #drop couple columns to avoid repeating columns in left_join
  select(-ppp_adj) %>% 
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
  inner_join(ppp_halfmonth_portgroup_adj_inf, by=c("PACFIN_GROUP_PORT_CODE2"= "PACFIN_GROUP_PORT_CODE", "season", "half_month")) %>% 
  select(-PACFIN_GROUP_PORT_CODE2)


proportion_pots_to_port_group_by_halfmonth_grab_price_fixed <- rbind(proportion_pots_to_port_group_by_halfmonth_grab_price_noNAs,proportion_pots_to_port_group_by_halfmonth_grab_price_NAs)


#--------------------------------------------------------
#weight port group specific crab price by proportion of pots in grid

weighted_crab_price <- proportion_pots_to_port_group_by_halfmonth_grab_price_fixed %>% 
  mutate(price_multiply_prop = ppp_adj * prop_pots_to_port_group) %>% 
  group_by(GRID5KM_ID, season, half_month) %>% 
  summarise(weighted_crab_ppp = sum(price_multiply_prop)) 


#------------------------------------------------

#-------------------------
#https://www.youtube.com/watch?v=9whoSguh7Z4

#specify points where want to estimate/interpolate the variable (the unknown points)
# that would be the grid centroids
grid_centroids <- read_csv(here::here('DCRB_sdmTMB','data','dist to ports','grid_centroids.csv'))
grid_centroids_sf <- st_as_sf(grid_centroids, 
                              coords = c("grd_x", "grd_y"),
                              crs = 4326
)
plot(grid_centroids_sf)


library(gstat)


#data to be used for interpolation
weighted_crab_price_points <- weighted_crab_price %>% 
  left_join(grid_centroids) 

weighted_crab_price_points_sf <- st_as_sf(weighted_crab_price_points, 
                                          coords = c("grd_x", "grd_y"),
                                          crs = 4326
)



##This is where need to loop through all season and half-month combos
subset <- weighted_crab_price_points_sf %>% 
  filter(season=="2019-2020", half_month=="September_1")
plot(subset)


#locations specifies the dataset. idp = alpha, how important are we going to make distance
test_idw <- gstat::idw(formula=weighted_crab_ppp~1, 
                       locations = subset, 
                       newdata=grid_centroids_sf, 
                       idp =1) #idp default is 1
#var1.pred = the interpolated value at the point
#for those points that were the input, the interpolated var1.pred is exactly the same as the input value


test_join <- st_join(test_idw, grid_centroids_sf) %>% 
  #after this don't need geometry column, only grid ID
  #and also don't need var1.var column
  select(var1.pred, GRID5KM_ID) %>% 
  st_set_geometry(NULL) %>% 
  rename(weighted_crab_ppp = var1.pred) %>% 
  #but do need columns denoting season and half-month - these would need to be added here
  mutate(season = "2019-2020", half_month="September_1") %>% 
  #reorder columns
  select(GRID5KM_ID, season, half_month, weighted_crab_ppp)

#now would just need to loop this heaps of times....
#start a dummy df into which rbind all idw data (at each season - half-month combo)?

# columns <- c("GRID5KM_ID", "season", "half_month", "weighted_crab_ppp")
# dummy_df <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
# colnames(dummy_df) = columns
# df_crab_price <- dummy_df 

df_crab_price <- df_crab_price %>% 
  rbind(test_join)

unique(df_crab_price$half_month)


###
#note that e.g. 2007-2008 is OR data only so ends in August_1; 
#2008-2009 ends at August_1 -- only OR logs, OR closed Aug 14 

#2009-2010 - actually has some point for September_1, even though fishery supposedly closed on Aug 14 - if from fishtix, could've landed after Aug 14 but pots were out of water by then
#2009-2010 even September_1 has data...

#2012-2013, 2013-2014 starts late (December_2)
#2014-2015 doesn't have August_2, but has August_1 and September_1
#2015-2016 starts January_1, matches data from DFWs
#2017-2018 starts January_2, matches data from DFWs
#2018-2019 starts January_1, matches data from DFWs

#interpolated_crab_price_2019_2020 <-  df_crab_price
#nrow(interpolated_crab_price_2019_2020)
#write_rds(interpolated_crab_price_2019_2020,here::here('DCRB_sdmTMB', 'data', 'port group crab price', 'v2', "interpolated_crab_price_2019_2020.rds"))

#------------------------------
#2014-2015 season had no pots in August_2, but as fishery (in WA) was open
#we need crab prices for grid cells in that month for the presence/absence model
#this was accidentally done by re-doing September_1 instead of August_2 so September_1 ended up repeating in df
#first drop the erroneous September_1
interpolated_crab_price_2014_2015 <- interpolated_crab_price_2014_2015[1:27576,]

interpolated_crab_price_2014_2015_august1_september1 <- interpolated_crab_price_2014_2015 %>% 
  filter(half_month=='August_1' | half_month=='September_1') %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(weighted_crab_ppp = mean(weighted_crab_ppp)) %>% 
  mutate(season = "2014-2015", half_month = "August_2") %>% 
  select(GRID5KM_ID, season, half_month, weighted_crab_ppp) %>% 
  ungroup()

interpolated_crab_price_2014_2015 <- rbind(interpolated_crab_price_2014_2015, interpolated_crab_price_2014_2015_august1_september1)
#write_rds(interpolated_crab_price_2014_2015,here::here('DCRB_sdmTMB', 'data', 'port group crab price', 'v2', "interpolated_crab_price_2014_2015.rds"))


#------------------------------
#2013-2014 needs crab price for December_1 -- use data for December_2
interpolated_crab_price_2013_2014

interpolated_crab_price_2013_2014_december_fix <- interpolated_crab_price_2013_2014 %>% 
  filter(half_month=="December_2") %>% 
  mutate(half_month = str_replace(half_month, "December_2", "December_1"))

interpolated_crab_price_2013_2014 <- rbind(interpolated_crab_price_2013_2014, interpolated_crab_price_2013_2014_december_fix)
#write_rds(interpolated_crab_price_2013_2014,here::here('DCRB_sdmTMB', 'data', "port group crab price",'v2', "interpolated_crab_price_2013_2014.rds"))


#2017-2018 needs crab price for January_1 -- use data for January_2
interpolated_crab_price_2017_2018

interpolated_crab_price_2017_2018_january_fix <- interpolated_crab_price_2017_2018 %>% 
  filter(half_month=="January_2") %>% 
  mutate(half_month = str_replace(half_month, "January_2", "January_1"))

interpolated_crab_price_2017_2018 <- rbind(interpolated_crab_price_2017_2018, interpolated_crab_price_2017_2018_january_fix)
#write_rds(interpolated_crab_price_2017_2018,here::here('DCRB_sdmTMB', 'data', "port group crab price",'v2', "interpolated_crab_price_2017_2018.rds"))

#------------------------------

interpolated_crab_price_all <- rbind(interpolated_crab_price_2007_2008,
                                     interpolated_crab_price_2008_2009,
                                     interpolated_crab_price_2009_2010,
                                     interpolated_crab_price_2010_2011,
                                     interpolated_crab_price_2011_2012,
                                     interpolated_crab_price_2012_2013,
                                     interpolated_crab_price_2013_2014,
                                     interpolated_crab_price_2014_2015,
                                     interpolated_crab_price_2015_2016,
                                     interpolated_crab_price_2016_2017,
                                     interpolated_crab_price_2017_2018,
                                     interpolated_crab_price_2018_2019,
                                     interpolated_crab_price_2019_2020
                                     )

#write_rds(interpolated_crab_price_all,here::here('DCRB_sdmTMB', 'data', 'port group crab price','v2', "interpolated_crab_price_all.rds"))

#------------------------------

study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel.rds"))


study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel %>% 
  left_join(interpolated_crab_price_all, by=c('GRID5KM_ID', 'season', 'half_month'))
#the cases where grid has NA for crab price should be cases where grids were closed (season closures etc)
#these should get removed when get around to dealing with open/closed areas (grids)
#not all NAs are during closed periods - so needs fixing
#discrepancy is likely due to half_monthly by landing day or Set Date
#we can ignore 2007-2008 and 2008-2009 as those will get dropped from the analysis
#some other cases are when fishery fully closed, so fix only times when fishery is open
#actually only 2 specific cases when fishery open but NAs for crab price
# season    half_month
# 2013-2014 December_1
# 2017-2018 January_1 
#fill these with the second half of that month (same as for fuel price)
#code for this is above


#write_rds(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth_faults_canyon_escarp_portdist_fuel_crabprice.rds"))












### do fuel pricing with the same proportion of pots to port groups as with dist to port

#prop pots to port group done across full data set
proportion_pots_to_port_group <- read_rds(here::here('DCRB_sdmTMB', 'data', "proportion_pots_to_port_group_across_all_data.rds")) %>% 
  ungroup()
#but if join that to fuel prices, end up missing some cases where e.g. fuel surveys ended
#proportion_pots_to_port_group is missing 2 grids: 121931 122578
#for 121931 use prop pots to port from 121933: CWA = 9.644169e-01, NPS = 3.558309e-02
#for 122578 use prop pots to port from 122579: CLW = 0.2112838131, CWA = 0.7692591820

df_fix_NAs <- data.frame (GRID5KM_ID  = c(121931,121931,122578,122578),
                          PACFIN_GROUP_PORT_CODE = c("CWA", "NPS", "CLW", "CWA"),
                          prop_pots_to_port_group = c(9.644169e-01, 3.558309e-02, 0.2112838131, 0.7692591820)
)

proportion_pots_to_port_group <- rbind(proportion_pots_to_port_group, df_fix_NAs)





restricted_study_area <- read_sf(here::here('DCRB_sdmTMB','data', 'restricted_study_area.shp')) %>% 
  st_set_geometry(NULL) 

df_full_final_raw <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_raw.rds')) 
restricted_study_area_grids <- sort(unique(restricted_study_area$GRID5KM_ID))
df_full_final_in_restricted_study_area <- df_full_final_raw %>% filter(GRID5KM_ID %in% restricted_study_area_grids) 


proportion_pots_to_port_group_restricted_study_area <- df_full_final_in_restricted_study_area %>% 
  left_join(proportion_pots_to_port_group, by=c('GRID5KM_ID'))

proportion_pots_to_port_group_restricted_study_area_crab_price  <- proportion_pots_to_port_group_restricted_study_area %>% 
  left_join(ppp_halfmonth_portgroup_adj_inf, by=c('season', 'half_month', 'PACFIN_GROUP_PORT_CODE'))



proportion_pots_to_port_group_restricted_study_area_crab_price_noNAs <- proportion_pots_to_port_group_restricted_study_area_crab_price %>%  
  filter(!is.na(ppp_adj)) 

#missing monthly ppp from pacfin: https://reports.psmfc.org/pacfin/f?p=501:402:13662342683453:INITIAL::::
NAs_crab_ppp <-  read_csv(here::here('DCRB_sdmTMB', 'data', "NAs_crab_ppp.csv")) %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_start", "season_end"), sep = "-") %>% 
  mutate(month2 = month_name) %>% 
  mutate(year = ifelse(month_name == "December", season_start, season_end)) %>% 
  select(-season_start, -season_end, -month2) 

NAs_crab_ppp$year <- as.numeric(NAs_crab_ppp$year)

NAs_crab_ppp_adj_inf <- NAs_crab_ppp %>% 
  left_join(cpi, by = c('year')) %>% 
  mutate(ppp_adj = ppp * convert2020) %>% 
  #drop columns no longer needed
  select(-(ppp:convert2020))

proportion_pots_to_port_group_restricted_study_area_crab_price_NAs <- proportion_pots_to_port_group_restricted_study_area_crab_price %>% 
  filter(is.na(ppp_adj)) %>% 
  select( -ppp_adj) %>% 
  left_join(NAs_crab_ppp_adj_inf, by=c("PACFIN_GROUP_PORT_CODE", "season", "month_name"))

proportion_pots_to_port_group_restricted_study_area_crab_price_fixed <- rbind(proportion_pots_to_port_group_restricted_study_area_crab_price_noNAs, proportion_pots_to_port_group_restricted_study_area_crab_price_NAs)



#--------------------------------------------------------
#weight port group specific crab price by proportion of pots in grid

weighted_crab_price <- proportion_pots_to_port_group_restricted_study_area_crab_price_fixed %>% 
  mutate(price_multiply_prop = ppp_adj * prop_pots_to_port_group) %>% 
  group_by(GRID5KM_ID, season, half_month) %>% 
  summarise(weighted_crab_ppp = sum(price_multiply_prop)) %>% 
  rename(weighted_crab_ppp_v2 = weighted_crab_ppp)


write_rds(weighted_crab_price,here::here('DCRB_sdmTMB', 'data',  "weighted_crab_price_fix.rds"))

#this part will be done in prep for sdmTMb script
# testtest7 <- df_full_final_in_restricted_study_area %>% left_join(weighted_crab_price)
# View(testtest7 %>% filter(open_closed=="open"))


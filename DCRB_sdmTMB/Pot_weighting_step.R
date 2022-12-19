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

# read in df with all OR and WA pots as point

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))


#-------------------------------------------------------------
#fix cases with pots with NA for GridID

#list cases where GridID = NA
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(is.na(GRID5KM_ID))
unique(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA$SetID2)

traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridNA %>% 
  mutate(
    GRID5KM_ID = case_when(
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2017-2018_150255', 'OR_2017-2018_150487') ~ 108730,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_60805', 'OR_2009-2010_63542', 'OR_2009-2010_63844', 'OR_2013-2014_114062', 'OR_2013-2014_115031',
                                        'OR_2013-2014_116872', 'OR_2013-2014_118538', 'OR_2013-2014_118660', 'OR_2013-2014_118809', 'OR_2013-2014_120280',
                                        'OR_2013-2014_120830', 'OR_2013-2014_121068', 'OR_2013-2014_121501', 'OR_2013-2014_121662', 'OR_2013-2014_134735','OR_2015-2016_134735') ~ 91891,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_47313', 'OR_2018-2019_29393', 'OR_2018-2019_29397', 'OR_2018-2019_29400', 'OR_2018-2019_29404',
                                        'OR_2018-2019_29412', 'OR_2018-2019_29414', 'OR_2018-2019_29418', 'OR_2018-2019_29428', 'OR_2018-2019_29432',
                                        'OR_2008-2009_35331','OR_2008-2009_34625','OR_2008-2009_21606','OR_2008-2009_22112','OR_2008-2009_23531') ~ 117311,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_5402', 'WA_2009-2010_5407', 'WA_2010-2011_98515', 'WA_2010-2011_98521', 'WA_2010-2011_98528',
                                        'WA_2010-2011_98536', 'WA_2010-2011_98543', 'WA_2013-2014_17748', 'WA_2014-2015_4429', 'WA_2015-2016_11939',
                                        'WA_2017-2018_24912') ~ 117640,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2017-2018_7660', 'WA_2019-2020_11073', 'WA_2019-2020_11076', 'WA_2019-2020_11084', 'WA_2019-2020_11089',
                                        'WA_2019-2020_11092', 'WA_2019-2020_11098', 'WA_2019-2020_11134') ~ 122588,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2009-2010_15317', 'WA_2009-2010_15318', 'WA_2009-2010_8387', 'WA_2009-2010_8389', 'WA_2009-2010_8391',
                                        'WA_2012-2013_21600', 'WA_2012-2013_21603', 'WA_2012-2013_21612', 'WA_2012-2013_21617', 'WA_2012-2013_21623',
                                        'WA_2012-2013_21625', 'WA_2012-2013_21628', 'WA_2012-2013_21633', 'WA_2012-2013_21639', 'WA_2017-2018_24617',
                                        'WA_2017-2018_24623', 'WA_2017-2018_24629', 'WA_2017-2018_24635', 'WA_2017-2018_24641', 'WA_2017-2018_24647',
                                        'WA_2017-2018_24653', 'WA_2017-2018_24659', 'WA_2017-2018_24665', 'WA_2017-2018_24671', 'WA_2017-2018_24677',
                                        'WA_2017-2018_24683', 'WA_2017-2018_24689', 'WA_2017-2018_24695', 'WA_2017-2018_24701', 'WA_2017-2018_24707',
                                        'WA_2017-2018_24713', 'WA_2017-2018_24719', 'WA_2017-2018_24725', 'WA_2017-2018_24731', 'WA_2017-2018_24737',
                                        'WA_2017-2018_24743', 'WA_2017-2018_24749', 'WA_2017-2018_24755', 'WA_2017-2018_24761', 'WA_2017-2018_24767',
                                        'WA_2017-2018_24773', 'WA_2017-2018_24779', 'WA_2017-2018_24785', 'WA_2017-2018_23332', 'WA_2017-2018_23335',
                                        'WA_2017-2018_23341', 'WA_2018-2019_28295', 'WA_2018-2019_28298', 'WA_2018-2019_28301', 'WA_2018-2019_28306',
                                        'WA_2018-2019_28309', 'WA_2018-2019_28312' ) ~ 122589,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2018-2019_2662', 'WA_2018-2019_28260', 'WA_2018-2019_28264', 'WA_2018-2019_28268', 'WA_2018-2019_28272',
                                        'WA_2018-2019_5671', 'WA_2018-2019_5674', 'WA_2018-2019_5677', 'WA_2018-2019_5680', 'WA_2018-2019_5683',
                                        'WA_2017-2018_23466', 'WA_2017-2018_23476', 'WA_2017-2018_23481', 'WA_2017-2018_23486', 'WA_2014-2015_28049',
                                        'WA_2014-2015_28051', 'WA_2014-2015_28053', 'WA_2014-2015_28055', 'WA_2014-2015_28057', 'WA_2014-2015_28059',
                                        'WA_2012-2013_15792', 'WA_2012-2013_15800', 'WA_2012-2013_15807', 'WA_2012-2013_15814', 'WA_2012-2013_15821',
                                        'WA_2012-2013_15829', 'WA_2012-2013_15837', 'WA_2012-2013_15844', 'WA_2012-2013_15852', 'WA_2012-2013_15860',
                                        'WA_2012-2013_15867', 'WA_2012-2013_15875', 'WA_2012-2013_15877', 'WA_2012-2013_15885', 'WA_2012-2013_15888',
                                        'WA_2012-2013_15892', 'WA_2012-2013_15900', 'WA_2012-2013_15909', 'WA_2012-2013_15916', 'WA_2012-2013_15922',
                                        'WA_2012-2013_15930', 'WA_2012-2013_15932', 'WA_2012-2013_15940', 'WA_2012-2013_15944', 'WA_2012-2013_15946',
                                        'WA_2012-2013_15954', 'WA_2012-2013_15965', 'WA_2012-2013_15970', 'WA_2017-2018_23471', 'WA_2018-2019_5686') ~ 122919,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_9919', 'WA_2013-2014_15078', 'WA_2013-2014_15080', 'WA_2013-2014_15081', 'WA_2013-2014_15083',
                                        'WA_2013-2014_15085', 'WA_2013-2014_15087', 'WA_2013-2014_15089', 'WA_2013-2014_15091', 'WA_2013-2014_15093',
                                        'WA_2013-2014_15095', 'WA_2013-2014_15097', 'WA_2013-2014_15099', 'WA_2013-2014_15101', 'WA_2013-2014_15103',
                                        'WA_2013-2014_15105', 'WA_2013-2014_15107', 'WA_2013-2014_15109', 'WA_2013-2014_15111', 'WA_2013-2014_15113',
                                        'WA_2013-2014_15115', 'WA_2013-2014_15117', 'WA_2013-2014_15119', 'WA_2013-2014_15121', 'WA_2013-2014_15123',
                                        'WA_2013-2014_15125', 'WA_2013-2014_15127', 'WA_2013-2014_15129', 'WA_2013-2014_15131', 'WA_2013-2014_15133',
                                        'WA_2013-2014_15135', 'WA_2013-2014_15137', 'WA_2013-2014_15139', 'WA_2013-2014_15141', 'WA_2013-2014_15143',
                                        'WA_2013-2014_15145', 'WA_2013-2014_15147', 'WA_2013-2014_15149', 'WA_2013-2014_15151', 'WA_2013-2014_15153',
                                        'WA_2013-2014_15155', 'WA_2013-2014_15157', 'WA_2013-2014_15159', 'WA_2015-2016_1078', 'WA_2015-2016_1088',
                                        'WA_2015-2016_10945', 'WA_2015-2016_10963', 'WA_2015-2016_1120', 'WA_2015-2016_1132', 'WA_2015-2016_1138',
                                        'WA_2015-2016_1163', 'WA_2015-2016_1179', 'WA_2015-2016_11795', 'WA_2015-2016_11809', 'WA_2015-2016_11820',
                                        'WA_2015-2016_11841', 'WA_2015-2016_11858', 'WA_2015-2016_1199', 'WA_2015-2016_1233', 'WA_2015-2016_1250',
                                        'WA_2015-2016_5428', 'WA_2015-2016_5444', 'WA_2015-2016_5462', 'WA_2015-2016_24635', 'WA_2015-2016_24638',
                                        'WA_2015-2016_24641', 'WA_2015-2016_24643', 'WA_2015-2016_24646', 'WA_2016-2017_26835', 'WA_2017-2018_11644',
                                        'WA_2019-2020_14099') ~ 120941,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2013-2014_28525', 'WA_2017-2018_26745') ~ 120280,
      is.na(GRID5KM_ID) & SetID2 %in% c('WA_2015-2016_3586', 'WA_2015-2016_3592', 'WA_2015-2016_3594', 'WA_2015-2016_3599', 'WA_2015-2016_3605',
                                        'WA_2015-2016_3610', 'WA_2015-2016_3614', 'WA_2015-2016_3630', 'WA_2015-2016_3632', 'WA_2015-2016_3634',
                                        'WA_2015-2016_3640', 'WA_2015-2016_3642', 'WA_2015-2016_3648', 'WA_2017-2018_7662', 'WA_2017-2018_7665',
                                        'WA_2017-2018_7668' ) ~ 122259
    )
  )

##############
# these points are in a grid that is not part of the 5x5km gridding because land areas are excluded by Blake in an earlier step
#NA -- WA_2012-2013_22421, WA_2012-2013_22423, WA_2012-2013_22425, WA_2012-2013_22427, WA_2012-2013_22429,
#      WA_2012-2013_22431, WA_2012-2013_22433, WA_2012-2013_22435, WA_2012-2013_22437, WA_2012-2013_22441,
#      WA_2012-2013_22443, WA_2012-2013_22445, WA_2012-2013_22447, WA_2012-2013_22449, WA_2012-2013_22451,
#      WA_2012-2013_22453, WA_2012-2013_22455, WA_2012-2013_22457, WA_2012-2013_22459, WA_2012-2013_22461,
#      WA_2012-2013_22463, WA_2012-2013_22465, WA_2012-2013_22467, WA_2012-2013_22469, WA_2012-2013_22471,
#      WA_2012-2013_22473, WA_2012-2013_22475, WA_2012-2013_22477, WA_2012-2013_22479, WA_2012-2013_22481,
#      WA_2012-2013_22483, WA_2012-2013_22485, WA_2012-2013_22486

#the part of the df that didn't have NAs for GridID
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 %>% 
  filter(!is.na(GRID5KM_ID))

all_logs_points <- rbind(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_NOGridNA,traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_GridFIXED) %>% 
  #remove the final cases where GridID is NA
  filter(!is.na(GRID5KM_ID)) #33 pots removed, so 0%

#-------------------------------------------------------------
#are there repeating GridIDs with different x/y or AREA

test <- all_logs_points %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(n_dist_AREA = n_distinct(AREA))
#Yes (grids that get broken up by land features)
#easiest fix is just to drop those columns

all_logs_points_v2 <- all_logs_points %>% 
  select(-grd_x, -grd_y, -AREA)

#-------------------------------------------------------------


#aggregate from points to grid level - response variable will be number of pots in grid
#Need to at least adjust for OR 30% data entry, and WA 33% pot reduction
#but also try to adjust for compliance? like ODFW said, using vessel IDs in FishTIx as the total that was out there?


#Bring in Fishtix file

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 


##### FIXING OR 30% DATA ENTRY (plus fix compliance and pot weighting based on pot limits etc at the same time)
#filter fishtix for OR, selected years, DCRB etc
#create column for season based on landing date - remove months 9, 10, 11 for OR
#get distinct season and vessel_num combos
#then join OR license info (this will have repeating rows due to the way OR license data is set up)
#look for cases where pot limit for vessel number varies, and fix those

fishtix_OR_2007_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE == 'O') %>% 
  filter(LANDING_YEAR %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  filter(LANDING_MONTH %in% c(12,1,2,3,4,5,6,7,8)) %>% #OR fishery always closes in August
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, LANDING_MONTH, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_CATCH_AREA_DESCRIPTION, CDFW_AREA_BLOCK, PACFIN_PORT_CODE, PACFIN_PORT_NAME, 
         GEAR_NAME, MARKET_CATEGORY, PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #Troy ODFW: Bay Dungeness crab is 825 and ocean/Columbia River Dungeness crab is 824. same data in 'species code' as well
  filter(MARKET_CATEGORY ==824) %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)") %>% 
  filter(GEAR_NAME %in% c("CRAB POT", "CRAB RING", "FISH POT")) %>% 
  mutate(season_start = ifelse(LANDING_MONTH == 12, LANDING_YEAR, LANDING_YEAR-1)) %>% 
  mutate(season_end = ifelse(LANDING_MONTH == 12, LANDING_YEAR+1, LANDING_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  #don't need seasons we don't have logs for, or that are 100% entered
  filter(season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013',
                       '2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) 

##out of this data an unique FISH_TICKET_ID and PACFIN_CATCH_AREA_DESCRIPTION combos
#0.15% of rows are in PacFIn areas "45 46' N TO 47 20' N; CAPE FALCON TO CAPE ELIZABETH" 
#or PACFIN_CATCH_AREA_DESCRIPTION =="47 20' N TO 220(T.)" -- and only this second one is full yon WA side (0.06% of rows)

#"UNKNOWN PSMFC AREA WITHIN PACIFIC COUNCIL REGION (W-O-C)" and "MULTIPLE AREAS" could be anywhere and not
#really helpful in weighting/scaling pots for cross-border effort



# Read in and join license & pot limit info
OR_pot_limit_info_raw <- read_csv(here::here('wdfw', 'data', 'OR', 'OregonCrabPermitData2007-2020.csv'))

OR_pot_limit_info <- OR_pot_limit_info_raw %>% 
  rename(Vessel = Docnum,
         PermitNumber = Number)

#don't need the dates as in this case don't match using the start and end dates
# OR_pot_limit_info %<>%
#   mutate(Begindate=as.Date(Begindate,"%d/%m/%Y"),#these are different in the 2007-2019 license data file
#          Enddate=as.Date(Enddate,"%d/%m/%Y"))


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



#maybe a simple left join is enough
#PacFin vessel_num seems to match Docnum in OR license data (which was renamed to 'Vessel')
OR_pot_limit_info_v2 <- OR_pot_limit_info %>% 
  select(PermitNumber, Vessel, Potlimit)

#create a df to build a lookup table - left join pot limit info to Fish tickets
test_OR_fishtix_2008_2020_joined_test <- fishtix_OR_2007_2020 %>% 
  left_join(OR_pot_limit_info_v2, by=c("VESSEL_NUM" = "Vessel")) %>%
  filter(!is.na(VESSEL_NUM)) %>% #no NA's
  filter(VESSEL_NUM != "") %>%  #but 6 fishtickets with "" for vessel number
  #find unique VESSEL_NUM & Potlimit combos
  distinct(VESSEL_NUM, Potlimit) 

summary_of_Pot_Limits <- test_OR_fishtix_2008_2020_joined_test %>% 
  filter(!is.na(Potlimit)) %>% 
  group_by(VESSEL_NUM) %>% 
  #find cases where VESSEL_NUM only ever has one unique Potlimit, or multiple different Potlimit
  mutate(distint_pot_lims = n_distinct(Potlimit))
#note that the saved csv was done before filtering for market code and gear type
#write_csv(summary_of_Pot_Limits,here::here('DCRB_sdmTMB', 'data', "summary_of_OR_Pot_Limits_from_FishTix.csv"))
#manually figure out cases where same vessel ID has multiple license numbers
#this continues on line 270


#------------------------

##investigate what % of fishtix in each season didn't find a License and PotLimit
test <- fishtix_OR_2007_2020 %>% 
  distinct(season, FISH_TICKET_ID, VESSEL_NUM)

test_v2 <- test %>% 
  left_join(OR_pot_limit_info_v2, by=c("VESSEL_NUM" = "Vessel"))

summary_test <- test_v2 %>% 
  distinct(season, FISH_TICKET_ID, VESSEL_NUM, PermitNumber, Potlimit) %>% 
  filter(!is.na(VESSEL_NUM)) %>% #no cases
  filter(VESSEL_NUM != "") %>% #6 unique fish tickets with "" for VESSEL_NUM, = 0% of data
  filter(VESSEL_NUM != "MISSING") %>% #2 unique fish tickets
  group_by(season) %>% 
  summarise(n_unique_fishtix = n_distinct(FISH_TICKET_ID))

summary_test_NAs <- test_v2 %>% 
  distinct(season, FISH_TICKET_ID, VESSEL_NUM, PermitNumber, Potlimit) %>% 
  filter(is.na(Potlimit)) %>% 
  filter(!is.na(VESSEL_NUM)) %>% 
  filter(VESSEL_NUM != "") %>% 
  filter(VESSEL_NUM != "MISSING") %>%
  group_by(season) %>% 
  summarise(n_unique_fishtix_NA = n_distinct(FISH_TICKET_ID))

summary_test_v2 <- summary_test %>% 
  left_join(summary_test_NAs, by=c("season")) %>% 
  mutate(percent_NAs = n_unique_fishtix_NA/n_unique_fishtix*100)
#0.01% - 0.7% of Fishtix didn't find a License/PotLimit in different seasons 
#after filtered by market code and gear type
#only few vessels for which all fishtic end up being NA -- 4 vessels that don't match to OR permit data
#517487 (2009-2010)
#OR941JY (2010-2011)
#OR127AEJ (2011-2012, 2012-2013, 2013-2014
#OR67YW (2014-2015) 
#all are 'CDFG block area'
#so few NA vessels that no real difference

#------------------------


#take fishtix, filter into 2 groups based on vessel ids that only ever have 1 pot limit (done abow,csv saved), or more
#make a list of vessel num that are in group 2 (filter based on is, or is not in list)

#list of Vessel_num that have multiple PotLimits - created manually
list_multiple_PotLims <- c("239090","240574","241783","242041","245645","249430","252676","253269","254696", "259524",
                           "260045","260390","274604","280688","281914","290648","295021","296325","503045","507255",
                           "513937","514104","519132","530276","534685","535690","536838","538936","541256","544607",
                           "545806","548600","549776","550191","553717","554987","555742","558619","562157","563679",
                           "563894","567048","572345","574591","578844","580135","581686","589114","590281","593575",
                           "599982","602145","609463","610349","610786","614651","623983","624681","626289","626614",
                           "630351","630696","632162","640718","640904","642576","646009","903821","909150","929612",
                           "976374","978135","988264","995548","1027755","1034163","1187928","AK1114AM","OR348ZC","OR701ZY",
                           "OR908AEZ","OR969YC","605598","977171","OR849YP") 

#group 1
fishtix_OR_2007_2020_single_PotLim <- fishtix_OR_2007_2020 %>% 
  filter(!VESSEL_NUM %in% list_multiple_PotLims)

#group 2
fishtix_OR_2007_2020_multi_PotLim <- fishtix_OR_2007_2020 %>% 
  filter(VESSEL_NUM %in% list_multiple_PotLims)

#read in manually created file of OR PotLims for vessels in FishTix (note that not all vessel nums in fishticket mateched to raw OR permit data)
lookup_table_OR_PotLimits <- read_csv(here::here('DCRB_sdmTMB', 'data', 'lookup_table_OR_vessels_in_FishTix_with_PotLimits.csv')) 


#group 1 - just left join pot limit to fishtix
fishtix_OR_2007_2020_single_PotLim_v2 <- fishtix_OR_2007_2020_single_PotLim %>% 
  left_join(lookup_table_OR_PotLimits, by=c("VESSEL_NUM")) %>% 
  select(-distint_pot_lims, -season.y) %>% 
  rename(season = season.x)
#only this group has Fishtix that didn't find an OR permit and PotLim

#group 2 - look at all seasons for each vessel, manually join pot limit to fish tix based on season
fishtix_OR_2007_2020_multi_PotLim_v2 <- fishtix_OR_2007_2020_multi_PotLim %>% 
  group_by(VESSEL_NUM, season) %>% 
  summarise(n_rows = n())

fishtix_OR_2007_2020_multi_PotLim_v3 <- fishtix_OR_2007_2020_multi_PotLim %>% 
  mutate(
    Potlimit = case_when(
      VESSEL_NUM == "1027755" & season %in% c('2010-2011','2011-2012') ~ 200,
      VESSEL_NUM == "1027755" & !season %in% c('2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "1034163" & season %in% c('2013-2014','2017-2018') ~ 300,
      VESSEL_NUM == "1034163" & !season %in% c('2013-2014','2017-2018') ~ 500,
      VESSEL_NUM == "1187928" & season %in% c('2009-2010','2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "1187928" & !season %in% c('2009-2010','2010-2011','2011-2012') ~ 500,
      VESSEL_NUM == "239090" & season %in% c('2013-2014','2014-2015') ~ 300,
      VESSEL_NUM == "239090" & !season %in% c('2013-2014','2014-2015') ~ 200,
      VESSEL_NUM == "240574" & season %in% c('2007-2008','2008-2009') ~ 200,
      VESSEL_NUM == "240574" & !season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "241783" & season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "241783" & !season %in% c('2007-2008','2008-2009') ~ 200,
      VESSEL_NUM == "242041" & season %in% c('2011-2012','2012-2013') ~ 200,
      VESSEL_NUM == "242041" & !season %in% c('2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "245645" & season %in% c('2015-2016','2016-2017','2017-2018','2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "245645" & !season %in% c('2015-2016','2016-2017','2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "249430" & season %in% c('2010-2011') ~ 300,
      VESSEL_NUM == "249430" & !season %in% c('2010-2011') ~ 500,
      VESSEL_NUM == "252676" & season %in% c('2015-2016','2016-2017') ~ 300,
      VESSEL_NUM == "252676" & !season %in% c('2015-2016','2016-2017') ~ 500,
      VESSEL_NUM == "253269" & season %in% c('2008-2009','2009-2010','2010-2011') ~ 500,
      VESSEL_NUM == "253269" & !season %in% c('2008-2009','2009-2010','2010-2011') ~ 200,
      VESSEL_NUM == "254696" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "254696" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "259524" & season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "259524" & !season %in% c('2007-2008','2008-2009') ~ 200,
      VESSEL_NUM == "260045" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "260045" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "260390" & season %in% c('2013-2014','2014-2015') ~ 200,
      VESSEL_NUM == "260390" & !season %in% c('2013-2014','2014-2015') ~ 300,
      VESSEL_NUM == "274604" & season %in% c('2017-2018','2018-2019') ~ 300,
      VESSEL_NUM == "274604" & !season %in% c('2017-2018','2018-2019') ~ 200,
      VESSEL_NUM == "280688" & season %in% c('2007-2008','2008-2009') ~ 200,
      VESSEL_NUM == "280688" & !season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "281914" & season %in% c('2010-2011','2011-2012') ~ 500,
      VESSEL_NUM == "281914" & !season %in% c('2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "290648" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 500,
      VESSEL_NUM == "290648" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "295021" & season %in% c('2007-2008') ~ 500,
      VESSEL_NUM == "295021" & !season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "296325" & season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "296325" & !season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "503045" & season %in% c('2010-2011','2011-2012','2012-2013','2013-2014') ~ 200,
      VESSEL_NUM == "503045" & !season %in% c('2010-2011','2011-2012','2012-2013','2013-2014') ~ 300,
      VESSEL_NUM == "507255" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "507255" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "513937" & season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "513937" & !season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "514104" & season %in% c('2016-2017') ~ 300,
      VESSEL_NUM == "514104" & !season %in% c('2016-2017') ~ 200,
      VESSEL_NUM == "519132" & season %in% c('2016-2017','2017-2018','2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "519132" & !season %in% c('2016-2017','2017-2018','2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "530276" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "530276" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 200,
      VESSEL_NUM == "534685" & season %in% c('2014-2015','2015-2016') ~ 300,
      VESSEL_NUM == "534685" & !season %in% c('2014-2015','2015-2016') ~ 500,
      VESSEL_NUM == "535690" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "535690" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "536838" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "536838" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "538936" & season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "538936" & !season %in% c('2007-2008') ~ 500,
      VESSEL_NUM == "541256" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 500,
      VESSEL_NUM == "541256" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "544607" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "544607" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "545806" & season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "545806" & !season %in% c('2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "548600" & season %in% c('2019-2020') ~ 200,
      VESSEL_NUM == "548600" & !season %in% c('2019-2020') ~ 300,
      VESSEL_NUM == "549776" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "549776" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "550191" & season %in% c('2009-2010') ~ 500,
      VESSEL_NUM == "550191" & !season %in% c('2009-2010') ~ 300,
      VESSEL_NUM == "553717" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "553717" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "554987" & season %in% c('2015-2016','2016-2017','2017-2018','2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "554987" & !season %in% c('2015-2016','2016-2017','2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "555742" & season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "555742" & !season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "558619" & season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "558619" & !season %in% c('2007-2008') ~ 200,
      VESSEL_NUM == "562157" & season %in% c('2013-2014','2014-2015') ~ 500,
      VESSEL_NUM == "562157" & !season %in% c('2013-2014','2014-2015') ~ 200,
      VESSEL_NUM == "563679" & season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "563679" & !season %in% c('2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "563894" & season %in% c('2008-2009','2009-2010','2010-2011','2011-2012') ~ 200,
      VESSEL_NUM == "563894" & !season %in% c('2008-2009','2009-2010','2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "567048" & season %in% c('2013-2014','2014-2015') ~ 500,
      VESSEL_NUM == "567048" & !season %in% c('2013-2014','2014-2015') ~ 300,
      VESSEL_NUM == "572345" & season %in% c('2015-2016','2016-2017','2017-2018') ~ 300,
      VESSEL_NUM == "572345" & !season %in% c('2015-2016','2016-2017','2017-2018') ~ 200,
      VESSEL_NUM == "574591" & season %in% c('2012-2013','2013-2014') ~ 300,
      VESSEL_NUM == "574591" & !season %in% c('2012-2013','2013-2014') ~ 500,
      VESSEL_NUM == "578844" & season %in% c('2007-2008') ~ 500,
      VESSEL_NUM == "578844" & !season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "580135" & season %in% c('2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "580135" & !season %in% c('2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "581686" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011') ~ 300,
      VESSEL_NUM == "581686" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011') ~ 500,
      VESSEL_NUM == "589114" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "589114" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "590281" & season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "590281" & !season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "593575" & season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "593575" & !season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "599982" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "599982" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "602145" & season %in% c('2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "602145" & !season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "605598" & season %in% c('2012-2013','2013-2014','2014-2015') ~ 200,
      VESSEL_NUM == "605598" & season %in% c('2015-2016','2016-2017') ~ 300,
      VESSEL_NUM == "605598" & season %in% c('2019-2020') ~ 500,
      VESSEL_NUM == "609463" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 200,
      VESSEL_NUM == "609463" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "610349" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "610349" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "610786" & season %in% c('2015-2016','2016-2017') ~ 200,
      VESSEL_NUM == "610786" & !season %in% c('2015-2016','2016-2017') ~ 300,
      VESSEL_NUM == "614651" & season %in% c('2012-2013','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018') ~ 300,
      VESSEL_NUM == "614651" & !season %in% c('2012-2013','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018') ~ 500,
      VESSEL_NUM == "623983" & season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "623983" & !season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "624681" & season %in% c('2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "624681" & !season %in% c('2011-2012','2012-2013') ~ 500,
      VESSEL_NUM == "626289" & season %in% c('2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "626289" & !season %in% c('2010-2011','2011-2012') ~ 500,
      VESSEL_NUM == "626614" & season %in% c('2019-2020') ~ 300,
      VESSEL_NUM == "626614" & !season %in% c('2019-2020') ~ 200,
      VESSEL_NUM == "630351" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "630351" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "630696" & season %in% c('2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "630696" & !season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "632162" & season %in% c('2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "632162" & !season %in% c('2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "640718" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "640718" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "640904" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "640904" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "642576" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 200,
      VESSEL_NUM == "642576" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 300,
      VESSEL_NUM == "646009" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013','2013-2014') ~ 500,
      VESSEL_NUM == "646009" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012','2012-2013','2013-2014') ~ 300,
      VESSEL_NUM == "903821" & season %in% c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018') ~ 300,
      VESSEL_NUM == "903821" & !season %in% c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018') ~ 500,
      VESSEL_NUM == "909150" & season %in% c('2007-2008','2008-2009') ~ 200,
      VESSEL_NUM == "909150" & !season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "929612" & season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "929612" & !season %in% c('2007-2008') ~ 500,
      VESSEL_NUM == "976374" & season %in% c('2007-2008','2008-2009') ~ 300,
      VESSEL_NUM == "976374" & !season %in% c('2007-2008','2008-2009') ~ 500,
      VESSEL_NUM == "977171" & season %in% c('2007-2008') ~ 200,
      VESSEL_NUM == "977171" & season %in% c('2008-2009','2009-2010','2010-2011') ~ 300,
      VESSEL_NUM == "977171" & season %in% c('2011-2012','2012-2013','2013-2014','2014-2015','2015-2016','2016-2017') ~ 500,
      VESSEL_NUM == "978135" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 200,
      VESSEL_NUM == "978135" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "988264" & season %in% c('2013-2014','2014-2015','2015-2016','2016-2017') ~ 300,
      VESSEL_NUM == "988264" & !season %in% c('2013-2014','2014-2015','2015-2016','2016-2017') ~ 200,
      VESSEL_NUM == "995548" & season %in% c('2010-2011','2011-2012','2012-2013') ~ 300,
      VESSEL_NUM == "995548" & !season %in% c('2010-2011','2011-2012','2012-2013') ~ 500,
      VESSEL_NUM == "AK1114AM" & season %in% c('2015-2016','2016-2017') ~ 300,
      VESSEL_NUM == "AK1114AM" & !season %in% c('2015-2016','2016-2017') ~ 500,
      VESSEL_NUM == "OR348ZC" & season %in% c('2007-2008') ~ 200,
      VESSEL_NUM == "OR348ZC" & !season %in% c('2007-2008') ~ 300,
      VESSEL_NUM == "OR701ZY" & season %in% c('2017-2018','2018-2019','2019-2020') ~ 500,
      VESSEL_NUM == "OR701ZY" & !season %in% c('2017-2018','2018-2019','2019-2020') ~ 300,
      VESSEL_NUM == "OR849YP" & season %in% c('2007-2008','2008-2009','2009-2010') ~ 500,
      VESSEL_NUM == "OR849YP" & !season %in% c('2007-2008','2008-2009','2009-2010') ~ 200,
      VESSEL_NUM == "OR908AEZ" & season %in% c('2019-2020') ~ 500,
      VESSEL_NUM == "OR908AEZ" & !season %in% c('2019-2020') ~ 300,
      VESSEL_NUM == "OR969YC" & season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012') ~ 300,
      VESSEL_NUM == "OR969YC" & !season %in% c('2007-2008','2008-2009','2009-2010','2010-2011','2011-2012') ~ 200,
    ))
#check for NAs, and duplication etc -- no NAs, no row duplication 

fishtix_OR_2007_2020_with_PotLim <- rbind(fishtix_OR_2007_2020_single_PotLim_v2,fishtix_OR_2007_2020_multi_PotLim_v3)
#nrow(fishtix_OR_2007_2020_with_PotLim)
#nrow(fishtix_OR_2007_2020) #number of rows still same as original data


#-------------------------------------------
## OR
#calculate the max number of lines in water in half-month steps
#based on unique vessels that participated in that time step and their pot limits - landed in OR in Pacfin
#--> we might lose a little something due to cross border effort...
#also pot limit might be diferent if have dual license, but we can't really do much about that

fishtix_OR_2007_2020_with_PotLim_v2 <- fishtix_OR_2007_2020_with_PotLim %>% 
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_MONTH, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>%
  select(-d, -period, -month_name) %>% 
  group_by(season, half_month) %>% 
  distinct(VESSEL_NUM, Potlimit) %>% 
  filter(!is.na(Potlimit)) %>% 
  summarise(total_pots = sum(Potlimit))


##test with all data in OR side
##use the pots data (all_logs_points_v2) where GridIDs have been fixed
#############THIS IS WHERE NEED TO FIX LANDING DATE VS SET DATE

#need to specify half-month using landing date, not set date
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


#join Fisticket1 into data
OR_logs_fishticket1 <- all_logs_points_v2 %>%
  #filter just to be OR logs
  filter(Landing_logbook_state == 'OR') %>% 
  left_join(raw_OR_logs_selected_columns_v4, by="SetID") %>% 
  #drop columns not needed
  select(-(FishTicket3:n_char_tix2))
#ALL data find a Fishticket1 - noNAs
#each SetID associated with only 1 FishTicket1

fishtix_landing_date_only <- fishtix_OR_2007_2020_with_PotLim %>% 
  select(FTID, LANDING_DATE) %>% 
  distinct() #each FTID associated with only 1 landing date

#join landing date via PacFIn FishTIx
OR_logs_fishticket1_with_landing_date <- OR_logs_fishticket1 %>% 
  left_join(fishtix_landing_date_only, by = c("FishTicket1" = "FTID")) %>% 
  filter(!is.na(LANDING_DATE))
#number of unique SetIDs and data rows are still the same
#but some cases on NA landing date, 8 unique Fishtickets. Those don';'t seem to exists in PacFin - drop them

#here note that half-month is calcualted based on landing date to match PacFIn half-months
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 <- OR_logs_fishticket1_with_landing_date %>%  
  #filter(Landing_logbook_state == 'OR') %>% 
  #create one half-month label based on SetID (this is to match other predictor variable dfs)
  rename(month_name_SetID = month_name ) %>% 
  mutate(d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month_SetID = paste0(month_name_SetID,"_",period)) %>% 
  select(-d, -period) %>% 
  #create another half-month to only be used to link with PacFIn max pot counts
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_DATE, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>% 
  select(-d, -period)
glimpse(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2)



# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>% 
#   filter(!is.na(GRID5KM_ID)) %>% #this is actually already done
#   # count up all traps in 2-week period (not by indv vessel)
#   group_by(season, half_month) %>%  
#   summarise(
#     n_traps_all=n(), na.rm=TRUE
#   ) %>% 
#   #left join to get max pots in that time step
#   left_join(fishtix_OR_2007_2020_with_PotLim_v2, by=c("season", "half_month")) %>% 
#   # create a column with weighting - proportion of max allowed traps
#   mutate(trap_limit_weight = total_pots/n_traps_all) %>% #now weight for pot limit
#   ungroup()
# #here effectively find a scaler for each half-month step
# #each pot in this half-month step is worth x pots
# 
# 
# # join the "weighting key" back to the simulated pots data
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v3_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>% 
#   left_join(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2_XX,by=c('season', 'half_month'))
# 
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v4_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v3_XX %>% 
#   group_by(season, half_month, GRID5KM_ID) %>%  
#   # this is the new/key step -- weighted_traps 
#   summarise(tottraps=sum(trap_limit_weight)) 
# glimpse(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v4_XX) 




###do Troy's 'double dip' of pot limits way

#sum pot limits for all that logged in a given time step
#max lines in water based on logbooks (and vessel pot limits)
double_dipping <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>% 
  #here need to drop cases where OR pot limit = NA
    filter(!is.na(OR_Pot_Limit)) %>% 
     group_by(season, half_month) %>% 
     distinct(Vessel, OR_Pot_Limit) %>% 
     filter(!is.na(OR_Pot_Limit)) %>% 
     summarise(total_pots_logs = sum(OR_Pot_Limit))

#join to what is the max lines in water based on Fish tickets
double_dipping_v2 <- double_dipping %>% left_join(fishtix_OR_2007_2020_with_PotLim_v2)

#what is the ration between sum of pot limits from FIsh tickets
#and sum of pot limtis from logs
double_dipping_v3 <- double_dipping_v2 %>% mutate(ratio = total_pots/total_pots_logs)


#actually don't think this is the most correct way
#join the ratio value to OR logs (each pot is a row/point)
#traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v3_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>% 
#     left_join(double_dipping_v3,by=c('season', 'half_month'))

#double_dipping_final <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v3_XX %>% 
#     group_by(season, half_month, GRID5KM_ID) %>%  
#     # this is the new/key step -- weighted_traps 
#     summarise(tottraps=sum(ratio)) 


#instead do 'double dipping' weighting this way:
#weight logged pots by each vessel's pot limit
OR_weighted_pots_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>%
  filter(!is.na(GRID5KM_ID)) %>% #this is actually already done
  #here need to drop cases where OR pot limit = NA
  filter(!is.na(OR_Pot_Limit)) %>% 
  # count up all traps in 2-week period 
  group_by(season, half_month, Vessel, OR_PermitNumber, OR_Pot_Limit) %>%
  summarise(
    n_traps_all=n(), na.rm=TRUE
  ) %>%
  # create a column with weighting - proportion of max allowed traps
  mutate(trap_limit_weight = OR_Pot_Limit/n_traps_all) %>% #now weight for pot limit
  ungroup()
#here effectively find a scaler for each half-month step
#each pot in this half-month step is worth x pots

# join the "weighting key" back to the simulated pots data
OR_weighted_pots_XX2 <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v2 %>% 
  #here need to drop cases where OR pot limit = NA
  filter(!is.na(OR_Pot_Limit)) %>% 
  left_join(OR_weighted_pots_XX,by=c('season', 'half_month','Vessel','OR_PermitNumber','OR_Pot_Limit'))

OR_weighted_pots_XX3 <- OR_weighted_pots_XX2 %>% 
  group_by(season, half_month, GRID5KM_ID) %>%  
  # sum weighted_traps in each grid and half-month
  #so far weighted by pot limits based on who logged data
  summarise(tottraps=sum(trap_limit_weight)) #%>%  
glimpse(OR_weighted_pots_XX3) 

#'double dip' - weight the weighted pots by the ratio of max lines in water 
#between fishtickets and logbooks
OR_weighted_pots_XX4 <- OR_weighted_pots_XX3 %>% 
  left_join(double_dipping_v3,by=c('season', 'half_month')) %>% 
  mutate(tottraps_FINAL = tottraps * ratio) %>% 
  select(-(tottraps:ratio))
glimpse(OR_weighted_pots_XX4)




#Now do the same with WA and sum weighted pot number in grids








#------------------------------------
#------------------------------------
#I think this stuff is bit unnecessary, it was just a check to see if catch landed in OR
#came form CA (i.e., labelled as having a CA block area)

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
#although also couple cases where CDFW_AREA_BLOCK is NA, or 0 but agency is C 
#looks like all logbooked landings were to OR or WA ports (distance_to_ports step script)
#but there could be landings to OR ports that were in CA waters?

#so maybe fine to filter PacFIn fishtickets to W and O data, and just make sure that CDFW_AREA_BLOCK is NA 
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
#------------------------------------
#------------------------------------------------------------------------




##repeat  step on WA landings
#filter fishtix for WA, selected years, DCRB etc
#create column for season based on landing date - remove months 10, 11 for WA
#get distinct season and vessel_num combos
#then join WA license info 
#look for cases where pot limit for vessel number varies, and fix those

fishtix_WA_2010_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE == 'W') %>% 
  filter(LANDING_YEAR %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  filter(LANDING_MONTH %in% c(12,1,2,3,4,5,6,7,8,9)) %>% #WA fishery always closes in September
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, FTID, VESSEL_NUM, LANDING_DATE,  LANDING_YEAR, LANDING_MONTH, NUM_OF_DAYS_FISHED, AGENCY_CODE,
         VESSEL_REGISTRATION_ID, VESSEL_ID, VESSEL_NUM, FISHER_LICENSE_NUM, 
         PACFIN_SPECIES_CODE,  NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, CATCH_AREA_CODE, PACFIN_CATCH_AREA_DESCRIPTION, CDFW_AREA_BLOCK, PACFIN_PORT_CODE, PACFIN_PORT_NAME, GEAR_NAME,
         PRICE_PER_POUND, EXVESSEL_REVENUE, AFI_PRICE_PER_POUND, AFI_EXVESSEL_REVENUE, LANDED_WEIGHT_LBS, REMOVAL_TYPE_NAME, PARTICIPATION_GROUP_NAME) %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% 
  #filter out personal catch and research catch here
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)" | REMOVAL_TYPE_NAME == "COMMERCIAL(DIRECT SALES)") %>% 
  #filter out catch that is from Puget Sound as not part of WA coastal catches
  #and as those vessels won't appear in wA coastal fishery permit data
  filter(PACFIN_CATCH_AREA_DESCRIPTION != 'PUGET SOUND') %>% 
  #also filter out area 61
  filter(CATCH_AREA_CODE != "61") %>% 
  #if gear name/code is trawl, then likely crab caught in non commercial DCRB fishery (i.e. was bycatch but was sold)
  #this was chatted with Troy (ODFW) re: OR data
  filter(GEAR_NAME %in% c("SHELLFISH POT (CRAB)", "SHELLFISH POT (NON-CRAB)", "BOTTOMFISH POT")) %>% 
  #also take out catch labelled as treaty Indian fisher
  filter(PARTICIPATION_GROUP_NAME != "TREATY INDIAN COMMERCIAL FISHER") %>% 
  mutate(season_start = ifelse(LANDING_MONTH == 12, LANDING_YEAR, LANDING_YEAR-1)) %>% 
  mutate(season_end = ifelse(LANDING_MONTH == 12, LANDING_YEAR+1, LANDING_YEAR)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  select(-season_start, -season_end) %>% 
  #don't need seasons we don't have logs for, or that are 100% entered
  filter(season %in% c('2009-2010','2010-2011','2011-2012','2012-2013',
                       '2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')) 



#based on FISHER_LICENSE_NUM column try join WA pot limit, and if NAs then look into those

WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))
WA_pot_limit_info$License_ID <- as.character(WA_pot_limit_info$License_ID)

fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM <- fishtix_WA_2010_2020 %>% 
  left_join(WA_pot_limit_info, by = c("FISHER_LICENSE_NUM" = "License_ID"))

#those fishtix that found a WA pot limit
fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_OK <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  filter(!is.na(Pot_Limit))
nrow(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_OK) / nrow(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM) *100
#89.6% of rows (which may not be the same as unique fishtickets, as those sometimes repeat in PacFin data)
#88.2% if filter out area 61, 88.3% after filter for gear type, 99.99 after remove "TREATY INDIAN COMMERCIAL FISHER"

#this hasn't been updated for new filters
#same, but broken down by season:
# n_row_no_NAs <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_OK %>% #note that this df is created below
#   distinct(FISH_TICKET_ID, season) %>% 
#   group_by(season) %>% 
#   summarise(n_tix_no_NA = n())
# 
# n_row_NAs <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs %>% 
#   distinct(FISH_TICKET_ID, season) %>% 
#   group_by(season) %>% 
#   summarise(n_tix_NA = n())
# 
# summary_table <- n_row_no_NAs %>% 
#   left_join(n_row_NAs, by=c("season")) %>% 
#   mutate(percent_match = n_tix_no_NA/(n_tix_no_NA+n_tix_NA))

# season    n_tix_no_NA n_tix_NA percent_match
# 2009-2010        4890      684         0.877
# 2010-2011        5371      744         0.878
# 2011-2012        4690      524         0.900
# 2012-2013        4919      650         0.883
# 2013-2014        4315      553         0.886
# 2014-2015        3830      397         0.906
# 2015-2016        4044      353         0.920
# 2016-2017        4594      510         0.900
# 2017-2018        4220      647         0.867
# 2018-2019        4363      566         0.885
# 2019-2020        4077      545         0.882

fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  filter(is.na(Pot_Limit))
nrow(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs) / nrow(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM) *100
#10.4% of rows; 
#11.8% if filter out area 61. 11.7% after filter for gear type, 0.0085 after remove "TREATY INDIAN COMMERCIAL FISHER"
#not updated for new filters:
#length(unique(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs$FISH_TICKET_ID)) #6173 unique Fish ticket IDs
#nrow(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs) # but 250 rows of data
#length(unique(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs$VESSEL_NUM)) #12 unique vESSSEL_NUM,  including "MISSING" and "UNKNOWN"

unique_WA_vessels_NAs <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_NAs %>% 
  distinct(VESSEL_NUM, VESSEL_ID, VESSEL_REGISTRATION_ID, FISHER_LICENSE_NUM)
#lot of there are cases with Vessel_num = 'unknown'
#only 10 unique vessel_nums that didn't find potlimit
#only missing/unknown once added new filters. only 3 unique combos after remove "TREATY INDIAN COMMERCIAL FISHER"


##investigate what % of fishtix in each season didn't find a PotLimit
xx_test <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  distinct(FISH_TICKET_ID, VESSEL_NUM)
nrow(xx_test %>% filter(VESSEL_NUM == 'UNKNOWN' | VESSEL_NUM == 'MISSING')) / nrow(xx_test) *100
#11.1% of FIshtickets don't have VESSEL_NUM / 12.2 after new filters, 0.009 after remove "TREATY INDIAN COMMERCIAL FISHER"

test <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  distinct(season, FISH_TICKET_ID, VESSEL_NUM, FISHER_LICENSE_NUM, Pot_Limit) 

test_v2 <- test %>% 
  group_by(season) %>% 
  summarise(n_unique_fishtix = n_distinct(FISH_TICKET_ID))

summary_test_NAs <- test %>% 
  distinct(season, FISH_TICKET_ID, VESSEL_NUM, FISHER_LICENSE_NUM, Pot_Limit) %>% 
  filter(is.na(Pot_Limit)) %>% 
  filter(!is.na(VESSEL_NUM)) %>% 
  filter(!VESSEL_NUM %in% c("", "MISSING", "UNKNOWN")) %>% 
  group_by(season) %>% 
  summarise(n_unique_fishtix_NA = n_distinct(FISH_TICKET_ID))

summary_test_v2 <- summary_test %>% 
  left_join(summary_test_NAs, by=c("season")) %>% 
  mutate(percent_NAs = n_unique_fishtix_NA/n_unique_fishtix*100)
#0.11% - 2.29% of Fishtix didn't find a PotLimit in different seasons 
#Also there is a large % of fishtix with no vessel_num recorded
#after new filters only "unknown" and "missing' vessel_num cases left, so table is only NAs

#--------------------------------------------
## WA
#calculate the max number of lines in water in half-month steps
#based on unique vessels that participated in that time step and their pot limits -- landed in WA in PacFin
#--> we might lose a little something due to cross border effort...
#also pot limit might be diferent if have dual license, but we can't really do much about that


fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_PotLim <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_MONTH, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>%
  select(-d, -period, -month_name) %>% 
  group_by(season, half_month) %>% 
  distinct(VESSEL_NUM, Pot_Limit) %>% 
  filter(!is.na(Pot_Limit)) %>% 
  summarise(total_pots = sum(Pot_Limit))



##test with all data in WA side
##use the pots data (all_logs_points_v2) where GridIDs have been fixed
#############THIS IS WHERE NEED TO FIX LANDING DATE VS SET DATE

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
WA_logs_fishticket1 <-  all_logs_points_v2 %>%
  #filter just to be WA logs
  filter(Landing_logbook_state == 'WA') %>% 
  left_join(raw_WA_logs_selected_columns, by="SetID")
#each SetID associated with only 1 FishTicket1

#not all data find a Fishticket1
has_NAs <- WA_logs_fishticket1 %>% filter(is.na(FishTicket1))
unique(has_NAs$SetID) #1450 SetIDs
summary_table <- has_NAs %>% group_by(Pot_State) %>% summarise(n_distinct_SetID = n_distinct(SetID))
#Pot_State    n_distinct_SetID
#OR                 35
#WA               1436
nrow(has_NAs)/nrow(WA_logs_fishticket1)*100 ##0.9% of pots/of WA logs didn't have a Fishticket number recorded in logbooks to be matched to PacFin tickets



fishtix_landing_date_only <- fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM %>% 
  select(FTID, LANDING_DATE) %>% 
  distinct() #each FTID associated with only 1 landing date

#join landing date via PacFIn FishTIx
WA_logs_fishticket1_with_landing_date <- WA_logs_fishticket1 %>% 
  left_join(fishtix_landing_date_only, by = c("FishTicket1" = "FTID")) %>% 
  filter(!is.na(LANDING_DATE))
#quite a lot of NAs, sometimes they have Fishticket1 and sometimes they don't
#do a mix of half-month created based on landing_date, or by set date if no landing date available


#here note that half-month is calculated based on landing date to match PacFIn half-months
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA <- WA_logs_fishticket1_with_landing_date %>%  
  #filter(Landing_logbook_state == 'WA') %>% 
  #create one half-month label based on SetID (this is to match other predictor variable dfs)
  #there are some cases where SetDate is NA< which is not useful - we'll drop these later
  #filter(!is.na(SetDate)) %>% 
  rename(month_name_SetID = month_name ) %>% 
  mutate(d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(half_month_SetID = paste0(month_name_SetID,"_",period)) %>% 
  select(-d, -period) %>% 
  #create another half-month to only be used to link with PacFIn max pot counts
  mutate(d=day(LANDING_DATE),period=ifelse(d<=15,1,2)) %>% 
  mutate(month_name = month(LANDING_DATE, label=TRUE, abbr=FALSE)) %>% 
  mutate(half_month = paste0(month_name,"_",period)) %>% 
  select(-d, -period)
glimpse(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA)






#---------- old way before 'double dipping'------------
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v2_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA %>% 
#   filter(!is.na(GRID5KM_ID)) %>% 
#   # count up all traps in 2-week period (not by indv vessel)
#   group_by(season, half_month) %>%  
#   summarise(
#     n_traps_all=n(), na.rm=TRUE
#   ) %>% 
#   #left join to get max pots ib that time step
#   left_join(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_PotLim, by=c("season", "half_month")) %>% 
#   # create a column with weighting - proportion of max allowed traps
#   mutate(trap_limit_weight = total_pots/n_traps_all) %>% #now weight for pot limit
#   ungroup()
# #here effectively find a scaler for each half-month step
# #each pot in this half-month step is worth x pots
# 
# 
# # join the "weighting key" back to the simulated pots data
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v3_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA %>% 
#   left_join(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v2_XX,by=c('season', 'half_month'))
# 
# #this is where also adjust for WA 33% summer reduction?
# # traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v3_XX %>% 
# #   group_by(season, half_month, GRID5KM_ID) %>%  
# #   # this is the new/key step -- weighted_traps 
# #   summarise(tottraps=sum(trap_limit_weight)) 
# # glimpse(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_XX) 
#--------------------------

#sum pot limits for all that logged in a given time step
#max lines in water based on logbooks (and vessel pot limits)
WA_double_dipping <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA %>% 
  group_by(season, half_month) %>% 
  distinct(Vessel, WA_Pot_Limit) %>% 
  filter(!is.na(WA_Pot_Limit)) %>% 
  summarise(total_pots_logs = sum(WA_Pot_Limit))

#join to what is the max lines in water based on Fish tickets
WA_double_dipping_v2 <- WA_double_dipping %>% left_join(fishtix_WA_2010_2020_join_by_FISHER_LICENSE_NUM_PotLim)

#what is the ration between sum of pot limits from FIsh tickets
#and sum of pot limtis from logs
WA_double_dipping_v3 <- WA_double_dipping_v2 %>% mutate(ratio = total_pots/total_pots_logs)


#instead do 'double dipping' weighting this way:
#weight logged pots by each vessel's pot limit
WA_weighted_pots_XX <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA %>%
  filter(!is.na(GRID5KM_ID)) %>% #this is actually already done
  # count up all traps in 2-week period 
  #here need to drop cases where WA pot limit = NA
  filter(!is.na(WA_Pot_Limit)) %>% 
  group_by(season, half_month, Vessel, WA_License, WA_Pot_Limit) %>%
  summarise(
    n_traps_all=n(), na.rm=TRUE
  ) %>%
  # create a column with weighting - proportion of max allowed traps
  mutate(trap_limit_weight = WA_Pot_Limit/n_traps_all) %>% #now weight for pot limit
  ungroup()
#here effectively find a scaler for each half-month step
#each pot in this half-month step is worth x pots

# join the "weighting key" back to the simulated pots data
WA_weighted_pots_XX2 <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA %>% 
  #here need to drop cases where WA pot limit = NA
  filter(!is.na(WA_Pot_Limit)) %>% 
  left_join(WA_weighted_pots_XX,by=c('season', 'half_month','Vessel','WA_License','WA_Pot_Limit'))

WA_weighted_pots_XX3_no_reduction <- WA_weighted_pots_XX2 %>% 
  filter(!(Pot_State == "WA" & season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                                                   '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))) %>% 
  group_by(season, half_month, GRID5KM_ID) %>%  
  # sum weighted_traps in each grid and half-month
  #so far weighted by pot limits based on who logged data
  summarise(tottraps=sum(trap_limit_weight)) #%>%  
glimpse(WA_weighted_pots_XX3_no_reduction) 

WA_weighted_pots_XX3_with_reduction <- WA_weighted_pots_XX2 %>% 
  #only applies in WA waters
  filter(Pot_State == "WA" & season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                                                 '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September')) %>% 
  #if data in WA waters, and within of Jul-Sep of 2018-2019 and May-Sep 2019-2020, 33% pot reduction
  group_by(season, half_month, GRID5KM_ID) %>%  
  # sum weighted_traps in each grid and half-month
  #so far weighted by pot limits based on who logged data
  summarise(tottraps=sum(trap_limit_weight*0.66))
glimpse(WA_weighted_pots_XX3_with_reduction) 

#'double dip' - weight the weighted pots by the ratio of max lines in water 
#between fishtickets and logbooks
WA_weighted_pots_XX4 <- rbind(WA_weighted_pots_XX3_no_reduction,WA_weighted_pots_XX3_with_reduction) %>% 
  left_join(WA_double_dipping_v3,by=c('season', 'half_month')) %>% 
  mutate(tottraps_FINAL = tottraps * ratio) %>% 
  select(-(tottraps:ratio))









##test for adjusting for summer 33%

#if no 33% reduction
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_no_reduction <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v3_XX %>% 
#   filter(!(Pot_State == "WA" & season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
#                                                    '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))) %>% 
#   #if data in OR waters, or outside of Jul-Sep of 2018-2019 and May-Sep 2019-2020, not pot reduction
#   group_by(season, half_month, GRID5KM_ID) %>%  
#   # this is the new/key step -- weighted_traps 
#   summarise(tottraps=sum(trap_limit_weight)) 
# 
# #if  33% reduction
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_with_reduction <- traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v3_XX %>% 
#   #only applies in WA waters
#   filter(Pot_State == "WA" & season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
#          '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September')) %>% 
#   #if data in WA waters, and within of Jul-Sep of 2018-2019 and May-Sep 2019-2020, 33% pot reduction
#   group_by(season, half_month, GRID5KM_ID) %>%  
#   # this is the new/key step -- weighted_traps 
#   summarise(tottraps=sum(trap_limit_weight*0.66))
# 
# traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_XX <- rbind(traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_no_reduction,traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_with_reduction)






#---------------------------------

#now sum weighted pots across OR and WA data 
#need to make sure column names match 
#or jsut join to a df of all grid and time step combos?
#WA_weighted_pots_XX4 (was: traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_WA_v4_XX before 'double dipping')
#OR_weighted_pots_XX4 (was: traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020_v4_XX before 'double dipping')

WA_data <- WA_weighted_pots_XX4 %>% 
  rename(tottraps_WA_data = tottraps_FINAL)

OR_data <- OR_weighted_pots_XX4 %>% 
  rename(tottraps_OR_data = tottraps_FINAL)



study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed.rds"))
#grids that were in pieces and had repeating gridID have been fixed
#drop the exisitng predictors, so have one df with just the response variable
study_area_grids_with_all_season_halfmonth_combos_df <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed %>% 
  select(-SST_avg, -wind_avg) %>% 
  #join tottraps data
  left_join(WA_data, by=c('season', 'half_month','GRID5KM_ID')) %>% 
  left_join(OR_data, by=c('season', 'half_month','GRID5KM_ID')) %>% 
  rowwise() %>% 
  #for now, if both datasets had NA, make number of pots in grid be 0
  #later will fix those that ned to be NA (will remove grids that were closed)
  mutate(tottraps = sum(tottraps_WA_data, tottraps_OR_data, na.rm = TRUE))

#save df
#write_rds(study_area_grids_with_all_season_halfmonth_combos_df,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_response_var.rds"))












#----------------------------------
#----------------------------------
#----------------------------------
#this is about testing couple ways of doing 'my way' for fixing OR 30% entry -- but don't think its correct after all

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
#----------------------------------

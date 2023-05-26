#fishery persistence


#-------------------------------------------------------------------------------------------------

library(here)
library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)
library(plotmo)
library(viridis)
library(ggridges)


#-------------------------------------------------------------------------------------------------

###WINTER

#-------------------------------------------------------------------------------------------------


winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230324.rds'))
glimpse(winter) 

winter_persistence <- winter %>% filter(tottraps>0) %>%  group_by(GRID5KM_ID) %>% summarise(n_seasons = n_distinct(season))

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))

winter_persistence_sf <- study_area %>%  left_join(winter_persistence, by=c('GRID5KM_ID'))

#Replace na values with 0 
winter_persistence_sf[is.na(winter_persistence_sf)] = 0

#n_seasons grouping
winter_persistence_sf <- winter_persistence_sf %>% 
  mutate(n_seasons_grouping = case_when(n_seasons == 0 ~ '0', 
                                        n_seasons %in% c(1,2) ~ '1-2', 
                                        n_seasons %in% c(3,4) ~ '3-4',                                        
                                        n_seasons %in% c(5,6) ~ '5-6',
                                        n_seasons %in% c(7,8) ~ '7-8', 
                                        n_seasons %in% c(9,10) ~ '9-10',
                                        n_seasons == 11 ~ '11'))


# #export shapefile for QGIS
# #st_write(winter_persistence_sf, "winter_persistence_sf.shp")


#-------------------------------------------------------------------------------------------------

###SUMMER

#-------------------------------------------------------------------------------------------------

summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230324.rds'))
glimpse(summer) 

summer_persistence <- summer %>% filter(tottraps>0) %>%  group_by(GRID5KM_ID) %>% summarise(n_seasons = n_distinct(season))

study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))

summer_persistence_sf <- study_area %>%  left_join(summer_persistence, by=c('GRID5KM_ID'))

#Replace na values with 0 
summer_persistence_sf[is.na(summer_persistence_sf)] = 0

summer_persistence_sf <- summer_persistence_sf %>% 
  mutate(n_seasons_grouping = case_when(n_seasons == 0 ~ '0', 
                                        n_seasons %in% c(1,2) ~ '1-2', 
                                        n_seasons %in% c(3,4) ~ '3-4',                                        
                                        n_seasons %in% c(5,6) ~ '5-6',
                                        n_seasons %in% c(7,8) ~ '7-8', 
                                        n_seasons %in% c(9,10) ~ '9-10',
                                        n_seasons == 11 ~ '11'))

# #export shapefile for QGIS
# #st_write(summer_persistence_sf, "summer_persistence_sf.shp")


p <- ggplot(summer_persistence_sf, aes(factor(n_seasons_grouping))) + geom_bar() 
p


#-------------------------------------------------------------------------------------------------
#check confidentiality for figure 2

#need df where pots are points
traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020 <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))

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


#don't need the 2 first OR seasons
all_logs_points  <- all_logs_points %>% filter(!season %in% c('2007-2008', '2008-2009'))

#assign winter vs summer
all_logs_points_winter_summer <- all_logs_points %>% 
  mutate(winter_summer = ifelse(month_name %in% c('December', 'January', 'February', 'March', 'April'), 'winter','summer')) %>% 
  #the raw logs had few cases with no month name
  filter(!is.na(month_name))

#separate dfs for winter and summer
all_logs_points_winter <- all_logs_points_winter_summer %>% filter(winter_summer =="winter")
all_logs_points_summer <- all_logs_points_winter_summer %>% filter(winter_summer =="summer")


##check for confidentiality
#early WA data are the ones with no actual vessel ID, but still works
confidentiality_winter_persistence_sf <- all_logs_points_winter %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(n_unique_vessels = n_distinct(Vessel)) %>% 
  mutate(confidential=ifelse(n_unique_vessels < 3, 'Y', 'N'))

winter_persistence_sf_conf <- winter_persistence_sf %>% 
  left_join(confidentiality_winter_persistence_sf, by=c('GRID5KM_ID')) %>% 
  #all cases of NA for confidentiality label were grids with 0 seasons used, so label them as N
  mutate(confidential = ifelse(n_seasons == 0,'N', confidential))

#export shapefile for QGIS
#st_write(winter_persistence_sf_conf, "winter_persistence_sf_conf.shp")


confidentiality_summer_persistence_sf <- all_logs_points_summer %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(n_unique_vessels = n_distinct(Vessel)) %>% 
  mutate(confidential=ifelse(n_unique_vessels < 3, 'Y', 'N'))

summer_persistence_sf_conf <- summer_persistence_sf %>% 
  left_join(confidentiality_summer_persistence_sf, by=c('GRID5KM_ID')) %>% 
  #all cases of NA for confidentiality label were grids with 0 seasons used, so label them as N
  mutate(confidential = ifelse(n_seasons == 0,'N', confidential)) %>% 
  mutate(confidential = case_when(is.na(confidential) & n_seasons == 4 ~ 'N', 
                                  is.na(confidential) & n_seasons < 4 ~ 'Y',
                                  !is.na(confidential) ~ confidential))

#export shapefile for QGIS
#st_write(summer_persistence_sf_conf, "summer_persistence_sf_conf.shp")







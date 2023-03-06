# predictor variable: bathymetry

#there is a lot of unnecessary code here as originally compared the depth values based on
#GEBCO depth extracted to pot points, zonal statistic used on GEBCO depth and study area grids,
#and the original depth layer from Blake which didn't have real depth for ports and embayments
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

# bathymetry extracted in QGIS using zonal statistics, from gebco bathymetry - blake depth layer missing port/embayment depths
path_bathy_lamb <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/data files/study_area_depth_lamb_azi.shp"
bathy_lamb <- st_read(path_bathy_lamb, quiet = TRUE) 
#file has some >0m values, all really close to coast/bays
# --> I think make call that if the value used, e.g. median >0m, make it -1


#all processed logbook data as points
all_logs_points <- read_rds(here::here('DCRB_sdmTMB', 'data','traps_g_ALL_WA_2010_2020_and_ALL_OR_2008_2020.rds'))


#extract gebco depth to points
bathy <- raster(here::here('DCRB_sdmTMB','data', 'gebco bathy','gebco_2022_n49.5703_s35.2617_w-126.29_e-119.6104.tif'))

all_logs_points_sf <- all_logs_points %>%
  st_as_sf(coords=c('point_x','point_y'),crs=st_crs(bathy_lamb)) %>% 
  #st_transform(4326) 
  st_transform(st_crs(bathy))


# do the raster extract with the bathymetry grid
bathy_points <- raster::extract(bathy,all_logs_points_sf)

# add depth as a column variable
all_logs_points_sf_GEBCObathy <- all_logs_points_sf %>% 
  mutate(depth_gebco=bathy_points)

#save the sf version
#write_rds(all_logs_points_sf_GEBCObathy,here::here('DCRB_sdmTMB', 'data', "all_logs_points_GEBCObathy_sf.rds"))
#all_logs_points_sf_GEBCObathy <- read_rds(here::here('DCRB_sdmTMB', 'data','all_logs_points_GEBCObathy_sf.rds'))


#first need to drop geometry if want to eg plot the two depth values against each other
all_logs_points_sf_GEBCObathy_NOgeom <- all_logs_points_sf_GEBCObathy %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
              lat = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)
#write_rds(all_logs_points_sf_GEBCObathy_NOgeom,here::here('DCRB_sdmTMB', 'data', "all_logs_points_GEBCObathy_df.rds"))



#compare GEBCO and Blake vms bathymetry data
#compare zonal statistic extracted depth to grid and mean/median of pot assigned depths in that grid


all_logs_points_sf_GEBCObathy_NOgeom <- read_rds(here::here('DCRB_sdmTMB', 'data', "all_logs_points_GEBCObathy_df.rds"))

library(scattermore) #supposed to make plotting large datasets quicker

summary(all_logs_points_sf_GEBCObathy_NOgeom$depth) #depth extracted to pot in logbook processing step, blake depth file
summary(all_logs_points_sf_GEBCObathy_NOgeom$depth_gebco)

# when depth is assigned to pots, i.e. points:
# even GEBCO has some weird deep points, -2106m, there seems to be a slight glitch in the GEBCO layer in some pixels...?
# 8 grid cells have deeper depth rasters than -500m 
# 18 grids have raster values deeper than -300m
# also GEBCO gives some 'on land' values --> I think make call that if the value used, e.g. median >0m, make it -1
#Blake vms depth has high depth values for ports and embayments

subset <- all_logs_points_sf_GEBCObathy_NOgeom %>% 
  filter(depth > -200) %>% 
  filter(depth_gebco > -200) %>% 
  filter(depth_gebco < 0)

plot_of_depths <- ggplot(subset, aes(x=depth, y=depth_gebco)) +
  geom_scattermore()
plot_of_depths
#if limit data to 0 to -200m range, then pretty 1-to-1 relationship between Blake vms bathy layer and GEBCO


#-------------------------------------------------------------

bathy_lamb_NOgeom <- bathy_lamb %>%   st_set_geometry(NULL)
plot(bathy_lamb_NOgeom$X_mean, bathy_lamb_NOgeom$X_median)
summary(bathy_lamb_NOgeom$X_mean)
summary(bathy_lamb_NOgeom$X_median)
#the mean and median zonal statistic from GEBCO are pretty similar

#-------------------------------------------------------------
#fix cases with pots with NA for GridID

#list cases where GridID = NA
all_logs_points_sf_GEBCObathy_NOgeom_GridNA <- all_logs_points_sf_GEBCObathy_NOgeom %>% #read file in on line 68
  filter(is.na(GRID5KM_ID))
unique(all_logs_points_sf_GEBCObathy_NOgeom_GridNA$SetID2)

all_logs_points_sf_GEBCObathy_NOgeom_GridFIXED <- all_logs_points_sf_GEBCObathy_NOgeom_GridNA %>% 
  mutate(
    GRID5KM_ID = case_when(
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2017-2018_150255', 'OR_2017-2018_150487') ~ 108730,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_60805', 'OR_2009-2010_63542', 'OR_2009-2010_63844', 'OR_2013-2014_114062', 'OR_2013-2014_115031',
                                        'OR_2013-2014_116872', 'OR_2013-2014_118538', 'OR_2013-2014_118660', 'OR_2013-2014_118809', 'OR_2013-2014_120280',
                                        'OR_2013-2014_120830', 'OR_2013-2014_121068', 'OR_2013-2014_121501', 'OR_2013-2014_121662', 'OR_2013-2014_134735') ~ 91891,
      is.na(GRID5KM_ID) & SetID2 %in% c('OR_2009-2010_47313', 'OR_2018-2019_29393', 'OR_2018-2019_29397', 'OR_2018-2019_29400', 'OR_2018-2019_29404',
                                        'OR_2018-2019_29412', 'OR_2018-2019_29414', 'OR_2018-2019_29418', 'OR_2018-2019_29428', 'OR_2018-2019_29432') ~ 117311,
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


all_logs_points_sf_GEBCObathy_NOgeom_NOGridNA <- all_logs_points_sf_GEBCObathy_NOgeom %>% 
  filter(!is.na(GRID5KM_ID))

all_logs_points_sf_GEBCObathy_NOgeom2 <- rbind(all_logs_points_sf_GEBCObathy_NOgeom_NOGridNA,all_logs_points_sf_GEBCObathy_NOgeom_GridFIXED)

#-------------------------------------------------------------


#assign depth for a grid cell based on the depth of each pot in it
summary_from_points <- all_logs_points_sf_GEBCObathy_NOgeom2 %>% 
  #need to filter out port and embayments, large negative values 
  #actually no, as no longer need to compare the different depth data
  #filter(depth > -1000) %>% 
  #drop out the few cases of no grid ID
  filter(!is.na(GRID5KM_ID)) %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(mean_depth_vms = mean(depth),
            median_depth_vms = median(depth),
            mean_depth_gebco = mean(depth_gebco),
            median_depth_gebco = median(depth_gebco),
            stdev_depth_gebco = sd(depth_gebco)
            )

#fix repeating gridIDs
bathy_lamb_NOgeom_fix <- bathy_lamb_NOgeom %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(X_mean = mean(X_mean),
            X_median = mean(X_median),
            X_stdev = mean(X_stdev)
         )

#join to study area grids - do this way as some study area grids may not have had points
depth_comparison <- bathy_lamb_NOgeom_fix %>% 
  left_join(summary_from_points, by="GRID5KM_ID") 


#compare e.g., median depth from points to median depth from grids (vms bathy grid)
plot(depth_comparison$median_depth_vms, depth_comparison$X_median)
plot(depth_comparison$median_depth_gebco, depth_comparison$X_median) #mdian depth from pots, vs median depth from zonal stat (X_median)
#overall there is a good relationship between median depth from pots, and median depth from zonal statistics
#but this breaks down in about 20-30 grid cells that are on the continental slope edge
#have one column for depth values for grids averaged from points in that grid, and a second column for depth
#in each grid from GEBCO and zonal statistics

#----------------------------------------------------------

# just use the depth_comparison df, work on the median extracted depth
study_area_grids_with_bathy <- depth_comparison %>% 
  select(-mean_depth_vms, -median_depth_vms) %>% 
  rename(depth_zonal_mean = X_mean,
         depth_zonal_median = X_median,
         depth_zonal_sd = X_stdev,
         depth_point_mean = mean_depth_gebco,
         depth_point_median = median_depth_gebco,
         depth_point_sd = stdev_depth_gebco
         ) %>% 
  #if point extracted mean/median depth is > 0 (19 unique grids), make it -1
  mutate(depth_point_median = ifelse(depth_point_median > 0, -1, depth_point_median)) %>% 
  mutate(depth_point_mean = ifelse(depth_point_mean > 0, -1, depth_point_mean)) 

median_depth_positive <- study_area_grids_with_bathy %>% 
  filter(depth_zonal_median > 0)
median_depth_negative <- study_area_grids_with_bathy %>% 
  filter(depth_zonal_median <= 0)
  #then, if zonal median depth is >0 (73 unique grids), use median point depth, if no point depth, then make it -1
median_depth_positive <- median_depth_positive %>%  
  mutate(depth_zonal_median = ifelse(is.na(depth_point_median), -1, depth_point_median))

study_area_grids_with_bathy <- rbind(median_depth_positive, median_depth_negative)


mean_depth_positive <- study_area_grids_with_bathy %>% 
  filter(depth_zonal_mean > 0)
mean_depth_negative <- study_area_grids_with_bathy %>% 
  filter(depth_zonal_mean <= 0)
#if zonal mean depth is >0, use mean point depth, if no point depth, then make it -1
mean_depth_positive <- mean_depth_positive %>%  
  mutate(depth_zonal_mean = ifelse(is.na(depth_point_mean), -1, depth_point_mean))

study_area_grids_with_bathy <- rbind(mean_depth_positive, mean_depth_negative)


#originally filled in NAs in depth_point_mean with depth_zonal_mean. 
#later decision on restricting study area to only grids that have ever had effort makes it redundant
#but earlier filter of filter(depth > -1000) caused a loss of some embayment grids that had pots to calc depth mean
#to fix that, at this point, restricted study_area_grids_with_bathy to the new study area to obtain a df
#study_area_grids_with_bathy_restricted, from which selected gridID, depth mean and depth sd columns for df study_area_grids_with_bathy_restricted_v2. 
#Also it has few NAs for sd for grid that had only 1 pot to calc depth mean. change the sd to 0 instead
#Export that and use it in a later script to fix couple of the depth values (instead of resaving outputs of all scripts)
#write_csv(study_area_grids_with_bathy_restricted_v2,here::here('DCRB_sdmTMB', 'data', "bathymetry_fixes.csv"))


#grids that didn't have point data to get bathymetry, use zonal statistic value
study_area_grids_with_bathy <- study_area_grids_with_bathy %>% 
  mutate(depth_point_mean = ifelse(is.na(depth_point_mean), depth_zonal_mean, depth_point_mean),
         depth_point_median = ifelse(is.na(depth_point_median), depth_zonal_median, depth_point_median),
         depth_point_sd = ifelse(is.na(depth_point_sd), depth_zonal_sd, depth_point_sd)
         )

#grab the file with wind etc and join depth to that?


study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed.rds"))
#grids that were in pieces and had repeating gridID have been fixed

#just join based on GRID5KM_ID as depth is always the same (no effect by season on half_month time steps)
study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth <- study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed %>% 
  left_join(study_area_grids_with_bathy, by="GRID5KM_ID")

#save df
#write_rds(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth,here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed_depth.rds"))



#maybe also fix some weird cases of low depth values (slight glitches in GeBCO?)








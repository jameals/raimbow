# predictor variabe: bathymetry

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

summary(all_logs_points_sf_GEBCObathy_NOgeom$depth)
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

#if limit data to 0 to -200m range, then pretty 1-to-1 relationship between Blake vms bathy layer and GEBCO


#-------------------------------------------------------------

bathy_lamb_NOgeom <- bathy_lamb %>%   st_set_geometry(NULL)
plot(bathy_lamb_NOgeom$X_mean, bathy_lamb_NOgeom$X_median)
summary(bathy_lamb_NOgeom$X_mean)
summary(bathy_lamb_NOgeom$X_median)
#the mean and median zonal statistic from GEBCO are pretty similar

summary_from_points <- all_logs_points_sf_GEBCObathy_NOgeom %>% 
  #need to filter out port and embayments, large negative values
  filter(depth > -1000) %>% 
  group_by(GRID5KM_ID) %>% 
  summarise(mean_depth_vms = mean(depth),
            median_depth_vms = median(depth),
            mean_depth_gebco = mean(depth_gebco),
            median_depth_gebco = median(depth_gebco),
            )

depth_comparison <- summary_from_points %>% 
  left_join(bathy_lamb_NOgeom, by="GRID5KM_ID") %>% 
  select(-NGDC_GRID, -ORIG_AREA, -US_EEZ, -AREA, -layer, -path)


#compare e.g., median depth from points to median depth from grids (vms bathy grid)
plot(depth_comparison$median_depth_vms, depth_comparison$X_median)
plot(depth_comparison$median_depth_gebco, depth_comparison$X_median)
#overall there is a good relationship between median depth from pots, and median depth from zonal statistics
#but this breaks down in about 20-30 grid cells that are on the continental slope edge
#have one column for depth values for grids averaged from points in that grid, and a second column for depth
#in each grid from GEBCO and zonal statistics

#----------------------------------------------------------

# all study area grids with all time step combos
study_area_grids_with_all_season_halfmonth_combos_sf <- read_rds(here::here('DCRB_sdmTMB', 'data','study_area_grids_with_all_season_halfmonth_combos_sf.rds'))
#grids in multiple pieces have repeating grid ID. 
#fix this similarly to wind/SST, just keep one grid ID of each

# bathymetry extracted in QGIS using zonal statistics, from gebco bathymetry - blake depth layer missing port/embayment depths
path_bathy_lamb <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/data files/study_area_depth_lamb_azi.shp"
bathy_lamb <- st_read(path_bathy_lamb, quiet = TRUE) 
#grids in multiple pieces have repeating grid ID. 
#fix this similarly to wind/SST, just keep one grid ID of each
#also fix depth >0 just to be 0. maybe also fix some weird cases of low depth values (slight glitches in GeBCO maybe)

# logbook points with GEBCO bathymetry
all_logs_points_sf_GEBCObathy_NOgeom <- read_rds(here::here('DCRB_sdmTMB', 'data', "all_logs_points_GEBCObathy_df.rds"))
#due to original joining to grid, some NAs. in these cases default to using grid based depth  






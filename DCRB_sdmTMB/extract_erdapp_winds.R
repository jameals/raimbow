# ERDAPP wind speed and SST extraction for DCRB fishing distribution modeling
library(rerddap)
library(tidyverse)
library(magrittr)

# Winds dataset we're using:
# https://upwell.pfeg.noaa.gov/erddap/griddap/nceiPH53sstd1day.html
# AVHRR Pathfinder Version 5.3 L3-Collated (L3C) SST, Global, 0.0417°, 1981-present, Daytime (1 Day Composite) 
# nceiPH53sstd1day

# LOAD study area coords
study_area <- read_sf(here::here('DCRB_sdmTMB','data', 'study_area.shp'))
xy <- read_rds(here('DCRB_sdmTMB','data','study_grid_centroids.rds'))
# we need to do the extraction in lat/lon space, so calc that here
latlons <- xy %>% 
  st_as_sf(coords=c("X","Y"),crs=st_crs(study_area),remove=F) %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(GRID5KM_ID=unique(xy$GRID5KM_ID))

windslat <- c(41.5,48.5)
# 360+range(vms$LONGITUDE) # Longitudes in the winds dataset are in degrees E
windslon <- c(-125.5,-123)

# sample day of winds data, to test the code and to obtain unique lat/lons
# get info about the dataset
(out<-info("nceiPH53sstd1day"))
# do an extraction for wind speed and SST
wind_sample <- griddap(out, # daily Pathfinder winds dataset
                       time=c("2009-01-01","2009-01-02"),
                       latitude=windslat,
                       longitude=windslon,
                       fields=c("wind_speed","sea_surface_temperature")) %>% 
  pluck("data")

# This next part is where we match each 5km grid cell to its nearest-neighbor winds dataset grid point
library(RANN)

winds_latlons <- wind_sample %>% select(lat,lon) %>% distinct()

# Match VMS lat/lon to ERDDAP lat/lon
erddap_grd_match <- RANN::nn2(winds_latlons,latlons %>% dplyr::select("Y","X"),k=1)$nn.idx

# add appropriate erddap lat/lons to 5km grid lat/lons data
latlons %<>% mutate(matchlat=wind_sample$lat[erddap_grd_match],matchlon=wind_sample$lon[erddap_grd_match])

# only pull the winds data for the lat/lons that we need
needed_latlons <- latlons %>% select(matchlat,matchlon) %>% distinct()

# Finally, we pull all the data
library(tictoc)
tic("Extracting wind speed and SST data: ")
winds_all <- purrr::map_df(2007:2020, function(yr) {
  times <- c(paste0(yr,"-01-01"),paste0(yr,"-12-31"))
  winds <- griddap(out,
                   time=times,
                   latitude=windslat,
                   longitude=windslon,
                   fields=c("wind_speed","sea_surface_temperature")) %>% 
    pluck("data") %>% 
    filter(lat %in% unique(needed_latlons$matchlat)&lon %in% unique(needed_latlons$matchlon))
  out <- winds %>% as_tibble() %>% mutate(utcdate=as_date(time))
  out
})
toc()

# save the full winds dataset
write_rds(winds_all,here('DCRB_sdmTMB','data','erdapp_winds_SST.rds'))
# and the lat/lon matching key
write_rds(latlons, here('DCRB_sdmTMB','data','grd_erdapp_winds_latlon_match.rds'))


#-------------------------------------------------------------------

#summarise wind and SST data for each gridn in each time step
#read files

erdapp_winds_SST <- read_rds(here('DCRB_sdmTMB','data','erdapp_winds_SST.rds'))
glimpse(erdapp_winds_SST)
grd_erdapp_winds_latlon_match <- read_rds(here('DCRB_sdmTMB','data','grd_erdapp_winds_latlon_match.rds'))
glimpse(grd_erdapp_winds_latlon_match)

#matchlat and matchlon should match the erddap data exactly

joined_df <- erdapp_winds_SST %>% 
  left_join(grd_erdapp_winds_latlon_match, by = c("lat" = "matchlat", "lon" = "matchlon"))

#there are lots of NAs for Grid ID
test_join <- joined_df %>% filter(!is.na(GRID5KM_ID))
unique(test_join$GRID5KM_ID)
#now have same number of unique grid IDs as before
study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 
erdapp_grids_id <- sort(unique(test_join$GRID5KM_ID))
identical(study_area_grids_id, erdapp_grids_id) #TRUE
#and unique grid IDs are the same as in the study area


#summarise wind and SST for each grid in each half month time step
sumamrised_wind_SST <- test_join %>% 
  #drop useless columns
  select(-time, -lat, -lon) %>% 
  rename(grd_x = X, grd_y = Y, SST = sea_surface_temperature) %>% 
  #specify which rows (dates) fall into each of half monthly time steps
  mutate(month = lubridate::month(utcdate, label = TRUE, abbr = FALSE)) %>% 
  mutate(day = lubridate::day(utcdate)) %>% 
  mutate(month_interval = ifelse(day <= 15, 1, 2)) %>% 
  mutate(half_month = paste0(month,"_",month_interval)) %>% 
  #also need to specify crab season
  mutate(year = lubridate::year(utcdate)) %>% 
  mutate(season_start = ifelse(month == "December", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "December", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end)) %>% 
  #drop useless columns
  select(-month, -day, -month_interval, -year, -season_start, -season_end, -utcdate) %>% 
  group_by(GRID5KM_ID, grd_x, grd_y, season, half_month) %>% #keep centroid x and y coords
  summarise(SST_avg = mean(SST, na.rm = TRUE),
            wind_avg = mean(wind_speed, na.rm = TRUE))
glimpse(sumamrised_wind_SST)
#there are some extra time steps in this df (e.g., oct-nov months)



study_area_grids_with_all_season_halfmonth_combos <- read_rds(here::here('DCRB_sdmTMB', 'data', "study_area_grids_with_all_season_halfmonth_combos_sf.rds"))

study_area_grids_with_all_season_halfmonth_combos <- study_area_grids_with_all_season_halfmonth_combos %>% 
  st_set_geometry(NULL) %>% 
  #some grids are in multiple pieces as broken up by land
  #we'll just use one value for each predictor for all
  distinct(GRID5KM_ID, season, half_month)

study_area_grids_with_all_season_halfmonth_combos_wind_SST <- study_area_grids_with_all_season_halfmonth_combos %>% 
  left_join(sumamrised_wind_SST, by=c("GRID5KM_ID", "season", "half_month"))
#no NAs for wind
#some NAs for SST
nrow(study_area_grids_with_all_season_halfmonth_combos_wind_SST)


#---------------------
#Fix NAs in SST

has_NAs <- study_area_grids_with_all_season_halfmonth_combos_wind_SST %>% 
  filter(is.na(SST_avg))
unique(has_NAs$GRID5KM_ID)
#75 grids
#NA for SST as grid centroid likely on land
#although sometimes missing for only one time step 
#we could fill these in by using SST value of next grid in that same time step
#but because of grid numbering, the next or closest grid number might not actually be a physically adjacent grid

df_has_NAs_SST <-  study_area_grids_with_all_season_halfmonth_combos_wind_SST %>% 
  filter(is.na(SST_avg))
#nrow(df_has_NAs_SST) #6459
df_no_NAs_SST <-  study_area_grids_with_all_season_halfmonth_combos_wind_SST %>% 
  filter(!is.na(SST_avg))
#nrow(df_no_NAs_SST) #371945


df_has_NAs_SST_fix <- df_has_NAs_SST %>% 
  #create a secondary GridID to use for mathcing
  mutate(GRID5KM_ID_v2 = GRID5KM_ID+1) %>% 
  #now join SST using the next grids SST
  left_join(sumamrised_wind_SST, by=c("GRID5KM_ID_v2"= "GRID5KM_ID", "season", "half_month")) %>% 
  select(GRID5KM_ID:wind_avg.x,SST_avg.y) %>% 
  rename(grd_x = grd_x.x, grd_y = grd_y.x, wind_avg = wind_avg.x)
#some SST vlaues were found
df_has_NAs_SST_fix_v2 <- df_has_NAs_SST_fix %>% 
  #create a different secondary GridID to use for mathcing
  mutate(GRID5KM_ID_v3 = GRID5KM_ID-1) %>% 
  #now join SST using the next grids SST
  left_join(sumamrised_wind_SST, by=c("GRID5KM_ID_v3"= "GRID5KM_ID", "season", "half_month")) %>% 
  select(GRID5KM_ID:wind_avg.x,SST_avg.y,SST_avg) %>% 
  rename(grd_x = grd_x.x, grd_y = grd_y.x, wind_avg = wind_avg.x)
#some more SST values were found

df_has_NAs_SST_fix_v3 <- df_has_NAs_SST_fix_v2 %>% 
  mutate(SST_avg = ifelse(is.na(SST_avg), SST_avg.y, SST_avg)) %>% 
  mutate(SST_avg = ifelse(is.na(SST_avg), SST_avg.x, SST_avg)) %>% #this didn't actually help
  select(-SST_avg.x, -SST_avg.y)

still_has_Nas <- df_has_NAs_SST_fix_v3 %>% filter(is.na(SST_avg))
unique(still_has_Nas$GRID5KM_ID)
#13 grids left
#some of them up far up estuaries and don't have neighbouring grids (+/- 1)


df_has_NAs_SST_fix_v4 <- df_has_NAs_SST_fix_v3 %>% 
  mutate(
    GRID5KM_ID_v4 = case_when(
      GRID5KM_ID == 118633 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 116988 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 95854 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 95855 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 96185 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 97177 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 89583 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 88593 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 91233 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 91563 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 104440 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 124898 & is.na(SST_avg) ~ 118962,
      GRID5KM_ID == 129513 & is.na(SST_avg) ~ 118962
      
    )
  ) %>% 
  #now join SST using the next grids SST
  left_join(sumamrised_wind_SST, by=c("GRID5KM_ID_v4"= "GRID5KM_ID", "season", "half_month")) %>% 
  select(GRID5KM_ID:SST_avg.x, ,SST_avg.y) %>% 
  rename(grd_x = grd_x.x, grd_y = grd_y.x, wind_avg = wind_avg.x, SST_avg = SST_avg.x)

df_has_NAs_SST_fix_v5 <- df_has_NAs_SST_fix_v4 %>% 
  mutate(SST_avg = ifelse(is.na(SST_avg), SST_avg.y, SST_avg)) %>% 
  select( -SST_avg.y)
nrow(df_has_NAs_SST_fix_v5) #6459


study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed <- rbind(df_no_NAs_SST, df_has_NAs_SST_fix_v5) 

#write_rds(study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed, here('DCRB_sdmTMB','data','study_area_grids_with_all_season_halfmonth_combos_wind_SST_fixed.rds'))

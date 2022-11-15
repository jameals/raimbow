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



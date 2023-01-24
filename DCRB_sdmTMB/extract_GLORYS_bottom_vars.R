# Clean and organize GLORYS oxygen data downloaded from 
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/download?dataset=cmems_mod_glo_bgc_my_0.25_P1D-m

library(tidyverse)
library(tidync)
library(lubridate)
library(ncdf4)
library(gstat)
library(terra)
library(sf)
library(tictoc)
library(doParallel)

# Leena's study grid centroids
gr <- read_csv("C:/Users/Owen.Liu/Documents/NWFSC Research/Dungeness Fleet Dynamics/Leena/study_area_grids_centroids.csv")
# bottom layer of GLORYS grid (previously extracted, see below)
glorys_bottom_key <- read_rds(here::here('DCRB_sdmTMB','data','GLORYS_bottom_layer.rds'))

# Do the extraction for all files
fls <- list.files("C:/Users/Owen.Liu/Documents/NWFSC Research/Dungeness Fleet Dynamics/Leena/",full.names = T) %>% 
  str_subset("cmems")

extract_interp_o2 <- function(nc_file_name){
  tic("loading")
  dat <- tidync(nc_file_name) %>% 
    # extract to df
    hyper_tibble() %>% 
    # filter for bottom layer
    left_join(glorys_bottom_key,by=c('longitude','latitude','depth')) %>% 
    filter(include) %>% 
    mutate(x=longitude,y=latitude) %>% 
    # format dates (date format and origin found by using ncdf4::nc_open)
    mutate(date=as.POSIXct("1950-01-01 00:00:00")+hours(time)) %>% 
    dplyr::select(date,x,y,depth,o2)
  toc()
  # get unique dates
  datelist <- unique(dat$date)
  tic("Interpolating.")
  out <- purrr::map_df(datelist, function(d){
    datsub <- dat %>% filter(date==d)
    # model o2 relationship across space utilizing 4 nearest neighbors
    gs <- gstat(formula=o2~1, locations=~x+y, data=datsub, nmax=4, set=list(idp = 0))
    # # spatialize
    ys <- st_as_sf(datsub,coords=c("x","y"),crs=4326) %>% vect()
    # make voronoi triangles for interpolation
    v <- voronoi(ys)
    r <- rast(v,res=0.1)
    
    nn <- interpolate(r, gs, debug.level=0)
    
    datext <- extract(nn,gr[c('grd_x','grd_y')])
    
    datout <- gr %>% 
      mutate(o2pred=datext$var1.pred) %>% 
      mutate(date=d)
    datout
  })
  toc()
  return(out)
}

# Actually process the files
cores <- detectCores() - 1
# cl <- makeCluster(cores)
# Initiate cluster
registerDoParallel(cores=cores)

foreach(ind=1:length(fls)) %dopar% {
  pkgs <- c("tidyverse","tidync","lubridate","gstat","terra","sf","tictoc")
  lapply(pkgs, require, character.only=TRUE)
  processed_dat <- extract_interp_o2(fls[ind])
  yrs <- unique(year(processed_dat$date)) %>% paste(collapse="_")
  write_rds(processed_dat,here::here("DCRB_sdmTMB","data",paste0("GLORYS_oxygen_interp_",yrs,".rds")))
}

# combine
allyrs_GLORYS_o2 <- list.files(here::here('DCRB_sdmTMB','data'),full.names=T) %>% 
  str_subset("GLORYS_oxygen") %>% 
  purrr::map_df(read_rds)
write_rds(allyrs_GLORYS_o2,here::here('DCRB_sdmTMB','data','GLORYS_o2_5kgrd_2007-2020.rds'))

#### previous and scratch work ####
## try with one file
fp <- "C:/Users/Owen.Liu/Documents/NWFSC Research/Dungeness Fleet Dynamics/Leena/cmems_mod_glo_bgc_my_0.25_P1D-m_2020.nc"

dat2020 <- tidync(fp) %>% hyper_tibble()
glimpse(dat2020)

# try finding just the bottom layer of each lat/lon point
x <- dat2020 %>% 
  dplyr::select(o2,longitude,latitude,depth,time) %>% 
  group_by(time,longitude,latitude) %>% 
  slice_max(order_by=depth) %>% 
  ungroup() %>% 
  # fix time dimension to something reasonable and then clean up
  # by looking at the ncdf, it looks like time is in hours since 1950-01-01 00:00:00
  mutate(date=as.POSIXct("1950-01-01 00:00:00")+hours(time)) %>% 
  dplyr::select(date,longitude,latitude,depth,o2)

# bottom depth layer key (to make subsetting faster later)
glorys_bottom_key <- x %>% 
  distinct(longitude,latitude,depth) %>% 
  mutate(include=TRUE)
write_rds(glorys_bottom_key,here::here('DCRB_sdmTMB','data','GLORYS_bottom_layer.rds'))

# try plotting to ensure this makes sense
y <- x %>% 
  ungroup() %>% 
  filter(date==min(x$date))
ggplot(y,aes(longitude,latitude,color=depth))+
  geom_point()
# looks okay
ggplot(y,aes(longitude,latitude,color=o2))+
  geom_point()
# also looks okay i think

# grid overlap
ggplot()+
  geom_point(data=gr,aes(grd_x,grd_y,color="grid"))+
  geom_point(data=y,aes(longitude,latitude,color="data"))

# # interpolate\
y <- y %>% mutate(x=longitude,y=latitude)
# model o2 relationship across space utilizing 4 nearest neighbors
gs <- gstat(formula=o2~1, locations=~x+y, data=y, nmax=4, set=list(idp = 0))
# # spatialize
ys <- st_as_sf(y,coords=c("longitude","latitude"),crs=4326) %>% vect()
# make voronoi triangles for interpolation
v <- voronoi(ys)
r <- rast(v,res=0.1)
# vr <- rasterize(v,r,"o2")
# plot(vr)

nn <- interpolate(r, gs, debug.level=0)
# nnmsk <- mask(nn, vr)
# plot(nnmsk, 1)

datext <- extract(nn,gr[c('grd_x','grd_y')])
glimpse(datext)

datout <- gr %>% mutate(o2pred=datext$var1.pred)

datout %>% 
  st_as_sf(coords=c('grd_x','grd_y'),crs=4326) %>% 
  st_transform(32610) %>% 
  ggplot(aes(col=o2pred))+
  geom_sf()



#-------
#summarising bottom O2 data on half-month steps

GLORYS_o2_5kgrd_2007_2020 <- read_rds(here::here('DCRB_sdmTMB','data','GLORYS','GLORYS_o2_5kgrd_2007-2020.rds'))
glimpse(GLORYS_o2_5kgrd_2007_2020 )

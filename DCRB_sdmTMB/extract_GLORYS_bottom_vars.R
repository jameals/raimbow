# Clean and organize GLORYS oxygen data downloaded from 
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/download?dataset=cmems_mod_glo_bgc_my_0.25_P1D-m

library(tidyverse)
library(tidync)
library(lubridate)

# try with one file
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

# try plotting to ensure this makes sense
y <- x %>% 
  ungroup() %>% 
  slice_min(date)
ggplot(y,aes(longitude,latitude,color=depth))+
  geom_point()
# looks okay
ggplot(y,aes(longitude,latitude,color=o2))+
  geom_point()
# also looks okay i think

# Do the extraction for all files
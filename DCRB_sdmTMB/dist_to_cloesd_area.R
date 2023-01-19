#distance to nearest cloesd area/grid

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
library(nngeo)

#-------------------------------------------------------------------------------------------------

# closed areas info was joined to the "full df"

df_full_with_closed_areas <- read_rds(here::here('DCRB_sdmTMB', 'data', "df_full_not_final.rds")) %>% 
  #but drop other predictor columns 
  select(GRID5KM_ID:grd_y, open_closed)

#currently grid centroid location grd_x and grd_y are in lat and lon
#keep them but also create coordinates in UTM 10 zone (grd_x_UTM10 and grd_y_UTM10)
df_full_sf <- st_as_sf(df_full_with_closed_areas, 
                       coords = c("grd_x", "grd_y"),
                       crs = 4326,
                       remove=F
) %>% 
  # project to UTM zone 10
  st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")
#length units are in meters

#-------------------------------------------------------------------------------------------------

df_open_grids <- df_full_sf %>% 
  filter(open_closed == "open")

df_closed_grids <- df_full_sf %>% 
  filter(open_closed == "closed")

#-------------------------------------------------------------------------------------------------

#In a given time_step, for each open grid what is the distance to the nearest closed grid

df_open_grids_subset <- df_open_grids %>% 
  filter(season=="2009-2010", half_month=="September_1")
plot(df_open_grids_subset)

df_closed_grids_subset <- df_closed_grids %>% 
  filter(season=="2009-2010", half_month=="September_1")
plot(df_closed_grids_subset)


neares_closed_grid <- st_nn(df_open_grids_subset, df_closed_grids_subset, returnDist = T, k = 1) 



nn_to_df <- function(nn_out) {
  out <- map(names(nn_out), ~enframe(pluck(nn_out, .), name = "row_id", value = .) %>% 
               unnest(cols=all_of(.x))) 
  bind_cols(out[[1]], out[[2]] %>% select(-row_id))
}
myDF <- nn_to_df(nn_out=neares_closed_grid)

test_output <- df_open_grids_subset %>% 
  cbind(myDF) %>% 
  #we don't need the columns for row_id and nn
  select(-(row_id:nn)) %>% 
  #make dist column be in km
  mutate(dist = dist/1000) %>% 
  rename(dist_to_closed_km = dist)

plot(test_output)

test_output <- test_output %>% 
  st_set_geometry(NULL) %>% 
  select(GRID5KM_ID, season, half_month, dist_to_closed_km)


# columns <- c("GRID5KM_ID", "season", "half_month", "dist_to_closed_km")
# dummy_df <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
# colnames(dummy_df) = columns
# df_dist_to_closed <- dummy_df 

df_dist_to_closed <- df_dist_to_closed %>% 
  rbind(test_output)

unique(df_dist_to_closed$half_month)






#df_dist_to_closed_2009_2010 <-  df_dist_to_closed
#nrow(df_dist_to_closed_2009_2010)
#write_rds(df_dist_to_closed_2009_2010,here::here('DCRB_sdmTMB', 'data', 'closed areas', "df_dist_to_closed_2009_2010.rds"))

#no logbook data for WA for 2008-08 and 2008-09 - currently WA grids are jsut labelled clodes
#but that is not exactly accurate, so probably better to drop OR 2008-08 and 2008-09 data


#2018-2019 -- all grids are open between May_1 and August_1 so distance to closed area is 0 for all grids 
#fix this separately later



#------------------------------

dist_to_closed_all <- rbind(df_dist_to_closed_2009_2010,
                            df_dist_to_closed_2010_2011,
                            df_dist_to_closed_2011_2012,
                            df_dist_to_closed_2012_2013,
                            df_dist_to_closed_2013_2014,
                            df_dist_to_closed_2014_2015,
                            df_dist_to_closed_2015_2016,
                            df_dist_to_closed_2016_2017,
                            df_dist_to_closed_2017_2018,
                            df_dist_to_closed_2018_2019,
                            df_dist_to_closed_2019_2020
                            )

#write_rds(dist_to_closed_all,here::here('DCRB_sdmTMB', 'data', 'closed areas', "dist_to_closed_all.rds"))


#------------------------------


df_full_with_closed_areas <- read_rds(here::here('DCRB_sdmTMB', 'data', "df_full_not_final.rds")) 

#when join all this to the 'full' df, any grids that are closed will have NA for distance to closed area (or we can make it 0)
#those grids get dropped out anyways from the actual analysis

df_full_with_dist_to_closed_areas_dist <- df_full_with_closed_areas %>% 
  left_join(dist_to_closed_all, by=c('season', 'half_month','GRID5KM_ID'))

#2007-08 and 2008-09 are NAs as no logbooks for WA
#fix 2018-2019 -- all grids are open between May_1 and August_1 so distance to closed area is 0 for all grids 

df_full_with_dist_to_closed_areas_dist <- df_full_with_dist_to_closed_areas_dist %>% 
  mutate(dist_to_closed_km = 
           ifelse(season=="2018-2019" & open_closed=="open" & is.na(dist_to_closed_km), 0, dist_to_closed_km))




#write_rds(df_full_with_dist_to_closed_areas_dist,here::here('DCRB_sdmTMB', 'data', "df_full_with_dist_to_closed_areas_not_final_20230120.rds"))




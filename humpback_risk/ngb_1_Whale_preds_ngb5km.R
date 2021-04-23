###############################################################################
library(eSDM)
library(dplyr)
library(purrr)
library(readr)
library(sf)


###############################################################################
### Load Blake's 5km equal area grid
load("../raimbow-local/RDATA_files/Grid_5km_landerased.RDATA")
# grid.5km.ea <- st_read("../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp",
#                        stringsAsFactors = FALSE)
grid.5km.lno.cent <- st_centroid(grid.5km.lno)
# grid.5km.ea.cent <- st_centroid(grid.5km.ea)

### Load ~3km grid
grid.3km <- read_csv("../raimbow-local/Data/Grid_Nonrectangle_3km_WEAR.csv") %>% 
  mutate(base_idx = seq_along(lon180)) %>% 
  select(lon180, lat, base_idx, area_km) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant")
# grid.3km.cent <- st_centroid(grid.3km)

### Load Mn predictions and create sf object
mn.preds <- read_csv("../raimbow-local/Data/Whale_preds/WEAR3km_76_2005-01-01to2018-07-30_BiDaily_dens.csv", 
                     col_types = cols(.default = col_double())) %>% 
  select(mlon, mlat, pixel, starts_with("X76.dens.")) %>% 
  purrr::set_names(~ paste0("Mn_", .)) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant") %>% 
  rename(base_idx = Mn_pixel)

identical(st_geometry(grid.3km), st_geometry(mn.preds)) #TRUE


###############################################################################
# Join Karin predictions to 5km grid by sample 3km grid using 5km centroids
d <- st_intersects(grid.5km.lno.cent, mn.preds)
table(sapply(d, length))

d.key <- data.frame(
  GRID5KM_ID = grid.5km.lno.cent$GRID5KM_ID, 
  base_idx = as.numeric(d)               
) %>% 
  filter(!is.na(base_idx))

mn.close.sf <- d.key %>% 
  left_join(st_drop_geometry(mn.preds), by = c("base_idx")) %>% 
  left_join(grid.5km.lno) %>% 
  st_sf(agr = "constant")

mn.close <- mn.close.sf %>% 
  st_drop_geometry() %>% 
  select(GRID5KM_ID, starts_with("Mn_X76")) %>% 
  purrr::set_names(c("GRID5KM_ID", gsub("[.]", "_", paste0("H_", substr(names(.)[-1], 15, 22)))))

write_csv(mn.close, path = "../raimbow-local/Outputs/Mn_preds_ngb5km.csv")


###############################################################################
###############################################################################
library(tidyverse)
library(sf)
load("../raimbow-local/RDATA_files/Grid_5km_landerased.RDATA")
# Ngb
mn.close <- read_csv("../raimbow-local/Outputs/Mn_preds_ngb5km.csv") %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_sf(agr = "constant")

# Overlaid
mn.overlaid <- read_csv("../raimbow-local/Outputs/WEAR5km_76_Model1_dens_2009-01-02to2018-07-30.csv") %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_sf(agr = "constant")


# Comparisons
sum(!(mn.overlaid$GRID5KM_ID %in% mn.close$GRID5KM_ID))
sum(!(mn.close$GRID5KM_ID %in% mn.overlaid$GRID5KM_ID))

sum(mn.close$area_km_lno * mn.close$Mn_X76.dens.2005.01.01, na.rm = TRUE)
sum(mn.overlaid$area_km_lno * mn.overlaid$H_05_01_01, na.rm = TRUE)

sum(mn.overlaid$area_km_lno * mn.overlaid$H_09_01_02, na.rm = TRUE)
sum(mn.close$area_km_lno * mn.close$Mn_X76.dens.2009.01.02, na.rm = TRUE)

plot(mn.overlaid["H_09_01_02"], axes = TRUE, border = NA)
plot(mn.close["Mn_X76.dens.2009.01.02"], axes = TRUE, border = NA)

###############################################################################

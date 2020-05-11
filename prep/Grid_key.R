# Create key(s) for grid cell ID and applicable info classifiers

###############################################################################
### Grid cell - Region
library(dplyr)
library(sf)

grid.5km.path <- "../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"

grid.5km <- st_read(grid.5km.path)

reg.bound <- c(32.5, 34.4, 36.3, 38.76683, 42, 46.25, 50)
reg.names <- c("CA-S", "CA-SCen", "CA-Cen", "CA-N", "OR", "WA")
stopifnot(length(reg.names) == length(reg.bound) - 1)

grid.region <- grid.5km %>% 
  st_drop_geometry() %>% 
  filter(!duplicated(GRID5KM_ID)) %>% 
  arrange(GRID5KM_ID) %>% 
  mutate(region_idx = findInterval(LATITUDE, reg.bound, left.open = TRUE)) %>% 
  filter(region_idx != 0) %>% 
  mutate(region_ts = factor(reg.names[region_idx], levels = rev(reg.names)), 
         Region = as.character(region_ts), 
         Region = case_when(Region == "WA" ~ "WA", 
                            Region == "OR" ~ "OR", 
                            Region == "CA-N" ~ "NorCA", 
                            Region == "CA-Cen" ~ "CenCA", 
                            Region == "CA-SCen" ~ "CenCA", 
                            Region == "CA-S" ~ "CA-S")) %>% 
  select(GRID5KM_ID, region_ts, Region)

###############################################################################

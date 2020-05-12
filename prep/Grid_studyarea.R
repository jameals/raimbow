# Define the study area, i.e. the grid cells used

library(dplyr)
library(lubridate)
library(rnaturalearth)
library(sf)


#------------------------------------------------------------------------------
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  path.fish <- "../raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS"
  path.hump <- "../raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
  path.blue <- "../raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds"
  
  grid.5km.path <- "../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  
  file.out  <- "../raimbow-local/RDATA_files/Grid_studyarea.rds"
  
} else {
  stop("Invlaid user")
}


### Load total objects
x.fish <- readRDS(path.fish)

x.hump <- readRDS(path.hump) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

x.blue <- readRDS(path.blue) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

stopifnot(
  !anyNA(x.hump$Humpback_dens_mean), 
  !anyNA(x.blue$Blue_occurrence_mean)
)

### Get grid cells with non-NA values for all, and save that of fishing data
fish.id <- sort(unique(x.fish$GRID5KM_ID))
hump.id <- sort(unique(x.hump$GRID5KM_ID))
blue.id <- sort(unique(x.blue$GRID5KM_ID))

saveRDS(fish.id, file = file.out)

#------------------------------------------------------------------------------
### Examine and plot the grid cells that have non-NA fishing values but NA whale preds
sum(!(fish.id %in% hump.id))
sum(!(fish.id %in% blue.id))

z.hump <- fish.id[(!(fish.id %in% hump.id))]
z.blue <- fish.id[(!(fish.id %in% blue.id))]

grid.5km <- st_read(grid.5km.path)
grid.5km.na <- grid.5km %>% filter(GRID5KM_ID %in% c(z.hump, z.blue))
grid.5km.fishnona <- grid.5km %>% filter(GRID5KM_ID %in% c(fish.id))

rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")), 
  ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
    filter(admin %in% c("Canada", "Mexico")) %>% 
    st_geometry()
)

# Could save these using png() and dev.off()
# Plot - grid is blue, grid with non-NA fishing and NA whale is red
plot(st_geometry(grid.5km), axes = TRUE, border = NA, col = "blue")
plot(st_geometry(grid.5km.na), add = TRUE, border = NA, col = "red")
plot(rmap.base, add = TRUE, border = "tan", col = NA)

# Plot - grid is blue, grid with non-NA fishing is green
plot(st_geometry(grid.5km), axes = TRUE, border = NA, col = "blue")
plot(st_geometry(grid.5km.fishnona), add = TRUE, border = NA, col = "green")
plot(rmap.base, add = TRUE, border = "tan", col = NA)

#------------------------------------------------------------------------------


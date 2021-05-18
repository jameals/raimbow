# Define the study area, i.e. the grid cells used

library(dplyr)
library(lubridate)
library(rnaturalearth)
library(sf)


#------------------------------------------------------------------------------
source("User_script_local.R")
if (user == "JS") {
  path.fish <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels.RDS"
  path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
  path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"
  grid.5km.path <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  file.out <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_studyarea.rds"
  
} else if (user == "SMW") {
  path.fish <- "C:/SMW/RAIMBOW/raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS"
  path.hump <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
  path.blue <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds"
  
  grid.5km.path <- "C:/SMW/RAIMBOW/raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  
  file.out  <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_studyarea.rds"
  
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
  !anyNA(x.fish$Num_DCRB_VMS_pings), 
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


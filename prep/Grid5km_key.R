# Create key(s) for grid cell ID, Region (tradeoff and time series), 
#   and CA_OFFShOR (CDFW large blocks)

###############################################################################
### Grid cell - Region
library(dplyr)
library(sf)

source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  path.fish <- "C:/SMW/RAIMBOW/raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS"
  path.studyarea.rds <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_studyarea.rds"
  grid.5km.path <- "C:/SMW/RAIMBOW/raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  cdfw.path <- "C:/SMW/RAIMBOW/raimbow-local/Data/CDFW/MAN_CA_largeOffshoreblocks[1]/MAN_CA_largeOffshoreblocks.shp"
  file.out <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key_region.rds"
  
} else {
  stop("Invlaid user")
}

# Read in fishing data and grid study area, and confirm all fishing grid cells are in study area
x.fish <- readRDS(path.fish)
x.fish %>% group_by(Region, CA_OFFSHOR) %>% summarise(count = n())

grid.studyarea <- readRDS(path.studyarea.rds)
stopifnot(all(x.fish$GRID5KM_ID %in% grid.studyarea))

# Read in grid and CDFW large blocks, from which to get latitude information
#   Checks done to ensure that st_make_valid functionally does not change anything
cdfw.block <- st_read(cdfw.path) %>%
  st_make_valid() %>% 
  st_transform(4326)
st_bbox(cdfw.block)

cdfw.block.summ <- cdfw.block %>% 
  group_by(BLOCK10_ID) %>% #unnecessary
  summarise(xmin = round(st_bbox(.data$geometry)["xmin"], 3), 
            xmax = round(st_bbox(.data$geometry)["xmax"], 3), 
            ymin = round(st_bbox(.data$geometry)["ymin"], 3), 
            ymax = round(st_bbox(.data$geometry)["ymax"], 3))

cdfw.bound <- c(cdfw.block.summ$ymin, 42)

as.data.frame(st_drop_geometry(cdfw.block.summ))
all.equal(cdfw.block.summ$ymin[-1], head(cdfw.block.summ$ymax, -1))

st_crs(cdfw.block)
plot(cdfw.block[1], axes = TRUE)


# Read in 5km grid and intersect with CDFW blocks to get block IDs
#   Update: can't intersect b/c some grid cell centroids don't interesect with the polygons
#   Update2: LATITUDE and LONGITUDE are functionally equal to sf-calculated centroids
grid.5km.sa <- st_read(grid.5km.path) %>% 
  filter(GRID5KM_ID %in% grid.studyarea) %>% 
  # st_centroid() %>%
  # st_intersection(select(cdfw.block, CA_OFFSHOR = BLOCK10_ID)) %>% 
  # mutate(lon = st_coordinates(.data$geometry)[, 1], 
  #        lat = st_coordinates(.data$geometry)[, 2], 
  #        lon_diff = LONGITUDE - lon, 
  #        lat_diff = LATITUDE - lat) %>% 
  st_drop_geometry()


###############################################################################
# Define boundaries used for whale risk time series. Need to do this rather
#   than intersection b/c time series regions do not directly correspond
# NorCA/CA-N: CDFW blocks 1040, 1041, 1042
# CenCA: CDFW blocks 1036, 1037, 1038
#   For time series, this is split into CA-Cen and CA-SCen
# Block 1035: CA-S for time series, CenCA for Region
# reg.bound <- c(32.5, 34.4, 36.3, 38.76683, 42, 46.25, 50) # Old values
reg.bound <- c(32.5, 34.5, 36.3, 38.833, 42, 46.25, 50)
reg.names <- c("CA-S", "CA-SCen", "CA-Cen", "CA-N", "OR", "WA")
stopifnot(length(reg.names) == length(reg.bound) - 1)

grid.region.idx <- grid.5km.sa %>% 
  filter(!duplicated(GRID5KM_ID)) %>% 
  arrange(GRID5KM_ID) %>% 
  mutate(region_idx = findInterval(LATITUDE, reg.bound, left.open = TRUE), 
         block_idx = findInterval(LATITUDE, cdfw.bound, left.open = TRUE)) 
# Check that all study area grid cells were assigned a region
stopifnot(
  all(between(grid.region.idx$region_idx, 1, length(reg.names)))
  # all(between(grid.region.idx$block_idx, 1, 10)) # 4 cells are in OR
)

grid.region <- grid.region.idx %>% 
  filter(region_idx != 0) %>% 
  mutate(region_ts = factor(reg.names[region_idx], levels = rev(reg.names)), 
         Region = as.character(region_ts), 
         Region = case_when(Region == "WA" ~ "WA", 
                            Region == "OR" ~ "OR", 
                            Region == "CA-N" ~ "NorCA", 
                            Region == "CA-Cen" ~ "CenCA", 
                            Region == "CA-SCen" ~ "CenCA", 
                            Region == "CA-S" ~ "CenCA"), 
         CA_OFFSHOR = cdfw.block.summ$BLOCK10_ID[block_idx]) %>% 
  select(GRID5KM_ID, region_ts, Region, CA_OFFSHOR)
stopifnot(nrow(grid.region) == length(grid.studyarea))

saveRDS(grid.region, file = file.out)

###############################################################################

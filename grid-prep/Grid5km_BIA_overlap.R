# Code to determine 5km equal area grid cells overlap with Mn and Bm BIAs
#   Ouput csv file only contains grid cell IDs that overlap with
#   an Mn or Bm BIA
# Humpback sci name: Megaptera novaeangliae, hence the 'mn' abbreviation
# Blue sci name: Balaenoptera musculus, hence the 'bm' abbreviation

# By Sam Woodman, March 2019

##### NOTE: May 2020, this file was moved to 'raimbow/prep' #####

library(dplyr)
library(maps)
library(sf)


source("User_script_local.R")
if (user == "JS") {
  grid.5km.path <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  path.bia.shp <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/BIAs/CetMap_BIA_WGS84/CetMap_BIA_WGS84.shp"
  
  file.out <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_BIA_overlap.rds"
  
} else if (user == "SMW") {
  grid.5km.path <- "../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  path.bia.shp <- "../raimbow-local/Data/CetMap_BIA_WGS84/CetMap_BIA_WGS84.shp"
  
  file.out <- "../raimbow-local/RDATA_files/Grid5km_BIA_overlap.rds"
  
} else {
  stop("Invlaid user")
}


map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))


###############################################################################
#------------------------------------------------------------------------------
# Prep

### Load files
grid.5km.ea <- st_read(grid.5km.path, stringsAsFactors = FALSE)
bia.sf <- st_read(path.bia.shp, stringsAsFactors = FALSE)


### Humpback BIAs (Mn)
bia.mn.sf <- bia.sf %>% 
  filter(sci_name == "Megaptera novaeangliae", 
         region == "West Coast")

plot(st_geometry(bia.mn.sf), axes = TRUE, xlim = c(-130, -115), ylim = c(31, 49))
plot(map.base, add = TRUE, col = "tan", border = NA)


### Blue whale BIAs (Bm)
bia.bm.sf <- bia.sf %>% 
  filter(sci_name == "Balaenoptera musculus")

plot(st_geometry(bia.bm.sf), axes = TRUE, xlim = c(-130, -115), ylim = c(31, 49))
plot(map.base, add = TRUE, col = "tan", border = NA)


#------------------------------------------------------------------------------
# Process and export csv file

### Get intersect
grid.mn.int <- st_intersects(grid.5km.ea, bia.mn.sf)
grid.bm.int <- st_intersects(grid.5km.ea, bia.bm.sf)

grid.int <- grid.5km.ea %>% 
  mutate(BIA_mn = ifelse(vapply(grid.mn.int, length, 1) > 0, 1, 0), 
         BIA_bm = ifelse(vapply(grid.bm.int, length, 1) > 0, 1, 0)) 

# Sanity check
plot(grid.int["BIA_mn"], axes = TRUE, border = NA)
plot(grid.int["BIA_bm"], axes = TRUE, border = NA)

### Write csv file
grid.out <- grid.int %>% 
  filter(BIA_mn == 1 | BIA_bm == 1) %>% 
  st_set_geometry(NULL)

# write_csv(grid.out, path = "../raimbow-local/Outputs/Grid5km_BIA_overlap.csv")
saveRDS(grid.out, file = file.out)

###############################################################################

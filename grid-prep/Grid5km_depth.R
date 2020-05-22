# Script to save RDS file of grid cell - depth key


#------------------------------------------------------------------------------
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  # grid.5km.path <- "C:/SMW/RAIMBOW/raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  grid.depth.path <- "C:/SMW/RAIMBOW/raimbow-local/Data/5x5 km grid shapefile/weighted_mean_NGDC_depths_for_5km_gridcells.csv"
  
  path.grid.studyarea <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_studyarea.rds"
  
  file.out  <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_depth.rds"
  
} else {
  stop("Invlaid user")
}


#------------------------------------------------------------------------------
# Read/process/save
grid.depth <- read.csv(grid.depth.path)
grid.studyarea <- readRDS(path.grid.studyarea)

names(grid.depth)

names(grid.depth) <- c("GRID5KM_ID", "depth")

all(grid.studyarea %in% grid.depth$Gridcell_ID)
summary(grid.depth$depth)

saveRDS(grid.depth, file = file.out)

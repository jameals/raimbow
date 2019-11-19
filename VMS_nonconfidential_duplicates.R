# Code for identifying and removing duplicate rows in CA-only, 
#   non-confidential VMS data


###############################################################################
# Determine user and thus relevant file paths
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  path.data <- "../raimbow-local/Data/Non-confidential VMS monthly data/"
  
} else {
  stop("User not recognized")
}


###############################################################################
x.raw <- read.csv(
  paste0(path.data, "Non-confidential VMS data summarized by 5km grid cell 2009-18 All 3 states.csv"), 
  stringsAsFactors = FALSE
)
y.raw <- read.csv(
  paste0(path.data, "Non-confidential VMS data summarized by 5km grid cell 2009-18 CA only.csv"), 
  stringsAsFactors = FALSE
)

x.raw.paste.row <- apply(x.raw, 1, paste, collapse = "-")
y.raw.paste.row <- apply(y.raw, 1, paste, collapse = "-")

sum(duplicated(x.raw.paste.row))
sum(duplicated(y.raw.paste.row))

head(y.raw[duplicated(y.raw.paste.row), ])


# For instance, the data for GRID5KM_ID 52653, year_mo "2011_02", is in the CA only csv file twice


y.nodup <- y.raw[!duplicated(y.raw.paste.row), ]

write.csv(
  y.nodup, 
  file = paste0(path.data, "Non-confidential VMS data summarized by 5km grid cell 2009-18 CA only - no duplicates SMW.csv"), 
  row.names = FALSE
)


###############################################################################
### Sam experimental
# library(dplyr)
# library(sf)
# 
# grid.5km.geom <- sf::st_read("Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp") %>% 
#   st_drop_geometry()
# 
# x <- x.raw %>% left_join(grid.5km.geom) %>% filter(LATITUDE < 40)
# y <- y.nodup %>% 
#   left_join(grid.5km.geom) %>% 
#   filter(LATITUDE < 40, 
#          dollars_DCRB_noncon != "0")
# 
# x.paste.row <- apply(x, 1, paste, collapse = "-")
# y.paste.row <- apply(y, 1, paste, collapse = "-")
# 
# d <- y.paste.row[!(y.paste.row %in% x.paste.row)]
# 
# 
# 
# x %>% filter(GRID5KM_ID == 55953, year_mo == "2009_05")
# y %>% filter(GRID5KM_ID == 55953, year_mo == "2009_05")

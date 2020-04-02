### Overlay KAF Mn predictions (~3km grid) onto Blake's 5km ea grid
### Use TJ's shapefile to erase land from KAF predictions before overlaying to 
###   make overlay more accurate

### Currently overlays predictions from 1 Jan 2005 to 14 Aug 2020.

# NOTE: The overlaid (onto 5km grid) predictions predict higher abundance 
#   because land hasn't been cut out, duh


###############################################################################
library(eSDM)
library(dplyr)
library(purrr)
library(readr)
library(sf)

path.data <- "../raimbow-local/Data/"
path.rdata <- "../raimbow-local/RDATA_files/"


###############################################################################
### Load Blake's 5km equal area grid
grid.5km.ea <- st_read(paste0(path.data, "5x5 km grid shapefile/five_km_grid_polys_geo.shp"), 
                       stringsAsFactors = FALSE)
st_crs(grid.5km.ea) <- 4326

### Load ~3km grid
grid.3km <- read_csv(paste0(path.data, "Grid_Nonrectangle_3km_WEAR.csv")) %>% 
  mutate(base_idx = seq_along(lon180)) %>% 
  select(lon180, lat, base_idx, area_km) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant")

# mn.preds1 <- read_csv("../Risk_assessment_R/Whale_preds/KAF_03212019_Model1/WEAR3km_76_2009-01-02to2017-12-30daily_dens.csv")
# mn.preds2 <- read_csv("../Risk_assessment_R/Whale_preds/KAF_03212019_Model1/WEAR3km_76_2018-01-01to2018-07-30Biwk_Dens.csv")
# identical(mn.preds1[, 1:5], mn.preds2[, 1:5])

### Load Mn predictions and create sf object
mn.preds.csv <- read_csv(paste0(path.data, "Humpback 3km models/WEAR3km_76_2005-01-01to2019-08-14_Bidaily_dens.csv"), 
                         col_types = cols(.default = col_double())) 
mn.preds <- mn.preds.csv %>% 
  select(mlon, mlat, pixel, starts_with("76.dens.")) %>% 
  purrr::set_names(~ paste0("Mn_", .)) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant") %>% 
  rename(base_idx = Mn_pixel)

identical(st_geometry(grid.3km), st_geometry(mn.preds)) #TRUE
rm(mn.preds.csv)
save.image(paste0(path.rdata, "Whale_preds_3kmgrid.RDATA"))


#----------------------------------------------------------
# # This section shows that 3 columns have all NAs but otherwise,
# #   all columns of mn.preds have NAs at the same indices
# mn.preds.df <- st_drop_geometry(mn.preds) %>% #[2:1659]
#   select(starts_with("Mn_76.dens."), starts_with("BiWkSt2018"))
# mn.preds.na <- map(mn.preds.df, function(i) which(is.na(i)))
# mn.preds.temp <- vapply(
#   mn.preds.na, function(i, j) {identical(i, j)}, as.logical(1),
#   j = mn.preds.na[[1]]
# )
# names(which(!mn.preds.temp))
# [1] "Mn_76.dens.2010.12.27" "Mn_76.dens.2010.12.29" "Mn_76.dens.2010.12.31"
# # ^ Confirmed in KAF original file that these columns are all NA (ROMS issue)
#----------------------------------------------------------


###############################################################################
# Remove land from Mn predictions

load(paste0(path.rdata, "Whale_preds_3kmgrid.RDATA"))

#----------------------------------------------------------
### Prep land file and erase land from grid
# st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
land.sfc <- st_read("C:/SMW/eSDM/Ensemble Case Study/GIS_files_forJVR/Shapefiles/World_countries.shp") %>% 
  st_geometry() %>% 
  st_crop(st_bbox(grid.5km.ea)) %>% 
  st_combine() %>% 
  st_union()

# system.time(z <- st_intersects(grid.3km, land.sfc)) #63s
# z.which <- which(sapply(z, length) > 0)
# system.time(grid.3km.lint <- st_difference(grid.3km[z.which, ], land.sfc)) #18min
# save(z.which, grid.3km.lint, file = "RDATA_files/Grid_3km_lint.RDATA")
load(paste0(path.rdata, "Grid_3km_lint.RDATA"))

grid.3km.lno <- rbind(grid.3km[-z.which, ], grid.3km.lint) %>% 
  arrange(base_idx) %>% 
  dplyr::mutate(area_km_lno = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  select(base_idx, area_km, area_km_lno)

# plot(grid.3km.lno["area_km"], axes = TRUE, border = NA)
# plot(grid.3km.lno["area_km_lno"], axes = TRUE, border = NA)


#----------------------------------------------------------
### Join Mn predictions and grid-with-land-erased
mn.preds.lno.3km <- left_join(
  grid.3km.lno, st_drop_geometry(mn.preds), by = "base_idx"
)

# Sanity checks
sum(!is.na(mn.preds$Mn_76.dens.2009.01.06))     #53885
sum(!is.na(mn.preds.lno.3km$Mn_76.dens.2009.01.06)) #53885
# plot(mn.preds.lno.3km[5], axes = T, border = NA)
# plot(mn.preds.lno.3km[1660], axes = T, border = NA)


###############################################################################
# Overlay onto 5km EA grid, do sanity checks, do additional 
#   processing steps for Blake, and write to csv

#----------------------------------------------------------
### Overlay and sanity checks
tmp.over <- overlay_sdm(
  st_geometry(grid.5km.ea), st_transform(mn.preds.lno.3km, st_crs(grid.5km.ea)), 
  4:(ncol(mn.preds.lno.3km) - 1), 0
) 
mn.preds.lno.5kmover <- tmp.over %>% 
  mutate(GRID5KM_ID = grid.5km.ea$GRID5KM_ID, 
         area_km = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  select(GRID5KM_ID, area_km, starts_with("Mn_76"))
#830s = ~14min
rm(tmp.over)
# save.image(paste0(path.rdata, "Whale_preds_overlaid5kmgrid.RDATA"))


load(paste0(path.rdata, "Whale_preds_overlaid5kmgrid.RDATA"))
identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.ea))

# Sanity check 1
identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.ea))
eSDM::model_abundance(mn.preds, "Mn_76.dens.2009.01.02")
eSDM::model_abundance(mn.preds.lno.3km, "Mn_76.dens.2009.01.02")
eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.02")
sum(mn.preds.lno.5kmover$Mn_76.dens.2009.01.02 * mn.preds.lno.5kmover$area_km, na.rm = TRUE)

eSDM::model_abundance(mn.preds.lno.3km, "Mn_76.dens.2009.01.04")
eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.04")

# # Sanity check 2
# plot(mn.preds.lno.5kmover[1], axes = T, border = NA)
# plot(mn.preds.lno.5kmover[1650], axes = T, border = NA)

# # Sanity check 3
# mn.preds.overlaid.df <- st_set_geometry(mn.preds.lno.5kmover, NULL) %>% #[2:1659]
#   select(starts_with("Mn_76.dens."))
# mn.preds.overlaid.na <- map(mn.preds.overlaid.df, function(i) which(is.na(i)))
# mn.preds.overlaid.temp <- vapply(
#   mn.preds.overlaid.na, function(i, j) {identical(i, j)}, as.logical(1),
#   j = mn.preds.overlaid.na[[1]]
# )
# names(which(!mn.preds.overlaid.temp))
# # "Mn_76.dens.2010.12.27.overlaid" "Mn_76.dens.2010.12.29.overlaid" "Mn_76.dens.2010.12.31.overlaid"
# # ^ Matches pre-overlay


#----------------------------------------------------------
### Additional processing steps for Blake
# 1) For csv file, only include 5km grid ids that have non-NA predictions
# 2) Convert any remaining NA values to '-999'
# 3) Column names: H_YR_MO_DD (bidaily) or HBW_YR_MO_DD (biweekly)
#   H = humpback whale, YR = last 2 digits for year (09 - 18), MO = month (01 - 12), and DD = day (01 - 31), 
#   All ^ with with a leading zero where applicable?
mn.names.curr <- names(st_drop_geometry(mn.preds.lno.5kmover))
mn.out.names <- vapply(strsplit(mn.names.curr[-c(1:2)], "[.]"), function(i) {
  if (i[1] == "Mn_76") {
    paste("H", substr(i[3], 3, 4), i[4], i[5], sep = "_")
    
  } else if (grepl("BiWkSt", i[1])) {
    paste("HBW", substr(i[1], 9, 10), i[2], i[3], sep = "_")
    
  } else {
    stop("Naming oopsie")
  }
}, character(1))


mn.preds.lno.5kmover.out <- mn.preds.lno.5kmover %>% 
  select(GRID5KM_ID, contains("Mn_76.den")) %>%
  filter(!is.na(Mn_76.dens.2009.01.02)) %>% 
  st_set_geometry(NULL) %>% 
  purrr::set_names(c("GRID5KM_ID", mn.out.names)) %>% 
  mutate(H_10_12_27 = -999, H_10_12_29 = -999, H_10_12_31 = -999)

sum(is.na(mn.preds.lno.5kmover.out))
unique(mn.preds.lno.5kmover.out$H_10_12_27)
unique(mn.preds.lno.5kmover.out$H_10_12_29)
unique(mn.preds.lno.5kmover.out$H_10_12_31)


#----------------------------------------------------------
### Write to csv
# write_csv(
#   mn.preds.lno.5kmover.out, 
#   path = "../raimbow-local/Outputs/WEAR5km_76_2005-01-02to2019-08-14_Bidaily_dens.csv"
# )

### Remove pre-2009 data, and write to csv
mn.preds.lno.5kmover.out.2009 <- mn.preds.lno.5kmover.out %>% 
  select(GRID5KM_ID, starts_with(paste0("H_", sprintf("%02d", 9:19))))

write_csv(
  mn.preds.lno.5kmover.out.2009, 
  path = "../raimbow-local/Outputs/WEAR5km_76_2009-01-02to2019-08-14_Bidaily_dens.csv"
)


###############################################################################
# # Grid5km_landerase.R
# system.time(z2 <- st_intersects(grid.5km.ea, land.sfc)) #
# z2.which <- which(sapply(z2, length) > 0)
# system.time(grid.5km.lint <- st_difference(grid.3km[z2.which, ], land.sfc)) #18min
# save(z2.which, grid.5km.lint, file = "RDATA_files/Grid_5km_lint.RDATA")

# # Sanity comparison
# x <- read_csv("../raimbow-local/Outputs/WEAR5km_76_2005-01-02to2019-08-14_Bidaily_dens.csv")
#   y <- read_csv("../raimbow-local/Outputs/WEAR5km_76_Model1_dens_2009-01-02to2018-07-30.csv")
# tail(names(x))
# tail(names(y))



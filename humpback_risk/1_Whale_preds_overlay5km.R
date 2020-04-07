### Overlay KAF Mn preds (~3km grid) onto Blake's 5km EA (land-erased) grid
### By overlaying onto 5km land-erased grid, we don't have to do any 
###   land-erasing from the original predictions

### Currently overlays predictions from 1 Jan 2005 to 14 Aug 2019


###############################################################################
library(eSDM)
library(dplyr)
library(purrr)
library(readr)
library(sf)

path.data <- "../raimbow-local/Data/"
path.rdata <- "../raimbow-local/RDATA_files/"


###############################################################################
# Load Mn predictions and create sf object
mn.preds.csv <- read_csv(paste0(path.data, "Humpback 3km models/WEAR3km_76_2005-01-01to2019-08-14_Bidaily_dens.csv"), 
                         col_types = cols(.default = col_double())) 
mn.preds <- mn.preds.csv %>% 
  select(mlon, mlat, starts_with("76.dens.")) %>% 
  purrr::set_names(~ paste0("Mn_", .)) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant")


# Remove rows of mn.preds with all NA
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
# # [1] "Mn_76.dens.2010.12.27" "Mn_76.dens.2010.12.29" "Mn_76.dens.2010.12.31"
# # ^ Confirmed in KAF original file that these columns are all NA (ROMS issue)
#----------------------------------------------------------

# B/c of ^, we can remove rows with all NA for a quicker overlay
allna <- apply(st_drop_geometry(mn.preds), 1, function(i) all(is.na(i)))
stopifnot(length(allna) == nrow(mn.preds))
which.allna <- unname(which(allna))

mn.preds.nona <- mn.preds %>% slice(-which.allna)

save(mn.preds.nona, file = paste0(path.rdata, "Whale_preds_3kmgrid.RDATA"))


###############################################################################
# Overlay onto 5km EA grid, do sanity checks, do additional 
#   processing steps for Blake, and write to csv

#----------------------------------------------------------
# Load processed Mn preds
load(paste0(path.rdata, "Whale_preds_3kmgrid.RDATA"))

# Load Blake's 5km equal area grid, land-erased
# This file is created in 'raimbow-whaleRisk/Grid5km_landerase.R'
load(paste0(path.rdata, "Grid_5km_landerased.RDATA"))

#----------------------------------------------------------
### Overlay and sanity checks
tmp.over <- overlay_sdm( #~13.5min
  st_geometry(grid.5km.lno), mn.preds.nona, seq_len(ncol(mn.preds) - 1), 0
)

mn.preds.lno.5kmover <- tmp.over %>% 
  mutate(GRID5KM_ID = grid.5km.lno$GRID5KM_ID, 
         area_km_lno = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  select(GRID5KM_ID, area_km_lno, starts_with("Mn_76"))
# identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.lno))
# save(tmp.over, file = paste0(path.rdata, "Whale_preds_overlaid5kmgrid.RDATA"))


load(paste0(path.rdata, "Whale_preds_overlaid5kmgrid.RDATA"))

# Sanity check 1
identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.lno))
eSDM::model_abundance(mn.preds, "Mn_76.dens.2009.01.02")
eSDM::model_abundance(mn.preds.nona, "Mn_76.dens.2009.01.02")
eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.02")
sum(mn.preds.lno.5kmover$Mn_76.dens.2009.01.02 * mn.preds.lno.5kmover$area_km, na.rm = TRUE)

eSDM::model_abundance(mn.preds.nona, "Mn_76.dens.2009.01.04")
eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.04")

# # Sanity check 2
# plot(mn.preds.nona[1650], axes = T, border = NA)
# plot(mn.preds.lno.5kmover[1650], axes = T, border = NA)


#----------------------------------------------------------
### Additional processing steps for Blake
# 1) For csv file, only include 5km grid ids that have non-NA predictions
# 2) Convert any remaining NA values to '-999'
# 3) Column names: H_YR_MO_DD (bidaily) or HBW_YR_MO_DD (biweekly)
#   H = humpback whale, YR = last 2 digits for year (09 - 18), MO = month (01 - 12), and DD = day (01 - 31), 
#   All ^ with with a leading zero where applicable?
mn.names.curr <- mn.preds.lno.5kmover %>% 
  st_drop_geometry() %>% 
  select(starts_with("Mn_")) %>% 
  names()
mn.out.names <- vapply(strsplit(mn.names.curr, "[.]"), function(i) {
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
write_csv(
  mn.preds.lno.5kmover.out,
  path = "../raimbow-local/Outputs/WEAR5km_76_2005-01-02to2019-08-14_Bidaily_dens.csv"
)

### Remove pre-2009 data, and write to csv
mn.preds.lno.5kmover.out.2009 <- mn.preds.lno.5kmover.out %>% 
  select(GRID5KM_ID, starts_with(paste0("H_", sprintf("%02d", 9:19))))

write_csv(
  mn.preds.lno.5kmover.out.2009, 
  path = "../raimbow-local/Outputs/WEAR5km_76_2009-01-02to2019-08-14_Bidaily_dens.csv"
)

###############################################################################

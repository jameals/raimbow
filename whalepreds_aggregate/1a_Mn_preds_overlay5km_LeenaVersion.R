### Overlay KAF Mn preds (~3km grid) onto Blake's 5km EA (land-erased) grid
### By overlaying onto 5km land-erased grid, we don't have to do any 
###   land-erasing from the original predictions

### Currently overlays predictions from 1 Jan 2005 to 14 Aug 2019


###############################################################################
library(dplyr)
# devtools::install_github("smwoodman/eSDM") #Currently need dev version of eSDM
library(eSDM)
library(purrr)
library(readr)
library(sf)

source(here::here("whalepreds_aggregate", "Whalepreds_aggregate.R"), local = TRUE, echo = FALSE)
source(here::here("User_script_local.R"), local = TRUE, echo = FALSE)

if (user == "JS") {
  
} else if (user == "SMW") {
  path.mn.preds <- "C:/SMW/RAIMBOW/raimbow-local/Data/Humpback 3km models/WEAR3km_76_2005-01-01to2019-08-14_Bidaily_dens.csv"
  path.grid.5km.lno <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.rds"
  
  file.out.all.csv <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens_allyrs.csv"
  file.out.csv <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens.csv"
  file.out.rds <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens.rds"
}

#Leena
#this was the original input file, but what Karin shared (https://drive.google.com/drive/folders/1DSnNFRTpGcjPkLEvUkuf1ADBWwDEgQRY)
#doesn't seem to have bidaily data files, only monthly or biweekly
#This is for original input data:
#path.mn.preds <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback 3km models/Model1_working model for testing/WEAR3km_76_2005-01-01to2019-08-14_Bidaily_dens.csv"
#this is for new data (testing biweekly or montly  data file):
path.mn.preds <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Humpback 3km models/Mn_3km_2005-01-01to2020-09-29_monthly.csv"

path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
#path.save2 <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.RDATA"
#load(path.save2)

file.out.all.csv <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_bidaily_dens_allyrs.csv"
file.out.csv <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_bidaily_dens.csv"
file.out.rds <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_bidaily_dens.rds"

###############################################################################
# Load Mn predictions and create sf object - reading CSV takes a while
mn.preds.csv <- read_csv(path.mn.preds, col_types = cols(.default = col_double())) 
mn.preds <- mn.preds.csv %>% 
  #for 2005-2019 data
  #select(mlon, mlat, starts_with("76.dens.")) %>%  #I think column names have changed, can't see anything like "76.dens"
  #for 2005-2020 data
  select(mlon, mlat, starts_with("Avg_monthly")) %>% 
  purrr::set_names(~ paste0("Mn_", .)) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant") #this works in R v4


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

#adjust above code for 2005-2020 dataset
mn.preds.df <- st_drop_geometry(mn.preds) %>% #[2:1659]
  select(starts_with("Mn_Avg_monthly"))
mn.preds.na <- map(mn.preds.df, function(i) which(is.na(i)))
mn.preds.temp <- vapply(
     mn.preds.na, function(i, j) {identical(i, j)}, as.logical(1),
     j = mn.preds.na[[1]]
   )
names(which(!mn.preds.temp))
#character(0)
#maybe now as monthly level, no missing cases (no missing FULL months)
#----------------------------------------------------------

# B/c of ^, we can remove rows with all NA for a quicker overlay
allna <- apply(st_drop_geometry(mn.preds), 1, function(i) all(is.na(i)))
stopifnot(length(allna) == nrow(mn.preds))
which.allna <- unname(which(allna))

mn.preds.nona <- mn.preds[-which.allna, ]


###############################################################################
# Overlay onto 5km EA grid, do sanity checks, do additional 
#   processing steps requested by Blake, and write to csv

### Land-erased polygon
#grid.5km.lno <- readRDS(path.grid.5km.lno)
glimpse(grid.5km.lno)

### Overlay and sanity checks
tmp.over <- overlay_sdm( #~13.5min on Sam's computer #this is the part that doesn't work with R v4, but worked in R 3.6, took ~20min, ~10min on monthly data 2005-2020
  st_geometry(grid.5km.lno), mn.preds.nona, seq_len(ncol(mn.preds) - 1), 
  overlap.perc = 0
)

mn.preds.lno.5kmover <- tmp.over %>% 
  mutate(GRID5KM_ID = grid.5km.lno$GRID5KM_ID, 
         area_km_lno = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  #for 2005-2019 data
  #select(GRID5KM_ID, area_km_lno, starts_with("Mn_76")) #I think column names have changed
  #for 2005-2020 data
  select(GRID5KM_ID, area_km_lno, starts_with("Mn_Avg_monthly"))


# # Sanity check 0
# identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.lno))

# # Sanity check 1 ##some of these only work on the orginal which was bidaily, won't work on monthly file from Karin
# identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.lno))
# eSDM::model_abundance(mn.preds, "Mn_76.dens.2009.01.02")
# eSDM::model_abundance(mn.preds.nona, "Mn_76.dens.2009.01.02")
# eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.02")
# sum(mn.preds.lno.5kmover$Mn_76.dens.2009.01.02 * mn.preds.lno.5kmover$area_km, na.rm = TRUE)
# 
# eSDM::model_abundance(mn.preds.nona, "Mn_76.dens.2009.01.04")
# eSDM::model_abundance(mn.preds.lno.5kmover, "Mn_76.dens.2009.01.04")

# # Sanity check 2
# plot(mn.preds.nona[1650], axes = T, border = NA)
# plot(mn.preds.lno.5kmover[1650], axes = T, border = NA)
#on a monthly input file use:
#plot(mn.preds.nona[190], axes = T, border = NA)
#plot(mn.preds.lno.5kmover[190], axes = T, border = NA)

#----------------------------------------------------------
### Additional processing steps for Blake
# 1) For csv file, only include 5km grid ids that have non-NA predictions
# 2) Convert any remaining NA values to '-999'
# 3) Column names: H_YR_MO_DD (bidaily) or HBW_YR_MO_DD (biweekly)
#   H = humpback whale, YR = last 2 digits for year (09 - 18), MO = month (01 - 12), and DD = day (01 - 31), 
#   All ^ with with a leading zero where applicable?


#I think column names have changed
#I get lost here
mn.names.curr <- mn.preds.lno.5kmover %>% 
  st_drop_geometry() %>% 
  select(starts_with("Mn_")) %>% 
  names()
mn.out.names <- vapply(strsplit(mn.names.curr, "[.]"), function(i) {
  if (i[1] == "Mn_76") {
    paste("H", substr(i[3], 3, 4), i[4], i[5], sep = "_") ##this section applies only if original input was bidaily? #column names in this file are "Mn_76.dens.2019.08.14"
    
  } else if (grepl("BiWkSt", i[1])) {
    paste("HBW", substr(i[1], 9, 10), i[2], i[3], sep = "_") #column names in this file are "BiWkSt2018.01.01" --> so 9 & 10 refer to the year (18), and i[2] and i[3] grab the month and day?
    
  } else {
    stop("Naming oopsie")
  }
}, character(1))

#adjust above for new monthly input data
mn.names.curr <- mn.preds.lno.5kmover %>% 
  st_drop_geometry() %>% 
  select(starts_with("Mn_")) %>% 
  names()
mn.out.names <- vapply(strsplit(mn.names.curr, "[_]"), function(i) { #new file uses _ instead of . to separate column name elements?
  if (i[1] == "Mn") {
    paste("H", substr(i[4], 3, 4), i[5], i[6], sep = "_") ##this section applies only if original input was bidaily? #column names in this file are "Mn_76.dens.2019.08.14"
    
  }  else {
    stop("Naming oopsie")
  }
}, character(1))





#this would need to be adjusted as input was not bidaily...
mn.preds.lno.5kmover.out <- mn.preds.lno.5kmover %>% 
  #select(GRID5KM_ID, area_km_lno, contains("Mn_76.den")) %>% #this is for bidaily input file
  select(GRID5KM_ID, area_km_lno, contains("Mn_Avg_monthly")) %>%
  #filter(!is.na(Mn_76.dens.2009.01.02)) %>% 
  filter(!is.na(Mn_Avg_monthly_2005_01_01)) %>% 
  st_drop_geometry() %>% 
  purrr::set_names(c("GRID5KM_ID", "area_km_lno", mn.out.names)) #%>% 
  #mutate(H_10_12_27 = -999, H_10_12_29 = -999, H_10_12_31 = -999) #this line doesn't apply to monthly inout file

sum(is.na(mn.preds.lno.5kmover.out))
#these don't apply to monthly input file
unique(mn.preds.lno.5kmover.out$H_10_12_27)
unique(mn.preds.lno.5kmover.out$H_10_12_29)
unique(mn.preds.lno.5kmover.out$H_10_12_31)


#----------------------------------------------------------
### Write to csv and rds
#for monthly data
file.out.all.csv <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_MONTHLY2005_2020_dens_allyrs.csv"
file.out.csv <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_MONTHLY2009_2020_dens.csv"
file.out.rds <- "E:/Leena/Documents/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_MONTHLY2009_2020_dens.rds"

write_csv(mn.preds.lno.5kmover.out, path = file.out.all.csv)

### Remove pre-2009 data
mn.preds.lno.5kmover.out.2009 <- mn.preds.lno.5kmover.out %>% 
  #select(GRID5KM_ID, area_km_lno, starts_with(paste0("H_", sprintf("%02d", 9:19)))) #this current code cuts data at end of 2019
  select(GRID5KM_ID, area_km_lno, starts_with(paste0("H_", sprintf("%02d", 9:20)))) 

write_csv(mn.preds.lno.5kmover.out.2009, path = file.out.csv)
saveRDS(mn.preds.lno.5kmover.out.2009, file = file.out.rds)

###############################################################################

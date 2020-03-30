# Get humpback densities
# fyi humpback.sum is contained within Whale_risk_monthly_summ.RData but JS and SW decided it was cleaner to avoid loading all the other df's in that RData file

###############################################################################
library(dplyr)
library(purrr)
library(readr)
library(sf)
library(tidyr)

##
#root.dir <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/"

## directory where VMS data that has been matched to fish tickets, vessel lengths, blwh predictions, and includes regional identifiers (CDFW offshore, etc)
#orig_dir = "/Output_Data/"

#root.dir_Mn <- "~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/"

source("~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Analysis/Whale risk/Preprocessing_whalepredictions.R", local = TRUE, echo = FALSE)

### 1) Update this so all desired year/months are included
# (This controls the dates for which risk is calculated, 
#   and then displayed in the plots)
# this example df.key.ym runs from Nov 2009 thru Jul 2018
df.key.ym <- tibble(
  year = c(rep(2009, 2), sapply(2010:2018, rep, 12), rep(2019, 7)),
  month = sprintf("%02d", c(11, 12, rep(1:12, 9), 1:7))
)

###############################################################################
###############################################################################
# Humpback predictions processing

### Read in KAF Mn predictions that have been overlaid onto Blake's 5km EA grid

##

humpback.raw <- read_csv(paste0(root.dir_Mn,"Input_Data/Humpback whale data/Forney et al./","WEAR5km_76_Model1_dens_2009-01-02to2018-07-30.csv")) %>% 
  mutate(H_10_12_27 = NA, H_10_12_29 = NA, H_10_12_31 = NA)

grid.5km.geom <- st_read(paste0(root.dir_Mn,"Input_Data/Grids/5x5 km grid shapefile/five_km_grid_polys_geo.shp"))


#------------------------------------------------------------------------------
### Summarize KAF Mn preds by month, and format
###   Using the function above
### Join with areas from 5km grid with land erased for abundance calcs

##

load(paste0(root.dir_Mn,"Input_Data/Grids/","Grid_5km_landerased.RDATA"))

##

range.dates <- seq(
  from = as.Date("2008-01-01"), to = as.Date("2018-08-01"), by = "months"
)

# aggregating bidaily predictions by month
humpback.sum.dens <- raimbow_pre_whalepreds_aggregate(
  humpback.raw, 2:2481, 3:10, aggr.level = NULL, range.dates = range.dates, 
  se.calc = FALSE #Note: would need to change naming below if se.calc = TRUE
)  %>% 
  set_names(c("GRID5KM_ID", paste0("H_Avg_", substr(names(.)[-1], 10, 16)))) %>% 
  left_join(grid.5km.geom, by = "GRID5KM_ID") %>% 
  left_join(st_set_geometry(grid.5km.lno, NULL)) %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno, starts_with("H_Avg_"))

## Jameal asked Sam about this warning. Sam says it's fine, just b/c hump predictions go back to 2005. Warning message:
# In raimbow_pre_whalepreds_aggregate(humpback.raw, 2:2481, 3:10,  :
#                                       Not all column dates fall within an interval defined by range.dates

# Check that there are no NA humpback predictions
stopifnot(!any(is.na(humpback.sum.dens)))

# Calculate abundances because we care more about the number of whales rather than their densities? good assumption to revisit
tmp <- select(humpback.sum.dens, starts_with("H_Avg"))
humpback.sum.abund <- as.data.frame(lapply(tmp, function(i, j) {
  i * j
}, j = humpback.sum.dens$area_km_lno)); rm(tmp)

humpback.sum <- humpback.sum.dens %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno) %>% 
  bind_cols(humpback.sum.abund)

rm(humpback.sum.abund, humpback.sum.dens)

glimpse(humpback.sum)
head(as.data.frame(humpback.sum))

# this is in wide format, change to long so that column headers are:
# "GRID5KM_ID"    "LONGITUDE"     "LATITUDE"      "area_km_lno" "Year_Month" "H_Avg". 

#note area_km_lno is the area of the grid cell after land has been erased. So it is ok to go to/from abundance and density
# how to add leading zeroes: https://stackoverflow.com/questions/5812493/how-to-add-leading-zeros
humpback.sum.long <- humpback.sum %>% 
  gather(key = "col_name", value = "H_Avg_Abund", 
         -GRID5KM_ID, -LONGITUDE, -LATITUDE, -area_km_lno) %>% 
  mutate(year = as.numeric(substr(col_name, 7, 10)), 
         month = as.numeric(substr(col_name, 12, 13)), 
         Year_Month = paste(year, sprintf("%02d", month), sep = "_")) %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno, Year_Month, H_Avg_Abund)

tail(as.data.frame(humpback.sum.long))

write.csv(humpback.sum.long, paste0(root.dir,orig_dir,"Humpback whale abundance monthly abundance predictions 2009-2018.csv"), row.names = FALSE)

###############################################################################

##
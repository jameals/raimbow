# Code for creating maps for Jameal's Ocean Visions presentation; 
#   expects to be run in SMW's RAIMBOW project. 
#   Thus, other users will need to update file paths
# By Sam Woodman, April 2019


###############################################################################
library(dplyr)
library(maps)
library(readr)
library(sf)

source("JS_OceanVisions/JS_OceanVisions_map_funcs.R")

# Determine user and thus relevant file paths
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  path.data.grid <- "../raimbow-local/Data/5x5 km grid shapefile/"
  path.data.bia <- "../raimbow-local/Data/CetMap_BIA_WGS84/"
  path.ov <- "../raimbow-local/JS_OceanVisions/"
  path.plots <- "../raimbow-local/JS_OceanVisions/Plots/"
  
} else {
  stop("User not recognized")
}



###############################################################################
#------------------------------------------------------------------------------
### Blake's 5km grid
grid.5km.ea <- st_read(paste0(path.data.grid, "five_km_grid_polys_geo.shp"), 
                       stringsAsFactors = FALSE)

### Plot map
map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
map.base2 <- st_geometry(st_as_sf(maps::map('state', plot = FALSE, fill = TRUE)))

### BIAs
bia.sf <- st_read(paste0(path.data.bia, "CetMap_BIA_WGS84.shp"), 
                  stringsAsFactors = FALSE) 


#------------------------------------------------------------------------------
### Read in and process file from Jameal
# Column 'B_or_A_April1': "pre" means pre-April 1; "post" means April 1 and after
# Column 'Vessel_Size':   "small" means less than 45'; "large" means at least 45'
x <- read_csv(paste0(path.ov, "Whale and Dungeness crab fishery risk for 2016-17 averaged across 5km grid cells.csv")) %>% 
  mutate(B_or_A_April1 = ifelse(B_or_A_April1 == "April 1 and After", "post", "pre"), 
         BIA_mn_noNAs = ifelse(is.na(BIA_mn_noNAs), 0, BIA_mn_noNAs), 
         BIA_bm_noNAs = ifelse(is.na(BIA_bm_noNAs), 0, BIA_bm_noNAs), 
         Vessel_Size = ifelse(Vessel_Size == "<45 ft", "small", "large"), 
         Region = ifelse(is.na(Region), "Other", Region))

### File-specific processing 1
# Replace mean/risk values of 'NA' with '0'
x.tmp <- x %>% select(starts_with("mean_"))
x.tmp[is.na(x.tmp)] <- 0

x <- x %>% 
  select(-starts_with("mean_")) %>% 
  bind_cols(x.tmp)
rm(x.tmp)

### File-specific processing 2
# For three grid cells with both CenCA and NorCA entries (63522, 63523, 63524),
#   select only the NorCA entries
#   Did this because 63522 and 63523 only have NorCA entries for post-Apr 1
# x.tab <- table(x$GRID5KM_ID)
# > x.tab[x.tab != 4]
# 
# 63522 63523 63524 
# 6     6     8 
d <- x %>% 
  filter(GRID5KM_ID %in% c(63522, 63523, 63524), 
         Region == "NorCA")
x <- x %>% 
  filter(!(GRID5KM_ID %in% c(63522, 63523, 63524))) %>% 
  bind_rows(d) %>% 
  arrange(GRID5KM_ID, B_or_A_April1)
rm(d)


#----------------------------------------------------------
### Join x with 5km grid sf object
x.sf <- grid.5km.ea %>% 
  left_join(x) %>% 
  filter(!is.na(B_or_A_April1))
length(unique(x.sf$geometry))


#----------------------------------------------------------
# # Sam's experimental code - can ignore
# st_bbox(x.sf)
# plot(st_geometry(x.sf), axes = TRUE)
# plot((x.sf["Region"]), axes = TRUE)
# 
# bia.sfc <- bia.sf %>% 
#   filter(sci_name == "Megaptera novaeangliae",
#          region == "West Coast") %>%
#   # filter(sci_name == "Balaenoptera musculus") %>%
#   st_geometry()
# plot(st_geometry(x.sf), axes = TRUE)
# plot(st_geometry(x.sf %>% filter(BIA_bm_noNAs == 1)), add = T, border = "green", col = NA)
# plot(bia.sfc, add = T, border = "red", col = NA)
# 
# plot(st_geometry(x.sf), axes = TRUE)
# z <- st_union(st_combine(x.sf %>% filter(BIA_bm_noNAs == 1, !duplicated(GRID5KM_ID))))
# plot(z, add = T, border = "red")
# plot(bia.sfc, add = T, border = "green", col = NA)


#----------------------------------------------------------
###############################################################################
# Make maps of risk
# TODO: User must change 'Prep for plotting' objects and ensure they're
#   consistent with one another, 
#   as well as comment/uncomment appropriate sections in plot_ov_bia()

#----------------------------------------------------------
# Pertinent column names of x
# "mean_Hump_risk_lbs"      "mean_Hump_risk_dollars"  "mean_Hump_risk_pings"    "mean_Hump_risk_vessels"
# "mean_Blue_risk_lbs"      "mean_Blue_risk_dollars"  "mean_Blue_risk_pings"    "mean_Blue_risk_vessels"
# "mean_lbs_DCRB" "mean_dollars_DCRB"       "mean_Num_DCRB_VMS_pings" "mean_Num_DCRB_Vessels"

#----------------------------------------------------------
# User todo - see instructions about
x.curr.col <- "mean_Hump_risk_dollars"
x.curr <- x.sf %>%
  select(B_or_A_April1, Vessel_Size, !!x.curr.col)
plot.main.curr <- "Humpback risk ($)" 
filename.curr <- "MeanHumpRiskDollars_perc_mn.png"
col.pal <- rev(RColorBrewer::brewer.pal(6, "Spectral"))

# # Sanity plot
# tmp <- x.curr %>% filter(B_or_A_April1 == "post", Vessel_Size == "small")
# plot(tmp[x.curr.col], axes = TRUE, key.length = 1, border = NA); rm(tmp)

#----------------------------------------------------------
# Make and save plots (after running prep section above)
# Uncomment appropriate BIA args 
#   If bia.curr = NULL, then comment out both bia.leg.txt

png(paste0(path.plots, filename.curr), units = "in", width = 6.5, height = 9.5, res = 300) 
#width of 6.5 for '$', 8 for 'vessels' or 'pings in title
plot_ov_bia(
  x.curr, x.curr.col, map.base2, plot.main.curr,
  b.val.type = "perc", col.pal, 
  # bia.curr = filter(bia.sf, sci_name == "Megaptera novaeangliae", region == "West Coast"),
  bia.curr = filter(bia.sf, sci_name == "Balaenoptera musculus"),
  # bia.curr = NULL,
  # bia.leg.txt = "Humpback whale BIA",
  bia.leg.txt = "Blue whale BIA",
  cdfw.offshore = TRUE
)
dev.off()

###############################################################################

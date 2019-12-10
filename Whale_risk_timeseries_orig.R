###############################################################################
### Calculate and summarize (via time series) Mn entanglement risk using:
###   1) Karin's 3km Mn predictions overlayed onto Blake's 5km grid
###   2) monthly VMS data. 

# Renamed from Whale_risk_monthly_summ_2009-18.R


###############################################################################
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(tidyr)


###############################################################################
# Determine user and thus relevant file paths
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  file.data.mnpreds <- "../raimbow-local/Outputs/WEAR5km_76_Model1_dens_2009-01-02to2018-07-30.csv"
  file.data.grid <- "../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  file.vms <- paste0("../raimbow-local/Data/Non-confidential VMS monthly data/", 
                     "Non-confidential VMS data summarized by 5km grid cell 2009-18 All 3 states.csv")
  path.rdata <- "../raimbow-local/RDATA_files/"
  path.plots <- "../raimbow-local/Plots/Risksum_linear_monthly_nonconfidential_0918/"
  
} else {
  stop("User not recognized")
}

source("whalepreds_aggregate/Whalepreds_aggregate.R", local = TRUE, echo = FALSE)


###############################################################################
# TODO USER: 

### 1) Update this so all desired year/months are included
# (This controls the dates for which risk is calculated, and then displayed in the plots)
# df.key.ym <- tibble(
#   year = c(rep(2015, 2), sapply(2016:2017, rep, 12)), 
#   month = sprintf("%02d", c(11, 12, rep(1:12, 2)))
# )
# # Below is example df.key.ym for Nov 2009 - Dec 2018
# df.key.ym <- tibble(
#   year = c(rep(2009, 2), sapply(2010:2018, rep, 12)),
#   month = sprintf("%02d", c(11, 12, rep(1:12, 9)))
# )

df.key.ym <- tibble(
  year = c(rep(2009, 2), sapply(2010:2017, rep, 12), rep(2018, 7)),
  month = sprintf("%02d", c(11, 12, rep(1:12, 8), 1:7))
)

### 2) Specify how to represent fishing effort, ie column name from file.vms
# vms.colname <- "dollars_DCRB_noncon"
vms.colname <- "Num_DCRB_VMS_pings_noncon"


###############################################################################
###############################################################################
# Humpback predictions processing

### Read in KAF Mn predictions that have been overlaid onto Blake's 5km EA grid
humpback.raw <- read_csv(file.data.mnpreds) %>% 
  mutate(H_10_12_27 = NA, H_10_12_29 = NA, H_10_12_31 = NA)
grid.5km.geom <- st_read(file.data.grid)


#------------------------------------------------------------------------------
### Summarize KAF Mn preds by month, and format
###   Using the function above
### Join with areas from 5km grid with land erased for abundance calcs
load(paste0(path.rdata, "Grid_5km_landerased.RDATA"))

range.dates <- seq(
  from = as.Date("2008-01-01"), to = as.Date("2018-08-01"), by = "months"
)

humpback.sum.dens <- whalepreds_aggregate(
  humpback.raw, 2:2481, 3:10, aggr.level = NULL, range.dates = range.dates, 
  se.calc = FALSE #Note: would need to change naming below if se.calc = TRUE
)  %>% 
  set_names(c("GRID5KM_ID", paste0("H_Avg_", substr(names(.)[-1], 10, 16)))) %>% 
  left_join(grid.5km.geom, by = "GRID5KM_ID") %>% 
  left_join(st_set_geometry(grid.5km.lno, NULL)) %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno, starts_with("H_Avg_"))

# Check that there are no NA humpback predictions
stopifnot(!any(is.na(humpback.sum.dens)))

# Calculate abundances
tmp <- select(humpback.sum.dens, starts_with("H_Avg"))
humpback.sum.abund <- as.data.frame(lapply(tmp, function(i, j) {
  i * j
}, j = humpback.sum.dens$area_km_lno)); rm(tmp)

humpback.sum <- humpback.sum.dens %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno) %>% 
  bind_cols(humpback.sum.abund)

rm(humpback.sum.abund, humpback.sum.dens)


###############################################################################
# Fishing data processing

#------------------------------------------------------------------------------
### Process fishing data; commented out code is for looking at the amount
#   of confidential data
fish.summ <- read_csv(file.vms)  %>%
  rename(VMS_curr = !!vms.colname) %>%
  mutate(VMS_metric = suppressWarnings(as.numeric(VMS_curr)),
         year_mo = paste0("DC_", year_mo)) %>%
  group_by(year_mo, GRID5KM_ID) %>%
  summarise(year = unique(year),
            confid_any = any(VMS_curr == "CONFIDENTIAL"),
            VMS_sum = sum(VMS_metric, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(confid_and_num = confid_any & (VMS_sum > 0)) %>%
  select(year, year_mo, GRID5KM_ID, confid_any, confid_and_num, 
         VMS_sum)

### fish.out1 needs to be a data frame with 1) the grid ID (GRID5KM_ID) in the
#   first column, and 2) all subsequent columns being the monthly sums of 
#   the fishing data with names paste0("DC_", year_mo)
fish.out1 <- fish.summ %>%
  select(year_mo, GRID5KM_ID, VMS_sum) %>%
  spread(key = year_mo, value = VMS_sum)

### Make data frame have rows for all grid cells with either Mn or fish data
fish.out <- humpback.raw %>%
  select(GRID5KM_ID) %>%
  left_join(st_set_geometry(grid.5km.geom, NULL)) %>%
  left_join(fish.out1) %>% 
  select(GRID5KM_ID, LONGITUDE, LATITUDE, starts_with("DC"))

rm(fish.out1)
stopifnot(!any(duplicated(fish.out$GRID5KM_ID)))


# ### SMW experimental: Plot grid cells that have data but not Mn predictions
# fish.raw <- read_csv(file.vms)
# all(fish.raw$GRID5KM_ID %in% fish.out$GRID5KM_ID)
# d <- unique(fish.raw$GRID5KM_ID[!(fish.raw$GRID5KM_ID %in% fish.out$GRID5KM_ID)])
# plot(grid.5km.geom %>% filter(GRID5KM_ID %in% d) %>% st_geometry(), axes = TRUE,
#      xlim = c(-127, -120), ylim = c(35, 42), col = "red", border = "red")
# plot(st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))),
#      add = TRUE, border = "tan", col = NA)
# rm(d)


###############################################################################
# For loop for calculating risk and (if desired) writing 72 csv files

#------------------------------------------------------------------------------
# Prep - create 'key' data frame

### Using monthly VMS data
# df.key set up at the top of the script in 'todo user' area


### Add Mn and fish data column names to data frame
df.key <- df.key.ym %>% 
  mutate(humpback_name = paste("H_Avg", year, month, sep = "_"), 
         fish_name = paste("DC", year, month, sep = "_"))

risk.all <- humpback.all <- fish.all <- 
  humpback.sum %>% select(GRID5KM_ID, LONGITUDE, LATITUDE, area_km_lno)
name.idx <- ncol(risk.all)


# # Run the following to rescale whale and fishing values to 0-1
# #   Does not change relative patterns because risk is linear
# range(humpback.sum[, -c(1:4)], na.rm = TRUE)
# range(fish.out[, -c(1:3)], na.rm = TRUE)
# 
# humpback.sum <- cbind(
#   humpback.sum[, c(1:4)], 
#   humpback.sum[, -c(1:4)] / max(humpback.sum[, -c(1:4)], na.rm = TRUE)
# )
# fish.out <- cbind(
#   fish.out[, c(1:3)], 
#   fish.out[, -c(1:3)] / max(fish.out[, -c(1:3)], na.rm = TRUE)
# )


#------------------------------------------------------------------------------
### For loop; must set appropriate path below for output
for (i in 1: nrow(df.key)) {
  # Conditional for months where there is no fishing data, and thus no column
  #   in fishing data frame
  if (df.key$fish_name[i] %in% names(fish.out)) {
    df.out <- data.frame(
      humpback.sum %>% select(GRID5KM_ID, df.key$humpback_name[i]), 
      fish.out %>% select(df.key$fish_name[i])
    )
  } else {
    df.out <- data.frame(
      humpback.sum %>% select(GRID5KM_ID, df.key$humpback_name[i]), 
      as.numeric(NA)
    )
  }
  
  # Calculate risk, and add data to applicable data frames
  df.out <- df.out %>% 
    set_names(c("GRID5KM_ID", "humpback_curr", "fish_curr")) %>% 
    mutate(risk_curr = humpback_curr * fish_curr)
  
  risk.all <- cbind(risk.all, df.out$risk_curr)
  names(risk.all)[i+name.idx] <- paste("Mn_DC_risk", df.key$year[i], df.key$month[i], sep = "_")
  
  humpback.all <- cbind(humpback.all, df.out$humpback_curr)
  names(humpback.all)[i+name.idx] <- paste("Mn", df.key$year[i], df.key$month[i], sep = "_")
  
  fish.all <- cbind(fish.all, df.out$fish_curr)
  names(fish.all)[i+name.idx] <- paste("DC", df.key$year[i], df.key$month[i], sep = "_")
  
  
  # file.out <- paste0("Mn_DC_risk", df.key$year[i], "_", df.key$month[i], ".csv")
  # write.csv(df.out, file = file.out, row.names = FALSE)
  
  rm(df.out) #, file.out)
}; rm(i, name.idx)


# d <- lapply(fish.all, function(i) head(sort(i, decreasing = TRUE)))
# d2 <- lapply(fish.all, function(i) fish.all$GRID5KM_ID[which.max(i)])
# rm(d, d2)

#------------------------------------------------------------------------------
# Post-processing

### Monthly
risk.nona.any <- apply(select(risk.all, starts_with("Mn_DC_risk")), 1, function(i) any(!is.na(i))) 
fish.nona.any <- apply(select(fish.out, starts_with("DC_")), 1, function(i) any(!is.na(i)))
sum(risk.nona.any) #906; Missing 3 are b/c fish did not overlap with Mn preds
sum(fish.nona.any) #907; Missing 3 are b/c fish did not overlap with Mn preds

# Fishing data has smaller spatial footprint, and thus fish NAs should match risk NAs
#   However, it shouldn't be a show-stopper if this isn't true
# identical(risk.nona.any, fish.nona.any)

# Will do all plotting with nona data frames for ease of computation, 
#   but mostly for summing data only across cells that have risk
risk.all.nona <- risk.all[risk.nona.any, ]
humpback.all.nona <- humpback.all[risk.nona.any, ]
fish.all.nona <- fish.all[risk.nona.any, ]


# # Save RDATA file that can be used in future if running just plotting code
# save.image(paste0(path.rdata, "Whale_risk_timeseries.Rdata"))

# # Save files for Whale_risk_maps
# save(
#   df.key.ym, risk.all, fish.out, humpback.all,
#   file = paste0(path.rdata, "Whale_risk_formaps.Rdata")
# )


###############################################################################
###############################################################################


###############################################################################
# Plot regional risk, Mn abundance, and fish sum in several formats
#   To make plots, can either run all code above or uncomment load() call below


#------------------------------------------------------------------------------
# Prep

library(dplyr)
library(purrr)
library(readr)
library(sf)
# load(paste0(path.rdata, "Whale_risk_monthly_summ_2009-18.Rdata"))
source("Whale_risk_timeseries_funcs.R", local = TRUE, echo = FALSE)

risk.all.curr <- risk.all.nona
humpback.all.curr <- humpback.all.nona
fish.all.curr <- fish.all.nona

# # TODO USER: specify path to where plots should be saved
# plot.filepath <- "Plots/Risksum_linear_monthly_nonconfidential_0918/"

# Define regions; regions are processed as (min, max]
reg.bound <- c(32.5, 34.4, 36.3, 38.76683, 42, 46.25, 50)

reg.uswc <- c(reg.bound[1], reg.bound[7])
reg.wa <- c(reg.bound[6], reg.bound[7])
reg.or <- c(reg.bound[5], reg.bound[6])
reg.ca.north <- c(reg.bound[4], reg.bound[5])
reg.ca.central <- c(reg.bound[3], reg.bound[4])
reg.ca.southcentral <- c(reg.bound[2], reg.bound[3])
reg.ca.south <- c(reg.bound[1], reg.bound[2])


#------------------------------------------------------------------------------
# Plot each region individually (i.e. in black and white in it's own plot)

#----------------------------------------------------------
### Prep
regions.list <- list(
  "USWC" = reg.uswc, WA = reg.wa, OR = reg.or, 
  "CA-N" = reg.ca.north, "CA-Cen" = reg.ca.central, 
  "CA-SCen" = reg.ca.southcentral, "CA-S" = reg.ca.south
)
regions.main <- c(
  "Whole coast", "Washington", "Oregon", "California - North", 
  "California - Central", "California - South-Central", "California - South"
)

### Make and save individual, 3-panel plots for the whole US west coast
png(paste0(path.plots, "Linear_humpback_risk_wholecoast.png"), width = 6, height = 7, units = "in", res = 300)
risksum_plot_3row(
  risk.all.curr, humpback.all.curr, fish.all.curr,
  df.key, reg.uswc, plot.main = "Whole coast", par.flag = TRUE
)
dev.off()


### Make and save individual, 3-panel plots for each region
regions.filename <- c(
  "Linear_humpback_risk_wholecoast.png", "Linear_humpback_risk_wa.png", 
  "Linear_humpback_risk_or.png", "Linear_humpback_risk_ca-n.png", 
  "Linear_humpback_risk_ca-cen.png", "Linear_humpback_risk_ca-scen.png", 
  "Linear_humpback_risk_ca-s.png"
)
for (i in seq_along(regions.list)) {
  png(paste0(path.plots, "Regions/", regions.filename[i], ".png"),
      width = 6, height = 7, units = "in", res = 300)
  risksum_plot_3row(
    risk.all.curr, humpback.all.curr, fish.all.curr,
    df.key, regions.list[[i]], plot.main = regions.main[i], par.flag = TRUE
  )
  dev.off()
}; rm(i)


### Make and save one, tall plot/file for all regions
png(paste0(path.plots, "Linear_humpback_risk_all.png"), width = 6, height = 7*6, units = "in", res = 300)
par(mfrow = c(3 * 7, 1))
for (i in seq_along(regions.list)) {
  risksum_plot_3row(
    risk.all.curr, humpback.all.curr, fish.all.curr, 
    df.key, regions.list[[i]], plot.main = regions.main[i], par.flag = FALSE
  )
}; rm(i)
dev.off()


#------------------------------------------------------------------------------
# Plot color-coded, regional sums on a single plot
#   Plot both raw and area-normalized sums. Also print areas of each region
#   Area-normalized means that the sums (risk, Mn abund, fish) in each region
#   is dividied by the area of risk cells with any non-NA values in that region

### Flag for if thirdly VMS data is being used
thirdly.flag.curr <- FALSE

### Regions in which to sum values for plots. CA-South is not included because
###   there is (virtually) no fishing there
### To plot only data from CA, you can comment out the WA and OR lines in 
###   'regions.list2'
regions.list2 <- list(
  "WA" = reg.wa, "OR" = reg.or,
  "CA-N" = reg.ca.north, "CA-Cen" = reg.ca.central, "CA-SCen" = reg.ca.southcentral
)

### Make and save plots
png(paste0(path.plots, "Linear_humpback_risk_regional.png"), width = 8, height = 10, units = "in", res = 300)
risksum_plot_allin1(
  risk.all.curr, humpback.all.curr, fish.all.curr,
  df.key, regions.list2, col.pal = "Set1",
  plot.main = c("Risk sum", "Humpback sum (abundance)", "Fishing sum (# VMS points)"),
  area.flag = FALSE, par.flag = TRUE, thirdly.flag = thirdly.flag.curr
)
dev.off()

png(paste0(path.plots, "Linear_humpback_risk_regional_areanormalized.png"), width = 8, height = 10, units = "in", res = 300)
risksum_plot_allin1(
  risk.all.curr, humpback.all.curr, fish.all.curr,
  df.key, regions.list2, col.pal = "Set1",
  plot.main = c("Risk sum - area normalized", "Humpback sum (abundance) - area normalized",
                "Fishing sum (# VMS points) - area normalized"),
  area.flag = TRUE, par.flag = TRUE, thirdly.flag = thirdly.flag.curr
)
dev.off()

#------------------------------------------------------------------------------

###############################################################################
# SMW experimental - can ignore

### Create map of regions for visulaization purposes

# map.base1 <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
# map.base2 <- st_geometry(st_as_sf(maps::map('state', plot = FALSE, fill = TRUE)))
# grid.toplot <-risk.all.curr %>%
#   select(GRID5KM_ID) %>%
#   left_join(grid.5km.lno) %>%
#   st_as_sf() %>%
#   st_geometry()
# reg.txt <- c(
#   "WA", "OR", "CA-N", "CA-Cen", "CA-SCen", "CA-S"
# )
# reg.areas <- c("9,765", "15,721", "8,392", "10,708", "3,601", "303")
# 
# 
# png(paste0(path.plots, "Region_map.png"), height = 8.5, width = 5, units = "in", res = 300)
# plot(map.base1, axes = TRUE, border = NA, col = "tan",
#      main = "Regions used for linear whale risk",
#      xlim = c(-135, -115), ylim = c(27, 51))
# plot(map.base2, add = TRUE, border = "black", col = "tan")
# plot(grid.toplot, add = TRUE, col = "purple", border = NA)
# abline(h = reg.bound, col = "red")
# for (i in 1:6) text(rev(reg.txt)[i], x = -135, y = reg.bound[i] + 0.5, pos = 4)
# legend("topright", legend = c("Region boundary", "Cells with non-NA risk"),
#        col = c("red", "purple"), lty = c(1, 0), pch = c(NA, 15), pt.cex = c(NA, 2))
# legend("bottomleft", legend = paste0(reg.txt, " - ", reg.areas, " km2"), col = NA,
#        title = "Area of cells with non-NA risk")
# graphics::box()
# dev.off()


###############################################################################
### Create map of where fishing data is confidential

# map.base1 <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
# map.base2 <- st_geometry(st_as_sf(maps::map('state', plot = FALSE, fill = TRUE)))
# 
# grid.nona.any <- fish.all.nona %>%
#   select(GRID5KM_ID) %>%
#   left_join(grid.5km.geom) %>%
#   st_as_sf()
# 
# grid.thirdly <- st_read(
#   "Data/Thirdly PacFIN informed VMS shapefile/Feist_et_al_5km_cumulative_thirdly_Dungeness_CONFIDENTIAL_lamb.shp"
# ) %>%
#   filter(!is.na(DJO2010_13)) %>%
#   st_transform(4326)
# 
# # Function for plotting grid cell confidentiality map
# grid_confidential_map <- function(
#   file.name, plot.main, grid.base, grid.curr, leg.txt)
# {
#   png(file.name, height = 8.5, width = 5, units = "in", res = 300)
#   plot(map.base1, axes = TRUE, border = NA, col = "tan",
#        main = plot.main,
#        xlim = c(-135, -115), ylim = c(27, 51))
#   plot(map.base2, add = TRUE, border = "black", col = "tan")
#   plot(st_geometry(grid.base), add = TRUE, col = "red", border = NA)
#   plot(st_geometry(grid.curr), add = TRUE, col = "blue", border = NA)
#   legend("topright", legend = leg.txt, col = c("red", "blue"), pch = 15, pt.cex = 2)
#   graphics::box()
#   dev.off()
# }
# 
# # Map of grid cells that have non-NA value FOR ANY month
# grid_confidential_map(
#   file.name = paste0(path.plots, "Grid_nona_any.png"),
#   plot.main = "Grid cells with any non-confidential data",
#   grid.base = grid.thirdly, leg.txt = c("Thirdly grid", "Monthly grid"),
#   grid.curr = grid.nona.any
# )
# 
# # Map of grid cells that have non-NA value BY month
# for (i in unique(fish.summ$year_mo)) {
#   print(i)
#   i.txt <- substr(i, 4, 10)
#   
#   curr.sf <- fish.summ %>%
#     filter(year_mo == i) %>%
#     left_join(grid.5km.geom, by = "GRID5KM_ID") %>%
#     st_as_sf()
#   
#   grid_confidential_map(
#     file.name = paste0(path.plots, "Grid_nona_monthly/Grid_nona_", i.txt, ".png"),
#     plot.main = paste("Grid cells for", i.txt, "with non-NA values"),
#     grid.base = grid.nona.any, grid.curr = curr.sf,
#     leg.txt = c("Monthly grid (all)", "Monthly grid")
#   )
#   rm(i.txt, curr.sf)
# }; rm(i)
# 
# # Map of grid cells that any confidential value BY month
# for (j in unique(fish.summ$year_mo)) {
#   print(j)
#   j.txt <- substr(j, 4, 10)
#   
#   curr.sf <- fish.summ %>%
#     filter(year_mo == j) %>%
#     left_join(grid.5km.geom, by = "GRID5KM_ID") %>%
#     st_as_sf()
#   
#   grid_confidential_map(
#     file.name = paste0(path.plots, "Grid_confidential/Grid_confidential_", j.txt, ".png"),
#     plot.main = paste("Grid cells with VMS data for", j.txt),
#     grid.base = curr.sf, grid.curr = filter(curr.sf, !confid_any),
#     leg.txt = c("Confidential", "Non-confidential")
#   )
#   rm(j.txt, curr.sf)
# }; rm(j)


###############################################################################
### Plot entanglements...

# library(readxl)
# entang.col <- RColorBrewer::brewer.pal(6, "Set1")
# entang.reg.txt <- c("WA", "OR", "CA-N", "CA-Cen", "CA-SCen", "CA-S")
# entang.season.idx <- seq(from = 5, to = 72, by = 12) %>% 
#   set_names(paste0(2010:2015, "-", 11:16))
# entang.df <- read_xlsx("Data/Entanglements/Entanglements_known_gear_set_location_SMW.xlsx") 
# 
# entang.summ <- entang.df %>% 
#   filter(Year > 2010, !is.na(Season_SMW), Season_SMW %in% names(entang.season.idx)) %>% 
#   # select(Year, Month, Region_SMW, Season_SMW) %>% 
#   # tidyr::unite(Year, Month, col = "yrmo", sep = "-") %>% 
#   group_by(Region_SMW, Season_SMW) %>% 
#   summarise(count = n()) %>% 
#   ungroup() %>% 
#   mutate(Region_SMW_num = unname(sapply(Region_SMW, function(i) which(i == entang.reg.txt))), 
#          Season_SMW_idx = unname(sapply(Season_SMW, function(i) entang.season.idx[i == names(entang.season.idx)])))
# 
# 
# 
# 
# png(paste0(path.plots, "Entanglements.png"), height = 3, width = 10, units = "in", res = 300)
# # opar <- par(mfrow = c(3, 1))
# 
# plot.new()
# plot.window(xlim = c(0, 83), ylim = c(0, max(entang.summ$count + 1)))
# title("Entanglements with confirmed location and season")
# points(x = entang.summ$Season_SMW_idx, y = entang.summ$count, 
#        pch = 13, col = entang.col[entang.summ$Region_SMW_num], cex = 2, lwd = 2)
# axis(2)
# rug(x = 1:72, ticksize = -0.02)
# axis.idx = seq(from = 5, to = 72, by = 12)
# axis(
#   1, at = (1:72)[axis.idx], 
#   labels = paste(df.key$year, df.key$month, sep = "-")[axis.idx], 
#   las = 3, cex.axis = 1
# )
# abline(v = seq(from = 4.5, to = 72, by = 12), col = "grey")
# abline(v = 36.5, col = "red")
# graphics::box()
# 
# # par(opar)
# dev.off()

###############################################################################

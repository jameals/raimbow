# Script to select whale preds from grid cells with fishing effort, 
#   and group by region

#------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)


source("User_script_local.R")
if (user == "JS") {
  path.grid.studyarea <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_studyarea.rds"
  path.grid.lno <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid_5km_landerased.RDATA"
  path.hump <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds"
  path.blue <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds"
  
  file.out <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_whale.rds"
  
  
} else if (user == "SMW") {
  # path.key.region <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key_region.rds"
  # path.key.bia <- "../raimbow-local/RDATA_files/Grid5km_BIA_overlap.rds"
  path.grid.studyarea <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_studyarea.rds"
  path.grid.lno <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.RDATA"
  path.hump <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
  path.blue <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds"
  
  file.out <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_whale.rds"
  
} else {
  stop("Invalid user")
}


#------------------------------------------------------------------------------
# Filter for study area grid cells, and calculate humpback abundance
grid.studyarea <- readRDS(path.grid.studyarea)
load(path.grid.lno) #variable is grid.5km.lno
stopifnot(all(grid.studyarea %in% grid.5km.lno$GRID5KM_ID))

x.blue <- readRDS(path.blue) %>%
  filter(GRID5KM_ID %in% grid.studyarea) %>% 
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

x.hump <- readRDS(path.hump) %>%
  filter(GRID5KM_ID %in% grid.studyarea) %>% 
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

# Convert humpback values to abundance, so both values are non-area-normalized
#   TODO: confirm that it's correct to multiply SE by constant
x.hump <- x.hump %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") %>% 
  mutate(Humpback_abund_mean = Humpback_dens_mean * area_km_lno, 
         Humpback_abund_se = Humpback_dens_se * area_km_lno) %>% 
  select(-area_km_lno, -Humpback_dens_mean, -Humpback_dens_se)


# All grid cells are present 127 times, which is the number of unique year-months
table(table(x.blue$GRID5KM_ID))
table(table(x.hump$GRID5KM_ID))
length(unique(x.blue$year_month))

sum(is.na(x.blue$Blue_occurrence_mean))
sum(is.na(x.hump$Humpback_abund_mean))

# Confirms the 24 and 3, respectivley, study area grid cells not in whale data
sum(!(grid.studyarea %in% x.blue$GRID5KM_ID)) #24
sum(!(grid.studyarea %in% x.hump$GRID5KM_ID)) #3


#------------------------------------------------------------------------------
# Join whale datasets, join identifier variables, and save to file
# key.region <- readRDS(path.key.region) %>% select(-region_ts)
# key.bia <- readRDS(path.key.bia) %>% 
#   select(GRID5KM_ID, BIA_mn, BIA_bm)

# When joining the whale datasets, 254 rows will be added to the number from the humpback data
sum(!(x.blue$GRID5KM_ID %in% x.hump$GRID5KM_ID)) #254

x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month")) %>% 
  # left_join(key.region, by = "GRID5KM_ID") %>%
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") %>% 
  mutate(normalized_humpback = as.vector(scale(Humpback_abund_mean, 
                                               center = min(Humpback_abund_mean, na.rm=TRUE), 
                                               scale = diff(range(Humpback_abund_mean, na.rm=TRUE)))),
         normalized_blue = as.vector(scale(Blue_occurrence_mean, 
                                           center = min(Blue_occurrence_mean, na.rm=TRUE), 
                                           scale = diff(range(Blue_occurrence_mean, na.rm=TRUE))))) %>% 
  select(GRID5KM_ID, year_month, area_km_lno, everything())

saveRDS(x.whale, file = file.out)


###############################################################################
# # Summarize stuff
# #   How to carry along uncertainty.?
# hump.summ <- x.hump %>% 
#   group_by(Region, year_month) %>% 
#   summarise(Humpback_dens_sum = sum(Humpback_dens_mean, na.rm = TRUE), 
#             count = n())
# 
# blue.summ <- x.blue %>% 
#   group_by(Region, year_month) %>% 
#   summarise(Blue_occurrence_sum = sum(Blue_occurrence_mean, na.rm = TRUE), 
#             count = n())
# 
# 
# table(hump.summ$count, hump.summ$Region)
# table(blue.summ$count, blue.summ$Region)


# ggplot(hump.summ, aes(x = year_month, y = Humpback_dens_sum, color = Region, group = Region)) + 
#   geom_point() + 
#   geom_line()

###############################################################################

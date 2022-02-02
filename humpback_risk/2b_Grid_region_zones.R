### Define and save regions (zones) used in other parts of humpback_risk
# Uses the output from "2_Whale_risk.Rmd", hence the 2b

# These regions/zones are visualized (plotted) in 
#TODO

###############################################################################
library(dplyr)

source(here::here("User_script_local.R"))
if (user == "JS") {
  
} else if (user == "SMW") {
  file.risk <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Whale_risk_long_nona.Rdata"
  file.out.region.zone <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_region_zones.Rdata"
  file.out.zone <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_zones.Rdata"
  
} else {
  stop("User not recognized")
}

# path.rdata <- "../raimbow-local/RDATA_files/"


# load(paste0(path.rdata, "Whale_risk_long_nona.Rdata"))
# load(paste0(path.rdata, "Grid_5km_landerased.RDATA"))


###############################################################################
load(file.risk)

# See https://wildlife.ca.gov/Conservation/Marine/Whale-Safe-Fisheries
# Large regions: WA, OR, CA-N, CA-Cen, CA-SCen, CA-S
reg.bound <- c(32.5, 34+(27/60), 36, 38+(46.125/60), 42, 46.25, 50)
reg.names <- c("CA-S", "CA-SCen", "CA-Cen", "CA-N", "OR", "WA")
stopifnot(length(reg.names) == length(reg.bound) - 1)

grid.region <- all.df %>% 
  filter(!duplicated(GRID5KM_ID)) %>% 
  arrange(GRID5KM_ID) %>% 
  mutate(region.idx = findInterval(LATITUDE, reg.bound, left.open = TRUE), 
         region = factor(reg.names[region.idx], levels = rev(reg.names))) %>% 
  select(GRID5KM_ID, region)

save(reg.bound, reg.names, grid.region, file = file.out.region.zone)
# file = paste0(path.rdata, "Grid_region.Rdata")
# )


###############################################################################
# Counties
zone.bound <- c(
  42, 40+(10/60), 38+(46.125/60), 37+(11/60), 36, 34+(27/60), 32+(32/60)
)

zone.list <- list(
  "zone1" = c(zone.bound[1], zone.bound[2]), 
  "zone2" = c(zone.bound[2], zone.bound[3]), 
  "zone3" = c(zone.bound[3], zone.bound[4]), 
  "zone4" = c(zone.bound[4], zone.bound[5]), 
  "zone5" = c(zone.bound[5], zone.bound[6]), 
  "zone6" = c(zone.bound[6], zone.bound[7])
)

zone.names <- names(zone.list)

# Add in WA and OR
zone.bound <- rev(c(50, 46.25, zone.bound))
zone.names <- rev(c("WA", "OR", zone.names))
stopifnot(length(zone.bound) == length(zone.names) + 1)

grid.zone <- all.df %>%
  filter(!duplicated(GRID5KM_ID)) %>%
  arrange(GRID5KM_ID) %>%
  mutate(region.idx = findInterval(LATITUDE, zone.bound, 
                                   left.open = TRUE),
         region = factor(zone.names[region.idx], 
                         levels = rev(zone.names))) %>%
  select(GRID5KM_ID, region) %>% 
  arrange(GRID5KM_ID)

save(
  zone.bound, zone.names, grid.zone, 
  file = file.out.zone
)

###############################################################################

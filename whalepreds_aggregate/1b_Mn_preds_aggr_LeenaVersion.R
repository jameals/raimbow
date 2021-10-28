# Code for aggregating Mn predictions by month, after overlaying them
#   onto 5km grid


###############################################################################
library(tidyverse)
library(lubridate)
library(sf)

source(here::here("whalepreds_aggregate", "Whalepreds_aggregate.R"), local = TRUE, echo = FALSE)
source(here::here("User_script_local.R"), local = TRUE, echo = FALSE)

if (user == "JS") {
  
} else if (user == "SMW") {
  path.mnpreds.rds <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens.rds"
  file.out <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
}

#Leena
path.mnpreds.rds <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_bidaily_dens_20211027.rds"
file.out <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly_20211028.rds"

#if want to read in new 2009-2020 monthly input file:
path.mnpreds.rds <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_wide_MONTHLY2009_2020_dens.rds"
file.out <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"

###############################################################################
### Read in KAF Mn predictions that have been overlaid onto Blake's 5km EA grid
mn.overlaid <- readRDS(path.mnpreds.rds) %>% 
  #mutate(H_10_12_27 = NA, H_10_12_29 = NA, H_10_12_31 = NA) %>%  #this line is not relevant if didn't use bidaily input
  select(GRID5KM_ID, area_km_lno, everything())


### Aggregate them by month  ##NOT RELEVANT IF USED MONTHLY INPUT
# TODO user: update date.max as needed
date.max <- as.Date("2019-08-01")
date.max <- as.Date("2020-09-01")
range.dates <- seq(from = as.Date("2009-01-01"), to = date.max, by = "months")

mn.aggr <- whalepreds_aggregate(
  mn.overlaid, 3:ncol(mn.overlaid), 3:10, aggr.level = NULL, #3:ncl selects columns from 3 to max column. 3:10 specifies date info in the column name
  range.dates = range.dates, se.calc = TRUE
) %>% 
  set_names(c("GRID5KM_ID", "area_km_lno", paste0("Mn_", names(.)[-c(1:2)]))) %>% 
  set_names(gsub("Avg_user_", "", names(.))) %>% 
  set_names(gsub("user_", "", names(.)))
#edit above if already working on monthly data
colnames(mn.overlaid)<-gsub("H_","",colnames(mn.overlaid))
mn.aggr <- mn.overlaid %>% 
  set_names(c("GRID5KM_ID", "area_km_lno", paste0("Mn_20", names(.)[-c(1:2)]))) %>% 
  set_names(gsub("Avg_user_", "", names(.))) %>% 
  set_names(gsub("user_", "", names(.)))


mn.proc <- mn.aggr %>% 
  gather(key = "key", value = "value", -GRID5KM_ID, -area_km_lno) %>% 
  mutate(type = ifelse(grepl("SE", key), "se", "pred"),
         date = ymd(ifelse(type == "se", substr(key, 7, 16), substr(key, 4, 13)))) #7,16 refers to this column naming: Mn_SE_2019_07_01, 4,13 refers to this column naming: Mn_2009_01_01

mn.proc.long <- mn.proc %>% 
  select(-key) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  #rename(Humpback_dens_mean = pred, Humpback_dens_se = se) #monthly 2005-2020 input didn't have any SE info/it could not be calculated
  rename(Humpback_dens_mean = pred)


###############################################################################
saveRDS(mn.proc.long, file = file.out)

###############################################################################

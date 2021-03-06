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


###############################################################################
### Read in KAF Mn predictions that have been overlaid onto Blake's 5km EA grid
mn.overlaid <- readRDS(path.mnpreds.rds) %>% 
  mutate(H_10_12_27 = NA, H_10_12_29 = NA, H_10_12_31 = NA) %>% 
  select(GRID5KM_ID, area_km_lno, everything())


### Aggregate them by month
# TODO user: update date.max as needed
date.max <- as.Date("2019-08-01")
range.dates <- seq(from = as.Date("2009-01-01"), to = date.max, by = "months")

mn.aggr <- whalepreds_aggregate(
  mn.overlaid, 3:ncol(mn.overlaid), 3:10, aggr.level = NULL, 
  range.dates = range.dates, se.calc = TRUE
) %>% 
  set_names(c("GRID5KM_ID", "area_km_lno", paste0("Mn_", names(.)[-c(1:2)]))) %>% 
  set_names(gsub("Avg_user_", "", names(.))) %>% 
  set_names(gsub("user_", "", names(.)))

mn.proc <- mn.aggr %>% 
  gather(key = "key", value = "value", -GRID5KM_ID, -area_km_lno) %>% 
  mutate(type = ifelse(grepl("SE", key), "se", "pred"),
         date = ymd(ifelse(type == "se", substr(key, 7, 16), substr(key, 4, 13))))

mn.proc.long <- mn.proc %>% 
  select(-key) %>%
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(Humpback_dens_mean = pred, Humpback_dens_se = se)


###############################################################################
saveRDS(mn.proc.long, file = file.out)

###############################################################################

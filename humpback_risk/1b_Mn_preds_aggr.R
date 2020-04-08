library(tidyverse)
library(lubridate)
library(sf)

source("whalepreds_aggregate/Whalepreds_aggregate.R", local = TRUE, echo = FALSE)

file.data.mnpreds <- "../raimbow-local/Outputs/WEAR5km_76_2009-01-02to2019-08-14_Bidaily_dens.csv"
file.landerased <- "../raimbow-local/RDATA_files/Grid_5km_landerased.RDATA"


### Read in KAF Mn predictions that have been overlaid onto Blake's 5km EA grid
load(file.landerased)
humpback.raw <- read_csv(file.data.mnpreds) %>% 
  mutate(H_10_12_27 = NA, H_10_12_29 = NA, H_10_12_31 = NA) %>% 
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") %>% 
  select(GRID5KM_ID, area_km_lno, everything())



range.dates <- seq(
  from = as.Date("2009-01-01"), to = as.Date("2019-08-01"), by = "months"
)

mn.aggr <- whalepreds_aggregate(
  humpback.raw, 3:ncol(humpback.raw), 3:10, aggr.level = NULL, 
  range.dates = range.dates, se.calc = TRUE
) %>% 
  set_names(c("GRID5KM_ID", "area_km_lno", paste0("Mn_", names(.)[-c(1:2)]))) %>% 
  set_names(gsub("Avg_user_", "", names(.))) %>% 
  set_names(gsub("user_", "", names(.)))

mn.proc <- mn.aggr %>% 
  gather(key = "key", value = "value", -GRID5KM_ID, -area_km_lno) %>% 
  mutate(type = ifelse(grepl("SE", key), "se", "pred"),
         date = ymd(ifelse(type == "se", substr(key, 7, 16), substr(key, 4, 13))))

mn.proc2 <- mn.proc %>% 
  select(-key) %>%
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(Humpback_dens_mean = pred, Humpback_dens_se = se)

saveRDS(mn.proc2, file = "../raimbow-local/Outputs/Humpback_5km_long_monthly.rds")

library(dplyr)
library(lubridate)

# SW
# x.hump <- readRDS("../raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
#JS
x.hump <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

# SW
# x.blue <- readRDS("../raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

# JS 
x.blue <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

#SW
# x.orig <- readRDS("../raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
#   select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))
# rm(x.hump)
#JS
# x.orig <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
#   select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))

x.orig.noinfo <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"),
         -contains("H_Avg_Abund"), -contains("Blue_")) 

#rm(x.hump)

##### TODO - discuss
x.reg.key.test <- x.orig.noinfo %>% 
  select(Region, GRID5KM_ID) %>% 
  distinct()
x.reg.key.test[which(duplicated(x.reg.key.test$GRID5KM_ID)), ]

# ^ Shows that several grid cells have multiple 'Region' specifications - this is a temporary fix
x.orig.noinfo <- x.orig.noinfo %>% 
  mutate(Region = ifelse(GRID5KM_ID %in% c(63521:63524), "CenCA", Region))
x.reg.key <- x.orig.noinfo %>% 
  select(Region, GRID5KM_ID) %>% 
  distinct()

#####


x.orig <- x.orig.noinfo %>%
  left_join(x.blue, by = c("year_month", "GRID5KM_ID")) %>% 
  left_join(x.hump, by = c("year_month", "GRID5KM_ID"))


x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month")) %>% 
  left_join(x.reg.key)
# rm(x.hump, x.blue)

#####

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
d.noinfo <- effort_mgmt(
  x = x.orig.noinfo,
  early.data.method = "pile", 
  delay.date = as.Date("2009-11-15"),
  delay.region = "CenCA",
  delay.method.shift = "pile",
  delay.method.fidelity = "spatial",
  closure.date = as.Date("2010-04-01"),
  closure.region = "BIA",
  closure.method = "temporal",
  closure.redist.percent = 10
)

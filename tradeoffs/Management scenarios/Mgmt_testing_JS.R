library(dplyr)
library(lubridate)

# SW
x.hump <- readRDS("../raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
#JS
x.hump <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

#SW
x.orig <- readRDS("../raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
  left_join(x.hump, by = c("year_month", "GRID5KM_ID"))
rm(x.hump)
#JS
x.orig <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
  left_join(x.hump, by = c("year_month", "GRID5KM_ID"))
rm(x.hump)


# source("tradeoffs/Management scenarios/Funcs_management_scenarios.R") # this is a relict, right Sam? so we can delete?
d <- effort_mgmt(
  x = x.orig,
  delay.date = as.Date("2009-12-15"),
  delay.region = "CenCA",
  delay.method.shift = "lag",
  delay.method.fidelity = "temporal",
  closure.date = as.Date("2010-04-01"),
  closure.region = "CenCA",
  closure.method = "temporal",
  closure.redist.percent = 10
)
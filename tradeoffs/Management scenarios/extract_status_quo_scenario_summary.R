# pull out sq summaries

library(tidyverse)
library(sf)

# read in sq data
x.orig.noinfo <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS")

##### summarize effort for sq scenario

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# STATUS QUO
scenario.output.df.noinfo.sq <- effort_mgmt(
  x = x.orig.noinfo,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method = NULL,
  delay.method.fidelity = NULL,
  closure.date = NULL,
  closure.region = NULL,
  closure.method = NULL,
  closure.redist.percent = 100,
  depth.val = NULL,
  reduction.before.date = NULL,
  reduction.before.percent = 50,
  reduction.before.region = NULL,
  reduction.after.date = NULL,
  reduction.after.percent = 50,
  reduction.after.region = NULL
)

# grab whale data

# JS 
x.whale <-readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_whale.rds")

# Load and prep grid cell - area key
load("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid_5km_landerased.RDATA")
area.key <- grid.5km.lno %>% 
  st_drop_geometry() %>% 
  select(GRID5KM_ID, area_km_lno) %>% 
  distinct()

### Calculate and summarize risk for sq scenario with normalized outputs

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
risk_out_sq <- risk_mgmt(
  x = scenario.output.df.noinfo.sq, 
  x.col = Num_DCRB_VMS_pings, 
  y = x.whale,
  risk.unit = "dens", 
  area.key = area.key
)
glimpse(risk_out_sq)

range(risk_out_sq$Num_DCRB_VMS_pings) # 1048 is max, which is different than the input into this function

risk_out_sq_list_by_yr_mth <- risk_out_sq %>% split(.$year_month)

# write out for trial run using prioritizr
write_rds(risk_out_sq_list_by_yr_mth, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_2009_2019_list_by_yr_mth_",today(),".rds"))

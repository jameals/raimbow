# pull out sq summaries
rm(list = ls())

library(tidyverse)
library(sf)

# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- sapply(strsplit(x, " "), function(i) i[[1]])
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# read in sq data
x.orig.noinfo <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS")

# read in season start date key
season.st.date.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/season_start_dates/start_dates_by_CA_region.rds") %>% 
  mutate(crab_year = gsub("-", "_", .data$crab_season), 
         Region = unname(sapply(CA_region, simpleCap))) %>% 
  select(crab_year, Region, start_of_season_oneperc)

##### summarize effort for sq scenario

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# STATUS QUO
scenario.output.df.noinfo.sq <- effort_mgmt(
  x = x.orig.noinfo,
  season.st.key = season.st.date.key, 
  preseason.days = 3,
  season.st.backstop = NULL, 
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method = NULL,
  delay.method.fidelity = NULL,
  closure.date = NULL,
  closure.region = NULL,
  closure.method = NULL,
  closure.redist.percent = 100,
  depth.shallow = NULL, 
  depth.deep = NULL, 
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
CA_fishing_metrics_range_2009_2019 <- read_rds(here:: here(
  "grid-prep",
  "CA_fishing_metrics_range_2009_2019.rds")
)


source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
risk_out_sq <- risk_mgmt(
  x = scenario.output.df.noinfo.sq, 
  x.col = Num_DCRB_VMS_pings, 
  y = x.whale,
  risk.unit = "dens", 
  area.key = area.key,
  scale.list = CA_fishing_metrics_range_2009_2019, 
  ym.min = "2009_11", 
  ym.max = "2019_07"
)
glimpse(risk_out_sq)

range(risk_out_sq$Num_DCRB_VMS_pings) # 1354 is max

# write out for use in tradeoff figures Rmd
write_rds(risk_out_sq, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_2009_2019_yr_mth_",today(),".rds"))

risk_out_sq_list_by_yr_mth <- risk_out_sq %>% split(.$year_month)

# write out for trial run using prioritizr
write_rds(risk_out_sq_list_by_yr_mth, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_2009_2019_list_by_yr_mth_",today(),".rds"))

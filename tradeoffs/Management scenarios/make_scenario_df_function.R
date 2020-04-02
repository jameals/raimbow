# 022720
# Dependencies: run prep_data_for_scenario_df_function.R, make_scenarios_table.R

library(foreign)
library(lubridate)
library(tidyverse)
library(reshape2)
library(scales)
library(zoo)
library(ggrepel)
library(sf)
library(data.table)
library(wesanderson)
library(viridis)
library(here)
#library(ggerr)

con_df_weekly_years_5km_CA <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_2009-2018_fishtix_humpback_blue_whales_grids.RDS")

scenario_table <- read_rds(here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table.RDS"
  )
)


con_df_weekly_years_5km_CA %>%
  #mutate(spatial_domain = ifelse(is_empty(spatial_domain)==TRUE,NA,spatial_domain),
         #time_period = ifelse(is_empty(time_period)==TRUE,NA,time_period),
  #       ) %>% 
  group_by(crab_year, Region, season) # crab year, spatial domain, temporal domain in which fishing still occurs


scenario_summary_function <- function(df, spatial_domain, scenario_lag, scenario_df_name) {
  
}